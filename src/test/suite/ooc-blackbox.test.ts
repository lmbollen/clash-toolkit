import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import { promises as fs } from 'fs';
import { execFile } from 'child_process';
import { promisify } from 'util';

import { buildPerModuleScript, perModuleConcurrency } from '../../yosys-runner';
import { getDefaultOutOfContextScript, resolveScript } from '../../synthesis-targets';
import { ComponentInfo } from '../../clash-manifest-types';

const execFileAsync = promisify(execFile);

/**
 * Out-of-context synthesis stubs a component's sub-components as Yosys black
 * boxes so each run covers that component's own logic and nothing below it.
 *
 * The script is the whole contract — everything below either reads it or feeds
 * it to a real Yosys and checks what came out.
 */
suite('Out-of-context black boxes', () => {

	const comp = (
		name: string,
		dependencies: string[],
		verilogFiles: string[] = [`/design/${name}.v`]
	): ComponentInfo => ({
		name, dependencies, verilogFiles, directory: '/design'
	});

	const graph = (...comps: ComponentInfo[]): Map<string, ComponentInfo> =>
		new Map(comps.map(c => [c.name, c]));

	/** Apex → Mid → Leaf, the shape that shows stubbing is one level deep. */
	const chain = (): Map<string, ComponentInfo> => graph(
		comp('Leaf', []),
		comp('Mid', ['Leaf']),
		comp('Apex', ['Mid']),
	);

	const scriptFor = (
		name: string,
		byName: Map<string, ComponentInfo>,
		flow: 'synthesize' | 'elaborate' = 'synthesize',
		customScript?: string
	) => buildPerModuleScript(byName.get(name)!, byName, flow, '/out', customScript);

	// ── Script shape ─────────────────────────────────────────────────────────

	test('direct dependencies are read as black boxes, own Verilog in full', () => {
		const script = scriptFor('Apex', chain());
		assert.match(script, /read_verilog -lib "\/design\/Mid\.v"/);
		assert.match(script, /^read_verilog "\/design\/Apex\.v"$/m);
		assert.doesNotMatch(
			script, /read_verilog "\/design\/Mid\.v"/,
			'a stubbed dependency must never also be read in full'
		);
	});

	test('stubbing stops at direct dependencies', () => {
		// Mid is a black box, so its body — and therefore Leaf — is never
		// referenced. Reading Leaf would elaborate logic this run must exclude.
		const script = scriptFor('Apex', chain());
		assert.doesNotMatch(
			script, /Leaf\.v/,
			'a grandchild is unreachable through a black box and must not be read'
		);
	});

	test('no flatten — flattening would defeat the stubs', () => {
		assert.doesNotMatch(scriptFor('Apex', chain()), /^flatten$/m);
	});

	test('black-box instances are kept against optimization', () => {
		// opt/opt_clean/clean delete an instance whose outputs happen to be
		// unused; the component would then vanish from the diagram silently.
		const script = scriptFor('Apex', chain());
		assert.match(script, /setattr -set keep 1 t:Mid/);
		// After proc (so the cells exist) and before the opt passes.
		assert.ok(
			script.indexOf('\nproc\n') < script.indexOf('setattr -set keep'),
			'keep must be set after proc'
		);
		assert.ok(
			script.indexOf('setattr -set keep') < script.indexOf('opt -purge'),
			'keep must be set before the first opt pass'
		);
	});

	test('every direct dependency gets a keep selector, on one line', () => {
		const byName = graph(comp('A', []), comp('B', []), comp('Top', ['A', 'B']));
		const script = scriptFor('Top', byName);
		assert.match(script, /setattr -set keep 1 t:A t:B/);
	});

	test('a leaf component emits no keep line', () => {
		assert.doesNotMatch(scriptFor('Leaf', chain()), /setattr -set keep/);
	});

	test('dependencies outside the run are not stubbed', () => {
		// A vendor primitive Clash instantiates is not a component of ours: we
		// have no Verilog to stub it from, and `keep` on it would be a no-op.
		const byName = graph(comp('Top', ['altpll']));
		const script = scriptFor('Top', byName);
		assert.doesNotMatch(script, /setattr -set keep/);
		assert.doesNotMatch(script, /read_verilog -lib/);
	});

	test('a file shared with a dependency is read in full, not stubbed', () => {
		// Clash can emit several components into one file. The full read has to
		// win, or the component being synthesized would stub away its own body.
		const shared = '/design/Shared.v';
		const byName = graph(
			comp('Dep', [], [shared]),
			comp('Top', ['Dep'], [shared, '/design/Top.v']),
		);
		const script = scriptFor('Top', byName);
		assert.doesNotMatch(script, /read_verilog -lib "\/design\/Shared\.v"/);
		assert.match(script, /^read_verilog "\/design\/Shared\.v"$/m);
	});

	test('elaboration reads dependencies in full, transitively', () => {
		// Its netlist has to carry the real definitions for drill-down to work.
		const script = scriptFor('Apex', chain(), 'elaborate');
		assert.doesNotMatch(script, /read_verilog -lib/);
		assert.doesNotMatch(script, /setattr -set keep/);
		assert.match(script, /read_verilog "\/design\/Mid\.v"/);
		assert.match(script, /read_verilog "\/design\/Leaf\.v"/);
	});

	// ── The script is a user-editable template ───────────────────────────────

	test('the built-in template is what an unconfigured run uses', () => {
		// Resolving the default template by hand must reproduce the script the
		// runner writes, or the panel would be showing a script nothing runs.
		const apex = chain().get('Apex')!;
		const expected = resolveScript(getDefaultOutOfContextScript(), {
			files: apex.verilogFiles,
			libFiles: ['/design/Mid.v'],
			blackBoxes: ['Mid'],
			topModule: 'Apex',
			outputDir: '/out',
			outputBaseName: 'Apex',
		});
		assert.strictEqual(scriptFor('Apex', chain()), expected);
	});

	test('a custom out-of-context script replaces the default', () => {
		const custom = '# mine\n{libFiles}\n{files}\nhierarchy -top {topModule}\n{keepBlackBoxes}\n';
		const script = scriptFor('Apex', chain(), 'synthesize', custom);
		assert.match(script, /^# mine$/m);
		assert.match(script, /read_verilog -lib "\/design\/Mid\.v"/);
		assert.match(script, /^read_verilog "\/design\/Apex\.v"$/m);
		assert.match(script, /hierarchy -top Apex/);
		assert.match(script, /setattr -set keep 1 t:Mid/);
		assert.doesNotMatch(script, /memory -nomap/, 'the default body is gone');
	});

	test('the target script never leaks onto the out-of-context path', () => {
		// The two are stored separately; an out-of-context run has no synth_*
		// step, so the target's script has nothing to say about it.
		const script = scriptFor('Apex', chain());
		assert.doesNotMatch(script, /synth_/);
	});

	test('elaboration ignores the out-of-context script', () => {
		const custom = '# should not appear\n{files}\n';
		const script = scriptFor('Apex', chain(), 'elaborate', custom);
		assert.doesNotMatch(script, /should not appear/);
		assert.match(script, /opt_clean/);
	});

	// ── Concurrency ──────────────────────────────────────────────────────────

	test('concurrency stays within one process per core, capped at 8', () => {
		const cores = os.cpus()?.length ?? 1;
		for (const n of [1, 2, 5, 50]) {
			const jobs = perModuleConcurrency(n);
			assert.ok(jobs >= 1, `${n} components: at least one job, got ${jobs}`);
			assert.ok(jobs <= 8, `${n} components: capped at 8, got ${jobs}`);
			assert.ok(jobs <= n, `${n} components: never more jobs than work, got ${jobs}`);
			assert.ok(
				jobs <= Math.max(1, cores - 1),
				`${n} components: leaves a core for the editor, got ${jobs} on ${cores} cores`
			);
		}
	});

	// ── Against a real Yosys ─────────────────────────────────────────────────

	/**
	 * The assertions above describe the script; this one proves Yosys agrees.
	 * Skipped when Yosys is absent rather than failing — the rest of the suite
	 * does not require a toolchain.
	 */
	test('Yosys keeps the instance and excludes the sub-component\'s logic', async function () {
		this.timeout(60000);
		try {
			await execFileAsync('yosys', ['-V']);
		} catch {
			this.skip();
			return;
		}

		const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-ooc-'));
		try {
			// Mid's output is deliberately unused inside Apex: without `keep`,
			// Yosys deletes the instance and the component disappears.
			await fs.writeFile(path.join(dir, 'Leaf.v'),
				'module Leaf(input c, input [7:0] x, output reg [7:0] o);\n' +
				'  always @(posedge c) o <= x * 8\'d7;\n' +
				'endmodule\n');
			await fs.writeFile(path.join(dir, 'Mid.v'),
				'module Mid(input c, input [7:0] x, output [7:0] o);\n' +
				'  Leaf l(.c(c), .x(x), .o(o));\n' +
				'endmodule\n');
			await fs.writeFile(path.join(dir, 'Apex.v'),
				'module Apex(input c, input [7:0] x, output [7:0] o);\n' +
				'  wire [7:0] unused;\n' +
				'  Mid m(.c(c), .x(x), .o(unused));\n' +
				'  assign o = x + 8\'d1;\n' +
				'endmodule\n');

			const byName = graph(
				comp('Leaf', [], [path.join(dir, 'Leaf.v')]),
				comp('Mid', ['Leaf'], [path.join(dir, 'Mid.v')]),
				comp('Apex', ['Mid'], [path.join(dir, 'Apex.v')]),
			);

			const jsonPath = path.join(dir, 'Apex.json');
			const script = buildPerModuleScript(
				byName.get('Apex')!, byName, 'synthesize', dir
			);
			const scriptPath = path.join(dir, 'synth.ys');
			await fs.writeFile(scriptPath, script);

			await execFileAsync('yosys', ['-q', '-s', scriptPath], { cwd: dir });

			const netlist = JSON.parse(await fs.readFile(jsonPath, 'utf8')) as {
				modules: Record<string, {
					attributes?: Record<string, string>;
					cells?: Record<string, { type: string }>;
				}>;
			};

			const apexCells = Object.values(netlist.modules['Apex']?.cells ?? {});
			assert.ok(
				apexCells.some(c => c.type === 'Mid'),
				`the Mid instance must survive despite its unused output, got ` +
				`${JSON.stringify(apexCells.map(c => c.type))}`
			);
			assert.notStrictEqual(
				Number(netlist.modules['Mid']?.attributes?.blackbox ?? 0), 0,
				'Mid must be a black box in the netlist'
			);
			assert.strictEqual(
				Object.keys(netlist.modules['Mid']?.cells ?? {}).length, 0,
				'a black box holds no logic of its own'
			);
			assert.ok(
				!('Leaf' in netlist.modules),
				'Leaf is below a black box and must not appear at all'
			);

			// Apex's own logic is there, and Leaf's multiplier is not.
			const stats = JSON.parse(
				await fs.readFile(path.join(dir, 'stats.json'), 'utf8')
			) as { design?: { num_cells_by_type?: Record<string, number> } };
			const byType = stats.design?.num_cells_by_type ?? {};
			assert.ok(byType['Mid'] === 1, `Mid counted once, got ${JSON.stringify(byType)}`);
			assert.ok(
				!Object.keys(byType).some(t => t.includes('mul')),
				`Leaf's multiplier must not be counted here, got ${JSON.stringify(byType)}`
			);
		} finally {
			await fs.rm(dir, { recursive: true, force: true });
		}
	});
});
