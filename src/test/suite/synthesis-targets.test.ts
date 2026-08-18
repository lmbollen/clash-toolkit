import * as assert from 'assert';
import * as path from 'path';
import * as os from 'os';
import { promises as fs } from 'fs';
import { spawn } from 'child_process';
import * as vscode from 'vscode';
import {
	SYNTHESIS_TARGETS,
	TARGET_IDS,
	getDefaultScript,
	getTarget,
	resolveScript,
	computeScriptDiff,
} from '../../synthesis-targets';
import { YosysRunner } from '../../yosys-runner';
import { YosysOptions } from '../../yosys-types';
import { waitForSvg } from '../../netlist-diagram';

// ── Target registry ─────────────────────────────────────────────────────────

suite('Synthesis Targets — registry', () => {
	test('SYNTHESIS_TARGETS contains all expected target ids', () => {
		const expected = ['generic', 'ice40', 'ecp5', 'xilinx', 'gowin', 'quicklogic', 'sf2'];
		for (const id of expected) {
			assert.ok(SYNTHESIS_TARGETS.has(id), `Missing target: ${id}`);
		}
	});

	test('TARGET_IDS matches SYNTHESIS_TARGETS keys', () => {
		assert.strictEqual(TARGET_IDS.length, SYNTHESIS_TARGETS.size);
		for (const id of TARGET_IDS) {
			assert.ok(SYNTHESIS_TARGETS.has(id), `TARGET_IDS contains ${id} not in map`);
		}
	});

	test('every target has required fields', () => {
		for (const [id, target] of SYNTHESIS_TARGETS) {
			assert.ok(target.id, `${id} missing id`);
			assert.ok(target.label, `${id} missing label`);
			assert.ok(target.defaultScript.length > 0, `${id} has empty defaultScript`);
			assert.strictEqual(target.id, id, `${id} id mismatch`);
		}
	});

	test('generic target has null synthCommand', () => {
		const generic = SYNTHESIS_TARGETS.get('generic')!;
		assert.strictEqual(generic.synthCommand, null);
	});

	test('non-generic targets have a synthCommand', () => {
		for (const [id, target] of SYNTHESIS_TARGETS) {
			if (id === 'generic') { continue; }
			assert.ok(target.synthCommand, `${id} should have a synthCommand`);
			assert.ok(target.synthCommand!.startsWith('synth_'), `${id} synthCommand should start with synth_`);
		}
	});

	test('default scripts contain expected placeholders', () => {
		const placeholders = ['{files}', '{topModule}', '{outputDir}', '{outputBaseName}'];
		for (const [id, target] of SYNTHESIS_TARGETS) {
			for (const ph of placeholders) {
				assert.ok(
					target.defaultScript.includes(ph),
					`${id} default script missing placeholder ${ph}`
				);
			}
		}
	});

	test('default scripts write a JSON netlist and no Graphviz output', () => {
		// The diagram is rendered from the JSON netlist by netlistsvg, so
		// `write_json` is load-bearing for schematics and Yosys's `show` is not
		// used at all any more.
		for (const [id, target] of SYNTHESIS_TARGETS) {
			assert.ok(
				target.defaultScript.includes('write_json "{outputDir}/{outputBaseName}.json"'),
				`${id} default script must write the JSON netlist the diagram is rendered from`
			);
			assert.ok(
				!target.defaultScript.includes('show'),
				`${id} default script should not invoke Yosys \`show\``
			);
		}
	});

	test('non-generic default scripts contain their synth command', () => {
		for (const [id, target] of SYNTHESIS_TARGETS) {
			if (id === 'generic') { continue; }
			assert.ok(
				target.defaultScript.includes(target.synthCommand!),
				`${id} default script should contain ${target.synthCommand}`
			);
		}
	});

	// The legacy `abc` flow drives abc as an interactive co-process and can
	// deadlock against its prompt, timing out the whole run after 600s. Every
	// target that reaches abc must therefore go through abc9, which hands it a
	// script file and lets it exit. See "The abc9 flow" in synthesis-targets.ts.
	test('no default script uses the legacy abc flow', () => {
		for (const [id, target] of SYNTHESIS_TARGETS) {
			const legacyAbc = target.defaultScript
				.split('\n')
				.map(l => l.trim())
				.filter(l => /^abc\b/.test(l));
			assert.deepStrictEqual(
				legacyAbc, [],
				`${id} drives legacy abc directly (${legacyAbc.join('; ')}) — use abc9`
			);
			assert.ok(
				!/\babc\b(?!9)/.test(target.defaultScript.replace(/^#.*$/gm, '')),
				`${id} default script mentions plain abc outside a comment`
			);
		}
	});

	test('targets whose synth command reaches abc opt into abc9', () => {
		// xilinx takes -abc9; sf2 has no such option, so its map_luts step is
		// driven by hand. Either way the script must name abc9.
		for (const id of ['xilinx', 'sf2']) {
			const script = SYNTHESIS_TARGETS.get(id)!.defaultScript;
			assert.ok(script.includes('abc9'), `${id} should use abc9`);
		}
		assert.ok(
			SYNTHESIS_TARGETS.get('xilinx')!.defaultScript.includes('synth_xilinx -top {topModule} -abc9'),
			'xilinx should pass -abc9 to synth_xilinx'
		);
		const sf2 = SYNTHESIS_TARGETS.get('sf2')!.defaultScript;
		for (const line of ['synth_sf2 -top {topModule} -run :map_luts', 'abc9 -lut 4', 'synth_sf2 -top {topModule} -run map_cells:']) {
			assert.ok(sf2.includes(line), `sf2 script should contain "${line}"`);
		}
	});
});

// ── getTarget / getDefaultScript ────────────────────────────────────────────

suite('Synthesis Targets — getTarget / getDefaultScript', () => {
	test('getTarget returns correct target for known id', () => {
		const ecp5 = getTarget('ecp5');
		assert.strictEqual(ecp5.id, 'ecp5');
		assert.strictEqual(ecp5.synthCommand, 'synth_ecp5');
	});

	test('getTarget throws loudly for unknown id', () => {
		assert.throws(
			() => getTarget('nonexistent_target'),
			/Unknown synthesis target "nonexistent_target"/
		);
	});

	test('getDefaultScript returns non-empty script for known target', () => {
		const script = getDefaultScript('ice40');
		assert.ok(script.length > 0);
		assert.ok(script.includes('synth_ice40'));
	});

	test('getDefaultScript throws loudly for unknown target', () => {
		assert.throws(
			() => getDefaultScript('nonexistent'),
			/Unknown synthesis target "nonexistent"/
		);
	});
});

// ── resolveScript ───────────────────────────────────────────────────────────

suite('Synthesis Targets — resolveScript', () => {
	const simpleTemplate = `# Script
{files}
hierarchy -check -top {topModule}
synth_ecp5 -top {topModule}
write_verilog {outputDir}/{outputBaseName}_synth.v
write_json {outputDir}/{outputBaseName}.json
`;

	test('replaces {files} with read_verilog lines', () => {
		const result = resolveScript(simpleTemplate, {
			files: ['/path/to/a.v', '/path/to/b.v'],
			topModule: 'top',
			outputDir: '/out',
			outputBaseName: 'top',
		});
		// Paths are double-quoted so workspaces containing spaces work.
		assert.ok(result.includes('read_verilog "/path/to/a.v"'));
		assert.ok(result.includes('read_verilog "/path/to/b.v"'));
	});

	test('replaces {topModule} in all occurrences', () => {
		const result = resolveScript(simpleTemplate, {
			files: ['/a.v'],
			topModule: 'myDesign',
			outputDir: '/out',
			outputBaseName: 'myDesign',
		});
		// synth_ecp5 -top myDesign and hierarchy -check -top myDesign
		const matches = result.match(/myDesign/g);
		assert.ok(matches && matches.length >= 3, `Expected at least 3 occurrences of myDesign, got ${matches?.length}`);
	});

	test('replaces {outputDir} and {outputBaseName}', () => {
		const result = resolveScript(simpleTemplate, {
			files: ['/a.v'],
			topModule: 'top',
			outputDir: '/build/synth',
			outputBaseName: 'myMod',
		});
		assert.ok(result.includes('/build/synth/myMod_synth.v'));
		assert.ok(result.includes('/build/synth/myMod.json'));
	});

	test('handles empty files array', () => {
		const result = resolveScript('{files}\nhierarchy -top {topModule}', {
			files: [],
			topModule: 'top',
			outputDir: '/out',
			outputBaseName: 'top',
		});
		// {files} should resolve to empty string
		assert.ok(result.startsWith('\nhierarchy'));
	});

	test('handles single file', () => {
		const result = resolveScript('{files}', {
			files: ['/only.v'],
			topModule: 'top',
			outputDir: '/out',
			outputBaseName: 'top',
		});
		assert.strictEqual(result, 'read_verilog "/only.v"');
	});

	test('resolves a real default script without leftover placeholders', () => {
		const template = getDefaultScript('ecp5');
		const result = resolveScript(template, {
			files: ['/design/top.v', '/design/sub.v'],
			topModule: 'topEntity',
			outputDir: '/build',
			outputBaseName: 'topEntity',
		});
		assert.ok(!result.includes('{files}'), 'Should not contain {files}');
		assert.ok(!result.includes('{topModule}'), 'Should not contain {topModule}');
		assert.ok(!result.includes('{outputDir}'), 'Should not contain {outputDir}');
		assert.ok(!result.includes('{outputBaseName}'), 'Should not contain {outputBaseName}');
		assert.ok(result.includes('read_verilog "/design/top.v"'));
		assert.ok(result.includes('synth_ecp5 -top topEntity'));
	});
});

// ── computeScriptDiff ───────────────────────────────────────────────────────

suite('Synthesis Targets — computeScriptDiff', () => {
	test('identical scripts produce all equal lines', () => {
		const script = 'line1\nline2\nline3';
		const diff = computeScriptDiff(script, script);
		assert.ok(diff.every(l => l.kind === 'equal'), 'All lines should be equal');
		assert.strictEqual(diff.length, 3);
	});

	test('empty strings produce empty diff', () => {
		const diff = computeScriptDiff('', '');
		// Empty string splits to [''], so one equal line
		assert.strictEqual(diff.length, 1);
		assert.strictEqual(diff[0].kind, 'equal');
	});

	test('added lines are marked as added', () => {
		const diff = computeScriptDiff('line1\nline2', 'line1\nnew_line\nline2');
		const added = diff.filter(l => l.kind === 'added');
		assert.ok(added.length >= 1, 'Should have at least one added line');
		assert.ok(added.some(l => l.text === 'new_line'), 'new_line should be added');
	});

	test('removed lines are marked as removed', () => {
		const diff = computeScriptDiff('line1\nold_line\nline2', 'line1\nline2');
		const removed = diff.filter(l => l.kind === 'removed');
		assert.ok(removed.length >= 1, 'Should have at least one removed line');
		assert.ok(removed.some(l => l.text === 'old_line'), 'old_line should be removed');
	});

	test('replaced line shows as remove + add', () => {
		const diff = computeScriptDiff('aaa\nbbb\nccc', 'aaa\nBBB\nccc');
		const removed = diff.filter(l => l.kind === 'removed');
		const added = diff.filter(l => l.kind === 'added');
		assert.ok(removed.some(l => l.text === 'bbb'), 'bbb should be removed');
		assert.ok(added.some(l => l.text === 'BBB'), 'BBB should be added');
	});

	test('completely different scripts show all removes then adds', () => {
		const diff = computeScriptDiff('a\nb', 'x\ny');
		const removed = diff.filter(l => l.kind === 'removed');
		const added = diff.filter(l => l.kind === 'added');
		assert.strictEqual(removed.length, 2);
		assert.strictEqual(added.length, 2);
	});

	test('diff preserves line order', () => {
		const diff = computeScriptDiff('a\nb\nc', 'a\nX\nc');
		const texts = diff.map(l => l.text);
		// 'a' should come before 'X' and 'c'
		assert.ok(texts.indexOf('a') < texts.indexOf('X') || texts.indexOf('a') < texts.indexOf('c'));
	});

	test('diff of real script modification', () => {
		const defaultScript = 'read_verilog {files}\nhierarchy -check -top {topModule}\nsynth_ecp5 -top {topModule}\nstat -width';
		const customScript = 'read_verilog {files}\nhierarchy -check -top {topModule}\nsynth_ecp5 -top {topModule} -noccu2\nstat -width\nstat -json';
		const diff = computeScriptDiff(defaultScript, customScript);

		// Original synth line removed, modified one added
		const removed = diff.filter(l => l.kind === 'removed');
		const added = diff.filter(l => l.kind === 'added');
		assert.ok(removed.some(l => l.text === 'synth_ecp5 -top {topModule}'));
		assert.ok(added.some(l => l.text === 'synth_ecp5 -top {topModule} -noccu2'));
		assert.ok(added.some(l => l.text === 'stat -json'));
	});
});

// ── End-to-end Yosys support per target ─────────────────────────────────────
//
// Regression test that catches when a target listed in SYNTHESIS_TARGETS is
// not actually runnable on the installed Yosys — e.g. the `synth_intel`
// command exists but its tech library files were not packaged in the build.
// We exercise the *default script* (not just the bare `synth_*` command), so
// a target only counts as supported if the full pipeline the extension runs
// also succeeds (`hierarchy -check`, `check -assert`, `write_json`, etc.).
//
// A trivial AND gate is enough: every `synth_*` command must be able to map
// it. If it fails, the target is genuinely unsupported on this toolchain.
//
// Skips cleanly when `yosys` is not on PATH so unit-only test runs still pass.

const TARGET_PROBE_VERILOG = `
module synth_target_probe (
    input  a,
    input  b,
    output y
);
    assign y = a & b;
endmodule
`;

function yosysOnPath(): Promise<boolean> {
	return new Promise(resolve => {
		const proc = spawn('yosys', ['--version'], { timeout: 5000 });
		proc.on('error', () => resolve(false));
		proc.on('close', code => resolve(code === 0));
	});
}

suite('Synthesis Targets — installed Yosys supports every offered target', () => {
	let outputChannel: vscode.OutputChannel;
	let yosysRunner: YosysRunner;
	let tmpDir: string;

	suiteSetup(async function () {
		this.timeout(15_000);
		outputChannel = vscode.window.createOutputChannel('Test Synth Target Coverage');
		yosysRunner = new YosysRunner(outputChannel);
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-synth-target-'));
		await fs.writeFile(
			path.join(tmpDir, 'synth_target_probe.v'),
			TARGET_PROBE_VERILOG
		);
	});

	suiteTeardown(async () => {
		if (outputChannel) { outputChannel.dispose(); }
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	for (const targetId of TARGET_IDS) {
		test(`target "${targetId}" runs end-to-end on installed Yosys`, async function () {
			this.timeout(60_000);
			if (!(await yosysOnPath())) { this.skip(); return; }

			const outDir = path.join(tmpDir, targetId);
			await fs.mkdir(outDir, { recursive: true });

			const result = await yosysRunner.synthesize({
				workspaceRoot: tmpDir,
				outputDir: outDir,
				topModule: 'synth_target_probe',
				verilogPath: path.join(tmpDir, 'synth_target_probe.v'),
				targetFamily: targetId as YosysOptions['targetFamily'],
			});

			assert.ok(
				result.success,
				`Target "${targetId}" is offered in SYNTHESIS_TARGETS but the ` +
				`installed Yosys cannot run its default script.\n` +
				`Either remove the target from synthesis-targets.ts or fix the ` +
				`toolchain so the required tech files are present.\n` +
				`Errors:\n  ${result.errors.map(e => e.message).join('\n  ')}`
			);
			assert.ok(result.jsonPath, `Target "${targetId}" produced no JSON netlist`);
			const stat = await fs.stat(result.jsonPath!);
			assert.ok(stat.size > 0, `Target "${targetId}" produced an empty netlist`);

			// netlistsvg must cope with every target's technology-mapped
			// netlist — a target whose cells it cannot draw would silently
			// produce runs without diagrams.
			assert.ok(result.svgPath, `Target "${targetId}" reported no diagram path`);
			assert.ok(
				await waitForSvg(result.svgPath!),
				`Target "${targetId}" rendered no diagram at ${result.svgPath}`
			);
			const svg = await fs.readFile(result.svgPath!, 'utf8');
			assert.ok(svg.startsWith('<svg'), `Target "${targetId}" diagram is not an SVG`);
		});
	}
});
