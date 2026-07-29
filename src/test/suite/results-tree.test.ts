import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import { promises as fs } from 'fs';
import * as vscode from 'vscode';
import { SynthesisResultsTreeProvider } from '../../synthesis-results-tree';
import { ModuleSynthesisResult } from '../../yosys-types';
import { NextpnrResult } from '../../nextpnr-types';
import { loadRunModules } from '../../run-loader';

/**
 * Tests for how the sidebar tree renders PNR timing, utilization, and
 * critical-path data — this is what makes those numbers visible without
 * the user opening log files.
 */
suite('Synthesis Results Tree', () => {

	function makeModule(name: string, overrides: Partial<ModuleSynthesisResult> = {}): ModuleSynthesisResult {
		return {
			name,
			success: true,
			elapsedMs: 10,
			errors: [],
			statistics: { rawStats: '', cellCount: 10, wireCount: 20 },
			...overrides,
		};
	}

	test('shows placeholder when no results and no PNR', async () => {
		const tree = new SynthesisResultsTreeProvider();
		const roots = await tree.getChildren();
		assert.strictEqual(roots.length, 1);
		assert.match(String(roots[0].label), /No synthesis results/);
	});

	test('shows module rows only when PNR is absent', async () => {
		const tree = new SynthesisResultsTreeProvider();
		tree.refresh([makeModule('top'), makeModule('sub')]);
		const roots = await tree.getChildren();
		assert.strictEqual(roots.length, 2);
		assert.strictEqual(roots[0].label, 'top');
		assert.strictEqual(roots[1].label, 'sub');
	});

	test('appends Timing / Utilization / Critical Paths sections when PNR ran', async () => {
		const tree = new SynthesisResultsTreeProvider();
		const pnr: NextpnrResult = {
			success: true,
			output: '',
			warnings: [],
			errors: [],
			timing: {
				maxFrequency: 120.5,
				criticalPathDelay: 8.2,
				constraintsMet: true,
			},
			utilization: {
				luts: { used: 142, total: 24288 },
				registers: { used: 80, total: 24288 },
			},
			criticalPaths: [
				{
					from: 'clkA',
					to: 'clkA',
					totalDelay: 8.2,
					steps: [
						{ delay: 0.5, type: 'clk-to-q', fromCell: 'ff1', toCell: 'ff1' },
						{ delay: 7.7, type: 'routing',  fromCell: 'ff1', toCell: 'ff2', net: 'n1' },
					],
				},
			],
		};
		tree.refresh([makeModule('top')], pnr);

		const roots = await tree.getChildren();
		const labels = roots.map(r => String(r.label));
		assert.deepStrictEqual(
			labels,
			['top', 'Timing', 'Utilization', 'Critical Paths'],
			'module + three PNR sections'
		);
	});

	test('Timing section lists Fmax, critical-path delay, constraint status', async () => {
		const tree = new SynthesisResultsTreeProvider();
		const pnr: NextpnrResult = {
			success: true,
			output: '',
			warnings: [],
			errors: [],
			timing: {
				maxFrequency: 250.0,
				criticalPathDelay: 4.0,
				setupSlack: 1.2,
				constraintsMet: false,
			},
		};
		tree.refresh([], pnr);

		const roots = await tree.getChildren();
		const timingSection = roots.find(r => r.label === 'Timing');
		assert.ok(timingSection, 'Timing section present');

		const rows = await tree.getChildren(timingSection);
		const rowMap = new Map(rows.map(r => [String(r.label), String(r.description ?? '')]));
		assert.ok(rowMap.get('Max Frequency')?.includes('250.00 MHz'));
		assert.ok(rowMap.get('Critical Path Delay')?.includes('4.00 ns'));
		assert.ok(rowMap.get('Setup Slack')?.includes('1.200 ns'));
		assert.strictEqual(rowMap.get('Constraints'), 'MISSED');
	});

	test('Utilization section skips buckets without data', async () => {
		const tree = new SynthesisResultsTreeProvider();
		const pnr: NextpnrResult = {
			success: true,
			output: '',
			warnings: [],
			errors: [],
			utilization: {
				luts: { used: 5, total: 100 },
				// no registers / bram / dsp / io
			},
		};
		tree.refresh([], pnr);

		const utilSection = (await tree.getChildren())
			.find(r => r.label === 'Utilization');
		assert.ok(utilSection);
		const rows = await tree.getChildren(utilSection);
		assert.strictEqual(rows.length, 1, 'only the LUT row');
		assert.ok(String(rows[0].description).includes('5 / 100'));
	});

	test('Critical path is expandable into step rows', async () => {
		const tree = new SynthesisResultsTreeProvider();
		const pnr: NextpnrResult = {
			success: true,
			output: '',
			warnings: [],
			errors: [],
			criticalPaths: [
				{
					from: 'clkA',
					to: 'clkA',
					totalDelay: 5.5,
					steps: [
						{ delay: 0.5, type: 'clk-to-q' },
						{ delay: 5.0, type: 'routing', fromCell: 'a', toCell: 'b', net: 'wireX' },
					],
				},
			],
		};
		tree.refresh([], pnr);

		const cpSection = (await tree.getChildren())
			.find(r => r.label === 'Critical Paths');
		assert.ok(cpSection);

		const pathRows = await tree.getChildren(cpSection);
		assert.strictEqual(pathRows.length, 1);
		assert.ok(String(pathRows[0].description).includes('5.50 ns'));

		const stepRows = await tree.getChildren(pathRows[0]);
		assert.strictEqual(stepRows.length, 2);
		assert.match(String(stepRows[0].label), /clk-to-q/);
		assert.match(String(stepRows[1].label), /routing/);
	});

	test('module rows expand into the components they instantiate', async () => {
		// A hierarchical netlist on disk is what makes drill-down possible: the
		// parent's diagram draws sub-components as boxes, and these rows are how
		// you get inside those boxes.
		const dir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-tree-hier-'));
		try {
			const netlistPath = path.join(dir, 'top.json');
			await fs.writeFile(netlistPath, JSON.stringify({
				modules: {
					leaf: { ports: { i: {} }, cells: { n: { type: '$not' } } },
					mid: {
						ports: { i: {} },
						cells: { l: { type: 'leaf' }, a: { type: '$add' } },
					},
					top: {
						attributes: { top: '00000000000000000000000000000001' },
						cells: { m: { type: 'mid' } },
					},
				},
			}), 'utf8');

			const tree = new SynthesisResultsTreeProvider();
			tree.refresh([makeModule('top', { diagramJsonPath: netlistPath })]);

			const [topRow] = await tree.getChildren();
			assert.strictEqual(
				topRow.collapsibleState,
				vscode.TreeItemCollapsibleState.Collapsed,
				'a module with sub-components must be expandable'
			);

			// top → mid (plus the cell-type breakdown rows, which come after)
			const topChildren = await tree.getChildren(topRow);
			const mid = topChildren.find(c => c.label === 'mid');
			assert.ok(mid, `expected a "mid" row, got: ${topChildren.map(c => c.label)}`);
			assert.strictEqual(
				mid.collapsibleState,
				vscode.TreeItemCollapsibleState.Collapsed,
				'mid instantiates leaf, so it expands further'
			);
			assert.strictEqual(
				mid.command?.command,
				'clash-toolkit.viewComponentDiagram',
				'clicking a component opens its diagram'
			);

			// mid → leaf: the hierarchy is walkable to any depth.
			const midChildren = await tree.getChildren(mid);
			assert.deepStrictEqual(midChildren.map(c => c.label), ['leaf']);
			assert.strictEqual(
				midChildren[0].collapsibleState,
				vscode.TreeItemCollapsibleState.None,
				'leaf has no sub-components'
			);
		} finally {
			await fs.rm(dir, { recursive: true, force: true });
		}
	});

	test('out-of-context results nest by their component graph', async () => {
		// Each component was synthesized standalone (flattened netlists, no
		// hierarchy to read), so the graph recorded on the results is what makes
		// the view read the same as a whole-design run.
		const tree = new SynthesisResultsTreeProvider();
		tree.refresh([
			makeModule('accum', { outOfContext: true }),
			makeModule('mult_unsigned', { outOfContext: true }),
			makeModule('pipelined_sum', {
				outOfContext: true,
				subComponents: ['mult_unsigned'],
			}),
			makeModule('top_entity', {
				outOfContext: true,
				subComponents: ['accum', 'pipelined_sum'],
			}),
		]);

		// Only the top is a root: everything else is claimed as a child.
		const roots = await tree.getChildren();
		assert.deepStrictEqual(roots.map(r => String(r.label)), ['top_entity']);

		const topChildren = await tree.getChildren(roots[0]);
		const moduleRows = topChildren.filter(c => /^synthesisModule/.test(String(c.contextValue)));
		assert.deepStrictEqual(
			moduleRows.map(c => String(c.label)),
			['accum', 'pipelined_sum'],
			'direct sub-components appear under the top, in graph order'
		);

		// Nesting continues, and each level keeps its own statistics.
		const pipelined = moduleRows.find(c => c.label === 'pipelined_sum')!;
		const deep = await tree.getChildren(pipelined);
		assert.ok(
			deep.some(c => c.label === 'mult_unsigned'),
			`expected mult_unsigned under pipelined_sum, got ${deep.map(c => c.label)}`
		);
		assert.match(String(pipelined.description), /10 cells/);
	});

	test('out-of-context rows say so, on the row and in the tooltip', async () => {
		const tree = new SynthesisResultsTreeProvider();
		tree.refresh([makeModule('accum', { outOfContext: true })]);
		const [row] = await tree.getChildren();
		assert.match(String(row.description), /out of context/);
		const tooltip = String((row.tooltip as vscode.MarkdownString).value);
		// The tooltip must name the three reasons these figures are not
		// comparable with a whole-design run, not just "synthesized alone".
		assert.match(tooltip, /no technology mapping/);
		assert.match(tooltip, /synthesisTarget/);
		assert.match(tooltip, /include this component's descendants/);
		assert.match(tooltip, /optimized against its parent/);
	});

	test('whole-design rows carry no out-of-context marker', async () => {
		const tree = new SynthesisResultsTreeProvider();
		tree.refresh([makeModule('top')]);
		const [row] = await tree.getChildren();
		assert.doesNotMatch(String(row.description), /out of context/);
	});

	test('a cyclic component graph falls back to a flat list', async () => {
		// Defensive: a malformed graph must neither hang the view nor empty it.
		const tree = new SynthesisResultsTreeProvider();
		tree.refresh([
			makeModule('a', { subComponents: ['b'] }),
			makeModule('b', { subComponents: ['a'] }),
		]);
		const roots = await tree.getChildren();
		assert.deepStrictEqual(roots.map(r => String(r.label)), ['a', 'b']);
		// Expanding still terminates: the ancestor guard stops the recursion.
		const aChildren = await tree.getChildren(roots[0]);
		assert.deepStrictEqual(
			aChildren.filter(c => /^synthesisModule/.test(String(c.contextValue)))
				.map(c => String(c.label)),
			['b'],
		);
	});

	test('a module with no netlist has no component rows', async () => {
		const tree = new SynthesisResultsTreeProvider();
		tree.refresh([makeModule('top')]);
		const [topRow] = await tree.getChildren();
		const children = await tree.getChildren(topRow);
		assert.strictEqual(
			children.some(c => c.contextValue === 'subComponent-diagram'),
			false
		);
	});

	test('re-running synthesis clears any stale PNR sections', async () => {
		const tree = new SynthesisResultsTreeProvider();
		tree.refresh([makeModule('top')], {
			success: true, output: '', warnings: [], errors: [],
			timing: { maxFrequency: 100, constraintsMet: true },
		});
		// First pass: PNR section is there.
		assert.ok((await tree.getChildren())
			.some(r => r.label === 'Timing'));

		// User re-runs synthesis → refresh without PNR result.
		tree.refresh([makeModule('top')]);
		const roots = await tree.getChildren();
		assert.strictEqual(
			roots.some(r => r.label === 'Timing'),
			false,
			'PNR section should be cleared'
		);
	});
});

/**
 * The component graph of a per-module run is persisted to
 * `per-module/hierarchy.json`, because each component's netlist has been
 * flattened and can no longer be asked what it instantiates. Without that file
 * the Run History view could only show a flat list.
 */
suite('Run Loader — per-module hierarchy', () => {
	let runRoot: string;

	async function makeRun(hierarchy?: unknown): Promise<string> {
		const root = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-run-hier-'));
		const perModule = path.join(root, '03-yosys', 'per-module');
		for (const name of ['top_entity', 'accum']) {
			const dir = path.join(perModule, name);
			await fs.mkdir(dir, { recursive: true });
			await fs.writeFile(
				path.join(dir, 'stats.json'),
				JSON.stringify({ design: { num_cells: 5, num_wires: 7 } }),
				'utf8'
			);
		}
		if (hierarchy) {
			await fs.writeFile(
				path.join(perModule, 'hierarchy.json'),
				JSON.stringify(hierarchy),
				'utf8'
			);
		}
		return root;
	}

	teardown(async () => {
		if (runRoot) { await fs.rm(runRoot, { recursive: true, force: true }); }
	});

	test('restores the hierarchy and the out-of-context flag', async () => {
		runRoot = await makeRun({
			top: 'top_entity',
			outOfContext: true,
			components: { top_entity: ['accum'], accum: [] },
		});

		const { modules, topModule } = await loadRunModules(runRoot);
		assert.strictEqual(topModule, 'top_entity');

		const top = modules.find(m => m.name === 'top_entity');
		const accum = modules.find(m => m.name === 'accum');
		assert.ok(top && accum, 'both components should load');
		assert.deepStrictEqual(top.subComponents, ['accum']);
		assert.deepStrictEqual(accum.subComponents, []);
		assert.strictEqual(top.outOfContext, true);
		assert.strictEqual(accum.outOfContext, true);
		// Statistics still come from each component's own stats.json.
		assert.strictEqual(top.statistics?.cellCount, 5);
	});

	test('components named in the graph but absent on disk are dropped', async () => {
		runRoot = await makeRun({
			top: 'top_entity',
			outOfContext: true,
			components: { top_entity: ['accum', 'deleted_module'], accum: [] },
		});
		const { modules } = await loadRunModules(runRoot);
		assert.deepStrictEqual(
			modules.find(m => m.name === 'top_entity')?.subComponents,
			['accum'],
			'a child with no output directory cannot be shown'
		);
	});

	test('runs recorded before hierarchy.json existed still load, flat', async () => {
		runRoot = await makeRun();
		const { modules } = await loadRunModules(runRoot);
		assert.strictEqual(modules.length, 2);
		for (const m of modules) {
			assert.strictEqual(m.subComponents, undefined);
			assert.strictEqual(m.outOfContext, undefined);
		}
	});
});
