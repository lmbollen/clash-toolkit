import * as assert from 'assert';
import * as path from 'path';
import * as os from 'os';
import { promises as fs } from 'fs';
import * as vscode from 'vscode';
import {
	defaultSkinPath,
	instantiatedModules,
	renderNetlistSvg,
	selectTopModule,
	svgPathForNetlist,
	topModuleOf,
} from '../../netlist-renderer';
import {
	ensureSubDiagram,
	fireDiagramRender,
	subComponentsOf,
	subDiagramPath,
	waitForSvg,
} from '../../netlist-diagram';

/**
 * Tests for netlistsvg-based diagram rendering — the replacement for the old
 * Yosys `show` + Graphviz `dot` pipeline.
 *
 * The netlists below are hand-written in the shape `write_json` emits, so these
 * tests need no Yosys on PATH; the end-to-end "does every synthesis target
 * render" check lives in synthesis-targets.test.ts.
 */

/** Two modules: a top instantiating `sub`, plus an `$and` cell and a port. */
const HIERARCHICAL_NETLIST = {
	creator: 'test',
	modules: {
		sub: {
			attributes: { src: 'sub.v:1' },
			ports: {
				i: { direction: 'input', bits: [2] },
				o: { direction: 'output', bits: [3] },
			},
			cells: {
				buf: {
					type: '$not',
					port_directions: { A: 'input', Y: 'output' },
					connections: { A: [2], Y: [3] },
				},
			},
		},
		top: {
			attributes: { top: '00000000000000000000000000000001', src: 'top.v:1' },
			ports: {
				a: { direction: 'input', bits: [2] },
				b: { direction: 'input', bits: [3] },
				y: { direction: 'output', bits: [4] },
			},
			cells: {
				gate: {
					type: '$and',
					port_directions: { A: 'input', B: 'input', Y: 'output' },
					connections: { A: [2], B: [3], Y: [5] },
				},
				inst: {
					type: 'sub',
					port_directions: { i: 'input', o: 'output' },
					connections: { i: [5], o: [4] },
				},
			},
		},
	},
};

suite('Netlist Renderer — pure helpers', () => {
	test('svgPathForNetlist swaps the .json suffix for .svg', () => {
		assert.strictEqual(svgPathForNetlist('/o/top_entity.json'), '/o/top_entity.svg');
		// A netlist without the suffix still gets one appended rather than
		// overwriting the netlist itself.
		assert.strictEqual(svgPathForNetlist('/o/top_entity'), '/o/top_entity.svg');
	});

	test('the bundled netlistsvg skin exists on disk', async () => {
		const skin = defaultSkinPath();
		const contents = await fs.readFile(skin, 'utf8');
		assert.ok(contents.includes('<svg'), `${skin} does not look like an SVG skin`);
	});

	test('selectTopModule moves the top attribute to the named module', () => {
		const netlist = JSON.parse(JSON.stringify(HIERARCHICAL_NETLIST));
		assert.strictEqual(selectTopModule(netlist, 'sub'), true);
		assert.strictEqual(netlist.modules.sub.attributes.top, '1');
		assert.ok(
			!('top' in netlist.modules.top.attributes),
			'the previous top must lose the attribute, else the choice is ambiguous'
		);
		// Other attributes are left alone.
		assert.strictEqual(netlist.modules.sub.attributes.src, 'sub.v:1');
	});

	test('selectTopModule reports an unknown module instead of throwing', () => {
		const netlist = JSON.parse(JSON.stringify(HIERARCHICAL_NETLIST));
		assert.strictEqual(selectTopModule(netlist, 'nonexistent'), false);
		// The design top is untouched, so rendering still has a module to draw.
		assert.ok('top' in netlist.modules.top.attributes);
	});

	test('topModuleOf honours the top attribute, not file order', () => {
		// `sub` comes first in the file; `top` carries the attribute.
		assert.strictEqual(topModuleOf(HIERARCHICAL_NETLIST), 'top');
		assert.strictEqual(topModuleOf({ modules: { only: {} } }), 'only');
		assert.strictEqual(topModuleOf({ modules: {} }), undefined);
	});
});

suite('Netlist Renderer — component hierarchy', () => {
	test('instantiatedModules finds the sub-components of a module', () => {
		assert.deepStrictEqual(instantiatedModules(HIERARCHICAL_NETLIST, 'top'), ['sub']);
		// A leaf module instantiates nothing drillable.
		assert.deepStrictEqual(instantiatedModules(HIERARCHICAL_NETLIST, 'sub'), []);
	});

	test('primitives are not sub-components', () => {
		// `top` also contains an $and cell; only module-typed cells count.
		assert.ok(!instantiatedModules(HIERARCHICAL_NETLIST, 'top').includes('$and'));
	});

	test('black-box library cells are excluded', () => {
		// This is what keeps a tech-mapped netlist from listing LUT4 / TRELLIS_FF
		// as components to drill into — they have no internals to draw.
		const netlist = {
			modules: {
				LUT4: {
					attributes: { blackbox: '00000000000000000000000000000001' },
					ports: { A: { direction: 'input', bits: [2] } },
				},
				real_sub: {
					ports: { i: { direction: 'input', bits: [2] } },
					cells: { c: { type: '$not', connections: { A: [2], Y: [3] } } },
				},
				top: {
					attributes: { top: '00000000000000000000000000000001' },
					cells: {
						l: { type: 'LUT4', connections: { A: [2] } },
						s: { type: 'real_sub', connections: { i: [2] } },
					},
				},
			},
		};
		assert.deepStrictEqual(instantiatedModules(netlist, 'top'), ['real_sub']);
	});

	test('a module instantiated many times is listed once', () => {
		const netlist = {
			modules: {
				leaf: { ports: { i: {} }, cells: { c: { type: '$not' } } },
				top: {
					cells: {
						a: { type: 'leaf' },
						b: { type: 'leaf' },
						c: { type: 'leaf' },
					},
				},
			},
		};
		assert.deepStrictEqual(instantiatedModules(netlist, 'top'), ['leaf']);
	});

	test('unknown module names yield no sub-components', () => {
		assert.deepStrictEqual(instantiatedModules(HIERARCHICAL_NETLIST, 'nope'), []);
	});
});

suite('Netlist Renderer — rendering', () => {
	let tmpDir: string;

	suiteSetup(async () => {
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-netlistsvg-'));
	});

	suiteTeardown(async () => {
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	async function writeNetlist(name: string, netlist: unknown): Promise<string> {
		const p = path.join(tmpDir, `${name}.json`);
		await fs.writeFile(p, JSON.stringify(netlist), 'utf8');
		return p;
	}

	test('renders a hierarchical netlist to an SVG', async function () {
		this.timeout(30_000);
		const netlistPath = await writeNetlist('hierarchy', HIERARCHICAL_NETLIST);
		const svgPath = await renderNetlistSvg({ netlistPath });

		assert.strictEqual(svgPath, svgPathForNetlist(netlistPath));
		const svg = await fs.readFile(svgPath, 'utf8');
		assert.ok(svg.startsWith('<svg'), 'output should be an SVG document');
		// The top module's own contents: the instance box is labelled with its
		// module name, and its ports are drawn.
		assert.ok(svg.includes('>sub<'), 'sub-module instance should be drawn as a box');
		assert.ok(svg.includes('>a<') && svg.includes('>y<'), 'top ports should be drawn');
	});

	test('topModule selects which module is drawn', async function () {
		this.timeout(30_000);
		const netlistPath = await writeNetlist('select', HIERARCHICAL_NETLIST);
		const svgPath = path.join(tmpDir, 'select-sub.svg');
		await renderNetlistSvg({ netlistPath, svgPath, topModule: 'sub' });

		const svg = await fs.readFile(svgPath, 'utf8');
		// `sub`'s own ports, not the top's.
		assert.ok(svg.includes('>i<') && svg.includes('>o<'), 'sub ports should be drawn');
		assert.ok(!svg.includes('>sub<'), 'sub should be the diagram, not a box inside it');
	});

	test('a missing netlist is an error, not a silent empty SVG', async () => {
		await assert.rejects(
			() => renderNetlistSvg({ netlistPath: path.join(tmpDir, 'absent.json') }),
			/ENOENT|no such file/i
		);
	});

	test('an unparseable netlist reports the file that is broken', async function () {
		this.timeout(15_000);
		const netlistPath = path.join(tmpDir, 'broken.json');
		await fs.writeFile(netlistPath, '{ this is not json', 'utf8');
		await assert.rejects(
			() => renderNetlistSvg({ netlistPath }),
			/broken\.json is not valid JSON/
		);
	});

	test('a netlist with no modules is rejected', async function () {
		this.timeout(15_000);
		const netlistPath = await writeNetlist('empty', { creator: 'test', modules: {} });
		await assert.rejects(
			() => renderNetlistSvg({ netlistPath }),
			/contains no modules/
		);
	});
});

suite('Netlist Renderer — background rendering', () => {
	let tmpDir: string;
	let outputChannel: vscode.OutputChannel;

	suiteSetup(async () => {
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-netlistsvg-bg-'));
		outputChannel = vscode.window.createOutputChannel('Test Netlist Diagram');
	});

	suiteTeardown(async () => {
		outputChannel?.dispose();
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	test('fireDiagramRender renders out-of-process and waitForSvg joins it', async function () {
		// Covers the child-process path used by every synthesis run: the render
		// must complete without the caller having awaited it directly.
		this.timeout(60_000);
		const netlistPath = path.join(tmpDir, 'background.json');
		await fs.writeFile(netlistPath, JSON.stringify(HIERARCHICAL_NETLIST), 'utf8');

		const svgPath = fireDiagramRender(netlistPath, outputChannel, 'top');
		assert.strictEqual(svgPath, svgPathForNetlist(netlistPath));

		assert.strictEqual(await waitForSvg(svgPath), true, 'no SVG was produced');
		const svg = await fs.readFile(svgPath, 'utf8');
		assert.ok(svg.startsWith('<svg'));
	});

	test('a netlist the renderer rejects yields no diagram instead of throwing', async function () {
		// The child exits non-zero; the caller must see "no diagram", and the
		// reason must reach the output channel rather than an unhandled
		// rejection.
		this.timeout(60_000);
		const netlistPath = path.join(tmpDir, 'unrenderable.json');
		await fs.writeFile(netlistPath, '{ "modules": {} }', 'utf8');

		const svgPath = fireDiagramRender(netlistPath, outputChannel, 'top');
		assert.strictEqual(await waitForSvg(svgPath), false);
	});

	test('a missing netlist yields no diagram instead of throwing', async function () {
		this.timeout(30_000);
		const netlistPath = path.join(tmpDir, 'never-written.json');
		const svgPath = fireDiagramRender(netlistPath, outputChannel);
		assert.strictEqual(await waitForSvg(svgPath), false);
	});

	test('waitForSvg reports false for a path nothing is rendering', async () => {
		assert.strictEqual(
			await waitForSvg(path.join(tmpDir, 'unrelated.svg')),
			false
		);
	});
});

suite('Netlist Renderer — sub-component diagrams', () => {
	let tmpDir: string;
	let outputChannel: vscode.OutputChannel;
	let netlistPath: string;

	suiteSetup(async () => {
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-subdiagram-'));
		outputChannel = vscode.window.createOutputChannel('Test Sub Diagrams');
		netlistPath = path.join(tmpDir, 'top.json');
		await fs.writeFile(netlistPath, JSON.stringify(HIERARCHICAL_NETLIST), 'utf8');
	});

	suiteTeardown(async () => {
		outputChannel?.dispose();
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	test('subComponentsOf reads the hierarchy from a netlist on disk', async () => {
		assert.deepStrictEqual(await subComponentsOf(netlistPath, 'top'), ['sub']);
		assert.deepStrictEqual(await subComponentsOf(netlistPath, 'sub'), []);
	});

	test('a missing netlist yields no sub-components instead of throwing', async () => {
		assert.deepStrictEqual(
			await subComponentsOf(path.join(tmpDir, 'absent.json'), 'top'),
			[]
		);
	});

	test('sub-component diagrams live in diagrams/ beside the netlist', () => {
		assert.strictEqual(
			subDiagramPath(netlistPath, 'sub'),
			path.join(tmpDir, 'diagrams', 'sub.svg')
		);
	});

	test('module names that are not filename-safe get a distinct file each', () => {
		// `$paramod` copies and escaped Verilog ids would otherwise collide.
		const a = subDiagramPath(netlistPath, '$paramod$aaa\\CCU2C');
		const b = subDiagramPath(netlistPath, '$paramod$bbb\\CCU2C');
		assert.notStrictEqual(a, b, 'different modules must not share a file');
		for (const p of [a, b]) {
			assert.ok(
				!/[$\\]/.test(path.basename(p)),
				`${path.basename(p)} should be filename-safe`
			);
		}
	});

	test('ensureSubDiagram renders the sub-component on demand', async function () {
		this.timeout(60_000);
		const svgPath = await ensureSubDiagram(netlistPath, 'sub', outputChannel);
		assert.strictEqual(svgPath, subDiagramPath(netlistPath, 'sub'));

		const svg = await fs.readFile(svgPath!, 'utf8');
		assert.ok(svg.startsWith('<svg'));
		// It is `sub` that was drawn: its own ports, and not a box named `sub`.
		assert.ok(svg.includes('>i<') && svg.includes('>o<'));
		assert.ok(!svg.includes('>sub<'));
	});

	test('ensureSubDiagram reuses an already-rendered diagram', async function () {
		this.timeout(60_000);
		const first = await ensureSubDiagram(netlistPath, 'sub', outputChannel);
		const stat = await fs.stat(first!);
		const second = await ensureSubDiagram(netlistPath, 'sub', outputChannel);
		assert.strictEqual(second, first);
		assert.strictEqual(
			(await fs.stat(second!)).mtimeMs,
			stat.mtimeMs,
			'the SVG should be served from disk, not re-rendered'
		);
	});

	test('ensureSubDiagram reports failure for a module that is not there', async function () {
		this.timeout(60_000);
		const broken = path.join(tmpDir, 'nomodules.json');
		await fs.writeFile(broken, JSON.stringify({ modules: {} }), 'utf8');
		assert.strictEqual(
			await ensureSubDiagram(broken, 'whatever', outputChannel),
			undefined
		);
	});
});
