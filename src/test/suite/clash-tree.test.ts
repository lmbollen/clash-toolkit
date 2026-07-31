import * as assert from 'assert';
import * as vscode from 'vscode';
import { ClashTreeProvider } from '../../clash-tree';
import { HaskellFunctionsTreeProvider } from '../../haskell-functions-tree';
import { SynthesisResultsTreeProvider } from '../../synthesis-results-tree';
import { RunHistoryTreeProvider } from '../../run-history-tree';
import { ModuleSynthesisResult } from '../../yosys-types';

/**
 * Tests for the one-view sidebar: three sections whose rows still come from the
 * three providers that owned them when they were separate views.  What matters
 * here is routing — a row must reach the same provider again when it is
 * expanded, including for node types that both Results and History use.
 */
suite('Clash Tree', () => {

	function makeTree() {
		const functions = new HaskellFunctionsTreeProvider();
		const results = new SynthesisResultsTreeProvider();
		const history = new RunHistoryTreeProvider();
		return {
			functions, results, history,
			tree: new ClashTreeProvider(functions, results, history),
		};
	}

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

	async function labels(tree: ClashTreeProvider, element?: unknown): Promise<string[]> {
		const children = await tree.getChildren(element);
		const items = await Promise.all(children.map(c => tree.getTreeItem(c)));
		return items.map(i => String(i.label));
	}

	test('root shows the three sections', async () => {
		const { tree } = makeTree();
		assert.deepStrictEqual(await labels(tree), ['Functions', 'Results', 'History']);
	});

	test('each section delegates to its own provider', async () => {
		const { tree, results } = makeTree();
		results.refresh([makeModule('top')]);
		const [functionsSection, resultsSection, historySection] = await tree.getChildren();

		assert.deepStrictEqual(
			await labels(tree, functionsSection),
			['Open a Haskell file to see functions'],
		);
		assert.deepStrictEqual(await labels(tree, resultsSection), ['top']);
		// No workspace root was set, so history reports that rather than runs.
		assert.deepStrictEqual(await labels(tree, historySection), ['No workspace open']);
	});

	test('a section row expands through the provider it came from', async () => {
		const { tree, results } = makeTree();
		results.refresh([makeModule('top', {
			statistics: {
				rawStats: '',
				cellCount: 3,
				wireCount: 4,
				cellTypes: new Map([['$add', 2], ['$dff', 1]]),
			},
		})]);

		const [, resultsSection] = await tree.getChildren();
		const [module] = await tree.getChildren(resultsSection);
		assert.deepStrictEqual(await labels(tree, module), ['$add', '$dff']);
	});

	test('function rows keep their own type, so the title-bar buttons still see them', async () => {
		const { tree, functions } = makeTree();
		functions.refresh([{
			name: 'topEntity',
			typeSignature: 'Signal System Int -> Signal System Int',
			isMonomorphic: true,
			filePath: '/tmp/Example.hs',
			moduleName: 'Example',
			range: new vscode.Range(0, 0, 0, 0),
		}], '/tmp/Example.hs');

		const [functionsSection] = await tree.getChildren();
		const [monoSection] = await tree.getChildren(functionsSection);
		const [fn] = await tree.getChildren(monoSection) as { info?: { name: string } }[];
		assert.strictEqual(fn.info?.name, 'topEntity');
	});

	test('a sub-provider change event refreshes only its own section', async () => {
		const { tree, history } = makeTree();
		const changed: string[] = [];
		tree.onDidChangeTreeData(node => {
			changed.push(String((node as vscode.TreeItem).label));
		});

		history.refresh();
		assert.deepStrictEqual(changed, ['History']);
	});

	test('section status replaces the per-view banner', async () => {
		const { tree } = makeTree();
		assert.strictEqual(tree.sectionStatus('results'), undefined);

		tree.setSectionStatus('results', 'Synthesis — topEntity (P&R running…)');
		assert.match(tree.sectionStatus('results')!, /P&R running/);

		const [, resultsSection] = await tree.getChildren();
		const item = await tree.getTreeItem(resultsSection);
		assert.match(String(item.description), /P&R running/);

		tree.setSectionStatus('results');
		assert.strictEqual(tree.sectionStatus('results'), undefined);
	});
});
