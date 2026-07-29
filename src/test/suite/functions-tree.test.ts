import * as assert from 'assert';
import * as vscode from 'vscode';
import { HaskellFunctionsTreeProvider } from '../../haskell-functions-tree';
import { FunctionInfo } from '../../types';

/**
 * The Haskell Functions view has to distinguish four situations that all used to
 * look like "an empty list": HLS is missing, HLS is still working, analysis is
 * running, and the file genuinely has no functions.
 */
suite('Haskell Functions Tree — HLS states', () => {
	function makeFunction(name: string, mono: boolean): FunctionInfo {
		return {
			name,
			range: new vscode.Range(0, 0, 0, 0),
			typeSignature: 'Unsigned 8 -> Unsigned 8',
			isMonomorphic: mono,
			filePath: '/tmp/Example.hs',
			moduleName: 'Example',
		};
	}

	test('with no file open, invites the user to open one', () => {
		const tree = new HaskellFunctionsTreeProvider();
		const [row] = tree.getChildren() as unknown as vscode.TreeItem[];
		assert.match(String(row.label), /Open a Haskell file/);
	});

	test('a missing Haskell extension is reported, with an install action', () => {
		const tree = new HaskellFunctionsTreeProvider();
		tree.setHlsUnavailable('extension-missing', '/tmp/Example.hs');
		const [row] = tree.getChildren() as unknown as vscode.TreeItem[];

		assert.match(String(row.label), /HLS unavailable/);
		assert.match(String(row.label), /not installed/);
		assert.strictEqual(
			row.command?.command,
			'clash-toolkit.installHaskellExtension',
			'the row must offer the fix, not just state the problem'
		);
	});

	test('an inactive Haskell extension is reported, with a retry action', () => {
		const tree = new HaskellFunctionsTreeProvider();
		tree.setHlsUnavailable('extension-inactive', '/tmp/Example.hs');
		const [row] = tree.getChildren() as unknown as vscode.TreeItem[];

		assert.match(String(row.label), /did not start/);
		assert.strictEqual(row.command?.command, 'clash-toolkit.refreshHaskellFunctions');
	});

	test('no symbols yet is reported as a state, not as zero functions', () => {
		const tree = new HaskellFunctionsTreeProvider();
		tree.setNoSymbolsYet('/tmp/Example.hs');
		const [row] = tree.getChildren() as unknown as vscode.TreeItem[];

		assert.match(String(row.label), /No symbols from HLS yet/);
		assert.match(String(row.description), /still be loading/);
		// The tooltip must own up to the ambiguity rather than blaming HLS.
		assert.match(
			String((row.tooltip as vscode.MarkdownString).value),
			/indistinguishable from a file which really\s+defines no top-level functions/,
		);
		assert.strictEqual(tree.isAwaitingHls(), true);
	});

	test('a "nothing yet" view is not confused with an empty result', () => {
		const tree = new HaskellFunctionsTreeProvider();
		tree.setNoSymbolsYet('/tmp/Example.hs');
		const waiting = tree.getChildren() as unknown as vscode.TreeItem[];
		assert.strictEqual(waiting.length, 1);
		assert.ok(!waiting.some(r => /Monomorphic/.test(String(r.label))));

		// Once HLS answers, the sections appear — an actual verdict, and the view
		// stops asking to be re-checked.
		tree.refresh([], '/tmp/Example.hs');
		assert.strictEqual(tree.isAwaitingHls(), false);
		const answered = tree.getChildren() as unknown as vscode.TreeItem[];
		assert.deepStrictEqual(
			answered.map(r => String(r.label)),
			['Monomorphic (0)', 'Polymorphic (0)'],
		);
	});

	test('results are split into monomorphic and polymorphic sections', () => {
		const tree = new HaskellFunctionsTreeProvider();
		tree.refresh(
			[makeFunction('topEntity', true), makeFunction('genericAdd', false)],
			'/tmp/Example.hs',
		);
		const roots = tree.getChildren() as unknown as vscode.TreeItem[];
		assert.deepStrictEqual(
			roots.map(r => String(r.label)),
			['Monomorphic (1)', 'Polymorphic (1)'],
		);

		const mono = tree.getChildren(roots[0] as never);
		assert.deepStrictEqual(mono.map(n => String((n as vscode.TreeItem).label)), ['topEntity']);
	});

	test('analysis in progress shows a spinner, not an empty list', () => {
		const tree = new HaskellFunctionsTreeProvider();
		tree.setLoading('/tmp/Example.hs');
		const [row] = tree.getChildren() as unknown as vscode.TreeItem[];
		assert.match(String(row.label), /Analyzing/);
	});
});
