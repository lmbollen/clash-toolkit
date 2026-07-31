import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import { promises as fs } from 'fs';
import * as vscode from 'vscode';
import { RunModuleNode } from '../../run-history-tree';

/**
 * Inspecting a design means opening one diagram after another — down a
 * hierarchy, or across a run's modules. Each one must replace the last rather
 * than leave a tab behind, or a few minutes of clicking buries the editor.
 */
suite('Diagram Tabs', () => {
	let tmpDir: string;

	const svg =
		'<svg xmlns="http://www.w3.org/2000/svg" width="8" height="8">'
		+ '<rect width="8" height="8"/></svg>';

	suiteSetup(async () => {
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-diagram-tabs-'));
	});

	suiteTeardown(async () => {
		await vscode.commands.executeCommand('workbench.action.closeAllEditors');
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	setup(async () => {
		await vscode.commands.executeCommand('workbench.action.closeAllEditors');
	});

	/** A history row carrying a real SVG, as the tree builds them. */
	async function diagramNode(name: string): Promise<RunModuleNode> {
		const svgPath = path.join(tmpDir, `${name}.svg`);
		await fs.writeFile(svgPath, svg);
		return new RunModuleNode(name, tmpDir, svgPath, [], undefined);
	}

	/** Tabs settle a tick after the open command resolves. */
	async function openTabs(): Promise<readonly vscode.Tab[]> {
		for (let i = 0; i < 20; i++) {
			const tabs = vscode.window.tabGroups.all.flatMap(g => g.tabs);
			if (tabs.length > 0) { return tabs; }
			await new Promise(r => setTimeout(r, 50));
		}
		return vscode.window.tabGroups.all.flatMap(g => g.tabs);
	}

	test('opening one diagram after another leaves a single tab', async function () {
		this.timeout(20000);

		for (const name of ['top_entity', 'accum', 'adder']) {
			await vscode.commands.executeCommand(
				'clash-toolkit.openHistoryDiagram',
				await diagramNode(name),
			);
		}

		const tabs = await openTabs();
		assert.strictEqual(tabs.length, 1, `expected one tab, got: ${tabs.map(t => t.label).join(', ')}`);
		assert.strictEqual(tabs[0].label, 'adder.svg', 'the last diagram opened is the one shown');
	});

	// Pinning is how the editor is told to keep something; a diagram the user
	// pinned must survive the next one being opened.
	test('a pinned diagram is kept and the next opens alongside it', async function () {
		this.timeout(20000);

		await vscode.commands.executeCommand(
			'clash-toolkit.openHistoryDiagram',
			await diagramNode('keep_me'),
		);
		await openTabs();
		await vscode.commands.executeCommand('workbench.action.pinEditor');

		await vscode.commands.executeCommand(
			'clash-toolkit.openHistoryDiagram',
			await diagramNode('next_one'),
		);

		let labels: string[] = [];
		for (let i = 0; i < 20; i++) {
			labels = vscode.window.tabGroups.all.flatMap(g => g.tabs).map(t => t.label);
			if (labels.length > 1) { break; }
			await new Promise(r => setTimeout(r, 50));
		}
		assert.deepStrictEqual(labels.sort(), ['keep_me.svg', 'next_one.svg']);
	});
});
