import * as assert from 'assert';
import * as path from 'path';
import { promises as fs } from 'fs';
import * as vscode from 'vscode';

interface Manifest {
	contributes: {
		commands: { command: string; title: string; icon?: string }[];
		menus: Record<string, { command: string; when?: string; group?: string }[]>;
		views: Record<string, { id: string; name: string }[]>;
		viewsContainers: Record<string, { id: string }[]>;
	};
}

/**
 * The manifest and the implementation have to agree.
 *
 * Nothing in TypeScript connects `contributes.commands` to
 * `registerCommand`, or a menu's `view ==` clause to the view that gets
 * created: a renamed view id or a command declared but never registered
 * compiles, packages, and only fails when a user clicks it.
 */
suite('Contributions', () => {
	let manifest: Manifest;
	let registered: string[];

	suiteSetup(async () => {
		const repoRoot = path.resolve(__dirname, '../../..');
		manifest = JSON.parse(await fs.readFile(path.join(repoRoot, 'package.json'), 'utf8'));
		registered = await vscode.commands.getCommands(true);
	});

	test('every declared command is registered', () => {
		const missing = manifest.contributes.commands
			.map(c => c.command)
			.filter(id => !registered.includes(id));
		assert.deepStrictEqual(missing, [], 'declared in package.json but never registered');
	});

	test('every menu entry points at a declared command', () => {
		const declared = new Set(manifest.contributes.commands.map(c => c.command));
		const undeclared: string[] = [];
		for (const [menu, entries] of Object.entries(manifest.contributes.menus)) {
			for (const entry of entries) {
				if (!declared.has(entry.command)) { undeclared.push(`${menu}: ${entry.command}`); }
			}
		}
		assert.deepStrictEqual(undeclared, []);
	});

	// The sidebar is one view; a `when` clause naming a view that no longer
	// exists silently hides every button on it.
	test('every menu `when` clause names a view that exists', () => {
		const viewIds = new Set(
			Object.values(manifest.contributes.views).flat().map(v => v.id)
		);
		const stale: string[] = [];
		for (const [menu, entries] of Object.entries(manifest.contributes.menus)) {
			for (const entry of entries) {
				for (const [, id] of (entry.when ?? '').matchAll(/view\s*==\s*([\w.-]+)/g)) {
					if (!viewIds.has(id)) { stale.push(`${menu}: ${entry.command} → ${id}`); }
				}
			}
		}
		assert.deepStrictEqual(stale, []);
	});

	test('the view lives in a declared container', () => {
		const containers = new Set(
			manifest.contributes.viewsContainers.activitybar.map(c => c.id)
		);
		for (const container of Object.keys(manifest.contributes.views)) {
			assert.ok(containers.has(container), `no such container: ${container}`);
		}
	});

	/**
	 * Row actions that also work from the palette, because they act on the
	 * whole workspace rather than on the row they are attached to.
	 */
	const PALETTE_SAFE = new Set(['clash-toolkit.clearRunHistory']);

	// A command whose handler reads the tree item it was invoked on does
	// nothing from the palette, where there is no item to pass it.
	test('row actions that need their item are hidden from the palette', () => {
		const hidden = new Set(
			manifest.contributes.menus.commandPalette
				.filter(e => e.when === 'false')
				.map(e => e.command)
		);
		const exposed = (manifest.contributes.menus['view/item/context'] ?? [])
			.map(e => e.command)
			.filter(id => !hidden.has(id) && !PALETTE_SAFE.has(id));
		assert.deepStrictEqual(exposed, []);
	});

	// An inline action with no icon is invisible on the row it belongs to.
	test('every inline row action has an icon', () => {
		const byId = new Map(manifest.contributes.commands.map(c => [c.command, c]));
		const iconless = (manifest.contributes.menus['view/item/context'] ?? [])
			.filter(e => (e.group ?? '').startsWith('inline'))
			.map(e => e.command)
			.filter(id => !byId.get(id)?.icon);
		assert.deepStrictEqual(iconless, []);
	});
});
