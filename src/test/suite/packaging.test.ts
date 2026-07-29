import * as assert from 'assert';
import * as path from 'path';
import { promises as fs } from 'fs';

/**
 * Invariants of what gets published.
 *
 * `vsce` reads `.vscodeignore` and nothing else — not `.gitignore` — so an
 * untracked directory is still packaged unless this file excludes it. Version
 * 0.3.0 shipped `.clash/debug.log`, the extension's own debug log, with absolute
 * paths from the machine that built it.
 *
 * `scripts/verify-package.js` is the real gate (it inspects the actual file list
 * and runs from `vscode:prepublish`). These tests are the cheap half: they fail
 * in the normal test run if the rules that gate go missing, without spawning
 * vsce.
 */
suite('Packaging rules', () => {
	const repoRoot = path.resolve(__dirname, '../../..');
	let ignoreRules: string[];
	let packageJson: {
		scripts: Record<string, string>;
		dependencies: Record<string, string>;
	};

	suiteSetup(async () => {
		const raw = await fs.readFile(path.join(repoRoot, '.vscodeignore'), 'utf8');
		ignoreRules = raw
			.split('\n')
			.map(l => l.trim())
			.filter(l => l.length > 0 && !l.startsWith('#'));
		packageJson = JSON.parse(
			await fs.readFile(path.join(repoRoot, 'package.json'), 'utf8')
		);
	});

	test('.vscodeignore excludes extension state written into the repo', () => {
		// The regression that shipped in 0.3.0.
		assert.ok(
			ignoreRules.includes('.clash/**'),
			'.clash/** must stay excluded: it holds debug logs and synthesis runs '
			+ 'with local paths, and .gitignore does not apply to packaging'
		);
		assert.ok(ignoreRules.includes('*.log'), '*.log must stay excluded');
	});

	test('.vscodeignore excludes sources, fixtures and docs', () => {
		for (const rule of ['src/**', 'test-project/**', 'book/**', 'out/test/**']) {
			assert.ok(ignoreRules.includes(rule), `${rule} must stay excluded`);
		}
	});

	test('the package gate runs from vscode:prepublish', () => {
		// Wiring it here rather than into our own `package`/`publish` scripts is
		// deliberate: `npx vsce publish` bypasses those, but never prepublish.
		assert.match(
			packageJson.scripts['vscode:prepublish'],
			/verify-package/,
			'vscode:prepublish must run scripts/verify-package.js so that both '
			+ '`vsce package` and a bare `npx vsce publish` are gated'
		);
	});

	test('the diagram renderer is a runtime dependency, not a dev one', () => {
		// It ships inside the .vsix; in devDependencies it would be absent at
		// runtime and every diagram would fail.
		assert.ok(
			packageJson.dependencies.netlistsvg,
			'netlistsvg must be a runtime dependency'
		);
	});

	test('the gate script exists and is runnable', async () => {
		const gate = path.join(repoRoot, 'scripts', 'verify-package.js');
		const contents = await fs.readFile(gate, 'utf8');
		assert.match(contents, /\.clash\//, 'the gate must check for .clash/');
		assert.match(
			contents,
			/netlistsvg\/lib\/default\.svg/,
			'the gate must check the netlistsvg skin still ships'
		);
	});
});
