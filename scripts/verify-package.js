#!/usr/bin/env node
/**
 * Guard the contents of the published .vsix.
 *
 * `vsce` honours `.vscodeignore` and nothing else — not `.gitignore` — so a
 * directory that is untracked is still packaged unless `.vscodeignore` says
 * otherwise. Version 0.3.0 shipped `.clash/debug.log`, the extension's own debug
 * log, complete with absolute paths from the machine that built it.
 *
 * This runs from `vscode:prepublish`, so it gates `vsce package` and
 * `vsce publish` alike — including a bare `npx vsce publish`, which bypasses any
 * check wired into an npm script of our own.
 *
 * It fails on two kinds of mistake:
 *   - something in FORBIDDEN is about to ship (sources, fixtures, logs, docs);
 *   - something in REQUIRED is missing, which is how an over-eager
 *     `.vscodeignore` rule would silently break the extension at runtime.
 */
'use strict';

const { execFileSync } = require('child_process');
const path = require('path');

/**
 * Paths that must never appear in the package, as `[matcher, why]`. A string
 * matcher is a prefix; a RegExp is tested against the whole path.
 */
const FORBIDDEN = [
	['.clash/', 'the extension\'s own debug logs, containing local paths'],
	['src/', 'TypeScript sources; the compiled output in out/ is what ships'],
	['test-project/', 'the Haskell test fixture project'],
	['book/', 'documentation sources; the book lives in the repository'],
	['.vscode-test/', 'downloaded VS Code builds used by the test runner'],
	['.direnv/', 'direnv/Nix state'],
	['.git/', 'repository metadata'],
	[/\.log$/, 'log files'],
	[/\.vsix$/, 'previously packaged extensions'],
	[/^out\/test\//, 'compiled tests'],
	[/\.js\.map$/, 'source maps'],
	[/\.d\.ts$/, 'type declarations'],
];

/** Paths that must be present, because the extension cannot run without them. */
const REQUIRED = [
	'package.json',
	'out/extension.js',
	'out/netlist-renderer.js',
	'resources/clash-icon.svg',
	// The netlistsvg skin the diagram renderer draws with: easy to lose to an
	// `.vscodeignore` rule aimed at the package's TypeScript sources.
	'node_modules/netlistsvg/lib/default.svg',
	'node_modules/netlistsvg/built/index.js',
	'node_modules/elkjs/lib/main.js',
];

function packagedFiles() {
	// Run vsce's own entry script under this Node rather than the `.bin`
	// shim: on Windows that shim is `vsce.cmd`, and since the fix for
	// CVE-2024-27980 Node refuses to spawn a `.cmd` without `shell: true`
	// (EINVAL) — which would block `vsce package` and `vsce publish` outright.
	const vsce = require.resolve('@vscode/vsce/vsce');
	// `vsce ls` does not itself run `vscode:prepublish`, but set a marker anyway
	// so a future version that does cannot recurse forever.
	const out = execFileSync(process.execPath, [vsce, 'ls'], {
		cwd: path.join(__dirname, '..'),
		encoding: 'utf8',
		maxBuffer: 64 * 1024 * 1024,
		env: { ...process.env, CLASH_VERIFY_PACKAGE: '1' },
	});
	return out.split('\n').map(l => l.trim()).filter(Boolean);
}

function main() {
	if (process.env.CLASH_VERIFY_PACKAGE === '1') {
		return; // re-entered from our own `vsce ls`
	}

	const files = packagedFiles();
	const problems = [];

	for (const [matcher, why] of FORBIDDEN) {
		const hits = files.filter(f =>
			typeof matcher === 'string' ? f.startsWith(matcher) : matcher.test(f)
		);
		if (hits.length > 0) {
			problems.push(
				`  ✗ ${hits.length} file(s) matching ${matcher} would be packaged (${why}):\n` +
				hits.slice(0, 5).map(f => `      ${f}`).join('\n') +
				(hits.length > 5 ? `\n      … and ${hits.length - 5} more` : '')
			);
		}
	}

	for (const required of REQUIRED) {
		if (!files.includes(required)) {
			problems.push(`  ✗ ${required} is missing from the package`);
		}
	}

	if (problems.length > 0) {
		console.error(
			`\nverify-package: the .vsix contents are wrong (${files.length} files listed).\n` +
			'Fix .vscodeignore, then re-run:\n\n' +
			problems.join('\n') + '\n'
		);
		process.exit(1);
	}

	console.log(`verify-package: ${files.length} files, contents look right.`);
}

main();
