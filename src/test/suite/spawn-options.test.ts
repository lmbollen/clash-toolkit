import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Every external tool the extension spawns must pass `windowsHide: true`.
 *
 * Node defaults it to `false`, and the extension host is a GUI process with no
 * console attached — so on Windows each console-mode child (yosys, nextpnr,
 * cabal, the OSS CAD Suite extractor) gets a freshly allocated console window
 * that flashes on screen. It is per *process*, so the damage scales: probing
 * iCE40 package support spawns one nextpnr per candidate package concurrently,
 * and `yosysJobs: auto` runs up to eight synthesis jobs at once.
 *
 * This is checked against the sources rather than by observing a spawn, because
 * the failure is only visible on Windows and only to a human watching the
 * screen. Nothing else would catch a new spawn site that forgets it.
 */
suite('Spawn Options Test Suite', () => {
	const repoRoot = path.resolve(__dirname, '../../..');

	/** Modules that spawn external tools. `netlist-diagram` is excluded: it
	 *  forks Electron-as-node, a GUI binary that allocates no console. */
	const modules = [
		'clash-compiler.ts',
		'nextpnr-runner.ts',
		'toolchain.ts',
		'yosys-runner.ts',
		'tool-provider.ts',
	];

	/**
	 * Whether a spawn call starting at `lines[i]` is covered — either the
	 * inline options object names `windowsHide`, or the options argument is an
	 * identifier whose declaration elsewhere in the file does.
	 */
	function isCovered(source: string, lines: string[], i: number): boolean {
		const window = lines.slice(i, i + 14).join('\n');
		if (window.includes('windowsHide')) { return true; }
		// e.g. `spawn(cmd, args, spawnOpts)` — resolve the variable.
		const byVar = /spawn\([^;]*?,\s*([A-Za-z_$][\w$]*)\s*\)/.exec(window);
		if (byVar) {
			const decl = new RegExp(
				`(const|let|var)\\s+${byVar[1]}\\b[\\s\\S]{0,600}?windowsHide`
			);
			return decl.test(source);
		}
		return false;
	}

	for (const mod of modules) {
		test(`${mod}: every spawn passes windowsHide`, () => {
			const file = path.join(repoRoot, 'src', mod);
			const source = fs.readFileSync(file, 'utf8');
			const lines = source.split('\n');

			const uncovered: string[] = [];
			let sites = 0;
			lines.forEach((line, i) => {
				if (!/(?:^|[^.\w])spawn\(/.test(line)) { return; }
				if (line.trim().startsWith('*') || line.trim().startsWith('//')) { return; }
				sites++;
				if (!isCovered(source, lines, i)) {
					uncovered.push(`${mod}:${i + 1}: ${line.trim()}`);
				}
			});

			assert.ok(sites > 0, `${mod} should contain at least one spawn call`);
			assert.deepStrictEqual(
				uncovered, [],
				'spawn sites missing windowsHide:\n  ' + uncovered.join('\n  ')
			);
		});
	}

	test('the guard actually detects a missing windowsHide', () => {
		// Guards that silently pass are worse than no guard, so prove this one
		// fails on a spawn call that omits the option.
		const bad = [
			'const proc = spawn(cmd, args, {',
			'    cwd,',
			'    env: process.env',
			'});',
		];
		assert.strictEqual(isCovered(bad.join('\n'), bad, 0), false);

		const good = [...bad];
		good[2] = '    env: process.env, windowsHide: true';
		assert.strictEqual(isCovered(good.join('\n'), good, 0), true);
	});
});
