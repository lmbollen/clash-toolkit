import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import { promises as fs } from 'fs';
import * as vscode from 'vscode';
import {
	CLASH_IGNORE_ENTRY,
	DecisionStore,
	GITIGNORE_DECISION_KEY,
	GitignoreDecision,
	mentionsClashDir,
	offerGitignoreEntry,
	withClashIgnored,
} from '../../gitignore';

/**
 * The extension writes `.clash/` into the user's repository, so it offers to
 * ignore it — once. These tests cover the paths that must never reach a prompt:
 * asking again after an answer, or asking about a file that has already said
 * something about `.clash`, are both ways of being a nuisance in someone's repo.
 */
suite('Gitignore Offer', () => {
	let outputChannel: vscode.OutputChannel;
	let tmpDir: string;
	let counter = 0;

	suiteSetup(async () => {
		outputChannel = vscode.window.createOutputChannel('Test Gitignore');
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-gitignore-'));
	});

	suiteTeardown(async () => {
		outputChannel.dispose();
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	/** An in-memory stand-in for `context.workspaceState`. */
	function store(initial?: GitignoreDecision): DecisionStore & { value?: GitignoreDecision } {
		return {
			value: initial,
			get(key: string) {
				assert.strictEqual(key, GITIGNORE_DECISION_KEY);
				return this.value;
			},
			async update(key: string, value: GitignoreDecision | undefined) {
				assert.strictEqual(key, GITIGNORE_DECISION_KEY);
				this.value = value;
			},
		};
	}

	/** A workspace directory, optionally containing a `.gitignore`. */
	async function workspace(gitignore?: string): Promise<string> {
		const dir = path.join(tmpDir, `ws${counter++}`);
		await fs.mkdir(dir, { recursive: true });
		if (gitignore !== undefined) {
			await fs.writeFile(path.join(dir, '.gitignore'), gitignore);
		}
		return dir;
	}

	suite('recognising an existing entry', () => {
		test('matches the forms git accepts for the same directory', () => {
			for (const line of ['.clash', '.clash/', '/.clash', '/.clash/', '**/.clash', '  .clash/  ']) {
				assert.ok(mentionsClashDir(`node_modules\n${line}\ndist\n`), `should match: ${line}`);
			}
		});

		// Appending an ignore rule under an explicit un-ignore would quietly
		// reverse what the file says.
		test('a negated entry counts as the file having an opinion', () => {
			assert.ok(mentionsClashDir('!.clash/\n'));
		});

		test('does not match a comment, a different path, or a longer name', () => {
			assert.ok(!mentionsClashDir('# .clash\n'));
			assert.ok(!mentionsClashDir('.clash-cache/\n'));
			assert.ok(!mentionsClashDir('build/.clash\n'));
			assert.ok(!mentionsClashDir(''));
		});
	});

	suite('what gets written', () => {
		test('appends the entry under a comment, separated from what was there', () => {
			assert.strictEqual(
				withClashIgnored('dist/\nnode_modules/\n'),
				'dist/\nnode_modules/\n\n'
				+ '# Clash Toolkit: generated Verilog, netlists, diagrams and run history\n'
				+ '.clash/\n',
			);
		});

		test('does not open with a stray blank line in an empty file', () => {
			assert.ok(!withClashIgnored('').startsWith('\n'));
			assert.ok(withClashIgnored('').endsWith(`${CLASH_IGNORE_ENTRY}\n`));
		});

		test('a file with no trailing newline still gets one', () => {
			assert.ok(withClashIgnored('dist/').startsWith('dist/\n\n#'));
		});

		test('the result is recognised, so the offer is not made twice', () => {
			assert.ok(mentionsClashDir(withClashIgnored('dist/\n')));
		});
	});

	suite('when not to ask', () => {
		test('a workspace with no .gitignore is left alone', async () => {
			const dir = await workspace();
			const state = store();
			assert.strictEqual(await offerGitignoreEntry(state, dir, outputChannel), 'skipped');
			assert.strictEqual(state.value, undefined, 'nothing to record');
			await assert.rejects(() => fs.access(path.join(dir, '.gitignore')), 'no .gitignore is created');
		});

		test('a .gitignore that already covers .clash is left alone', async () => {
			const dir = await workspace('dist/\n.clash/\n');
			assert.strictEqual(
				await offerGitignoreEntry(store(), dir, outputChannel),
				'skipped',
			);
		});

		test('a workspace that answered before is not asked again', async () => {
			const dir = await workspace('dist/\n');
			for (const answer of ['added', 'declined'] as GitignoreDecision[]) {
				const state = store(answer);
				assert.strictEqual(
					await offerGitignoreEntry(state, dir, outputChannel),
					'skipped',
				);
				assert.strictEqual(state.value, answer, 'the recorded answer is untouched');
			}
			// The declined workspace's file is still as the user left it.
			assert.strictEqual(await fs.readFile(path.join(dir, '.gitignore'), 'utf8'), 'dist/\n');
		});
	});
});
