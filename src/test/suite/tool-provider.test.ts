import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { ManagedToolchain, extractionDest, pathKeyOf, suitePathSubdirs } from '../../tool-provider';
import { describeExitCode } from '../../toolchain';

/**
 * Unit tests for the managed-toolchain resolver. These cover the pure,
 * offline behaviour (which commands it can supply, and how it resolves names
 * when nothing is installed) — the actual download is not exercised here.
 */
suite('ManagedToolchain Test Suite', () => {
	let outputChannel: vscode.OutputChannel;
	let toolchain: ManagedToolchain;

	suiteSetup(() => {
		outputChannel = vscode.window.createOutputChannel('Test Managed Toolchain');
		// A throwaway global-storage dir guarantees "not installed" state, and
		// an in-memory globalState stub stands in for the real Memento.
		const fakeStorage = path.join(os.tmpdir(), `clash-tp-test-${process.pid}`);
		const store = new Map<string, unknown>();
		const fakeContext = {
			globalStorageUri: vscode.Uri.file(fakeStorage),
			globalState: {
				get: (key: string, dflt?: unknown) => (store.has(key) ? store.get(key) : dflt),
				update: (key: string, value: unknown) => { store.set(key, value); return Promise.resolve(); },
				keys: () => [...store.keys()],
			},
		} as unknown as vscode.ExtensionContext;
		toolchain = new ManagedToolchain(fakeContext, outputChannel);
	});

	suiteTeardown(() => {
		outputChannel?.dispose();
	});

	test('canProvide recognises the bundled tools', () => {
		assert.strictEqual(toolchain.canProvide('yosys'), true);
		assert.strictEqual(toolchain.canProvide('nextpnr-ecp5'), true);
		assert.strictEqual(toolchain.canProvide('nextpnr-ice40'), true);
		assert.strictEqual(toolchain.canProvide('nextpnr-himbaechel'), true);
	});

	test('canProvide rejects tools outside the suite', () => {
		// cabal/ghc are Haskell tooling, not part of the OSS CAD Suite.
		assert.strictEqual(toolchain.canProvide('cabal'), false);
		assert.strictEqual(toolchain.canProvide('ghc'), false);
		// Diagrams are rendered by the bundled netlistsvg library, so Graphviz
		// is no longer a tool the extension provides or needs.
		assert.strictEqual(toolchain.canProvide('dot'), false);
	});

	test('is not installed against a throwaway storage dir', () => {
		assert.strictEqual(toolchain.isInstalled(), false);
	});

	test('resolve returns the name unchanged when nothing is installed', () => {
		// A providable tool that is (almost certainly) not on the test PATH and
		// has no managed copy resolves back to its bare name.
		assert.strictEqual(
			toolchain.resolve('nextpnr-himbaechel'),
			'nextpnr-himbaechel'
		);
		// A command this provider can't supply is always passed through.
		assert.strictEqual(
			toolchain.resolve('some-unknown-command-xyz'),
			'some-unknown-command-xyz'
		);
	});

	test('describeStatus reports the not-installed state', () => {
		const status = toolchain.describeStatus();
		assert.ok(
			status.includes('not installed') || status.includes('unavailable'),
			`unexpected status line: ${status}`
		);
	});

	// ---------------------------------------------------------------
	// Spawn environment. The Windows rules can't be exercised by running
	// them here, so the platform-dependent decisions are pure functions
	// taking the platform explicitly.
	// ---------------------------------------------------------------

	test('Windows needs lib/ on PATH as well as bin/', () => {
		// The Windows suite ships no wrapper scripts and keeps its runtime DLLs
		// in lib/; without that directory on PATH every binary fails to load.
		assert.deepStrictEqual(suitePathSubdirs('win32'), ['bin', 'lib']);
		// bin/ comes first so the executables still win over anything in lib/.
		assert.strictEqual(suitePathSubdirs('win32')[0], 'bin');
	});

	test('Linux and macOS need only bin/ on PATH', () => {
		assert.deepStrictEqual(suitePathSubdirs('linux'), ['bin']);
		assert.deepStrictEqual(suitePathSubdirs('darwin'), ['bin']);
	});

	test('pathKeyOf reuses the existing casing instead of adding a duplicate', () => {
		// Windows spells it `Path`; writing `PATH` alongside it would leave the
		// child with two entries and no defined winner.
		// eslint-disable-next-line @typescript-eslint/naming-convention -- Windows' own spelling is the point
		assert.strictEqual(pathKeyOf({ Path: 'C:\\Windows' }), 'Path');
		assert.strictEqual(pathKeyOf({ PATH: '/usr/bin' }), 'PATH');
		assert.strictEqual(pathKeyOf({ path: '/usr/bin' }), 'path');
		// Nothing to reuse — fall back to the POSIX spelling.
		assert.strictEqual(pathKeyOf({ HOME: '/home/x' }), 'PATH');
	});

	test('spawnEnv prepends the managed directories for a managed binary', () => {
		const binDir = path.join(toolchain.location, 'bin');
		const env = toolchain.spawnEnv(path.join(binDir, 'yosys'));
		const entries = (env[pathKeyOf(env)] || '').split(path.delimiter);
		const expected = suitePathSubdirs().map(sub => path.join(toolchain.location, sub));
		assert.deepStrictEqual(
			entries.slice(0, expected.length),
			expected,
			'managed directories should lead PATH'
		);
		assert.ok(
			entries.length > expected.length,
			'the inherited PATH should still follow'
		);
	});

	test('spawnEnv leaves the environment alone for unmanaged commands', () => {
		// A tool the user provides themselves must never see a managed install
		// injected ahead of it.
		assert.strictEqual(toolchain.spawnEnv('yosys'), process.env);
		assert.strictEqual(toolchain.spawnEnv(undefined), process.env);
		assert.strictEqual(toolchain.spawnEnv('/usr/local/bin/yosys'), process.env);
	});

	// ---------------------------------------------------------------
	// Extraction destination. The suite's deepest entry is ~132 characters
	// below its own root, so on Windows a globalStorage path of any real
	// length puts it past MAX_PATH unless the extractor is opted out.
	// ---------------------------------------------------------------

	test('Windows extracts through a long-path destination', () => {
		assert.strictEqual(
			extractionDest('C:\\Users\\someone\\AppData\\Roaming\\Code\\User\\globalStorage', 'win32'),
			'\\\\?\\C:\\Users\\someone\\AppData\\Roaming\\Code\\User\\globalStorage'
		);
		// UNC roots spell the prefix differently; getting this wrong would
		// point the extractor at a path that does not exist.
		assert.strictEqual(
			extractionDest('\\\\server\\share\\storage', 'win32'),
			'\\\\?\\UNC\\server\\share\\storage'
		);
		// Idempotent, so a destination that already carries the prefix is not
		// mangled into `\\?\\\?\…`.
		assert.strictEqual(extractionDest('\\\\?\\C:\\storage', 'win32'), '\\\\?\\C:\\storage');
	});

	test('Linux and macOS extract to the plain path', () => {
		// The prefix is Windows-only syntax; elsewhere it would be a literal
		// directory name.
		assert.strictEqual(extractionDest('/home/x/.config/Code/storage', 'linux'), '/home/x/.config/Code/storage');
		assert.strictEqual(extractionDest('/Users/x/Library/storage', 'darwin'), '/Users/x/Library/storage');
	});

	// ---------------------------------------------------------------
	// Exit-code diagnostics
	// ---------------------------------------------------------------

	test('describeExitCode explains Windows loader failures', () => {
		// 0xC0000135 — what a managed binary returns when its DLLs aren't on
		// PATH. It prints nothing, so the code is the only evidence.
		assert.match(describeExitCode(0xC0000135, 'win32'), /DLL was not found/);
		assert.match(describeExitCode(0xC000007B, 'win32'), /mismatch/);
		assert.match(describeExitCode(0xC0000142, 'win32'), /initialise/);
	});

	test('describeExitCode stays quiet for ordinary and non-Windows exits', () => {
		assert.strictEqual(describeExitCode(1, 'win32'), '');
		assert.strictEqual(describeExitCode(null, 'win32'), '');
		assert.strictEqual(describeExitCode(0xC0000135, 'linux'), '');
	});
});
