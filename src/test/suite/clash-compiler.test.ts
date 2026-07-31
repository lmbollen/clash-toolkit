import * as assert from 'assert';
import * as vscode from 'vscode';
import { ClashCompiler, resolveCabalJobs } from '../../clash-compiler';

suite('Clash Compiler Test Suite', () => {
	let outputChannel: vscode.OutputChannel;
	let compiler: ClashCompiler;

	suiteSetup(() => {
		outputChannel = vscode.window.createOutputChannel('Test Clash Compiler');
		compiler = new ClashCompiler(outputChannel);
	});

	suiteTeardown(() => {
		if (outputChannel) {
			outputChannel.dispose();
		}
	});

	test('Should parse diagnostics from Clash error output', () => {
		const output =
			"/tmp/Wrapper.hs:5:1: error:\n" +
			"    Not in scope: Foo.bar\n" +
			"    Module Foo does not export bar\n";

		const diagnostics = compiler.parseDiagnostics(output, '/tmp/Wrapper.hs');
		assert.ok(diagnostics.length > 0, 'Should produce at least one diagnostic');
		assert.strictEqual(diagnostics[0].severity, vscode.DiagnosticSeverity.Error);
	});

	test('Should parse warnings from Clash output', () => {
		const output =
			"/tmp/Test.hs:10:5: warning:\n" +
			"    Unused import of Foo\n";

		const diagnostics = compiler.parseDiagnostics(output, '/tmp/Test.hs');
		assert.ok(diagnostics.length > 0, 'Should produce at least one diagnostic');
		assert.strictEqual(diagnostics[0].severity, vscode.DiagnosticSeverity.Warning);
	});

	test('Should return empty diagnostics for clean output', () => {
		const output = 'Compiling...\nDone.\n';
		const diagnostics = compiler.parseDiagnostics(output, '/tmp/Wrapper.hs');
		assert.strictEqual(diagnostics.length, 0);
	});

	test('Should only create diagnostics for the wrapper file', () => {
		const output =
			'/other/file.hs:1:1: error:\n' +
			'    Some error\n';

		const diagnostics = compiler.parseDiagnostics(output, '/tmp/Wrapper.hs');
		assert.strictEqual(diagnostics.length, 0, 'Should not create diagnostics for other files');
	});

	suite('cabal --jobs', () => {
		test('auto means one job per core', () => {
			assert.strictEqual(resolveCabalJobs('auto'), '$ncpus');
			assert.strictEqual(resolveCabalJobs('AUTO'), '$ncpus');
			assert.strictEqual(resolveCabalJobs('$ncpus'), '$ncpus');
		});

		test('a job count is passed through, as a number or a string', () => {
			assert.strictEqual(resolveCabalJobs(4), '4');
			assert.strictEqual(resolveCabalJobs('4'), '4');
			assert.strictEqual(resolveCabalJobs(1), '1');
		});

		// A bad setting must not become a bad argument: cabal would refuse to
		// start and the user would see a flag error instead of their build.
		test('nonsense falls back to cabal\'s own default', () => {
			assert.strictEqual(resolveCabalJobs(undefined), undefined);
			assert.strictEqual(resolveCabalJobs(null), undefined);
			assert.strictEqual(resolveCabalJobs(''), undefined);
			assert.strictEqual(resolveCabalJobs(0), undefined);
			assert.strictEqual(resolveCabalJobs(-2), undefined);
			assert.strictEqual(resolveCabalJobs(2.5), undefined);
			assert.strictEqual(resolveCabalJobs('many'), undefined);
		});
	});
});
