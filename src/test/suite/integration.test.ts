import * as assert from 'assert';
import * as path from 'path';
import * as vscode from 'vscode';
import { promises as fs } from 'fs';
import { CodeGenerator, GenerationConfig } from '../../code-generator';
import { ClashCompiler } from '../../clash-compiler';
import { YosysRunner } from '../../yosys-runner';
import { NextpnrRunner } from '../../nextpnr-runner';
import { ClashManifestParser, pnrTargetClock } from '../../clash-manifest-parser';
import { FunctionInfo } from '../../types';
import { getDefaultElaborationScript } from '../../synthesis-targets';
import { ensureSubDiagram, subComponentsOf, waitForSvg } from '../../netlist-diagram';


/**
 * End-to-end integration tests that run the full synthesis and PnR flow
 * on the test-project workspace.
 *
 * These tests require that cabal, clash-ghc, yosys, and nextpnr-ecp5
 * are all available in the environment.
 */
suite('Integration: Full Synthesis + PnR Flow', () => {
	let outputChannel: vscode.OutputChannel;
	let codeGenerator: CodeGenerator;
	let clashCompiler: ClashCompiler;
	let yosysRunner: YosysRunner;
	let nextpnrRunner: NextpnrRunner;
	let wsRoot: string;

	// Shared state across the ordered test steps
	let generatedModuleName: string;
	let synthProjectRoot: string;
	let cabalProjectDir: string | null;
	let verilogPath: string;
	let allVerilogFiles: string[] | undefined;
	let topModule: string;
	let yosysJsonPath: string;
	let manifestPath: string | undefined;

	/** The function we synthesize through the whole flow. */
	const testFunc: FunctionInfo = {
		name: 'topEntity',
		range: new vscode.Range(0, 0, 0, 0),
		typeSignature: 'Clock Dom50 -> Reset Dom50 -> Enable Dom50 -> Vec 8 (DSignal Dom50 0 (Unsigned 16)) -> DSignal Dom50 3 (Unsigned 16)',
		isMonomorphic: true,
		filePath: '', // set in suiteSetup
		moduleName: 'Example.Project',
	};

	suiteSetup(async function () {
		this.timeout(30000);

		outputChannel = vscode.window.createOutputChannel('Test Integration');
		codeGenerator = new CodeGenerator(outputChannel);
		clashCompiler = new ClashCompiler(outputChannel);
		yosysRunner = new YosysRunner(outputChannel);
		nextpnrRunner = new NextpnrRunner(outputChannel);

		const workspaceFolders = vscode.workspace.workspaceFolders;
		if (!workspaceFolders || workspaceFolders.length === 0) {
			throw new Error('No workspace folder — cannot run integration tests');
		}
		wsRoot = workspaceFolders[0].uri.fsPath;
		testFunc.filePath = path.join(wsRoot, 'src', 'Example', 'Project.hs');

		// Clean any stale synth project left by previous test suites so
		// leftover wrapper files don't cause compilation errors.
		const synthRoot = CodeGenerator.getSynthProjectRoot(wsRoot);
		await fs.rm(synthRoot, { recursive: true, force: true });
	});

	suiteTeardown(() => {
		if (outputChannel) {
			outputChannel.dispose();
		}
	});

	// ---------------------------------------------------------------
	// Step 1: Generate wrapper module
	// ---------------------------------------------------------------

	test('Step 1: Generate wrapper module', async function () {
		this.timeout(15000);

		const genConfig: GenerationConfig = {
			modulePrefix: 'ClashSynth_',
		};

		const result = await codeGenerator.generateWrapper(testFunc, genConfig, wsRoot);

		assert.ok(result.filePath, 'Should produce a wrapper file');
		assert.ok(result.moduleName, 'Should produce a module name');
		assert.ok(result.content.includes('topEntity'), 'Wrapper should contain topEntity');
		assert.ok(result.content.includes('import qualified Example.Project'), 'Wrapper should import user module');
		assert.ok(result.content.includes('Synthesize'), 'Wrapper should have Synthesize annotation');

		generatedModuleName = result.moduleName;

		// Verify the file was written
		const stat = await fs.stat(result.filePath);
		assert.ok(stat.isFile(), 'Wrapper file should exist on disk');
	});

	// ---------------------------------------------------------------
	// Step 2: Set up synthesis cabal project
	// ---------------------------------------------------------------

	test('Step 2: Ensure synthesis cabal project', async function () {
		this.timeout(15000);

		const synthInfo = await codeGenerator.ensureSynthProject(wsRoot, testFunc.filePath);

		synthProjectRoot = synthInfo.synthRoot;
		cabalProjectDir = synthInfo.cabalProjectDir;

		// Verify key files exist
		const cabalProjectFile = path.join(synthProjectRoot, 'cabal.project');
		const cabalFile = path.join(synthProjectRoot, 'clash-synth.cabal');
		const clashMain = path.join(synthProjectRoot, 'bin', 'Clash.hs');

		const [cpStat, cfStat, cmStat] = await Promise.all([
			fs.stat(cabalProjectFile),
			fs.stat(cabalFile),
			fs.stat(clashMain),
		]);

		assert.ok(cpStat.isFile(), 'cabal.project should exist');
		assert.ok(cfStat.isFile(), 'clash-synth.cabal should exist');
		assert.ok(cmStat.isFile(), 'bin/Clash.hs should exist');

		// Should have detected the user project
		assert.ok(cabalProjectDir, 'Should detect cabal project dir');

		// The .cabal file should depend on the user package
		const cabalContent = await fs.readFile(cabalFile, 'utf8');
		assert.ok(cabalContent.includes('simple'), 'Should depend on user package "simple"');
	});

	// ---------------------------------------------------------------
	// Step 3: Compile with Clash (Haskell → Verilog)
	// ---------------------------------------------------------------

	test('Step 3: Compile to Verilog with Clash', async function () {
		// This can take a very long time on a cold cabal build
		this.timeout(600_000);

		const projectDirs = CodeGenerator.getProjectDirectories(wsRoot, testFunc);

		const result = await clashCompiler.compileToVerilog(
			path.join(synthProjectRoot, 'src', `${generatedModuleName}.hs`),
			{
				workspaceRoot: wsRoot,
				outputDir: projectDirs.root,
				moduleName: generatedModuleName,
				hdlDir: projectDirs.verilog,
				synthProjectRoot,
				cabalProjectDir: cabalProjectDir ?? undefined,
			}
		);

		assert.ok(result.success, `Clash compilation should succeed.\nErrors: ${result.errors.join('\n')}`);
		assert.ok(result.verilogPath, 'Should produce a Verilog file');

		verilogPath = result.verilogPath!;
		allVerilogFiles = result.allVerilogFiles;
		manifestPath = result.manifest?.manifestPath;

		// Determine top module from manifest or filename
		if (result.manifest?.top_component?.name) {
			topModule = result.manifest.top_component.name;
		} else {
			topModule = path.basename(verilogPath, '.v');
		}

		// Verify the Verilog file exists and is non-empty
		const stat = await fs.stat(verilogPath);
		assert.ok(stat.size > 0, 'Verilog file should be non-empty');
	});

	// ---------------------------------------------------------------
	// Step 3b: Elaborate with Yosys (hierarchy + proc, no tech mapping)
	// ---------------------------------------------------------------

	test('Step 3b: Elaborate with Yosys', async function () {
		this.timeout(60_000);

		assert.ok(verilogPath, 'Step 3 must have produced a Verilog path');

		const projectDirs = CodeGenerator.getProjectDirectories(wsRoot, testFunc);
		const verilogInput = allVerilogFiles || verilogPath;

		// Elaboration uses a dedicated output dir so it doesn't collide with the
		// Step-4 synthesis artefacts that also live under projectDirs.yosys.
		const elabDir = path.join(projectDirs.yosys, 'elaborate');
		await fs.rm(elabDir, { recursive: true, force: true });

		const result = await yosysRunner.synthesize({
			workspaceRoot: wsRoot,
			outputDir: elabDir,
			topModule,
			verilogPath: verilogInput,
			targetFamily: 'elaborate',
			customScript: getDefaultElaborationScript(),
		});

		assert.ok(
			result.success,
			`Yosys elaboration should succeed.\nErrors: ${result.errors.map(e => e.message).join('\n')}`
		);
		assert.ok(result.jsonPath, 'Elaboration should produce a JSON netlist');

		const jsonStat = await fs.stat(result.jsonPath!);
		assert.ok(jsonStat.size > 0, 'Elaboration JSON should be non-empty');

		// Logic-depth sidecar should be written by the `tee -o ... ltp -noff` line.
		const logicDepthFile = path.join(elabDir, 'logic_depth.txt');
		const ldStat = await fs.stat(logicDepthFile);
		assert.ok(ldStat.size > 0, 'logic_depth.txt should be non-empty');

		const ltpText = await fs.readFile(logicDepthFile, 'utf8');
		assert.ok(
			/Longest topological path/i.test(ltpText),
			`logic_depth.txt should contain ltp header; got:\n${ltpText}`
		);

		// Elaboration produces generic word-level cells, never tech-mapped ones.
		const netlist = JSON.parse(await fs.readFile(result.jsonPath!, 'utf8')) as {
			modules?: Record<string, { cells?: Record<string, { type: string }> }>;
		};
		const allCellTypes = new Set<string>();
		for (const mod of Object.values(netlist.modules ?? {})) {
			for (const cell of Object.values(mod.cells ?? {})) {
				allCellTypes.add(cell.type);
			}
		}
		for (const t of allCellTypes) {
			assert.ok(
				!/^(LUT[0-9]|TRELLIS_|SB_LUT|DFF\b|FD\w*E\b)/.test(t),
				`Elaborated netlist should not contain tech-mapped cell "${t}"`
			);
		}

		// netlistsvg renders that same netlist into the schematic the Elaborate
		// command opens.
		assert.ok(result.svgPath, 'Elaboration should report a diagram path');
		assert.ok(
			await waitForSvg(result.svgPath!),
			`Elaboration rendered no diagram at ${result.svgPath}`
		);
		const svg = await fs.readFile(result.svgPath!, 'utf8');
		assert.ok(svg.startsWith('<svg'), 'Elaboration diagram should be an SVG');

		// Elaboration keeps the hierarchy, so the top module's sub-components
		// must be offered for drill-down and be renderable on demand. This is
		// the whole-design path — no out-of-context synthesis involved.
		const subs = await subComponentsOf(result.jsonPath!, topModule);
		if (subs.length > 0) {
			const child = subs[0];
			const childSvg = await ensureSubDiagram(result.jsonPath!, child, outputChannel);
			assert.ok(childSvg, `No diagram rendered for sub-component ${child}`);
			assert.ok(
				(await fs.readFile(childSvg!, 'utf8')).startsWith('<svg'),
				`${child} diagram should be an SVG`
			);
		}
	});

	// ---------------------------------------------------------------
	// Step 4: Synthesize with Yosys (Verilog → netlist JSON)
	// ---------------------------------------------------------------

	test('Step 4: Synthesize with Yosys', async function () {
		this.timeout(120_000);

		assert.ok(verilogPath, 'Step 3 must have produced a Verilog path');

		const projectDirs = CodeGenerator.getProjectDirectories(wsRoot, testFunc);
		const verilogInput = allVerilogFiles || verilogPath;

		// Whole-design synthesis — the same path the extension's P&R flow uses
		// (nextpnr needs a merged netlist).
		const result = await yosysRunner.synthesize({
			workspaceRoot: wsRoot,
			outputDir: projectDirs.yosys,
			topModule,
			verilogPath: verilogInput,
			targetFamily: 'ecp5',
		});

		assert.ok(result.success, `Yosys synthesis should succeed.\nErrors: ${result.errors.map(e => e.message).join('\n')}`);
		assert.ok(result.jsonPath, 'Should produce a JSON netlist');

		yosysJsonPath = result.jsonPath!;

		// Verify synthesis produced the JSON file
		const stat = await fs.stat(yosysJsonPath);
		assert.ok(stat.size > 0, 'JSON netlist should be non-empty');

		// Statistics should be populated
		if (result.statistics) {
			assert.ok(
				result.statistics.cellCount === undefined || result.statistics.cellCount >= 0,
				'Cell count should be non-negative'
			);
		}
	});

	// ---------------------------------------------------------------
	// Step 4b: Target frequency comes from the manifest
	// ---------------------------------------------------------------

	// This is the number place & route constrains against, and it has to be a
	// property of the entity being synthesized — not a setting — so that two top
	// entities in one workspace can want different frequencies.
	test('Step 4b: Target frequency is read from the manifest domain', async function () {
		this.timeout(15000);

		if (!manifestPath) {
			this.skip();
			return;
		}

		const parser = new ClashManifestParser();
		const manifest = await parser.parseManifest(manifestPath);

		// The test-project's top entity is clocked by Dom50: 20000 ps → 50 MHz.
		// The manifest also lists System, which is exactly what must not be
		// picked up instead.
		assert.strictEqual(pnrTargetClock(manifest)?.domain, 'Dom50');
		assert.strictEqual(pnrTargetClock(manifest)?.frequencyMHz, 50);
	});

	// ---------------------------------------------------------------
	// Step 4c: Per-module synthesis
	// ---------------------------------------------------------------

	test('Step 4c: Per-module synthesis produces individual .il and .json', async function () {
		this.timeout(120_000);

		assert.ok(verilogPath, 'Step 3 must have produced a Verilog path');

		if (!manifestPath) {
			this.skip();
			return;
		}

		const projectDirs = CodeGenerator.getProjectDirectories(wsRoot, testFunc);
		const parser = new ClashManifestParser();
		const components = await parser.buildDependencyGraph(manifestPath);

		if (components.length <= 1) {
			// Single component — per-module falls back to regular synthesis
			this.skip();
			return;
		}

		const verilogInput = allVerilogFiles || verilogPath;

		const result = await yosysRunner.synthesizePerModule(components, {
			workspaceRoot: wsRoot,
			outputDir: projectDirs.yosys,
			topModule,
			verilogPath: verilogInput,
			targetFamily: 'ecp5',
		});

		assert.ok(result.success, `Per-module synthesis should succeed.\nErrors: ${result.errors.map(e => e.message).join('\n')}`);
		assert.ok(result.moduleResults, 'Should have per-module results');
		assert.strictEqual(result.moduleResults!.length, components.length, 'Should have one result per component');

		// Verify each module produced its files
		for (const mr of result.moduleResults!) {
			assert.ok(mr.success, `Module ${mr.name} should succeed`);
			assert.ok(mr.elapsedMs >= 0, `${mr.name} elapsedMs should be non-negative`);

			if (mr.rtlilPath) {
				const stat = await fs.stat(mr.rtlilPath);
				assert.ok(stat.size > 0, `${mr.name} .il file should be non-empty`);
			}

			if (mr.diagramJsonPath) {
				const stat = await fs.stat(mr.diagramJsonPath);
				assert.ok(stat.size > 0, `${mr.name} diagram JSON should be non-empty`);

				// Verify JSON is valid
				const content = await fs.readFile(mr.diagramJsonPath, 'utf8');
				const json = JSON.parse(content);
				assert.ok(json, `${mr.name} diagram JSON should be parseable`);
			}

			// Every module gets its own netlistsvg diagram, rendered from that
			// module's netlist.
			assert.ok(mr.svgPath, `${mr.name} should report a diagram path`);
			assert.ok(
				await waitForSvg(mr.svgPath!),
				`${mr.name} rendered no diagram at ${mr.svgPath}`
			);
			const svg = await fs.readFile(mr.svgPath!, 'utf8');
			assert.ok(svg.startsWith('<svg'), `${mr.name} diagram should be an SVG`);

			// Out-of-context is what this flow does, and the results must say so
			// — the figures above exclude cross-boundary optimization.
			assert.strictEqual(
				mr.outOfContext, true,
				`${mr.name} should be marked as synthesized out of context`
			);
			assert.ok(
				Array.isArray(mr.subComponents),
				`${mr.name} should record its component graph`
			);
		}

		// The graph is persisted so Run History can rebuild the same nesting
		// from a flattened per-module run.
		const hierarchyPath = path.join(
			projectDirs.yosys, 'per-module', 'hierarchy.json'
		);
		const hierarchy = JSON.parse(await fs.readFile(hierarchyPath, 'utf8')) as {
			top: string;
			outOfContext: boolean;
			components: Record<string, string[]>;
		};
		assert.strictEqual(hierarchy.top, topModule);
		assert.strictEqual(hierarchy.outOfContext, true);
		assert.deepStrictEqual(
			Object.keys(hierarchy.components).sort(),
			result.moduleResults!.map(m => m.name).sort(),
			'every synthesized component should appear in the graph'
		);
		// The top must instantiate something, or this design would not exercise
		// hierarchical presentation at all.
		assert.ok(
			(hierarchy.components[topModule] ?? []).length > 0,
			`${topModule} should instantiate at least one component`
		);
	});

	// ---------------------------------------------------------------
	// Step 5: Place & Route with nextpnr-ecp5
	// ---------------------------------------------------------------

	test('Step 5: Place & Route with nextpnr-ecp5', async function () {
		this.timeout(120_000);

		assert.ok(yosysJsonPath, 'Step 4 must have produced a JSON netlist path');

		const projectDirs = CodeGenerator.getProjectDirectories(wsRoot, testFunc);

		const result = await nextpnrRunner.placeAndRoute({
			family: 'ecp5',
			jsonPath: yosysJsonPath,
			outputDir: projectDirs.nextpnr,
			topModule,
			ecp5: {
				device: '25k',
				package: 'CABGA381',
				speedGrade: '6',
			},
		});

		assert.ok(result.success, `nextpnr should succeed.\nErrors: ${result.errors.map(e => e.message).join('\n')}`);
		assert.ok(result.textcfgPath, 'Should produce a textcfg file');

		// Verify textcfg exists
		const stat = await fs.stat(result.textcfgPath!);
		assert.ok(stat.size > 0, 'Textcfg should be non-empty');

		// Timing info should be present
		if (result.timing) {
			if (result.timing.maxFrequency !== undefined) {
				assert.ok(result.timing.maxFrequency > 0, 'Max frequency should be positive');
			}
		}

		// Utilization info should be present
		if (result.utilization) {
			assert.ok(
				result.utilization.luts === undefined || result.utilization.luts.used >= 0,
				'LUT count should be non-negative'
			);
		}
	});
});
