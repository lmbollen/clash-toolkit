import * as vscode from 'vscode';
import * as path from 'path';
import { HLSClient, HASKELL_EXTENSION_ID } from './hls-client';
import { FunctionDetector } from './function-detector';
import { CodeGenerator, GenerationConfig, ProjectDirectories } from './code-generator';
import { promises as fsp } from 'fs';
import { ClashCompiler, ClashCompilationResult } from './clash-compiler';
import { YosysRunner } from './yosys-runner';
import { ensureSubDiagram, waitForSvg } from './netlist-diagram';
import { NextpnrRunner } from './nextpnr-runner';
import { ECP5Device, ECP5Package, PNR_FAMILIES } from './nextpnr-types';
import { FunctionInfo } from './types';
import { ClashManifestParser } from './clash-manifest-parser';
import { ToolchainChecker } from './toolchain';
import { initializeToolProvider, getToolProvider } from './tool-provider';
import { initializeLogger, getLogger } from './file-logger';
import { ClashCodeActionProvider } from './clash-code-actions';
import { SynthesisSettingsPanel } from './synthesis-settings-panel';
import { SynthesisResultsTreeProvider } from './synthesis-results-tree';
import { RunHistoryTreeProvider, RunModuleNode, RunNode } from './run-history-tree';
import { loadRun } from './run-loader';
import { HaskellFunctionsTreeProvider, FunctionNode } from './haskell-functions-tree';
import { getDefaultElaborationScript } from './synthesis-targets';

// Output channel for logging
let outputChannel: vscode.OutputChannel;

/**
 * Open a rendered SVG diagram in VS Code's built-in image preview editor.
 * If `svgPath` is undefined (e.g. the run produced no netlist), shows a helpful
 * message instead of failing silently.
 */
async function openSvgPreview(svgPath?: string): Promise<void> {
	if (!svgPath) {
		vscode.window.showWarningMessage(
			'No diagram for this module — the run produced no JSON netlist to render.'
		);
		return;
	}
	// Diagram rendering is fire-and-forget during synthesis so it doesn't stall
	// the pipeline; here is where we actually need the file, so wait for any
	// in-flight render to finish.
	const ready = await waitForSvg(svgPath);
	if (!ready) {
		vscode.window.showWarningMessage(
			'Diagram not available — rendering it failed. Check the output channel for details.'
		);
		return;
	}
	try {
		await vscode.commands.executeCommand(
			'vscode.openWith',
			vscode.Uri.file(svgPath),
			'imagePreview.previewEditor'
		);
	} catch {
		// Fall back to plain open if the image preview editor isn't available.
		await vscode.commands.executeCommand('vscode.open', vscode.Uri.file(svgPath));
	}
}
/**
 * Push a fresh set of results into the Synthesis Results view and label which
 * run produced them.  Centralised so live runs and history selections agree on
 * how the banner above the tree is formatted.
 */
function showSynthesisResults(
	modules: import('./yosys-types').ModuleSynthesisResult[],
	pnr: import('./nextpnr-types').NextpnrResult | undefined,
	message: string,
): void {
	synthesisTreeProvider.refresh(modules, pnr);
	if (synthesisResultsView) {
		// Derived from the results rather than passed in, so a run loaded from
		// history is labelled the same way a fresh one is.
		const ooc = modules.some(m => m.outOfContext);
		synthesisResultsView.message = ooc
			? `${message} · out-of-context: each component synthesized on its own, ` +
				'generic cells only — not comparable with a whole-design run'
			: message;
	}
}

let synthesisTreeProvider: SynthesisResultsTreeProvider;
let synthesisResultsView: vscode.TreeView<unknown>;
let runHistoryTreeProvider: RunHistoryTreeProvider;
let haskellFunctionsTreeProvider: HaskellFunctionsTreeProvider;
let haskellFunctionsTreeView: vscode.TreeView<import('./haskell-functions-tree').FunctionTreeNode>;
let hlsClient: HLSClient;
let clashDiagnostics: vscode.DiagnosticCollection;
let functionDetector: FunctionDetector;
let codeGenerator: CodeGenerator;
let clashCompiler: ClashCompiler;
let yosysRunner: YosysRunner;
let nextpnrRunner: NextpnrRunner;
let toolchain: ToolchainChecker;

// ── Pipeline types (declared early so caches can reference them) ─────────────

type ProgressReporter = vscode.Progress<{ message?: string; increment?: number }>;

interface CompilationOutput {
	wrapperResult: { filePath: string; moduleName: string };
	compileResult: ClashCompilationResult;
	projectDirs: ReturnType<typeof CodeGenerator.getProjectDirectories>;
	topModule: string;
	verilogInput: string | string[];
}

interface SynthesisOutput {
	synthResult: import('./yosys-types').YosysSynthesisResult;
	moduleResults: import('./yosys-types').ModuleSynthesisResult[];
	topModule: string;
	sdcFrequencyMHz: number | undefined;
	projectDirs: CompilationOutput['projectDirs'];
}

export function activate(context: vscode.ExtensionContext) {

	// Create output channel for logging
	outputChannel = vscode.window.createOutputChannel('Clash Synthesis');
	context.subscriptions.push(outputChannel);

	// Initialize file logger for crash debugging
	const workspaceFolders = vscode.workspace.workspaceFolders;
	if (workspaceFolders && workspaceFolders.length > 0) {
		const logger = initializeLogger(workspaceFolders[0].uri.fsPath);
		logger.info('Extension activating...');
		logger.info(`Workspace: ${workspaceFolders[0].uri.fsPath}`);
	}

	// Initialize HLS client and function detector
	hlsClient = new HLSClient(outputChannel);
	functionDetector = new FunctionDetector(hlsClient, outputChannel);
	codeGenerator = new CodeGenerator(outputChannel);
	clashCompiler = new ClashCompiler(outputChannel);
	clashDiagnostics = vscode.languages.createDiagnosticCollection('clash');
	context.subscriptions.push(clashDiagnostics);
	yosysRunner = new YosysRunner(outputChannel);
	nextpnrRunner = new NextpnrRunner(outputChannel);
	// Managed toolchain: lets the extension download and use its own copies of
	// yosys/nextpnr when they aren't on the user's PATH.
	initializeToolProvider(context, outputChannel);
	toolchain = new ToolchainChecker(outputChannel);

	// Register sidebar tree view for synthesis results.
	// createTreeView (instead of registerTreeDataProvider) gives us a `.message`
	// banner that we use to label which run is currently being shown.
	synthesisTreeProvider = new SynthesisResultsTreeProvider();
	synthesisResultsView = vscode.window.createTreeView(
		'clash-toolkit.synthesisResults',
		{ treeDataProvider: synthesisTreeProvider }
	);
	context.subscriptions.push(synthesisResultsView);

	// Register sidebar tree view for run history
	runHistoryTreeProvider = new RunHistoryTreeProvider();
	runHistoryTreeProvider.setWorkspaceRoot(
		vscode.workspace.workspaceFolders?.[0]?.uri.fsPath
	);
	context.subscriptions.push(
		vscode.window.registerTreeDataProvider(
			'clash-toolkit.runHistory',
			runHistoryTreeProvider
		)
	);

	// Register sidebar tree view for Haskell functions.
	// createTreeView (instead of registerTreeDataProvider) gives us a .selection
	// property so title-bar buttons can read the currently selected function.
	haskellFunctionsTreeProvider = new HaskellFunctionsTreeProvider();
	haskellFunctionsTreeView = vscode.window.createTreeView(
		'clash-toolkit.haskellFunctions',
		{ treeDataProvider: haskellFunctionsTreeProvider }
	);
	context.subscriptions.push(haskellFunctionsTreeView);

	// Refresh functions view when active editor changes to a Haskell file
	context.subscriptions.push(
		vscode.window.onDidChangeActiveTextEditor(editor => {
			if (editor && hlsClient.isHaskellDocument(editor.document)) {
				refreshHaskellFunctionsTree(editor.document);
			}
			// When switching to a non-Haskell window, keep showing the last Haskell file's contents.
		})
	);

	// Auto-refresh once HLS reports on the file it is showing.
	//
	// HLS publishes diagnostics when it has typechecked a file, which is the only
	// documented signal available to another extension — there is no readiness
	// API. This only re-detects while the view is in one of its "HLS gave us
	// nothing" states, so ordinary editing (which also changes diagnostics) does
	// not trigger repeated detection.
	context.subscriptions.push(
		vscode.languages.onDidChangeDiagnostics(e => {
			if (!haskellFunctionsTreeProvider.isAwaitingHls()) { return; }
			const editor = vscode.window.activeTextEditor;
			if (!editor || !hlsClient.isHaskellDocument(editor.document)) { return; }
			const uri = editor.document.uri.toString();
			if (!e.uris.some(u => u.toString() === uri)) { return; }
			hlsClient.invalidate(editor.document);
			refreshHaskellFunctionsTree(editor.document);
		})
	);

	// Refresh the functions view when a Haskell file is saved.
	// Refreshes the tree only — there is no extension-level cache to invalidate;
	// cabal handles incremental compilation correctly on its own.
	context.subscriptions.push(
		vscode.workspace.onDidSaveTextDocument(doc => {
			if (hlsClient.isHaskellDocument(doc)) {
				if (vscode.window.activeTextEditor?.document === doc) {
					refreshHaskellFunctionsTree(doc);
				}
			}
		})
	);

	// Seed with the already-open file (if any)
	if (vscode.window.activeTextEditor &&
		hlsClient.isHaskellDocument(vscode.window.activeTextEditor.document)) {
		refreshHaskellFunctionsTree(vscode.window.activeTextEditor.document);
	}

	// Command: open extension settings panel
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.openSettings', () => {
			SynthesisSettingsPanel.show(toolchain);
		})
	);

	// Command: open the Verilog files for a module (inline tree action)
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.openSynthesizedVerilog', async (item) => {
			const files: string[] | undefined = item?.result?.verilogFiles;
			if (!files?.length) {
				vscode.window.showInformationMessage('No Verilog files available for this module.');
				return;
			}
			for (const f of files) {
				const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(f));
				await vscode.window.showTextDocument(doc, { preview: false });
			}
		})
	);

	// Command: refresh the Haskell functions tree manually
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.refreshHaskellFunctions', () => {
			const editor = vscode.window.activeTextEditor;
			if (editor && hlsClient.isHaskellDocument(editor.document)) {
				hlsClient.invalidate(editor.document);
				refreshHaskellFunctionsTree(editor.document);
			}
		})
	);

	// Command: open the Haskell extension in the Marketplace — the action on the
	// sidebar row shown when HLS is unavailable because it isn't installed.
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.installHaskellExtension', async () => {
			await vscode.commands.executeCommand(
				'workbench.extensions.search',
				`@id:${HASKELL_EXTENSION_ID}`
			);
		})
	);

	// Command: open the rendered SVG diagram for a specific module
	// (inline tree action). Falls back gracefully when no diagram was rendered.
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.viewModuleDiagram', (item) => {
			openSvgPreview(item?.result?.svgPath);
		})
	);

	// Command: open the diagram of a sub-component (a module instantiated by
	// the one above it in the tree). Unlike the top-level diagram, this one is
	// rendered on first open rather than during synthesis — there is no point
	// laying out every module of a design nobody has asked to see.
	context.subscriptions.push(
		vscode.commands.registerCommand(
			'clash-toolkit.viewComponentDiagram',
			async (item?: { netlistPath?: string; moduleName?: string }) => {
				if (!item?.netlistPath || !item.moduleName) { return; }
				const svgPath = await vscode.window.withProgress(
					{
						location: vscode.ProgressLocation.Window,
						title: `Rendering diagram for ${item.moduleName}…`,
					},
					() => ensureSubDiagram(item.netlistPath!, item.moduleName!, outputChannel)
				);
				if (!svgPath) {
					vscode.window.showWarningMessage(
						`Could not render a diagram for ${item.moduleName}. ` +
						'Check the output channel for details.'
					);
					return;
				}
				await openSvgPreview(svgPath);
			}
		)
	);

	// ── Run History commands ──────────────────────────────────────────────

	// Command: refresh the run history tree
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.refreshRunHistory', () => {
			runHistoryTreeProvider.refresh();
		})
	);

	// Command: open Verilog from a history module node
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.openHistoryVerilog', async (item) => {
			const files: string[] | undefined =
				item instanceof RunModuleNode ? item.verilogFiles : undefined;
			if (!files?.length) {
				vscode.window.showInformationMessage('No Verilog files available for this module.');
				return;
			}
			for (const f of files) {
				const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(f));
				await vscode.window.showTextDocument(doc, { preview: false });
			}
		})
	);

	// Command: open SVG diagram from a history module node
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.openHistoryDiagram', (item) => {
			const svgPath: string | undefined =
				item instanceof RunModuleNode ? item.svgPath : undefined;
			openSvgPreview(svgPath);
		})
	);

	// Command: load a previous run's results into the Synthesis Results view.
	// Triggered by clicking a run in the Run History tree.
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.selectRun', async (item) => {
			if (!(item instanceof RunNode)) { return; }
			try {
				const loaded = await loadRun(item.runRoot);
				const cmd = loaded.meta?.command ?? 'run';
				const ts = loaded.meta?.timestamp
					? new Date(loaded.meta.timestamp).toLocaleString()
					: item.runId;
				const label = `${cmd} — ${item.qualifiedName} · ${ts}`;
				showSynthesisResults(loaded.modules, loaded.pnr, label);
			} catch (e) {
				const msg = e instanceof Error ? e.message : String(e);
				vscode.window.showErrorMessage(`Failed to load run: ${msg}`);
			}
		})
	);

	// Command: delete a run from history
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.deleteRun', async (item) => {
			if (!item?.runRoot) { return; }
			const answer = await vscode.window.showWarningMessage(
				`Delete run ${item.runId}? This cannot be undone.`,
				{ modal: true },
				'Delete'
			);
			if (answer === 'Delete') {
				try {
					await fsp.rm(item.runRoot, { recursive: true, force: true });
					runHistoryTreeProvider.refresh();
				} catch (e) {
					const msg = e instanceof Error ? e.message : String(e);
					vscode.window.showErrorMessage(`Failed to delete run: ${msg}`);
				}
			}
		})
	);

	// Command: navigate to a function (used by tree item clicks)
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.goToFunction', async (info) => {
			if (!info?.filePath || !info?.range) { return; }
			const uri = vscode.Uri.file(info.filePath);
			const doc = await vscode.workspace.openTextDocument(uri);
			const editor = await vscode.window.showTextDocument(doc);
			const start = new vscode.Position(
				info.range.start.line,
				info.range.start.character
			);
			editor.selection = new vscode.Selection(start, start);
			editor.revealRange(
				new vscode.Range(start, start),
				vscode.TextEditorRevealType.InCenterIfOutsideViewport
			);
		})
	);

	// Register commands
	registerCommands(context);

	// Register code action provider for Haskell files
	context.subscriptions.push(
		vscode.languages.registerCodeActionsProvider(
			{ language: 'haskell', scheme: 'file' },
			new ClashCodeActionProvider(functionDetector),
			{ providedCodeActionKinds: ClashCodeActionProvider.providedCodeActionKinds }
		)
	);

	outputChannel.appendLine('Clash Toolkit activated');
	outputChannel.appendLine('Make sure Haskell Language Server is running for full functionality');
	
	// Validate toolchain on activation (after a brief delay for direnv).
	// Tracked as a disposable so deactivate() cancels it — otherwise the
	// callback can fire after `outputChannel` is disposed (e.g. during test
	// teardown) and surface as an uncaught "Channel has been closed".
	if (workspaceFolders && workspaceFolders.length > 0) {
		const cwd = workspaceFolders[0].uri.fsPath;
		let cancelled = false;
		const handle = setTimeout(async () => {
			if (cancelled) { return; }
			await toolchain.checkAll(cwd);
			if (cancelled) { return; }
			outputChannel.appendLine('');
			outputChannel.appendLine(toolchain.formatSummary());
		}, 2000);
		context.subscriptions.push({
			dispose: () => { cancelled = true; clearTimeout(handle); }
		});
	}
	
	const logger = getLogger();
	if (logger) {
		logger.info('Extension activated successfully');
		logger.info(`Debug log: ${logger.getLogPath()}`);
	}
}

/**
 * Detect functions in the given document and update the Haskell functions
 * sidebar tree.
 *
 * HLS exposes no readiness API — the Haskell extension's `activate` returns
 * nothing and its language client is private — so the view reports what *can* be
 * established cheaply:
 *
 *   - **unavailable**: the Haskell extension is missing, or installed and could
 *     not be activated. Nothing will change on its own, so the row offers the fix.
 *   - **nothing yet**: HLS is there but returned no symbols. That is deliberately
 *     not reported as "0 functions" — while HLS loads a project it answers with
 *     nothing, which is indistinguishable from a file that has none. The view
 *     re-checks when HLS next reports on the file (see the diagnostics
 *     subscription in `activate`).
 *   - **results**: symbols came back, so the sections are populated.
 *
 * A sequence guard drops results superseded by a newer refresh, e.g. when the
 * user switches files while a slow detection is in flight.
 */
let functionsTreeRefreshSeq = 0;

async function refreshHaskellFunctionsTree(doc: vscode.TextDocument): Promise<void> {
	const seq = ++functionsTreeRefreshSeq;
	const superseded = () => seq !== functionsTreeRefreshSeq;

	haskellFunctionsTreeProvider.setLoading(doc.fileName);
	const availability = await hlsClient.ensureActivated();
	if (superseded()) { return; }

	if (!availability.available) {
		haskellFunctionsTreeProvider.setHlsUnavailable(
			availability.reason ?? 'extension-inactive',
			doc.fileName,
		);
		haskellFunctionsTreeView.message = `⚠ ${availability.message}`;
		return;
	}

	// "Did HLS answer?" is a question about symbols, not about functions: a file
	// with symbols but no top-level functions is a real answer and must not be
	// reported as "still waiting".
	const symbols = await hlsClient.getDocumentSymbols(doc);
	if (superseded()) { return; }
	if (symbols.length === 0) {
		haskellFunctionsTreeProvider.setNoSymbolsYet(doc.fileName);
		haskellFunctionsTreeView.message =
			'⏳ HLS has not returned symbols for this file yet — it may still be loading the ' +
			'project. This updates when HLS reports on the file, or use Refresh.';
		return;
	}

	let functions: FunctionInfo[] = [];
	try {
		functions = await functionDetector.detectFunctions(doc);
	} catch { /* HLS answered but analysis failed; report an empty result */ }
	if (superseded()) { return; }
	haskellFunctionsTreeProvider.refresh(functions, doc.fileName);
	haskellFunctionsTreeView.message = undefined;
}

/**
 * Explain an empty function-detection result to the user.
 *
 * Distinguishes "HLS is unavailable" (Haskell extension missing/inactive)
 * from a genuinely empty result, and offers to install the Haskell
 * extension when it is missing.
 */
function notifyNoFunctionsDetected(fallbackMessage: string): void {
	const hls = hlsClient.checkAvailability();
	if (hls.reason === 'extension-missing') {
		vscode.window
			.showErrorMessage(hls.message!, 'Install Haskell Extension')
			.then((choice) => {
				if (choice === 'Install Haskell Extension') {
					vscode.commands.executeCommand(
						'workbench.extensions.search',
						`@id:${HASKELL_EXTENSION_ID}`
					);
				}
			});
	} else if (hls.reason === 'extension-inactive') {
		vscode.window.showWarningMessage(hls.message!);
	} else {
		vscode.window.showWarningMessage(fallbackMessage);
	}
}

function registerCommands(context: vscode.ExtensionContext) {
	// Detect Functions command
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.detectFunctions', async () => {
			await detectFunctionsCommand();
		})
	);

	// Synthesize Function command
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.synthesizeFunction', async (arg?: unknown) => {
			await synthesizeFunctionCommand(unwrapFuncArg(arg));
		})
	);

	// Elaborate command — hierarchy + proc, no technology mapping
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.elaborate', async (arg?: unknown) => {
			await elaborateCommand(unwrapFuncArg(arg));
		})
	);

	// Place & Route command
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.placeAndRoute', async (arg?: unknown) => {
			await placeAndRouteCommand(unwrapFuncArg(arg));
		})
	);

	// Synthesize command (no PnR)
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.synthesize', async (arg?: unknown) => {
			await synthesizeCommand(unwrapFuncArg(arg));
		})
	);

	// Check toolchain availability
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.checkToolchain', async () => {
			const cwd = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
			toolchain.clearCache();
			await toolchain.checkAll(cwd);
			outputChannel.show(true);
			outputChannel.appendLine('');
			outputChannel.appendLine(toolchain.formatSummary());
			// HLS is provided by the Haskell extension, not a CLI binary, so it
			// is reported separately from the spawned-tool checks above.
			const hls = hlsClient.checkAvailability();
			outputChannel.appendLine(
				hls.available
					? '  ✓ HLS (via Haskell extension): available'
					: `  ✗ HLS: ${hls.message}`
			);
			const provider = getToolProvider();
			if (provider) {
				outputChannel.appendLine('');
				outputChannel.appendLine(provider.describeStatus());
			}
		})
	);

	// Download/manage the bundled toolchain (yosys, nextpnr) so the
	// extension can run without those tools on the user's PATH.
	context.subscriptions.push(
		vscode.commands.registerCommand('clash-toolkit.installToolchain', async () => {
			const provider = getToolProvider();
			if (!provider) { return; }
			await provider.promptInstallOrManage();
			// A fresh install changes tool availability — drop cached verdicts.
			toolchain.clearCache();
		})
	);
}

/**
 * Command: Detect Functions
 * Scans the current document or workspace for Haskell functions
 */
async function detectFunctionsCommand() {
	const logger = getLogger();
	
	try {
		if (logger) {
			await logger.operation('detectFunctionsCommand', 'Starting');
		}
		
		outputChannel.show(true);
		outputChannel.appendLine('='.repeat(60));
		outputChannel.appendLine('Detecting Functions...');
		outputChannel.appendLine('='.repeat(60));

		let functions: FunctionInfo[];

		// Check if there's an active editor with a Haskell file
		const activeEditor = vscode.window.activeTextEditor;
		if (activeEditor && hlsClient.isHaskellDocument(activeEditor.document)) {
			// Detect in current document
			outputChannel.appendLine(`Scanning current document: ${activeEditor.document.fileName}`);
			if (logger) {
				await logger.debug(`Scanning document: ${activeEditor.document.fileName}`);
			}
			functions = await functionDetector.detectFunctions(activeEditor.document);
		} else {
			// Detect in all open documents
			outputChannel.appendLine('Scanning all open Haskell documents...');
			if (logger) {
				await logger.debug('Scanning all open documents');
			}
			functions = await functionDetector.detectFunctionsInWorkspace();
		}

		if (logger) {
			await logger.info(`Detected ${functions.length} functions`);
		}

		if (functions.length === 0) {
			notifyNoFunctionsDetected(
				'No functions detected. Make sure HLS is running and the file is properly indexed.'
			);
			outputChannel.appendLine('No functions found');
			if (logger) {
				await logger.warn('No functions found');
			}
			return;
		}

		// Show function picker
		if (logger) {
			await logger.debug(`Showing picker with ${functions.length} functions`);
		}
		
		const selected = await functionDetector.showFunctionPicker(functions);
		
		if (logger) {
			if (selected) {
				await logger.info(`User selected: ${selected.name}`);
			} else {
				await logger.debug('User cancelled picker');
			}
		}
		
		if (selected) {
			if (logger) {
				await logger.debug(`Analyzing function: ${selected.name}`);
			}
			
			outputChannel.appendLine('');
			outputChannel.appendLine('Selected Function Analysis:');
			outputChannel.appendLine('-'.repeat(60));
			outputChannel.appendLine(functionDetector.getAnalysis(selected));
			
			if (logger) {
				await logger.debug(`Is monomorphic: ${selected.isMonomorphic}`);
			}
			
			// If it's monomorphic, offer to synthesize
			if (selected.isMonomorphic) {
				if (logger) {
					await logger.debug('Showing synthesize confirmation dialog');
				}
				
				const action = await vscode.window.showInformationMessage(
					`${selected.name} is synthesizable. Would you like to synthesize it now?`,
					'Synthesize',
					'Cancel'
				);
				
				if (logger) {
					await logger.info(`User action: ${action || 'cancelled'}`);
				}
				
				if (action === 'Synthesize') {
					await synthesizeCommand(selected);
				}
			} else {
				if (logger) {
					await logger.info('Function is polymorphic - showing info message');
				}
				
				vscode.window.showInformationMessage(
					`${selected.name} is polymorphic and cannot be directly synthesized. ` +
					'Create a monomorphic wrapper function to synthesize it.'
				);
			}
		}
		
		if (logger) {
			await logger.operation('detectFunctionsCommand', 'Completed');
		}

	} catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		vscode.window.showErrorMessage(`Error detecting functions: ${message}`);
		outputChannel.appendLine(`ERROR: ${message}`);
		
		const logger = getLogger();
		if (logger) {
			await logger.error('detectFunctionsCommand failed', error);
		}
	}
}

// ── Shared pipeline helpers ──────────────────────────────────────────────────

/**
 * When view/title toolbar buttons are clicked, VS Code passes the focused tree
 * item as the first argument.  Unwrap FunctionNode → FunctionInfo; pass through
 * a real FunctionInfo unchanged; return undefined for anything else.
 */
export function unwrapFuncArg(arg: unknown): FunctionInfo | undefined {
	if (arg instanceof FunctionNode) { return arg.info; }
	// A real FunctionInfo has a string `name` and a `range` object.
	if (arg && typeof (arg as FunctionInfo).name === 'string' && (arg as FunctionInfo).range) {
		return arg as FunctionInfo;
	}
	return undefined;
}

/**
 * Resolve which function to synthesize.
 *
 * Priority order:
 *   1. `providedFunc` — explicit argument (e.g. from a code-action).
 *   2. Selected item in the Haskell Functions tree view.
 *   3. QuickPick over functions detected in the active editor.
 */
async function pickFunction(providedFunc?: FunctionInfo): Promise<FunctionInfo | undefined> {
	if (providedFunc) { return providedFunc; }

	// Check the Haskell Functions tree selection first — this is what the title-bar
	// buttons should use when the user has highlighted a function in the sidebar.
	const treeSelection = haskellFunctionsTreeView?.selection[0];
	if (treeSelection instanceof FunctionNode) {
		return treeSelection.info;
	}

	const activeEditor = vscode.window.activeTextEditor;
	if (!activeEditor || !hlsClient.isHaskellDocument(activeEditor.document)) {
		vscode.window.showErrorMessage('Please select a function in the Haskell Functions panel or open a Haskell file');
		return undefined;
	}

	const functions = await functionDetector.detectFunctions(activeEditor.document);
	if (functions.length === 0) {
		notifyNoFunctionsDetected('No functions found to synthesize');
		return undefined;
	}

	const items = functions.map(f => ({
		label: f.name,
		description: f.typeSignature || 'no type signature',
		detail: `${f.filePath}:${f.range.start.line}`,
		function: f
	}));

	const sel = await vscode.window.showQuickPick(items, { placeHolder: 'Select function to synthesize' });
	return sel?.function;
}

/**
 * Step 2 of the pipeline: synthesize or elaborate with Yosys.
 *
 * Always returns a non-empty `moduleResults` array — even when Yosys
 * produces only a single top-level JSON — so the results view always has
 * data to display.
 *
 * `flow === 'elaborate'` always runs per-module (each component preserves
 * its hierarchy so its diagram shows sub-instances as boxes).
 *
 * `flow === 'synthesize'` honours `outOfContext`: when true, every
 * component is synthesized standalone with its own diagram + utilization;
 * when false, a single whole-design Yosys invocation runs.
 */
async function runYosynsSynthesis(
	compiled: CompilationOutput,
	wsRoot: string,
	flow: 'synthesize' | 'elaborate',
	outOfContext: boolean,
	targetFamily: string,
	progress: ProgressReporter,
	customScript?: string,
	abortSignal?: AbortSignal
): Promise<SynthesisOutput> {
	const { compileResult, projectDirs, topModule, verilogInput } = compiled;

	const stageLabel = flow === 'elaborate' ? 'Elaborating' : 'Synthesizing';
	progress.report({ message: `${stageLabel} with Yosys…`, increment: 40 });
	outputChannel.appendLine(`\n=== Step 3: ${stageLabel} with Yosys ===`);
	if (flow === 'synthesize') {
		outputChannel.appendLine(`Mode: ${outOfContext ? 'out-of-context (per-component)' : 'whole-design'}`);
		if (outOfContext) {
			// Be explicit about what this mode does, because it is not simply
			// "the same synthesis, per component": no synth_* runs at all.
			outputChannel.appendLine(
				'  Each component is synthesized on its own, with the components it'
			);
			outputChannel.appendLine(
				'  instantiates flattened into it, using generic optimization only'
			);
			outputChannel.appendLine(
				'  (proc, flatten, opt, memory -nomap) — no technology mapping, so the'
			);
			outputChannel.appendLine(
				'  target below and any custom script do not apply to these figures,'
			);
			outputChannel.appendLine(
				'  which overlap between parent and child and are not comparable with'
			);
			outputChannel.appendLine(
				'  a whole-design run.'
			);
		}
	}
	outputChannel.appendLine(`Target: ${targetFamily}`);

	let synthResult: import('./yosys-types').YosysSynthesisResult;

	const yosysOpts = {
		workspaceRoot: wsRoot, outputDir: projectDirs.yosys,
		topModule, verilogPath: verilogInput, targetFamily,
		customScript: customScript || undefined,
		abortSignal
	} as import('./yosys-types').YosysOptions;

	const perModuleFlow = flow === 'elaborate' || outOfContext;

	if (perModuleFlow && compileResult.manifest) {
		const manifestParser = new ClashManifestParser();
		const components = await manifestParser.buildDependencyGraph(compileResult.manifest.manifestPath);

		if (components.length > 1) {
			synthResult = flow === 'elaborate'
				? await yosysRunner.elaboratePerModule(components, yosysOpts)
				: await yosysRunner.synthesizePerModule(components, yosysOpts);
		} else {
			synthResult = await yosysRunner.synthesize(yosysOpts);
		}
	} else {
		synthResult = await yosysRunner.synthesize(yosysOpts);
	}

	if (!synthResult.success) {
		if (synthResult.errors.length > 0) {
			outputChannel.appendLine('Errors:');
			synthResult.errors.forEach(e => outputChannel.appendLine(`  ${e.message}`));
		}
		throw new Error(`Yosys ${flow === 'elaborate' ? 'elaboration' : 'synthesis'} failed — check the output channel for details`);
	}
	outputChannel.appendLine(`✓ ${flow === 'elaborate' ? 'Elaboration' : 'Synthesis'} complete`);

	if (synthResult.statistics) {
		outputChannel.appendLine(`  Cells: ${synthResult.statistics.cellCount ?? 'N/A'}`);
		outputChannel.appendLine(`  Wires: ${synthResult.statistics.wireCount ?? 'N/A'}`);
	}

	// Parse SDC target frequency if available.
	let sdcFrequencyMHz: number | undefined;
	if (compileResult.manifest) {
		sdcFrequencyMHz = await new ClashManifestParser()
			.parseSdcFrequency(compileResult.manifest.directory);
		if (sdcFrequencyMHz) {
			outputChannel.appendLine(`  SDC target: ${sdcFrequencyMHz.toFixed(2)} MHz`);
		}
	}

	// Normalise into ModuleSynthesisResult[] so the panel always has data.
	const moduleResults: import('./yosys-types').ModuleSynthesisResult[] =
		synthResult.moduleResults ?? [{
			name: topModule,
			success: synthResult.success,
			diagramJsonPath: synthResult.jsonPath,
			svgPath: synthResult.svgPath,
			verilogFiles: Array.isArray(verilogInput) ? verilogInput : [verilogInput],
			elapsedMs: 0,
			statistics: synthResult.statistics,
			errors: synthResult.errors
		}];

	return { synthResult, moduleResults, topModule, sdcFrequencyMHz, projectDirs };
}

/**
 * Generate the Clash wrapper and run the full Clash build.
 *
 * Caching at the extension level is intentionally absent — cabal already
 * does its own incremental build, which is the only correctness-safe layer
 * that knows about transitive dependencies.  Anything we layered on top of
 * that risked returning stale Verilog after edits cabal couldn't observe
 * (TH splices, env-driven generation, ...), so we always re-dispatch.
 */
async function runCompile(
	func: FunctionInfo,
	wsRoot: string,
	progress: ProgressReporter,
	runId?: string,
	abortSignal?: AbortSignal
): Promise<CompilationOutput> {
	const projectDirs = CodeGenerator.getProjectDirectories(wsRoot, func, runId);
	const genConfig: GenerationConfig = { modulePrefix: 'ClashSynth_' };

	outputChannel.appendLine(`\nRun: ${projectDirs.runId}`);
	outputChannel.appendLine(`Run directory: ${projectDirs.runRoot}`);

	progress.report({ message: 'Generating Clash wrapper…', increment: 10 });
	outputChannel.appendLine('\n=== Step 1: Generating Clash Wrapper ===');
	const wrapperResult = await codeGenerator.generateWrapper(func, genConfig, wsRoot);
	outputChannel.appendLine(`✓ Generated: ${wrapperResult.filePath}`);

	const synthInfo = await codeGenerator.ensureSynthProject(wsRoot, func.filePath);

	progress.report({ message: 'Compiling to Verilog with Clash…', increment: 20 });
	outputChannel.appendLine('\n=== Step 2: Compiling with Clash ===');

	const compileResult = await clashCompiler.compileToVerilog(wrapperResult.filePath, {
		workspaceRoot: wsRoot,
		outputDir: projectDirs.root,
		moduleName: wrapperResult.moduleName,
		hdlDir: projectDirs.verilog,
		synthProjectRoot: synthInfo.synthRoot,
		cabalProjectDir: synthInfo.cabalProjectDir ?? undefined,
		abortSignal
	});

	if (!compileResult.success) {
		if (abortSignal?.aborted) {
			throw new Error('Compilation cancelled');
		}
		if (compileResult.errors.length > 0) {
			outputChannel.appendLine('Errors:');
			compileResult.errors.forEach(e => outputChannel.appendLine(`  ${e}`));
		}
		// Surface wrapper errors as squiggles in the generated file too.
		clashDiagnostics.set(
			vscode.Uri.file(wrapperResult.filePath),
			clashCompiler.parseDiagnostics(compileResult.output, wrapperResult.filePath)
		);
		throw new Error('Clash compilation failed — check the output channel for details');
	}
	clashDiagnostics.clear();
	outputChannel.appendLine(`✓ Verilog: ${compileResult.verilogPath}`);

	// compileToVerilog guarantees these on success — a violation is an
	// internal error we want to hear about, not paper over with basenames.
	if (!compileResult.manifest || !compileResult.allVerilogFiles || !compileResult.verilogPath) {
		throw new Error('Internal error: Clash compile succeeded without manifest/Verilog metadata');
	}
	const topModule = compileResult.manifest.top_component.name;
	const verilogInput = compileResult.allVerilogFiles;

	return { wrapperResult, compileResult, projectDirs, topModule, verilogInput };
}

/**
 * Write a small `run.json` file in the run root summarising what was done.
 * Lets a future history view list past runs with their command/target/outcome
 * without having to re-parse Yosys/nextpnr logs. Failure is non-fatal.
 */
async function writeRunMetadata(
	projectDirs: ProjectDirectories,
	func: FunctionInfo,
	command: 'elaborate' | 'synthesize' | 'place-and-route' | 'generate-verilog',
	extra: Record<string, unknown> = {},
): Promise<void> {
	try {
		await fsp.mkdir(projectDirs.runRoot, { recursive: true });
		const qualifiedName = func.moduleName ? `${func.moduleName}.${func.name}` : func.name;
		const meta = {
			runId: projectDirs.runId,
			command,
			function: qualifiedName,
			functionFile: func.filePath,
			timestamp: new Date().toISOString(),
			...extra,
		};
		await fsp.writeFile(
			path.join(projectDirs.runRoot, 'run.json'),
			JSON.stringify(meta, null, 2),
			'utf8',
		);
		runHistoryTreeProvider?.refresh();
	} catch {
		/* non-fatal */
	}
}

/**
 * Run the full Clash → Yosys pipeline.  Always re-runs both stages — see
 * runCompile for why we don't memoize at the extension level.
 */
async function runPipeline(
	func: FunctionInfo,
	wsRoot: string,
	flow: 'synthesize' | 'elaborate',
	outOfContext: boolean,
	targetFamily: string,
	progress: ProgressReporter,
	customScript?: string,
	runId?: string,
	abortSignal?: AbortSignal
): Promise<{ compiled: CompilationOutput; synthesis: SynthesisOutput }> {
	const compiled = await runCompile(func, wsRoot, progress, runId, abortSignal);
	const synthesis = await runYosynsSynthesis(compiled, wsRoot, flow, outOfContext, targetFamily, progress, customScript, abortSignal);
	return { compiled, synthesis };
}

// ── Commands ─────────────────────────────────────────────────────────────────

/**
 * Command: Generate Verilog
 * Generates the Clash wrapper and compiles to Verilog, then shows the result.
 */
async function synthesizeFunctionCommand(providedFunc?: FunctionInfo) {
	const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!wsRoot) { vscode.window.showErrorMessage('No workspace folder open'); return; }

	const func = await pickFunction(providedFunc);
	if (!func) { return; }

	// Fix the run id up front so a failure anywhere in the pipeline can still
	// be recorded against the correct run directory.
	const runDirs = CodeGenerator.getProjectDirectories(wsRoot, func);

	outputChannel.show(true);
	outputChannel.appendLine('='.repeat(60));
	outputChannel.appendLine(`Generate Verilog: ${func.name}`);
	outputChannel.appendLine('='.repeat(60));

	try {
		await vscode.window.withProgress({
			location: vscode.ProgressLocation.Notification,
			title: `Compiling ${func.name} to Verilog`,
			cancellable: true
		}, async (progress, token) => {
			const abortController = new AbortController();
			token.onCancellationRequested(() => abortController.abort());
			const { compileResult, projectDirs } =
				await runCompile(func, wsRoot, progress, runDirs.runId, abortController.signal);
			progress.report({ message: 'Done', increment: 100 });

			await writeRunMetadata(projectDirs, func, 'generate-verilog', {
				success: compileResult.success,
				topModule: compileResult.manifest?.top_component?.name,
			});

			outputChannel.appendLine('');
			outputChannel.appendLine('='.repeat(60));
			outputChannel.appendLine('✓ Verilog generated!');
			outputChannel.appendLine('='.repeat(60));
			if (compileResult.warnings.length > 0) {
				outputChannel.appendLine('Warnings:');
				compileResult.warnings.forEach(w => outputChannel.appendLine(`  ${w}`));
			}

			if (compileResult.verilogPath) {
				outputChannel.appendLine(`Verilog: ${compileResult.verilogPath}`);
			}
		});
	} catch (error) {
		const msg = error instanceof Error ? error.message : String(error);
		vscode.window.showErrorMessage(`Verilog generation failed: ${msg}`);
		outputChannel.appendLine(`ERROR: ${msg}`);
		await writeRunMetadata(runDirs, func, 'generate-verilog', { success: false, error: msg });
	}
}

/**
 * Command: Elaborate
 * Runs Clash → Yosys with an elaboration-only script (hierarchy + proc,
 * no technology mapping), then opens the diagram panel.
 *
 * The elaborated netlist shows the design as Yosys understands it before
 * any synthesis decisions — generic word-level cells (`$add`, `$mux`,
 * `$dff`, `$mem_v2`, ...) — which is the most readable representation
 * for inspecting what Clash produced.
 */
async function elaborateCommand(providedFunc?: FunctionInfo) {
	const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!wsRoot) { vscode.window.showErrorMessage('No workspace folder open'); return; }

	const cfg = vscode.workspace.getConfiguration('clash-toolkit');
	if (!(await toolchain.require('yosys', cfg.get<string>('yosysCommand', 'yosys'), '-V', wsRoot))) { return; }

	const func = await pickFunction(providedFunc);
	if (!func) { return; }

	// Fix the run id up front so failures can be recorded against the run.
	const runDirs = CodeGenerator.getProjectDirectories(wsRoot, func);

	outputChannel.show(true);
	outputChannel.appendLine('='.repeat(60));
	outputChannel.appendLine(`Elaborate: ${func.name}`);
	outputChannel.appendLine('='.repeat(60));

	try {
		await vscode.window.withProgress({
			location: vscode.ProgressLocation.Notification,
			title: `Elaborating ${func.name}`,
			cancellable: true
		}, async (progress, token) => {
			const abortController = new AbortController();
			token.onCancellationRequested(() => abortController.abort());
			// Elaboration uses its own script template — user-overridable via the
			// `elaborationScript` setting.  The target dropdown is irrelevant at
			// this stage (no tech mapping), so we pass 'generic' and ignore it.
			// The custom script is only consulted for single-component designs;
			// multi-component elaborations run per-module via `elaboratePerModule`,
			// which uses its own internal script.
			const userScript = cfg.get<string>('elaborationScript', '') || undefined;
			const customScript = userScript ?? getDefaultElaborationScript();

			const { synthesis: { synthResult, moduleResults, projectDirs } } =
				await runPipeline(
					func, wsRoot,
					'elaborate',
					true,
					'generic',
					progress,
					customScript,
					runDirs.runId,
					abortController.signal
				);

			progress.report({ message: 'Done', increment: 100 });
			outputChannel.appendLine('');
			outputChannel.appendLine('='.repeat(60));
			outputChannel.appendLine('✓ Elaboration Complete!');
			outputChannel.appendLine('='.repeat(60));
			if (synthResult.statistics) {
				outputChannel.appendLine(`  Cells: ${synthResult.statistics.cellCount ?? 'N/A'}`);
				outputChannel.appendLine(`  Wires: ${synthResult.statistics.wireCount ?? 'N/A'}`);
				if (synthResult.statistics.logicDepth !== undefined) {
					outputChannel.appendLine(`  Logic depth: ${synthResult.statistics.logicDepth} cell(s)`);
				}
			}

			const elabLabel = `Elaboration — ${func.name}`;
			showSynthesisResults(moduleResults, undefined, elabLabel);

			await writeRunMetadata(projectDirs, func, 'elaborate', {
				success: synthResult.success,
				cellCount: synthResult.statistics?.cellCount,
				wireCount: synthResult.statistics?.wireCount,
				logicDepth: synthResult.statistics?.logicDepth,
			});

			await openSvgPreview(synthResult.svgPath);

			vscode.window.showInformationMessage(
				`Elaboration complete — ${func.name}. Cells: ${synthResult.statistics?.cellCount ?? 'N/A'}`
			);
		});
	} catch (error) {
		const msg = error instanceof Error ? error.message : String(error);
		vscode.window.showErrorMessage(`Elaboration error: ${msg}`);
		outputChannel.appendLine(`ERROR: ${msg}`);
		await writeRunMetadata(runDirs, func, 'elaborate', { success: false, error: msg });
	}
}

/**
 * Command: Synthesize (no Place & Route)
 * Calls runClashCompilation → runYosynsSynthesis, then always opens the
 * diagram panel so the user gets circuit visualisations.
 */
async function synthesizeCommand(providedFunc?: FunctionInfo) {
	const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!wsRoot) { vscode.window.showErrorMessage('No workspace folder open'); return; }

	const cfg = vscode.workspace.getConfiguration('clash-toolkit');
	if (!(await toolchain.require('yosys', cfg.get<string>('yosysCommand', 'yosys'), '-V', wsRoot))) { return; }

	const func = await pickFunction(providedFunc);
	if (!func) { return; }

	// Fix the run id up front so failures can be recorded against the run.
	const runDirs = CodeGenerator.getProjectDirectories(wsRoot, func);

	outputChannel.show(true);
	outputChannel.appendLine('='.repeat(60));
	outputChannel.appendLine(`Synthesize: ${func.name}`);
	outputChannel.appendLine('='.repeat(60));

	try {
		await vscode.window.withProgress({
			location: vscode.ProgressLocation.Notification,
			title: `Synthesizing ${func.name}`,
			cancellable: true
		}, async (progress, token) => {
			const abortController = new AbortController();
			token.onCancellationRequested(() => abortController.abort());
			const targetFamily = cfg.get<string>('synthesisTarget', 'generic');
			const customScript = cfg.get<string>(`synthesisScript.${targetFamily}`, '') || undefined;
			const outOfContext = cfg.get<boolean>('outOfContext', false);
			const { synthesis: { synthResult, moduleResults, sdcFrequencyMHz, projectDirs } } =
				await runPipeline(
					func, wsRoot,
					'synthesize',
					outOfContext,
					targetFamily, progress,
					customScript,
					runDirs.runId,
					abortController.signal
				);

			progress.report({ message: 'Done', increment: 100 });
			outputChannel.appendLine('');
			outputChannel.appendLine('='.repeat(60));
			outputChannel.appendLine('✓ Synthesis Complete!');
			outputChannel.appendLine('='.repeat(60));
			if (sdcFrequencyMHz) {
				outputChannel.appendLine(`  Target: ${sdcFrequencyMHz.toFixed(2)} MHz`);
			}
			if (moduleResults.length > 1) {
				outputChannel.appendLine('');
				outputChannel.appendLine('Per-Module Results:');
				for (const mr of moduleResults) {
					const cells = mr.statistics?.cellCount !== undefined ? `${mr.statistics.cellCount} cells` : 'no cell count';
					const wires = mr.statistics?.wireCount !== undefined ? `, ${mr.statistics.wireCount} wires` : '';
					outputChannel.appendLine(`  ${mr.success ? '✓' : '✗'} ${mr.name}: ${cells}${wires}  [${mr.elapsedMs}ms]`);
				}
			}

			const synthLabel = `Synthesis — ${func.name}`;
			showSynthesisResults(moduleResults, undefined, synthLabel);

			await writeRunMetadata(projectDirs, func, 'synthesize', {
				target: targetFamily,
				outOfContext,
				success: synthResult.success,
				cellCount: synthResult.statistics?.cellCount,
				wireCount: synthResult.statistics?.wireCount,
				logicDepth: synthResult.statistics?.logicDepth,
				sdcFrequencyMHz,
				moduleCount: moduleResults.length,
			});

			await openSvgPreview(synthResult.svgPath);

			vscode.window.showInformationMessage(
				`Synthesis complete — ${func.name}. Cells: ${synthResult.statistics?.cellCount ?? 'N/A'}`
			);
		});
	} catch (error) {
		const msg = error instanceof Error ? error.message : String(error);
		vscode.window.showErrorMessage(`Synthesis error: ${msg}`);
		outputChannel.appendLine(`ERROR: ${msg}`);
		await writeRunMetadata(runDirs, func, 'synthesize', { success: false, error: msg });
	}
}

/**
 * Command: Place & Route
 * Runs the synthesis pipeline, then hands the netlist to nextpnr for timing.
 * PnR always uses whole-design synthesis so nextpnr gets a merged netlist.
 */
async function placeAndRouteCommand(providedFunc?: FunctionInfo) {
	const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!wsRoot) { vscode.window.showErrorMessage('No workspace folder open'); return; }

	const cfg = vscode.workspace.getConfiguration('clash-toolkit');
	if (!(await toolchain.require('yosys', cfg.get<string>('yosysCommand', 'yosys'), '-V', wsRoot))) { return; }

	// Determine nextpnr family from the configured synthesis target
	const synthTarget = cfg.get<string>('synthesisTarget', 'generic');
	const familyInfo = PNR_FAMILIES.get(synthTarget);
	if (!familyInfo) {
		const supported = [...PNR_FAMILIES.keys()].join(', ');
		vscode.window.showErrorMessage(
			`Place & Route is not available for the "${synthTarget}" synthesis target. ` +
			`Supported targets: ${supported}. Change the target in settings.`
		);
		return;
	}

	if (!(await toolchain.require(familyInfo.binary, familyInfo.binary, '--version', wsRoot))) { return; }

	const func = await pickFunction(providedFunc);
	if (!func) { return; }

	// Fix the run id up front so failures can be recorded against the run.
	const runDirs = CodeGenerator.getProjectDirectories(wsRoot, func);

	// Device picker — populated from PNR_FAMILIES registry
	const deviceChoice = await vscode.window.showQuickPick(
		familyInfo.devices,
		{ placeHolder: `Select target ${synthTarget.toUpperCase()} device` }
	);
	if (!deviceChoice) { return; }

	// Package picker — probe nextpnr for valid packages
	const validPackages = await NextpnrRunner.getValidPackages(deviceChoice.value, familyInfo.binary, familyInfo.deviceFlag);
	let packageChoice: { label: string; value: string } | undefined;
	if (validPackages.length > 0) {
		const packageOptions = validPackages.map(pkg => ({ label: pkg, value: pkg }));
		packageChoice = await vscode.window.showQuickPick(packageOptions, { placeHolder: 'Select package type' });
		if (!packageChoice) { return; }
	}

	outputChannel.show(true);
	outputChannel.appendLine('='.repeat(60));
	outputChannel.appendLine(`Synthesize + Place & Route: ${func.name}`);
	outputChannel.appendLine('='.repeat(60));

	try {
		await vscode.window.withProgress({
			location: vscode.ProgressLocation.Notification,
			title: `Implementing ${func.name} on ${synthTarget.toUpperCase()}`,
			cancellable: true
		}, async (progress, token) => {
			// Bridge VS Code's cancellation token into an AbortController that
			// every pipeline stage (cabal/Clash, Yosys, nextpnr) listens on —
			// Cancel must kill whichever child process is currently running,
			// not just nextpnr.
			const abortController = new AbortController();
			const cancelSub = token.onCancellationRequested(() => abortController.abort());

			// Steps 1–3: Clash + Yosys pipeline.
			// PnR always uses whole-design synthesis (nextpnr needs a merged netlist).
			const customScript = cfg.get<string>(`synthesisScript.${synthTarget}`, '') || undefined;
			const { synthesis: { synthResult, moduleResults, topModule, sdcFrequencyMHz, projectDirs } } =
				await runPipeline(func, wsRoot, 'synthesize', false, synthTarget, progress, customScript, runDirs.runId, abortController.signal);

			const pnrInProgressLabel = `Synthesis — ${func.name} (P&R running…)`;
			showSynthesisResults(moduleResults, undefined, pnrInProgressLabel);

			// Step 4: Place & Route
			if (!synthResult.jsonPath) {
				throw new Error('No JSON output from Yosys — cannot run PnR');
			}

			progress.report({ message: `Place and Route with ${familyInfo.binary}…`, increment: 60 });
			outputChannel.appendLine(`\n=== Step 4: Place & Route with ${familyInfo.binary} ===`);

			// Build family-specific options.
			//
			// Target frequency precedence: an SDC constraint always wins.  If no
			// SDC is present we fall back to the `pnrTargetFrequencyMHz` setting,
			// which maps to nextpnr's `--freq` flag so analysis runs without an
			// SDC still get a meaningful "did we hit the target?" verdict.
			const userFreq = cfg.get<number | null>('pnrTargetFrequencyMHz', null);
			const effectiveFreq = sdcFrequencyMHz
				?? (typeof userFreq === 'number' && userFreq > 0 ? userFreq : undefined);

			const selectedDevice = deviceChoice as import('./nextpnr-types').DeviceOption;
			const pnrOpts: import('./nextpnr-types').NextpnrOptions = {
				family: familyInfo.family,
				jsonPath: synthResult.jsonPath,
				outputDir: projectDirs.nextpnr,
				topModule,
				frequency: effectiveFreq,
				device: selectedDevice.value,
				packageName: packageChoice?.value,
				vopt: selectedDevice.vopt ? [selectedDevice.vopt] : undefined,
				routedSvg: cfg.get<boolean>('pnrWriteRoutedSvg', true),
				abortSignal: abortController.signal,
				progressUpdate: (msg) => progress.report({ message: msg }),
			};

			if (familyInfo.family === 'ecp5') {
				// 5G parts (um5g-*) only support speed grade 8
				const is5G = selectedDevice.value.startsWith('um5g');
				pnrOpts.ecp5 = {
					device: selectedDevice.value as ECP5Device,
					package: (packageChoice?.value ?? 'CABGA381') as ECP5Package,
					speedGrade: is5G ? '8' : '6',
				};
			}

			let pnrResult: import('./nextpnr-types').NextpnrResult;
			try {
				pnrResult = await nextpnrRunner.placeAndRoute(pnrOpts);
			} finally {
				cancelSub.dispose();
			}

			if (token.isCancellationRequested) {
				throw new Error('Place and route cancelled');
			}

			if (!pnrResult.success) {
				// Surface the actual reason on the toast and pop the output
				// channel so the user sees what nextpnr complained about.
				outputChannel.show(true);
				const headline = pnrResult.errors[0]?.message?.trim();
				const detail = headline && headline.length > 0
					? headline
					: 'nextpnr did not produce a placed design';
				throw new Error(`Place and route failed: ${detail}`);
			}

			// Refresh the sidebar with both synth module results and the PNR
			// timing / utilization / critical-path data so users see everything
			// in the tree instead of having to scroll the output channel.
			showSynthesisResults(moduleResults, pnrResult, `Place & Route — ${func.name}`);

			await writeRunMetadata(projectDirs, func, 'place-and-route', {
				target: synthTarget,
				device: deviceChoice.value,
				deviceLabel: deviceChoice.label,
				packageName: packageChoice?.value,
				success: pnrResult.success,
				cellCount: synthResult.statistics?.cellCount,
				maxFrequencyMHz: pnrResult.timing?.maxFrequency,
				constraintsMet: pnrResult.timing?.constraintsMet,
			});

			progress.report({ message: 'Done', increment: 100 });
			outputChannel.appendLine('');
			outputChannel.appendLine('='.repeat(60));
			outputChannel.appendLine('✓ FPGA Implementation Complete!');
			outputChannel.appendLine('='.repeat(60));
			outputChannel.appendLine(`  Config:    ${pnrResult.textcfgPath}`);
			if (pnrResult.reportJsonPath) {
				outputChannel.appendLine(`  Report:    ${pnrResult.reportJsonPath}`);
			}
			if (pnrResult.routedSvgPath) {
				outputChannel.appendLine(`  Routed SVG: ${pnrResult.routedSvgPath}`);
			}

			if (pnrResult.timing) {
				const t = pnrResult.timing;
				outputChannel.appendLine('');
				outputChannel.appendLine('Timing Analysis:');
				outputChannel.appendLine('-'.repeat(40));
				if (t.prePlacementFrequency) {
					outputChannel.appendLine(`  Pre-Routing:   ${t.prePlacementFrequency.toFixed(2)} MHz (estimate)`);
				}
				if (t.maxFrequency) {
					outputChannel.appendLine(`  Max Frequency: ${t.maxFrequency.toFixed(2)} MHz (actual)`);
				}
				if (t.criticalPathDelay) {
					outputChannel.appendLine(`  Critical Path: ${t.criticalPathDelay.toFixed(2)} ns`);
				}
				outputChannel.appendLine(`  Constraints: ${t.constraintsMet ? '✓ MET' : '✗ FAILED'}`);
			}

			if (pnrResult.utilization) {
				const u = pnrResult.utilization;
				outputChannel.appendLine('');
				outputChannel.appendLine('Resource Utilization:');
				outputChannel.appendLine('-'.repeat(40));
				if (u.luts) {
					outputChannel.appendLine(`  LUTs:      ${u.luts.used}/${u.luts.total} (${((u.luts.used / u.luts.total) * 100).toFixed(1)}%)`);
				}
				if (u.registers) {
					outputChannel.appendLine(`  Registers: ${u.registers.used}/${u.registers.total} (${((u.registers.used / u.registers.total) * 100).toFixed(1)}%)`);
				}
				if (u.bram) {
					outputChannel.appendLine(`  BRAM:      ${u.bram.used}/${u.bram.total} (${((u.bram.used / u.bram.total) * 100).toFixed(1)}%)`);
				}
				if (u.io) {
					outputChannel.appendLine(`  IO:        ${u.io.used}/${u.io.total} (${((u.io.used / u.io.total) * 100).toFixed(1)}%)`);
				}
			}

			const action = await vscode.window.showInformationMessage(
				'✓ Place & Route complete!',
				'Open PnR Folder'
			);
			if (action === 'Open PnR Folder') {
				await vscode.commands.executeCommand('revealFileInOS', vscode.Uri.file(projectDirs.nextpnr));
			}
		});
	} catch (error) {
		const msg = error instanceof Error ? error.message : String(error);
		vscode.window.showErrorMessage(`Implementation error: ${msg}`);
		outputChannel.appendLine(`ERROR: ${msg}`);
		// The banner was set to "(P&R running…)" before the PnR stage; every
		// failure path lands here, so replace it or it lies indefinitely.
		if (synthesisResultsView?.message?.includes('P&R running')) {
			synthesisResultsView.message = `Synthesis — ${func.name} (P&R failed)`;
		}
		await writeRunMetadata(runDirs, func, 'place-and-route', {
			target: synthTarget,
			device: deviceChoice.value,
			packageName: packageChoice?.value,
			success: false,
			error: msg,
		});
	}
}

export function deactivate() {
	const logger = getLogger();
	if (logger) {
		logger.info('Extension deactivating...');
	}
	
	if (outputChannel) {
		outputChannel.dispose();
	}
	
	if (logger) {
		logger.info('Extension deactivated');
	}
}
