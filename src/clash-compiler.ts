import * as vscode from 'vscode';
import { spawn } from 'child_process';
import * as path from 'path';
import { promises as fs, createWriteStream, WriteStream } from 'fs';
import { ClashManifestParser } from './clash-manifest-parser';
import { ParsedClashManifest } from './clash-manifest-types';
import { getLogger } from './file-logger';
import { parseJobSetting } from './parallelism';
import { toolInvocation } from './toolchain';

/**
 * Result of Clash compilation
 */
export interface ClashCompilationResult {
	success: boolean;
	verilogPath?: string;
	/** All Verilog files including dependencies (from manifest) */
	allVerilogFiles?: string[];
	/** Parsed manifest with metadata */
	manifest?: ParsedClashManifest;
	errors: string[];
	warnings: string[];
	output: string;
}

/**
 * Options for Clash compilation
 */
export interface ClashCompilationOptions {
	workspaceRoot: string;
	outputDir: string;
	moduleName: string;
	hdlDir?: string;
	/** Root of the synthesis cabal project (.clash/synth-project).
	 *  When set, cabal is invoked with this as cwd so the wrapper module
	 *  and all user-package dependencies are resolved through cabal. */
	synthProjectRoot?: string;
	/** The user's cabal project directory (where cabal.project lives).
	 *  When set, the compiler passes --project-dir and --project-file so
	 *  that relative paths in the imported cabal.project resolve correctly. */
	cabalProjectDir?: string;
	/** Abort signal — cancels the build by killing the cabal process. */
	abortSignal?: AbortSignal;
	/** How many packages cabal may build at once (`--jobs`).  Either a count
	 *  or `'auto'` for one job per core; omit to leave cabal's default. */
	cabalJobs?: number | string;
	/** Modules GHC may compile in parallel within a package
	 *  (`--ghc-options=-jN`); omit to leave GHC single-threaded. */
	ghcJobs?: number;
}

/**
 * Translate the `cabalJobs` setting into cabal's `--jobs` argument.
 *
 * cabal spells "one job per core" `$ncpus`; `auto` is the friendlier spelling
 * we expose.  Anything we can't make sense of returns undefined so the build
 * runs with cabal's own default rather than failing on a bad argument.
 */
export function resolveCabalJobs(setting: number | string | undefined | null): string | undefined {
	// cabal has its own spelling for "one per core" and works out the count
	// itself, so unlike the other tools this one stays a token rather than
	// being resolved to a number here.
	const parsed = parseJobSetting(setting);
	if (parsed === undefined) { return undefined; }
	return parsed === 'auto' ? '$ncpus' : String(parsed);
}

/**
 * Handles Clash compilation from Haskell to Verilog
 */
export class ClashCompiler {
	private manifestParser: ClashManifestParser;

	/** The fixed command and arguments used to invoke the Clash compiler
	 *  via the synthesis cabal project. */
	private static readonly CLASH_SUBCOMMAND = 'run';
	/** Follows the subcommand and its build flags. */
	private static readonly CLASH_TARGET_ARGS = ['clash-synth:clash', '--'];

	constructor(private outputChannel: vscode.OutputChannel) {
		this.manifestParser = new ClashManifestParser();
	}

	/**
	 * Compile a Clash module to Verilog.
	 *
	 * When options.synthProjectRoot is set, we run cabal inside the
	 * synthesis cabal project which depends on the user's package.
	 * This ensures all transitive dependencies are resolved correctly.
	 * Otherwise we fall back to the old -i flag approach.
	 */
	async compileToVerilog(
		wrapperPath: string,
		options: ClashCompilationOptions
	): Promise<ClashCompilationResult> {
		this.outputChannel.appendLine('');
		this.outputChannel.appendLine('='.repeat(60));
		this.outputChannel.appendLine('Clash Compilation');
		this.outputChannel.appendLine('='.repeat(60));
		this.outputChannel.appendLine(`Module: ${options.moduleName}`);
		this.outputChannel.appendLine(`Workspace: ${options.workspaceRoot}`);
		this.outputChannel.appendLine('');

		// Determine HDL output directory
		const hdlDir = options.hdlDir || path.join(options.outputDir, 'verilog');

		// Get Clash command.  A configured override may carry a wrapper in front
		// of cabal (`nix run nixpkgs#cabal-install --`), whose args lead.
		const { command, args: wrapperArgs } = toolInvocation('cabal');

		// Determine cwd and extra args depending on whether we have a
		// synthesis cabal project.
		let cwd: string;
		const cabalFlags: string[] = [];  // cabal-level flags (before subcommand)
		const buildFlags: string[] = [];  // build flags (after the subcommand)
		const extraArgs: string[] = [];   // GHC/Clash flags (after --)

		// Parallelism.  Without --jobs, a cold dependency tree is built one
		// package at a time on a single core, which dominates the first run.
		const jobs = resolveCabalJobs(options.cabalJobs);
		if (jobs) { buildFlags.push(`--jobs=${jobs}`); }
		// GHC-level parallelism is opt-in: it is part of the build plan, so
		// turning it on or off rebuilds every package in the plan once.
		if (options.ghcJobs && options.ghcJobs > 1) {
			buildFlags.push(`--ghc-options=-j${Math.floor(options.ghcJobs)}`);
		}

		if (options.synthProjectRoot) {
			if (options.cabalProjectDir) {
				// Run with --project-dir pointing at the user's project root
				// so that packages: paths in their cabal.project resolve
				// correctly.  --project-file selects our synth project file.
				cwd = options.cabalProjectDir;
				const projectFile = path.join(options.synthProjectRoot, 'cabal.project');
				cabalFlags.push(`--project-dir=${options.cabalProjectDir}`);
				cabalFlags.push(`--project-file=${projectFile}`);
				this.outputChannel.appendLine(`Using synth project: ${options.synthProjectRoot}`);
				this.outputChannel.appendLine(`Project dir: ${options.cabalProjectDir}`);
			} else {
				// No user cabal.project — run inside the synth project directly
				cwd = options.synthProjectRoot;
				this.outputChannel.appendLine(`Using synth project: ${cwd}`);
			}
		} else {
			// Legacy fallback: add wrapper dir to GHC search path
			cwd = options.workspaceRoot;
			const wrapperDir = path.dirname(wrapperPath);
			extraArgs.push(`-i${wrapperDir}`);
		}

		// Build Clash command arguments — use module name, not file path
		// cabalFlags go before the subcommand, buildFlags between it and the
		// target, extraArgs after --
		const args = [
			...wrapperArgs,
			...cabalFlags,
			ClashCompiler.CLASH_SUBCOMMAND,
			...buildFlags,
			...ClashCompiler.CLASH_TARGET_ARGS,
			...extraArgs,
			options.moduleName,
			'--verilog',
			'-fclash-hdldir', hdlDir
		];

		this.outputChannel.appendLine(`Running: ${command} ${args.join(' ')}`);
		this.outputChannel.appendLine('');

		// Open a persistent build log alongside the output channel so the
		// full Clash output is never lost when the channel scrolls past it.
		const buildLogPath = path.join(options.workspaceRoot, '.clash', 'clash-build.log');
		let logStream: WriteStream | undefined;
		try {
			await fs.mkdir(path.dirname(buildLogPath), { recursive: true });
			logStream = createWriteStream(buildLogPath, { flags: 'w' });
			logStream.write(`Clash Build Log\n`);
			logStream.write(`Time:    ${new Date().toISOString()}\n`);
			logStream.write(`Command: ${command} ${args.join(' ')}\n`);
			logStream.write(`CWD:     ${cwd}\n`);
			logStream.write(`${'='.repeat(60)}\n\n`);
		} catch {
			// If we can't open the log file, continue without it.
			logStream = undefined;
		}
		this.outputChannel.appendLine(`Build log: ${buildLogPath}`);
		this.outputChannel.appendLine('');

		return new Promise((resolve) => {
			const logger = getLogger();
			const finishLog = logger?.command(command, args, cwd);
			const clash = spawn(command, args, {
				cwd,
				env: process.env,
				windowsHide: true,
			});

			let stdout = '';
			let stderr = '';
			let settled = false;
			let killTimer: ReturnType<typeof setTimeout> | undefined;
			const errors: string[] = [];
			const warnings: string[] = [];

			// Cancellation: kill cabal with SIGTERM, escalate to SIGKILL if
			// it ignores the request (e.g. mid-GHC-compilation). Both are
			// guarded: an abort raised before the child is running — including
			// one already set when compile() was called, or a spawn that failed
			// because cabal is missing — has nothing to signal, and on Windows
			// kill() throws EINVAL there instead of quietly doing nothing.
			const onAbort = () => {
				if (settled) { return; }
				this.outputChannel.appendLine('\nCancelled — terminating cabal/Clash build');
				try { clash.kill('SIGTERM'); } catch { /* never started, or already gone */ }
				killTimer = setTimeout(() => {
					if (!settled && clash.exitCode === null) {
						try { clash.kill('SIGKILL'); } catch { /* gone */ }
					}
				}, 5000);
			};
			if (options.abortSignal?.aborted) {
				onAbort();
			} else {
				options.abortSignal?.addEventListener('abort', onAbort, { once: true });
			}
			const cleanup = () => {
				settled = true;
				if (killTimer) { clearTimeout(killTimer); }
				options.abortSignal?.removeEventListener('abort', onAbort);
			};

			clash.stdout.on('data', (data) => {
				const text = data.toString();
				stdout += text;
				this.outputChannel.append(text);
				logStream?.write(text);
			});

			clash.stderr.on('data', (data) => {
				const text = data.toString();
				stderr += text;
				this.outputChannel.append(text);
				logStream?.write(text);

				// Parse for errors and warnings
				if (text.toLowerCase().includes('error')) {
					errors.push(text.trim());
				}
				if (text.toLowerCase().includes('warning')) {
					warnings.push(text.trim());
				}
			});

			clash.on('error', (error) => {
				cleanup();
				this.outputChannel.appendLine(`\nERROR: Failed to spawn Clash command: ${error.message}`);
				this.outputChannel.appendLine(`Command: ${command} ${args.join(' ')}`);
				logStream?.end(`\nERROR: ${error.message}\n`);
				resolve({
					success: false,
					errors: [error.message],
					warnings: [],
					output: stdout + stderr
				});
			});

			clash.on('close', async (code) => {
				cleanup();
				finishLog?.then(fn => fn(code));
				logStream?.end(`\n${'='.repeat(60)}\nExit code: ${code}\n`);
				this.outputChannel.appendLine('');
				this.outputChannel.appendLine(`Clash exited with code ${code}`);

				if (options.abortSignal?.aborted) {
					resolve({
						success: false,
						errors: ['Compilation cancelled'],
						warnings,
						output: stdout + stderr
					});
					return;
				}

				if (code === 0) {
					this.outputChannel.appendLine('✓ Compilation successful');

					// Find generated Verilog file
					// The manifest is the single source of truth for the
					// generated design — locating outputs by directory-listing
					// heuristics would silently mask a broken Clash run, so any
					// missing piece here is a loud failure.
					try {
						// Clash generates: hdlDir/ModuleName.topEntity/{t_name}.v
						// where t_name comes from the Synthesize annotation.
						const moduleDir = path.join(hdlDir, `${options.moduleName}.topEntity`);
						const manifestPath = await this.manifestParser.findManifest(moduleDir);
						if (!manifestPath) {
							throw new Error(
								`No clash-manifest.json in ${moduleDir} — Clash reported success ` +
								'but did not produce a manifest; cannot locate the generated design.'
							);
						}
						this.outputChannel.appendLine(`✓ Found manifest: ${manifestPath}`);
						const manifest = await this.manifestParser.parseManifest(manifestPath);

						const verilogPath = path.join(moduleDir, `${manifest.top_component.name}.v`);
						try {
							await fs.access(verilogPath);
						} catch {
							throw new Error(
								`Manifest names top component "${manifest.top_component.name}" ` +
								`but ${verilogPath} does not exist.`
							);
						}
						this.outputChannel.appendLine(`✓ Generated Verilog: ${verilogPath}`);

						// Collect all Verilog files including dependencies
						const allVerilogFiles = await this.manifestParser.collectAllVerilogFiles(manifestPath);
						this.outputChannel.appendLine(`✓ Collected ${allVerilogFiles.length} Verilog file(s):`);
						allVerilogFiles.forEach(f => {
							this.outputChannel.appendLine(`  - ${path.basename(f)}`);
						});

						// Display useful manifest info
						for (const clock of manifest.topClocks) {
							this.outputChannel.appendLine(
								`✓ Clock ${clock.port}: ${clock.frequencyMHz.toFixed(2)} MHz ` +
								`(domain ${clock.domain})`
							);
						}

						const { clocks, resets } = this.manifestParser.getClockResetPorts(manifest);
						if (clocks.length === 0) {
							this.outputChannel.appendLine('✓ No clock ports — combinational design');
						}
						if (resets.length > 0) {
							this.outputChannel.appendLine(`✓ Reset signals: ${resets.join(', ')}`);
						}

						resolve({
							success: true,
							verilogPath,
							allVerilogFiles,
							manifest,
							errors: [],
							warnings,
							output: stdout + stderr
						});
					} catch (err) {
						const message = err instanceof Error ? err.message : String(err);
						this.outputChannel.appendLine(`✗ ${message}`);
						resolve({
							success: false,
							errors: [message],
							warnings,
							output: stdout + stderr
						});
					}
				} else {
					this.outputChannel.appendLine(`✗ Compilation failed with code ${code}`);
					resolve({
						success: false,
						errors: errors.length > 0 ? errors : ['Compilation failed'],
						warnings,
						output: stdout + stderr
					});
				}
			});
		});
	}

	/**
	 * Parse Clash error messages and create diagnostics
	 */
	parseDiagnostics(
		output: string,
		wrapperPath: string
	): vscode.Diagnostic[] {
		const diagnostics: vscode.Diagnostic[] = [];

		// Match GHC/Clash error locations in all three formats:
		//   path/file.hs:12:5: error:            (point)
		//   path/file.hs:12:5-8: error:          (column span)
		//   path/file.hs:(12,5)-(14,10): error:  (block span)
		const errorRegex =
			/([^\n:][^\n]*?):(?:(\d+):(\d+)(?:-\d+)?|\((\d+),(\d+)\)-\(\d+,\d+\)):\s*(error|warning):\s*(.+?)(?=\n[^\s]|\n*$)/gs;

		let match;
		while ((match = errorRegex.exec(output)) !== null) {
			const [, filePath, pointLine, pointCol, spanLine, spanCol, severity, message] = match;
			const lineStr = pointLine ?? spanLine;
			const colStr = pointCol ?? spanCol;

			// Only create diagnostics for the wrapper file
			if (filePath.includes(path.basename(wrapperPath))) {
				const line = parseInt(lineStr, 10) - 1; // VS Code uses 0-based
				const col = parseInt(colStr, 10) - 1;

				const diagnostic = new vscode.Diagnostic(
					new vscode.Range(line, col, line, col + 10),
					message.trim(),
					severity === 'error'
						? vscode.DiagnosticSeverity.Error
						: vscode.DiagnosticSeverity.Warning
				);

				diagnostic.source = 'Clash';
				diagnostics.push(diagnostic);
			}
		}

		return diagnostics;
	}
}
