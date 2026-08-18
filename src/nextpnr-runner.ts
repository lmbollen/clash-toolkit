import * as vscode from 'vscode';
import { spawn } from 'child_process';
import * as path from 'path';
import * as os from 'os';
import { promises as fs } from 'fs';
import {
	NextpnrOptions,
	NextpnrResult,
	NextpnrWarning,
	NextpnrError,
	TimingInfo,
	UtilizationInfo,
	CriticalPath,
	PNR_FAMILIES
} from './nextpnr-types';
import { resolveJobCount } from './parallelism';
import { getLogger } from './file-logger';
import { toolInvocation } from './toolchain';
import { resolveTool, toolSpawnEnv } from './tool-provider';

/**
 * Shape of the JSON produced by nextpnr's `--report` flag.
 *
 * Empirically (nextpnr 2024 builds):
 *   fmax: { "<clock-net>": { achieved: MHz, constraint: MHz } }
 *   utilization: { "<cell-type>": { used, available } }
 *   critical_paths: [{ from, to, path: [{ delay, type, ... }] }]
 */
export interface NextpnrReport {
	fmax?: Record<string, { achieved?: number; constraint?: number }>;
	utilization?: Record<string, { used?: number; available?: number }>;
	critical_paths?: Array<{
		from?: string;
		to?: string;
		path?: Array<{
			delay?: number;
			type?: string;
			net?: string;
			from?: { cell?: string; port?: string };
			to?: { cell?: string; port?: string };
		}>;
	}>;
}

/**
 * Handles nextpnr place-and-route execution
 */
export class NextpnrRunner {
	private outputChannel: vscode.OutputChannel;

	constructor(outputChannel: vscode.OutputChannel) {
		this.outputChannel = outputChannel;
	}

	/**
	 * Run nextpnr place-and-route
	 */
	async placeAndRoute(options: NextpnrOptions): Promise<NextpnrResult> {
		this.outputChannel.appendLine('\n=== Place and Route with nextpnr ===');
		this.outputChannel.appendLine(`Family: ${options.family}`);
		this.outputChannel.appendLine(`Top Module: ${options.topModule}`);
		this.outputChannel.appendLine(`JSON: ${options.jsonPath}`);

		// Ensure output directory exists
		await fs.mkdir(options.outputDir, { recursive: true });

		// Determine output paths
		const textcfgPath = path.join(options.outputDir, `${options.topModule}.config`);
		const reportJsonPath = path.join(options.outputDir, 'report.json');
		const routedSvgPath = options.routedSvg
			? path.join(options.outputDir, `${options.topModule}.routed.svg`)
			: undefined;

		// Build nextpnr command
		const args = NextpnrRunner.buildNextpnrArgs(options, textcfgPath, {
			reportJsonPath,
			routedSvgPath,
		});
		// The user may point this family's binary somewhere else, or put a
		// wrapper in front of it; those leading args go before ours.
		const { command, args: wrapperArgs } =
			toolInvocation(NextpnrRunner.getExecutable(options.family));
		const fullArgs = [...wrapperArgs, ...args];

		this.outputChannel.appendLine(`\nRunning: ${command} ${fullArgs.join(' ')}`);
		this.outputChannel.appendLine('');

		// Run nextpnr
		const nextpnrResult = await this.runNextpnr(command, fullArgs, options, {
			reportJsonPath,
			routedSvgPath,
		});

		return nextpnrResult;
	}

	/**
	 * Get the nextpnr executable name for a given FPGA family.
	 */
	static getExecutable(family: string): string {
		switch (family) {
			case 'ecp5':
				return 'nextpnr-ecp5';
			case 'ice40':
				return 'nextpnr-ice40';
			case 'gowin':
				return 'nextpnr-himbaechel';
			default:
				return 'nextpnr-generic';
		}
	}

	/** Known package names per nextpnr family to probe. */
	private static readonly CANDIDATE_PACKAGES: Record<string, string[]> = {
		'nextpnr-ecp5': [
			'CABGA256', 'CABGA381', 'CABGA554', 'CABGA756',
			'CSFBGA285', 'CSFBGA381', 'CSFBGA554',
		],
		'nextpnr-ice40': [
			'sg48', 'cm36', 'cm49', 'cm81', 'cm121', 'cm225',
			'qn84', 'cb81', 'cb121', 'cb132', 'vq100', 'tq144',
			'ct256', 'bg121',
		],
		'nextpnr-himbaechel': [],  // gowin packages depend heavily on part; skip probing
	};

	/** Cache: "binary:device" → valid packages (survives for the session). */
	private static packageCache = new Map<string, string[]>();

	/** Monotonic id so concurrent package probes get distinct temp files. */
	private static probeCounter = 0;

	/**
	 * Discover which packages a nextpnr binary accepts for the given device
	 * by probing each candidate package.
	 *
	 * @param device    Device value (e.g. '25k', 'hx8k', 'GW1N-9')
	 * @param binary    nextpnr binary name (default: 'nextpnr-ecp5')
	 * @param deviceFlag  How to pass the device: 'prefix' → `--<device>`, 'device' → `--device <device>`
	 *
	 * Results are cached per binary+device so the probe only runs once per session.
	 */
	static async getValidPackages(
		device: string,
		binary = 'nextpnr-ecp5',
		deviceFlag: 'prefix' | 'device' = 'prefix'
	): Promise<string[]> {
		const cacheKey = `${binary}:${device}`;
		const cached = NextpnrRunner.packageCache.get(cacheKey);
		if (cached) { return cached; }

		const candidates = NextpnrRunner.CANDIDATE_PACKAGES[binary] ?? [];
		if (candidates.length === 0) {
			NextpnrRunner.packageCache.set(cacheKey, []);
			return [];
		}

		// Write a minimal valid JSON netlist so nextpnr can parse it.
		// Using /dev/null causes a JSON parse error that masks real
		// "Unsupported package" errors.
		// Keyed by PID *and* a counter: two overlapping probes in the same
		// extension host must not share a file, or one call's cleanup unlinks
		// the other's input mid-probe (an unreadable input is then
		// misclassified as a valid package).
		const probeJson = path.join(
			os.tmpdir(),
			`nextpnr-probe-${process.pid}-${NextpnrRunner.probeCounter++}.json`
		);
		await fs.writeFile(probeJson, '{"modules":{}}');

		const deviceArgs = deviceFlag === 'prefix'
			? [`--${device}`]
			: ['--device', device];

		try {
			const { command, args: wrapperArgs } = toolInvocation(binary);
			const probes = candidates.map(pkg =>
				new Promise<{ pkg: string; ok: boolean }>(resolve => {
					const resolvedBinary = resolveTool(command);
					const proc = spawn(resolvedBinary, [
						...wrapperArgs,
						...deviceArgs, '--package', pkg, '--json', probeJson,
					], { timeout: 5000, env: toolSpawnEnv(resolvedBinary), windowsHide: true });

					let combined = '';
					proc.stdout.on('data', (d) => { combined += d.toString(); });
					proc.stderr.on('data', (d) => { combined += d.toString(); });
					proc.on('error', () => resolve({ pkg, ok: false }));
					proc.on('close', () => {
						// The empty netlist always causes a non-zero exit
						// ("no top module"), so we can't rely on the exit code.
						// A package is valid if nextpnr didn't reject it.
						const rejected = /unsupported package/i.test(combined);
						resolve({ pkg, ok: !rejected });
					});
				})
			);
			const results = await Promise.all(probes);
			const valid: string[] = [];
			for (const { pkg, ok } of results) {
				if (ok) { valid.push(pkg); }
			}

			NextpnrRunner.packageCache.set(cacheKey, valid);
			return valid;
		} finally {
			try { await fs.unlink(probeJson); } catch { /* ignore */ }
		}
	}

	/**
	 * Build nextpnr command-line arguments.
	 *
	 * Emits `--report <json>` and `--detailed-timing-report` by default so we
	 * get structured timing/utilization data instead of having to scrape the
	 * text log.  `--timing-allow-fail` is also on by default so a slow design
	 * still produces a usable Fmax report instead of hard-failing PNR.
	 */
	static buildNextpnrArgs(
		options: NextpnrOptions,
		textcfgPath: string,
		extras: { reportJsonPath?: string; routedSvgPath?: string } = {}
	): string[] {
		const args: string[] = [];

		// Input JSON
		args.push('--json', options.jsonPath);

		// Output flag varies by family
		switch (options.family) {
			case 'ice40':  args.push('--asc', textcfgPath); break;
			case 'gowin':  args.push('--write', textcfgPath); break;
			default:       args.push('--textcfg', textcfgPath); break;
		}

		// ECP5-specific options (legacy path)
		if (options.family === 'ecp5' && options.ecp5) {
			args.push('--' + options.ecp5.device);
			args.push('--package', options.ecp5.package);

			if (options.ecp5.speedGrade) {
				args.push('--speed', options.ecp5.speedGrade);
			}
		}

		// Generic device / package (used for ice40, gowin, and new-style ecp5)
		if (options.device && !options.ecp5) {
			const familyInfo = PNR_FAMILIES.get(options.family);
			if (familyInfo?.deviceFlag === 'prefix') {
				// e.g. --hx8k, --25k
				args.push('--' + options.device);
			} else {
				// e.g. --device GW1N-9
				args.push('--device', options.device);
			}
		}
		if (options.packageName && !options.ecp5) {
			args.push('--package', options.packageName);
		}

		// Extra --vopt (e.g. family=GW1N-9C for Gowin himbaechel)
		if (options.vopt) {
			for (const v of options.vopt) {
				args.push('--vopt', v);
			}
		}

		// Constraints file
		if (options.constraintsFile) {
			switch (options.family) {
				case 'ecp5': args.push('--lpf', options.constraintsFile); break;
				// The gowin executable is nextpnr-himbaechel, which has no
				// --cst option — constraints go through --vopt cst=<file>.
				case 'gowin': args.push('--vopt', `cst=${options.constraintsFile}`); break;
				default: args.push('--pcf', options.constraintsFile); break;
			}
		}

		// Frequency constraint
		if (options.frequency) {
			args.push('--freq', options.frequency.toString());
		}

		// Seed for reproducibility
		if (options.seed !== undefined) {
			args.push('--seed', options.seed.toString());
		}

		// Threading.  Capped at 4 on `auto`: nextpnr only threads some passes,
		// and past a handful of threads the routing phase stops gaining. A user
		// who sets nextpnrThreads explicitly is not capped.
		args.push('--threads', String(resolveJobCount(options.threads, { cap: 4 })));

		// Structured output — always enabled.  The JSON report is our primary
		// source for timing/utilization; detailed-timing-report adds per-net
		// data; timing-allow-fail lets analysis runs complete even when the
		// design doesn't meet the user's SDC/--freq constraint.
		if (extras.reportJsonPath) {
			args.push('--report', extras.reportJsonPath);
			args.push('--detailed-timing-report');
		}
		args.push('--timing-allow-fail');

		// Optional routed-layout SVG
		if (extras.routedSvgPath) {
			args.push('--routed-svg', extras.routedSvgPath);
		}

		// Additional arguments
		if (options.extraArgs) {
			args.push(...options.extraArgs);
		}

		return args;
	}

	/**
	 * Execute nextpnr.
	 *
	 * @internal — public so regression tests can drive it with a fake binary.
	 * Not part of the extension's public API.
	 */
	async runNextpnr(
		executable: string,
		args: string[],
		options: NextpnrOptions,
		paths: { reportJsonPath: string; routedSvgPath?: string }
	): Promise<NextpnrResult> {
		const { reportJsonPath, routedSvgPath } = paths;
		return new Promise((resolve) => {
			let stdout = '';
			let stderr = '';
			let resolved = false;
			let cancelled = false;
			const warnings: NextpnrWarning[] = [];
			const errors: NextpnrError[] = [];

			const finalize = (result: NextpnrResult) => {
				if (resolved) { return; }
				resolved = true;
				options.abortSignal?.removeEventListener('abort', onAbort);
				resolve(result);
			};

			const logger = getLogger();
			const resolvedExecutable = resolveTool(executable);
			const finishLog = logger?.command(resolvedExecutable, args);
			const nextpnr = spawn(resolvedExecutable, args, {
				env: toolSpawnEnv(resolvedExecutable),
				windowsHide: true,
			});

			const onAbort = () => {
				cancelled = true;
				this.outputChannel.appendLine('\n⏹  Cancellation requested — stopping nextpnr…');
				try { nextpnr.kill('SIGTERM'); } catch { /* already exited */ }
				// Hard kill if it doesn't honour SIGTERM quickly
				setTimeout(() => {
					try { nextpnr.kill('SIGKILL'); } catch { /* gone */ }
				}, 3000);
			};
			if (options.abortSignal) {
				if (options.abortSignal.aborted) { onAbort(); }
				else { options.abortSignal.addEventListener('abort', onAbort); }
			}

			// Last few lines tracked so we can echo them in the failure error.
			const recentLines: string[] = [];
			const pushLine = (line: string) => {
				const trimmed = line.trim();
				if (!trimmed) { return; }
				recentLines.push(trimmed);
				if (recentLines.length > 8) { recentLines.shift(); }
			};

			// Heuristic progress phases: nextpnr prints these as it advances.
			const progressPatterns: Array<[RegExp, string]> = [
				[/packing/i, 'Packing design'],
				[/placing/i, 'Placing'],
				[/place_constraints/i, 'Placing constraints'],
				[/heap_placer|heap placement|sa placement|Run #\d+/i, 'Placement search'],
				[/routing/i, 'Routing'],
				[/Critical path/i, 'Timing analysis'],
				[/writing.*config|generated.*\.config/i, 'Writing config'],
			];
			const reportProgress = (line: string) => {
				if (!options.progressUpdate) { return; }
				for (const [re, label] of progressPatterns) {
					if (re.test(line)) {
						options.progressUpdate(label);
						return;
					}
				}
			};

			// Per-stream buffers so a line straddling a pipe-chunk boundary is
			// reassembled before matching, instead of being seen as two
			// fragments neither of which matches.
			let lineBufOut = '';
			let lineBufErr = '';
			const processLine = (line: string) => {
				pushLine(line);
				reportProgress(line);
				const lower = line.trim().toLowerCase();
				// Anchored: nextpnr prefixes messages with "Warning:"/"ERROR:",
				// and substring matching would record e.g. a path containing
				// "error" as an error.
				if (lower.startsWith('warning')) { warnings.push({ message: line.trim() }); }
				if (lower.startsWith('error') || lower.startsWith('fatal')) {
					errors.push({ message: line.trim() });
				}
			};
			const handleStream = (text: string, isStderr: boolean) => {
				this.outputChannel.append(text);
				if (isStderr) { stderr += text; } else { stdout += text; }
				const buffered = (isStderr ? lineBufErr : lineBufOut) + text;
				const lines = buffered.split(/\r?\n/);
				const rest = lines.pop() ?? '';
				if (isStderr) { lineBufErr = rest; } else { lineBufOut = rest; }
				for (const line of lines) { processLine(line); }
			};

			nextpnr.stdout.on('data', (data) => handleStream(data.toString(), false));
			nextpnr.stderr.on('data', (data) => handleStream(data.toString(), true));

			nextpnr.on('error', (error) => {
				this.outputChannel.appendLine(`\nERROR: Failed to spawn ${executable}: ${error.message}`);
				this.outputChannel.appendLine('Make sure nextpnr is installed and in your PATH');
				finishLog?.then(fn => fn(null));
				finalize({
					success: false,
					output: stdout + stderr,
					warnings,
					errors: [{ message: error.message }]
				});
			});

			// Use 'close' so stdout/stderr have fully drained before we parse
			// timing/utilization or check for written report files. ('exit'
			// fires earlier and would give us truncated output.)
			const handleTermination = async (code: number | null, signal: NodeJS.Signals | null) => {
				finishLog?.then(fn => fn(code));
				this.outputChannel.appendLine('');
				this.outputChannel.appendLine(
					`nextpnr ${signal ? `terminated by ${signal}` : `exited with code ${code}`}`
				);

				const logPath = path.join(options.outputDir, 'nextpnr.log');
				const fullOutput = stdout + stderr;
				try {
					await fs.writeFile(logPath, fullOutput, 'utf8');
					this.outputChannel.appendLine(`Log saved: ${logPath}`);
				} catch (err) {
					this.outputChannel.appendLine(`Warning: Could not save log file: ${err}`);
				}

				if (cancelled) {
					finalize({
						success: false,
						output: fullOutput,
						warnings,
						errors: [{ message: 'Cancelled by user' }]
					});
					return;
				}

				if (code === 0) {
					this.outputChannel.appendLine('✓ Place and route successful');

					// The structured JSON report is required — we always pass
					// --report, so a missing/unparseable file means something is
					// wrong and must fail loudly rather than silently degrading
					// to text-scraped guesses. The text parse of the combined
					// output (nextpnr logs to STDERR) only *supplements* fields
					// the report lacks, e.g. the pre-placement estimate.
					const report = await NextpnrRunner.loadReportJson(reportJsonPath);
					if (!report) {
						const message =
							`nextpnr exited successfully but its JSON report (${reportJsonPath}) ` +
							'is missing or unparseable — timing/utilization results cannot be trusted.';
						this.outputChannel.appendLine(`✗ ${message}`);
						finalize({
							success: false,
							output: fullOutput,
							warnings,
							errors: [{ message }]
						});
						return;
					}
					const timing = NextpnrRunner.timingFromReport(report, this.parseTiming(fullOutput));
					const utilization = NextpnrRunner.utilizationFromReport(report, options.family)
						?? this.parseUtilization(fullOutput);
					const criticalPaths = report
						? NextpnrRunner.criticalPathsFromReport(report)
						: [];

					// Display timing analysis in output channel
					if (timing) {
						this.outputChannel.appendLine('');
						this.outputChannel.appendLine('Timing Analysis:');
						this.outputChannel.appendLine('-'.repeat(60));
						if (timing.prePlacementFrequency !== undefined) {
							this.outputChannel.appendLine(`  Pre-Routing Est.:  ${timing.prePlacementFrequency.toFixed(2)} MHz (after placement)`);
						}
						if (timing.maxFrequency !== undefined) {
							this.outputChannel.appendLine(`  Max Frequency:     ${timing.maxFrequency.toFixed(2)} MHz (after routing)`);
						}
						if (timing.criticalPathDelay !== undefined) {
							this.outputChannel.appendLine(`  Critical Path:     ${timing.criticalPathDelay.toFixed(2)} ns`);
						}
						if (timing.setupSlack !== undefined) {
							this.outputChannel.appendLine(`  Setup Slack:       ${timing.setupSlack.toFixed(2)} ns`);
						}
						if (timing.holdSlack !== undefined) {
							this.outputChannel.appendLine(`  Hold Slack:        ${timing.holdSlack.toFixed(2)} ns`);
						}
						this.outputChannel.appendLine(`  Constraints:       ${timing.constraintsMet ? '✓ MET' : '✗ FAILED'}`);
					}

					// Display utilization in output channel
					if (utilization) {
						this.outputChannel.appendLine('');
						this.outputChannel.appendLine('Resource Utilization:');
						this.outputChannel.appendLine('-'.repeat(60));
						if (utilization.luts) {
							const pct = ((utilization.luts.used / utilization.luts.total) * 100).toFixed(1);
							this.outputChannel.appendLine(`  LUTs:      ${utilization.luts.used.toString().padStart(5)}/${utilization.luts.total.toString().padEnd(5)} (${pct}%)`);
						}
						if (utilization.registers) {
							const pct = ((utilization.registers.used / utilization.registers.total) * 100).toFixed(1);
							this.outputChannel.appendLine(`  Registers: ${utilization.registers.used.toString().padStart(5)}/${utilization.registers.total.toString().padEnd(5)} (${pct}%)`);
						}
						if (utilization.bram) {
							const pct = ((utilization.bram.used / utilization.bram.total) * 100).toFixed(1);
							this.outputChannel.appendLine(`  BRAM:      ${utilization.bram.used.toString().padStart(5)}/${utilization.bram.total.toString().padEnd(5)} (${pct}%)`);
						}
						if (utilization.dsp) {
							const pct = ((utilization.dsp.used / utilization.dsp.total) * 100).toFixed(1);
							this.outputChannel.appendLine(`  DSP:       ${utilization.dsp.used.toString().padStart(5)}/${utilization.dsp.total.toString().padEnd(5)} (${pct}%)`);
						}
						if (utilization.io) {
							const pct = ((utilization.io.used / utilization.io.total) * 100).toFixed(1);
							this.outputChannel.appendLine(`  IO:        ${utilization.io.used.toString().padStart(5)}/${utilization.io.total.toString().padEnd(5)} (${pct}%)`);
						}
					}

					// Save timing and utilization reports
					this.outputChannel.appendLine('');
					try {
						if (timing) {
							const timingReport = this.formatTimingReport(timing);
							const timingPath = path.join(options.outputDir, 'timing.txt');
							await fs.writeFile(timingPath, timingReport, 'utf8');
							this.outputChannel.appendLine(`Timing report saved: ${timingPath}`);
						}
						if (utilization) {
							const utilReport = this.formatUtilizationReport(utilization);
							const utilPath = path.join(options.outputDir, 'utilization.txt');
							await fs.writeFile(utilPath, utilReport, 'utf8');
							this.outputChannel.appendLine(`Utilization report saved: ${utilPath}`);
						}
						if (timing || utilization) {
							const summaryReport = this.formatSummaryReport(timing, utilization, options.topModule);
							const summaryPath = path.join(options.outputDir, 'summary.txt');
							await fs.writeFile(summaryPath, summaryReport, 'utf8');
							this.outputChannel.appendLine(`Summary report saved: ${summaryPath}`);
						}
					} catch (err) {
						this.outputChannel.appendLine(`Warning: Could not save reports: ${err}`);
					}

					const textcfgPath = path.join(options.outputDir, `${options.topModule}.config`);
					const reportExists = await fs.access(reportJsonPath).then(() => true, () => false);
					const svgExists = routedSvgPath
						? await fs.access(routedSvgPath).then(() => true, () => false)
						: false;

					finalize({
						success: true,
						textcfgPath,
						output: fullOutput,
						timing,
						utilization,
						criticalPaths: criticalPaths.length > 0 ? criticalPaths : undefined,
						reportJsonPath: reportExists ? reportJsonPath : undefined,
						routedSvgPath: svgExists ? routedSvgPath : undefined,
						warnings,
						errors
					});
				} else {
					this.outputChannel.appendLine(`✗ Place and route failed with code ${code}`);
					// Promote the most useful failure line so the caller can
					// surface it without forcing the user to scroll the log.
					const headlineError = errors.find(e => /\b(error|fatal)\b/i.test(e.message))
						?? (recentLines.length > 0 ? { message: recentLines[recentLines.length - 1] } : undefined);
					const finalErrors: NextpnrError[] = headlineError
						? [headlineError, ...errors.filter(e => e !== headlineError)]
						: errors;
					if (finalErrors.length === 0) {
						finalErrors.push({ message: `nextpnr exited with code ${code}` });
					}
					finalize({
						success: false,
						output: fullOutput,
						warnings,
						errors: finalErrors
					});
				}
			};

			nextpnr.on('close', handleTermination);
		});
	}

	/** Read and parse the JSON report written by nextpnr's `--report` flag. */
	static async loadReportJson(reportJsonPath: string): Promise<NextpnrReport | undefined> {
		try {
			const raw = await fs.readFile(reportJsonPath, 'utf8');
			return JSON.parse(raw) as NextpnrReport;
		} catch {
			return undefined;
		}
	}

	/**
	 * Derive our TimingInfo shape from the JSON report.
	 *
	 * `fmax` contains one entry per clock domain; we take the worst (slowest)
	 * achieved frequency as the design Fmax.  Pre-placement frequency and
	 * individual setup/hold slack aren't in the JSON, so we keep any values
	 * the text parser pulled out (e.g. slack from nextpnr's own log lines).
	 */
	static timingFromReport(
		report: NextpnrReport,
		textFallback?: TimingInfo
	): TimingInfo {
		const timing: TimingInfo = textFallback
			? { ...textFallback, constraintsMet: textFallback.constraintsMet }
			: { constraintsMet: true };

		// Fmax across clock domains.
		const fmaxEntries = Object.values(report.fmax ?? {});
		if (fmaxEntries.length > 0) {
			const achieved = fmaxEntries
				.map(e => e.achieved)
				.filter((x): x is number => typeof x === 'number');
			if (achieved.length > 0) {
				timing.maxFrequency = Math.min(...achieved);
			}
			// Constraint met iff every constrained clock achieved its target.
			const missed = fmaxEntries.some(
				e => typeof e.achieved === 'number'
					&& typeof e.constraint === 'number'
					&& e.achieved < e.constraint
			);
			if (missed) { timing.constraintsMet = false; }
		}

		// Critical-path delay: take the longest sum-of-delays across all paths.
		const paths = report.critical_paths ?? [];
		if (paths.length > 0) {
			let worst = 0;
			for (const p of paths) {
				const total = (p.path ?? []).reduce(
					(acc, step) => acc + (step.delay ?? 0),
					0
				);
				if (total > worst) { worst = total; }
			}
			if (worst > 0) { timing.criticalPathDelay = worst; }
		}

		return timing;
	}

	/**
	 * Extract critical paths from the JSON report, sorted slowest-first.
	 * Empty array when no paths are reported (e.g. purely combinational
	 * design or no clock constraints).
	 */
	static criticalPathsFromReport(report: NextpnrReport): CriticalPath[] {
		const paths: CriticalPath[] = [];
		for (const p of report.critical_paths ?? []) {
			const steps = (p.path ?? []).map(step => ({
				delay: step.delay ?? 0,
				type: step.type ?? 'unknown',
				fromCell: step.from?.cell,
				toCell: step.to?.cell,
				net: step.net,
			}));
			const totalDelay = steps.reduce((a, s) => a + s.delay, 0);
			paths.push({
				from: p.from ?? '<unknown>',
				to: p.to ?? '<unknown>',
				totalDelay,
				steps,
			});
		}
		paths.sort((a, b) => b.totalDelay - a.totalDelay);
		return paths;
	}

	/**
	 * Derive our UtilizationInfo shape from the JSON report's `utilization` map.
	 *
	 * nextpnr reports one entry per primitive cell type (family-specific names
	 * like TRELLIS_FF, SB_LUT4, DP16KD, ...).  We aggregate them into the
	 * canonical LUT/FF/BRAM/DSP/IO buckets the rest of the extension expects.
	 */
	static utilizationFromReport(
		report: NextpnrReport,
		family: string
	): UtilizationInfo | undefined {
		const util = report.utilization;
		if (!util) { return undefined; }

		// Per-family cell-type → bucket map, covering the primitive names the
		// corresponding `synth_*` pass emits.
		const buckets: Record<string, { lut: RegExp; ff: RegExp; bram: RegExp; dsp: RegExp; io: RegExp }> = {
			ecp5: {
				lut: /^TRELLIS_COMB$|^TRELLIS_SLICE$|^LUT4$/,
				ff: /^TRELLIS_FF$/,
				bram: /^DP16KD$|^PDPW16KD$/,
				dsp: /^MULT18X18D$|^ALU54B$/,
				io: /^TRELLIS_IO$|^SIOLOGIC$|^IOLOGIC$/,
			},
			ice40: {
				lut: /^SB_LUT4$/,
				ff: /^SB_DFF/,
				bram: /^SB_RAM40_4K$/,
				dsp: /^SB_MAC16$/,
				io: /^SB_IO$/,
			},
			gowin: {
				lut: /^LUT[1-6]$/,
				ff: /^DFF[A-Z]*$/,
				bram: /^BSRAM$|^RAM16S|^B?SRAM/,
				dsp: /^MULT|^ALU/,
				io: /^IOB|^IDDR|^ODDR/,
			},
		};

		// Generic heuristic for unknown families: substring match.
		const rules = buckets[family] ?? {
			lut: /LUT|COMB/i,
			ff: /FF|DFF/i,
			bram: /BRAM|RAM|EBR/i,
			dsp: /DSP|MULT|MAC/i,
			io: /\bIO\b|IOB|PAD/i,
		};

		const sum = (rx: RegExp): { used: number; total: number } | undefined => {
			let used = 0;
			let total = 0;
			let matched = false;
			for (const [name, entry] of Object.entries(util)) {
				if (!rx.test(name)) { continue; }
				matched = true;
				used += entry.used ?? 0;
				total += entry.available ?? 0;
			}
			// A bucket with no known capacity would render as NaN%/Infinity%
			// downstream — omit it rather than divide by zero.
			return matched && total > 0 ? { used, total } : undefined;
		};

		const info: UtilizationInfo = {
			luts: sum(rules.lut),
			registers: sum(rules.ff),
			bram: sum(rules.bram),
			dsp: sum(rules.dsp),
			io: sum(rules.io),
		};

		// Drop any undefined buckets so the result serialises cleanly.
		for (const k of Object.keys(info) as (keyof UtilizationInfo)[]) {
			if (info[k] === undefined) { delete info[k]; }
		}
		return Object.keys(info).length > 0 ? info : undefined;
	}

	/**
	 * Parse timing information from nextpnr output
	 */
	private parseTiming(output: string): TimingInfo | undefined {
		const timing: TimingInfo = {
			constraintsMet: true
		};

		// Parse max frequencies
		// nextpnr reports frequency twice:
		//   1. After placement (higher, optimistic)
		//   2. After routing (lower, actual achievable frequency)
		// We want the LAST occurrence (post-routing)
		const freqRegex = /Max frequency for clock.*?:\s*([\d.]+)\s*MHz/gi;
		const freqMatches = [...output.matchAll(freqRegex)];
		
		if (freqMatches.length > 0) {
			if (freqMatches.length === 1) {
				// Only one frequency reported (simple design or no routing)
				timing.maxFrequency = parseFloat(freqMatches[0][1]);
			} else {
				// Multiple frequencies: first is pre-routing estimate, last is post-routing actual
				timing.prePlacementFrequency = parseFloat(freqMatches[0][1]);
				timing.maxFrequency = parseFloat(freqMatches[freqMatches.length - 1][1]);
			}
		}

		// Parse critical path delay
		// Look for "X.XX ns logic, Y.YY ns routing" pattern which gives total delay
		const logicRoutingMatch = output.match(/([\d.]+)\s*ns logic,\s*([\d.]+)\s*ns routing/i);
		if (logicRoutingMatch) {
			const logicDelay = parseFloat(logicRoutingMatch[1]);
			const routingDelay = parseFloat(logicRoutingMatch[2]);
			timing.criticalPathDelay = logicDelay + routingDelay;
		} else {
			// Fallback: "Critical path delay: 12.34 ns"
			const delayMatch = output.match(/Critical path.*?delay:\s*([\d.]+)\s*ns/i);
			if (delayMatch) {
				timing.criticalPathDelay = parseFloat(delayMatch[1]);
			}
		}

		// Parse slack
		const setupSlackMatch = output.match(/Setup slack.*?:\s*([-\d.]+)\s*ns/i);
		if (setupSlackMatch) {
			timing.setupSlack = parseFloat(setupSlackMatch[1]);
			if (timing.setupSlack < 0) {
				timing.constraintsMet = false;
			}
		}

		const holdSlackMatch = output.match(/Hold slack.*?:\s*([-\d.]+)\s*ns/i);
		if (holdSlackMatch) {
			timing.holdSlack = parseFloat(holdSlackMatch[1]);
			if (timing.holdSlack < 0) {
				timing.constraintsMet = false;
			}
		}

		// Check for timing failure messages
		if (output.includes('FAIL') || output.includes('not met')) {
			timing.constraintsMet = false;
		}

		// No parseable timing data → report "no data" instead of fabricating
		// a constraintsMet:true verdict backed by nothing.
		const hasData = timing.maxFrequency !== undefined
			|| timing.prePlacementFrequency !== undefined
			|| timing.criticalPathDelay !== undefined
			|| timing.setupSlack !== undefined
			|| timing.holdSlack !== undefined;
		return hasData ? timing : undefined;
	}

	/**
	 * Parse utilization information from nextpnr output
	 */
	private parseUtilization(output: string): UtilizationInfo | undefined {
		const util: UtilizationInfo = {};

		// Parse LUT usage (ECP5 format): "Total LUT4s:         8/24288     0%"
		const lutMatch = output.match(/Total LUT4s?:\s*(\d+)\/(\d+)/i);
		if (lutMatch) {
			util.luts = {
				used: parseInt(lutMatch[1]),
				total: parseInt(lutMatch[2])
			};
		} else {
			// Try older format: "LUT:  1234/12000"
			const legacyLutMatch = output.match(/LUT:\s*(\d+)\/(\d+)/i);
			if (legacyLutMatch) {
				util.luts = {
					used: parseInt(legacyLutMatch[1]),
					total: parseInt(legacyLutMatch[2])
				};
			}
		}

		// Parse register usage (ECP5 format): "TRELLIS_FF:       8/  24288     0%"
		const ffMatch = output.match(/TRELLIS_FF:\s*(\d+)\s*\/\s*(\d+)/i);
		if (ffMatch) {
			util.registers = {
				used: parseInt(ffMatch[1]),
				total: parseInt(ffMatch[2])
			};
		} else {
			// Try "Total DFFs:         8/24288     0%"
			const dffMatch = output.match(/Total DFFs?:\s*(\d+)\/(\d+)/i);
			if (dffMatch) {
				util.registers = {
					used: parseInt(dffMatch[1]),
					total: parseInt(dffMatch[2])
				};
			} else {
				// Fallback to legacy formats
				const legacyRegMatch = output.match(/(?:DFF|REG):\s*(\d+)\/(\d+)/i);
				if (legacyRegMatch) {
					util.registers = {
						used: parseInt(legacyRegMatch[1]),
						total: parseInt(legacyRegMatch[2])
					};
				}
			}
		}

		// Parse BRAM usage (ECP5 format): "DP16KD:       0/     56     0%"
		const dp16kdMatch = output.match(/DP16KD:\s*(\d+)\s*\/\s*(\d+)/i);
		if (dp16kdMatch) {
			util.bram = {
				used: parseInt(dp16kdMatch[1]),
				total: parseInt(dp16kdMatch[2])
			};
		} else {
			// Try legacy formats: "BRAM:  5/56" or "EBR:  5/56"
			const bramMatch = output.match(/(?:BRAM|EBR):\s*(\d+)\/(\d+)/i);
			if (bramMatch) {
				util.bram = {
					used: parseInt(bramMatch[1]),
					total: parseInt(bramMatch[2])
				};
			}
		}

		// Parse DSP usage: "DSP:  0/28"
		const dspMatch = output.match(/DSP:\s*(\d+)\/(\d+)/i);
		if (dspMatch) {
			util.dsp = {
				used: parseInt(dspMatch[1]),
				total: parseInt(dspMatch[2])
			};
		}

		// Parse IO usage (ECP5 format): "TRELLIS_IO:      19/    197     9%"
		const trellisIoMatch = output.match(/TRELLIS_IO:\s*(\d+)\s*\/\s*(\d+)/i);
		if (trellisIoMatch) {
			util.io = {
				used: parseInt(trellisIoMatch[1]),
				total: parseInt(trellisIoMatch[2])
			};
		} else {
			// Try legacy format: "IO:  8/256"
			const ioMatch = output.match(/IO:\s*(\d+)\/(\d+)/i);
			if (ioMatch) {
				util.io = {
					used: parseInt(ioMatch[1]),
					total: parseInt(ioMatch[2])
				};
			}
		}

		return Object.keys(util).length > 0 ? util : undefined;
	}

	/**
	 * Format timing information for report file
	 */
	private formatTimingReport(timing: TimingInfo): string {
		let report = 'Timing Analysis Report\n';
		report += '='.repeat(60) + '\n';
		report += `Generated: ${new Date().toISOString()}\n\n`;

		if (timing.prePlacementFrequency !== undefined) {
			report += `Pre-Routing Est. Freq: ${timing.prePlacementFrequency.toFixed(2)} MHz\n`;
		}
		if (timing.maxFrequency !== undefined) {
			report += `Maximum Frequency:     ${timing.maxFrequency.toFixed(2)} MHz\n`;
		}

		if (timing.criticalPathDelay !== undefined) {
			report += `Critical Path Delay:   ${timing.criticalPathDelay.toFixed(3)} ns\n`;
		}

		if (timing.setupSlack !== undefined) {
			const status = timing.setupSlack >= 0 ? 'PASS' : 'FAIL';
			report += `Setup Slack:           ${timing.setupSlack.toFixed(3)} ns [${status}]\n`;
		}

		if (timing.holdSlack !== undefined) {
			const status = timing.holdSlack >= 0 ? 'PASS' : 'FAIL';
			report += `Hold Slack:            ${timing.holdSlack.toFixed(3)} ns [${status}]\n`;
		}

		report += '\n';
		report += `Overall Status:        ${timing.constraintsMet ? '✓ CONSTRAINTS MET' : '✗ CONSTRAINTS FAILED'}\n`;

		return report;
	}

	/**
	 * Format utilization information for report file
	 */
	private formatUtilizationReport(util: UtilizationInfo): string {
		let report = 'Resource Utilization Report\n';
		report += '='.repeat(60) + '\n';
		report += `Generated: ${new Date().toISOString()}\n\n`;

		const formatResource = (name: string, used: number, total: number): string => {
			const percent = ((used / total) * 100).toFixed(2);
			const bar = this.createBar(used, total, 40);
			return `${name.padEnd(15)} ${used.toString().padStart(6)} / ${total.toString().padEnd(6)} (${percent.padStart(6)}%)  ${bar}\n`;
		};

		if (util.luts) {
			report += formatResource('LUTs', util.luts.used, util.luts.total);
		}

		if (util.registers) {
			report += formatResource('Registers', util.registers.used, util.registers.total);
		}

		if (util.bram) {
			report += formatResource('BRAM/EBR', util.bram.used, util.bram.total);
		}

		if (util.dsp) {
			report += formatResource('DSP', util.dsp.used, util.dsp.total);
		}

		if (util.io) {
			report += formatResource('IO', util.io.used, util.io.total);
		}

		return report;
	}

	/**
	 * Format combined summary report with timing and utilization
	 */
	private formatSummaryReport(
		timing: TimingInfo | undefined, 
		utilization: UtilizationInfo | undefined,
		topModule: string
	): string {
		let report = 'nextpnr Place & Route Summary\n';
		report += '='.repeat(70) + '\n';
		report += `Module:    ${topModule}\n`;
		report += `Generated: ${new Date().toISOString()}\n`;
		report += '='.repeat(70) + '\n\n';

		// Timing Section
		if (timing) {
			report += 'TIMING ANALYSIS\n';
			report += '-'.repeat(70) + '\n';
			
			if (timing.prePlacementFrequency !== undefined) {
				report += `  Pre-Routing Estimate:  ${timing.prePlacementFrequency.toFixed(2)} MHz (after placement)\n`;
			}
			if (timing.maxFrequency !== undefined) {
				report += `  Maximum Frequency:     ${timing.maxFrequency.toFixed(2)} MHz (after routing)\n`;
				if (timing.prePlacementFrequency !== undefined) {
					const overhead = timing.prePlacementFrequency - timing.maxFrequency;
					const overheadPct = (overhead / timing.prePlacementFrequency * 100).toFixed(1);
					report += `  Routing Overhead:      ${overhead.toFixed(2)} MHz (${overheadPct}%)\n`;
				}
			}
			if (timing.criticalPathDelay !== undefined) {
				report += `  Critical Path Delay:   ${timing.criticalPathDelay.toFixed(3)} ns\n`;
			}
			if (timing.setupSlack !== undefined) {
				const status = timing.setupSlack >= 0 ? 'PASS' : 'FAIL';
				report += `  Setup Slack:           ${timing.setupSlack.toFixed(3)} ns [${status}]\n`;
			}
			if (timing.holdSlack !== undefined) {
				const status = timing.holdSlack >= 0 ? 'PASS' : 'FAIL';
				report += `  Hold Slack:            ${timing.holdSlack.toFixed(3)} ns [${status}]\n`;
			}
			report += `  Timing Constraints:    ${timing.constraintsMet ? '✓ MET' : '✗ FAILED'}\n`;
			report += '\n';
		}

		// Utilization Section
		if (utilization) {
			report += 'RESOURCE UTILIZATION\n';
			report += '-'.repeat(70) + '\n';
			
			const formatLine = (name: string, used: number, total: number): string => {
				const percent = ((used / total) * 100).toFixed(1);
				return `  ${name.padEnd(15)} ${used.toString().padStart(6)} / ${total.toString().padEnd(8)} (${percent.padStart(5)}%)\n`;
			};

			if (utilization.luts) {
				report += formatLine('LUTs:', utilization.luts.used, utilization.luts.total);
			}
			if (utilization.registers) {
				report += formatLine('Registers:', utilization.registers.used, utilization.registers.total);
			}
			if (utilization.bram) {
				report += formatLine('BRAM/EBR:', utilization.bram.used, utilization.bram.total);
			}
			if (utilization.dsp) {
				report += formatLine('DSP Blocks:', utilization.dsp.used, utilization.dsp.total);
			}
			if (utilization.io) {
				report += formatLine('IO Pins:', utilization.io.used, utilization.io.total);
			}
			report += '\n';
		}

		report += '='.repeat(70) + '\n';
		report += 'For detailed analysis, see timing.txt and utilization.txt\n';
		report += '='.repeat(70) + '\n';

		return report;
	}

	/**
	 * Create a text-based bar chart
	 */
	private createBar(used: number, total: number, width: number): string {
		const percent = total > 0 ? Math.min(1, used / total) : 0;
		const filled = Math.round(percent * width);
		const empty = width - filled;
		return '[' + '█'.repeat(filled) + '░'.repeat(empty) + ']';
	}
}
