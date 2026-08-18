import * as vscode from 'vscode';
import { spawn } from 'child_process';
import * as path from 'path';
import { promises as fs } from 'fs';
import {
	YosysOptions,
	YosysSynthesisResult,
	SynthesisStatistics,
	ModuleSynthesisResult,
	YosysWarning,
	YosysError
} from './yosys-types';
import { ComponentInfo } from './clash-manifest-types';
import { JobSetting, resolveJobCount } from './parallelism';
import { getLogger } from './file-logger';
import { getDefaultOutOfContextScript, getDefaultScript, resolveScript } from './synthesis-targets';
import { toolInvocation } from './toolchain';
import { resolveTool, toolSpawnEnv } from './tool-provider';
import { fireDiagramRender } from './netlist-diagram';

/**
 * `per-module/hierarchy.json` — the component graph of a per-module run, written
 * so the Run History view can rebuild the hierarchy from disk. Each entry maps a
 * component to the components it instantiates directly.
 */
export interface PerModuleHierarchy {
	/** Top component of the run. */
	top: string;
	/** True when the components were synthesized out of context. */
	outOfContext: boolean;
	/** component → direct sub-components. */
	components: Record<string, string[]>;
}

/**
 * Resolve the yosys executable + leading args from the user's
 * `clash-toolkit.toolCommands` overrides — the same command the pre-flight
 * toolchain check probes, so "check passes but synthesis spawns a different
 * yosys" cannot happen.
 */
function resolveYosysCommand(): { cmd: string; baseArgs: string[] } {
	const { command, args } = toolInvocation('yosys');
	// Resolve through the managed toolchain: falls back to a downloaded copy
	// when the user has no yosys on PATH, otherwise returns the name unchanged.
	return { cmd: resolveTool(command), baseArgs: args };
}

/**
 * Split a stdio chunk stream into complete lines, buffering partial lines
 * across chunk boundaries so a line straddling a pipe-chunk boundary is not
 * missed or double-counted by line-oriented matchers.
 */
function makeLineSplitter(onLine: (line: string) => void): (chunk: string) => void {
	let buf = '';
	return (chunk: string) => {
		buf += chunk;
		const lines = buf.split('\n');
		buf = lines.pop() ?? '';
		for (const line of lines) { onLine(line); }
	};
}

/**
 * The `auto` ceiling for per-module Yosys runs. Yosys is single-threaded, so
 * one process per core is the natural limit; past 8 the runs contend for memory
 * rather than CPU, and a design with a large block RAM can hold a lot of it per
 * process. A user who sets `clash-toolkit.yosysJobs` explicitly is not capped.
 */
export const YOSYS_AUTO_JOB_CAP = 8;

/**
 * How many Yosys processes a per-module pass may run at once — `auto` by
 * default, overridden by `clash-toolkit.yosysJobs`, and never more than there
 * are components to synthesize.
 */
export function perModuleConcurrency(componentCount: number, setting?: JobSetting): number {
	return resolveJobCount(setting, { cap: YOSYS_AUTO_JOB_CAP, work: componentCount });
}

/**
 * Run `fn` over `items` with at most `limit` in flight, returning results in
 * **input order** regardless of completion order — per-module callers rely on
 * the components staying in the dependency order the manifest parser produced
 * (leaves first, top last).
 */
async function mapPool<T, R>(
	items: T[],
	limit: number,
	fn: (item: T, index: number) => Promise<R>
): Promise<R[]> {
	const results = new Array<R>(items.length);
	let next = 0;
	const worker = async (): Promise<void> => {
		for (;;) {
			const i = next++;
			if (i >= items.length) { return; }
			results[i] = await fn(items[i], i);
		}
	};
	const workers = Array.from(
		{ length: Math.max(1, Math.min(limit, items.length)) },
		() => worker()
	);
	await Promise.all(workers);
	return results;
}

/**
 * Every Verilog file below `name`, inclusive. Only the elaboration flow needs
 * this: it reads dependencies in full, so `hierarchy -check` has to be able to
 * resolve the whole subtree. The synthesis flow stubs its direct dependencies
 * instead and never looks past them.
 */
function collectDepVerilog(
	name: string,
	byName: Map<string, ComponentInfo>,
	visited: Set<string> = new Set()
): string[] {
	if (visited.has(name)) { return []; }
	visited.add(name);
	const comp = byName.get(name);
	if (!comp) { return []; }
	const files: string[] = [];
	for (const dep of comp.dependencies) {
		files.push(...collectDepVerilog(dep, byName, visited));
	}
	files.push(...comp.verilogFiles);
	return files;
}

/**
 * The Yosys script for one component of a per-module pass.
 *
 * Synthesis resolves the out-of-context template — the built-in one, or the
 * user's `outOfContextScript` when they have edited it. Its dependencies are
 * read with `read_verilog -lib` (port interfaces kept, bodies discarded), so
 * they stay opaque black boxes and only this component's own logic is
 * synthesized. Only **direct** dependencies are stubbed: a black box has no
 * body, so the components *it* instantiates are never referenced and
 * `hierarchy -check` never has to resolve them, however deep the design goes.
 *
 * Elaboration instead reads the whole subtree in full, because its netlist has
 * to carry the real sub-module definitions for the diagram to be drilled into.
 */
export function buildPerModuleScript(
	component: ComponentInfo,
	byName: Map<string, ComponentInfo>,
	flow: 'synthesize' | 'elaborate',
	moduleDir: string,
	outOfContextScript?: string
): string {
	if (flow === 'elaborate') {
		const depVerilog = new Set<string>();
		for (const dep of component.dependencies) {
			for (const f of collectDepVerilog(dep, byName)) { depVerilog.add(f); }
		}
		let script = `# Per-Module Elaboration: ${component.name}\n\n`;
		for (const vFile of depVerilog) { script += `read_verilog "${vFile}"\n`; }
		for (const vFile of component.verilogFiles) { script += `read_verilog "${vFile}"\n`; }
		script += `\nhierarchy -check -top ${component.name}\n\n`;
		// Just enough to make the design `write_json`-able; no flatten, so
		// sub-component instances stay visible.
		script += `proc\nopt_clean\n\n`;
		script += `# Machine-readable statistics\n`;
		script += `tee -q -o "${path.join(moduleDir, 'stats.json')}" stat -json\n`;
		script += `# Report longest topological path (combinational depth)\n`;
		script += `tee -o "${path.join(moduleDir, 'logic_depth.txt')}" ltp -noff\n\n`;
		script += `# Write RTLIL\nwrite_rtlil "${path.join(moduleDir, `${component.name}.il`)}"\n\n`;
		script += `# Strip timing-only cells\ndelete */t:$specify2 */t:$specify3\nopt_clean\nclean\n`;
		script += `write_json "${path.join(moduleDir, `${component.name}.json`)}"\n`;
		return script;
	}

	// Restricted to components this run knows about: anything else (a vendor
	// primitive, say) is not ours to stub — we have no Verilog for it.
	const blackBoxes = component.dependencies.filter(d => byName.has(d));
	const stubFiles = new Set<string>();
	for (const dep of blackBoxes) {
		for (const f of byName.get(dep)?.verilogFiles ?? []) { stubFiles.add(f); }
	}
	// A file this component reads in full must not also be stubbed — Clash can
	// put several components in one file, and the full read has to win.
	for (const own of component.verilogFiles) { stubFiles.delete(own); }

	return resolveScript(outOfContextScript || getDefaultOutOfContextScript(), {
		files: component.verilogFiles,
		libFiles: [...stubFiles],
		blackBoxes,
		topModule: component.name,
		outputDir: moduleDir,
		outputBaseName: component.name,
	});
}

/**
 * Handles Yosys synthesis from Verilog
 */
export class YosysRunner {
	constructor(private outputChannel: vscode.OutputChannel) {}

	/**
	 * Synthesize Verilog using Yosys
	 */
	async synthesize(options: YosysOptions): Promise<YosysSynthesisResult> {
		this.outputChannel.appendLine('');
		this.outputChannel.appendLine('='.repeat(60));
		this.outputChannel.appendLine('Yosys Synthesis');
		this.outputChannel.appendLine('='.repeat(60));
		
		// Log input files
		if (Array.isArray(options.verilogPath)) {
			this.outputChannel.appendLine(`Input: ${options.verilogPath.length} Verilog file(s)`);
			options.verilogPath.forEach(f => {
				this.outputChannel.appendLine(`  - ${path.basename(f)}`);
			});
		} else {
			this.outputChannel.appendLine(`Input: ${options.verilogPath}`);
		}
		
		this.outputChannel.appendLine(`Top Module: ${options.topModule}`);
		this.outputChannel.appendLine('');

		// Create output directory
		await fs.mkdir(options.outputDir, { recursive: true });

		// Generate Yosys script
		const scriptPath = await this.generateScript(options);
		this.outputChannel.appendLine(`Generated script: ${scriptPath}`);
		this.outputChannel.appendLine('');

		// Let yosys write its own log file — more reliable than capturing
		// stdout+stderr ourselves (survives crashes, flushes in real time).
		const logPath = path.join(options.outputDir, 'yosys.log');
		const { cmd: yosysCmd, baseArgs } = resolveYosysCommand();
		const yosysArgs = [...baseArgs, '-l', logPath, '-s', scriptPath];

		// Safety net: abc is known to hang indefinitely on some large-RAM
		// designs, and this is the path every P&R run takes.
		const TIMEOUT_MS = 600_000;

		return new Promise((resolve) => {
			const logger = getLogger();
			const finishLog = logger?.command(yosysCmd, yosysArgs, options.workspaceRoot);
			const yosys = spawn(yosysCmd, yosysArgs, {
				cwd: options.workspaceRoot,
				env: toolSpawnEnv(yosysCmd)
			});

			let stdout = '';
			let stderr = '';
			let settled = false;
			let timedOut = false;
			let killTimer: ReturnType<typeof setTimeout> | undefined;
			const warnings: YosysWarning[] = [];
			const errors: YosysError[] = [];

			// SIGTERM first; escalate to SIGKILL if yosys/abc ignores it.
			// Both are guarded: an abort that lands before the child is running
			// — including one already raised when synthesize() was called, or a
			// spawn that fails because yosys is missing — has nothing to signal,
			// and on Windows that throws EINVAL instead of quietly doing nothing.
			const kill = (why: string) => {
				if (settled) { return; }
				this.outputChannel.appendLine(`\nWARNING: ${why} — terminating yosys`);
				try { yosys.kill('SIGTERM'); } catch { /* never started, or already gone */ }
				killTimer = setTimeout(() => {
					if (!settled) {
						try { yosys.kill('SIGKILL'); } catch { /* gone */ }
					}
				}, 5000);
			};
			const timeoutHandle = setTimeout(() => {
				timedOut = true;
				kill(`Yosys timed out after ${TIMEOUT_MS / 1000}s`);
			}, TIMEOUT_MS);
			const onAbort = () => kill('Synthesis cancelled');
			if (options.abortSignal?.aborted) {
				onAbort();
			} else {
				options.abortSignal?.addEventListener('abort', onAbort, { once: true });
			}
			const cleanup = () => {
				settled = true;
				clearTimeout(timeoutHandle);
				if (killTimer) { clearTimeout(killTimer); }
				options.abortSignal?.removeEventListener('abort', onAbort);
			};

			// Line-buffered so messages straddling chunk boundaries aren't
			// missed, and so a path containing "error" isn't recorded as one.
			const splitOut = makeLineSplitter((line) => {
				if (/^Warning:/i.test(line.trim())) {
					warnings.push({ message: line.trim() });
				}
			});
			const splitErr = makeLineSplitter((line) => {
				if (/^ERROR:/.test(line.trim())) {
					errors.push({ message: line.trim() });
				}
			});

			yosys.stdout.on('data', (data) => {
				const text = data.toString();
				stdout += text;
				this.outputChannel.append(text);
				splitOut(text);
			});

			yosys.stderr.on('data', (data) => {
				const text = data.toString();
				stderr += text;
				this.outputChannel.append(text);
				splitErr(text);
			});

			yosys.on('error', (error) => {
				cleanup();
				this.outputChannel.appendLine(`\nERROR: Failed to spawn ${yosysCmd}: ${error.message}`);
				this.outputChannel.appendLine('Make sure Yosys is installed and in your PATH');
				resolve({
					success: false,
					output: stdout + stderr,
					warnings,
					errors: [{ message: error.message }]
				});
			});

			yosys.on('close', async (code) => {
				cleanup();
				finishLog?.then(fn => fn(code));
				this.outputChannel.appendLine('');
				this.outputChannel.appendLine(`Yosys exited with code ${code}`);
				this.outputChannel.appendLine(`Log: ${logPath}`);

				if (options.abortSignal?.aborted || timedOut) {
					const reason = timedOut
						? `Yosys timed out after ${TIMEOUT_MS / 1000}s`
						: 'Synthesis cancelled';
					resolve({
						success: false,
						output: stdout + stderr,
						warnings,
						errors: [{ message: reason }]
					});
					return;
				}

				if (code === 0) {
					this.outputChannel.appendLine('✓ Synthesis successful');

					// Parse statistics: prefer the structured stats.json written
					// by the script's `stat -json`, fall back to text parsing
					// for custom scripts that don't emit one.
					// Missing/unparseable stats.json is a loud failure — the
					// run's outputs cannot be trusted to be complete.
					let stats: SynthesisStatistics;
					try {
						stats = await YosysRunner.loadStatistics(options.outputDir, stdout);
					} catch (statsErr) {
						const message = statsErr instanceof Error ? statsErr.message : String(statsErr);
						this.outputChannel.appendLine(`✗ ${message}`);
						resolve({
							success: false,
							output: stdout + stderr,
							warnings,
							errors: [{ message }]
						});
						return;
					}

					// Save human-readable statistics report
					try {
						const statsReport = this.formatStatisticsReport(stats);
						const statsPath = path.join(options.outputDir, 'statistics.txt');
						await fs.writeFile(statsPath, statsReport, 'utf8');
						this.outputChannel.appendLine(`Statistics report saved: ${statsPath}`);
					} catch (err) {
						this.outputChannel.appendLine(`Warning: Could not save statistics report: ${err}`);
					}

					// Check for output files
					const synthesizedPath = path.join(
						options.outputDir,
						`${options.topModule}_synth.v`
					);
					const jsonPath = path.join(options.outputDir, `${options.topModule}.json`);
					// The diagram is rendered from the same JSON netlist the
					// script just wrote — no separate Yosys `show` output.
					const svgPath = fireDiagramRender(
						jsonPath, this.outputChannel, options.topModule
					);

					resolve({
						success: true,
						synthesizedVerilogPath: synthesizedPath,
						jsonPath,
						svgPath,
						statistics: stats,
						output: stdout + stderr,
						warnings,
						errors: []
					});
				} else {
					this.outputChannel.appendLine(`✗ Synthesis failed with code ${code}`);
					resolve({
						success: false,
						output: stdout + stderr,
						warnings,
						errors: errors.length > 0 ? errors : [{ message: 'Synthesis failed' }]
					});
				}
			});
		});
	}

	/**
	 * Generate Yosys synthesis script.
	 *
	 * Uses the template system from synthesis-targets.ts.  If the user has
	 * provided a custom script via `options.customScript`, it is used as the
	 * template; otherwise the built-in default for the selected target is used.
	 */
	private async generateScript(options: YosysOptions): Promise<string> {
		const outputBaseName = path.basename(options.topModule);

		// Determine which Verilog files to use
		let verilogFiles: string[];
		if (Array.isArray(options.verilogPath)) {
			verilogFiles = options.verilogPath;
			this.outputChannel.appendLine(`Using ${verilogFiles.length} Verilog files from manifest (includes dependencies, deduplicated)`);
		} else {
			const verilogDir = path.dirname(options.verilogPath);
			const allFiles = await fs.readdir(verilogDir);
			verilogFiles = allFiles
				.filter(f => f.endsWith('.v') && !f.includes('_shim'))
				.map(f => path.join(verilogDir, f));
			this.outputChannel.appendLine(`Scanning directory: found ${verilogFiles.length} Verilog files`);
		}

		// Select the script template
		const template = options.customScript || getDefaultScript(options.targetFamily || 'generic');

		// Resolve placeholders
		const script = resolveScript(template, {
			files: verilogFiles,
			topModule: options.topModule,
			outputDir: options.outputDir,
			outputBaseName,
		});

		// Write script to file
		const scriptPath = path.join(options.outputDir, 'synth.ys');
		await fs.writeFile(scriptPath, script);

		return scriptPath;
	}

	/**
	 * Load synthesis statistics for a run.
	 *
	 * Reads the `stats.json` written by the script's `stat -json`, which is
	 * machine-readable and immune to text-format drift, and augments it with
	 * ltp output (a text-only field) parsed from the Yosys stdout log.
	 *
	 * Throws when the JSON file is missing or unparseable — a custom script
	 * that removed the `stat -json` line should fail loudly, not silently
	 * degrade to guessing statistics out of the text log.
	 */
	static async loadStatistics(outputDir: string, textOutput: string): Promise<SynthesisStatistics> {
		const jsonPath = path.join(outputDir, 'stats.json');
		let raw: string;
		try {
			raw = await fs.readFile(jsonPath, 'utf8');
		} catch {
			throw new Error(
				`stats.json not found in ${outputDir} — the synthesis script must emit ` +
				'machine-readable statistics. Keep the ' +
				'`tee -q -o "{outputDir}/stats.json" stat -json` line in custom scripts.'
			);
		}
		const stats = YosysRunner.parseStatsJson(raw);

		// Pull logic depth from the log — ltp isn't part of stats.json.
		if (stats.logicDepth === undefined) {
			const depth = YosysRunner.parseLogicDepth(textOutput);
			if (depth !== undefined) { stats.logicDepth = depth; }
		}

		return stats;
	}

	/**
	 * Parse the JSON emitted by `stat -json` into our SynthesisStatistics shape.
	 *
	 * The shape is documented at <https://yosyshq.readthedocs.io/projects/yosys/en/latest/cmd/stat.html>
	 * and empirically contains a `design` object aggregating all modules with
	 * `num_cells`, `num_wires`, `num_cells_by_type`, and (when `-tech`/`-liberty`
	 * is given) `area` / `estimated_num_transistors`.
	 */
	static parseStatsJson(jsonText: string): SynthesisStatistics {
		interface StatsBlock {
			num_cells?: number;
			num_wires?: number;
			num_cells_by_type?: Record<string, number>;
			area?: number | string;
			estimated_num_transistors?: number | string;
		}
		interface StatsJson {
			design?: StatsBlock;
			modules?: Record<string, StatsBlock>;
		}

		const stats: SynthesisStatistics = { rawStats: jsonText.trim() };
		let parsed: StatsJson;
		try {
			parsed = JSON.parse(jsonText) as StatsJson;
		} catch (err) {
			// Loud failure — a truncated/corrupt stats.json must not silently
			// yield empty statistics.
			throw new Error(
				`stats.json is unparseable: ${err instanceof Error ? err.message : String(err)}`
			);
		}

		// Prefer the aggregated `design` block if present, otherwise merge
		// every module (yosys omits `design` for single-module designs).
		const block = parsed.design
			?? (parsed.modules ? YosysRunner.mergeStatsBlocks(Object.values(parsed.modules)) : undefined);
		if (!block) {
			throw new Error(
				'stats.json contains neither a "design" nor a "modules" block — ' +
				'not valid `stat -json` output.'
			);
		}

		if (typeof block.num_cells === 'number') { stats.cellCount = block.num_cells; }
		if (typeof block.num_wires === 'number') { stats.wireCount = block.num_wires; }

		if (block.num_cells_by_type) {
			const types = new Map<string, number>();
			for (const [k, v] of Object.entries(block.num_cells_by_type)) {
				if (typeof v === 'number') { types.set(k, v); }
			}
			if (types.size > 0) { stats.cellTypes = types; }
		}

		// Area metrics: `area` (from -liberty) takes precedence over
		// `estimated_num_transistors` (from -tech cmos).
		const areaValue = block.area ?? block.estimated_num_transistors;
		if (areaValue !== undefined) {
			const n = typeof areaValue === 'number' ? areaValue : parseFloat(String(areaValue));
			if (Number.isFinite(n)) { stats.chipArea = n; }
		}

		return stats;
	}

	private static mergeStatsBlocks(blocks: Array<{
		num_cells?: number;
		num_wires?: number;
		num_cells_by_type?: Record<string, number>;
		area?: number | string;
		estimated_num_transistors?: number | string;
	}>): {
		num_cells: number;
		num_wires: number;
		num_cells_by_type: Record<string, number>;
		area?: number | string;
		estimated_num_transistors?: number | string;
	} {
		const merged: {
			num_cells: number;
			num_wires: number;
			num_cells_by_type: Record<string, number>;
			area?: number | string;
			estimated_num_transistors?: number | string;
		} = { num_cells: 0, num_wires: 0, num_cells_by_type: {} };
		for (const b of blocks) {
			merged.num_cells += b.num_cells ?? 0;
			merged.num_wires += b.num_wires ?? 0;
			for (const [k, v] of Object.entries(b.num_cells_by_type ?? {})) {
				merged.num_cells_by_type[k] = (merged.num_cells_by_type[k] ?? 0) + v;
			}
		}
		return merged;
	}

	/** Extract the longest topological path length from ltp output text. */
	static parseLogicDepth(text: string): number | undefined {
		const m = text.match(/Longest topological path in\s+\S+\s+\(length=(\d+)\)/);
		return m ? parseInt(m[1], 10) : undefined;
	}

	/**
	 * Format synthesis statistics for report file
	 */
	private formatStatisticsReport(stats: SynthesisStatistics): string {
		let report = 'Yosys Synthesis Statistics Report\n';
		report += '='.repeat(60) + '\n';
		report += `Generated: ${new Date().toISOString()}\n\n`;

		if (stats.cellCount !== undefined) {
			report += `Number of Cells:    ${stats.cellCount}\n`;
		}

		if (stats.wireCount !== undefined) {
			report += `Number of Wires:    ${stats.wireCount}\n`;
		}

		if (stats.chipArea !== undefined) {
			report += `Chip Area:          ${stats.chipArea}\n`;
		}

		if (stats.logicDepth !== undefined) {
			report += `Logic Depth (ltp):  ${stats.logicDepth} cell(s)\n`;
		}

		if (stats.cellTypes && stats.cellTypes.size > 0) {
			report += '\nCell Types:\n';
			report += '-'.repeat(40) + '\n';
			const sortedTypes = Array.from(stats.cellTypes.entries())
				.sort((a, b) => b[1] - a[1]); // Sort by count descending
			for (const [type, count] of sortedTypes) {
				report += `  ${type.padEnd(26)} ${count.toString().padStart(6)}\n`;
			}
		}

		if (stats.rawStats) {
			report += '\nRaw Statistics:\n';
			report += '-'.repeat(60) + '\n';
			report += stats.rawStats + '\n';
		}

		return report;
	}

	/**
	 * Per-module synthesis: each component gets its own .il (RTLIL) and
	 * .json netlist output, allowing individual circuit diagrams.
	 *
	 * Falls back to regular `synthesize()` for single-component designs.
	 */
	async synthesizePerModule(
		components: ComponentInfo[],
		options: YosysOptions
	): Promise<YosysSynthesisResult> {
		return this.runPerModulePass(components, options, 'synthesize');
	}

	/**
	 * Per-module elaboration: each component is elaborated independently
	 * with its hierarchy preserved (no flatten), so diagrams render
	 * sub-component instances as boxes rather than expanding them.
	 *
	 * Falls back to regular `synthesize()` (with the caller-supplied
	 * elaboration script) for single-component designs.
	 */
	async elaboratePerModule(
		components: ComponentInfo[],
		options: YosysOptions
	): Promise<YosysSynthesisResult> {
		return this.runPerModulePass(components, options, 'elaborate');
	}

	/**
	 * Shared driver for per-module synthesis and per-module elaboration.
	 * Differences between the two flows:
	 *
	 *  - synthesize: sub-components are read `-lib` (interfaces only) so they
	 *    stay opaque black boxes, and the body is
	 *    `proc\nsetattr -set keep\nopt -purge\nmemory -nomap\nopt` — no
	 *    `flatten`, so a component's netlist and figures cover its own logic
	 *    plus the instances it contains, and nothing below them.
	 *
	 *  - elaborate: dependencies are read in full and the body is
	 *    `proc\nopt_clean` — the hierarchy is intact and the sub-component
	 *    definitions travel in the netlist, so diagrams can be drilled into.
	 *
	 * Neither flow has any cross-component data dependency: a component's run
	 * needs its dependencies' *Verilog*, never their results. So every
	 * component is dispatched concurrently, bounded by `perModuleConcurrency`.
	 */
	private async runPerModulePass(
		components: ComponentInfo[],
		options: YosysOptions,
		flow: 'synthesize' | 'elaborate'
	): Promise<YosysSynthesisResult> {
		if (components.length <= 1) {
			return this.synthesize(options);
		}

		const isSynth = flow === 'synthesize';
		const flowLabel = isSynth ? 'Per-Module Synthesis' : 'Per-Module Elaboration';
		const verb = isSynth ? 'Synthesizing' : 'Elaborating';
		const jobs = perModuleConcurrency(components.length, options.yosysJobs);

		this.outputChannel.appendLine('');
		this.outputChannel.appendLine('='.repeat(60));
		this.outputChannel.appendLine(flowLabel);
		this.outputChannel.appendLine(
			`${components.length} components detected — ${verb.toLowerCase()} ` +
			`${jobs} at a time`
		);
		this.outputChannel.appendLine('='.repeat(60));

		const perModuleDir = path.join(options.outputDir, 'per-module');
		await fs.mkdir(perModuleDir, { recursive: true });

		// Build lookup so we can resolve dependency Verilog files
		const byName = new Map(components.map(c => [c.name, c]));

		/**
		 * The components a component instantiates directly, restricted to ones
		 * this run actually produced results for — the tree uses this to show
		 * the design hierarchy without having to read it back out of a netlist.
		 */
		const directSubComponents = (c: ComponentInfo): string[] =>
			c.dependencies.filter(d => byName.has(d));

		const runComponent = async (component: ComponentInfo): Promise<ModuleSynthesisResult> => {
			const moduleDir = path.join(perModuleDir, component.name);
			await fs.mkdir(moduleDir, { recursive: true });

			const startTime = Date.now();

			const ilPath = path.join(moduleDir, `${component.name}.il`);
			const jsonPath = path.join(moduleDir, `${component.name}.json`);
			const scriptPath = path.join(moduleDir, 'synth.ys');

			const script = buildPerModuleScript(
				component, byName, flow, moduleDir, options.outOfContextScript
			);
			await fs.writeFile(scriptPath, script);

			const moduleLogPath = path.join(moduleDir, 'yosys.log');
			const run = await this.runYosysScript(
				scriptPath, options.workspaceRoot, false, options.abortSignal, undefined, moduleLogPath
			);
			const elapsed = Date.now() - startTime;

			if (run.code === 0) {
				// Missing stats.json turns this module into a loud failure —
				// its outputs cannot be trusted to be complete.
				let statistics: SynthesisStatistics;
				try {
					statistics = await YosysRunner.loadStatistics(moduleDir, run.stdout);
				} catch (statsErr) {
					const message = statsErr instanceof Error ? statsErr.message : String(statsErr);
					this.outputChannel.appendLine(`  ✗ ${component.name}: ${message}`);
					return {
						name: component.name,
						success: false,
						elapsedMs: elapsed,
						errors: [{ message }]
					};
				}
				this.outputChannel.appendLine(`  ✓ ${component.name} (${elapsed}ms)`);
				// Name the module explicitly: the netlist also carries this
				// component's black-box stubs, and it's the component itself
				// we want drawn (sub-instances as boxes).
				const svgPath = fireDiagramRender(
					jsonPath, this.outputChannel, component.name
				);
				return {
					name: component.name,
					success: true,
					netlistPath: jsonPath,
					rtlilPath: ilPath,
					diagramJsonPath: jsonPath,
					svgPath,
					verilogFiles: component.verilogFiles,
					subComponents: directSubComponents(component),
					// Only the synthesis flow optimizes, so only it loses
					// cross-boundary optimization by running per component.
					outOfContext: isSynth,
					elapsedMs: elapsed,
					statistics,
					errors: []
				};
			}

			this.outputChannel.appendLine(`  ✗ ${component.name} failed (${elapsed}ms)`);
			return {
				name: component.name,
				success: false,
				elapsedMs: elapsed,
				errors: run.errors.length > 0
					? run.errors
					: [{ message: `${flowLabel} of ${component.name} failed with code ${run.code}` }]
			};
		};

		// Results come back in component order even though the runs finish out
		// of order, so `moduleResults` still ends leaves-first / top-last.
		const moduleResults = await mapPool(components, jobs, async (component) => {
			// Cancelling stops *scheduling* further components; the runs already
			// in flight are killed through the abort signal inside runYosysScript.
			if (options.abortSignal?.aborted) {
				return {
					name: component.name,
					success: false,
					elapsedMs: 0,
					errors: [{ message: `${flowLabel} cancelled` }]
				} satisfies ModuleSynthesisResult;
			}
			return runComponent(component);
		});

		if (options.abortSignal?.aborted) {
			return {
				success: false,
				output: '',
				warnings: [],
				errors: [{ message: `${flowLabel} cancelled` }],
				// Only what actually completed: the rest are components the pool
				// never got to, and reporting those as failures would read as if
				// the cancelled run had found something wrong with them.
				moduleResults: moduleResults.filter(r => r.success)
			};
		}

		// Persist the hierarchy alongside the per-module output. Reading it back
		// out of a netlist would mean re-parsing every one of them, so both
		// sidebar views take the graph from here instead.
		try {
			const hierarchy: PerModuleHierarchy = {
				top: options.topModule,
				outOfContext: isSynth,
				components: Object.fromEntries(
					components.map(c => [c.name, directSubComponents(c)])
				),
			};
			await fs.writeFile(
				path.join(perModuleDir, 'hierarchy.json'),
				JSON.stringify(hierarchy, null, 2),
				'utf8'
			);
		} catch (err) {
			// Non-fatal: only the history view's nesting depends on it.
			this.outputChannel.appendLine(`Warning: could not write hierarchy.json: ${err}`);
		}

		const failures = moduleResults.filter(r => !r.success);
		if (failures.length > 0) {
			return {
				success: false,
				output: '',
				warnings: [],
				errors: failures.flatMap(f => f.errors),
				moduleResults
			};
		}

		// Also produce a combined whole-design result for statistics
		const topResult = moduleResults[moduleResults.length - 1];
		return {
			success: true,
			jsonPath: topResult.netlistPath,
			svgPath: topResult.svgPath,
			statistics: topResult.statistics,
			output: '',
			warnings: [],
			errors: [],
			moduleResults
		};
	}

	/**
	 * Run a Yosys script and collect output.
	 *
	 * @param timeoutMs - Optional wall-clock timeout in milliseconds.  If
	 *   Yosys does not exit within this time the process is killed and the
	 *   call resolves with code === null and a descriptive error message.
	 *   Defaults to 600 000 ms (10 minutes) as a safety net against hangs
	 *   caused by abc running on unexpectedly large circuits.
	 */
	private runYosysScript(
		scriptPath: string,
		cwd: string,
		verbose: boolean,
		abortSignal?: AbortSignal,
		timeoutMs = 600_000,
		logFile?: string
	): Promise<{
		code: number | null;
		stdout: string;
		stderr: string;
		warnings: YosysWarning[];
		errors: YosysError[];
	}> {
		return new Promise((resolve) => {
			const logger = getLogger();
			// When a logfile is requested, use yosys's native -l option —
			// it's flushed in real time and survives a crash of this extension.
			const { cmd: yosysCmd, baseArgs } = resolveYosysCommand();
			const args = logFile
				? [...baseArgs, '-l', logFile, '-s', scriptPath]
				: [...baseArgs, '-s', scriptPath];
			const finishLog = logger?.command(yosysCmd, args, cwd);
			const yosys = spawn(yosysCmd, args, {
				cwd,
				env: toolSpawnEnv(yosysCmd)
			});

			let stdout = '';
			let stderr = '';
			let resolved = false;
			let killTimer: ReturnType<typeof setTimeout> | undefined;
			const warnings: YosysWarning[] = [];
			const errors: YosysError[] = [];

			// Declare timer handle early so finish() can clear it regardless
			// of declaration order (all calls to finish() are async).
			let timeoutHandle: ReturnType<typeof setTimeout>;

			// SIGTERM first; escalate to SIGKILL if yosys/abc ignores it so
			// the process doesn't keep burning CPU after we've given up on it.
			const kill = () => {
				// Guarded: a child that never started has nothing to signal, and
				// on Windows kill() throws EINVAL there rather than returning false.
				try { yosys.kill('SIGTERM'); } catch { /* never started, or already gone */ }
				killTimer = setTimeout(() => {
					if (yosys.exitCode === null) {
						try { yosys.kill('SIGKILL'); } catch { /* gone */ }
					}
				}, 5000);
			};

			// If an abort signal fires, kill the child process.
			const onAbort = () => { kill(); };

			const finish = (code: number | null, extraErrors: YosysError[] = []) => {
				if (resolved) { return; }
				resolved = true;
				clearTimeout(timeoutHandle);
				if (killTimer) { clearTimeout(killTimer); }
				abortSignal?.removeEventListener('abort', onAbort);
				finishLog?.then(fn => fn(code));
				resolve({
					code,
					stdout,
					stderr,
					warnings,
					errors: [...errors, ...extraErrors]
				});
			};

			// Wall-clock timeout — kill the process if it runs too long.
			timeoutHandle = setTimeout(() => {
				const msg = `Yosys timed out after ${timeoutMs / 1000}s — killing process`;
				this.outputChannel.appendLine(`\nWARNING: ${msg}`);
				kill();
				finish(null, [{ message: msg }]);
			}, timeoutMs);

			if (abortSignal) {
				if (abortSignal.aborted) {
					kill();
				} else {
					abortSignal.addEventListener('abort', onAbort, { once: true });
				}
			}

			// Line-buffered so messages straddling chunk boundaries aren't
			// missed, and so incidental "error" substrings aren't recorded.
			const splitOut = makeLineSplitter((line) => {
				if (/^Warning:/i.test(line.trim())) {
					warnings.push({ message: line.trim() });
				}
			});
			const splitErr = makeLineSplitter((line) => {
				if (/^ERROR:/.test(line.trim())) {
					errors.push({ message: line.trim() });
				}
			});

			yosys.stdout.on('data', (data) => {
				const text = data.toString();
				stdout += text;
				if (verbose) { this.outputChannel.append(text); }
				splitOut(text);
			});

			yosys.stderr.on('data', (data) => {
				const text = data.toString();
				stderr += text;
				if (verbose) { this.outputChannel.append(text); }
				splitErr(text);
			});

			yosys.on('error', (error) => {
				finish(null, [{ message: error.message }]);
			});

			yosys.on('close', (code) => {
				finish(code);
			});
		});
	}

}
