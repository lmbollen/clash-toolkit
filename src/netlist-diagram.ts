import * as vscode from 'vscode';
import * as path from 'path';
import { promises as fs } from 'fs';
import { fork } from 'child_process';
import { createHash } from 'crypto';
import { getLogger } from './file-logger';
import {
	instantiatedModules,
	readNetlist,
	renderNetlistSvg,
	svgPathForNetlist,
	YosysNetlist,
} from './netlist-renderer';

/**
 * Diagram rendering, orchestrated around {@link ./netlist-renderer}.
 *
 * Rendering is CPU-bound JavaScript (ELK layout), so it runs in a forked child
 * process: a big design would otherwise block the extension host for seconds at
 * a time. The child is `out/netlist-renderer.js` — the same module, run as a
 * script.
 *
 * Renders are fire-and-forget: synthesis resolves as soon as Yosys is done and
 * the diagram catches up in the background. Consumers that actually need the
 * file — the auto-open after Elaborate/Synthesize, the tree-view diagram
 * button — `await waitForSvg(path)` so they block only at the point of use.
 */

/** Safety net: a runaway layout shouldn't leave a child process behind. */
const RENDER_TIMEOUT_MS = 300_000;

/** In-flight renders keyed by the SVG path they will produce. */
const pendingRenders = new Map<string, Promise<string | undefined>>();

/** The compiled child entry point, next to this module in `out/`. */
function rendererScript(): string {
	return path.join(__dirname, 'netlist-renderer.js');
}

/**
 * Render `netlistPath` in a child process.
 *
 * Never rejects: a failed diagram is a warning, not a failed synthesis. Returns
 * the SVG path on success and undefined otherwise.
 */
async function renderInChild(
	netlistPath: string,
	svgPath: string,
	topModule: string | undefined,
	outputChannel: vscode.OutputChannel
): Promise<string | undefined> {
	// The extension/test may have disposed the channel by the time this
	// fire-and-forget render finishes; swallow the resulting "Channel has been
	// closed" so it isn't attributed to whatever is running at the time.
	const warn = (msg: string) => {
		try { outputChannel.appendLine(msg); } catch { /* channel disposed */ }
	};

	try {
		await fs.access(netlistPath);
	} catch {
		// Yosys wrote no netlist — an empty design, or a custom script with no
		// `write_json` line.
		warn(
			`Warning: no JSON netlist at ${netlistPath} — diagram skipped. ` +
			'Custom synthesis scripts must keep their `write_json` line to get a diagram.'
		);
		return undefined;
	}

	const args = ['--netlist', netlistPath, '--svg', svgPath];
	if (topModule) { args.push('--top', topModule); }

	const logger = getLogger();
	const finishLog = logger?.command('netlistsvg', args);
	const script = rendererScript();

	return new Promise<string | undefined>((resolve) => {
		let settled = false;
		let timer: ReturnType<typeof setTimeout> | undefined;
		const done = (code: number | null, result: string | undefined) => {
			if (settled) { return; }
			settled = true;
			if (timer) { clearTimeout(timer); }
			finishLog?.then(fn => fn(code));
			resolve(result);
		};

		// Set once the in-process fallback has taken over, so the child's own
		// failure `close` doesn't settle the promise ahead of it — the SVG would
		// then still be on its way when waitForSvg() has already given up.
		let fallback: Promise<string | undefined> | undefined;
		const fallBack = (err: unknown) => {
			// Couldn't spawn at all (unusual host, sandboxed environment).
			// Rendering in-process stalls this process, but a diagram is better
			// than none.
			fallback = renderInProcess(netlistPath, svgPath, topModule, warn, err);
			void fallback.then(p => done(null, p));
		};

		let child: ReturnType<typeof fork>;
		try {
			child = fork(script, args, {
				// `execArgv: []` keeps an inspector port from the extension host
				// out of the child (it would fail to bind); ELECTRON_RUN_AS_NODE
				// makes VS Code's Electron binary behave as a plain node.
				execArgv: [],
				env: { ...process.env, ELECTRON_RUN_AS_NODE: '1' },
				stdio: ['ignore', 'pipe', 'pipe', 'ipc'],
			});
		} catch (err) {
			fallBack(err);
			return;
		}

		timer = setTimeout(() => {
			child.kill('SIGKILL');
			warn(
				`Warning: diagram rendering timed out after ${RENDER_TIMEOUT_MS / 1000}s ` +
				`for ${path.basename(netlistPath)} — diagram skipped.`
			);
			done(null, undefined);
		}, RENDER_TIMEOUT_MS);

		let stderr = '';
		child.stderr?.on('data', (d: Buffer) => { stderr += d.toString(); });

		child.on('error', (err) => { fallBack(err); });

		child.on('close', (code) => {
			if (fallback) { return; } // the fallback owns the outcome
			if (code === 0) {
				done(code, svgPath);
			} else {
				warn(
					`Warning: netlistsvg exited with code ${code}; diagram skipped. ${stderr.trim()}`
				);
				done(code, undefined);
			}
		});
	});
}

/** Last resort when the child can't be spawned: render here and now. */
async function renderInProcess(
	netlistPath: string,
	svgPath: string,
	topModule: string | undefined,
	warn: (msg: string) => void,
	spawnError: unknown
): Promise<string | undefined> {
	const why = spawnError instanceof Error ? spawnError.message : String(spawnError);
	warn(`Warning: could not spawn the diagram renderer (${why}); rendering in-process.`);
	try {
		return await renderNetlistSvg({ netlistPath, svgPath, topModule });
	} catch (err) {
		warn(
			'Warning: diagram rendering failed — ' +
			(err instanceof Error ? err.message : String(err))
		);
		return undefined;
	}
}

/**
 * Kick off a netlist → SVG render in the background and return the path the SVG
 * will have. The promise is registered so {@link waitForSvg} can join it later.
 *
 * @param topModule module to draw; defaults to the design top marked by Yosys.
 * @param svgPath   where to write it; defaults to the netlist path with `.svg`.
 */
export function fireDiagramRender(
	netlistPath: string,
	outputChannel: vscode.OutputChannel,
	topModule?: string,
	svgPath: string = svgPathForNetlist(netlistPath)
): string {
	const p = renderInChild(netlistPath, svgPath, topModule, outputChannel)
		.finally(() => {
			// Once finished the file is on disk (or never will be), so the
			// resolved promise has no further value.
			pendingRenders.delete(svgPath);
		});
	pendingRenders.set(svgPath, p);
	return svgPath;
}

// ---------------------------------------------------------------------------
// Sub-component diagrams
// ---------------------------------------------------------------------------

/**
 * Netlist cache for hierarchy queries.
 *
 * Expanding a module in the tree asks "which modules does this instantiate?",
 * which means parsing the netlist — several MB for a real design, and once per
 * expansion without this. Keyed by path + mtime so a re-run's netlist is never
 * served from a stale entry.
 */
const netlistCache = new Map<string, { mtimeMs: number; netlist: YosysNetlist }>();
const NETLIST_CACHE_LIMIT = 4;

async function loadNetlistCached(netlistPath: string): Promise<YosysNetlist | undefined> {
	let mtimeMs: number;
	try {
		mtimeMs = (await fs.stat(netlistPath)).mtimeMs;
	} catch {
		return undefined; // no netlist (e.g. a run whose files were deleted)
	}
	const hit = netlistCache.get(netlistPath);
	if (hit && hit.mtimeMs === mtimeMs) { return hit.netlist; }
	let netlist: YosysNetlist;
	try {
		netlist = await readNetlist(netlistPath);
	} catch {
		return undefined;
	}
	netlistCache.set(netlistPath, { mtimeMs, netlist });
	// Crude bound: drop the oldest insertion once over the limit. Netlists are
	// big and the tree only ever looks at the run(s) currently on screen.
	if (netlistCache.size > NETLIST_CACHE_LIMIT) {
		const oldest = netlistCache.keys().next().value;
		if (oldest !== undefined) { netlistCache.delete(oldest); }
	}
	return netlist;
}

/**
 * Sub-components instantiated by `moduleName` in `netlistPath`, each of which
 * has its own drawable diagram. Empty when the netlist is missing, the module
 * isn't in it, or the design was flattened.
 */
export async function subComponentsOf(
	netlistPath: string,
	moduleName: string
): Promise<string[]> {
	const netlist = await loadNetlistCached(netlistPath);
	if (!netlist) { return []; }
	return instantiatedModules(netlist, moduleName);
}

/**
 * Where a sub-component's diagram is written: a `diagrams/` directory beside the
 * netlist, so it never collides with the top-level `<top>.svg` or with the
 * per-module output directories.
 *
 * Module names may contain characters that are awkward in filenames (`$paramod`
 * copies, escaped Verilog ids). Those are replaced, and a short digest of the
 * original name is appended so two different modules cannot land on one file.
 */
export function subDiagramPath(netlistPath: string, moduleName: string): string {
	const safe = moduleName.replace(/[^A-Za-z0-9._-]/g, '_');
	const suffix = safe === moduleName
		? ''
		: '-' + createHash('sha1').update(moduleName).digest('hex').slice(0, 8);
	return path.join(path.dirname(netlistPath), 'diagrams', `${safe}${suffix}.svg`);
}

/**
 * Diagram for one sub-component, rendered on demand.
 *
 * Rendering every module of a design up front would mean dozens of ELK layouts
 * nobody asked for, so a sub-component is drawn the first time someone opens it
 * and reused afterwards. Concurrent requests for the same module share a single
 * render via the in-flight registry.
 *
 * @returns the SVG path, or undefined if it could not be rendered.
 */
export async function ensureSubDiagram(
	netlistPath: string,
	moduleName: string,
	outputChannel: vscode.OutputChannel
): Promise<string | undefined> {
	const svgPath = subDiagramPath(netlistPath, moduleName);

	const inFlight = pendingRenders.get(svgPath);
	if (inFlight) { return inFlight; }
	try {
		await fs.access(svgPath);
		return svgPath;
	} catch { /* not rendered yet */ }

	await fs.mkdir(path.dirname(svgPath), { recursive: true });
	fireDiagramRender(netlistPath, outputChannel, moduleName, svgPath);
	return (await waitForSvg(svgPath)) ? svgPath : undefined;
}

/**
 * Resolve once the in-flight render for `svgPath` has finished — or immediately
 * if there is none (already complete, or produced by an earlier run).
 *
 * @returns true if the SVG exists afterwards.
 */
export async function waitForSvg(svgPath: string): Promise<boolean> {
	const inFlight = pendingRenders.get(svgPath);
	if (inFlight) { await inFlight; }
	try {
		await fs.access(svgPath);
		return true;
	} catch {
		return false;
	}
}
