/**
 * Schematic rendering via [netlistsvg](https://github.com/nturley/netlistsvg).
 *
 * Yosys already writes a JSON netlist for every run (`write_json`); netlistsvg
 * turns that netlist into a schematic SVG, laying it out with ELK and drawing
 * gates from an SVG "skin". No external binary is involved — netlistsvg is a
 * bundled npm dependency — so diagrams work out of the box.
 *
 * This module deliberately imports nothing from `vscode`: it doubles as the
 * entry point of the child process that performs the (CPU-heavy) render, so it
 * must be requirable by a plain node. See {@link ./netlist-diagram} for the
 * orchestration side.
 */
import * as path from 'path';
import { promises as fs } from 'fs';

/** Only the parts of a Yosys JSON netlist this module touches. */
interface YosysModule {
	attributes?: Record<string, unknown>;
	ports?: Record<string, unknown>;
	cells?: Record<string, { type?: string }>;
}

export interface YosysNetlist {
	modules?: Record<string, YosysModule>;
}

/**
 * netlistsvg ships no type declarations; this is the one function we call.
 * `render` resolves to the finished SVG document as a string.
 */
interface NetlistSvgModule {
	render(skinData: string, netlist: unknown): Promise<string>;
}

/** Lazily required so a plain `import` of this module stays cheap. */
function netlistsvg(): NetlistSvgModule {
	// eslint-disable-next-line @typescript-eslint/no-require-imports
	return require('netlistsvg') as NetlistSvgModule;
}

/**
 * Absolute path of the default (digital) skin shipped inside the netlistsvg
 * package. `require.resolve` lands on `netlistsvg/built/index.js`, and the
 * skins live in the sibling `lib/` directory — resolving it this way survives
 * npm hoisting and the flattened layout inside the packaged .vsix.
 */
export function defaultSkinPath(): string {
	return path.join(
		path.dirname(require.resolve('netlistsvg')),
		'..',
		'lib',
		'default.svg'
	);
}

/** The SVG file that {@link renderNetlistSvg} produces for a given netlist. */
export function svgPathForNetlist(netlistPath: string): string {
	return netlistPath.replace(/\.json$/, '') + '.svg';
}

/**
 * Point netlistsvg at a specific module.
 *
 * netlistsvg renders the module carrying Yosys's `top` attribute (falling back
 * to the first one in the file), so selecting a module means moving that
 * attribute. Needed for the per-module elaboration flow, where the netlist also
 * contains every dependency module and we want the component itself — the
 * equivalent of the `show <module>` selector this replaced.
 *
 * @returns true if `topModule` was found (and is now marked top).
 */
export function selectTopModule(netlist: YosysNetlist, topModule: string): boolean {
	const modules = netlist.modules ?? {};
	if (!(topModule in modules)) { return false; }
	for (const [name, mod] of Object.entries(modules)) {
		if (!mod.attributes) {
			if (name !== topModule) { continue; }
			mod.attributes = {};
		}
		if (name === topModule) {
			mod.attributes.top = '1';
		} else {
			delete mod.attributes.top;
		}
	}
	return true;
}

/**
 * Whether a module is a black box — a cell library entry (`LUT4`, `TRELLIS_FF`,
 * `DP16KD`, …) that Yosys includes in the netlist for reference but which has no
 * internals. There is nothing to draw inside one, so they are never offered as
 * something to drill into.
 */
function isBlackbox(mod: YosysModule): boolean {
	const attr = mod.attributes?.blackbox;
	// Yosys writes attribute values as bit-strings ("000…001").
	return attr !== undefined && Number(attr) !== 0;
}

/**
 * Module names instantiated by `moduleName`, i.e. the sub-components whose own
 * diagrams a user can drill into from this module's diagram.
 *
 * A cell counts as a sub-module when its type names another module defined in
 * the same netlist. Primitives (`$add`, `$dff`, …) and black-box library cells
 * have no definition to draw, so they are excluded. The result is deduplicated
 * (a module instantiated three times is one entry) and sorted for stable
 * display.
 */
export function instantiatedModules(netlist: YosysNetlist, moduleName: string): string[] {
	const modules = netlist.modules ?? {};
	const parent = modules[moduleName];
	if (!parent) { return []; }

	const found = new Set<string>();
	for (const cell of Object.values(parent.cells ?? {})) {
		const type = cell.type;
		if (!type || type === moduleName) { continue; }
		const def = modules[type];
		if (!def || isBlackbox(def)) { continue; }
		// A module with neither cells nor ports has nothing to show.
		if (!def.cells && !def.ports) { continue; }
		found.add(type);
	}
	return [...found].sort();
}

/** The module Yosys marked as top, falling back to the first one defined. */
export function topModuleOf(netlist: YosysNetlist): string | undefined {
	const modules = netlist.modules ?? {};
	for (const [name, mod] of Object.entries(modules)) {
		if (mod.attributes && Number(mod.attributes.top) === 1) { return name; }
	}
	return Object.keys(modules)[0];
}

/** Read and parse a Yosys JSON netlist, with the file named in any error. */
export async function readNetlist(netlistPath: string): Promise<YosysNetlist> {
	const raw = await fs.readFile(netlistPath, 'utf8');
	try {
		return JSON.parse(raw) as YosysNetlist;
	} catch (err) {
		throw new Error(
			`${netlistPath} is not valid JSON: ` +
			(err instanceof Error ? err.message : String(err))
		);
	}
}

/**
 * Give a rendered schematic an opaque white background.
 *
 * netlistsvg draws in black on a transparent canvas, so the image preview
 * editor shows the diagram over whatever the active colour theme paints behind
 * it — on a dark theme, black lines and black text on a near-black background.
 * Schematics are black-on-white by convention and the skin's `#000` strokes are
 * not themeable, so the background travels with the file instead of following
 * the theme. That also keeps the SVG readable wherever else it is opened,
 * embedded or printed.
 */
export function withOpaqueBackground(svg: string): string {
	const openTag = /<svg\b[^>]*>/.exec(svg);
	if (!openTag) { return svg; }
	// stroke:none is not redundant — the document sets `svg { stroke:#000 }`,
	// which the rect would otherwise inherit as a black border boxing in the
	// whole diagram. fill is given explicitly for the same reason.
	const background =
		'<rect width="100%" height="100%" style="fill:#fff;stroke:none"/>';
	const insertAt = openTag.index + openTag[0].length;
	return svg.slice(0, insertAt) + background + svg.slice(insertAt);
}

export interface RenderOptions {
	/** Yosys JSON netlist to render (as written by `write_json`). */
	netlistPath: string;
	/** Where to write the SVG. Defaults to the netlist path with `.svg`. */
	svgPath?: string;
	/** Module to draw. Defaults to whichever module Yosys marked as top. */
	topModule?: string;
	/** Override the netlistsvg skin. Defaults to {@link defaultSkinPath}. */
	skinPath?: string;
}

/**
 * Render `netlistPath` to an SVG and return the path written.
 *
 * Throws when the netlist is missing, unparseable, or contains no modules —
 * the caller decides whether that is fatal (for diagrams it never is).
 */
export async function renderNetlistSvg(options: RenderOptions): Promise<string> {
	const svgPath = options.svgPath ?? svgPathForNetlist(options.netlistPath);
	const netlist = await readNetlist(options.netlistPath);
	if (!netlist.modules || Object.keys(netlist.modules).length === 0) {
		throw new Error(`${options.netlistPath} contains no modules`);
	}
	if (options.topModule && !selectTopModule(netlist, options.topModule)) {
		// Not fatal: fall back to the module Yosys marked as top. Happens when
		// the caller's module name doesn't survive synthesis (e.g. renamed by
		// `flatten`), and drawing the top module is still the right answer.
		process.stderr.write(
			`netlistsvg: module "${options.topModule}" not in ${options.netlistPath}; ` +
			'rendering the design top instead\n'
		);
	}

	const skin = await fs.readFile(options.skinPath ?? defaultSkinPath(), 'utf8');
	const svg = await netlistsvg().render(skin, netlist);
	if (!svg) {
		throw new Error('netlistsvg produced no output');
	}
	await fs.writeFile(svgPath, withOpaqueBackground(svg), 'utf8');
	return svgPath;
}

// ---------------------------------------------------------------------------
// Child-process entry point
// ---------------------------------------------------------------------------

/** Parse `--flag value` pairs; unknown flags are an error. */
function parseArgs(argv: string[]): RenderOptions {
	const opts: Partial<RenderOptions> = {};
	for (let i = 0; i < argv.length; i += 2) {
		const value = argv[i + 1];
		if (value === undefined) { throw new Error(`missing value for ${argv[i]}`); }
		switch (argv[i]) {
			case '--netlist': opts.netlistPath = value; break;
			case '--svg': opts.svgPath = value; break;
			case '--top': opts.topModule = value; break;
			case '--skin': opts.skinPath = value; break;
			default: throw new Error(`unknown argument ${argv[i]}`);
		}
	}
	if (!opts.netlistPath) { throw new Error('--netlist is required'); }
	return opts as RenderOptions;
}

if (require.main === module) {
	// Rendering runs out-of-process so a large design's ELK layout cannot block
	// the extension host. Exit code + stderr are the whole protocol.
	renderNetlistSvg(parseArgs(process.argv.slice(2)))
		.then(() => process.exit(0))
		.catch((err: unknown) => {
			process.stderr.write(
				(err instanceof Error ? err.message : String(err)) + '\n'
			);
			process.exit(1);
		});
}
