/**
 * Synthesis target definitions and script template helpers.
 *
 * Each target corresponds to a Yosys `synth_*` command (or generic passes)
 * and carries a default script template with placeholders that are resolved
 * at synthesis time.
 *
 * Placeholders:
 *   {files}          — expands to one `read_verilog <path>` line per input file
 *   {topModule}      — the top-level module name
 *   {outputDir}      — directory for output artefacts
 *   {outputBaseName} — base name for output files (usually === topModule)
 */

/** Describes a Yosys synthesis target. */
export interface SynthesisTarget {
	/** Machine-readable identifier used in settings and cache keys. */
	id: string;
	/** Human-readable label for UI dropdowns. */
	label: string;
	/** The `synth_*` Yosys command, or `null` for generic synthesis. */
	synthCommand: string | null;
	/** Default script template with placeholders. */
	defaultScript: string;
}

// ---------------------------------------------------------------------------
// Default script templates
// ---------------------------------------------------------------------------

function makeTargetScript(synthLine: string): string {
	return `# Read design files
{files}

# Elaborate design
hierarchy -check -top {topModule}

# Synthesize
${synthLine}

# Assert no unconnected / multiply-driven wires or unmapped cells
check -assert

# Machine-readable statistics (parsed by the extension)
tee -q -o "{outputDir}/stats.json" stat -json

# Report longest topological path (combinational depth)
tee -o "{outputDir}/logic_depth.txt" ltp -noff

# Write synthesized Verilog
write_verilog -noattr "{outputDir}/{outputBaseName}_synth.v"

# Strip timing-only cells before further export
delete */t:$specify2 */t:$specify3
opt_clean
clean

# Write JSON netlist — consumed by downstream tools and by netlistsvg, which
# the extension runs afterwards to render the schematic diagram.
write_json "{outputDir}/{outputBaseName}.json"
`;
}

const GENERIC_SCRIPT = `# Read design files
{files}

# Elaborate design
hierarchy -check -top {topModule}

# High-level synthesis
proc
opt
fsm
opt
memory
opt

# Technology mapping (generic)
techmap
opt

# Assert no unconnected / multiply-driven wires or combinational loops
check -assert

# Machine-readable statistics (parsed by the extension)
tee -q -o "{outputDir}/stats.json" stat -json

# Report longest topological path (combinational depth)
tee -o "{outputDir}/logic_depth.txt" ltp -noff

# Write synthesized Verilog
write_verilog -noattr "{outputDir}/{outputBaseName}_synth.v"

# Strip timing-only cells before further export
delete */t:$specify2 */t:$specify3
opt_clean
clean

# Write JSON netlist — consumed by downstream tools and by netlistsvg, which
# the extension runs afterwards to render the schematic diagram.
write_json "{outputDir}/{outputBaseName}.json"
`;

const ELABORATION_SCRIPT = `# Read design files
{files}

# Elaborate design hierarchy
hierarchy -check -top {topModule}

# Convert processes to netlist primitives and clean up
proc
opt_clean

# Assert no unconnected / multiply-driven wires or combinational loops
check -assert

# Machine-readable statistics (elaborated, pre-synthesis)
tee -q -o "{outputDir}/stats.json" stat -json

# Report longest topological path (combinational depth)
tee -o "{outputDir}/logic_depth.txt" ltp -noff

clean

# Write JSON netlist — consumed by downstream tools and by netlistsvg, which
# the extension runs afterwards to render the schematic diagram.
write_json "{outputDir}/{outputBaseName}.json"
`;

const OUT_OF_CONTEXT_SCRIPT = `# Sub-components as black boxes: port interfaces are kept, bodies are ignored,
# so this run covers {topModule}'s own logic and nothing below it.  Only direct
# dependencies need stubbing — a black box has no body, so what *it*
# instantiates is never referenced.
{libFiles}

# Read {topModule}'s own design files
{files}

# Elaborate this component alone
hierarchy -check -top {topModule}

# Convert processes to netlist primitives.
#
# No 'flatten': the black boxes above have nothing to inline, and flattening
# would defeat the point of stubbing them.
#
# No technology mapping either: a full 'synth' per component hangs on
# components holding large block RAMs, because memory_map + abc cannot finish
# on the resulting flip-flop array.  'memory -nomap' collects memories as $mem
# cells instead of expanding them.
proc

# Keep black-box instances even when their outputs are unused — without this,
# opt/opt_clean/clean delete them and the component silently disappears from
# the diagram and the cell counts.
{keepBlackBoxes}

opt -purge
memory -nomap
opt

# Machine-readable statistics (parsed by the extension)
tee -q -o "{outputDir}/stats.json" stat -json

# Report longest topological path (combinational depth)
tee -o "{outputDir}/logic_depth.txt" ltp -noff

# Write RTLIL
write_rtlil "{outputDir}/{outputBaseName}.il"

# Strip timing-only cells before further export
delete */t:$specify2 */t:$specify3
opt_clean
clean

# Write JSON netlist — rendered by netlistsvg into this component's diagram.
write_json "{outputDir}/{outputBaseName}.json"
`;

// ---------------------------------------------------------------------------
// Target registry
// ---------------------------------------------------------------------------

const targetList: SynthesisTarget[] = [
	{
		id: 'generic',
		label: 'Generic (technology-independent)',
		synthCommand: null,
		defaultScript: GENERIC_SCRIPT,
	},
	{
		id: 'ice40',
		label: 'Lattice iCE40',
		synthCommand: 'synth_ice40',
		defaultScript: makeTargetScript('synth_ice40 -top {topModule}'),
	},
	{
		id: 'ecp5',
		label: 'Lattice ECP5',
		synthCommand: 'synth_ecp5',
		defaultScript: makeTargetScript('synth_ecp5 -top {topModule}'),
	},
	{
		id: 'xilinx',
		label: 'AMD / Xilinx 7-series',
		synthCommand: 'synth_xilinx',
		defaultScript: makeTargetScript('synth_xilinx -top {topModule}'),
	},
	{
		id: 'gowin',
		label: 'Gowin',
		synthCommand: 'synth_gowin',
		defaultScript: makeTargetScript('synth_gowin -top {topModule}'),
	},
	{
		id: 'quicklogic',
		label: 'QuickLogic',
		synthCommand: 'synth_quicklogic',
		defaultScript: makeTargetScript('synth_quicklogic -top {topModule}'),
	},
	{
		id: 'sf2',
		label: 'Microsemi SmartFusion2',
		synthCommand: 'synth_sf2',
		defaultScript: makeTargetScript('synth_sf2 -top {topModule}'),
	},
];

/** All available synthesis targets, keyed by id. */
export const SYNTHESIS_TARGETS: ReadonlyMap<string, SynthesisTarget> =
	new Map(targetList.map(t => [t.id, t]));

/** Ordered list of target ids (for dropdowns). */
export const TARGET_IDS: readonly string[] = targetList.map(t => t.id);

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Return the default script template for the given target id.
 * Throws on unknown ids — silently substituting the generic script would
 * mask configuration bugs (e.g. a stale target id in workspace settings).
 */
export function getDefaultScript(targetId: string): string {
	return getTarget(targetId).defaultScript;
}

/** Default script for the elaboration-only stage (no technology mapping). */
export function getDefaultElaborationScript(): string {
	return ELABORATION_SCRIPT;
}

/**
 * Default script for one component of an out-of-context run.
 *
 * This is a *different script from the target's*, not a variation on it, which
 * is why it has its own template and its own `outOfContextScript` override
 * rather than reusing `synthesisScript.<target>`. Editing the ECP5 script does
 * not change what an out-of-context run does, and vice versa.
 */
export function getDefaultOutOfContextScript(): string {
	return OUT_OF_CONTEXT_SCRIPT;
}

/**
 * Return the SynthesisTarget for the given id.
 * Throws on unknown ids rather than falling back to generic.
 */
export function getTarget(targetId: string): SynthesisTarget {
	const target = SYNTHESIS_TARGETS.get(targetId);
	if (!target) {
		throw new Error(
			`Unknown synthesis target "${targetId}" — expected one of: ${TARGET_IDS.join(', ')}. ` +
			'Check the clash-toolkit.synthesisTarget setting.'
		);
	}
	return target;
}

/**
 * Replace placeholders in a script template with concrete values.
 *
 * `vars.files` should be an array of absolute Verilog paths — each is
 * expanded to a `read_verilog <path>` line.
 *
 * `libFiles` and `blackBoxes` only appear in the out-of-context template.
 * Both resolve to the empty string when absent or empty, which is why a leaf
 * component — no dependencies, so nothing to stub and nothing to keep — gets a
 * valid script out of the same template as everything else.
 */
export function resolveScript(
	template: string,
	vars: {
		files: string[];
		topModule: string;
		outputDir: string;
		outputBaseName: string;
		/** Read with `-lib`: interfaces kept, bodies discarded. */
		libFiles?: string[];
		/** Module names whose instances must survive optimization. */
		blackBoxes?: string[];
	}
): string {
	// Quote each path — unquoted paths containing spaces make yosys fail
	// with "File `/…/My' not found".
	const readLines = (files: string[], flag = '') =>
		files.map(f => `read_verilog ${flag}"${f}"`).join('\n');
	const keepLine = vars.blackBoxes?.length
		? `setattr -set keep 1 ${vars.blackBoxes.map(n => `t:${n}`).join(' ')}`
		: '';
	return template
		// Before {files}: the substituted text can itself contain "{files}" only
		// if a path does, which quoting already handles — but do the longer key
		// first regardless so {libFiles} is never matched as {files}.
		.replace(/\{libFiles\}/g, readLines(vars.libFiles ?? [], '-lib '))
		.replace(/\{keepBlackBoxes\}/g, keepLine)
		.replace(/\{files\}/g, readLines(vars.files))
		.replace(/\{topModule\}/g, vars.topModule)
		.replace(/\{outputDir\}/g, vars.outputDir)
		.replace(/\{outputBaseName\}/g, vars.outputBaseName);
}

// ---------------------------------------------------------------------------
// Diff computation for the webview
// ---------------------------------------------------------------------------

export type DiffLineKind = 'equal' | 'added' | 'removed';

export interface DiffLine {
	kind: DiffLineKind;
	text: string;
}

/**
 * Compute a simple line-by-line diff between two scripts.
 *
 * Uses a basic LCS (longest common subsequence) approach which is good
 * enough for the short scripts we deal with (10-40 lines).
 */
export function computeScriptDiff(defaultScript: string, userScript: string): DiffLine[] {
	const a = defaultScript.split('\n');
	const b = userScript.split('\n');

	// Build LCS table
	const m = a.length;
	const n = b.length;
	const dp: number[][] = Array.from({ length: m + 1 }, () => new Array(n + 1).fill(0));
	for (let i = 1; i <= m; i++) {
		for (let j = 1; j <= n; j++) {
			dp[i][j] = a[i - 1] === b[j - 1]
				? dp[i - 1][j - 1] + 1
				: Math.max(dp[i - 1][j], dp[i][j - 1]);
		}
	}

	// Back-track to produce diff lines
	const result: DiffLine[] = [];
	let i = m;
	let j = n;
	while (i > 0 || j > 0) {
		if (i > 0 && j > 0 && a[i - 1] === b[j - 1]) {
			result.push({ kind: 'equal', text: a[i - 1] });
			i--;
			j--;
		} else if (j > 0 && (i === 0 || dp[i][j - 1] >= dp[i - 1][j])) {
			result.push({ kind: 'added', text: b[j - 1] });
			j--;
		} else {
			result.push({ kind: 'removed', text: a[i - 1] });
			i--;
		}
	}
	result.reverse();
	return result;
}
