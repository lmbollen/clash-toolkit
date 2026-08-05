/**
 * Type definitions for Yosys synthesis integration
 */

/**
 * Options for Yosys synthesis
 */
export interface YosysOptions {
	/** Workspace root directory */
	workspaceRoot: string;
	
	/** Output directory for synthesis results */
	outputDir: string;
	
	/** Top module name to synthesize */
	topModule: string;
	
	/** Input Verilog file path(s) - can be single file or array for dependencies */
	verilogPath: string | string[];
	
	/** Technology library file (optional) */
	libertyFile?: string;
	
	/** Target FPGA family (optional). Use 'elaborate' for the elaboration-only stage. */
	targetFamily?: 'ice40' | 'ecp5' | 'xilinx' | 'gowin' | 'quicklogic' | 'sf2' | 'generic' | 'elaborate';

	/** User-customized synthesis script template. Overrides the default for targetFamily. */
	customScript?: string;

	/**
	 * User-customized out-of-context script template, used instead of the
	 * built-in one when the per-component synthesis flow runs. The target's
	 * `customScript` does not apply on that path — it has no `synth_*` step.
	 */
	outOfContextScript?: string;

	/**
	 * How many components a per-module pass may synthesize at once
	 * (`clash-toolkit.yosysJobs`). `auto`/unset derives it from the machine.
	 */
	yosysJobs?: number | string | null;

	/** Abort signal — cancels the run by killing the yosys process. */
	abortSignal?: AbortSignal;
}

/**
 * Result of Yosys synthesis
 */
export interface YosysSynthesisResult {
	/** Whether synthesis succeeded */
	success: boolean;
	
	/** Path to synthesized Verilog file */
	synthesizedVerilogPath?: string;
	
	/** Path to the JSON netlist output */
	jsonPath?: string;

	/** Path to the rendered SVG diagram (netlistsvg) */
	svgPath?: string;

	/** Synthesis statistics */
	statistics?: SynthesisStatistics;
	
	/** Full synthesis output */
	output: string;
	
	/** Parsed warnings */
	warnings: YosysWarning[];
	
	/** Parsed errors */
	errors: YosysError[];

	/** Per-module results when using parallel OOC synthesis */
	moduleResults?: ModuleSynthesisResult[];
}

/**
 * Result of synthesizing a single module in an OOC parallel flow
 */
export interface ModuleSynthesisResult {
	/** Module name */
	name: string;
	/** Whether synthesis succeeded */
	success: boolean;
	/** Path to synthesized netlist (JSON) */
	netlistPath?: string;
	/** Path to RTLIL (.il) file (per-module mode) */
	rtlilPath?: string;
	/** Path to the per-module JSON netlist (per-module mode) */
	diagramJsonPath?: string;
	/** Path to the rendered SVG diagram (netlistsvg) for this module */
	svgPath?: string;
	/**
	 * Names of the components this module instantiates directly.
	 *
	 * Recorded for out-of-context runs, where each component is synthesized
	 * standalone and its netlist is flattened — the hierarchy is no longer
	 * recoverable from the netlist itself, so it comes from the Clash manifest's
	 * dependency graph instead.
	 */
	subComponents?: string[];
	/**
	 * True when this module was synthesized out of context: on its own, with no
	 * visibility of its parent. Its statistics therefore exclude any
	 * optimization across the component boundary.
	 */
	outOfContext?: boolean;
	/** Clash-compiled Verilog source files for this module */
	verilogFiles?: string[];
	/** Synthesis time in milliseconds */
	elapsedMs: number;
	/** Statistics for this module */
	statistics?: SynthesisStatistics;
	/** Errors */
	errors: YosysError[];
}

/**
 * Synthesis statistics extracted from Yosys output
 */
export interface SynthesisStatistics {
	/** Number of cells in design */
	cellCount?: number;

	/** Number of wires in design */
	wireCount?: number;

	/** Chip area (if available) */
	chipArea?: number;

	/** Number of different cell types */
	cellTypes?: Map<string, number>;

	/** Longest topological path length (reported by `ltp`), in cells. */
	logicDepth?: number;

	/** Raw statistics text */
	rawStats: string;
}

/**
 * Yosys warning message
 */
export interface YosysWarning {
	/** Warning message */
	message: string;
	
	/** Source file (if available) */
	file?: string;
	
	/** Line number (if available) */
	line?: number;
}

/**
 * Yosys error message
 */
export interface YosysError {
	/** Error message */
	message: string;
	
	/** Source file (if available) */
	file?: string;
	
	/** Line number (if available) */
	line?: number;
}
