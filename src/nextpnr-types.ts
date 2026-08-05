/**
 * Types for nextpnr place-and-route integration
 */

/**
 * Supported FPGA families for nextpnr
 */
// Note: nexus/machxo2 were removed — their nextpnr binaries take different
// output/constraint flags (--fasm/--pdc) that buildNextpnrArgs does not
// emit, so claiming support was misleading. Re-add with proper arg handling.
export type NextpnrFamily = 'ecp5' | 'ice40' | 'gowin' | 'generic';

/**
 * ECP5 chip variants
 */
export type ECP5Device =
	| '25k' | 'um-25k' | 'um5g-25k'  // LFE5U-25F, LFE5UM-25F, LFE5UM5G-25F
	| '45k' | 'um-45k' | 'um5g-45k'  // LFE5U-45F, LFE5UM-45F, LFE5UM5G-45F
	| '85k' | 'um-85k' | 'um5g-85k'; // LFE5U-85F, LFE5UM-85F, LFE5UM5G-85F

/**
 * ECP5 package types
 */
export type ECP5Package =
	| 'CABGA256' | 'CABGA381' | 'CABGA554' | 'CABGA756'
	| 'CSFBGA285' | 'CSFBGA381' | 'CSFBGA554';

/**
 * Speed grades for ECP5
 */
export type ECP5SpeedGrade = '6' | '7' | '8';

// ---------------------------------------------------------------------------
// Device picker data — maps synthesis targets to nextpnr families and devices
// ---------------------------------------------------------------------------

/** A device option shown in the device picker. */
export interface DeviceOption {
	label: string;
	value: string;
	description: string;
	/** Extra --vopt arguments required for this device (e.g. 'family=GW1N-9C' for Gowin). */
	vopt?: string;
}

/** Per-family PnR metadata. */
export interface PnrFamilyInfo {
	/** nextpnr family id */
	family: NextpnrFamily;
	/** nextpnr binary name */
	binary: string;
	/** Devices the user can pick from. */
	devices: DeviceOption[];
	/** How the device value is passed to nextpnr (`--<value>` for ECP5, `--<flag> <value>` for others). */
	deviceFlag: 'prefix' | 'device';
}

/**
 * Map from synthesis target id → PnR info.
 * Only targets that have a corresponding nextpnr binary are present.
 */
export const PNR_FAMILIES: ReadonlyMap<string, PnrFamilyInfo> = new Map([
	['ecp5', {
		family: 'ecp5' as NextpnrFamily,
		binary: 'nextpnr-ecp5',
		deviceFlag: 'prefix' as const,
		devices: [
			{ label: 'LFE5U-25F',    value: '25k',      description: '25k LUTs' },
			{ label: 'LFE5U-45F',    value: '45k',      description: '45k LUTs' },
			{ label: 'LFE5U-85F',    value: '85k',      description: '85k LUTs' },
			{ label: 'LFE5UM-25F',   value: 'um-25k',   description: '25k LUTs, 3.2G SERDES' },
			{ label: 'LFE5UM-45F',   value: 'um-45k',   description: '45k LUTs, 3.2G SERDES' },
			{ label: 'LFE5UM-85F',   value: 'um-85k',   description: '85k LUTs, 3.2G SERDES' },
			{ label: 'LFE5UM5G-25F', value: 'um5g-25k', description: '25k LUTs, 5G SERDES' },
			{ label: 'LFE5UM5G-45F', value: 'um5g-45k', description: '45k LUTs, 5G SERDES' },
			{ label: 'LFE5UM5G-85F', value: 'um5g-85k', description: '85k LUTs, 5G SERDES' },
		],
	}],
	['ice40', {
		family: 'ice40' as NextpnrFamily,
		binary: 'nextpnr-ice40',
		deviceFlag: 'prefix' as const,
		devices: [
			{ label: 'iCE40 LP384',   value: 'lp384',   description: '384 LCs' },
			{ label: 'iCE40 LP1K',    value: 'lp1k',    description: '1280 LCs' },
			{ label: 'iCE40 LP4K',    value: 'lp4k',    description: '3520 LCs' },
			{ label: 'iCE40 LP8K',    value: 'lp8k',    description: '7680 LCs' },
			{ label: 'iCE40 HX1K',    value: 'hx1k',    description: '1280 LCs' },
			{ label: 'iCE40 HX4K',    value: 'hx4k',    description: '3520 LCs' },
			{ label: 'iCE40 HX8K',    value: 'hx8k',    description: '7680 LCs' },
			{ label: 'iCE40 UP3K',    value: 'up3k',    description: '2800 LCs, UltraPlus' },
			{ label: 'iCE40 UP5K',    value: 'up5k',    description: '5280 LCs, UltraPlus' },
			{ label: 'iCE40 U4K',     value: 'u4k',     description: '3520 LCs' },
		],
	}],
	['gowin', {
		family: 'gowin' as NextpnrFamily,
		binary: 'nextpnr-himbaechel',
		deviceFlag: 'device' as const,
		devices: [
			{ label: 'GW1N-1 (QN48)',       value: 'GW1N-LV1QN48C6/I5',      description: '1152 LUTs' },
			{ label: 'GW1N-4 (LQ144)',      value: 'GW1N-UV4LQ144C6/I5',     description: '4608 LUTs' },
			{ label: 'GW1N-9 (QN88)',       value: 'GW1N-LV9QN88C6/I5',      description: '8640 LUTs', vopt: 'family=GW1N-9' },
			{ label: 'GW1N-9C (QN88)',      value: 'GW1N-LV9QN88C6/I5',      description: '8640 LUTs', vopt: 'family=GW1N-9C' },
			{ label: 'GW1NR-9 (QN88)',      value: 'GW1NR-LV9QN88PC6/I5',    description: '8640 LUTs, with SDRAM', vopt: 'family=GW1N-9' },
			{ label: 'GW1NR-9C (QN88)',     value: 'GW1NR-LV9QN88PC6/I5',    description: '8640 LUTs, with SDRAM', vopt: 'family=GW1N-9C' },
			{ label: 'GW1NSR-4C (QN48)',    value: 'GW1NSR-LV4CQN48PC7/I6',  description: '4608 LUTs, with SDRAM' },
			{ label: 'GW2A-18 (QN88)',      value: 'GW2A-LV18QN88C8/I7',     description: '20736 LUTs', vopt: 'family=GW2A-18' },
			{ label: 'GW2A-18C (QN88)',     value: 'GW2A-LV18QN88C8/I7',     description: '20736 LUTs', vopt: 'family=GW2A-18C' },
		],
	}],
]);

/**
 * Options for nextpnr execution
 */
export interface NextpnrOptions {
	/**
	 * FPGA family (ice40, ecp5, etc.)
	 */
	family: NextpnrFamily;

	/**
	 * Input JSON file from Yosys
	 */
	jsonPath: string;

	/**
	 * Output directory for results
	 */
	outputDir: string;

	/**
	 * Top module name
	 */
	topModule: string;

	/**
	 * Device-specific options for ECP5
	 */
	ecp5?: {
		device: ECP5Device;
		package: ECP5Package;
		speedGrade?: ECP5SpeedGrade;
	};

	/** Generic device string (e.g. 'hx8k', 'GW1N-9'). Used for ice40, gowin, etc. */
	device?: string;

	/** Generic package string. Used for ice40, gowin, etc. */
	packageName?: string;

	/** Extra --vopt arguments (e.g. ['family=GW1N-9C', 'cst=file.cst'] for Gowin himbaechel). */
	vopt?: string[];

	/**
	 * Constraints file (LPF for ECP5, PCF for iCE40, CST for Gowin)
	 */
	constraintsFile?: string;

	/**
	 * Target frequency in MHz (for timing analysis)
	 */
	frequency?: number;

	/**
	 * Seed for deterministic placement
	 */
	seed?: number;

	/**
	 * Threads for the passes that support them (`--threads`), from
	 * `clash-toolkit.nextpnrThreads`. `auto`/unset derives it from the machine.
	 *
	 * Note that this does not make a run reproducible on its own: the passes
	 * that thread are the ones whose result can depend on scheduling, so a
	 * fixed `seed` only pins the outcome at a fixed thread count.
	 */
	threads?: number | string | null;

	/**
	 * Write a routed-layout SVG alongside other outputs.  Handy for
	 * visualising where the design ended up on the fabric.
	 */
	routedSvg?: boolean;

	/**
	 * Additional nextpnr arguments
	 */
	extraArgs?: string[];

	/**
	 * Optional callback receiving short progress messages parsed from
	 * nextpnr's output. Used to surface the current stage in the VS Code
	 * progress notification so the user knows things are still moving.
	 */
	progressUpdate?: (message: string) => void;

	/**
	 * Optional abort signal. When triggered, the runner sends SIGTERM to
	 * nextpnr, then resolves with `success: false` and an
	 * error noting the cancellation.
	 */
	abortSignal?: AbortSignal;
}

/**
 * One step of a critical path — an edge in the post-route timing graph.
 * Types emitted by nextpnr include 'source', 'routing', 'setup', 'clk-to-q'.
 */
export interface CriticalPathStep {
	/** Delay contributed by this step (nanoseconds). */
	delay: number;
	/** Step kind as reported by nextpnr. */
	type: string;
	/** Source cell name, when this step has one. */
	fromCell?: string;
	/** Destination cell name. */
	toCell?: string;
	/** Net carrying the signal (populated for routing edges). */
	net?: string;
}

/**
 * A single critical path: start/end (clock domains or async) and the chain
 * of steps with their accumulated delay.
 */
export interface CriticalPath {
	/** Launch clock / async source (e.g. "posedge clk$..."). */
	from: string;
	/** Capture clock / async sink. */
	to: string;
	/** Sum of step delays, ns. */
	totalDelay: number;
	/** Ordered path steps. */
	steps: CriticalPathStep[];
}

/**
 * Result from nextpnr place-and-route
 */
export interface NextpnrResult {
	/**
	 * Whether PnR succeeded
	 */
	success: boolean;

	/**
	 * Path to generated textual config (if successful)
	 */
	textcfgPath?: string;

	/**
	 * Path to the JSON report written by nextpnr's --report flag
	 */
	reportJsonPath?: string;

	/**
	 * Path to the routed-layout SVG (if --routed-svg was requested)
	 */
	routedSvgPath?: string;

	/**
	 * Combined stdout/stderr output
	 */
	output: string;

	/**
	 * Timing information
	 */
	timing?: TimingInfo;

	/**
	 * Resource utilization
	 */
	utilization?: UtilizationInfo;

	/**
	 * Critical paths reported by nextpnr's --report.
	 * One entry per reported path, sorted by totalDelay descending.
	 */
	criticalPaths?: CriticalPath[];

	/**
	 * Warnings from nextpnr
	 */
	warnings: NextpnrWarning[];

	/**
	 * Errors from nextpnr
	 */
	errors: NextpnrError[];
}

/**
 * Timing information from nextpnr
 */
export interface TimingInfo {
	/**
	 * Maximum frequency achieved (MHz) - FINAL value after routing
	 * This is the actual achievable frequency including routing delays
	 */
	maxFrequency?: number;

	/**
	 * Maximum frequency estimate after placement, before routing (MHz)
	 * This is typically higher than maxFrequency as it doesn't include routing delays
	 */
	prePlacementFrequency?: number;

	/**
	 * Critical path delay (ns)
	 */
	criticalPathDelay?: number;

	/**
	 * Whether timing constraints were met
	 */
	constraintsMet: boolean;

	/**
	 * Setup slack (if available)
	 */
	setupSlack?: number;

	/**
	 * Hold slack (if available)
	 */
	holdSlack?: number;
}

/**
 * Resource utilization from nextpnr
 */
export interface UtilizationInfo {
	/**
	 * Logic cells used
	 */
	luts?: { used: number; total: number };

	/**
	 * Registers/flip-flops used
	 */
	registers?: { used: number; total: number };

	/**
	 * Block RAM (EBR) used
	 */
	bram?: { used: number; total: number };

	/**
	 * DSP blocks used
	 */
	dsp?: { used: number; total: number };

	/**
	 * IO pins used
	 */
	io?: { used: number; total: number };
}

/**
 * Warning from nextpnr
 */
export interface NextpnrWarning {
	message: string;
	location?: string;
}

/**
 * Error from nextpnr
 */
export interface NextpnrError {
	message: string;
	location?: string;
}
