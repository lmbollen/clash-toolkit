/**
 * Types for Clash manifest JSON files
 * 
 * Clash generates a clash-manifest.json file in each HDL output directory
 * that contains metadata about the generated design.
 */

/**
 * Clock domain configuration from Clash
 */
export interface ClashDomain {
	/** Clock edge to trigger on */
	active_edge: 'Rising' | 'Falling';
	
	/** How uninitialized values are handled */
	init_behavior: 'Defined' | 'Unknown';
	
	/** Clock period in picoseconds */
	period: number;
	
	/** Type of reset signal */
	reset_kind: 'Asynchronous' | 'Synchronous';
	
	/** Reset signal polarity */
	reset_polarity: 'ActiveHigh' | 'ActiveLow';
}

/**
 * Port information from top component
 */
export interface ClashPort {
	/** Port direction */
	direction: 'in' | 'out' | 'inout';
	
	/** Clock domain this port belongs to (if applicable) */
	domain?: string;
	
	/** Whether this is a clock signal */
	is_clock: boolean;
	
	/** Port name */
	name: string;
	
	/** Type name annotation (e.g., "[7:0]" for buses) */
	type_name: string;
	
	/** Bit width of the port */
	width: number;
}

/**
 * Top component metadata
 */
export interface ClashTopComponent {
	/** Component name */
	name: string;
	
	/** Flattened list of all ports */
	ports_flat: ClashPort[];
}

/**
 * Generated file metadata
 */
export interface ClashFile {
	/** File name (relative to manifest location) */
	name: string;
	
	/** SHA256 hash of file contents */
	sha256: string;
}

/**
 * Dependency information
 */
export interface ClashDependencies {
	/** List of transitive dependencies (other top entities this depends on) */
	transitive: string[];
}

/**
 * Complete Clash manifest structure
 */
export interface ClashManifest {
	/** List of component names in this design */
	components: string[];
	
	/** Dependency information */
	dependencies: ClashDependencies;
	
	/** Clock domain configurations */
	domains: Record<string, ClashDomain>;
	
	/** List of generated files */
	files: ClashFile[];
	
	/** Build flags used */
	flags: number[];
	
	/** Hash of the manifest content */
	hash: string;
	
	/** Top component metadata */
	top_component: ClashTopComponent;
	
	/** Clash version used */
	version: string;
}

/**
 * A clock the top entity is driven by.
 *
 * Every field is stated by the manifest: the port, the domain it declares
 * itself to be in, and that domain's period.  Nothing here is inferred.
 */
export interface TopClock {
	/** Clock port on the top entity. */
	port: string;

	/** Domain the port belongs to. */
	domain: string;

	/** The domain's clock period, in picoseconds. */
	periodPs: number;

	/** The same period expressed as a frequency. */
	frequencyMHz: number;
}

/**
 * Parsed manifest with additional computed information
 */
export interface ParsedClashManifest extends ClashManifest {
	/** Absolute path to the manifest file */
	manifestPath: string;

	/** Directory containing the manifest */
	directory: string;

	/** List of Verilog files (extracted from files array) */
	verilogFiles: string[];

	/**
	 * The top entity's clock ports paired with their domains, in port order.
	 * Empty for a design with no clock — not a stand-in for one.
	 */
	topClocks: TopClock[];
}

/**
 * A single Clash component in a design's dependency graph.
 * Used for out-of-context parallel synthesis.
 */
export interface ComponentInfo {
	/** Top component name from manifest */
	name: string;
	/** Verilog files for this component */
	verilogFiles: string[];
	/** Names of direct dependency components */
	dependencies: string[];
	/** Directory containing the manifest */
	directory: string;
}
