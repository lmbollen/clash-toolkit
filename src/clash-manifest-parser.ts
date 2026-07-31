import * as path from 'path';
import { promises as fs } from 'fs';
import {
	ClashManifest,
	ParsedClashManifest,
	ClashDomain,
	ClashPort,
	ComponentInfo,
	TopClock,
} from './clash-manifest-types';

/**
 * The clock place & route is constrained against, or undefined when the design
 * has none.
 *
 * nextpnr's `--freq` is a single number applied to the whole design, so a
 * design driven by clocks in two domains cannot be expressed by it. Rather than
 * pick one — which would produce a confident "constraints met" verdict about a
 * constraint half the design never had — this refuses, and says which clocks
 * are in play. Several ports in the *same* domain are one constraint and pass.
 */
export function pnrTargetClock(
	manifest: ParsedClashManifest | undefined,
): TopClock | undefined {
	if (!manifest) {
		throw new Error(
			'No Clash manifest for this run, so the design\'s clock is unknown — ' +
			'cannot decide what to constrain place & route against.'
		);
	}
	const byDomain = new Map(manifest.topClocks.map(c => [c.domain, c]));
	if (byDomain.size === 0) { return undefined; }
	if (byDomain.size === 1) { return manifest.topClocks[0]; }

	const described = [...byDomain.values()]
		.map(c => `${c.port} (${c.domain}, ${c.frequencyMHz.toFixed(2)} MHz)`)
		.join(', ');
	throw new Error(
		`${manifest.top_component.name} is driven by clocks in ${byDomain.size} ` +
		`domains — ${described}. nextpnr takes one --freq for the whole design, ` +
		'so there is no single target to place and route against.'
	);
}

/**
 * Parser and analyzer for Clash manifest files
 */
export class ClashManifestParser {
	/**
	 * Read and parse a clash-manifest.json file
	 */
	async parseManifest(manifestPath: string): Promise<ParsedClashManifest> {
		// Read the manifest file
		const content = await fs.readFile(manifestPath, 'utf8');
		const manifest: ClashManifest = JSON.parse(content);

		// Validate the shape before consumers dereference into it — a
		// truncated write or schema drift across Clash versions should
		// surface as a clear message, not a TypeError deep in a caller.
		const missing = ['files', 'domains', 'components', 'top_component']
			.filter(field => (manifest as unknown as Record<string, unknown>)[field] === undefined);
		if (!Array.isArray(manifest.files) || !Array.isArray(manifest.components) || missing.length > 0) {
			throw new Error(
				`Manifest ${manifestPath} is missing or has malformed fields: ` +
				`${missing.length > 0 ? missing.join(', ') : 'files/components must be arrays'}. ` +
				'It may be truncated or produced by an unsupported Clash version.'
			);
		}

		// Extract directory
		const directory = path.dirname(manifestPath);

		// Extract Verilog files
		const verilogFiles = manifest.files
			.filter(f => f.name.endsWith('.v'))
			.map(f => path.join(directory, f.name));

		// Pair the top entity's clocks with their domains
		const topClocks = this.topClocksOf(
			manifestPath,
			manifest.domains,
			manifest.top_component,
		);

		return {
			...manifest,
			manifestPath,
			directory,
			verilogFiles,
			topClocks
		};
	}

	/**
	 * Find clash-manifest.json in a directory
	 */
	async findManifest(directory: string): Promise<string | undefined> {
		const manifestPath = path.join(directory, 'clash-manifest.json');
		try {
			await fs.access(manifestPath);
			return manifestPath;
		} catch {
			return undefined;
		}
	}

	/**
	 * Recursively collect all Verilog files including dependencies
	 */
	async collectAllVerilogFiles(
		manifestPath: string,
		visited: Set<string> = new Set()
	): Promise<string[]> {
		// Avoid infinite loops
		const normalizedPath = path.resolve(manifestPath);
		if (visited.has(normalizedPath)) {
			return [];
		}
		visited.add(normalizedPath);

		// Parse this manifest
		const manifest = await this.parseManifest(manifestPath);
		const allFiles = [...manifest.verilogFiles];

		// Recursively process dependencies
		for (const dep of manifest.dependencies.transitive) {
			// Dependencies are typically paths relative to the HDL output directory
			// We need to find the manifest for each dependency
			const depManifestPath = await this.findDependencyManifest(
				manifest.directory,
				dep
			);

			if (depManifestPath) {
				const depFiles = await this.collectAllVerilogFiles(depManifestPath, visited);
				allFiles.push(...depFiles);
			}
		}

		// Deduplicate files by resolving to absolute paths
		// This prevents the same file from being added multiple times
		const uniqueFiles = Array.from(new Set(allFiles.map(f => path.resolve(f))));
		return uniqueFiles;
	}

	/**
	 * Build a dependency graph of all components reachable from a manifest.
	 * Returns components in dependency order (leaves first, top last).
	 *
	 * Each component's `dependencies` contains only **direct** dependencies,
	 * not transitive ones — even though the Clash manifest lists all deps
	 * as transitive.  This is important for OOC synthesis: when loading
	 * pre-synthesized JSON netlists, each JSON already contains its own
	 * transitive deps.  Loading a transitive dep separately would cause
	 * a "Re-definition of module" error in Yosys.
	 */
	async buildDependencyGraph(manifestPath: string): Promise<ComponentInfo[]> {
		const visited = new Set<string>();
		const components: ComponentInfo[] = [];
		await this.collectComponents(manifestPath, visited, components);

		// If we ended up with a single component whose manifest lists
		// multiple internal sub-modules (common in large Clash designs),
		// expand it into per-Verilog-module ComponentInfo entries so that
		// they can be synthesized in parallel.
		if (components.length === 1) {
			const manifest = await this.parseManifest(manifestPath);
			if (manifest.components.length > 1) {
				return this.expandInternalComponents(manifest);
			}
		}

		// Build a lookup for quick access
		const byName = new Map(components.map(c => [c.name, c]));

		// For each component, remove deps that are transitively reachable
		// through another dep (i.e. keep only the direct/minimal deps).
		for (const comp of components) {
			comp.dependencies = this.removeTransitiveDeps(comp.dependencies, byName);
		}

		return components;
	}

	/**
	 * Expand a single manifest that contains multiple internal components
	 * into separate ComponentInfo entries.
	 *
	 * Clash sometimes generates all sub-modules as separate .v files within
	 * a single manifest directory (no separate manifest per sub-module).
	 * In that case `dependencies.transitive` is empty but `components[]`
	 * lists all Verilog module names.
	 *
	 * We scan the Verilog files for module instantiations to build the
	 * dependency graph, then return components in topological order.
	 */
	private async expandInternalComponents(
		manifest: ParsedClashManifest
	): Promise<ComponentInfo[]> {
		const dir = manifest.directory;
		const componentNames = new Set(manifest.components);

		// Map each component name to its Verilog file.
		// Manifest files list name as e.g. "foo.v"; component name is "foo".
		const verilogByComponent = new Map<string, string>();
		const extraVerilogFiles: string[] = []; // .v files not matching any component
		const seenExtras = new Set<string>();
		for (const vFile of manifest.verilogFiles) {
			const base = path.basename(vFile, '.v');
			if (componentNames.has(base)) {
				verilogByComponent.set(base, vFile);
			} else if (!seenExtras.has(vFile)) {
				seenExtras.add(vFile);
				extraVerilogFiles.push(vFile);
			}
		}

		// For extra Verilog files (e.g. SpinalHDL-generated), extract the
		// actual module names they define so we can match by instantiation
		// rather than filename.  The filename may differ from the module
		// name significantly (e.g. "vex_risc_top_Riscv32imc0VexRiscv_HASH.v"
		// defines module "Riscv32imc0VexRiscv").
		const extraFileModules = new Map<string, string[]>(); // file → module names
		for (const extra of extraVerilogFiles) {
			const content = await fs.readFile(extra, 'utf8');
			const modules: string[] = [];
			const moduleRe = /^\s*module\s+(\w+)/gm;
			let m;
			while ((m = moduleRe.exec(content)) !== null) {
				modules.push(m[1]);
			}
			extraFileModules.set(extra, modules);
		}

		// Scan each Verilog file for instantiations of other components
		// in this manifest.  Use word-boundary matching to avoid false
		// positives (e.g. "wbStorage" matching inside "wbStorage_0").
		// Also track which extra Verilog files are referenced by each component.
		const deps = new Map<string, string[]>();
		const extraFilesFor = new Map<string, string[]>();
		for (const name of manifest.components) {
			const vFile = verilogByComponent.get(name);
			if (!vFile) {
				deps.set(name, []);
				extraFilesFor.set(name, []);
				continue;
			}
			const content = await fs.readFile(vFile, 'utf8');
			const moduleDeps: string[] = [];
			for (const other of manifest.components) {
				if (other !== name) {
					// Use word-boundary regex to avoid substring false positives
					const re = new RegExp('\\b' + other.replace(/[.*+?^${}()|[\]\\]/g, '\\$&') + '\\b');
					if (re.test(content)) {
						moduleDeps.push(other);
					}
				}
			}
			deps.set(name, moduleDeps);

			// Attach extra (non-component) Verilog files whose *module names*
			// appear in this component's Verilog (not by filename, which may
			// differ from the actual Verilog module name).
			const extras: string[] = [];
			for (const extra of extraVerilogFiles) {
				const moduleNames = extraFileModules.get(extra) || [];
				const referenced = moduleNames.some(modName => {
					const re = new RegExp('\\b' + modName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&') + '\\b');
					return re.test(content);
				});
				if (referenced) {
					extras.push(extra);
				}
			}
			extraFilesFor.set(name, extras);
		}

		// Topological sort — leaves first, top last, via simple iteration.
		const result: ComponentInfo[] = [];
		const completed = new Set<string>();
		const remaining = new Set(manifest.components);

		while (remaining.size > 0) {
			const ready: string[] = [];
			for (const name of remaining) {
				const d = deps.get(name) || [];
				if (d.every(dep => completed.has(dep))) {
					ready.push(name);
				}
			}

			if (ready.length === 0) {
				// Circular — add everything remaining
				for (const name of remaining) { ready.push(name); }
			}

			for (const name of ready) {
				remaining.delete(name);
				completed.add(name);

				// Collect Verilog files: the component's own file plus any
				// non-component .v files it references (e.g. SpinalHDL-generated
				// Verilog not listed in components[])
				const ownFile = verilogByComponent.get(name);
				const extras = extraFilesFor.get(name) || [];
				const vFiles = [...extras, ...(ownFile ? [ownFile] : [])];

				result.push({
					name,
					verilogFiles: vFiles,
					dependencies: deps.get(name) || [],
					directory: dir
				});
			}
		}

		return result;
	}

	/**
	 * Given a list of dep names, remove any that are transitively reachable
	 * through another dep in the list.
	 */
	private removeTransitiveDeps(
		deps: string[],
		byName: Map<string, ComponentInfo>
	): string[] {
		if (deps.length <= 1) { return deps; }

		// Collect all names transitively reachable from each dep
		const transitiveOf = new Map<string, Set<string>>();
		const getTransitive = (name: string): Set<string> => {
			if (transitiveOf.has(name)) { return transitiveOf.get(name)!; }
			const result = new Set<string>();
			transitiveOf.set(name, result); // cache early to handle cycles
			const comp = byName.get(name);
			if (comp) {
				for (const d of comp.dependencies) {
					result.add(d);
					for (const t of getTransitive(d)) {
						result.add(t);
					}
				}
			}
			return result;
		};

		// A dep is redundant if it's transitively included by another dep
		const allTransitive = new Set<string>();
		for (const d of deps) {
			for (const t of getTransitive(d)) {
				allTransitive.add(t);
			}
		}

		return deps.filter(d => !allTransitive.has(d));
	}

	/**
	 * Recursively collect components in post-order (dependencies before dependents).
	 * Returns the top_component.name of the manifest at `manifestPath`.
	 */
	private async collectComponents(
		manifestPath: string,
		visited: Set<string>,
		components: ComponentInfo[]
	): Promise<string | null> {
		const normalizedPath = path.resolve(manifestPath);
		if (visited.has(normalizedPath)) {
			const existing = components.find(c =>
				path.resolve(c.directory, 'clash-manifest.json') === normalizedPath
			);
			return existing?.name ?? null;
		}
		visited.add(normalizedPath);

		const manifest = await this.parseManifest(manifestPath);
		const depComponentNames: string[] = [];

		for (const dep of manifest.dependencies.transitive) {
			const depManifestPath = await this.findDependencyManifest(manifest.directory, dep);
			if (depManifestPath) {
				const depName = await this.collectComponents(depManifestPath, visited, components);
				if (depName) {
					depComponentNames.push(depName);
				}
			}
		}

		const name = manifest.top_component.name;
		components.push({
			name,
			verilogFiles: manifest.verilogFiles,
			dependencies: depComponentNames,
			directory: manifest.directory
		});
		return name;
	}

	/**
	 * Find manifest file for a dependency
	 * 
	 * Dependencies in the manifest are module names like "Other.Module.topEntity"
	 * We need to search for the corresponding manifest file
	 */
	private async findDependencyManifest(
		baseDirectory: string,
		dependencyName: string
	): Promise<string | undefined> {
		// Try common locations relative to base directory
		const searchPaths = [
			// Same parent directory (sibling module)
			path.join(path.dirname(baseDirectory), dependencyName, 'clash-manifest.json'),
			// In components subdirectory
			path.join(baseDirectory, '..', 'components', dependencyName, 'clash-manifest.json'),
			// Direct subdirectory
			path.join(baseDirectory, dependencyName, 'clash-manifest.json')
		];

		for (const searchPath of searchPaths) {
			try {
				await fs.access(searchPath);
				return searchPath;
			} catch {
				continue;
			}
		}

		return undefined;
	}

	/**
	 * Pair each of the top entity's clock ports with the domain it declares.
	 *
	 * The manifest states which ports are clocks, which domain each belongs to,
	 * and what every domain's period is — so this reads it, and never guesses
	 * around it.  A clock naming a domain the manifest doesn't define, or a
	 * domain without a usable period, means the manifest disagrees with itself:
	 * that is a hard error rather than a reason to substitute another domain,
	 * because the number ends up as the constraint place & route is judged
	 * against and a plausible wrong answer is worse than none.
	 *
	 * An empty result means the top entity has no clock port. That is a real
	 * property of a combinational design, not a missing value.
	 */
	private topClocksOf(
		manifestPath: string,
		domains: Record<string, ClashDomain>,
		topComponent: { name?: string; ports_flat?: ClashPort[] },
	): TopClock[] {
		const clockPorts = (topComponent.ports_flat ?? []).filter(p => p.is_clock);

		return clockPorts.map(port => {
			if (!port.domain) {
				throw new Error(
					`Manifest ${manifestPath}: clock port "${port.name}" declares no ` +
					'clock domain, so its frequency is unknown.'
				);
			}
			const domain = domains[port.domain];
			if (!domain) {
				throw new Error(
					`Manifest ${manifestPath}: clock port "${port.name}" is in domain ` +
					`"${port.domain}", which the manifest does not define ` +
					`(it defines: ${Object.keys(domains).join(', ') || 'none'}).`
				);
			}
			if (typeof domain.period !== 'number' || domain.period <= 0) {
				throw new Error(
					`Manifest ${manifestPath}: domain "${port.domain}" has no usable ` +
					`clock period (got ${JSON.stringify(domain.period)}).`
				);
			}
			return {
				port: port.name,
				domain: port.domain,
				periodPs: domain.period,
				// period is in ps: frequency = 1e12 / period Hz = 1e6 / period MHz
				frequencyMHz: 1_000_000 / domain.period,
			};
		});
	}

	/**
	 * Get clock and reset port names from manifest
	 */
	getClockResetPorts(manifest: ParsedClashManifest): {
		clocks: string[];
		resets: string[];
	} {
		const clocks: string[] = [];
		const resets: string[] = [];

		for (const port of manifest.top_component.ports_flat) {
			if (port.is_clock) {
				clocks.push(port.name);
			} else if (port.name.toUpperCase().includes('RST') || port.name.toUpperCase().includes('RESET')) {
				resets.push(port.name);
			}
		}

		return { clocks, resets };
	}

	/**
	 * Generate timing constraint information from manifest
	 */
	generateTimingConstraints(manifest: ParsedClashManifest): string {
		const constraints: string[] = [];
		
		constraints.push('# Timing Constraints Generated from Clash Manifest');
		constraints.push(`# Design: ${manifest.top_component.name}`);
		constraints.push('');

		// Clock constraints for each domain
		for (const [domainName, domain] of Object.entries(manifest.domains)) {
			const periodNs = domain.period / 1000; // ps to ns
			const freqMHz = 1000 / periodNs;

			constraints.push(`# Domain: ${domainName}`);
			constraints.push(`# Period: ${periodNs.toFixed(3)} ns (${freqMHz.toFixed(2)} MHz)`);

			// Find clock ports for this domain
			const clockPorts = manifest.top_component.ports_flat.filter(
				p => p.is_clock && p.domain === domainName
			);

			for (const clockPort of clockPorts) {
				constraints.push(`create_clock -period ${periodNs.toFixed(3)} [get_ports ${clockPort.name}]`);
			}

			constraints.push('');
		}

		return constraints.join('\n');
	}
}
