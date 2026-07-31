import * as path from 'path';
import { promises as fs } from 'fs';
import { ModuleSynthesisResult, SynthesisStatistics } from './yosys-types';
import { NextpnrResult } from './nextpnr-types';
import { NextpnrRunner } from './nextpnr-runner';
import { PerModuleHierarchy, YosysRunner } from './yosys-runner';

/**
 * Persisted summary written into each run's `run.json`.  Mirrored here (instead
 * of imported) so the loader stays decoupled from the tree-view module that
 * originally defined it.
 */
export interface RunMetadata {
    runId: string;
    command: string;
    function: string;
    functionFile: string;
    timestamp: string;
    success?: boolean;
    target?: string;
    mode?: string;
    cellCount?: number;
    wireCount?: number;
    logicDepth?: number;
    moduleCount?: number;
    maxFrequencyMHz?: number;
    device?: string;
    deviceLabel?: string;
    packageName?: string;
    /** The top entity's clocks, as the manifest stated them. */
    clocks?: { port: string; domain: string; frequencyMHz: number }[];
    constraintsMet?: boolean;
    topModule?: string;
}

export interface LoadedRun {
    modules: ModuleSynthesisResult[];
    pnr?: NextpnrResult;
    topModule?: string;
    meta?: RunMetadata;
}

export async function readRunMeta(runRoot: string): Promise<RunMetadata | undefined> {
    try {
        const raw = await fs.readFile(path.join(runRoot, 'run.json'), 'utf8');
        return JSON.parse(raw) as RunMetadata;
    } catch {
        return undefined;
    }
}

async function exists(p: string): Promise<boolean> {
    try { await fs.access(p); return true; } catch { return false; }
}

async function readPerModuleHierarchy(
    perModuleDir: string,
): Promise<PerModuleHierarchy | undefined> {
    try {
        const raw = await fs.readFile(path.join(perModuleDir, 'hierarchy.json'), 'utf8');
        const parsed = JSON.parse(raw) as PerModuleHierarchy;
        return parsed.components ? parsed : undefined;
    } catch {
        return undefined;
    }
}

async function loadStatsIfExists(dir: string): Promise<SynthesisStatistics | undefined> {
    try {
        const raw = await fs.readFile(path.join(dir, 'stats.json'), 'utf8');
        return YosysRunner.parseStatsJson(raw);
    } catch {
        return undefined;
    }
}

/**
 * The Verilog belonging to one component of a run.
 *
 * Clash writes a directory per component — `02-verilog/<Module.entity>/<name>.v`
 * — so this has to search the tree, not the top of it. Listing only the top
 * found nothing but directory names, which is why per-module runs (every
 * elaboration, and out-of-context synthesis) offered no Verilog to open at all.
 *
 * Only files named after the component are returned: a component whose Verilog
 * isn't there gets no rows rather than someone else's files.
 */
async function findVerilogFor(dir: string, moduleName: string): Promise<string[]> {
    const all = await findAllVerilog(dir);
    return all.filter(file => {
        const name = path.basename(file);
        return name === `${moduleName}.v`
            || name === `${moduleName}.sv`
            || name.startsWith(`${moduleName}_`);
    });
}

async function findAllVerilog(dir: string): Promise<string[]> {
    const out: string[] = [];
    try {
        const dirents = await fs.readdir(dir, { withFileTypes: true });
        for (const d of dirents) {
            const full = path.join(dir, d.name);
            if (d.isDirectory()) {
                out.push(...await findAllVerilog(full));
            } else if (d.name.endsWith('.v') || d.name.endsWith('.sv')) {
                out.push(full);
            }
        }
    } catch { /* dir absent */ }
    return out;
}

async function loadModuleFromDir(
    moduleName: string,
    moduleDir: string,
    verilogDir: string,
): Promise<ModuleSynthesisResult> {
    const stats = await loadStatsIfExists(moduleDir);

    // `<module>.dot.svg` is a legacy name from the Graphviz era — kept so runs
    // recorded by older extension versions still open from history.
    const svgCandidates = [
        path.join(moduleDir, `${moduleName}.svg`),
        path.join(moduleDir, `${moduleName}.dot.svg`),
    ];
    let svgPath: string | undefined;
    for (const c of svgCandidates) {
        if (await exists(c)) { svgPath = c; break; }
    }

    const jsonCandidates = [
        path.join(moduleDir, `${moduleName}_diagram.json`),
        path.join(moduleDir, `${moduleName}.json`),
    ];
    let diagramJsonPath: string | undefined;
    for (const c of jsonCandidates) {
        if (await exists(c)) { diagramJsonPath = c; break; }
    }

    const verilogFiles = await findVerilogFor(verilogDir, moduleName);

    return {
        name: moduleName,
        success: true,
        svgPath,
        diagramJsonPath,
        verilogFiles,
        elapsedMs: 0,
        statistics: stats,
        errors: [],
    };
}

/**
 * Reconstruct ModuleSynthesisResult[] from a run directory on disk.
 *
 * Handles both layouts produced by the runner:
 *   - per-module: `03-yosys/per-module/<moduleName>/...`
 *   - whole-design: a single set of files directly under `03-yosys/`.
 */
export async function loadRunModules(
    runRoot: string,
    meta?: RunMetadata,
): Promise<{ modules: ModuleSynthesisResult[]; topModule?: string }> {
    const yosysDir = path.join(runRoot, '03-yosys');
    const verilogDir = path.join(runRoot, '02-verilog');

    // Per-module mode
    const perModuleDir = path.join(yosysDir, 'per-module');
    let perModuleEntries: string[] = [];
    try {
        const dirents = await fs.readdir(perModuleDir, { withFileTypes: true });
        perModuleEntries = dirents.filter(d => d.isDirectory()).map(d => d.name).sort();
    } catch { /* not per-module mode */ }

    if (perModuleEntries.length > 0) {
        // Written by the per-module pass; absent for runs recorded before it
        // existed, which then simply show a flat list of components.
        const hierarchy = await readPerModuleHierarchy(perModuleDir);
        const modules: ModuleSynthesisResult[] = [];
        for (const name of perModuleEntries) {
            const module = await loadModuleFromDir(
                name, path.join(perModuleDir, name), verilogDir
            );
            if (hierarchy) {
                module.subComponents = (hierarchy.components[name] ?? [])
                    .filter(child => perModuleEntries.includes(child));
                module.outOfContext = hierarchy.outOfContext;
            }
            modules.push(module);
        }
        return { modules, topModule: hierarchy?.top ?? meta?.topModule };
    }

    // Whole-design mode — single module
    let yosysFiles: string[] = [];
    try { yosysFiles = await fs.readdir(yosysDir); } catch { return { modules: [] }; }

    let topModule = meta?.topModule;
    if (!topModule) {
        const svg = yosysFiles.find(f => f.endsWith('.svg'));
        if (svg) { topModule = path.basename(svg, '.svg').replace(/\.dot$/, ''); }
    }
    if (!topModule) {
        topModule = meta?.function?.split('.').pop() ?? 'design';
    }

    const stats = await loadStatsIfExists(yosysDir);

    let svgPath: string | undefined;
    for (const c of [
        path.join(yosysDir, `${topModule}.svg`),
        path.join(yosysDir, `${topModule}.dot.svg`),
    ]) {
        if (await exists(c)) { svgPath = c; break; }
    }
    if (!svgPath) {
        const svg = yosysFiles.find(f => f.endsWith('.svg'));
        if (svg) { svgPath = path.join(yosysDir, svg); }
    }

    let diagramJsonPath: string | undefined;
    const jsonCandidate = path.join(yosysDir, `${topModule}.json`);
    if (await exists(jsonCandidate)) { diagramJsonPath = jsonCandidate; }

    const verilogFiles = await findAllVerilog(verilogDir);

    const modules: ModuleSynthesisResult[] = [];
    if (stats || svgPath || verilogFiles.length > 0) {
        modules.push({
            name: topModule,
            success: meta?.success !== false,
            svgPath,
            diagramJsonPath,
            verilogFiles,
            elapsedMs: 0,
            statistics: stats,
            errors: [],
        });
    }
    return { modules, topModule };
}

/**
 * Reconstruct nextpnr timing/utilization/critical-path data from
 * `04-nextpnr/report.json`. Returns undefined when no report exists.
 */
export async function loadRunPnr(runRoot: string, meta?: RunMetadata): Promise<NextpnrResult | undefined> {
    const reportPath = path.join(runRoot, '04-nextpnr', 'report.json');
    const report = await NextpnrRunner.loadReportJson(reportPath);
    if (!report) { return undefined; }

    const family = meta?.target ?? 'generic';
    const timing = NextpnrRunner.timingFromReport(report);
    const utilization = NextpnrRunner.utilizationFromReport(report, family);
    const criticalPaths = NextpnrRunner.criticalPathsFromReport(report);

    return {
        success: meta?.success !== false,
        timing,
        utilization,
        criticalPaths,
        output: '',
        warnings: [],
        errors: [],
        reportJsonPath: reportPath,
    };
}

export async function loadRun(runRoot: string): Promise<LoadedRun> {
    const meta = await readRunMeta(runRoot);
    const { modules, topModule } = await loadRunModules(runRoot, meta);
    const pnr = await loadRunPnr(runRoot, meta);
    return { modules, pnr, topModule, meta };
}
