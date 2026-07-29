import * as vscode from 'vscode';
import { ModuleSynthesisResult } from './yosys-types';
import { CriticalPath, NextpnrResult } from './nextpnr-types';
import { subComponentsOf } from './netlist-diagram';

type SynthTreeNode =
    | ModuleTreeItem
    | SubComponentItem
    | UtilizationEntry
    | SectionItem
    | KeyValueItem
    | CriticalPathItem;

/**
 * Provides the tree data for the "Synthesis Results" sidebar view.
 *
 * Layout:
 *   ├─ <module-1>  (cells, wires, depth — expandable → cell-type breakdown)
 *   ├─ <module-N>
 *   ├─ Timing             ← only after Place & Route
 *   │   ├─ Max Frequency
 *   │   ├─ Constraint
 *   │   └─ Critical Path Delay
 *   ├─ Utilization        ← only after Place & Route
 *   │   ├─ LUTs
 *   │   └─ ...
 *   └─ Critical Paths     ← only after Place & Route
 *       └─ <from → to>   (expandable → step-by-step)
 *
 * PNR sections are cleared when the user re-runs elaborate or synthesize
 * so stale place-and-route numbers never survive a fresh synthesis.
 */
export class SynthesisResultsTreeProvider
    implements vscode.TreeDataProvider<SynthTreeNode>
{
    private readonly _onDidChangeTreeData =
        new vscode.EventEmitter<SynthTreeNode | undefined | null>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private results: ModuleSynthesisResult[] = [];
    private pnr: NextpnrResult | undefined;

    /**
     * Replace the current contents of the tree.
     *
     * `pnr` is undefined for elaborate / synthesize runs, which intentionally
     * clears any previous PNR section so the view never shows stale Fmax /
     * utilization from a prior placeAndRoute.
     */
    refresh(results: ModuleSynthesisResult[], pnr?: NextpnrResult): void {
        this.results = results;
        this.pnr = pnr;
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: SynthTreeNode): vscode.TreeItem {
        return element;
    }

    async getChildren(element?: SynthTreeNode): Promise<SynthTreeNode[]> {
        if (element instanceof ModuleTreeItem) {
            // Sub-components first — they are navigation; the cell-type
            // breakdown below them is reference data.
            return [
                ...element.childModules,
                ...await subComponentItems(element.netlistPath, element.subComponents),
                ...element.fpgaCells.map(
                    ([name, count]) => new UtilizationEntry(name, count)
                ),
            ];
        }
        if (element instanceof SubComponentItem) {
            return subComponentItems(element.netlistPath, element.subComponents);
        }
        if (element instanceof SectionItem) {
            return element.children;
        }
        if (element instanceof CriticalPathItem) {
            return element.stepItems;
        }

        // Root level
        if (this.results.length === 0 && !this.pnr) {
            const placeholder = new ModuleTreeItem('No synthesis results yet');
            placeholder.description = 'Run elaboration, synthesis or P&R to populate this view';
            placeholder.iconPath = new vscode.ThemeIcon('info');
            placeholder.contextValue = 'placeholder';
            return [placeholder];
        }

        // Per-module runs carry the component graph, so their rows nest exactly
        // like a whole-design run's do — synthesizing each component separately
        // shouldn't flatten how the design is presented.
        const byName = new Map(this.results.map(r => [r.name, r]));
        const claimed = new Set(
            this.results.flatMap(r => (r.subComponents ?? []).filter(n => byName.has(n)))
        );
        // Every component being claimed means the graph is cyclic — impossible
        // for a real design, but fall back to a flat list rather than showing an
        // empty view if it ever happens.
        const unclaimed = this.results.filter(r => !claimed.has(r.name));
        const topLevel = unclaimed.length > 0 ? unclaimed : this.results;

        const roots: SynthTreeNode[] = [];
        for (const r of topLevel) {
            roots.push(await this.buildModuleItem(r, byName, new Set()));
        }

        if (this.pnr) {
            const timingSection = buildTimingSection(this.pnr);
            if (timingSection) { roots.push(timingSection); }

            const utilSection = buildUtilizationSection(this.pnr);
            if (utilSection) { roots.push(utilSection); }

            const cpSection = buildCriticalPathSection(this.pnr.criticalPaths);
            if (cpSection) { roots.push(cpSection); }
        }

        return roots;
    }

    /**
     * Build a module row, recursing into the components it instantiates.
     *
     * Two sources of hierarchy, and a module may have either:
     *   - `subComponents` — components synthesized as their own results (the
     *     per-module / out-of-context flows). Those become full module rows,
     *     carrying their own statistics and diagram.
     *   - the netlist — for a whole-design run, whose sub-modules have no
     *     separate result and are rendered on demand instead.
     *
     * `ancestors` guards against a component graph that (however unlikely)
     * refers back into itself, which would otherwise recurse forever.
     */
    private async buildModuleItem(
        result: ModuleSynthesisResult,
        byName: Map<string, ModuleSynthesisResult>,
        ancestors: Set<string>,
    ): Promise<ModuleTreeItem> {
        const children: ModuleTreeItem[] = [];
        const nextAncestors = new Set(ancestors).add(result.name);
        for (const name of result.subComponents ?? []) {
            const child = byName.get(name);
            if (!child || nextAncestors.has(name)) { continue; }
            children.push(await this.buildModuleItem(child, byName, nextAncestors));
        }

        // Only consult the netlist when the run produced no separate results to
        // nest — otherwise a flattened OOC netlist would contribute nothing and
        // an unflattened one would duplicate the rows above.
        const fromNetlist = children.length === 0 && result.diagramJsonPath
            ? await subComponentsOf(result.diagramJsonPath, result.name)
            : [];

        return new ModuleTreeItem(result, fromNetlist, children);
    }
}

// ── Module item ──────────────────────────────────────────────────────────────

/**
 * What out-of-context synthesis actually does, stated wherever its results are
 * shown. Three things make these figures incomparable to a whole-design run, and
 * all three are visible in the script `runPerModulePass` writes
 * (`proc; flatten; opt -purge; memory -nomap; opt`):
 *
 *   - no technology mapping — no `synth_*` runs, so cells stay generic and the
 *     user's `synthesisTarget` (and any custom script) has no effect here;
 *   - `flatten` inlines each component's dependencies, so a component's figures
 *     include its descendants and per-component numbers overlap;
 *   - a component never sees the design above it, so nothing is optimized
 *     against its parent.
 */
export const OUT_OF_CONTEXT_NOTE =
    '**Synthesized out of context** — on its own, with the components it '
    + 'instantiates flattened into it, and no technology mapping.\n\n'
    + '- Cells stay generic (`$add`, `$dffe`, …), so the `synthesisTarget` does '
    + 'not apply to these figures.\n'
    + '- The figures include this component\'s descendants, so per-component '
    + 'numbers overlap rather than add up.\n'
    + '- Yosys never sees the design above this component, so nothing is '
    + 'optimized against its parent.\n\n'
    + 'Use them to compare components with each other, not to predict '
    + 'whole-design utilization.';

export class ModuleTreeItem extends vscode.TreeItem {
    /** Cell types sorted by count descending. */
    readonly fpgaCells: [string, number][];
    /** Modules this one instantiates, each drillable to its own diagram. */
    readonly subComponents: string[];
    /** Netlist the sub-component diagrams are rendered from. */
    readonly netlistPath?: string;
    /**
     * Sub-components that were synthesized as results of their own (per-module
     * runs), pre-built so the hierarchy can be shown with their statistics.
     */
    readonly childModules: ModuleTreeItem[];

    constructor(labelOrPlaceholder: string);
    constructor(
        result: ModuleSynthesisResult,
        subComponents?: string[],
        childModules?: ModuleTreeItem[],
    );
    constructor(
        arg: string | ModuleSynthesisResult,
        subComponents: string[] = [],
        childModules: ModuleTreeItem[] = [],
    ) {
        if (typeof arg === 'string') {
            super(arg, vscode.TreeItemCollapsibleState.None);
            this.fpgaCells = [];
            this.subComponents = [];
            this.childModules = [];
            return;
        }

        const r = arg;
        const fpgaCells = r.statistics?.cellTypes
            ? Array.from(r.statistics.cellTypes.entries())
                .sort((a, b) => b[1] - a[1])
            : [];

        super(
            r.name,
            fpgaCells.length > 0 || subComponents.length > 0 || childModules.length > 0
                ? vscode.TreeItemCollapsibleState.Collapsed
                : vscode.TreeItemCollapsibleState.None
        );

        this.fpgaCells = fpgaCells;
        this.subComponents = subComponents;
        this.childModules = childModules;
        this.netlistPath = r.diagramJsonPath;

        const cells = r.statistics?.cellCount;
        const wires = r.statistics?.wireCount;

        if (r.success) {
            const parts: string[] = [];
            if (cells !== undefined) { parts.push(`${cells.toLocaleString()} cells`); }
            if (wires !== undefined) { parts.push(`${wires.toLocaleString()} wires`); }
            if (r.statistics?.logicDepth !== undefined) {
                parts.push(`depth ${r.statistics.logicDepth}`);
            }
            // Say so on the row itself: these numbers come from synthesizing
            // this component alone, which is not what the whole design would do.
            if (r.outOfContext) { parts.push('out of context'); }
            this.description = parts.join(' · ') || 'OK';
            this.iconPath = new vscode.ThemeIcon(
                'pass',
                new vscode.ThemeColor('testing.iconPassed')
            );
            const depthStr = r.statistics?.logicDepth !== undefined
                ? `  ·  Depth: ${r.statistics.logicDepth}`
                : '';
            this.tooltip = new vscode.MarkdownString(
                `**${r.name}**\n\n` +
                `Cells: ${cells ?? '—'}  ·  Wires: ${wires ?? '—'}${depthStr}  ·  ${r.elapsedMs} ms` +
                (r.outOfContext ? `\n\n${OUT_OF_CONTEXT_NOTE}` : '')
            );
        } else {
            this.description = r.errors[0]?.message ?? 'failed';
            this.iconPath = new vscode.ThemeIcon(
                'error',
                new vscode.ThemeColor('testing.iconFailed')
            );
            this.tooltip = new vscode.MarkdownString(
                `**${r.name}** — failed\n\n` +
                r.errors.map(e => `- ${e.message}`).join('\n')
            );
        }

        // contextValue drives inline button visibility in package.json.
        // Tags are appended so menu "when" clauses can use regex matching.
        let ctx = 'synthesisModule';
        if (r.svgPath) { ctx += '-diagram'; }
        if (r.verilogFiles?.length) { ctx += '-verilog'; }
        this.contextValue = ctx;

        // Store the full result so the viewModuleDiagram command can use it.
        this.result = r;
    }

    // Attached by the constructor for modules built from a result.
    result?: ModuleSynthesisResult;
}

// ── Sub-component items ──────────────────────────────────────────────────────

/**
 * A module instantiated by the module above it in the tree.
 *
 * Clicking one renders (on first open) and shows that component's own diagram,
 * which is how a hierarchical design is inspected level by level: the parent's
 * diagram draws sub-components as boxes, and these rows go inside those boxes.
 *
 * Nesting is resolved lazily by the provider, so a deep hierarchy costs nothing
 * until it is expanded.
 */
export class SubComponentItem extends vscode.TreeItem {
    constructor(
        readonly netlistPath: string,
        readonly moduleName: string,
        readonly subComponents: string[],
    ) {
        super(
            moduleName,
            subComponents.length > 0
                ? vscode.TreeItemCollapsibleState.Collapsed
                : vscode.TreeItemCollapsibleState.None
        );
        this.iconPath = new vscode.ThemeIcon('symbol-module');
        this.description = 'component';
        this.contextValue = 'subComponent-diagram';
        this.tooltip = new vscode.MarkdownString(
            `**${moduleName}**\n\nOpen this component's diagram` +
            (subComponents.length > 0
                ? `\n\nInstantiates: ${subComponents.join(', ')}`
                : '')
        );
        this.command = {
            command: 'clash-toolkit.viewComponentDiagram',
            title: 'View Component Diagram',
            arguments: [this],
        };
    }
}

/** Build the sub-component rows for a module, resolving one level of nesting. */
export async function subComponentItems(
    netlistPath: string | undefined,
    subComponents: string[],
): Promise<SubComponentItem[]> {
    if (!netlistPath) { return []; }
    const items: SubComponentItem[] = [];
    for (const name of subComponents) {
        items.push(new SubComponentItem(
            netlistPath,
            name,
            await subComponentsOf(netlistPath, name),
        ));
    }
    return items;
}

// ── Utilization child items ──────────────────────────────────────────────────

class UtilizationEntry extends vscode.TreeItem {
    constructor(cellType: string, count: number) {
        super(cellType, vscode.TreeItemCollapsibleState.None);
        this.description = count.toLocaleString();
        this.iconPath = new vscode.ThemeIcon('symbol-constant');
        this.contextValue = 'utilizationEntry';
    }
}

// ── PNR section framework ────────────────────────────────────────────────────

/** Expandable section header with a fixed list of children. */
class SectionItem extends vscode.TreeItem {
    constructor(
        label: string,
        icon: string,
        readonly children: SynthTreeNode[]
    ) {
        super(label, vscode.TreeItemCollapsibleState.Expanded);
        this.iconPath = new vscode.ThemeIcon(icon);
        this.contextValue = 'pnrSection';
    }
}

/** Generic label/value row used inside PNR sections. */
class KeyValueItem extends vscode.TreeItem {
    constructor(label: string, value: string, icon = 'symbol-field', tooltip?: string) {
        super(label, vscode.TreeItemCollapsibleState.None);
        this.description = value;
        this.iconPath = new vscode.ThemeIcon(icon);
        this.contextValue = 'pnrRow';
        if (tooltip) { this.tooltip = tooltip; }
    }
}

/** Critical-path row — expandable into the step chain. */
class CriticalPathItem extends vscode.TreeItem {
    readonly stepItems: KeyValueItem[];

    constructor(path: CriticalPath) {
        const label = truncate(path.from, 24) + ' → ' + truncate(path.to, 24);
        super(label, vscode.TreeItemCollapsibleState.Collapsed);

        this.description = `${path.totalDelay.toFixed(2)} ns`;
        this.iconPath = new vscode.ThemeIcon('arrow-right');
        this.contextValue = 'pnrCriticalPath';
        this.tooltip = new vscode.MarkdownString(
            `**${path.from}** → **${path.to}**\n\n` +
            `Total delay: \`${path.totalDelay.toFixed(3)} ns\`  ·  ${path.steps.length} step(s)`
        );

        this.stepItems = path.steps.map((step, i) => {
            const fromLabel = step.fromCell ? step.fromCell : '';
            const toLabel = step.toCell ? step.toCell : '';
            const arrow = fromLabel && toLabel && fromLabel !== toLabel
                ? `${truncate(fromLabel, 18)} → ${truncate(toLabel, 18)}`
                : fromLabel || toLabel || step.type;
            const kv = new KeyValueItem(
                `${i + 1}. ${step.type}`,
                `${step.delay.toFixed(3)} ns  ${arrow}`,
                iconForStep(step.type),
                step.net ? `net: ${step.net}` : undefined,
            );
            return kv;
        });
    }
}

function iconForStep(type: string): string {
    switch (type) {
        case 'source':    return 'debug-start';
        case 'routing':   return 'circuit-board';
        case 'clk-to-q':  return 'watch';
        case 'setup':     return 'debug-stop';
        default:          return 'symbol-field';
    }
}

function truncate(s: string, max: number): string {
    if (s.length <= max) { return s; }
    return s.slice(0, max - 1) + '…';
}

// ── Section builders ─────────────────────────────────────────────────────────

function buildTimingSection(pnr: NextpnrResult): SectionItem | undefined {
    const t = pnr.timing;
    if (!t) { return undefined; }

    const rows: KeyValueItem[] = [];

    if (t.maxFrequency !== undefined) {
        rows.push(new KeyValueItem('Max Frequency', `${t.maxFrequency.toFixed(2)} MHz`, 'pulse'));
    }
    if (t.prePlacementFrequency !== undefined && t.prePlacementFrequency !== t.maxFrequency) {
        rows.push(new KeyValueItem(
            'Pre-Route Estimate',
            `${t.prePlacementFrequency.toFixed(2)} MHz`,
            'dashboard',
        ));
    }
    if (t.criticalPathDelay !== undefined) {
        rows.push(new KeyValueItem(
            'Critical Path Delay',
            `${t.criticalPathDelay.toFixed(2)} ns`,
            'clock',
        ));
    }
    if (t.setupSlack !== undefined) {
        rows.push(new KeyValueItem(
            'Setup Slack',
            `${t.setupSlack.toFixed(3)} ns`,
            t.setupSlack >= 0 ? 'pass' : 'error',
        ));
    }
    if (t.holdSlack !== undefined) {
        rows.push(new KeyValueItem(
            'Hold Slack',
            `${t.holdSlack.toFixed(3)} ns`,
            t.holdSlack >= 0 ? 'pass' : 'error',
        ));
    }

    rows.push(new KeyValueItem(
        'Constraints',
        t.constraintsMet ? 'MET' : 'MISSED',
        t.constraintsMet ? 'pass' : 'error',
    ));

    if (rows.length === 0) { return undefined; }
    return new SectionItem('Timing', 'watch', rows);
}

function buildUtilizationSection(pnr: NextpnrResult): SectionItem | undefined {
    const u = pnr.utilization;
    if (!u) { return undefined; }

    const rows: KeyValueItem[] = [];
    const addRow = (label: string, bucket?: { used: number; total: number }, icon = 'symbol-field') => {
        if (!bucket || bucket.total === 0) { return; }
        const pct = (bucket.used / bucket.total) * 100;
        rows.push(new KeyValueItem(
            label,
            `${bucket.used.toLocaleString()} / ${bucket.total.toLocaleString()} (${pct.toFixed(1)}%)`,
            icon,
        ));
    };

    addRow('LUTs',       u.luts,      'circuit-board');
    addRow('Registers',  u.registers, 'symbol-variable');
    addRow('BRAM',       u.bram,      'database');
    addRow('DSP',        u.dsp,       'symbol-operator');
    addRow('IO',         u.io,        'plug');

    if (rows.length === 0) { return undefined; }
    return new SectionItem('Utilization', 'graph', rows);
}

function buildCriticalPathSection(paths?: CriticalPath[]): SectionItem | undefined {
    if (!paths || paths.length === 0) { return undefined; }
    // Cap at 5 so the tree doesn't explode for large designs with many
    // cross-domain paths; users can still open report.json for the full list.
    const items = paths.slice(0, 5).map(p => new CriticalPathItem(p));
    return new SectionItem('Critical Paths', 'arrow-right', items);
}
