import * as vscode from 'vscode';
import * as path from 'path';
import { promises as fs } from 'fs';
import { ModuleSynthesisResult, SynthesisStatistics } from './yosys-types';
import { RunMetadata, readRunMeta, loadRunModules } from './run-loader';
import { subComponentsOf } from './netlist-diagram';
import {
    OUT_OF_CONTEXT_NOTE,
    SubComponentItem,
    subComponentItems,
} from './synthesis-results-tree';

// ── Node types ──────────────────────────────────────────────────────────────

type HistoryTreeNode =
    | FunctionGroupNode
    | RunNode
    | RunModuleNode
    | SubComponentItem
    | RunInfoItem;

/** Top-level: groups runs by qualified function name. */
class FunctionGroupNode extends vscode.TreeItem {
    constructor(
        readonly qualifiedName: string,
        readonly runsDir: string,
        readonly runIds: string[],
    ) {
        super(qualifiedName, vscode.TreeItemCollapsibleState.Expanded);
        this.iconPath = new vscode.ThemeIcon('symbol-function');
        this.contextValue = 'historyFunction';
        this.description = `${runIds.length} run${runIds.length === 1 ? '' : 's'}`;
    }
}

/** A single timestamped run. */
export class RunNode extends vscode.TreeItem {
    constructor(
        readonly runId: string,
        readonly runRoot: string,
        readonly qualifiedName: string,
        readonly meta: RunMetadata | undefined,
    ) {
        super(runId, vscode.TreeItemCollapsibleState.Collapsed);
        this.contextValue = 'historyRun';

        // Selecting a run loads its data into the Synthesis Results view.
        this.command = {
            command: 'clash-toolkit.selectRun',
            title: 'Show in Synthesis Results',
            arguments: [this],
        };

        if (meta) {
            const icon = meta.success === false ? 'error' : 'pass';
            const color = meta.success === false
                ? new vscode.ThemeColor('testing.iconFailed')
                : new vscode.ThemeColor('testing.iconPassed');
            this.iconPath = new vscode.ThemeIcon(icon, color);

            const parts: string[] = [meta.command];
            if (meta.target && meta.target !== 'generic') { parts.push(meta.target); }
            if (meta.cellCount !== undefined) { parts.push(`${meta.cellCount} cells`); }
            if (meta.maxFrequencyMHz !== undefined) { parts.push(`${meta.maxFrequencyMHz.toFixed(1)} MHz`); }
            this.description = parts.join(' · ');

            const ts = meta.timestamp ? new Date(meta.timestamp).toLocaleString() : runId;
            this.tooltip = new vscode.MarkdownString(
                `**${meta.command}** — ${meta.function}\n\n` +
                `Time: ${ts}\n\n` +
                (meta.target ? `Target: ${meta.target}\n\n` : '') +
                (meta.cellCount !== undefined ? `Cells: ${meta.cellCount}\n\n` : '') +
                (meta.maxFrequencyMHz !== undefined ? `Fmax: ${meta.maxFrequencyMHz.toFixed(2)} MHz\n\n` : '')
            );
        } else {
            this.iconPath = new vscode.ThemeIcon('question');
            this.description = 'no metadata';
        }
    }
}

/** A module within a run (from per-module Yosys results). */
export class RunModuleNode extends vscode.TreeItem {
    constructor(
        readonly moduleName: string,
        readonly runRoot: string,
        readonly svgPath: string | undefined,
        readonly verilogFiles: string[],
        readonly statistics: SynthesisStatistics | undefined,
        /** Netlist the module's sub-component diagrams are rendered from. */
        readonly netlistPath?: string,
        /** Modules this one instantiates — expandable when non-empty. */
        readonly subComponents: string[] = [],
        /** Components synthesized as runs of their own, nested under this one. */
        readonly childModules: RunModuleNode[] = [],
        /** True when this component was synthesized out of context. */
        readonly outOfContext = false,
    ) {
        super(
            moduleName,
            subComponents.length > 0 || childModules.length > 0
                ? vscode.TreeItemCollapsibleState.Collapsed
                : vscode.TreeItemCollapsibleState.None
        );

        const parts: string[] = [];
        if (statistics?.cellCount !== undefined) { parts.push(`${statistics.cellCount} cells`); }
        if (statistics?.wireCount !== undefined) { parts.push(`${statistics.wireCount} wires`); }
        if (statistics?.logicDepth !== undefined) { parts.push(`depth ${statistics.logicDepth}`); }
        if (outOfContext) { parts.push('out of context'); }
        this.description = parts.join(' · ') || '';

        this.iconPath = new vscode.ThemeIcon('symbol-module');
        if (outOfContext) {
            this.tooltip = new vscode.MarkdownString(
                `**${moduleName}**\n\n${OUT_OF_CONTEXT_NOTE}`
            );
        }

        let ctx = 'historyModule';
        if (svgPath) { ctx += '-diagram'; }
        if (verilogFiles.length > 0) { ctx += '-verilog'; }
        this.contextValue = ctx;
    }
}

/** Simple key/value info row inside a run node (for runs without per-module data). */
class RunInfoItem extends vscode.TreeItem {
    constructor(label: string, value: string, icon = 'symbol-field') {
        super(label, vscode.TreeItemCollapsibleState.None);
        this.description = value;
        this.iconPath = new vscode.ThemeIcon(icon);
        this.contextValue = 'historyInfo';
    }
}

// ── Tree provider ───────────────────────────────────────────────────────────

/**
 * TreeDataProvider for the Run History sidebar view.
 *
 * Scans `<workspace>/.clash/` for function directories, each containing a
 * `runs/` folder with timestamped subdirectories. Each run may contain a
 * `run.json` metadata file and per-module Yosys stats.
 */
export class RunHistoryTreeProvider
    implements vscode.TreeDataProvider<HistoryTreeNode>
{
    private readonly _onDidChangeTreeData =
        new vscode.EventEmitter<HistoryTreeNode | undefined | null>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private workspaceRoot: string | undefined;

    setWorkspaceRoot(root: string | undefined): void {
        this.workspaceRoot = root;
    }

    refresh(): void {
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: HistoryTreeNode): vscode.TreeItem {
        return element;
    }

    async getChildren(element?: HistoryTreeNode): Promise<HistoryTreeNode[]> {
        if (!this.workspaceRoot) {
            return [placeholder('No workspace open')];
        }

        // Root level — list function groups
        if (!element) {
            return this.getFunctionGroups();
        }

        // Function group → list runs
        if (element instanceof FunctionGroupNode) {
            return this.getRunNodes(element);
        }

        // Run → list modules (or info items)
        if (element instanceof RunNode) {
            return this.getRunChildren(element);
        }

        // Module / sub-component → list the components it instantiates
        if (element instanceof RunModuleNode) {
            return [
                ...element.childModules,
                ...await subComponentItems(element.netlistPath, element.subComponents),
            ];
        }
        if (element instanceof SubComponentItem) {
            return subComponentItems(element.netlistPath, element.subComponents);
        }

        return [];
    }

    // ── Helpers ─────────────────────────────────────────────────────────────

    private async getFunctionGroups(): Promise<HistoryTreeNode[]> {
        const clashDir = path.join(this.workspaceRoot!, '.clash');
        let entries: string[];
        try {
            const dirents = await fs.readdir(clashDir, { withFileTypes: true });
            entries = dirents
                .filter(d => d.isDirectory() && d.name !== 'synth-project')
                .map(d => d.name);
        } catch {
            return [placeholder('No run history yet')];
        }

        const groups: FunctionGroupNode[] = [];
        for (const name of entries.sort()) {
            const runsDir = path.join(clashDir, name, 'runs');
            try {
                const runDirents = await fs.readdir(runsDir, { withFileTypes: true });
                const runIds = runDirents
                    .filter(d => d.isDirectory())
                    .map(d => d.name)
                    .sort()
                    .reverse();
                if (runIds.length > 0) {
                    groups.push(new FunctionGroupNode(name, runsDir, runIds));
                }
            } catch {
                // No runs dir — skip
            }
        }

        if (groups.length === 0) {
            return [placeholder('No run history yet')];
        }
        return groups;
    }

    private async getRunNodes(group: FunctionGroupNode): Promise<RunNode[]> {
        const nodes: RunNode[] = [];
        for (const runId of group.runIds) {
            const runRoot = path.join(group.runsDir, runId);
            const meta = await readRunMeta(runRoot);
            nodes.push(new RunNode(runId, runRoot, group.qualifiedName, meta));
        }
        return nodes;
    }

    private async getRunChildren(run: RunNode): Promise<HistoryTreeNode[]> {
        const { modules } = await loadRunModules(run.runRoot, run.meta);
        if (modules.length > 0) {
            // Nest per-module runs by their recorded component graph, so a run
            // reads the same way whether or not it was synthesized per module.
            const byName = new Map(modules.map(m => [m.name, m]));
            const claimed = new Set(
                modules.flatMap(m => (m.subComponents ?? []).filter(n => byName.has(n)))
            );
            const nodes: RunModuleNode[] = [];
            for (const m of modules.filter(m => !claimed.has(m.name))) {
                nodes.push(await this.buildModuleNode(run, m, byName, new Set()));
            }
            return nodes;
        }

        // Fallback: show metadata as info items
        if (run.meta) {
            return metaToInfoItems(run.meta);
        }

        return [new RunInfoItem('No details available', '', 'info')];
    }

    /**
     * Build a module row for a past run, nesting the components it instantiates.
     * `ancestors` stops a self-referential component graph from recursing
     * forever.
     */
    private async buildModuleNode(
        run: RunNode,
        module: ModuleSynthesisResult,
        byName: Map<string, ModuleSynthesisResult>,
        ancestors: Set<string>,
    ): Promise<RunModuleNode> {
        const nextAncestors = new Set(ancestors).add(module.name);
        const children: RunModuleNode[] = [];
        for (const name of module.subComponents ?? []) {
            const child = byName.get(name);
            if (!child || nextAncestors.has(name)) { continue; }
            children.push(await this.buildModuleNode(run, child, byName, nextAncestors));
        }

        // Only ask the netlist when there are no nested results — see the same
        // reasoning in SynthesisResultsTreeProvider.buildModuleItem.
        const fromNetlist = children.length === 0 && module.diagramJsonPath
            ? await subComponentsOf(module.diagramJsonPath, module.name)
            : [];

        return new RunModuleNode(
            module.name,
            run.runRoot,
            module.svgPath,
            module.verilogFiles ?? [],
            module.statistics,
            module.diagramJsonPath,
            fromNetlist,
            children,
            module.outOfContext ?? false,
        );
    }
}

// ── Utilities ───────────────────────────────────────────────────────────────

function placeholder(text: string): RunInfoItem {
    const item = new RunInfoItem(text, '', 'info');
    return item;
}

function metaToInfoItems(meta: RunMetadata): RunInfoItem[] {
    const items: RunInfoItem[] = [];
    items.push(new RunInfoItem('Command', meta.command, 'terminal'));
    if (meta.target) { items.push(new RunInfoItem('Target', meta.target, 'circuit-board')); }
    if (meta.cellCount !== undefined) { items.push(new RunInfoItem('Cells', String(meta.cellCount), 'symbol-constant')); }
    if (meta.wireCount !== undefined) { items.push(new RunInfoItem('Wires', String(meta.wireCount), 'symbol-constant')); }
    if (meta.maxFrequencyMHz !== undefined) {
        items.push(new RunInfoItem('Max Frequency', `${meta.maxFrequencyMHz.toFixed(2)} MHz`, 'pulse'));
    }
    if (meta.device) { items.push(new RunInfoItem('Device', meta.deviceLabel ?? meta.device, 'cpu')); }
    const success = meta.success === true ? 'Yes' : meta.success === false ? 'No' : 'Unknown';
    items.push(new RunInfoItem('Success', success, meta.success === false ? 'error' : 'pass'));
    return items;
}
