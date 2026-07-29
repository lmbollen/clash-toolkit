import * as vscode from 'vscode';
import { FunctionInfo } from './types';

export type FunctionTreeNode = SectionNode | FunctionNode;

/**
 * Provides the "Haskell Functions" sidebar view.
 *
 * Shows two collapsible sections — Synthesizable (monomorphic) on top,
 * Polymorphic (grayed) below — for the currently active Haskell file.
 */
export class HaskellFunctionsTreeProvider
    implements vscode.TreeDataProvider<FunctionTreeNode>
{
    private readonly _onDidChangeTreeData =
        new vscode.EventEmitter<FunctionTreeNode | undefined | null>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private mono: FunctionInfo[] = [];
    private poly: FunctionInfo[] = [];
    private fileName: string | undefined;
    /** What the view is currently reporting; drives the placeholder rows. */
    private status: 'idle' | 'analyzing' | 'no-symbols-yet' | 'hls-missing' | 'hls-inactive' | 'results' = 'idle';

    /** Called while our own analysis of an already-responsive HLS is running. */
    setLoading(fileName: string): void {
        this.fileName = fileName;
        this.status = 'analyzing';
        this.mono = [];
        this.poly = [];
        this._onDidChangeTreeData.fire(undefined);
    }

    /**
     * Called when HLS is installed and active but returned no symbols for this
     * file. That is not the same as "this file has no functions": HLS answers
     * with nothing while it is still loading the project, and from here the two
     * are indistinguishable — so the view reports the fact, not a verdict.
     *
     * The view leaves this state when HLS next reports on the file (see the
     * diagnostics-driven refresh in `extension.ts`) or when the user refreshes.
     */
    setNoSymbolsYet(fileName: string): void {
        this.fileName = fileName;
        this.status = 'no-symbols-yet';
        this.mono = [];
        this.poly = [];
        this._onDidChangeTreeData.fire(undefined);
    }

    /**
     * Whether the view is in one of the "HLS gave us nothing" states. The
     * diagnostics-driven auto-refresh only fires then, so ordinary editing does
     * not trigger a re-detect on every keystroke.
     */
    isAwaitingHls(): boolean {
        return this.status === 'no-symbols-yet'
            || this.status === 'hls-inactive'
            || this.status === 'hls-missing';
    }

    /**
     * Called when HLS cannot answer at all: the Haskell extension is missing, or
     * it is installed but could not be activated. Nothing will change on its own,
     * so the view offers the fix instead of a spinner.
     */
    setHlsUnavailable(reason: 'extension-missing' | 'extension-inactive', fileName?: string): void {
        this.fileName = fileName;
        this.status = reason === 'extension-missing' ? 'hls-missing' : 'hls-inactive';
        this.mono = [];
        this.poly = [];
        this._onDidChangeTreeData.fire(undefined);
    }

    /** Called with the final results once analysis completes. */
    refresh(functions: FunctionInfo[], fileName?: string): void {
        this.status = fileName ? 'results' : 'idle';
        this.fileName = fileName;
        this.mono = functions.filter(f => f.isMonomorphic)
            .sort((a, b) => a.name.localeCompare(b.name));
        this.poly = functions.filter(f => !f.isMonomorphic)
            .sort((a, b) => a.name.localeCompare(b.name));
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: FunctionTreeNode): vscode.TreeItem {
        return element;
    }

    getChildren(element?: FunctionTreeNode): FunctionTreeNode[] {
        if (element instanceof SectionNode) {
            return element.kind === 'mono'
                ? this.mono.map(f => new FunctionNode(f, false))
                : this.poly.map(f => new FunctionNode(f, true));
        }

        // Root level — one explicit row per state, so an empty list is never
        // ambiguous between "HLS is missing", "HLS is still working" and "this
        // file has no functions".
        switch (this.status) {
            case 'analyzing':
                return [statusRow('Analyzing…', 'loading~spin')];

            case 'no-symbols-yet':
                return [statusRow(
                    'No symbols from HLS yet',
                    'clock',
                    'it may still be loading the project',
                    'The Haskell Language Server returned nothing for this file. While it '
                    + 'loads a project — which can take minutes on a cold cache — it answers '
                    + 'with nothing, and that is indistinguishable from a file which really '
                    + 'defines no top-level functions.\n\nThis view re-checks when HLS next '
                    + 'reports on the file; the refresh button re-checks now.',
                )];

            case 'hls-missing':
                return [statusRow(
                    'HLS unavailable — Haskell extension not installed',
                    'error',
                    'click to install',
                    'Function detection needs the Haskell Language Server, which the '
                    + '`haskell.haskell` extension provides.\n\nClick this row to open it in '
                    + 'the Marketplace.',
                    {
                        command: 'clash-toolkit.installHaskellExtension',
                        title: 'Install the Haskell extension',
                    },
                )];

            case 'hls-inactive':
                return [statusRow(
                    'HLS unavailable — Haskell extension did not start',
                    'warning',
                    'click to retry',
                    'The Haskell extension is installed but did not activate, so no symbols '
                    + 'can be requested. Its own output channel usually says why.\n\nClick '
                    + 'this row to try again.',
                    {
                        command: 'clash-toolkit.refreshHaskellFunctions',
                        title: 'Retry',
                    },
                )];

            case 'idle':
                return [statusRow('Open a Haskell file to see functions', 'info')];

            case 'results':
                return [
                    new SectionNode('mono', this.mono.length),
                    new SectionNode('poly', this.poly.length),
                ];
        }
    }
}

/**
 * A single non-function row: state, not data.
 *
 * `vscode.TreeItem` isn't part of the node union, but `getTreeItem` returns
 * elements unchanged so a plain item works.
 */
function statusRow(
    label: string,
    icon: string,
    description?: string,
    tooltip?: string,
    command?: vscode.Command,
): FunctionTreeNode {
    const item = new vscode.TreeItem(label);
    item.iconPath = new vscode.ThemeIcon(icon);
    if (description) { item.description = description; }
    if (tooltip) { item.tooltip = new vscode.MarkdownString(tooltip); }
    if (command) { item.command = command; }
    item.contextValue = 'hlsStatus';
    return item as unknown as FunctionTreeNode;
}

// ── Section header nodes ─────────────────────────────────────────────────────

class SectionNode extends vscode.TreeItem {
    constructor(
        readonly kind: 'mono' | 'poly',
        count: number,
    ) {
        const label = kind === 'mono'
            ? `Monomorphic (${count})`
            : `Polymorphic (${count})`;
        super(label, vscode.TreeItemCollapsibleState.Expanded);
        this.contextValue = kind === 'mono' ? 'monoSection' : 'polySection';

        if (kind === 'mono') {
            this.iconPath = new vscode.ThemeIcon(
                'symbol-function',
                new vscode.ThemeColor('testing.iconPassed'),
            );
        } else {
            this.iconPath = new vscode.ThemeIcon(
                'symbol-function',
                new vscode.ThemeColor('disabledForeground'),
            );
        }
    }
}

// ── Function leaf nodes ──────────────────────────────────────────────────────

export class FunctionNode extends vscode.TreeItem {
    constructor(
        readonly info: FunctionInfo,
        readonly isPolymorphic: boolean,
    ) {
        super(info.name, vscode.TreeItemCollapsibleState.None);

        this.description = info.typeSignature
            ? `:: ${info.typeSignature}`
            : undefined;

        this.tooltip = new vscode.MarkdownString(
            `**${info.name}**` +
            (info.typeSignature ? `\n\n\`\`\`haskell\n:: ${info.typeSignature}\n\`\`\`` : '') +
            (isPolymorphic
                ? '\n\n*Polymorphic — cannot be synthesized directly.*'
                : '\n\n*Monomorphic — can be synthesized.*')
        );

        this.iconPath = isPolymorphic
            ? new vscode.ThemeIcon('symbol-variable', new vscode.ThemeColor('disabledForeground'))
            : new vscode.ThemeIcon('symbol-function', new vscode.ThemeColor('symbolIcon.functionForeground'));

        this.contextValue = isPolymorphic ? 'polyFunction' : 'monoFunction';

        // Navigate to the function on click
        this.command = {
            command: 'clash-toolkit.goToFunction',
            title: 'Go to function',
            arguments: [info],
        };
    }
}
