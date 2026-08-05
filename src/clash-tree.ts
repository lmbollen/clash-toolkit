import * as vscode from 'vscode';
import { HaskellFunctionsTreeProvider } from './haskell-functions-tree';
import { SynthesisResultsTreeProvider } from './synthesis-results-tree';
import { RunHistoryTreeProvider } from './run-history-tree';

/** The three parts of the workflow, each a collapsible section of one view. */
export type ClashSection = 'functions' | 'results' | 'history';

/**
 * Marks every node with the section it came from.
 *
 * The composite provider owns no data of its own: each section's rows are built
 * by the provider that already knew how, and a later `getChildren` for one of
 * those rows has to reach that same provider again. The rows are ordinary
 * `TreeItem`s with no back-reference, and some node classes (notably
 * `SubComponentItem`) are shared between Results and History, so a node's type
 * cannot say where it belongs. Stamping the section on the way out can.
 */
const SECTION = Symbol('clashSection');

/**
 * The parts of `TreeDataProvider` this file uses, with the element type erased.
 *
 * Written with method syntax on purpose: that makes the parameter types
 * bivariant, so each concrete provider — which is typed against its own node
 * union — satisfies this without a cast.
 */
interface SectionProvider {
    readonly onDidChangeTreeData?: vscode.Event<unknown>;
    getTreeItem(element: unknown): vscode.TreeItem | Thenable<vscode.TreeItem>;
    getChildren(element?: unknown): vscode.ProviderResult<unknown[]>;
}

/**
 * A top-level section header.
 *
 * Labels are upper-case, which is how VS Code styles its own section titles —
 * at a glance it separates a header from the rows under it, which are all
 * mixed-case identifiers and paths.
 */
class SectionNode extends vscode.TreeItem {
    constructor(
        readonly section: ClashSection,
        label: string,
        icon: string,
        collapsibleState: vscode.TreeItemCollapsibleState,
    ) {
        super(label.toUpperCase(), collapsibleState);
        this.iconPath = new vscode.ThemeIcon(icon);
        this.contextValue = `clashSection-${section}`;
        // An id makes VS Code remember whether the user collapsed this section;
        // without one, every refresh would reset it to the state above.
        this.id = `clash-section-${section}`;
    }
}

/**
 * The blank row that precedes a section.
 *
 * A tree has no separator API and no way to space rows apart, so the gap has to
 * be a row of its own: no label, no icon, nothing to expand. It is inert —
 * `getChildren` finds no section on it and returns nothing, and no menu matches
 * its `contextValue` — but it is still a row the user can land on, so it
 * identifies itself to screen readers as a separator rather than going past as
 * an unnamed item.
 */
class SpacerNode extends vscode.TreeItem {
    constructor(readonly before: ClashSection) {
        super('', vscode.TreeItemCollapsibleState.None);
        this.contextValue = 'clashSpacer';
        // Stable id for the same reason SectionNode has one: without it VS Code
        // treats each refresh's spacer as a new row.
        this.id = `clash-spacer-${before}`;
        this.accessibilityInformation = { label: 'separator', role: 'separator' };
    }
}

/**
 * The single "Clash Toolkit" sidebar view.
 *
 * Functions, Results and History used to be three separate views, each with its
 * own title bar and collapse state. They are now three sections of one tree:
 * this provider contributes the section headers and routes every other call
 * back to the provider that owns that section, which keeps the three of them
 * independent of each other and of this file.
 *
 * Each sub-provider still fires its own change event; those are forwarded as a
 * change to that section alone, so refreshing the run history does not re-read
 * the function list.
 */
export class ClashTreeProvider implements vscode.TreeDataProvider<unknown>, vscode.Disposable {
    private readonly _onDidChangeTreeData = new vscode.EventEmitter<unknown | undefined>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private readonly sections: SectionNode[] = [
        new SectionNode(
            'functions', 'Functions', 'symbol-function',
            vscode.TreeItemCollapsibleState.Expanded,
        ),
        new SectionNode(
            'results', 'Results', 'circuit-board',
            vscode.TreeItemCollapsibleState.Expanded,
        ),
        new SectionNode(
            'history', 'History', 'history',
            vscode.TreeItemCollapsibleState.Collapsed,
        ),
    ];

    /**
     * What the root actually contains: the sections, with a blank row before
     * each one after the first. `sections` stays the three real headers, so
     * status updates and event routing never have to step over the spacers.
     */
    private readonly rootNodes: readonly unknown[] = this.sections.flatMap(
        (section, i) => i === 0 ? [section] : [new SpacerNode(section.section), section],
    );

    private readonly disposables: vscode.Disposable[] = [];

    constructor(
        private readonly functions: HaskellFunctionsTreeProvider,
        private readonly results: SynthesisResultsTreeProvider,
        private readonly history: RunHistoryTreeProvider,
    ) {
        for (const section of this.sections) {
            const forwarded = this.providerFor(section.section)
                .onDidChangeTreeData?.(() => this._onDidChangeTreeData.fire(section));
            if (forwarded) { this.disposables.push(forwarded); }
        }
    }

    dispose(): void {
        this.disposables.forEach(d => d.dispose());
        this._onDidChangeTreeData.dispose();
    }

    async getTreeItem(element: unknown): Promise<vscode.TreeItem> {
        if (element instanceof SectionNode) { return element; }
        const section = sectionOf(element);
        return section
            ? this.providerFor(section).getTreeItem(element)
            : element as vscode.TreeItem;
    }

    async getChildren(element?: unknown): Promise<unknown[]> {
        if (!element) { return [...this.rootNodes]; }

        // A spacer carries no section, so it falls through the guard below and
        // expands to nothing — which is what makes it inert.
        const section = element instanceof SectionNode
            ? element.section
            : sectionOf(element);
        if (!section) { return []; }

        const children = await this.providerFor(section)
            .getChildren(element instanceof SectionNode ? undefined : element);
        return this.claim(section, children ?? []);
    }

    /**
     * Label a section with what it is currently showing — which run is in
     * Results, which file's functions are listed, why HLS has nothing to say.
     *
     * This is what the separate views' `TreeView.message` banners used to do.
     * One view has one banner and three sections have three things to say, so
     * the text moves onto the section header it belongs to.
     */
    setSectionStatus(section: ClashSection, description?: string, tooltip?: string): void {
        const node = this.sections.find(s => s.section === section);
        if (!node) { return; }
        node.description = description;
        node.tooltip = tooltip ? new vscode.MarkdownString(tooltip) : undefined;
        this._onDidChangeTreeData.fire(node);
    }

    /** The status text currently shown on a section, if any. */
    sectionStatus(section: ClashSection): string | undefined {
        const description = this.sections.find(s => s.section === section)?.description;
        return typeof description === 'string' ? description : undefined;
    }

    private providerFor(section: ClashSection): SectionProvider {
        switch (section) {
            case 'functions': return this.functions;
            case 'results': return this.results;
            case 'history': return this.history;
        }
    }

    private claim(section: ClashSection, items: readonly unknown[]): unknown[] {
        for (const item of items) {
            if (item && typeof item === 'object') {
                (item as Record<symbol, ClashSection>)[SECTION] = section;
            }
        }
        return [...items];
    }
}

function sectionOf(element: unknown): ClashSection | undefined {
    if (!element || typeof element !== 'object') { return undefined; }
    return (element as Record<symbol, ClashSection | undefined>)[SECTION];
}
