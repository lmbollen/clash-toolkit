import * as vscode from 'vscode';
import * as path from 'path';
import { promises as fs } from 'fs';
import { getLogger } from './file-logger';

/**
 * Offer to add the extension's working directory to the workspace's
 * `.gitignore`.
 *
 * Every run writes generated Verilog, netlists, diagrams and run history under
 * `.clash/`, which is build output rather than source — but it lands inside the
 * user's repository, so it is theirs to decide about. The offer is made once,
 * the answer is remembered per workspace, and "not right now" deliberately
 * remembers nothing so the question comes back.
 */

/** Where the answer is kept. Per workspace: it is about *this* repository. */
export const GITIGNORE_DECISION_KEY = 'clash-toolkit.gitignoreDecision';

/** A recorded answer. "Not right now" records nothing, and so is absent here. */
export type GitignoreDecision = 'added' | 'declined';

/** What an offer did — returned for logging and tests. */
export type GitignoreOutcome = GitignoreDecision | 'postponed' | 'skipped';

/** The entry written, and the comment that says what it is. */
export const CLASH_IGNORE_ENTRY = '.clash/';
export const CLASH_IGNORE_COMMENT =
    '# Clash Toolkit: generated Verilog, netlists, diagrams and run history';

/** The slice of `ExtensionContext.workspaceState` this needs. */
export interface DecisionStore {
    get(key: string): GitignoreDecision | undefined;
    update(key: string, value: GitignoreDecision | undefined): Thenable<void>;
}

/**
 * Whether `.gitignore` already has something to say about `.clash`.
 *
 * A negation (`!.clash`) counts: the file has an opinion about this path, and
 * appending an ignore rule underneath would quietly reverse it.
 */
export function mentionsClashDir(gitignore: string): boolean {
    return gitignore.split(/\r?\n/).some(line => {
        const pattern = line.trim();
        if (pattern === '' || pattern.startsWith('#')) { return false; }
        const normalized = pattern
            .replace(/^!/, '')
            .replace(/^\*\*\//, '')
            .replace(/^\//, '')
            .replace(/\/+$/, '');
        return normalized === '.clash';
    });
}

/**
 * The contents `.gitignore` should have once `.clash/` is ignored.
 *
 * Appended with a blank line above it and a comment, so the entry reads as
 * something an editor added on purpose rather than an unexplained path.
 */
export function withClashIgnored(gitignore: string): string {
    const body = gitignore.replace(/\s+$/, '');
    const prefix = body.length > 0 ? `${body}\n\n` : '';
    return `${prefix}${CLASH_IGNORE_COMMENT}\n${CLASH_IGNORE_ENTRY}\n`;
}

/**
 * Ask about `.gitignore`, unless there is nothing to ask.
 *
 * Silently does nothing when the workspace has no `.gitignore` (the extension
 * does not create one — a repository without one has made its own choice),
 * when `.clash` is already mentioned, or when this workspace has answered
 * before.
 */
export async function offerGitignoreEntry(
    store: DecisionStore,
    workspaceRoot: string,
    outputChannel: vscode.OutputChannel,
): Promise<GitignoreOutcome> {
    if (store.get(GITIGNORE_DECISION_KEY)) { return 'skipped'; }

    const gitignorePath = path.join(workspaceRoot, '.gitignore');
    let contents: string;
    try {
        contents = await fs.readFile(gitignorePath, 'utf8');
    } catch {
        // No .gitignore here, or it cannot be read — nothing to add to.
        return 'skipped';
    }
    if (mentionsClashDir(contents)) { return 'skipped'; }

    const yes = 'Yes';
    const no = 'No';
    const later = 'Not right now';
    const choice = await vscode.window.showInformationMessage(
        'Clash Toolkit writes generated Verilog, netlists and run history to '
        + `${CLASH_IGNORE_ENTRY} in this workspace. Add it to .gitignore?`,
        yes, no, later,
    );

    // Dismissing the notification is not an answer, so it is treated as "not
    // right now" and asked again next session.
    if (choice === undefined || choice === later) { return 'postponed'; }

    if (choice === no) {
        await store.update(GITIGNORE_DECISION_KEY, 'declined');
        outputChannel.appendLine(`Leaving ${CLASH_IGNORE_ENTRY} out of .gitignore (won't ask again).`);
        return 'declined';
    }

    try {
        await fs.writeFile(gitignorePath, withClashIgnored(contents), 'utf8');
    } catch (err) {
        // Nothing was written, so nothing is recorded either — the offer stands
        // and the next session can try again.
        const detail = err instanceof Error ? err.message : String(err);
        outputChannel.appendLine(`Could not update .gitignore: ${detail}`);
        getLogger()?.error(`Failed to update .gitignore: ${detail}`);
        vscode.window.showErrorMessage(`Could not update .gitignore: ${detail}`);
        return 'postponed';
    }

    await store.update(GITIGNORE_DECISION_KEY, 'added');
    outputChannel.appendLine(`Added ${CLASH_IGNORE_ENTRY} to .gitignore.`);
    return 'added';
}
