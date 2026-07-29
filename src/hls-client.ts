import * as vscode from 'vscode';
import * as path from 'path';

/** Marketplace id of the extension that provides HLS integration. */
export const HASKELL_EXTENSION_ID = 'haskell.haskell';

/**
 * Availability of HLS as seen through the Haskell extension.
 *
 * Note we cannot probe HLS directly: `executeDocumentSymbolProvider`
 * returns the same empty result whether no provider is registered or the
 * provider found nothing, so the Haskell extension's install/activation
 * state is the only reliable signal.
 */
export interface HLSAvailability {
    available: boolean;
    reason?: 'extension-missing' | 'extension-inactive';
    /** Human-readable, actionable explanation when unavailable. */
    message?: string;
}

/**
 * Manager for Haskell Language Server client
 *
 * This class provides access to HLS functionality without directly
 * managing the language client (which is typically handled by the
 * Haskell extension). We use VS Code's built-in language features
 * that communicate with HLS.
 */
export class HLSClient {
    private outputChannel: vscode.OutputChannel;

    /** Cache of document symbols keyed by URI + document version. */
    private symbolCache = new Map<string, { version: number; symbols: vscode.DocumentSymbol[] }>();

    constructor(outputChannel: vscode.OutputChannel) {
        this.outputChannel = outputChannel;
    }

    /**
     * Check whether HLS is available via the Haskell extension.
     *
     * `available: false` with reason 'extension-missing' means the Haskell
     * extension is not installed at all; 'extension-inactive' means it is
     * installed but has not (yet) activated. A user running HLS through a
     * custom LSP client is not detected here — callers should therefore only
     * consult this check to explain an empty symbol result, not to gate
     * requests.
     */
    checkAvailability(): HLSAvailability {
        const ext = vscode.extensions.getExtension(HASKELL_EXTENSION_ID);
        if (!ext) {
            return {
                available: false,
                reason: 'extension-missing',
                message:
                    `The Haskell extension (${HASKELL_EXTENSION_ID}) is not installed, ` +
                    'so HLS is unavailable and functions cannot be detected.',
            };
        }
        if (!ext.isActive) {
            return {
                available: false,
                reason: 'extension-inactive',
                message:
                    'The Haskell extension is installed but not active yet. ' +
                    'Open a Haskell file, give HLS a moment to start, then try again.',
            };
        }
        return { available: true };
    }

    /**
     * Start the Haskell extension if it is installed but hasn't activated.
     *
     * Its only activation events are `onLanguage:haskell`/`cabal`, so a window
     * restored without a Haskell file open leaves HLS unstarted — and the user
     * sees an empty function list with no explanation. Activating it ourselves
     * removes that state instead of reporting it.
     *
     * @returns the availability after the attempt.
     */
    async ensureActivated(): Promise<HLSAvailability> {
        const ext = vscode.extensions.getExtension(HASKELL_EXTENSION_ID);
        if (!ext) { return this.checkAvailability(); }
        if (ext.isActive) { return { available: true }; }
        try {
            this.outputChannel.appendLine('[HLS] Activating the Haskell extension…');
            await ext.activate();
        } catch (err) {
            this.outputChannel.appendLine(`[HLS] Activation failed: ${err}`);
        }
        return this.checkAvailability();
    }

    /** Forget the cached symbols for a document. */
    invalidate(document: vscode.TextDocument): void {
        this.symbolCache.delete(document.uri.toString());
    }

    /**
     * Get all symbols in a document, caching by document version so repeated
     * calls (e.g. from code-action providers on every cursor move) don't
     * hammer HLS with redundant requests.
     */
    async getDocumentSymbols(document: vscode.TextDocument): Promise<vscode.DocumentSymbol[]> {
        const cacheKey = document.uri.toString();
        const cached = this.symbolCache.get(cacheKey);
        if (cached && cached.version === document.version) {
            return cached.symbols;
        }

        try {
            const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
                'vscode.executeDocumentSymbolProvider',
                document.uri
            );
            const result = symbols || [];
            this.symbolCache.set(cacheKey, { version: document.version, symbols: result });
            return result;
        } catch (error) {
            this.outputChannel.appendLine(`[HLS] Error getting document symbols: ${error}`);
            return [];
        }
    }

    /**
     * Like getDocumentSymbols, but bypasses the cache and logs diagnostic
     * output.  Use this for explicit user-triggered detection commands.
     */
    async getDocumentSymbolsVerbose(document: vscode.TextDocument): Promise<vscode.DocumentSymbol[]> {
        this.outputChannel.appendLine(`[HLS] Requesting symbols for ${document.fileName}...`);
        try {
            const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
                'vscode.executeDocumentSymbolProvider',
                document.uri
            );
            const result = symbols || [];
            this.outputChannel.appendLine(`[HLS] Received ${result.length} symbols`);
            if (result.length === 0) {
                this.outputChannel.appendLine('[HLS] WARNING: No symbols returned. HLS may not be ready or file not indexed.');
                this.outputChannel.appendLine(`[HLS] Document language: ${document.languageId}`);
                this.outputChannel.appendLine(`[HLS] Document URI: ${document.uri.toString()}`);
            }
            // Warm the cache while we're here.
            this.symbolCache.set(document.uri.toString(), { version: document.version, symbols: result });
            return result;
        } catch (error) {
            this.outputChannel.appendLine(`[HLS] Error getting document symbols: ${error}`);
            return [];
        }
    }

    /**
     * Get hover information for a position (includes type signature)
     */
    async getHoverInfo(document: vscode.TextDocument, position: vscode.Position): Promise<vscode.Hover[]> {
        try {
            const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
                'vscode.executeHoverProvider',
                document.uri,
                position
            );
            
            return hovers || [];
        } catch (error) {
            this.outputChannel.appendLine(`Error getting hover info: ${error}`);
            return [];
        }
    }

    /**
     * Extract type signature from hover information
     */
    extractTypeSignature(hovers: vscode.Hover[]): string | null {
        for (const hover of hovers) {
            for (const content of hover.contents) {
                if (typeof content === 'string') {
                    const typeMatch = this.parseTypeFromString(content);
                    if (typeMatch) {
                        return typeMatch;
                    }
                } else if ('value' in content) {
                    // MarkdownString
                    const typeMatch = this.parseTypeFromString(content.value);
                    if (typeMatch) {
                        return typeMatch;
                    }
                }
            }
        }
        return null;
    }

    /**
     * Parse type signature from hover text.
     *
     * HLS returns hover info as markdown with fenced code blocks.  Type
     * signatures may span multiple lines, e.g.:
     *
     * ```haskell
     * functionName
     *   :: Clock Dom50
     *   -> Reset Dom50
     *   -> Signal Dom50 (Unsigned 8)
     * ```
     *
     * We collapse multi-line signatures into a single line so callers
     * always see the full type, not just the first argument.
     */
    private parseTypeFromString(text: string): string | null {
        // Extract content from a haskell code fence if present
        const fencePattern = /```haskell\s*\n([\s\S]*?)```/;
        const fenced = text.match(fencePattern);
        const block = fenced ? fenced[1] : text;

        // Look for name :: type (possibly across multiple lines)
        const typePattern = /^([a-zA-Z_][a-zA-Z0-9_']*)\s*::\s*([\s\S]+)/m;
        const match = block.match(typePattern);

        if (match && match[2]) {
            // Collapse newlines + leading whitespace into single spaces
            return match[2].replace(/\n\s*/g, ' ').trim();
        }

        // Fallback: look for :: anywhere
        const simplePattern = /::\s*([\s\S]+?)(?:```|$)/;
        const simpleMatch = text.match(simplePattern);

        if (simpleMatch && simpleMatch[1]) {
            return simpleMatch[1].replace(/\n\s*/g, ' ').trim();
        }

        return null;
    }

    /**
     * Check if a document is a Haskell file
     */
    isHaskellDocument(document: vscode.TextDocument): boolean {
        return document.languageId === 'haskell' || 
               document.fileName.endsWith('.hs');
    }

    /**
     * Get the module name from a document
     */
    async getModuleName(document: vscode.TextDocument): Promise<string | null> {
        const text = document.getText();
        const modulePattern = /^\s*module\s+([A-Z][A-Za-z0-9._]*)/m;
        const match = text.match(modulePattern);
        
        if (match && match[1]) {
            return match[1];
        }
        
        // Fallback: derive from file path
        // e.g., src/Example/Project.hs -> Example.Project
        const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
        if (workspaceFolder) {
            const relativePath = path.relative(workspaceFolder.uri.fsPath, document.fileName);
            const srcMatch = relativePath.match(/src[\/\\](.+)\.hs$/);
            if (srcMatch && srcMatch[1]) {
                return srcMatch[1].replace(/[\/\\]/g, '.');
            }
        }
        
        return null;
    }

    /**
     * Log to output channel
     */
    log(message: string): void {
        this.outputChannel.appendLine(message);
    }
}
