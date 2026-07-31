import * as vscode from 'vscode';
import * as path from 'path';
import { promises as fs, constants as fsConstants } from 'fs';
import { spawn } from 'child_process';
import { getLogger } from './file-logger';
import { getToolProvider, resolveTool, toolSpawnEnv } from './tool-provider';

/**
 * Walk PATH to resolve the absolute location of a command.
 * Cross-platform: honours PATHEXT on Windows. Returns undefined if not found.
 */
async function resolveCommandPath(cmd: string): Promise<string | undefined> {
    // An already-resolved absolute/relative path (e.g. a managed binary) needs
    // no PATH walk — report it directly if it's executable.
    if (cmd.includes('/') || cmd.includes(path.sep)) {
        try {
            await fs.access(cmd, fsConstants.X_OK);
            return cmd;
        } catch {
            return undefined;
        }
    }
    const PATH = process.env.PATH || '';
    const exts = process.platform === 'win32'
        ? (process.env.PATHEXT || '').split(';').filter(Boolean)
        : [''];
    for (const dir of PATH.split(path.delimiter)) {
        if (!dir) { continue; }
        for (const ext of exts) {
            const candidate = path.join(dir, cmd + ext);
            try {
                await fs.access(candidate, fsConstants.X_OK);
                return candidate;
            } catch { /* not executable here, keep looking */ }
        }
    }
    return undefined;
}

/**
 * The set of external tools the extension can use, with human-readable
 * descriptions for the settings panel info tooltips.
 */
export interface ToolDefinition {
    /** Internal id used as the cache key. */
    id: string;
    /** Tool name shown to the user. */
    label: string;
    /** Default executable name (overridable via settings for some tools). */
    defaultCommand: string;
    /** Flag used to probe for availability. */
    versionFlag: string;
    /** Why the extension needs this tool — shown in the info tooltip. */
    description: string;
}

export const TOOL_DEFINITIONS: readonly ToolDefinition[] = [
    {
        id: 'cabal',
        label: 'cabal',
        defaultCommand: 'cabal',
        versionFlag: '--version',
        description:
            'Cabal builds the Clash project and invokes the Clash compiler to '
            + 'generate Verilog from your Haskell sources. Required for every '
            + 'synthesis run.',
    },
    {
        id: 'yosys',
        label: 'yosys',
        defaultCommand: 'yosys',
        versionFlag: '-V',
        description:
            'Yosys is the open-source RTL synthesis suite that elaborates the '
            + 'Verilog and produces a gate-level netlist. Required for the '
            + 'Elaborate, Synthesize, and Place & Route commands.',
    },
    {
        id: 'nextpnr-ecp5',
        label: 'nextpnr-ecp5',
        defaultCommand: 'nextpnr-ecp5',
        versionFlag: '--version',
        description:
            'nextpnr-ecp5 places and routes the synthesized netlist onto the '
            + 'Lattice ECP5 fabric. Required for the Place & Route command.',
    },
];

/**
 * Represents the availability status of a single tool
 */
export interface ToolStatus {
    name: string;
    available: boolean;
    version?: string;
    error?: string;
    path?: string;
}

/**
 * Split a user-configured command string into command + args.
 * Honours double quotes so paths with spaces work:
 *   `"/opt/My Tools/bin/yosys" -m plug` → ['/opt/My Tools/bin/yosys', '-m', 'plug']
 */
export function splitCommand(command: string): string[] {
    const parts: string[] = [];
    const re = /"([^"]*)"|(\S+)/g;
    let m: RegExpExecArray | null;
    while ((m = re.exec(command)) !== null) {
        parts.push(m[1] !== undefined ? m[1] : m[2]);
    }
    return parts;
}

/**
 * Resolve the command for a tool from the `toolCommands` overrides.
 *
 * Every tool the extension spawns is named by its own id — `yosys`,
 * `nextpnr-ecp5`, `cabal` — so an absent override means "run the tool by name"
 * and the managed toolchain takes it from there. Pure so the precedence can be
 * tested without a workspace.
 */
export function resolveToolCommand(
    overrides: Record<string, string> | undefined,
    id: string,
): string {
    const configured = overrides?.[id]?.trim();
    return configured || id;
}

/**
 * The command string configured for a tool, ready to probe or spawn.
 *
 * The override exists for the cases neither PATH detection nor the managed
 * download can reach: a binary somewhere unusual, or a wrapper that has to run
 * in front of it (`wsl yosys`, `nix run nixpkgs#yosys --`). It applies to every
 * tool rather than yosys alone — place & route needs its nextpnr binary exactly
 * as much as synthesis needs yosys.
 */
export function toolCommand(id: string): string {
    const cfg = vscode.workspace.getConfiguration('clash-toolkit');
    const fromMap = resolveToolCommand(
        cfg.get<Record<string, string>>('toolCommands', {}),
        id,
    );
    if (fromMap !== id) { return fromMap; }

    // `yosysCommand` was the single-tool ancestor of `toolCommands`; it is
    // deprecated but still honoured, so an existing configuration keeps working
    // until its owner moves it over.
    if (id === 'yosys') {
        const legacy = cfg.get<string>('yosysCommand', '').trim();
        if (legacy && legacy !== 'yosys') { return legacy; }
    }
    return id;
}

/**
 * The configured command for a tool, split into the executable and any leading
 * wrapper arguments the caller must put in front of its own.
 */
export function toolInvocation(id: string): { command: string; args: string[] } {
    const parts = splitCommand(toolCommand(id));
    return { command: parts[0] || id, args: parts.slice(1) };
}

/**
 * Checks availability of external tools needed by the extension.
 * Results are cached per session and can be refreshed on demand.
 */
export class ToolchainChecker {
    /** Keyed by tool id; remembers which command string was probed so a
     *  settings change (e.g. a new yosysCommand) invalidates the entry. */
    private cache = new Map<string, { status: ToolStatus; command: string }>();
    private outputChannel: vscode.OutputChannel;

    constructor(outputChannel: vscode.OutputChannel) {
        this.outputChannel = outputChannel;
    }

    /**
     * Check if a command is available by running it with a version flag.
     * Returns a ToolStatus with availability info.
     */
    async check(
        name: string,
        command: string,
        versionFlag = '--version',
        cwd?: string
    ): Promise<ToolStatus> {
        const cached = this.cache.get(name);
        if (cached && cached.command === command) {
            return cached.status;
        }

        const status = await this.probe(name, command, versionFlag, cwd);
        this.cache.set(name, { status, command });
        return status;
    }

    /**
     * Probe a tool without caching.
     */
    private probe(
        name: string,
        command: string,
        versionFlag: string,
        cwd?: string
    ): Promise<ToolStatus> {
        return new Promise((resolve) => {
            const parts = splitCommand(command);
            // Resolve the executable token through the managed toolchain so a
            // probe reflects the same binary synthesis will actually spawn
            // (managed copy when the user has none on PATH).
            const cmd = resolveTool(parts[0]);
            const baseArgs = parts.slice(1);
            const args = [...baseArgs, versionFlag];

            const spawnOpts: { timeout: number; cwd?: string; env: NodeJS.ProcessEnv } =
                { timeout: 10_000, env: toolSpawnEnv(cmd) };
            if (cwd) {
                spawnOpts.cwd = cwd;
            }

            try {
                const logger = getLogger();
                const finishLog = logger?.command(cmd, args, spawnOpts.cwd);
                const proc = spawn(cmd, args, spawnOpts);

                let stdout = '';
                let stderr = '';

                proc.stdout.on('data', (d) => { stdout += d.toString(); });
                proc.stderr.on('data', (d) => { stderr += d.toString(); });

                proc.on('close', async (code) => {
                    finishLog?.then(fn => fn(code));
                    const output = (stdout + stderr).trim();
                    const firstLine = output.split('\n')[0] || '';

                    // Only a clean exit counts as available — a tool that
                    // prints an error and exits non-zero must not be reported
                    // as present with the error text shown as its "version".
                    if (code === 0) {
                        const resolvedPath = await resolveCommandPath(cmd);
                        resolve({
                            name,
                            available: true,
                            version: firstLine,
                            path: resolvedPath,
                        });
                    } else {
                        resolve({
                            name,
                            available: false,
                            error: firstLine
                                ? `Exited with code ${code}: ${firstLine}`
                                : `Exited with code ${code}`,
                        });
                    }
                });

                proc.on('error', (err) => {
                    resolve({
                        name,
                        available: false,
                        error: err.message,
                    });
                });
            } catch (err) {
                resolve({
                    name,
                    available: false,
                    error: err instanceof Error ? err.message : String(err),
                });
            }
        });
    }

    /**
     * Check all tools that the extension depends on.
     * Returns a map of tool name → status.
     */
    async checkAll(cwd?: string): Promise<Map<string, ToolStatus>> {
        const checks = TOOL_DEFINITIONS.map(def =>
            this.check(def.id, toolCommand(def.id), def.versionFlag, cwd)
        );

        await Promise.all(checks);
        return new Map(
            [...this.cache.entries()].map(([id, entry]) => [id, entry.status])
        );
    }

    /**
     * Snapshot the current cached statuses, ordered to match TOOL_DEFINITIONS.
     * Tools that have not been probed yet are returned with `available: false`.
     */
    snapshotStatuses(): ToolStatus[] {
        return TOOL_DEFINITIONS.map(def =>
            this.cache.get(def.id)?.status ?? {
                name: def.id,
                available: false,
                error: 'not yet probed',
            }
        );
    }

    /**
     * Clear the cache so the next check re-probes.
     */
    clearCache(): void {
        this.cache.clear();
    }

    /**
     * Require a specific tool before proceeding.
     * Shows an error message and returns false if the tool is missing.
     */
    async require(
        name: string,
        command: string,
        versionFlag = '--version',
        cwd?: string
    ): Promise<boolean> {
        let status = await this.check(name, command, versionFlag, cwd);
        if (status.available) { return true; }

        // The command isn't available. If the managed toolchain can supply it,
        // show the per-tool selection prompt and, on success, re-probe so the
        // run proceeds.
        const provider = getToolProvider();
        const baseCommand = splitCommand(command)[0] || name;
        if (provider?.canProvide(baseCommand)) {
            this.outputChannel.appendLine(
                `✗ ${name} is not available: ${status.error}. Offering managed install…`
            );
            const satisfied = await provider.promptToolSelection(baseCommand);
            if (satisfied) {
                provider.clearAvailabilityCache();
                // Drop the cached "unavailable" verdict so we re-probe the
                // freshly installed managed binary.
                this.cache.delete(name);
                status = await this.check(name, command, versionFlag, cwd);
                if (status.available) { return true; }
            } else {
                // User cancelled, left this tool unchecked, or the install
                // failed. promptToolSelection()/install() surfaced the reason.
                return false;
            }
        }

        // Every tool can be pointed somewhere else, so the same advice applies
        // whichever one is missing.
        const settingHint = 'clash-toolkit.toolCommands';
        const msg =
            `${name} is not available: ${status.error}. Make sure it is installed `
            + `and in your PATH, or point "${settingHint}" at it.`;

        this.outputChannel.appendLine(`✗ ${msg}`);
        vscode.window.showErrorMessage(msg, 'Open Settings').then((choice) => {
            if (choice === 'Open Settings') {
                vscode.commands.executeCommand(
                    'workbench.action.openSettings',
                    settingHint
                );
            }
        });
        return false;
    }

    /**
     * Format a summary of all tool statuses for the output channel.
     */
    formatSummary(): string {
        const lines: string[] = ['Toolchain Status:', '-'.repeat(40)];
        for (const [, { status }] of this.cache) {
            if (status.available) {
                lines.push(`  ✓ ${status.name}: ${status.version || 'available'}`);
            } else {
                lines.push(`  ✗ ${status.name}: ${status.error || 'not found'}`);
            }
        }
        return lines.join('\n');
    }
}
