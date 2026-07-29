import * as vscode from 'vscode';
import * as path from 'path';
import * as https from 'https';
import { createWriteStream, existsSync } from 'fs';
import { promises as fs } from 'fs';
import { spawn } from 'child_process';
import * as tar from 'tar';
import { getLogger } from './file-logger';

/**
 * Managed toolchain provider.
 *
 * The extension's synthesis flow spawns `yosys` and the `nextpnr-*` binaries.
 * Rather than force every user to install and PATH those tools themselves, this
 * class can download a self-contained
 * [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build) build — which
 * bundles all of them, mutually compatible — into the extension's private
 * global-storage directory, and resolve command names to the absolute paths of
 * those managed binaries.
 *
 * The public contract is deliberately small so the spawn-based runners don't
 * have to change how they invoke tools:
 *
 *   - {@link resolve} maps a bare command name (e.g. `yosys`) to an absolute
 *     path inside the managed install *if one is present*, otherwise returns
 *     the name unchanged so the existing PATH lookup still applies.
 *   - {@link spawnEnv} augments `process.env` so managed binaries find their
 *     co-located siblings and shared libraries.
 *   - {@link offerDownload} shows the "not found — download it?" notification
 *     and performs the install; called from the pre-flight tool check.
 */
export class ManagedToolchain {
    /**
     * OSS CAD Suite release this extension pins to. The date-stamped asset
     * names are derived from it. Bump deliberately — a new tag means a fresh
     * ~500 MB download for every user, so it should be a considered upgrade,
     * not an incidental one.
     */
    static readonly SUITE_TAG = '2026-07-20';

    /** Command base-names this provider is able to supply via the suite. */
    private static readonly PROVIDED = new Set([
        'yosys',
        'nextpnr-ecp5',
        'nextpnr-ice40',
        'nextpnr-himbaechel',
        'nextpnr-generic',
    ]);

    /**
     * Tools offered as checkboxes in the selection prompt, in display order.
     * Each maps to a command the runners spawn.
     */
    private static readonly PICKER_TOOLS: ReadonlyArray<{ id: string; label: string; detail: string }> = [
        { id: 'yosys', label: 'Yosys', detail: 'RTL synthesis — Elaborate, Synthesize, Place & Route' },
        { id: 'nextpnr-ecp5', label: 'nextpnr-ecp5', detail: 'Place & route for Lattice ECP5' },
        { id: 'nextpnr-ice40', label: 'nextpnr-ice40', detail: 'Place & route for Lattice iCE40' },
        { id: 'nextpnr-himbaechel', label: 'nextpnr-himbaechel', detail: 'Place & route for Gowin' },
    ];

    /** globalState key holding the ids of tools the user opted to have managed. */
    private static readonly MANAGED_KEY = 'clash-toolkit.managedTools';

    private readonly installRoot: string;
    private readonly binDir: string;
    private readonly markerFile: string;
    /** Cached availability, refreshed after a successful install. */
    private installed: boolean;
    /** Tool ids the user has opted to have the extension download & manage. */
    private managed: Set<string>;
    /** In-flight install, so concurrent misses share a single download. */
    private installPromise?: Promise<boolean>;

    constructor(
        private readonly context: vscode.ExtensionContext,
        private readonly outputChannel: vscode.OutputChannel
    ) {
        this.installRoot = path.join(
            context.globalStorageUri.fsPath,
            'oss-cad-suite'
        );
        this.binDir = path.join(this.installRoot, 'bin');
        this.markerFile = path.join(this.installRoot, '.clash-toolkit-tag');
        this.installed = this.detectInstalled();
        this.managed = new Set(
            context.globalState.get<string[]>(ManagedToolchain.MANAGED_KEY, [])
        );
    }

    /** Persist the per-tool managed opt-in set. */
    private async persistManaged(): Promise<void> {
        await this.context.globalState.update(
            ManagedToolchain.MANAGED_KEY,
            [...this.managed]
        );
    }

    /** Whether a managed binary for `name` exists on disk. */
    private hasManagedBinary(name: string): boolean {
        return this.canProvide(name) && existsSync(this.exePath(name));
    }

    /** Whether a managed install for the pinned tag is present on disk. */
    private detectInstalled(): boolean {
        try {
            if (!existsSync(this.markerFile)) { return false; }
            // A yosys binary in the expected place is the load-bearing check;
            // the marker alone could survive a half-deleted directory.
            return existsSync(this.exePath('yosys'));
        } catch {
            return false;
        }
    }

    /** Absolute path a managed binary *would* live at (may not exist). */
    private exePath(name: string): string {
        const exe = process.platform === 'win32' ? `${name}.exe` : name;
        return path.join(this.binDir, exe);
    }

    /** Cache of PATH-availability decisions, keyed by command name. */
    private readonly onPathCache = new Map<string, boolean>();

    /**
     * Whether `name` resolves to a runnable executable already — either an
     * explicit path that exists, or a bare name found on PATH (honouring
     * PATHEXT on Windows). Synchronous and cached so it's cheap per spawn.
     */
    private isAvailable(name: string): boolean {
        const cached = this.onPathCache.get(name);
        if (cached !== undefined) { return cached; }

        let found = false;
        if (name.includes('/') || name.includes(path.sep)) {
            found = existsSync(name);
        } else {
            const exts = process.platform === 'win32'
                ? (process.env.PATHEXT || '.EXE;.CMD;.BAT').split(';').filter(Boolean)
                : [''];
            for (const dir of (process.env.PATH || '').split(path.delimiter)) {
                if (!dir) { continue; }
                if (exts.some(ext => existsSync(path.join(dir, name + ext)))) {
                    found = true;
                    break;
                }
            }
        }
        this.onPathCache.set(name, found);
        return found;
    }

    /** Forget cached PATH decisions (e.g. after the user installs a tool). */
    clearAvailabilityCache(): void {
        this.onPathCache.clear();
    }

    /** True if this provider knows how to supply the given command. */
    canProvide(name: string): boolean {
        return ManagedToolchain.PROVIDED.has(name);
    }

    /** True once a managed install is available on disk. */
    isInstalled(): boolean {
        return this.installed;
    }

    /** Absolute directory the managed suite is (or would be) installed in. */
    get location(): string {
        return this.installRoot;
    }

    /** One-line human-readable summary for the toolchain report. */
    describeStatus(): string {
        if (this.installed) {
            const managedList = [...this.managed].sort().join(', ') || 'none';
            return (
                `Managed toolchain: OSS CAD Suite ${ManagedToolchain.SUITE_TAG} at ${this.installRoot}\n` +
                `  Managed tools: ${managedList}`
            );
        }
        const asset = this.assetForPlatform();
        return asset
            ? 'Managed toolchain: not installed (run "Clash: Install Toolchain" to choose tools to download)'
            : `Managed toolchain: unavailable for ${process.platform}/${process.arch}`;
    }

    /**
     * Manual entry point for the "Install Toolchain" command — shows the
     * per-tool selection prompt with no specific triggering tool.
     */
    async promptInstallOrManage(): Promise<void> {
        await this.promptToolSelection();
    }

    /**
     * Resolve a command name to a runnable executable.
     *
     * If the user opted to have this tool managed *and* a managed copy exists,
     * the managed absolute path is returned. Otherwise the name is returned
     * unchanged so the caller's normal PATH lookup applies (the user provides
     * their own). Never throws; safe to call on every spawn.
     */
    resolve(name: string): string {
        if (this.managed.has(name) && this.hasManagedBinary(name)) {
            return this.exePath(name);
        }
        return name;
    }

    /**
     * Environment for spawning `resolvedCommand`. When it is one of our managed
     * binaries, the managed `bin/` is prepended to PATH so it finds its
     * co-located siblings (and DLLs on Windows). For anything else — a tool the
     * user provides themselves — `process.env` is returned unchanged, so a
     * managed install never shadows the user's own tools.
     */
    spawnEnv(resolvedCommand?: string): NodeJS.ProcessEnv {
        const isManagedBinary =
            !!resolvedCommand &&
            resolvedCommand.startsWith(this.binDir + path.sep);
        if (!isManagedBinary) { return process.env; }
        const prev = process.env.PATH || '';
        return {
            ...process.env,
            PATH: this.binDir + (prev ? path.delimiter + prev : ''),
        };
    }

    /**
     * Show the per-tool checkbox prompt and act on the user's selection.
     *
     * Each providable tool is listed with a checkbox, pre-checked when the tool
     * is *not* already available on PATH (so the missing ones are offered for
     * download by default) and left unchecked when it is found. The user is
     * free to toggle any of them. The checked set is persisted as the tools the
     * extension will download & manage; if any checked tool isn't present yet,
     * the OSS CAD Suite is fetched (a single archive that supplies them all).
     *
     * @param triggerId the command that was missing and prompted this, if any.
     * @returns true if `triggerId` is usable afterwards (or no trigger); false
     *          if the user cancelled, declined it, or the install failed.
     */
    async promptToolSelection(triggerId?: string): Promise<boolean> {
        const asset = this.assetForPlatform();
        if (!asset) {
            vscode.window.showErrorMessage(
                'Clash Toolkit cannot auto-install tools: no OSS CAD Suite build ' +
                `is available for ${process.platform}/${process.arch}. Install the ` +
                'tools manually and make sure they are on your PATH.'
            );
            return false;
        }

        interface ToolPick extends vscode.QuickPickItem { id: string }
        const items: ToolPick[] = ManagedToolchain.PICKER_TOOLS.map((t) => {
            const onPath = this.isAvailable(t.id);
            const managedNow = this.managed.has(t.id) && this.hasManagedBinary(t.id);
            const description = onPath
                ? '$(check) found on PATH'
                : managedNow
                    ? '$(package) managed'
                    : '$(cloud-download) not found — download';
            return {
                id: t.id,
                label: t.label,
                description,
                detail: t.detail,
                // Missing tools are pre-checked (offer to download); found ones
                // aren't. An existing opt-in is preserved.
                picked: this.managed.has(t.id) || !onPath,
            };
        });

        const picked = await vscode.window.showQuickPick(items, {
            canPickMany: true,
            ignoreFocusOut: true,
            title: triggerId
                ? `${triggerId} was not found — choose tools for Clash Toolkit to download & manage`
                : 'Choose tools for Clash Toolkit to download & manage',
            placeHolder:
                `Checked tools are downloaded via the OSS CAD Suite (~${asset.approxMB} MB, ` +
                'one archive); unchecked tools use your PATH.',
        });
        if (!picked) { return false; } // dismissed — leave settings unchanged

        this.managed = new Set(picked.map((p) => p.id));
        await this.persistManaged();
        this.clearAvailabilityCache();

        // Fetch the archive only if a checked tool has no managed binary yet.
        const needsInstall = [...this.managed].some((id) => !this.hasManagedBinary(id));
        if (needsInstall) {
            const ok = await this.install(asset);
            if (!ok) { return false; }
        }

        if (!triggerId) { return true; }
        // Trigger is satisfied if it's now managed-on-disk or on the user's PATH.
        return this.resolve(triggerId) !== triggerId || this.isAvailable(triggerId);
    }

    /**
     * Download and extract the suite. Idempotent and safe to call concurrently:
     * overlapping callers share one download.
     */
    private install(asset: PlatformAsset): Promise<boolean> {
        if (this.installed) { return Promise.resolve(true); }
        if (this.installPromise) { return this.installPromise; }
        this.installPromise = this.doInstall(asset).finally(() => {
            this.installPromise = undefined;
        });
        return this.installPromise;
    }

    private async doInstall(asset: PlatformAsset): Promise<boolean> {
        const logger = getLogger();
        const stagingRoot = path.join(
            this.context.globalStorageUri.fsPath,
            '.download'
        );
        const archivePath = path.join(stagingRoot, asset.fileName);

        try {
            await fs.mkdir(stagingRoot, { recursive: true });
            // A previous failed install may have left a partial suite behind.
            await fs.rm(this.installRoot, { recursive: true, force: true });

            return await vscode.window.withProgress(
                {
                    location: vscode.ProgressLocation.Notification,
                    title: 'Clash Toolkit: installing OSS CAD Suite',
                    cancellable: true,
                },
                async (progress, token) => {
                    progress.report({ message: 'downloading…' });
                    logger?.info(`Downloading ${asset.url}`);
                    await downloadFile(asset.url, archivePath, token, (pct) => {
                        progress.report({
                            message: `downloading… ${pct}%`,
                        });
                    });
                    if (token.isCancellationRequested) {
                        this.outputChannel.appendLine('Toolchain download cancelled.');
                        return false;
                    }

                    progress.report({ message: 'extracting…' });
                    logger?.info(`Extracting ${archivePath} → ${this.context.globalStorageUri.fsPath}`);
                    await this.extract(archivePath, asset);

                    if (!existsSync(this.exePath('yosys'))) {
                        throw new Error(
                            'extraction completed but no yosys binary was found ' +
                            `at ${this.binDir}`
                        );
                    }

                    await fs.writeFile(this.markerFile, ManagedToolchain.SUITE_TAG);
                    this.installed = true;
                    this.outputChannel.appendLine(
                        `✓ OSS CAD Suite ${ManagedToolchain.SUITE_TAG} installed at ${this.installRoot}`
                    );
                    vscode.window.showInformationMessage(
                        'Clash Toolkit: toolchain installed. You can retry your command now.'
                    );
                    return true;
                }
            );
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            this.outputChannel.appendLine(`✗ Toolchain install failed: ${msg}`);
            logger?.error(`Toolchain install failed: ${msg}`);
            // Don't leave a half-extracted tree that detectInstalled() might
            // later mistake for a good install.
            await fs.rm(this.installRoot, { recursive: true, force: true }).catch(() => {});
            vscode.window.showErrorMessage(
                `Clash Toolkit: failed to install the toolchain — ${msg}`
            );
            return false;
        } finally {
            await fs.rm(archivePath, { force: true }).catch(() => {});
        }
    }

    /** Unpack the downloaded archive into the global-storage directory. */
    private async extract(archivePath: string, asset: PlatformAsset): Promise<void> {
        const destParent = this.context.globalStorageUri.fsPath;
        if (asset.fileName.endsWith('.tgz')) {
            // The tarball's top-level entry is `oss-cad-suite/…`, so extracting
            // into globalStorage yields exactly `installRoot`.
            await tar.x({ file: archivePath, cwd: destParent });
            return;
        }
        // Windows: a 7-Zip self-extracting .exe. Run it silently, extracting
        // into globalStorage (it writes an `oss-cad-suite/` subtree).
        await new Promise<void>((resolve, reject) => {
            const proc = spawn(archivePath, ['-y'], { cwd: destParent });
            proc.on('error', reject);
            proc.on('close', (code) => {
                if (code === 0) {
                    resolve();
                } else {
                    reject(new Error(`self-extractor exited with code ${code}`));
                }
            });
        });
    }

    /** Resolve the release asset for the current platform, or undefined. */
    private assetForPlatform(): PlatformAsset | undefined {
        const date = ManagedToolchain.SUITE_TAG.replace(/-/g, '');
        const base = `https://github.com/YosysHQ/oss-cad-suite-build/releases/download/${ManagedToolchain.SUITE_TAG}`;
        const make = (osName: string, arch: string, ext: string, approxMB: number): PlatformAsset => {
            const fileName = `oss-cad-suite-${osName}-${arch}-${date}.${ext}`;
            return { fileName, url: `${base}/${fileName}`, approxMB };
        };

        const arch = process.arch;
        switch (process.platform) {
            case 'linux':
                if (arch === 'x64') { return make('linux', 'x64', 'tgz', 730); }
                if (arch === 'arm64') { return make('linux', 'arm64', 'tgz', 620); }
                return undefined;
            case 'darwin':
                if (arch === 'x64') { return make('darwin', 'x64', 'tgz', 485); }
                if (arch === 'arm64') { return make('darwin', 'arm64', 'tgz', 510); }
                return undefined;
            case 'win32':
                if (arch === 'x64') { return make('windows', 'x64', 'exe', 335); }
                return undefined;
            default:
                return undefined;
        }
    }
}

interface PlatformAsset {
    fileName: string;
    url: string;
    /** Approximate download size in MB, for the download prompt. */
    approxMB: number;
}

/**
 * Stream a URL to a file, following GitHub's redirect to its CDN and reporting
 * integer percent progress. Rejects (and removes the partial file) on any HTTP
 * error, network error, or user cancellation.
 */
function downloadFile(
    url: string,
    dest: string,
    token: vscode.CancellationToken,
    onProgress: (pct: number) => void
): Promise<void> {
    return new Promise<void>((resolve, reject) => {
        const cleanupAndReject = (err: Error) => {
            fs.rm(dest, { force: true }).catch(() => {});
            reject(err);
        };

        const get = (target: string, redirectsLeft: number) => {
            const req = https.get(target, (res) => {
                const status = res.statusCode ?? 0;
                if (status >= 300 && status < 400 && res.headers.location) {
                    res.resume(); // drain
                    if (redirectsLeft <= 0) {
                        cleanupAndReject(new Error('too many redirects'));
                        return;
                    }
                    const next = new URL(res.headers.location, target).toString();
                    get(next, redirectsLeft - 1);
                    return;
                }
                if (status !== 200) {
                    res.resume();
                    cleanupAndReject(new Error(`HTTP ${status} downloading ${target}`));
                    return;
                }

                const total = Number(res.headers['content-length']) || 0;
                let received = 0;
                let lastPct = -1;
                const file = createWriteStream(dest);

                const abort = () => {
                    req.destroy();
                    file.destroy();
                    cleanupAndReject(new Error('download cancelled'));
                };
                const sub = token.onCancellationRequested(abort);

                res.on('data', (chunk: Buffer) => {
                    received += chunk.length;
                    if (total > 0) {
                        const pct = Math.floor((received / total) * 100);
                        if (pct !== lastPct) { lastPct = pct; onProgress(pct); }
                    }
                });
                res.on('error', (e) => { sub.dispose(); cleanupAndReject(e); });
                file.on('error', (e) => { sub.dispose(); cleanupAndReject(e); });
                file.on('finish', () => {
                    sub.dispose();
                    file.close((e) => (e ? cleanupAndReject(e) : resolve()));
                });
                res.pipe(file);
            });
            req.on('error', cleanupAndReject);
        };

        get(url, 5);
    });
}

let provider: ManagedToolchain | undefined;

/** Initialize the singleton. Call once from `activate`. */
export function initializeToolProvider(
    context: vscode.ExtensionContext,
    outputChannel: vscode.OutputChannel
): ManagedToolchain {
    provider = new ManagedToolchain(context, outputChannel);
    return provider;
}

/** The managed-toolchain singleton, or undefined before activation. */
export function getToolProvider(): ManagedToolchain | undefined {
    return provider;
}

/**
 * Resolve a command name against the managed install, if any. Falls back to the
 * unchanged name when no provider is initialized or no managed copy exists —
 * so the caller's normal PATH lookup still applies.
 */
export function resolveTool(name: string): string {
    return provider ? provider.resolve(name) : name;
}

/**
 * Spawn environment for `resolvedCommand`. Augmented (managed `bin/` on PATH)
 * only when it's one of our managed binaries; otherwise `process.env`.
 */
export function toolSpawnEnv(resolvedCommand?: string): NodeJS.ProcessEnv {
    return provider ? provider.spawnEnv(resolvedCommand) : process.env;
}
