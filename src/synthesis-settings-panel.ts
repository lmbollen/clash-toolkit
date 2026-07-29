import * as vscode from 'vscode';
import {
	SYNTHESIS_TARGETS,
	TARGET_IDS,
	getDefaultScript,
	getDefaultElaborationScript,
	computeScriptDiff,
	DiffLine,
} from './synthesis-targets';
import { ToolchainChecker, TOOL_DEFINITIONS } from './toolchain';

/**
 * Webview panel for extension settings.
 *
 * Opened via the gear icon in the sidebar.  Contains a "Synthesis" section
 * with a target dropdown, an editable script textarea, and an inline diff
 * view when the script has been modified from its default.
 */
export class SynthesisSettingsPanel {
	private static currentPanel?: vscode.WebviewPanel;
	private static messageListener?: vscode.Disposable;
	private static configListener?: vscode.Disposable;
	private static toolchain?: ToolchainChecker;

	/**
	 * Open (or reveal) the settings panel.
	 */
	static show(toolchain: ToolchainChecker): void {
		SynthesisSettingsPanel.toolchain = toolchain;
		const column = vscode.ViewColumn.One;

		if (SynthesisSettingsPanel.currentPanel) {
			SynthesisSettingsPanel.currentPanel.reveal(column);
			// Re-probe so the panel reflects current state on every open.
			SynthesisSettingsPanel.refreshTools(SynthesisSettingsPanel.currentPanel);
			return;
		}

		const panel = vscode.window.createWebviewPanel(
			'clashSettings',
			'Clash Synthesis Settings',
			column,
			{
				enableScripts: true,
				retainContextWhenHidden: true,
				localResourceRoots: [],
			}
		);

		SynthesisSettingsPanel.currentPanel = panel;

		panel.onDidDispose(() => {
			SynthesisSettingsPanel.currentPanel = undefined;
			SynthesisSettingsPanel.messageListener?.dispose();
			SynthesisSettingsPanel.messageListener = undefined;
			SynthesisSettingsPanel.configListener?.dispose();
			SynthesisSettingsPanel.configListener = undefined;
		});

		// Listen to webview messages
		SynthesisSettingsPanel.messageListener = panel.webview.onDidReceiveMessage(
			async (message) => {
				const cfg = vscode.workspace.getConfiguration('clash-toolkit');
				try {
					switch (message.type) {
						case 'changeTarget': {
							await cfg.update('synthesisTarget', message.targetId, vscode.ConfigurationTarget.Workspace);
							// sendState will be triggered by the config change listener
							break;
						}
						case 'saveScript': {
							const targetId = message.targetId as string;
							const script = message.script as string;
							const defaultScript = getDefaultScript(targetId);
							// If identical to default, clear the override
							if (script.trim() === defaultScript.trim()) {
								await cfg.update(`synthesisScript.${targetId}`, undefined, vscode.ConfigurationTarget.Workspace);
							} else {
								await cfg.update(`synthesisScript.${targetId}`, script, vscode.ConfigurationTarget.Workspace);
							}
							// The webview flashes "Saved" only on this confirmation,
							// not optimistically — cfg.update can fail (e.g. no
							// workspace open).
							panel.webview.postMessage({ type: 'saveResult', section: 'script', ok: true });
							break;
						}
						case 'resetScript': {
							const targetId = message.targetId as string;
							await cfg.update(`synthesisScript.${targetId}`, undefined, vscode.ConfigurationTarget.Workspace);
							break;
						}
						case 'saveElaborationScript': {
							const script = message.script as string;
							const defaultScript = getDefaultElaborationScript();
							if (script.trim() === defaultScript.trim()) {
								await cfg.update('elaborationScript', undefined, vscode.ConfigurationTarget.Workspace);
							} else {
								await cfg.update('elaborationScript', script, vscode.ConfigurationTarget.Workspace);
							}
							panel.webview.postMessage({ type: 'saveResult', section: 'elab', ok: true });
							break;
						}
						case 'resetElaborationScript': {
							await cfg.update('elaborationScript', undefined, vscode.ConfigurationTarget.Workspace);
							break;
						}
						case 'changeOutOfContext': {
							await cfg.update('outOfContext', message.outOfContext, vscode.ConfigurationTarget.Workspace);
							break;
						}
						case 'ready': {
							SynthesisSettingsPanel.sendState(panel);
							SynthesisSettingsPanel.refreshTools(panel);
							break;
						}
						case 'refreshTools': {
							SynthesisSettingsPanel.refreshTools(panel);
							break;
						}
					}
				} catch (err) {
					// cfg.update rejects when there is no workspace to write to,
					// among other causes — surface it instead of losing the save.
					const detail = err instanceof Error ? err.message : String(err);
					vscode.window.showErrorMessage(`Failed to update Clash settings: ${detail}`);
					if (message.type === 'saveScript' || message.type === 'saveElaborationScript') {
						panel.webview.postMessage({
							type: 'saveResult',
							section: message.type === 'saveScript' ? 'script' : 'elab',
							ok: false,
						});
					}
					// Re-send the persisted state so the UI reflects reality.
					SynthesisSettingsPanel.sendState(panel);
				}
			}
		);

		// Re-send state when configuration changes (e.g. after save/reset)
		SynthesisSettingsPanel.configListener = vscode.workspace.onDidChangeConfiguration(e => {
			if (e.affectsConfiguration('clash-toolkit') && SynthesisSettingsPanel.currentPanel) {
				SynthesisSettingsPanel.sendState(SynthesisSettingsPanel.currentPanel);
			}
		});

		panel.webview.html = SynthesisSettingsPanel.buildHtml();
	}

	// -----------------------------------------------------------------------
	// State
	// -----------------------------------------------------------------------

	/**
	 * Re-probe the toolchain and push the updated tool list to the panel.
	 * Called on initial open, on user-triggered refresh, and when the panel
	 * is revealed again.
	 */
	private static async refreshTools(panel: vscode.WebviewPanel): Promise<void> {
		const tc = SynthesisSettingsPanel.toolchain;
		if (!tc) { return; }
		try {
			panel.webview.postMessage({ type: 'toolsLoading' });
			tc.clearCache();
			const cwd = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
			await tc.checkAll(cwd);
			// The user may close the panel while the probes run — posting to a
			// disposed webview throws.
			if (SynthesisSettingsPanel.currentPanel !== panel) { return; }
			const statuses = tc.snapshotStatuses();
			const tools = TOOL_DEFINITIONS.map((def, i) => ({
				id: def.id,
				label: def.label,
				description: def.description,
				available: statuses[i].available,
				version: statuses[i].version,
				toolPath: statuses[i].path,
				error: statuses[i].error,
			}));
			panel.webview.postMessage({ type: 'tools', tools });
		} catch {
			// Callers fire-and-forget this method; never reject, and never
			// leave the refresh button stuck spinning on a live panel.
			if (SynthesisSettingsPanel.currentPanel === panel) {
				try {
					panel.webview.postMessage({ type: 'tools', tools: [] });
				} catch { /* panel disposed mid-error — nothing to unstick */ }
			}
		}
	}

	private static sendState(panel: vscode.WebviewPanel): void {
		try {
			SynthesisSettingsPanel.sendStateUnsafe(panel);
		} catch (err) {
			// e.g. a stale/unknown synthesisTarget in workspace settings makes
			// getDefaultScript throw — surface it loudly instead of leaving the
			// panel silently un-updated (or crashing a config listener).
			const detail = err instanceof Error ? err.message : String(err);
			vscode.window.showErrorMessage(`Clash settings panel: ${detail}`);
		}
	}

	private static sendStateUnsafe(panel: vscode.WebviewPanel): void {
		const cfg = vscode.workspace.getConfiguration('clash-toolkit');
		const targetId = cfg.get<string>('synthesisTarget', 'generic');
		const outOfContext = cfg.get<boolean>('outOfContext', false);
		const defaultScript = getDefaultScript(targetId);
		const customScript = cfg.get<string>(`synthesisScript.${targetId}`, '') || '';
		const diff: DiffLine[] = customScript
			? computeScriptDiff(defaultScript, customScript)
			: [];
		const targets = TARGET_IDS.map(id => ({
			id,
			label: SYNTHESIS_TARGETS.get(id)!.label,
		}));

		const elaborationDefault = getDefaultElaborationScript();
		const elaborationCustom = cfg.get<string>('elaborationScript', '') || '';
		const elaborationDiff: DiffLine[] = elaborationCustom
			? computeScriptDiff(elaborationDefault, elaborationCustom)
			: [];

		panel.webview.postMessage({
			type: 'state',
			targetId,
			outOfContext,
			targets,
			defaultScript,
			customScript,
			diff,
			elaborationDefault,
			elaborationCustom,
			elaborationDiff,
		});
	}

	// -----------------------------------------------------------------------
	// HTML
	// -----------------------------------------------------------------------

	private static buildHtml(): string {
		return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy"
  content="default-src 'none';
           style-src 'unsafe-inline';
           script-src 'unsafe-inline';">
<title>Clash Synthesis Settings</title>
<style>
  :root {
    --pad: 16px;
    --radius: 4px;
    --gap: 10px;
  }
  * { box-sizing: border-box; }
  body {
    font-family: var(--vscode-font-family);
    font-size: var(--vscode-font-size);
    color: var(--vscode-foreground);
    background: var(--vscode-editor-background);
    margin: 0;
    padding: var(--pad);
    overflow-y: auto;
    max-width: 800px;
  }

  h1 {
    font-size: 18px;
    font-weight: 600;
    margin: 0 0 20px 0;
    border-bottom: 1px solid var(--vscode-editorWidget-border, #444);
    padding-bottom: 8px;
  }

  h2 {
    font-size: 13px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    color: var(--vscode-descriptionForeground);
    margin: 24px 0 12px 0;
  }
  h2:first-of-type { margin-top: 0; }

  /* ── Form controls ── */
  .field {
    margin-bottom: 14px;
  }
  .field label {
    display: block;
    font-size: 12px;
    font-weight: 600;
    margin-bottom: 4px;
  }
  .field .description {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    margin-bottom: 6px;
  }

  select {
    padding: 4px 8px;
    border: 1px solid var(--vscode-dropdown-border, #555);
    border-radius: var(--radius);
    background: var(--vscode-dropdown-background);
    color: var(--vscode-dropdown-foreground);
    font-family: var(--vscode-font-family);
    font-size: 12px;
    min-width: 250px;
  }

  textarea {
    width: 100%;
    min-height: 280px;
    padding: 8px;
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: var(--radius);
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 12px;
    line-height: 1.6;
    resize: vertical;
    tab-size: 2;
  }
  textarea:focus {
    outline: 1px solid var(--vscode-focusBorder);
    border-color: var(--vscode-focusBorder);
  }

  .btn {
    padding: 4px 12px;
    border: none;
    border-radius: var(--radius);
    cursor: pointer;
    font-size: 12px;
    font-family: var(--vscode-font-family);
    background: var(--vscode-button-background);
    color: var(--vscode-button-foreground);
  }
  .btn:hover { background: var(--vscode-button-hoverBackground); }
  .btn-secondary {
    background: var(--vscode-button-secondaryBackground);
    color: var(--vscode-button-secondaryForeground);
  }
  .btn-secondary:hover { background: var(--vscode-button-secondaryHoverBackground); }

  .btn-row {
    display: flex;
    gap: 8px;
    align-items: center;
    margin-top: 8px;
  }

  .badge {
    display: inline-block;
    font-size: 10px;
    padding: 1px 7px;
    border-radius: 8px;
    background: var(--vscode-badge-background);
    color: var(--vscode-badge-foreground);
    vertical-align: middle;
    margin-left: 6px;
  }
  .badge.hidden { display: none; }

  .saved-msg {
    font-size: 11px;
    color: var(--vscode-testing-iconPassed, #4ec950);
    opacity: 0;
    transition: opacity 0.2s;
  }
  .saved-msg.show { opacity: 1; }
  .saved-msg.error { color: var(--vscode-testing-iconFailed, #f14c4c); }

  /* ── Diff section ── */
  .diff-section {
    margin-top: 12px;
  }
  .diff-header {
    display: flex;
    align-items: center;
    gap: 6px;
    font-size: 11px;
    font-weight: 600;
    cursor: pointer;
    user-select: none;
    padding: 4px 0;
  }
  .diff-header .chevron {
    font-size: 10px;
    transition: transform 0.15s;
  }
  .diff-header .chevron.open { transform: rotate(90deg); }

  .diff-body {
    display: none;
    border: 1px solid var(--vscode-editorWidget-border, #444);
    border-radius: var(--radius);
    background: var(--vscode-editor-background);
    padding: 8px;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    line-height: 1.6;
    white-space: pre;
    overflow-x: auto;
    max-height: 300px;
    overflow-y: auto;
  }
  .diff-body.visible { display: block; }

  .diff-added {
    background: var(--vscode-diffEditor-insertedTextBackground, rgba(0,180,0,0.15));
  }
  .diff-removed {
    background: var(--vscode-diffEditor-removedTextBackground, rgba(220,0,0,0.15));
    text-decoration: line-through;
    opacity: 0.7;
  }

  .placeholder-help {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    margin-top: 6px;
    line-height: 1.5;
  }
  .placeholder-help code {
    background: var(--vscode-textCodeBlock-background, rgba(128,128,128,0.15));
    padding: 1px 4px;
    border-radius: 3px;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
  }

  /* ── Tools section ── */
  .section-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin: 24px 0 12px 0;
  }
  .section-header:first-of-type { margin-top: 0; }
  .section-header h2 { margin: 0; }

  .tools-list {
    border: 1px solid var(--vscode-editorWidget-border, #444);
    border-radius: var(--radius);
  }
  .tool-row {
    display: grid;
    grid-template-columns: 24px 1fr auto 24px;
    align-items: center;
    gap: 10px;
    padding: 8px 12px;
    border-bottom: 1px solid var(--vscode-editorWidget-border, #444);
  }
  .tool-row:last-child { border-bottom: none; }
  .tool-row:first-child .info-icon .info-tooltip {
    bottom: auto;
    top: calc(100% + 8px);
  }

  .tool-status {
    font-size: 14px;
    line-height: 1;
    text-align: center;
  }
  .tool-status.ok { color: var(--vscode-testing-iconPassed, #4ec950); }
  .tool-status.missing { color: var(--vscode-testing-iconFailed, #f14c4c); }
  .tool-status.loading { color: var(--vscode-descriptionForeground); }

  .tool-info {
    display: flex;
    flex-direction: column;
    gap: 2px;
    min-width: 0;
  }
  .tool-name {
    font-weight: 600;
    font-size: 12px;
  }
  .tool-detail {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .tool-path {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    text-align: right;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    max-width: 360px;
  }
  .tool-path.missing-path {
    color: var(--vscode-testing-iconFailed, #f14c4c);
    font-style: italic;
    font-family: var(--vscode-font-family);
  }

  .info-icon {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 18px;
    height: 18px;
    border-radius: 50%;
    border: 1px solid var(--vscode-descriptionForeground);
    color: var(--vscode-descriptionForeground);
    font-size: 11px;
    font-weight: 600;
    cursor: help;
    user-select: none;
    position: relative;
  }
  .info-icon:hover {
    border-color: var(--vscode-foreground);
    color: var(--vscode-foreground);
  }
  .info-icon .info-tooltip {
    visibility: hidden;
    opacity: 0;
    position: absolute;
    bottom: calc(100% + 8px);
    right: 0;
    width: 300px;
    padding: 8px 10px;
    background: var(--vscode-editorHoverWidget-background, #252526);
    color: var(--vscode-editorHoverWidget-foreground, #cccccc);
    border: 1px solid var(--vscode-editorHoverWidget-border, #454545);
    border-radius: var(--radius);
    font-size: 11px;
    font-weight: normal;
    line-height: 1.5;
    text-align: left;
    white-space: normal;
    pointer-events: none;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
    z-index: 100;
    transition: opacity 0.15s ease, visibility 0s linear 0.15s;
  }
  .info-icon:hover .info-tooltip,
  .info-icon:focus .info-tooltip {
    visibility: visible;
    opacity: 1;
    transition: opacity 0.15s ease 0.1s, visibility 0s linear;
  }

  .btn-icon {
    background: transparent;
    border: 1px solid transparent;
    color: var(--vscode-foreground);
    padding: 2px 6px;
    border-radius: var(--radius);
    cursor: pointer;
    font-size: 12px;
  }
  .btn-icon:hover {
    background: var(--vscode-toolbar-hoverBackground, rgba(255,255,255,0.08));
  }
  .btn-icon.spinning { opacity: 0.5; pointer-events: none; }
</style>
</head>
<body>

<h1>Clash Synthesis Settings</h1>

<!-- ═══════════════════════════════════════════════════════════════════════ -->
<div class="section-header">
  <h2>Tools</h2>
  <button class="btn-icon" id="refresh-tools-btn" onclick="refreshTools()" title="Re-probe the toolchain">
    &#x21bb; Refresh
  </button>
</div>
<div class="tools-list" id="tools-list">
  <div class="tool-row">
    <div class="tool-status loading">…</div>
    <div class="tool-info"><div class="tool-name">Probing toolchain…</div></div>
    <div class="tool-path"></div>
    <div></div>
  </div>
</div>

<!-- ═══════════════════════════════════════════════════════════════════════ -->
<h2>Elaboration</h2>

<div class="field">
  <label>
    Elaboration Script
    <span class="badge hidden" id="elab-modified-badge">modified</span>
  </label>
  <div class="description">
    Yosys script used for the Elaborate command — hierarchy + proc only, no
    technology mapping.  Produces a word-level netlist with generic cells
    (<code>$add</code>, <code>$mux</code>, <code>$dff</code>, ...).
  </div>
  <textarea id="elab-script-editor" spellcheck="false"></textarea>
  <div class="placeholder-help">
    Placeholders:
    <code>{files}</code> input Verilog files &middot;
    <code>{topModule}</code> top module name &middot;
    <code>{outputDir}</code> output directory &middot;
    <code>{outputBaseName}</code> base filename
  </div>
  <div class="btn-row">
    <button class="btn" id="elab-save-btn" onclick="saveElaborationScript()">Save</button>
    <button class="btn btn-secondary" id="elab-reset-btn" onclick="resetElaborationScript()">Reset to Default</button>
    <span class="saved-msg" id="elab-saved-msg">Saved</span>
  </div>
</div>

<div class="diff-section" id="elab-diff-section" style="display:none">
  <div class="diff-header" onclick="toggleElabDiff()">
    <span class="chevron" id="elab-diff-chevron">&#9654;</span>
    <span>Changes from default</span>
  </div>
  <div class="diff-body" id="elab-diff-body"></div>
</div>

<!-- ═══════════════════════════════════════════════════════════════════════ -->
<h2>Synthesis</h2>

<div class="field">
  <label for="target-select">Target</label>
  <div class="description">FPGA family used for Yosys synthesis.</div>
  <select id="target-select"></select>
</div>

<div class="field">
  <label>
    <input type="checkbox" id="out-of-context" />
    Out-of-context synthesis
  </label>
  <div class="description">
    When enabled, multi-component designs are synthesized one component at a
    time, so each gets its own utilization stats and circuit diagram. Each
    component is synthesized on its own, with the components it instantiates
    flattened into it and <strong>no technology mapping</strong>
    (<code>proc</code>, <code>flatten</code>, <code>opt</code>,
    <code>memory -nomap</code>). The target and script below therefore do not
    apply to those results: cells stay generic, a component's figures include
    its descendants, and nothing is optimized against its parent. Use them to
    compare components with each other, not to predict whole-design
    utilization. When disabled, the whole design is synthesized as a single
    netlist with the selected target.
  </div>
</div>

<div class="field">
  <label>
    Synthesis Script
    <span class="badge hidden" id="modified-badge">modified</span>
  </label>
  <div class="description">
    Yosys script for the selected target. Edit below to customise. Use placeholders for dynamic values.
  </div>
  <textarea id="script-editor" spellcheck="false"></textarea>
  <div class="placeholder-help">
    Placeholders:
    <code>{files}</code> input Verilog files &middot;
    <code>{topModule}</code> top module name &middot;
    <code>{outputDir}</code> output directory &middot;
    <code>{outputBaseName}</code> base filename
  </div>
  <div class="btn-row">
    <button class="btn" id="save-btn" onclick="saveScript()">Save</button>
    <button class="btn btn-secondary" id="reset-btn" onclick="resetScript()">Reset to Default</button>
    <span class="saved-msg" id="saved-msg">Saved</span>
  </div>
</div>

<!-- ── Diff view (shows when script differs from default) ── -->
<div class="diff-section" id="diff-section" style="display:none">
  <div class="diff-header" onclick="toggleDiff()">
    <span class="chevron" id="diff-chevron">&#9654;</span>
    <span>Changes from default</span>
  </div>
  <div class="diff-body" id="diff-body"></div>
</div>

<!-- ═══════════════════════════════════════════════════════════════════════ -->

<script>
const vscode = acquireVsCodeApi();

let currentTargetId = '';
let currentDefault = '';
let savedTimer = null;
let elabCurrentDefault = '';
let elabSavedTimer = null;
// Unsaved-edit flags: while set, state pushes from the extension host must
// not overwrite the corresponding textarea (any clash-toolkit config change
// re-sends state, e.g. toggling the out-of-context checkbox).
let editorDirty = false;
let elabEditorDirty = false;

// ── Target change ───────────────────────────────────────────────────────

document.getElementById('target-select').addEventListener('change', function() {
  vscode.postMessage({ type: 'changeTarget', targetId: this.value });
});

document.getElementById('out-of-context').addEventListener('change', function() {
  vscode.postMessage({ type: 'changeOutOfContext', outOfContext: this.checked });
});

// ── Script editing ──────────────────────────────────────────────────────

const editor = document.getElementById('script-editor');

editor.addEventListener('input', function() {
  editorDirty = true;
  updateModifiedState();
});

// Allow Tab key to insert spaces in the textarea
editor.addEventListener('keydown', function(e) {
  if (e.key === 'Tab') {
    e.preventDefault();
    const start = this.selectionStart;
    const end = this.selectionEnd;
    this.value = this.value.substring(0, start) + '  ' + this.value.substring(end);
    this.selectionStart = this.selectionEnd = start + 2;
    editorDirty = true;
    updateModifiedState();
  }
});

function updateModifiedState() {
  const isModified = editor.value.trim() !== currentDefault.trim();
  document.getElementById('modified-badge').classList.toggle('hidden', !isModified);

  // Live diff update
  if (isModified) {
    renderDiffFromTexts(currentDefault, editor.value);
    document.getElementById('diff-section').style.display = '';
  } else {
    document.getElementById('diff-section').style.display = 'none';
  }
}

function saveScript() {
  vscode.postMessage({
    type: 'saveScript',
    targetId: currentTargetId,
    script: editor.value,
  });
  // "Saved" is flashed when the host confirms with a saveResult message —
  // not optimistically, since the config write can fail.
}

function resetScript() {
  editorDirty = false;
  vscode.postMessage({ type: 'resetScript', targetId: currentTargetId });
}

// ── Elaboration script editing ──────────────────────────────────────────

const elabEditor = document.getElementById('elab-script-editor');

elabEditor.addEventListener('input', function() {
  elabEditorDirty = true;
  updateElabModifiedState();
});

elabEditor.addEventListener('keydown', function(e) {
  if (e.key === 'Tab') {
    e.preventDefault();
    const start = this.selectionStart;
    const end = this.selectionEnd;
    this.value = this.value.substring(0, start) + '  ' + this.value.substring(end);
    this.selectionStart = this.selectionEnd = start + 2;
    elabEditorDirty = true;
    updateElabModifiedState();
  }
});

function updateElabModifiedState() {
  const isModified = elabEditor.value.trim() !== elabCurrentDefault.trim();
  document.getElementById('elab-modified-badge').classList.toggle('hidden', !isModified);
  if (isModified) {
    renderElabDiffFromTexts(elabCurrentDefault, elabEditor.value);
    document.getElementById('elab-diff-section').style.display = '';
  } else {
    document.getElementById('elab-diff-section').style.display = 'none';
  }
}

function saveElaborationScript() {
  vscode.postMessage({
    type: 'saveElaborationScript',
    script: elabEditor.value,
  });
  // "Saved" is flashed on host confirmation (saveResult), not optimistically.
}

function resetElaborationScript() {
  elabEditorDirty = false;
  vscode.postMessage({ type: 'resetElaborationScript' });
}

function toggleElabDiff() {
  const body = document.getElementById('elab-diff-body');
  const chevron = document.getElementById('elab-diff-chevron');
  const visible = body.classList.toggle('visible');
  chevron.classList.toggle('open', visible);
}

function renderElabDiff(diff) {
  const body = document.getElementById('elab-diff-body');
  body.innerHTML = '';
  for (const line of diff) {
    const div = document.createElement('div');
    if (line.kind === 'added') {
      div.className = 'diff-added';
      div.textContent = '+ ' + line.text;
    } else if (line.kind === 'removed') {
      div.className = 'diff-removed';
      div.textContent = '- ' + line.text;
    } else {
      div.textContent = '  ' + line.text;
    }
    body.appendChild(div);
  }
}

function renderElabDiffFromTexts(defaultText, userText) {
  const a = defaultText.split('\\n');
  const b = userText.split('\\n');
  const maxLen = Math.max(a.length, b.length);
  const body = document.getElementById('elab-diff-body');
  body.innerHTML = '';
  for (let i = 0; i < maxLen; i++) {
    const div = document.createElement('div');
    if (i >= a.length) {
      div.className = 'diff-added';
      div.textContent = '+ ' + b[i];
    } else if (i >= b.length) {
      div.className = 'diff-removed';
      div.textContent = '- ' + a[i];
    } else if (a[i] !== b[i]) {
      const rem = document.createElement('div');
      rem.className = 'diff-removed';
      rem.textContent = '- ' + a[i];
      body.appendChild(rem);
      div.className = 'diff-added';
      div.textContent = '+ ' + b[i];
    } else {
      div.textContent = '  ' + a[i];
    }
    body.appendChild(div);
  }
}

// ── Diff rendering ──────────────────────────────────────────────────────

function toggleDiff() {
  const body = document.getElementById('diff-body');
  const chevron = document.getElementById('diff-chevron');
  const visible = body.classList.toggle('visible');
  chevron.classList.toggle('open', visible);
}

function renderDiff(diff) {
  const body = document.getElementById('diff-body');
  body.innerHTML = '';
  for (const line of diff) {
    const div = document.createElement('div');
    if (line.kind === 'added') {
      div.className = 'diff-added';
      div.textContent = '+ ' + line.text;
    } else if (line.kind === 'removed') {
      div.className = 'diff-removed';
      div.textContent = '- ' + line.text;
    } else {
      div.textContent = '  ' + line.text;
    }
    body.appendChild(div);
  }
}

// Lightweight client-side diff for live updates while typing.
// The full LCS diff is sent from the extension when state loads.
function renderDiffFromTexts(defaultText, userText) {
  const a = defaultText.split('\\n');
  const b = userText.split('\\n');
  // Simple line-by-line comparison (not LCS, but fast for live typing)
  const maxLen = Math.max(a.length, b.length);
  const body = document.getElementById('diff-body');
  body.innerHTML = '';
  for (let i = 0; i < maxLen; i++) {
    const div = document.createElement('div');
    if (i >= a.length) {
      div.className = 'diff-added';
      div.textContent = '+ ' + b[i];
    } else if (i >= b.length) {
      div.className = 'diff-removed';
      div.textContent = '- ' + a[i];
    } else if (a[i] !== b[i]) {
      const rem = document.createElement('div');
      rem.className = 'diff-removed';
      rem.textContent = '- ' + a[i];
      body.appendChild(rem);
      div.className = 'diff-added';
      div.textContent = '+ ' + b[i];
    } else {
      div.textContent = '  ' + a[i];
    }
    body.appendChild(div);
  }
}

// ── State from extension host ───────────────────────────────────────────

window.addEventListener('message', event => {
  const msg = event.data;
  if (msg.type === 'state') {
    renderState(msg);
  } else if (msg.type === 'tools') {
    renderTools(msg.tools);
  } else if (msg.type === 'toolsLoading') {
    setToolsLoading();
  } else if (msg.type === 'saveResult') {
    handleSaveResult(msg);
  }
});

// Flash the outcome of a save once the extension host has actually
// persisted (or failed to persist) the value.
function handleSaveResult(msg) {
  const isElab = msg.section === 'elab';
  const el = document.getElementById(isElab ? 'elab-saved-msg' : 'saved-msg');
  if (msg.ok) {
    if (isElab) { elabEditorDirty = false; } else { editorDirty = false; }
    el.textContent = 'Saved';
    el.classList.remove('error');
  } else {
    el.textContent = 'Save failed';
    el.classList.add('error');
  }
  el.classList.add('show');
  if (isElab) {
    clearTimeout(elabSavedTimer);
    elabSavedTimer = setTimeout(() => el.classList.remove('show'), 2000);
  } else {
    clearTimeout(savedTimer);
    savedTimer = setTimeout(() => el.classList.remove('show'), 2000);
  }
}

// ── Tools section ───────────────────────────────────────────────────────

function refreshTools() {
  vscode.postMessage({ type: 'refreshTools' });
}

function setToolsLoading() {
  const btn = document.getElementById('refresh-tools-btn');
  btn.classList.add('spinning');
  btn.disabled = true;
}

function renderTools(tools) {
  const btn = document.getElementById('refresh-tools-btn');
  btn.classList.remove('spinning');
  btn.disabled = false;

  const list = document.getElementById('tools-list');
  list.innerHTML = '';
  for (const tool of tools) {
    const row = document.createElement('div');
    row.className = 'tool-row';

    const status = document.createElement('div');
    status.className = 'tool-status ' + (tool.available ? 'ok' : 'missing');
    status.textContent = tool.available ? '✓' : '✗';
    status.setAttribute('aria-label', tool.available ? 'available' : 'missing');
    row.appendChild(status);

    const info = document.createElement('div');
    info.className = 'tool-info';
    const name = document.createElement('div');
    name.className = 'tool-name';
    name.textContent = tool.label;
    info.appendChild(name);
    if (tool.version) {
      const detail = document.createElement('div');
      detail.className = 'tool-detail';
      detail.textContent = tool.version;
      detail.title = tool.version;
      info.appendChild(detail);
    } else if (tool.error) {
      const detail = document.createElement('div');
      detail.className = 'tool-detail';
      detail.textContent = tool.error;
      detail.title = tool.error;
      info.appendChild(detail);
    }
    row.appendChild(info);

    const pathCell = document.createElement('div');
    if (tool.toolPath) {
      pathCell.className = 'tool-path';
      pathCell.textContent = tool.toolPath;
      pathCell.title = tool.toolPath;
    } else {
      pathCell.className = 'tool-path missing-path';
      pathCell.textContent = 'not on PATH';
    }
    row.appendChild(pathCell);

    const infoIcon = document.createElement('span');
    infoIcon.className = 'info-icon';
    infoIcon.tabIndex = 0;
    infoIcon.setAttribute('aria-label', tool.label + ' info');
    const iconGlyph = document.createElement('span');
    iconGlyph.textContent = 'i';
    infoIcon.appendChild(iconGlyph);
    const tooltip = document.createElement('span');
    tooltip.className = 'info-tooltip';
    tooltip.textContent = tool.description;
    infoIcon.appendChild(tooltip);
    row.appendChild(infoIcon);

    list.appendChild(row);
  }
}

function renderState(msg) {
  // Switching target deliberately replaces the script editor's content, so
  // unsaved edits for the previous target are discarded in that case only.
  const targetChanged = currentTargetId !== '' && currentTargetId !== msg.targetId;

  // Populate target dropdown
  const sel = document.getElementById('target-select');
  if (sel.options.length === 0 || currentTargetId !== msg.targetId) {
    sel.innerHTML = '';
    for (const t of msg.targets) {
      const opt = document.createElement('option');
      opt.value = t.id;
      opt.textContent = t.label;
      sel.appendChild(opt);
    }
  }
  sel.value = msg.targetId;
  currentTargetId = msg.targetId;

  // Out-of-context checkbox
  document.getElementById('out-of-context').checked = !!msg.outOfContext;

  // Script editor: show custom script if set, otherwise default. Never
  // overwrite unsaved edits — any clash-toolkit config change re-sends
  // state (e.g. toggling the checkbox above), and clobbering the textarea
  // would silently destroy the user's work-in-progress.
  currentDefault = msg.defaultScript;
  const hasCustom = msg.customScript && msg.customScript.length > 0;
  if (!editorDirty || targetChanged) {
    editorDirty = false;
    editor.value = hasCustom ? msg.customScript : msg.defaultScript;

    // Modified badge
    document.getElementById('modified-badge').classList.toggle('hidden', !hasCustom);

    // Diff section
    if (hasCustom && msg.diff && msg.diff.length > 0) {
      document.getElementById('diff-section').style.display = '';
      renderDiff(msg.diff);
    } else {
      document.getElementById('diff-section').style.display = 'none';
    }
  } else {
    // Keep the user's edits; refresh badge/diff against the (possibly new)
    // default script.
    updateModifiedState();
  }

  // Elaboration editor — same dirty rule.
  elabCurrentDefault = msg.elaborationDefault || '';
  const hasElabCustom = msg.elaborationCustom && msg.elaborationCustom.length > 0;
  if (!elabEditorDirty) {
    elabEditor.value = hasElabCustom ? msg.elaborationCustom : elabCurrentDefault;
    document.getElementById('elab-modified-badge').classList.toggle('hidden', !hasElabCustom);
    if (hasElabCustom && msg.elaborationDiff && msg.elaborationDiff.length > 0) {
      document.getElementById('elab-diff-section').style.display = '';
      renderElabDiff(msg.elaborationDiff);
    } else {
      document.getElementById('elab-diff-section').style.display = 'none';
    }
  } else {
    updateElabModifiedState();
  }
}

// Request initial state
vscode.postMessage({ type: 'ready' });
</script>
</body>
</html>`;
	}
}
