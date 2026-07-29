# Debugging

## Log Channels

| Channel | Where | Content |
|---------|-------|---------|
| **Clash Synthesis** | Output panel | Extension operations, Clash/Yosys/nextpnr invocations |
| **File log** | `.clash/debug.log` in the workspace | Same detail, persisted to disk |
| **Extension Host** | Output panel | Extension lifecycle events |
| **Developer Tools Console** | Help → Toggle Developer Tools | Low-level errors, stack traces |

The file log is the one to reach for after a crash: the previous session is
rotated to `.clash/debug.log.old` on activation, so evidence survives a restart
that would otherwise clear the Output panel.

## Attaching the Debugger

1. Open the extension project in VS Code.
2. Press **F5** (launch config: *Run Extension*).
3. Reproduce the problem in the Extension Development Host window.
4. When an exception is thrown the debugger breaks at the throw site.

### Minimal launch (disable other extensions)

```jsonc
{
  "name": "Extension (Minimal)",
  "type": "extensionHost",
  "request": "launch",
  "args": [
    "--extensionDevelopmentPath=${workspaceFolder}",
    "--disable-extensions"
  ]
}
```

## Common Crash Causes

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| Crash on function detection | HLS not running | Wait for HLS to initialise; run `cabal build` first |
| File-system errors | Missing write permission or full disk | Check workspace permissions |
| High CPU then crash | Possible infinite loop (unlikely) | Check `htop`; file an issue |
| Memory growth | Large synthesis output | Restart VS Code; close heavy extensions |

## NixOS-Specific Issues

- **Node.js library mismatch:** VS Code Server bundles its own Node binary. On NixOS, verify with `ldd ~/.vscode-server/bin/*/node`.
- **Extension Host restarts:** Often caused by HLS or other extensions polling. Disable unrelated extensions to isolate.
- **Missing shared libraries:** Use `nix-ld` or `vscode-fhs` from Nixpkgs.

## Collecting a Bug Report

1. Stack trace from Developer Tools Console
2. Last entries from the *Clash Synthesis* output channel
3. Extension Host log
4. Steps to reproduce
5. `code --version` output
