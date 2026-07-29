# Circuit Diagrams

The extension renders circuit diagrams as SVGs via Yosys's `show` command and Graphviz's `dot`, then opens them with VS Code's built-in image preview editor.

## Viewing diagrams

- After **Clash: Elaborate**, the diagram opens automatically and one diagram is produced per module.
- After **Clash: Synthesize**, the diagram opens automatically. With `outOfContext` enabled, each module gets its own diagram; otherwise a single whole-design diagram is rendered.
- Click the diagram icon next to any module in the **Synthesis Results** sidebar (or the **Run History** view) to (re-)open that module's diagram.

## Per-module diagrams

The **Elaborate** command always produces one diagram per component. The top component's diagram preserves the hierarchy: sub-component instances are rendered as boxes rather than expanded into gates. Each sub-component has its own diagram showing its own internals.

For **Synthesize**, set `clash-toolkit.outOfContext` to `true` to get the same per-module breakdown, with each module synthesized standalone (so you also see individual utilization numbers).

## Troubleshooting

### "Diagram not available — Graphviz `dot` may have failed"

Yosys wrote the `.dot` file but `dot` could not convert it. Most commonly this happens when synthesizing a large design whole-design: the technology-mapped netlist has too many gates for `dot` to lay out. Either enable `outOfContext` to render sub-modules individually, or use **Elaborate** to view a higher-level diagram.

### "No diagram rendered — install Graphviz"

`dot` could not be resolved, either from the PATH or from a managed install.
Either accept the extension's offer to download it (see
[Getting Started](getting-started.md)), run **Clash: Install Toolchain** and tick
**Graphviz dot**, or install it yourself (`nix-shell -p graphviz`,
`apt install graphviz`, …) and re-run. Synthesis itself still succeeds without
`dot` — only the diagram is missing.

### Design was optimized away

If Yosys's optimization passes removed everything (e.g. constant outputs), the diagram will be empty. Check `.clash/<module>/03-yosys/yosys.log`.
