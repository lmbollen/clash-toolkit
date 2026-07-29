# Configuration

All settings live under `clash-toolkit` in VS Code settings.

| Setting | Default | Description |
|---------|---------|-------------|
| `yosysCommand` | `yosys` | Command to invoke Yosys |
| `synthesisTarget` | `generic` | Target FPGA family for Yosys synthesis. One of `generic`, `ice40`, `ecp5`, `xilinx`, `gowin`, `quicklogic`, `sf2`. Also selects the nextpnr binary for Place & Route |
| `outOfContext` | `false` | Out-of-context synthesis: when enabled, each component in a multi-component design is synthesized standalone with its own diagram + utilization stats |
| `pnrTargetFrequencyMHz` | *(unset)* | Target frequency passed to nextpnr as `--freq`. Used only when no SDC file is present — an SDC constraint takes precedence. Leave blank to let nextpnr report Fmax without a target |
| `pnrWriteRoutedSvg` | `true` | Write a routed-layout SVG alongside the nextpnr output, showing where the design landed on the fabric |
| `elaborationScript` | *(built-in)* | Custom Yosys script for the elaboration stage |
| `synthesisScript.<target>` | *(built-in)* | Custom Yosys script per target — one setting for each of the seven targets above |

## Custom Yosys Scripts

Every synthesis target ships a built-in Yosys script, and each can be overridden:
`elaborationScript` for the elaboration stage, and `synthesisScript.generic`,
`synthesisScript.ice40`, `synthesisScript.ecp5`, `synthesisScript.xilinx`,
`synthesisScript.gowin`, `synthesisScript.quicklogic`, and `synthesisScript.sf2`
for synthesis. An empty string means "use the built-in default", so clearing a
setting reverts it.

Scripts are expanded with these placeholders before Yosys runs:

| Placeholder | Expands to |
|-------------|-----------|
| `{files}` | The Verilog files to read |
| `{topModule}` | The top module name |
| `{outputDir}` | The stage's output directory |
| `{outputBaseName}` | Base name for generated output files |

The easiest way to edit these is **Clash: Open Settings** (the gear icon in the
sidebar), which shows the active script for the selected target and an inline
diff against the default so you can see exactly what you changed.

## Out-of-Context Synthesis

### Disabled (default)

The whole design is synthesized as a single netlist with target-specific commands (e.g. `synth_ecp5`). Produces one JSON netlist and one synthesized Verilog file. This matches what nextpnr consumes for place-and-route.

### Enabled

Each component in the dependency graph is synthesized independently, producing:
- An `.il` (RTLIL) file per module
- A `.json` netlist per module
- An `.svg` circuit diagram per module
- Per-module statistics (cell count, wire count, logic depth)

Useful for inspecting and comparing the synthesis result of each sub-module individually. The Place & Route command always uses the whole-design path regardless of this setting; nextpnr needs a merged netlist.

## Elaboration

The `Clash: Elaborate` command always runs per-module — its purpose is to expose what Clash produced *before* technology mapping, so each component's hierarchy is preserved and rendered with sub-component instances shown as boxes. This setting does not affect elaboration.

## Clash Invocation

The extension invokes Clash via: `cabal run clash-synth:clash --`

This runs the `clash` executable from the synthesis cabal project at `.clash/synth-project/`, which depends on your package through cabal. This ensures all transitive dependencies are resolved correctly.

The synth project is created and updated automatically — you don't need to manage it.
