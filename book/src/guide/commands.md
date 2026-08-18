# Commands

The main commands are available from the VS Code command palette
(`Ctrl+Shift+P`). Commands that act on a specific tree item are reachable only
from the **Clash Synthesis** sidebar — its view title bar and right-click
menus — and are hidden from the palette.

## Main flow

| Command | Description |
|---------|-------------|
| **Clash: Detect Functions** | Scan the current file for functions, show which are synthesisable |
| **Clash: Generate Verilog** | Generate a wrapper module and compile to Verilog with Clash |
| **Clash: Elaborate** | Clash → Yosys elaboration only (no tech mapping), one diagram per module |
| **Clash: Synthesize** | Full Clash → Yosys pipeline without place & route. Optional out-of-context mode for per-module diagrams |
| **Clash: Place & Route** | Full pipeline: Clash → Yosys → nextpnr |

## Toolchain

| Command | Description |
|---------|-------------|
| **Clash: Check Toolchain** | Probe every external tool (cabal, Yosys, nextpnr) and report what is reachable |
| **Clash: Install Toolchain** | Pick which EDA tools the extension should download and manage itself |
| **Clash: Install the Haskell Extension (for HLS)** | Reveal the Haskell extension in the Marketplace view. Function detection needs HLS, so this is also the action offered on the Functions row when the extension is missing |

## Sidebar

| Command | Where | Description |
|---------|-------|-------------|
| **Clash: Refresh** | Sidebar title bar | Re-read the active file's functions *and* the runs on disk |
| **Clash: Refresh Haskell Functions** | Palette | Re-scan the active file for functions only |
| **Clash: Go To Function** | Click a Functions item | Jump to the function's definition |
| **Clash: Open Settings** | Gear icon in the title bar | Open the settings panel (synthesis target and scripts) |
| **Clash: Open Synthesized Verilog** | Results item, inline icon | Open the Yosys-synthesized Verilog |
| **Clash: View Module Diagram** | Results item, inline icon | Open a module's schematic SVG |
| **Clash: View Component Diagram** | Click a sub-component row under a module | Open that component's own schematic (rendered on first open) |
| **Clash: Refresh Run History** | Palette | Re-read past runs from `.clash/` only |
| **Clash: Show Run in Synthesis Results** | Click a History item | Load a past run into the Results section |
| **Clash: Open Verilog (History)** | History item, inline icon | Open a past run's Verilog |
| **Clash: View Diagram (History)** | History item, inline icon | Open a past run's diagram |
| **Clash: Delete Run** | History run row, inline icon | Delete that run's output directory |
| **Clash: Delete Design History** | History design row, inline icon | Delete every run recorded for that design |
| **Clash: Clear Run History** | Trash icon on the History header | Delete the run history of every design |

The four main-flow commands (**Generate Verilog**, **Elaborate**, **Synthesize**,
**Place & Route**) also appear as icons in the sidebar title bar, so you can
drive the whole flow from the sidebar without the palette.

## Detect Functions

Scans the current Haskell file (or all open Haskell documents) using HLS document symbols and hover types. Shows a picker listing each function with its type signature, marking monomorphic functions with ✓ and polymorphic ones with ✗.

If you select a monomorphic function, you're offered the option to synthesize it immediately.

## Generate Verilog

An interactive command that:

1. Detects functions in the current file
2. Shows a picker with only synthesizable (monomorphic) functions
3. Generates a Clash wrapper module
4. Compiles to Verilog with Clash
5. Optionally runs Yosys synthesis

## Synthesize

Runs the full Clash compilation and Yosys synthesis pipeline without place & route. This is useful when you want to inspect synthesis results and circuit diagrams without targeting a specific FPGA.

Respects the `outOfContext` setting:
- **disabled (default)** — the whole design is synthesized as a single netlist
- **enabled** — each component is synthesized standalone (*out of context*), producing its own `.il` (RTLIL), `.json` (netlist), and `.svg` (diagram) plus per-component stats. This path runs a fixed generic script with no technology mapping, so the target does not apply, a component's figures include its descendants, and nothing is optimized against its parent — see [Configuration](configuration.md#out-of-context-synthesis)

Elaboration (`Clash: Elaborate`) always runs per-module regardless of this setting — its goal is to give a faithful per-component view of what Clash produced.

## Place & Route

The full FPGA implementation pipeline. After detecting and selecting a function:

1. Generates wrapper module
2. Compiles to Verilog with Clash
3. Synthesizes with Yosys for the configured target
4. Reads the top entity's target clock frequency from the Clash manifest
5. Runs the target's `nextpnr-*` binary with the selected device and package

The FPGA family comes from the `synthesisTarget` setting, **not** from a prompt.
Place & route is available for `ecp5`, `ice40`, and `gowin`; with any other
target (`generic`, `xilinx`, `quicklogic`, `sf2`) the command reports that P&R
is unavailable and stops, because no nextpnr binary is wired up for it.

Once the family is known you're prompted for a device, and then for a package if
that device offers a choice. The device list is family-specific — see
[Nextpnr Integration](../architecture/nextpnr-integration.md) for the full set.
