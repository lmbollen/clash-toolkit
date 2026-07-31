# Configuration

All settings live under `clash-toolkit` in VS Code settings, grouped into
**Synthesis**, **Place & Route**, **Build**, **Toolchain** and **Yosys Scripts**.

| Setting | Default | Description |
|---------|---------|-------------|
| `synthesisTarget` | `generic` | Target FPGA family for Yosys synthesis. One of `generic`, `ice40`, `ecp5`, `xilinx`, `gowin`, `quicklogic`, `sf2`. Also selects the nextpnr binary for Place & Route |
| `outOfContext` | `false` | Out-of-context synthesis: when enabled, each component in a multi-component design is synthesized standalone with its own diagram + utilization stats |
| `pnrWriteRoutedSvg` | `true` | Write a routed-layout SVG alongside the nextpnr output, showing where the design landed on the fabric |
| `cabalJobs` | `auto` | Packages cabal may build at once (`--jobs`). `auto` is one job per core; a number caps it; `1` builds sequentially |
| `ghcJobs` | *(unset)* | Modules GHC may compile in parallel within a package (`--ghc-options=-jN`). Leave blank to keep GHC single-threaded |
| `toolCommands` | `{}` | Per-tool command overrides, keyed by tool name (`cabal`, `yosys`, `nextpnr-*`) |
| `elaborationScript` | *(built-in)* | Custom Yosys script for the elaboration stage |
| `synthesisScript.<target>` | *(built-in)* | Custom Yosys script per target — one setting for each of the seven targets above |

**The P&R target frequency is not a setting.** It is the period of the top
entity's clock domain, read from the Clash manifest on every run — see
[Timing Analysis](timing-analysis.md#target-frequency).

## Tool Commands

The extension finds its tools on `PATH`, and offers to download and manage them
(**Clash: Install Toolchain**) when they aren't there. `toolCommands` is the
escape hatch for what neither route reaches — a binary somewhere unusual, or a
wrapper that has to run in front of it:

```json
"clash-toolkit.toolCommands": {
  "yosys": "/opt/oss-cad-suite/bin/yosys",
  "cabal": "nix run nixpkgs#cabal-install --",
  "nextpnr-ecp5": "wsl nextpnr-ecp5"
}
```

Keys are tool names: `cabal`, `yosys`, `nextpnr-ecp5`, `nextpnr-ice40`,
`nextpnr-himbaechel`. Values are split on spaces, so anything after the first
token becomes leading arguments; quote a path that contains spaces. An entry you
don't set means "run the tool by name", which is what leaves detection and the
managed download in charge.

The same command is used for the pre-flight toolchain probe and for the actual
run, so "the check passes but synthesis spawns something else" cannot happen.

> `yosysCommand` was the single-tool ancestor of this setting. It is deprecated
> but still honoured; move it to `toolCommands` under the key `yosys`.

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

> **Where scripts do not apply.** These scripts drive the whole-design path only.
> The per-component paths build their own fixed script, so your script is not used
> when a design with more than one component is elaborated, or synthesized with
> `outOfContext` enabled. See [Out-of-Context Synthesis](#out-of-context-synthesis)
> and [Elaboration](#elaboration) below.

## Out-of-Context Synthesis

### Disabled (default)

The whole design is synthesized as a single netlist with target-specific commands (e.g. `synth_ecp5`). Produces one JSON netlist and one synthesized Verilog file. This matches what nextpnr consumes for place-and-route.

### Enabled

Each component in the dependency graph is synthesized independently, producing:
- An `.il` (RTLIL) file per component
- A `.json` netlist per component
- An `.svg` circuit diagram per component
- Per-component statistics (cell count, wire count, logic depth)

Useful for inspecting and comparing each component's synthesis result on its own.
The Place & Route command always uses the whole-design path regardless of this
setting; nextpnr needs a merged netlist.

**What actually runs.** Each component is synthesized *out of context* with a
fixed script — `proc`, `flatten`, `opt -purge`, `memory -nomap`, `opt` — and
**no technology mapping**. No `synth_*` command runs, so:

- **The target and your custom script do not apply here.** The cells counted are
  generic Yosys cells (`$add`, `$dffe`, `$mem_v2`, …), not the target's `LUT4` /
  `TRELLIS_FF` / block RAMs. On the test design, whole-design `ecp5` synthesis
  reports 173 cells (`CCU2C`, `LUT4`, `TRELLIS_FF`) while the same design's
  components report 15 and 14 generic cells.
- **A component's figures include its descendants.** `flatten` inlines the
  components it instantiates, so per-component numbers overlap and adding them up
  means nothing.
- **Nothing is optimized against the parent.** A component never sees the design
  above it, so constants the parent would feed in aren't propagated and logic the
  parent leaves unused isn't pruned.

Use the numbers to compare components with each other, not to predict
whole-design utilization — for that, run with this setting off.

The fixed script is deliberate: a full `synth` per component hangs indefinitely
on components containing large block RAMs, because `memory_map` plus `abc` cannot
finish on the resulting flip-flop array. Keeping memories as `$mem` cells avoids
that.

The extension says so where those numbers appear: out-of-context rows in
**Results** and **History** are tagged `out of context` with the caveat in their
tooltip, the Results section header names the mode, and the output channel
repeats it at the start of the run.

**Hierarchy is preserved in the view.** Although each component is synthesized
standalone (and its netlist flattened), the results are still presented as the
design's hierarchy — the top component at the root, the components it
instantiates nested beneath it — so the view reads the same whether or not this
setting is on. The graph comes from the Clash manifest and is recorded in
`per-module/hierarchy.json`, which is also what lets History rebuild the same
nesting for a past run.

## Elaboration

The `Clash: Elaborate` command always runs per-component — its purpose is to
expose what Clash produced *before* technology mapping, so each component's
hierarchy is preserved and rendered with sub-component instances shown as boxes.
The `outOfContext` setting does not affect elaboration.

Unlike out-of-context synthesis, elaboration does **not** flatten: each component
is run through `proc` and `opt_clean` only, so its statistics and diagram cover
that component alone and its sub-components stay visible as instances.

`elaborationScript` applies to the whole-design path — a single-component design.
For a design with more than one component, the per-component script above is used
instead and a custom `elaborationScript` has no effect.

## Clash Invocation

The extension invokes Clash via: `cabal run --jobs=$ncpus clash-synth:clash --`

This runs the `clash` executable from the synthesis cabal project at `.clash/synth-project/`, which depends on your package through cabal. This ensures all transitive dependencies are resolved correctly.

The synth project is created and updated automatically — you don't need to manage it.

### Build parallelism

The first run of a project has to build its dependency tree, and cabal builds
one package at a time unless told otherwise. `cabalJobs` is what tells it
otherwise — it defaults to `auto`, which cabal spells `$ncpus`: as many
packages at once as you have cores. Set a number to leave headroom for the rest
of the machine, or `1` to go back to a sequential build. Changing it never
invalidates anything cabal has already built.

`cabalJobs` only parallelises *across* packages, so it cannot speed up the build
of a single large package — your own, typically. `ghcJobs` does that, by passing
`-jN` to GHC so it compiles that many modules at once. It is off by default for
one reason: GHC options are part of cabal's build plan, so turning it on (or off
again) changes every package's identity and rebuilds the whole plan once.
