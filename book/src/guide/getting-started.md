# Getting Started

## Prerequisites

### You provide: the Haskell toolchain

These must be available in your environment (e.g. via `nix develop`, `ghcup`, or
your system package manager). The extension cannot install them for you.

| Tool | Purpose |
|------|---------|
| **Cabal** (drives `cabal run clash-synth:clash --`) | Builds your project and invokes Clash to generate Verilog |
| **Haskell Language Server** | Function detection and type information |

### The extension provides: the EDA tools

Yosys and the `nextpnr-*` binaries do **not** have to be on your PATH. When a
command needs one that is missing, the extension offers to download a
self-contained [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build)
build into its own private storage and use it from there.

| Tool | Purpose | Needed for |
|------|---------|-----------|
| **Yosys** | Logic synthesis and statistics | Elaborate, Synthesize, Place & Route |
| **nextpnr-ecp5** | Place & route for Lattice ECP5 | Place & Route, `ecp5` target |
| **nextpnr-ice40** | Place & route for Lattice iCE40 | Place & Route, `ice40` target |
| **nextpnr-himbaechel** | Place & route for Gowin | Place & Route, `gowin` target |

Anything already on your PATH is used as-is — a managed download is only ever
offered for tools that are missing, and only for the ones you tick in the
prompt. Run **Clash: Check Toolchain** to probe cabal, Yosys, and
`nextpnr-ecp5`, or **Clash: Install Toolchain** to manage the download
explicitly. The iCE40 and Gowin binaries are checked when you actually run
Place & Route for those targets.

> The suite is a single archive (335–730 MB depending on platform) pinned to one
> release, so the first download takes a while regardless of how many tools you
> select.

Schematic diagrams need no tool at all: they are rendered by
[netlistsvg](https://github.com/nturley/netlistsvg), which is bundled with the
extension. See [Circuit Diagrams](circuit-diagrams.md).

See [Managed Toolchain](managed-toolchain.md) for the full details.

## Quick Start

1. Open a Clash project in VS Code (one that builds with `cabal build`).
2. Make sure HLS is running (install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)).
3. Open a `.hs` file containing monomorphic functions.
4. Either:
   - Run **Clash: Detect Functions** from the command palette and pick a function, or
   - Place your cursor on a monomorphic function and press `Ctrl+.` to use a code action.

## How Synthesis Works

1. **Function detection** — HLS provides document symbols and hover types. The type analyser checks whether a function is monomorphic (all concrete types, no type variables).

2. **Wrapper generation** — For a function like `topEntity` in module `Example.Project`, the extension generates a wrapper module under `.clash/synth-project/src/`:

   ```haskell
   {-# OPTIONS_GHC -Wno-orphans #-}
   module ClashSynth_TopEntity where

   import Clash.Prelude
   import qualified Example.Project

   topEntity = Example.Project.topEntity

   {-# ANN topEntity
     (Synthesize
       { t_name = "top_entity"
       , t_inputs = [ PortName "CLK"
                    , PortName "RST"
                    , PortName "EN"
                    , PortName "IND"
                    ]
       , t_output = PortName "OUT"
       }) #-}
   {-# OPAQUE topEntity #-}
   ```

   Compound types like `DiffClock` are handled automatically with `PortProduct` annotations.

3. **Synthesis cabal project** — The extension maintains a cabal project at `.clash/synth-project/` that depends on your package. This lets Clash resolve all transitive dependencies correctly.

4. **Clash compilation** — Runs `cabal run clash-synth:clash -- ClashSynth_TopEntity --verilog` inside the synth project.

5. **Yosys synthesis** — Runs Yosys with a script chosen by the
   `synthesisTarget` setting (`generic`, `ice40`, `ecp5`, `xilinx`, `gowin`,
   `quicklogic`, `sf2`). Each target's script is editable — see
   [Configuration](configuration.md). For multi-component designs with
   out-of-context mode enabled, each component is synthesized standalone with a
   fixed generic script instead — see [Configuration](configuration.md#out-of-context-synthesis).

6. **Place & route** — Runs nextpnr for the selected device and reports timing and utilisation. The target frequency is parsed from Clash-generated SDC files.
