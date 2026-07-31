# Synthesis Pipeline

## Wrapper Generation

The `CodeGenerator` creates a Clash wrapper module that re-exports the user's function as `topEntity` with a `Synthesize` annotation. The wrapper is written to `.clash/synth-project/src/`.

Port names are derived heuristically from the type signature:

| Type pattern | Port annotation |
|-------------|----------------|
| `Clock …` | `PortName "CLK"` |
| `DiffClock …` | `PortProduct "CLK" [PortName "p", PortName "n"]` |
| `Reset …` | `PortName "RST"` |
| `Enable …` | `PortName "EN"` |
| Anything else | `PortName "INA"`, `PortName "INB"`, … |
| Output | `PortName "OUT"` |

The synthesis cabal project (`ensureSynthProject`) maintains `cabal.project`, `clash-synth.cabal`, and `bin/Clash.hs`. It discovers the user's cabal project via `findCabalProject` and adds it as a dependency.

## Clash Compilation

`ClashCompiler.compileToVerilog()` runs:

```
cabal run clash-synth:clash -- <ModuleName> --verilog
```

with `--project-dir` and `--project-file` flags when a user cabal project is detected. The compiler parses stdout/stderr for errors and warnings, and locates the generated Verilog and `clash-manifest.json`.

## Yosys Synthesis

The runner exposes three flows, all sharing the same Yosys child-process plumbing:

### Whole-design (`synthesize`)

Default for **Synthesize** and always used for **Place & Route**. Generates a single Yosys script that reads every Verilog file, elaborates the hierarchy, runs target-specific synthesis (`synth_ecp5`, `synth_ice40`, etc.), and writes outputs (synthesized Verilog, netlist JSON, statistics, diagram).

### Per-module synthesis (`synthesizePerModule`)

Used by **Synthesize** when `outOfContext` is enabled. Each component in the dependency graph is synthesized independently with its own directory under `per-module/<name>/`:

1. Dependencies' Verilog files are read (not synthesized) so `hierarchy -check` passes
2. The component is flattened and optimized standalone — `proc`, `flatten`,
   `opt -purge`, `memory -nomap`, `opt`. There is **no** technology mapping: a
   full `synth` per component hangs on components with large block RAMs
   (`memory_map` + `abc` on the resulting flip-flop array), so the target's
   `synth_*` command and any custom script are not used on this path
3. Each component produces `.il` (RTLIL), `.json` (netlist), `.svg` (diagram), and per-component statistics whose cells are generic, include the component's descendants, and are not comparable with a whole-design run
4. The component graph is written to `per-module/hierarchy.json` so both sidebar views can present the results as the design hierarchy — a flattened netlist can no longer be asked what it instantiates

### Per-module elaboration (`elaboratePerModule`)

Always used by **Elaborate**. Same per-module loop as `synthesizePerModule`, but the script body is `proc + opt_clean` (no flatten, no tech mapping). The netlist therefore keeps the hierarchy, and the diagram is rendered for the component itself — sub-component instances appear as boxes rather than being expanded.

## Diagram Rendering

Yosys writes no diagram of its own; `netlist-renderer.ts` renders one from the
JSON netlist the script already emits, using the bundled
[netlistsvg](https://github.com/nturley/netlistsvg) library (ELK for layout, an
SVG skin for the cell symbols). The netlist's `top` attribute decides which
module is drawn, and the per-module flows override it with the component name so
the netlist's dependency modules don't win.

`netlist-diagram.ts` orchestrates that:

- renders in a forked child process (`out/netlist-renderer.js` run as a script),
  because ELK layout is CPU-bound JavaScript that would otherwise block the
  extension host for seconds on a large design;
- fire-and-forget — synthesis resolves as soon as Yosys exits, and each render
  is registered by its target path so `waitForSvg()` can join it at the point of
  use (opening a diagram) instead of at the point of creation;
- degrades to a warning in the output channel, never a failed synthesis; if the
  child cannot be spawned at all it falls back to rendering in-process.

## nextpnr Place & Route

`NextpnrRunner.placeAndRoute()` builds command-line arguments for the selected FPGA family and device, runs nextpnr, and parses timing and utilization from stdout.

The manifest-derived target frequency is passed via `--freq` when the top entity has a clock.
