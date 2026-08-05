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

Used by **Synthesize** when `outOfContext` is enabled. Each component in the dependency graph is synthesized independently with its own directory under `per-module/<name>/`, from the `outOfContextScript` template — its own setting, separate from `synthesisScript.<target>`, since this path issues no `synth_*` command at all:

1. The component's **direct** dependencies are read with `read_verilog -lib`,
   which keeps their port interfaces and discards their bodies. They become
   black boxes, so `hierarchy -check` passes without their contents being
   elaborated. One level of stubs is enough however deep the design goes: a
   black box has no body, so the components *it* instantiates are never
   referenced
2. The component is optimized standalone — `proc`, `opt -purge`,
   `memory -nomap`, `opt`, with **no** `flatten`. There is also **no**
   technology mapping: a full `synth` per component hangs on components with
   large block RAMs (`memory_map` + `abc` on the resulting flip-flop array), so
   the target's `synth_*` command and any custom script are not used on this
   path
3. Black-box instances get `setattr -set keep 1 t:<dep>` right after `proc`.
   Without it, `opt`/`opt_clean`/`clean` delete any instance whose outputs
   happen to be unused, and the component would silently vanish from the
   diagram and the cell counts
4. Each component produces `.il` (RTLIL), `.json` (netlist), `.svg` (diagram),
   and per-component statistics whose cells are generic, cover only that
   component's own logic (one opaque cell per sub-component), and are not
   comparable with a whole-design run
5. The component graph is written to `per-module/hierarchy.json` so both sidebar views can present the results as the design hierarchy, without either view having to read it back out of a netlist

Because a component's run needs its dependencies' *Verilog* and never their
*results*, the components have no ordering constraint between them and are all
dispatched concurrently — see [Concurrency](#concurrency) below.

### Per-module elaboration (`elaboratePerModule`)

Always used by **Elaborate**. Same per-module driver as `synthesizePerModule`, but dependencies are read in full (not `-lib`) and the script body is `proc + opt_clean` (no flatten, no tech mapping). The netlist therefore carries the real sub-module definitions, so its diagram can be drilled into; instances still appear as boxes rather than being expanded.

### Concurrency

Both per-module flows run through `mapPool`, which keeps up to
`perModuleConcurrency()` Yosys processes in flight. That resolves the
`clash-toolkit.yosysJobs` setting through the shared `resolveJobCount` in
`parallelism.ts`:
`auto` means one per core minus one for the editor, capped at 8 (past that the
runs contend for memory rather than CPU, and a design with a large block RAM can
hold a lot of it per process); an explicit number is honoured uncapped; and the
result never exceeds the component count. The same helper backs `cabalJobs`,
`ghcJobs` and `nextpnrThreads`.

Results are returned in **input order** regardless of completion order, so
`moduleResults` stays in the manifest parser's dependency order (leaves first,
top last) and the "combined" result at the end of the array is still the top
component. Cancelling stops further components being scheduled; runs already in
flight are killed through the abort signal inside `runYosysScript`.

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
