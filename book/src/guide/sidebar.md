# The Sidebar

The extension contributes a **Clash Synthesis** view to the activity bar: one
tree with three sections, covering the whole workflow — pick a function, inspect
what synthesis produced, and go back to earlier runs.

```
Clash Synthesis
  FUNCTIONS      ← what you can synthesize
    …

  RESULTS        ← what the last run produced
    …

  HISTORY        ← every previous run, on disk
    …
```

Each section header carries a status of its own: which file's functions are
listed, which run is loaded into Results, or why HLS has nothing to say. Hover a
header for the long form. Sections collapse independently and stay that way.

Headers are upper-cased and a blank row precedes each one, so a section reads as
a boundary rather than as one more row among its own contents. A tree has no
separator API, so those blank rows are real rows — inert ones, with nothing to
expand and no context menu, announced to screen readers as separators.

One view means one title bar. It holds the four main-flow actions —
**Generate Verilog**, **Elaborate**, **Synthesize**, **Place & Route** — plus
**Refresh** and **Open Settings**. Refresh re-reads everything that comes from
outside the extension: the active file's functions and the runs on disk.

Most of the extension's commands live here rather than in the command palette,
because they act on a specific tree item. See [Commands](commands.md) for the
full list.

## Functions

Shows the functions in the **currently active Haskell file**, split into two
expandable sections with counts:

- **Monomorphic (n)** — fully concrete types, so they can be synthesized
- **Polymorphic (n)** — greyed out; these cannot be synthesized directly

Each entry shows the function's type signature as its description, and hovering
gives the signature formatted as Haskell plus a note on why it can or cannot be
synthesized. **Clicking a function jumps to its definition.**

### When the list is empty

An empty list has several possible causes, so the view names the one that applies
instead of showing nothing:

| Row | Meaning | What to do |
|-----|---------|-----------|
| *Analyzing…* | The extension is analysing symbols HLS returned | Wait; it's quick |
| *No symbols from HLS yet* | HLS is reachable but returned nothing for this file | Usually it is still loading the project. The view re-checks when HLS next reports on the file; **Refresh** re-checks now |
| *HLS unavailable — Haskell extension not installed* | Function detection needs `haskell.haskell` | Click the row to open it in the Marketplace |
| *HLS unavailable — Haskell extension did not start* | It is installed but did not activate | Click the row to retry; its own output channel says why |
| *Open a Haskell file to see functions* | No Haskell file is active | Open one |
| *Monomorphic (0)* / *Polymorphic (0)* | HLS answered, and this file defines no top-level functions | Nothing — this is a real answer |

The distinction between the last two matters: "no symbols yet" is *not* a verdict.
While the Haskell Language Server loads a project it answers with nothing, which
looks exactly like a file with no functions, so the view refuses to claim either.

The extension starts the Haskell extension itself if it is installed but idle, and
re-checks automatically when HLS publishes diagnostics for the file it is showing —
that is the only signal another extension can observe, since HLS exposes no
readiness API. If a file's functions still don't appear after HLS has settled, use
**Refresh** in the title bar.

## Results

Populated by the most recent Elaborate, Synthesize, or Place & Route run.

Each synthesized module appears as a row showing `cells · wires · depth`, with a
green tick when it succeeded and a red error icon (plus the first error message)
when it did not. Expanding a module breaks it down by **cell type**, sorted by
count descending — this is the quickest way to see what a design actually
mapped to.

Two inline icons appear on a module row when the corresponding artefact exists:
open its **synthesized Verilog**, or open its **schematic diagram**.

After Place & Route, three extra sections are appended:

| Section | Contents |
|---------|----------|
| **Timing** | Max frequency, pre-route estimate (when it differs), critical-path delay, setup and hold slack, and whether constraints were **MET** or **MISSED** |
| **Utilization** | LUTs, registers, BRAM, DSP, IO — each as used / total with a percentage |
| **Critical Paths** | Each path as `from → to`, expandable into its individual steps |

Rows appear only when nextpnr actually reported that metric, so the exact set
varies by family and design. Utilization categories with a total of zero are
omitted rather than shown as `0 / 0`.

> **Critical Paths is capped at the five worst paths** to keep the tree usable on
> large designs with many cross-domain paths. When you need the complete list,
> read `report.json` in the run's `04-nextpnr/` directory.

> Those three sections are cleared whenever you re-run Elaborate or Synthesize.
> That is deliberate: neither command produces place-and-route numbers, so
> keeping them would leave stale Fmax and utilization figures on screen next to
> fresh synthesis results.

## History

Every run is written to its own timestamped directory under `.clash/`, so
nothing is overwritten. This view reads them back from disk — including runs
from previous sessions.

The tree is three levels deep:

```
Example.Project.topEntity        ← function, with a run count
└─ 2026-07-28_14-31-05           ← run id (its timestamp)
   ├─ top_entity                 ← module, with cells/wires/depth
   └─ accum
```

A run row summarises itself: the command that produced it, the target (when not
`generic`), cell count, and Fmax where applicable, with a tick or error icon
from the recorded outcome. Hovering shows the full timestamp, target, cells, and
Fmax. Runs whose `run.json` is missing or unreadable still appear, marked
*no metadata*.

**Clicking a run loads it back into Results**, so you can inspect an old run
exactly as if it had just finished — the Results header labels which run is
being shown.

Inline icons differ by row type:

- On the **History header**: clear the whole history — every design's runs.
- On a **design** row: delete that design's history, all of its runs at once.
- On a **run** row: delete the run, which removes its output directory from disk.
- On a **module** row: open that module's Verilog, or its diagram — each icon
  appears only when the artefact exists on disk.

Every deletion asks first, saying what will be removed, and takes the files with
it — these are directories under `.clash/`, not entries in a list. The generated
cabal project in `.clash/synth-project/` is never touched; it is not run output.

> A module row's Verilog is the file Clash generated for **that component**.
> Clash writes one directory per component under `02-verilog/`, so a component
> that produced no Verilog of its own simply has no icon.

Use **Refresh** in the title bar after changing `.clash/` outside the editor
(**Clash: Refresh Run History** in the palette refreshes only this section). The
`synth-project/` directory is skipped when enumerating
functions, since it holds the generated cabal project rather than run output.

See [Output Directory Structure](../architecture/directory-structure.md) for the
on-disk layout these entries map to.
