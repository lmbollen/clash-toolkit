# Circuit Diagrams

The extension renders circuit diagrams as SVGs with
[netlistsvg](https://github.com/nturley/netlistsvg), then opens them with VS
Code's built-in image preview editor.

Every Yosys script writes a JSON netlist (`write_json`); netlistsvg reads that
netlist, lays it out with [ELK](https://www.eclipse.org/elk/), and draws cells
from an SVG "skin" — so schematics look like schematics rather than a graph of
labelled boxes. netlistsvg ships inside the extension, so there is no diagram
tool to install: if synthesis produced a netlist, you get a diagram.

Rendering runs in a background process, so laying out a large design never
freezes VS Code. Opening a diagram waits for its render to finish if one is
still in flight.

## Viewing diagrams

- After **Clash: Elaborate**, the diagram opens automatically and one diagram is produced per module.
- After **Clash: Synthesize**, the diagram opens automatically. With `outOfContext` enabled, each module gets its own diagram; otherwise a single whole-design diagram is rendered.
- Click the diagram icon next to any module in the **Synthesis Results** sidebar (or the **Run History** view) to (re-)open that module's diagram.

## Drilling into sub-components

A diagram draws one module. The components it instantiates appear as boxes with
their ports — not expanded into gates — which keeps the diagram readable but
means the box alone tells you nothing about what is inside.

To go inside one, expand the module's row in **Synthesis Results** (or under a
run in **Run History**): every component it instantiates is listed beneath it,
and those rows expand in turn, so a deep hierarchy is walkable level by level.
Clicking a component opens its own diagram.

```
top_entity                 173 cells · 412 wires
├─ accum          component      ← click to open accum's diagram
├─ pipelined_sum  component
│  └─ mult_unsigned  component   ← expands as far as the hierarchy goes
└─ TRELLIS_FF     32             ← cell-type breakdown, as before
```

Those diagrams are rendered the first time you open one — laying out every
module of a design up front would cost far more than it's worth — and cached in
a `diagrams/` directory beside the netlist. Primitives (`$add`, `$dff`, …) and
black-box library cells (`LUT4`, `TRELLIS_FF`, `DP16KD`, …) are not listed:
they have no internals to draw.

Which components are listed depends on what survived synthesis. **Elaborate**
keeps the whole hierarchy. **Synthesize** keeps it for the `generic` target, but
the vendor targets (`synth_ecp5`, `synth_ice40`, …) flatten the design, so there
is nothing left to drill into — use **Elaborate** to see the structure.

`outOfContext` also gives you a row per component, but by a different route:
each component was synthesized as a run of its own, so its row carries its own
statistics and its own pre-rendered diagram instead of being drawn on demand from
the parent's netlist. Those components are flattened individually, so a
component's *diagram* shows its whole subtree inlined rather than sub-component
boxes — the hierarchy lives in the tree, not in the picture. Such rows are tagged
`out of context`; see
[Configuration](configuration.md#out-of-context-synthesis) for what that means
for the numbers.

## Per-module diagrams

The **Elaborate** command always produces one diagram per component. The top component's diagram preserves the hierarchy: sub-component instances are rendered as boxes rather than expanded into gates. Each sub-component has its own diagram showing its own internals.

For **Synthesize**, set `clash-toolkit.outOfContext` to `true` to get the same per-component breakdown, with each component synthesized standalone (so you also see per-component statistics — which count generic cells, since that path does no technology mapping; see [Configuration](configuration.md#out-of-context-synthesis)).

## Elaborate for readable schematics

Elaborated netlists still carry word-level cells (`$add`, `$mux`, `$dff`, …),
which netlistsvg draws as recognisable symbols. A technology-mapped netlist has
been shredded into hundreds of LUTs and flip-flops, so its diagram is faithful
but far harder to read. Use **Elaborate** to understand a design's structure and
**Synthesize** to see what actually got mapped.

## Troubleshooting

### "Diagram not available — rendering it failed"

The netlist was there but netlistsvg could not draw it; the output channel has
the error. Very large whole-design netlists are the usual cause — either enable
`outOfContext` to render sub-modules individually, or use **Elaborate** for a
higher-level diagram.

### "No diagram for this module — the run produced no JSON netlist to render"

There was no netlist to render from. If you edited the synthesis script (see
[Configuration](configuration.md)), check that it still has its
`write_json "{outputDir}/{outputBaseName}.json"` line — that file feeds both the
diagram and place-and-route.

### Design was optimized away

If Yosys's optimization passes removed everything (e.g. constant outputs), the diagram will be empty. Check `.clash/<module>/03-yosys/yosys.log`.
