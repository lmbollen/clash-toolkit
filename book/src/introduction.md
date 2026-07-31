# Clash Toolkit

> **Community extension** — maintained by [Lucas Bollen](https://github.com/lmbollen) (QBayLogic). Not an official release of the [Clash](https://clash-lang.org/) project.

Synthesize Verilog from Haskell functions using [Clash](https://clash-lang.org/), explore the result with [Yosys](https://yosyshq.net/yosys/), and place-and-route for ECP5, iCE40, and Gowin FPGAs with [nextpnr](https://github.com/YosysHQ/nextpnr) — all from inside VS Code.

The extension integrates with **Haskell Language Server** (HLS) to find functions in your Clash project, determines which ones are monomorphic (and therefore synthesisable), then drives the full hardware workflow:

```
Haskell source → Clash (Verilog) → Yosys (synthesis) → nextpnr (place & route)
```

At every stage you can inspect output, view statistics, and open a schematic diagram rendered with netlistsvg.

## Feature Highlights

- **Function detection** via HLS — finds monomorphic functions automatically
- **Code actions** — press `Ctrl+.` on a function to elaborate, synthesize, or place & route it directly
- **Managed toolchain** — Yosys and nextpnr can be downloaded on demand instead of installed by hand
- **Sidebar** — browse functions, inspect synthesis results, and revisit past runs from the *Clash Synthesis* view
- **Editable synthesis scripts** — override the Yosys script per target from the settings panel
- **Optional out-of-context synthesis** — synthesize each component standalone (generic cells, no technology mapping) for a per-component diagram and statistics
- **Timing targets from the design** — the top entity's clock domain sets the frequency place & route is judged against
- **Schematic diagrams** — netlistsvg-rendered SVG schematic per synthesis run, with no extra tool to install
- **Hierarchical inspection** — expand a module in the sidebar to reach the components it instantiates, each with its own diagram
- **Full PnR flow** — ECP5 / iCE40 / Gowin place & route with timing analysis and utilization reports
- **Debug logging** — all tool invocations logged to `.clash/debug.log`
