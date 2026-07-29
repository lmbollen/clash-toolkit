# Changelog

All notable changes to **Clash Toolkit** are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0] - 2026-07-29

Schematics no longer need an external tool, hierarchical designs can be inspected
level by level, and the sidebar explains itself when it has nothing to show.

### Added
- **Drill down into sub-components.** Module rows in **Synthesis Results** and
  **Run History** expand to list the components that module instantiates, and
  clicking one opens that component's own diagram. Rows nest as deep as the
  hierarchy goes, so a design can be read level by level instead of only at its
  top. Those diagrams are rendered on first open and cached in a `diagrams/`
  directory beside the netlist. Primitives (`$add`, `$dff`, …) and black-box
  library cells (`LUT4`, `TRELLIS_FF`, …) are not listed — they have no internals
  to draw.
- **Out-of-context runs are shown as the design's hierarchy** rather than a flat
  list of components: the top component at the root, what it instantiates nested
  beneath it, every row keeping its own statistics and diagram. A per-component
  netlist is flattened and cannot be asked what it instantiates, so the graph
  comes from the Clash manifest and is recorded in `per-module/hierarchy.json` —
  which is also what lets **Run History** rebuild the nesting for a past run.
  Runs recorded by earlier versions have no such file and still load flat.
- **Out-of-context results now say what they are.** Component rows are tagged
  `out of context`, and their tooltip names the three things that make those
  figures incomparable to a whole-design run: the path runs a fixed generic script
  with no technology mapping (so cells stay generic and neither `synthesisTarget`
  nor a custom script applies), each component is flattened so its figures include
  its descendants, and nothing is optimized against its parent. The Synthesis
  Results banner names the mode, the output channel spells it out when the run
  starts, and the setting's description says the same in both the settings panel
  and `settings.json`.
- **The Haskell Functions view explains an empty list** instead of just being
  empty: the Haskell extension not being installed (the row opens it in the
  Marketplace), it being installed but not activating (the row retries), HLS
  having returned nothing for the file yet, or HLS having answered that the file
  defines no top-level functions. The extension also starts the Haskell extension
  itself when it is installed but idle, and re-checks the file when HLS publishes
  diagnostics for it.

  "No symbols yet" is deliberately not reported as "0 functions": while HLS loads
  a project it answers with nothing, and from outside the two are
  indistinguishable. HLS exposes no readiness API — the Haskell extension's
  `activate` returns nothing and its language client is private — so this is what
  can be established without depending on VS Code internals.

### Changed
- **Circuit diagrams are rendered with
  [netlistsvg](https://github.com/nturley/netlistsvg)** instead of Yosys's `show`
  command plus Graphviz `dot`. They are drawn as schematics — gate symbols, buses,
  split/join nodes — from the JSON netlist every run already writes, and netlistsvg
  ships inside the extension, so diagrams no longer depend on any external tool.
  Rendering runs in a background process, so laying out a large design does not
  stall VS Code.

  This also fixes what whole-design diagrams showed for hierarchical designs:
  `show` wrote one graph per module into a single `.dot` file, of which `dot`
  rendered only the first — often a leaf, not the top.
- **Graphviz `dot` is no longer part of the toolchain**, and has been dropped from
  the **Clash: Check Toolchain** probe, the settings panel's Tools section, and the
  **Clash: Install Toolchain** checklist. The synthesis scripts no longer emit
  `.dot` files; their `write_json` line now feeds both place-and-route and the
  diagram, so a custom script must keep it to get a diagram.
- Diagrams recorded by earlier versions still open from **Run History**.
- The packaged extension grew from ~100 KB to ~2 MB, since netlistsvg and its
  layout engine now ship with it. Files that are never loaded at runtime —
  netlistsvg's browser bundle and CLI, its TypeScript sources, and elkjs's browser
  and unminified worker builds — are excluded from the package.

### Fixed
- Documentation claimed out-of-context synthesis tech-mapped each component
  standalone. It never did: that path runs `proc`, `flatten`, `opt -purge`,
  `memory -nomap`, `opt` and stops there — deliberately, because a full `synth`
  per component hangs on components containing large block RAMs. The guide and
  architecture docs now describe what actually runs and what its numbers mean.
- Documented that the editable Yosys scripts apply to the whole-design path only.
  The per-component paths build their own fixed script, so a custom
  `synthesisScript.*` is not used when `outOfContext` is enabled on a
  multi-component design, and a custom `elaborationScript` is not used when
  elaborating one. Both were previously ignored without saying so.

## [0.2.1] - 2026-07-28

### Fixed
- Marketplace repository, issues, and homepage links pointed at a stale
  repository name. The project now lives at
  [lmbollen/clash-toolkit](https://github.com/lmbollen/clash-toolkit) and all
  metadata points there, so the README's documentation links resolve correctly.
- Corrected the `git-repository-url` used by the generated documentation book.
- Bumped development dependencies and fixed a failing tool-provider test.

## [0.2.0] - 2026-07-20

### Added
- Managed toolchain: when `yosys`, `nextpnr-*`, or Graphviz `dot` are not on the
  user's PATH, the extension shows a per-tool checklist (missing tools
  pre-checked, tools found on PATH unchecked) and downloads the selected ones
  from a self-contained OSS CAD Suite build into its own global storage, then
  runs those managed binaries. The choice is per tool and persisted; unchecked
  tools continue to use the user's PATH. Added the **Clash: Install Toolchain**
  command to review and change the selection on demand.

## [0.1.0] - 2026-07-20

Initial public release.

### Added
- Function detection in Haskell sources via Haskell Language Server.
- Code actions and command palette entries to synthesize a selected function.
- Clash → Verilog generation through `cabal run clash-synth:clash`.
- Yosys synthesis for seven target families (generic, ice40, ecp5, xilinx, gowin, quicklogic, sf2).
- Whole-design and per-module synthesis modes.
- Graphviz-rendered schematic diagrams (SVG) per synthesis run.
- nextpnr place-and-route flow for ECP5, iCE40, and Gowin (with optional routed-layout SVG).
- Synthesis results and run history tree views.
- Configurable Yosys scripts per target with placeholder substitution.
- Toolchain check command to verify external tool availability.
