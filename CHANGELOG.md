# Changelog

All notable changes to **Clash Toolkit** are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Windows, exercised end to end for the first time: the managed toolchain now
installs and launches, and schematics are readable whatever colour theme you
run.

### Fixed
- **The managed toolchain works on Windows.** Every binary the OSS CAD Suite
  supplied was reported as missing. Unlike the Linux and macOS builds, the
  Windows suite ships no wrapper scripts and keeps its ~160 runtime DLLs in
  `lib/` rather than beside the executables, so `yosys.exe` and the
  `nextpnr-*.exe` binaries died in the loader with `STATUS_DLL_NOT_FOUND`
  before writing any output — which the probe could only read as "not
  installed". `lib/` now joins `bin/` on PATH, the way upstream's
  `environment.bat` does, and Windows spawns inherit the `YOSYSHQ_ROOT`,
  `PYTHON_EXECUTABLE` and `SSL_CERT_FILE` that script sets. A failure to launch
  no longer masquerades as a missing install either: the loader's NTSTATUS
  codes are spelled out, and the error names the binary that was actually
  spawned. Existing installs need no reinstall — the extracted tree was always
  correct, only the environment was wrong.
- **Installing the toolchain survives a long install path.** The Windows
  release asset is a 7-Zip self-extractor that is not long-path aware, and the
  suite's deepest entry sits 132 characters below its own root — so a global
  storage path of any real length pushed part of the archive past Windows'
  260-character limit, and the install died part-way through with a wall of
  "Can not open output file", leaving a half-extracted tree behind. The
  destination is now handed over in its `\?\` form, which opts the extractor
  out of that limit.
- **Cancelling a run no longer throws when the tool never started.** Signalling
  a child that failed to spawn — yosys missing, or the run cancelled before the
  process existed — quietly returns `false` on Linux and macOS but raises
  `EINVAL` on Windows, so the cancellation surfaced as an unhandled error
  instead of a cancelled run.
- **Diagrams are readable on a dark colour theme.** netlistsvg draws black
  lines and black text onto a transparent canvas, so the image preview editor
  showed each schematic over whatever the theme paints behind it — black on
  near-black, on any dark theme. Schematics are black-on-white by convention
  and the skin's colours are not themeable, so the SVG now carries its own
  white background, which also keeps it readable wherever else it is opened,
  embedded or printed. Diagrams already on disk are redrawn by the next run.
- **`npm test` and `vsce package` run on Windows.** `clean` called `rm -rf`,
  which cmd.exe has no equivalent of, so `pretest` never got as far as the
  suite; and `verify-package.js` spawned the `vsce.cmd` shim, which Node has
  refused to execute without a shell since the fix for CVE-2024-27980 — gating
  `vscode:prepublish`, and with it both packaging and publishing.

## [0.5.0] - 2026-08-05

Out-of-context synthesis now means what it says — each component is judged with
its sub-components stubbed as black boxes rather than flattened into it — and
the EDA tools got the same `auto` parallelism treatment cabal got in 0.4.0.

### Added
- `clash-toolkit.yosysJobs` — how many components Yosys synthesizes at once on
  the per-component paths. A component's run needs its dependencies' *Verilog*,
  never their *results*, so there was no reason for the old one-at-a-time loop.
  `auto` (the default) uses one single-threaded Yosys process per core, less one
  for the editor, capped at 8 — past that the runs contend for memory rather
  than CPU. An explicit number is honoured uncapped.
- `clash-toolkit.nextpnrThreads` — nextpnr's `--threads`, `auto` by default
  (capped at 4: only some passes thread, and routing stops gaining well before
  the core count). Note that a fixed `--seed` only pins a run's outcome at a
  fixed thread count, so set this to `1` for bit-identical results across
  machines.
- `clash-toolkit.outOfContextScript` — the per-component synthesis script is now
  editable, like the whole-design scripts already were. It is a separate script
  from `synthesisScript.<target>`, not a variant of it: an out-of-context run
  issues no `synth_*` command, so the target's script has nothing to say about
  it. It gets two placeholders of its own — `{libFiles}` (the sub-components
  read as black boxes) and `{keepBlackBoxes}` (keeps those instances through
  optimization; drop it and an instance whose outputs are unused is silently
  deleted). The settings panel's script editor follows the **Out-of-context**
  checkbox, so what it shows is always the script the next run will execute.

### Changed
- **Out-of-context synthesis stubs sub-components as black boxes instead of
  flattening them in.** The old `flatten` made each component's figures include
  its descendants, so per-component numbers overlapped and adding them up meant
  nothing. Now dependencies are read with `read_verilog -lib` — interfaces kept,
  bodies discarded — so a component's cells, statistics and diagram cover its
  own logic, with each sub-component as one opaque cell. The Results and History
  tooltips describe the new meaning.
- **Per-component elaboration reads dependencies in full**, so the netlist
  carries the real sub-module definitions and a component's diagram can be
  drilled into, rather than dead-ending at an empty box.
- **Sidebar sections read as sections.** Headers are upper-cased, the way
  VS Code styles its own, and a blank separator row precedes each one — inert,
  and announced to screen readers as a separator — so a section boundary no
  longer looks like one more row among the section's contents.

## [0.4.0] - 2026-07-31

The sidebar is one view instead of three, cabal builds in parallel, and the
numbers place & route is judged against now come from the design itself rather
than a setting.

### Added
- **cabal builds in parallel.** The Clash invocation now passes `--jobs`, which
  defaults to one job per core (`cabalJobs: auto`), so a project whose
  dependencies are not built yet no longer compiles them one package at a time on
  a single core. Set `clash-toolkit.cabalJobs` to a number to cap it, or to `1`
  for the old sequential behaviour; changing it never invalidates cabal's
  existing build products. The new `clash-toolkit.ghcJobs` additionally passes
  `-jN` to GHC for parallel *module* compilation inside a single package — off by
  default, because GHC options are part of cabal's build plan and toggling it
  rebuilds the plan once.
- **An offer to gitignore `.clash/`.** The extension writes generated Verilog,
  netlists and run history into the workspace, so on activation it asks once
  whether to add `.clash/` to an existing `.gitignore`. **Yes** appends it under a
  comment saying what it is, **No** is remembered, and **Not right now** records
  nothing so the question returns next session. Nothing is asked when the
  workspace has no `.gitignore` — one is never created — or when the file already
  mentions `.clash`, including an explicit `!.clash` un-ignore. The answer is
  stored per workspace, since it is about that repository.
- **Delete a design's history, or all of it.** The History section's header
  carries a trash icon that clears every design's runs, and each design row one
  that deletes just its own. Both name what they are about to remove and take the
  directories with them; `.clash/synth-project/` is left alone, being the
  generated cabal project rather than run output.
- **Clash: Refresh** — one title-bar button that re-reads both the active file's
  functions and the runs on disk. The two per-section refresh commands remain in
  the command palette.
- `clash-toolkit.toolCommands` — per-tool command overrides, keyed by tool name
  (`cabal`, `yosys`, `nextpnr-ecp5`, `nextpnr-ice40`, `nextpnr-himbaechel`). Only
  yosys could be pointed elsewhere before, though place & route needs its nextpnr
  binary just as much and cabal was hardcoded outright. A value may carry a
  wrapper (`nix run nixpkgs#yosys --`, `wsl yosys`), which is the case neither
  PATH detection nor the managed download can cover. The same command is used for
  the pre-flight probe and the run itself.

### Changed
- **The sidebar is one view instead of three.** Haskell Functions, Synthesis
  Results and Run History are now the **Functions**, **Results** and **History**
  sections of a single tree, so there is one title bar and one thing to size and
  scroll rather than three that had to be managed separately. Each section still
  has its own provider and collapses independently, and each header shows what
  that section is currently reporting — the file whose functions are listed, the
  run loaded into Results, or why HLS has nothing to say — which is what the
  per-view banners used to do.
- **The place & route target frequency now always comes from the design**: the
  period of the clock domain the top entity's clock port declares, read from the
  Clash manifest on every run. A design with no clocked domain gets no `--freq`,
  so nextpnr reports an unconstrained Fmax rather than a verdict about a
  constraint the design never had, and a design driven by clocks in two domains
  stops place & route with both named — one `--freq` covers the whole design and
  cannot be met by both.
- **Diagrams open as preview tabs.** Opening one component's diagram after
  another replaces what is on screen instead of leaving a tab behind for each,
  which is what walking a hierarchy does. Pin a tab (or double-click it) to keep
  that diagram and have the next one open alongside it.
- **The settings page is grouped.** `contributes.configuration` is now five
  ordered categories — Synthesis, Place & Route, Build, Toolchain, Yosys Scripts
  — instead of one flat list with the eight multiline script boxes in the middle
  of it. Setting ids are unchanged, so nothing needs migrating.

### Deprecated
- `clash-toolkit.yosysCommand`, superseded by `clash-toolkit.toolCommands` with
  the key `yosys`. Still honoured, so an existing configuration keeps working.

### Removed
- `clash-toolkit.pnrTargetFrequencyMHz`. A workspace-global number cannot be
  right for two top entities that run at different frequencies, and the manifest
  already states both. A value left in `settings.json` is ignored.
- The view ids `clash-toolkit.haskellFunctions`, `clash-toolkit.synthesisResults`
  and `clash-toolkit.runHistory`, replaced by the single
  `clash-toolkit.explorer`. Anything referring to the old ids — a custom
  keybinding, a saved layout — needs the new one.

### Fixed
- **Past runs offer their Verilog again.** Clash writes a directory per component
  under `02-verilog/`, and the history loader listed only the top of that
  directory — where there are no `.v` files, just directories. Every per-module
  run (which is every elaboration, and out-of-context synthesis) therefore
  recorded no Verilog and showed no icon to open it with. Each component now
  resolves to the Verilog Clash generated for it.
- **The target frequency was read from the wrong clock domain.** A manifest lists
  every domain the design *mentions*, and the parser picked `System` by name when
  present — on the bundled test project, reporting 100 MHz for a design clocked at
  50. Each of the top entity's clock ports is now paired with the domain that port
  declares, and nothing is inferred around a gap: a clock with no domain, a domain
  the manifest never defines, or a domain without a usable period is rejected when
  the manifest is parsed.

## [0.3.1] - 2026-07-29

### Fixed
- The published extension no longer contains `.clash/` — the debug logs and
  synthesis output the extension writes into the workspace while developing.
  `vsce` reads only `.vscodeignore`, never `.gitignore`, so being untracked was
  not enough: 0.3.0 shipped `.clash/debug.log` with absolute paths from the
  machine that built it. `scripts/verify-package.js` now checks the packaged file
  list from `vscode:prepublish`, which gates `vsce package` and a bare
  `npx vsce publish` alike, and fails if anything on its deny list would ship or
  anything the extension needs at runtime is missing.

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
