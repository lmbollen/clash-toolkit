# Changelog

All notable changes to **Clash Toolkit** are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
