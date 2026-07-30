# Clash Toolkit

[![Documentation](https://github.com/lmbollen/clash-toolkit/actions/workflows/docs.yml/badge.svg)](https://lmbollen.github.io/clash-toolkit/)

> **Community extension** — maintained by [Lucas Bollen](https://github.com/lmbollen) (QBayLogic). Not an official release of the [Clash](https://clash-lang.org/) project.

Synthesize Verilog from Haskell functions using [Clash](https://clash-lang.org/), explore the result with [Yosys](https://yosyshq.net/yosys/), and place-and-route for ECP5, iCE40, and Gowin FPGAs with [nextpnr](https://github.com/YosysHQ/nextpnr) — all from inside VS Code.

```
Haskell source → Clash (Verilog) → Yosys (synthesis) → nextpnr (place & route)
```

## Quick Start

1. Install the extension. You need Clash and HLS available (e.g. via `nix develop`).
   **Yosys and nextpnr are optional** — if a command needs one
   that isn't on your PATH, the extension shows a checklist of tools (missing
   ones pre-checked, found ones unchecked) and downloads the checked tools from
   a self-contained [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build)
   build into its own storage. You choose, per tool, which to manage.
2. Open a Clash project, make sure HLS is running.
3. Open a `.hs` file → **Ctrl+.** on a monomorphic function → **Synthesize** (or use the Command Palette).

Run **Clash: Check Toolchain** to verify tool availability, or **Clash: Install
Toolchain** to download the bundled toolchain up front. A tool you already have
on your PATH is always used in preference to the managed copy.

## Documentation

Full documentation is published at
**[lmbollen.github.io/clash-toolkit](https://lmbollen.github.io/clash-toolkit/)**,
built from the **[book/](book/)** directory with
[mdbook](https://rust-lang.github.io/mdBook/). To read it locally:

```bash
mdbook serve book   # http://localhost:3000
```

Highlights:

- [Getting Started](book/src/guide/getting-started.md) — prerequisites, first synthesis
- [Commands](book/src/guide/commands.md) — every command and what it runs
- [The Sidebar](book/src/guide/sidebar.md) — the three views and what their rows mean
- [Circuit Diagrams](book/src/guide/circuit-diagrams.md) — schematics and drilling into components
- [Configuration](book/src/guide/configuration.md) — settings reference
- [Architecture Overview](book/src/architecture/overview.md) — source layout and data flow
- [Developer Setup](book/src/dev/setup.md) — building, running, Nix shell
- [Testing](book/src/dev/testing.md) — test suites and how to run them

## License

[BSD 2-Clause](LICENSE) © 2026 Lucas Bollen.
