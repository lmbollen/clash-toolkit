# Managed Toolchain

Yosys, Graphviz, and the `nextpnr-*` binaries do not have to be installed by
hand. When a command needs one that is missing, the extension offers to download
a self-contained [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build)
build into its own private storage and run it from there.

This applies only to the EDA tools. Cabal and HLS are still yours to provide —
see [Getting Started](getting-started.md).

## PATH first

Nothing is downloaded behind your back, and a managed install never shadows your
own tools. For every tool the extension spawns:

1. If you opted to have that tool managed **and** a managed copy is on disk, the
   managed absolute path is used.
2. Otherwise the bare command name is used, so your normal PATH lookup applies.

So an existing Yosys keeps being used, and the download offer only ever appears
for tools that could not be found.

## Choosing what to manage

Both the automatic prompt and **Clash: Install Toolchain** show the same
checklist:

| Tool | Used for |
|------|----------|
| Yosys | Elaborate, Synthesize, Place & Route |
| Graphviz dot | Rendering the schematic SVGs |
| nextpnr-ecp5 | Place & route for Lattice ECP5 |
| nextpnr-ice40 | Place & route for Lattice iCE40 |
| nextpnr-himbaechel | Place & route for Gowin |

Each row is annotated with its current state — *found on PATH*, *managed*, or
*not found — download*. Missing tools are pre-checked and tools already on your
PATH are not, so accepting the default selection fills exactly the gaps. An
existing opt-in is preserved, so reopening the prompt will not silently
un-manage anything.

Ticking a tool records the opt-in and, if a managed binary for it is not already
present, fetches the archive. Dismissing the prompt changes nothing.

Because the suite is distributed as **one archive containing every tool**, the
download size does not depend on how many boxes you tick — selecting only
Graphviz costs the same as selecting everything:

| Platform | Approximate download |
|----------|---------------------|
| Linux x64 | 730 MB |
| Linux arm64 | 620 MB |
| macOS arm64 | 510 MB |
| macOS x64 | 485 MB |
| Windows x64 | 335 MB |

On any other platform or architecture, automatic installation is unavailable and
the extension says so rather than failing obscurely — install the tools yourself
and put them on your PATH.

## Where things land

The suite is extracted into the extension's global storage directory, in an
`oss-cad-suite/` subtree with the binaries under `bin/`. Nothing is written to
your workspace, and nothing outside that directory is touched. Alongside the
install, a marker file records which release is present.

When a managed binary runs, its environment is augmented so it can find its
co-located siblings and shared libraries — that is why using the suite's Yosys
with a system nextpnr is not something you need to think about.

## Pinned releases

The extension pins one OSS CAD Suite release rather than tracking the latest, so
every user gets the same mutually-compatible set of tools. Because a new pin
means a fresh multi-hundred-megabyte download for everyone, the pin is bumped
deliberately, not incidentally.

Concurrent misses share a single download, so two commands that both discover a
missing tool will not fetch the archive twice.

## Checking what resolved

**Clash: Check Toolchain** probes cabal, Yosys, `dot`, and `nextpnr-ecp5`, and
reports what is reachable. The Tools section of the
[Settings Panel](settings-panel.md) shows the same information continuously —
including the resolved path, which is the quickest way to tell whether a given
tool is coming from your PATH or from the managed install.

`nextpnr-ice40` and `nextpnr-himbaechel` are not part of that upfront probe.
They are checked when Place & Route runs for their target, which is also when
the download prompt would appear for them.
