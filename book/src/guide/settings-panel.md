# The Settings Panel

**Clash: Open Settings** — the gear icon in any of the three sidebar view title
bars — opens a dedicated panel titled *Clash Synthesis Settings*. It is a
friendlier front end for the settings described in
[Configuration](configuration.md), and the only practical way to edit the Yosys
scripts, since they are multi-line strings that are awkward to write in
`settings.json`.

The panel and VS Code settings stay in sync: edits made in `settings.json` are
reflected here as soon as the configuration changes.

## Tools

A live view of the toolchain. Three tools are probed — **cabal**, **Yosys**, and
**`nextpnr-ecp5`** — each listed with:

- whether it was found,
- its version string,
- the path it resolved to,
- or the error if the probe failed,

alongside the explanation of what that tool is needed for. **Refresh** re-probes
everything from scratch rather than reusing the cached result, which is what you
want after installing a tool or changing `yosysCommand`.

> The other place-and-route binaries (`nextpnr-ice40`, `nextpnr-himbaechel`) are
> **not** in this list. They are checked on demand when Place & Route runs for
> their target, so this panel showing only `nextpnr-ecp5` does not mean the
> others are missing.

This is the same information **Clash: Check Toolchain** reports, in a form you
can leave open while fixing your environment. See
[Managed Toolchain](managed-toolchain.md) for what to do about anything missing.

## Elaboration

The Yosys script used by **Clash: Elaborate** — hierarchy and `proc` only, no
technology mapping, producing a word-level netlist of generic cells.

The script is editable in place. A **modified** badge appears when it differs
from the built-in default, and **Reset to Default** clears the override.

## Synthesis

Three controls:

- **Target** — the FPGA family, matching `synthesisTarget`. Changing it switches
  which script the editor below is showing.
- **Out-of-context** — the `outOfContext` toggle (see
  [Configuration](configuration.md#out-of-context-synthesis)).
- **Synthesis script** — the Yosys script for the selected target, with the same
  **modified** badge and **Reset to Default** button as the elaboration script.

Because each target has its own script setting, switching the dropdown and
editing affects only that target. Overrides for the others are left untouched.

## The Inline Diff

When a script differs from its default, the panel shows a line-by-line diff
underneath the editor — added lines, removed lines, and unchanged context.

This matters more than it might sound. The default scripts do real work beyond
calling `synth_*`: they assert the design has no unconnected or multiply-driven
wires, emit the `stats.json` the extension parses for the sidebar, write the
`logic_depth.txt` report, and render the diagram via `show`. It is easy to break
one of those by accident while customising, and the diff makes it obvious what
you have changed.

> If a custom script drops the
> `tee -q -o "{outputDir}/stats.json" stat -json` line, synthesis fails loudly
> rather than silently reporting no statistics — the extension treats a missing
> `stats.json` as an error.

Remember that scripts are templates: `{files}`, `{topModule}`, `{outputDir}`,
and `{outputBaseName}` are substituted before Yosys runs. `{files}` becomes one
quoted `read_verilog` line per input file, so paths containing spaces are safe.
