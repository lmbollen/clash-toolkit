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
want after installing a tool or changing `toolCommands`.

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
  [Configuration](configuration.md#out-of-context-synthesis)). This *also*
  switches the editor, see below.
- **Synthesis script** — the Yosys script the next run will execute, with the
  same **modified** badge and **Reset to Default** button as the elaboration
  script.

Because each target has its own script setting, switching the dropdown and
editing affects only that target. Overrides for the others are left untouched.

### The editor follows the checkbox

Out-of-context synthesis runs a **different script**, not a variant of the
target's: it stubs the component's sub-components as black boxes and issues no
`synth_*` command. It is stored separately, as `outOfContextScript`.

So the editor is bound to whichever of the two the current settings would
actually run. Tick **Out-of-context** and the title changes to *Out-of-Context
Synthesis Script*, and the contents, the **modified** badge and the diff all
switch to `outOfContextScript` — along with two extra placeholders, `{libFiles}`
and `{keepBlackBoxes}`, that only mean anything on that path. Untick it and
everything switches back to the selected target's script. **Save** and **Reset
to Default** always act on whichever is on screen.

Unsaved edits survive an unrelated settings change, but switching the target or
the checkbox reloads the editor — it is bound to a different script at that
point, so there is nothing for the old text to be saved to.

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
