# Output Directory Structure

All generated files live under `.clash/` in the workspace root.

Because that lands inside your repository, the extension offers once per
workspace to add `.clash/` to an existing `.gitignore` — **Yes** writes the entry
with a comment saying what it is, **No** is remembered and never asked again, and
**Not right now** leaves the question open for the next session. The offer is
skipped entirely when the workspace has no `.gitignore` (the extension does not
create one) or when the file already mentions `.clash`. To change a **No** later,
add the entry yourself; the extension will then see it and stay quiet.

Each invocation writes into its own timestamped **run** directory, so previous
results are never overwritten. This is what the
[History](../guide/sidebar.md#history) section of the sidebar reads.

```
.clash/
  debug.log                          Debug log for all tool invocations
  debug.log.old                      Previous session's log, rotated on activation
  synth-project/                     Cabal project that depends on your package
    cabal.project
    clash-synth.cabal
    bin/Clash.hs
    src/                             Generated wrapper modules
  {Module}.{Function}/               One directory per synthesised function
    runs/
      {YYYY-MM-DD_HH-MM-SS}/         One directory per run, id is its timestamp
        run.json                     Run summary — command, target, stats, Fmax
        02-verilog/                  Clash Verilog output
          {Module}.topEntity/
            function_name.v          Main Verilog
            clash-manifest.json      Clash metadata
            *.sdc                    Timing constraints
            …
        03-yosys/                    Yosys synthesis results
          function_name_synth.v      Synthesized Verilog
          function_name.json         JSON netlist (for nextpnr and diagrams)
          function_name.svg          Schematic diagram (netlistsvg)
          diagrams/                  Sub-component diagrams, rendered on demand
            {Module}.svg
          stats.json                 Machine-readable statistics (`stat -json`)
          statistics.txt             Human-readable statistics report
          logic_depth.txt            Longest topological path (`ltp`)
          synth.ys                   Yosys script
          yosys.log                  Complete Yosys output
          per-module/                Per-module synthesis outputs
            hierarchy.json           Component graph + out-of-context flag
            {Module}/
              {Module}.il            RTLIL
              {Module}.json          JSON netlist
              {Module}.svg           Schematic diagram (netlistsvg)
              synth.ys
              yosys.log
        04-nextpnr/                  Place & route output
          function_name.config       Textual FPGA configuration
          function_name.routed.svg   Routed-layout SVG (when pnrWriteRoutedSvg)
          report.json                Machine-readable timing/utilisation report
          nextpnr.log
```

The run id is a local-time timestamp (`formatRunId`), so runs sort
chronologically by name. `synth-project/` is deliberately excluded when the Run
History view enumerates function directories.
