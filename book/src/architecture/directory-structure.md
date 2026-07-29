# Output Directory Structure

All generated files live under `.clash/` in the workspace root.

Each invocation writes into its own timestamped **run** directory, so previous
results are never overwritten. This is what the
[Run History](../guide/sidebar.md#run-history) view reads.

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
          function_name.json         JSON netlist (for nextpnr)
          stats.json                 Machine-readable statistics (`stat -json`)
          statistics.txt             Human-readable statistics report
          logic_depth.txt            Longest topological path (`ltp`)
          synth.ys                   Yosys script
          yosys.log                  Complete Yosys output
          per-module/                Per-module synthesis outputs
            {Module}/
              {Module}.il            RTLIL
              {Module}.json          JSON netlist
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
