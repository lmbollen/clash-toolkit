# Nextpnr Integration

## Supported Families

`PNR_FAMILIES` in `nextpnr-types.ts` maps a `synthesisTarget` to the nextpnr
binary and device list used for place & route. Only targets present in that map
support P&R:

| Target | Executable | Constraints format | Device flag |
|--------|-----------|-------------------|-------------|
| `ecp5` | `nextpnr-ecp5` | `.lpf` | `--<device>` prefix |
| `ice40` | `nextpnr-ice40` | `.pcf` | `--<device>` prefix |
| `gowin` | `nextpnr-himbaechel` | `.cst` via `--vopt` | `--device <value>` |

The **Place & Route** command picks the family from the configured
`synthesisTarget` and prompts for a device from that family's list — it is not
ECP5-only. Targets with no entry above (`generic`, `xilinx`, `quicklogic`,
`sf2`) can be synthesized but not placed and routed; the command reports this
and stops.

> **Nexus and MachXO2 are deliberately unsupported.** Their nextpnr binaries
> take different output and constraint flags (`--fasm`, `--pdc`) that
> `buildNextpnrArgs` does not emit, so listing them would be misleading. They
> can be re-added once argument handling covers them.

## ECP5 Devices

| Device | LUTs | Description |
|--------|------|-------------|
| `25k` / `um-25k` / `um5g-25k` | 24K | LFE5U-25F / LFE5UM-25F / LFE5UM5G-25F |
| `45k` / `um-45k` / `um5g-45k` | 44K | LFE5U-45F / LFE5UM-45F / LFE5UM5G-45F |
| `85k` / `um-85k` / `um5g-85k` | 84K | LFE5U-85F / LFE5UM-85F / LFE5UM5G-85F |

Known packages: `CABGA256`, `CABGA381`, `CABGA554`, `CABGA756`, `CSFBGA285`,
`CSFBGA381`, `CSFBGA554`. Speed grades: `6`, `7`, `8` (lower is faster).

The package picker is not hard-coded — `NextpnrRunner.getValidPackages()` probes
the nextpnr binary for the packages the chosen device actually supports, and the
prompt is skipped entirely when it reports none.

## iCE40 Devices

| Device | Logic cells | Notes |
|--------|-------------|-------|
| `lp384` | 384 | |
| `lp1k` / `hx1k` | 1280 | |
| `lp4k` / `hx4k` / `u4k` | 3520 | |
| `lp8k` / `hx8k` | 7680 | |
| `up3k` | 2800 | UltraPlus |
| `up5k` | 5280 | UltraPlus |

## Gowin Devices

Gowin goes through `nextpnr-himbaechel`, which needs both a device string and a
`family=` value passed via `--vopt`:

| Device | LUTs | Notes |
|--------|------|-------|
| `GW1N-LV1QN48C6/I5` | 1152 | GW1N-1 (QN48) |
| `GW1N-UV4LQ144C6/I5` | 4608 | GW1N-4 (LQ144) |
| `GW1N-LV9QN88C6/I5` | 8640 | GW1N-9 / GW1N-9C (QN88) — differ by `family=` |
| `GW1NR-LV9QN88PC6/I5` | 8640 | GW1NR-9 / GW1NR-9C, with SDRAM |
| `GW1NSR-LV4CQN48PC7/I6` | 4608 | GW1NSR-4C (QN48), with SDRAM |
| `GW2A-LV18QN88C8/I7` | 20736 | GW2A-18 / GW2A-18C |

## Command-Line Arguments

`NextpnrRunner.buildNextpnrArgs()` constructs:

```
nextpnr-ecp5 \
  --json design.json \
  --textcfg output.config \
  --25k \
  --package CABGA381 \
  --speed 6 \
  --freq 50 \              # top entity's clock domain, when it has one
  --lpf constraints.lpf    # when provided
```

