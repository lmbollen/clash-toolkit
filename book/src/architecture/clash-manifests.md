# Clash Manifests

Clash generates a `clash-manifest.json` in each HDL output directory. The extension parses this to:

- Determine the top component name and ports
- Discover dependencies between components
- Extract clock domain information
- Collect all Verilog files (including sub-modules)

## Manifest Structure

```json
{
  "components": ["top_entity"],
  "dependencies": { "transitive": ["Example.Project.accum"] },
  "domains": {
    "Dom50": {
      "active_edge": "Rising",
      "init_behavior": "Defined",
      "period": 20000,
      "reset_kind": "Asynchronous",
      "reset_polarity": "ActiveHigh"
    }
  },
  "files": [
    { "name": "top_entity.v", "sha256": "..." },
    { "name": "top_entity.sdc", "sha256": "..." }
  ],
  "top_component": {
    "name": "top_entity",
    "ports_flat": [
      { "direction": "in", "is_clock": true, "name": "CLK", "width": 1 },
      ...
    ]
  }
}
```

## Dependency Graph

`ClashManifestParser.buildDependencyGraph()` recursively follows dependency manifests and returns components in post-order (leaves first, top last). Each component's `dependencies` list is reduced to **direct only** — transitive deps are removed via `removeTransitiveDeps` to prevent Yosys "Re-definition of module" errors during OOC synthesis.

## Domain Analysis

Clock domain periods in the manifest are in **picoseconds**. The parser converts to MHz:
`frequencyMHz = 1_000_000 / periodPs`

For example, `Dom50` with `period: 20000` (20 ns) → 50 MHz. This is the value place
& route is constrained against — see [Timing Analysis](../guide/timing-analysis.md#target-frequency).

**Which domain counts.** `domains` lists every domain the design *mentions*, so
the parser never chooses one by name. `parseManifest` walks
`top_component.ports_flat`, and for every `is_clock` port pairs it with the domain
that port declares, producing `topClocks: TopClock[]` — port, domain, period, and
frequency, all as stated. A port with no domain, a domain the manifest does not
define, or a domain without a usable period throws: the manifest contradicts
itself and no substitute would be honest.

`pnrTargetClock(manifest)` then answers the one question place & route asks —
which single clock to constrain against. Empty `topClocks` means no target (a
combinational design); several ports sharing one domain is one target; two or more
domains throws, because nextpnr's `--freq` applies to the whole design and cannot
satisfy both.
