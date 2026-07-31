# Timing Analysis

After place & route with nextpnr, the extension reports timing information.

## Metrics

| Metric | Meaning |
|--------|---------|
| **Pre-Routing Frequency** | Estimated FMax before routing — optimistic upper bound |
| **Max Frequency** | Actual FMax after routing — the real achievable clock speed |
| **Critical Path Delay** | The longest combinational path in nanoseconds |
| **Constraints Met** | Whether the design meets the target frequency |

The **routing overhead** (difference between pre-routing and post-routing frequency) is typically 15–30% and is normal.

## Target Frequency

The frequency nextpnr is constrained against comes from the **Clash manifest**,
which states everything needed: which top-entity ports are clocks, which domain
each one is in, and every domain's period. A top entity whose clock port is in
`Dom50` (`period: 20000` ps) is placed and routed against 50 MHz, passed as
`--freq`.

This is deliberately not a setting. The target is a property of the entity being
synthesized — two top entities in one workspace can run at different frequencies,
and the manifest already states both.

Nothing is guessed around, because the number becomes a verdict: a frequency
belonging to some other part of the design would have place & route report
**constraints met** about a constraint the design never had. So:

| The manifest says | What happens |
|---|---|
| One clock domain across the top entity's clock ports | That domain's period is the target |
| No clock ports at all | No `--freq` — nextpnr reports an unconstrained Fmax, which is what a combinational design has |
| Clock ports in **two or more** domains | **Place & route stops**, naming each clock and its frequency. One `--freq` covers the whole design, so no single number can be met by both |
| A clock port with no domain, a domain the manifest never defines, or a domain without a usable period | **The manifest is rejected** when parsed — it disagrees with itself |

> For a multi-clock design, synthesis and elaboration still work normally; it is
> only place & route that has nothing to constrain against.

## Resource Utilization

The extension also reports resource utilization after place & route:

- **LUTs** — Look-up tables used vs. total available
- **Registers** — Flip-flops used vs. total
- **BRAM** — Block RAM tiles used vs. total
- **IO** — IO pins used vs. total

All values include usage percentages.
