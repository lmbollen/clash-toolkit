# Code Actions

The extension registers a code action provider for Haskell files. When your cursor is on a monomorphic function definition, pressing `Ctrl+.` (or clicking the lightbulb) offers:

- **Clash: Elaborate 'funcName'** — Clash compilation + Yosys elaboration, one diagram per module
- **Clash: Synthesize 'funcName'** — runs Clash compilation + Yosys synthesis (no PnR)
- **Clash: Place & Route 'funcName'** — full Clash + Yosys + nextpnr pipeline

Code actions skip the function detection and picker dialogs — the function under the cursor is used directly. This provides a fast workflow for synthesizing specific functions without navigating the command palette.

## How It Works

The provider calls `FunctionDetector.getFunctionAtPosition()` — a targeted single-symbol lookup rather than a full document scan, so it is cheap enough to run on every cursor move. Only monomorphic functions produce code actions; polymorphic functions and non-function positions are silently ignored.

Because the lookup is asynchronous, the provider also honours the cancellation token: if the cursor moves again before the lookup resolves, the superseded request returns no actions rather than building them from a stale position.
