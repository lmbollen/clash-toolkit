# Development Setup

## Prerequisites

The project uses [Nix](https://nixos.org/) to provide a reproducible development environment. The `flake.nix` at the repository root pulls in:

- **Node.js 20** + npm + TypeScript
- **GHC** with Clash and its compiler plugins
- **Cabal** for building the test Haskell project
- **Haskell Language Server** (HLS)
- **Yosys** for logic synthesis
- **nextpnr** for place & route (ice40, ecp5, …)
- **mdbook** for building this book

Enter the shell:

```bash
nix develop
```

The dev shell prints the resolved version of each tool on entry, which is the
quickest way to confirm the environment is what you expect.

Note that the dev shell provides these for *extension development*. At runtime
the extension does not depend on them being present — it can download its own
EDA tools, as described in [Getting Started](../guide/getting-started.md).

## Building the Extension

```bash
npm install          # once
npm run compile      # one-off build
npm run watch        # incremental recompilation (background)
```

## Running in VS Code

1. Open this repository in VS Code.
2. Press **F5** to launch the Extension Development Host.
3. In the new window, open the `test-project/` folder.
4. Open `src/Example/Project.hs` and wait for HLS to initialise.
5. Use the Command Palette (`Ctrl+Shift+P`) to invoke Clash commands.

## Project Layout

| Path | Purpose |
|------|---------|
| `src/` | Extension source (TypeScript) |
| `src/test/` | Mocha test suites |
| `test-project/` | Sample Haskell/Clash project used during development |
| `book/` | mdbook documentation (this book) |
| `flake.nix` | Nix dev-shell definition |

## Building the Documentation

```bash
mdbook build book     # render to book/book/ (git-ignored)
mdbook serve book     # live-reloading preview on localhost:3000
```

## NixOS Notes

`npm test` works from the terminal, including on NixOS — `runTest.ts` strips the
environment variables VS Code leaks into its integrated terminal and honours
`VSCODE_EXECUTABLE_PATH` for hosts where the downloaded Electron binary cannot
find system libraries (`libglib-2.0.so.0`, etc.). Point that variable at a
nix-wrapped Electron build if the default download fails. See the
[Testing](testing.md) chapter for details.
