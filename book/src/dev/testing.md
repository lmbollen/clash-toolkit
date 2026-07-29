# Testing

## Test Suites

All tests live under `src/test/suite/` and use **Mocha** in TDD mode (`suite` / `test`).

| File | Kind | What it covers |
|------|------|----------------|
| `type-analyzer.test.ts` | Unit | Monomorphic/polymorphic detection, edge cases |
| `code-generator.test.ts` | Unit | Wrapper generation, port annotations, DiffClock handling |
| `sdc-parser.test.ts` | Unit | SDC frequency parsing (period → MHz) |
| `synthesis-features.test.ts` | Unit | Commands, configuration, synthesis types |
| `synthesis-targets.test.ts` | Unit | Target registry, default/resolved scripts, script diffing, and that the installed Yosys supports every offered target |
| `code-actions.test.ts` | Unit | Code action provider for Haskell functions |
| `platform-tools.test.ts` | Unit | Yosys/nextpnr tool detection |
| `tool-provider.test.ts` | Unit | Managed toolchain resolution and install paths |
| `toolchain.test.ts` | Unit | Full toolchain availability |
| `clash-compiler.test.ts` | Unit | Clash compiler invocation helpers |
| `nextpnr-runner.test.ts` | Unit | nextpnr child-process lifecycle |
| `results-tree.test.ts` | Unit | Synthesis Results tree construction |
| `internal-components.test.ts` | Unit | Internal component expansion |
| `pnr-targets.test.ts` | Integration | End-to-end synthesis + place & route per target |
| `hls-client.test.ts` | Integration | HLS communication |
| `function-detector.test.ts` | Integration | Function detection from real Haskell files via HLS |
| `integration.test.ts` | Integration | Per-module synthesis, SDC parsing, end-to-end flows |

## Running Tests

### From the terminal

```bash
npm test
```

This compiles first (via `pretest`) and then launches a headless VS Code
instance against `test-project/`.

`runTest.ts` handles the two environment problems that used to make this fail:

- **NixOS / headless hosts.** Set `VSCODE_EXECUTABLE_PATH` to a nix-wrapped
  Electron binary (e.g. from `vscode-fhs`) if the downloaded VS Code build
  cannot find its system libraries. Leave it unset to let
  `@vscode/test-electron` download a matching build. It must be a real Electron
  binary, *not* the `code` CLI wrapper — the wrapper backgrounds the app and
  exits 0, so the test host never runs.
- **Running from VS Code's integrated terminal.** The parent editor leaks
  `ELECTRON_RUN_AS_NODE=1` and a set of `VSCODE_*` variables that would make the
  test host run as plain Node or attach to the running instance. `runTest.ts`
  strips them, so the integrated terminal works the same as an external one.

### From VS Code

1. **Ctrl+Shift+D** → select **Extension Tests**
2. Press **F5**

A second VS Code window opens with the test-project workspace, runs all suites, and reports results in the Debug Console. Use this when you want breakpoints.

## Writing a New Test

```typescript
import * as assert from 'assert';

suite('My Feature', () => {
    test('does the right thing', () => {
        assert.strictEqual(1 + 1, 2);
    });

    test('async operation', async function () {
        this.timeout(10_000);
        const result = await someAsyncCall();
        assert.ok(result);
    });
});
```

Place the file in `src/test/suite/` with a `.test.ts` suffix — the test runner picks it up automatically via the glob in `index.ts`.

## Debugging a Test

Set breakpoints in your `.test.ts` file, then launch **Extension Tests** with F5. Execution pauses at breakpoints; use the Debug panel to inspect state.
