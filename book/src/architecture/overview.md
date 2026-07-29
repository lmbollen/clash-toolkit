# Architecture Overview

## Source Layout

```
src/
  extension.ts              Activation, command registration, orchestration

  ── Front end (finding what to synthesize) ──
  clash-code-actions.ts     Code action provider (Ctrl+. on functions)
  hls-client.ts             HLS integration (document symbols, hover types)
  function-detector.ts      Function scanning and classification UI
  type-analyzer.ts          Monomorphism analysis

  ── Pipeline ──
  code-generator.ts         Wrapper generation, synth project, run directories
  clash-compiler.ts         Clash invocation and output parsing
  clash-manifest-parser.ts  clash-manifest.json parsing, SDC frequency extraction
  clash-manifest-types.ts   Types for manifest data structures
  yosys-runner.ts           Yosys script generation and execution
  yosys-types.ts            Types for Yosys synthesis results
  synthesis-targets.ts      Target registry, default scripts, placeholder expansion
  nextpnr-runner.ts         nextpnr invocation, timing/utilisation parsing
  nextpnr-types.ts          Families, device tables, options and results

  ── Tooling ──
  toolchain.ts              External tool availability checking
  tool-provider.ts          Managed OSS CAD Suite download and path resolution

  ── UI ──
  haskell-functions-tree.ts Haskell Functions view
  synthesis-results-tree.ts Synthesis Results view
  run-history-tree.ts       Run History view
  run-loader.ts             Reads a past run back off disk
  synthesis-settings-panel.ts  Settings webview (tools, scripts, inline diff)

  ── Support ──
  file-logger.ts            Debug file logging (.clash/debug.log)
  types.ts                  Shared FunctionInfo interface
```

## Key Types

```typescript
interface FunctionInfo {
  name: string;
  range: Range;
  typeSignature: string | null;
  isMonomorphic: boolean;
  filePath: string;
  moduleName: string | null;
}

interface ComponentInfo {
  name: string;
  verilogFiles: string[];
  dependencies: string[];   // direct only, not transitive
  directory: string;
}

type PortAnnotation =
  | { kind: 'name'; name: string }
  | { kind: 'product'; name: string; subPorts: string[] };
```

## Data Flow

```
User Code (.hs)
       │
       ▼
  HLS (symbols + hover)
       │
       ▼
  FunctionDetector → TypeAnalyzer
       │
       ▼
  CodeGenerator (wrapper .hs + synth project)
       │
       ▼
  ClashCompiler (cabal run clash → Verilog)
       │
       ▼
  ClashManifestParser (manifest + SDC frequency)
       │
       ▼
  YosysRunner (synthesis script → netlist JSON)
       │
       ▼
  NextpnrRunner (PnR → timing)
```

## Extension Activation

On activation (`onLanguage:haskell`):

1. Create the "Clash Synthesis" output channel
2. Initialize the file logger at `.clash/debug.log`
3. Instantiate the pipeline components (HLSClient, FunctionDetector,
   CodeGenerator, ClashCompiler, YosysRunner, NextpnrRunner) and the `clash`
   diagnostic collection
4. Initialize the managed tool provider, then the ToolchainChecker
5. Create the three sidebar views. Synthesis Results and Haskell Functions use
   `createTreeView` rather than `registerTreeDataProvider` — the former needs a
   `.message` banner to label which run is being displayed, the latter a
   `.selection` property so title-bar buttons can read the selected function.
   Run History needs neither, so it registers a plain data provider
6. Subscribe to active-editor changes so the functions view follows the current
   Haskell file
7. Register commands and the code action provider for Haskell files
8. Run toolchain validation after a 2-second delay (to allow direnv)
