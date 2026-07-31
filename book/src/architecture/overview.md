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
  clash-manifest-parser.ts  clash-manifest.json parsing, clock-domain analysis
  clash-manifest-types.ts   Types for manifest data structures
  yosys-runner.ts           Yosys script generation and execution
  yosys-types.ts            Types for Yosys synthesis results
  synthesis-targets.ts      Target registry, default scripts, placeholder expansion
  nextpnr-runner.ts         nextpnr invocation, timing/utilisation parsing
  nextpnr-types.ts          Families, device tables, options and results

  ── Tooling ──
  toolchain.ts              External tool availability checking
  tool-provider.ts          Managed OSS CAD Suite download and path resolution

  ── Diagrams ──
  netlist-renderer.ts       netlistsvg rendering; also the child-process entry point
  netlist-diagram.ts        Render orchestration, component hierarchy queries

  ── UI ──
  clash-tree.ts             The sidebar view; routes each section to its provider
  haskell-functions-tree.ts Functions section
  synthesis-results-tree.ts Results section
  run-history-tree.ts       History section
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
  ClashManifestParser (manifest + target frequency)
       │
       ▼
  YosysRunner (synthesis script → netlist JSON)
       │
       ├──────────────▶ netlist-diagram → netlistsvg (netlist JSON → SVG)
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
5. Create the sidebar view. The three tree providers are instantiated as before
   and handed to `ClashTreeProvider`, which contributes the Functions / Results
   / History section headers and routes every call back to whichever provider
   produced the row (each returned node is stamped with its section, since
   `SubComponentItem` rows can come from two of them). It is registered with
   `createTreeView` rather than `registerTreeDataProvider` for its `.selection`
   property, which title-bar buttons read to find the selected function
6. Subscribe to active-editor changes so the functions view follows the current
   Haskell file
7. Register commands and the code action provider for Haskell files
8. Run toolchain validation after a 2-second delay (to allow direnv)
