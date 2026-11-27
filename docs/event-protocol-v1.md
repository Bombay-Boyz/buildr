# buildr Event Protocol v1 Specification

**Specification Version**: 1.0  
**Status**: Canonical Standard & Published Specification (§9, §23)  
**Reference Implementation**: [buildr](https://github.com/Bombay-Boyz/buildr)  

---

## 1. Overview

The **buildr Event Protocol v1** is a streaming, language-agnostic, machine-readable protocol for reporting compiler and build orchestration progress. It is formatted as **JSON Lines** (newline-delimited JSON objects, RFC 7464 / UTF-8), designed to be emitted by compilers, build wrappers, or build systems and consumed by live terminal renderers, editors, or CI runners.

### Key Characteristics
1. **Nanosecond Timestamps**: Monotonically non-decreasing timestamps relative to the start of the build.
2. **Strict Monotonic Sequencing**: Every event carries a monotonically increasing integer sequence number `seq`.
3. **Honesty Annotations**: Every event explicitly declares whether its payload represents an `observed`, `reported`, `derived`, or `estimate` value.
4. **Transport Independence**: Transmitted over Unix domain sockets, Windows Named Pipes (`\\.\pipe\...`), standard input/output pipes, or recorded trace files (`.jsonl`).

---

## 2. Common Event Envelope

Every event line MUST be a single-line JSON object containing the following top-level keys:

```json
{
  "v": 1,
  "seq": 1042,
  "t": 18234567890,
  "src": "ghc",
  "valueKind": "observed",
  "kind": "ModuleStarted"
}
```

| Field | Type | Description |
|---|---|---|
| `v` | Integer | Protocol major version. Always `1` for this specification. |
| `seq` | Integer | Monotonically increasing sequence number starting at 1. |
| `t` | Integer | Monotonic timestamp in nanoseconds since build invocation. |
| `src` | String | Source subsystem: `"cabal"`, `"ghc"`, `"plan"`, `"graph"`, `"history"`, `"buildr"`, or compiler name (e.g. `"clang"`). |
| `valueKind` | String | Epistemic classification: `"observed"`, `"reported"`, `"derived"`, or `"estimate"`. |
| `kind` | String | Discriminated event payload kind. |

---

## 3. Event Catalog & Payload Specifications

### 3.1 `BuildStarted`
Emitted once at the start of the build by the orchestrator.

```json
{
  "v": 1, "seq": 1, "t": 0, "src": "buildr", "valueKind": "observed",
  "kind": "BuildStarted",
  "buildrVersion": "0.1.0.0",
  "cabalVersion": "3.12.1.0",
  "ghcVersion": "9.10.3",
  "profile": "-O1",
  "userFlags": ["--keep-going", "-j4"]
}
```

### 3.2 `PlanReady`
Emitted when the build plan has been calculated (e.g. from `plan.json` or equivalent DAG).

```json
{
  "v": 1, "seq": 2, "t": 12500000, "src": "plan", "valueKind": "reported",
  "kind": "PlanReady",
  "units": [
    {
      "puId": "buildr-0.1.0.0-inplace",
      "puPackage": "buildr",
      "puVersion": "0.1.0.0",
      "puComponent": "lib",
      "puStyle": "local",
      "puDependencies": ["base-4.20.0.0", "aeson-2.2.3.0"],
      "puRebuildReason": null
    }
  ]
}
```

### 3.3 `GraphKnown`
Emitted when the module dependency graph within a unit has been topologically sorted.

```json
{
  "v": 1, "seq": 3, "t": 25000000, "src": "graph", "valueKind": "derived",
  "kind": "GraphKnown",
  "unit": "buildr-0.1.0.0-inplace",
  "modules": [
    {
      "name": "Buildr.Model",
      "sourcePath": "src/Buildr/Model.hs",
      "isBoot": false
    }
  ],
  "edges": [
    {
      "source": "Buildr.Model",
      "target": "Buildr.Event",
      "isSourceImport": false
    }
  ],
  "origin": "ghc-M"
}
```

### 3.4 Unit Lifecycle: `UnitStarted`, `UnitCompleted`, `UnitFailed`, `UnitStopped`
Tracks component-level execution.

```json
{ "v": 1, "seq": 4, "t": 30000000, "src": "cabal", "valueKind": "observed", "kind": "UnitStarted", "unit": "buildr-0.1.0.0-inplace" }
{ "v": 1, "seq": 20, "t": 4500000000, "src": "cabal", "valueKind": "observed", "kind": "UnitCompleted", "unit": "buildr-0.1.0.0-inplace", "exitCode": 0 }
{ "v": 1, "seq": 21, "t": 4600000000, "src": "cabal", "valueKind": "observed", "kind": "UnitFailed", "unit": "buildr-0.1.0.0-inplace", "exitCode": 1, "logPath": "/tmp/cabal-fail.log" }
```

### 3.5 `ModuleStarted`
Emitted when a compiler begins compiling an individual source module.

```json
{
  "v": 1, "seq": 5, "t": 35000000, "src": "ghc", "valueKind": "observed",
  "kind": "ModuleStarted",
  "unit": "buildr-0.1.0.0-inplace",
  "module": "Buildr.Model",
  "k": 1,
  "n": 12,
  "src": "src/Buildr/Model.hs",
  "obj": "dist/Buildr/Model.o",
  "reason": null
}
```

### 3.6 `ModulePhase`
Emitted as compiler passes complete (e.g. Typecheck, Desugar, CoreOpt, CodeGen).

```json
{
  "v": 1, "seq": 6, "t": 120000000, "src": "ghc", "valueKind": "reported",
  "kind": "ModulePhase",
  "unit": "buildr-0.1.0.0-inplace",
  "module": "Buildr.Model",
  "phase": "CodeGen",
  "ms": 85,
  "alloc": 10485760
}
```

### 3.7 `ModuleFinished` & `ModuleSkipped`
```json
{
  "v": 1, "seq": 7, "t": 135000000, "src": "ghc", "valueKind": "reported",
  "kind": "ModuleFinished",
  "unit": "buildr-0.1.0.0-inplace",
  "module": "Buildr.Model"
}
```

### 3.8 `Diagnostic`
Structured compiler warning or error.

```json
{
  "v": 1, "seq": 8, "t": 140000000, "src": "ghc", "valueKind": "reported",
  "kind": "Diagnostic",
  "unit": "buildr-0.1.0.0-inplace",
  "diagnostic": {
    "severity": "Error",
    "code": 12345,
    "span": {
      "file": "src/Buildr/Model.hs",
      "startLine": 42,
      "startCol": 10,
      "endLine": 42,
      "endCol": 25
    },
    "message": ["Couldn't match type 'Int' with 'Bool'"],
    "hints": ["In the first argument of 'not'"],
    "flag": "-Wdeferred-type-errors"
  }
}
```

### 3.9 `BuildDone`
Final event terminating the build stream.

```json
{
  "v": 1, "seq": 100, "t": 5200000000, "src": "buildr", "valueKind": "observed",
  "kind": "BuildDone",
  "outcome": "BuildSuccess",
  "counts": {
    "bcDone": 12,
    "bcFailed": 0,
    "bcBlocked": 0,
    "bcNotReached": 0,
    "bcSkipped": 0
  },
  "wallNs": 5200000000
}
```

---

## 4. Invariants & Producer Guarantees

1. **Ordering Guarantee**: Within a module compile cycle, `ModuleStarted` precedes all `ModulePhase` events, which precede `ModuleFinished`.
2. **Conservation of Units & Modules**: The count of finished, failed, blocked, and skipped modules equals the total planned count upon `BuildDone`.
3. **Graceful Fallback**: Unknown fields or extra keys MUST be ignored by consumers for forward compatibility within version 1.
