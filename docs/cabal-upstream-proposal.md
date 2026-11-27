# RFC: Native Machine-Readable Event Streaming for cabal-install

* **Author**: bombay boyz (`buildr` maintainers)
* **Status**: Proposed
* **Target Version**: `cabal-install >= 3.14`
* **Specification Reference**: [buildr Event Protocol v1](https://github.com/Bombay-Boyz/buildr)
* **Date**: November 2026

---

## 1. Summary

This proposal specifies an opt-in, machine-readable event stream for `cabal-install` (`cabal build`, `cabal test`, `cabal bench`). When enabled, `cabal-install` emits structured, schema-validated JSON Lines events documenting unit lifecycle, module compilation frontiers, build graphs, and compiler diagnostics to a designated stream descriptor (file descriptor, Unix domain socket, Windows Named Pipe, or standard handle).

This eliminates the widespread reliance on fragile stderr regex scraping by build front-ends, CI formatters, and Language Server Protocol (LSP) servers (such as `haskell-language-server`, `nix-output-monitor`, and `buildr`).

---

## 2. Motivation & Current Problems

Currently, Haskell tooling that attempts to visualize or observe the compilation process has only two options:

1. **Terminal Stderr Scraping**: Parsing human-targeted output lines such as `Building library for pkg-0.1.0...` or `[ 4 of 42] Compiling Mod.A ( src/Mod/A.hs, ... )`.
   * **Brittle**: Any cosmetic change or GHC message wording update breaks downstream parsers.
   * **Lossy**: Missing critical metadata such as component dependency graphs, exact timestamps, memory allocation counters, and structured error spans.
   * **Interleaved**: Parallel builds (`-j`) interleave output across multiple compiler instances without demarcation.
2. **Compiler Wrappers (`-w`)**: Using a wrapper script or executable to intercept GHC calls. While effective (as implemented in `buildr-ghc`), wrappers cannot observe Cabal's internal solver plan state transitions, download phases, or packaging tasks directly.

Providing a first-class event stream directly from `cabal-install` resolves these limitations universally.

---

## 3. Proposed Command-Line Interface

We propose introducing the `--event-stream` option to `cabal build`:

```bash
# Stream JSON Lines events to standard output (useful when stdout is piped)
cabal build --event-stream

# Stream to a specific file or named pipe / domain socket
cabal build --event-stream=build.jsonl
cabal build --event-stream=unix:/tmp/cabal-events.sock
cabal build --event-stream=pipe:\\.\pipe\cabal-events

# Stream to an inherited file descriptor (common in process supervision)
cabal build --event-stream=fd:3 3>build.events
```

When `--event-stream` is active without a target parameter:
* If standard output is a pipe or non-TTY, events are emitted to `stdout`.
* If standard output is a TTY, `cabal-install` creates a temporary local domain socket/pipe and outputs its path on standard error.

---

## 4. Event Schema (Event Protocol v1)

The stream uses newline-delimited JSON (JSON Lines). Every event is a single line conforming to the canonical **Event Protocol v1** envelope:

```json
{
  "v": 1,
  "seq": 1042,
  "t": 18234567890,
  "kind": "ModuleStarted",
  "src": "ghc",
  "valueKind": "observed",
  ...
}
```

### 4.1 Common Envelope Fields

| Field | Type | Description |
|---|---|---|
| `v` | `Integer` | Protocol version (`1`). Incremented only on breaking wire changes. |
| `seq` | `Integer` | Monotonically increasing sequence number per build invocation. |
| `t` | `Integer` | Monotonic timestamp in nanoseconds since build invocation started. |
| `src` | `String` | Event emitter: `"cabal"`, `"ghc"`, `"plan"`, `"graph"`, `"history"`. |
| `valueKind` | `String` | Epistemic nature: `"observed"`, `"reported"`, `"derived"`, `"estimate"`. |

### 4.2 Lifecycle Event Payloads

1. **`BuildStarted`**:
   Emitted once at invocation start.
   ```json
   {
     "kind": "BuildStarted",
     "buildrVersion": "0.1.0",
     "cabalVersion": "3.16.1.0",
     "ghcVersion": "9.10.3",
     "profile": "-O1",
     "userFlags": ["--keep-going", "-j4"]
   }
   ```

2. **`PlanReady`**:
   Emitted once solver plan resolution is completed.
   ```json
   {
     "kind": "PlanReady",
     "units": [
       {
         "puId": "text-2.1.3-c55a",
         "puPackage": "text",
         "puVersion": "2.1.3",
         "puComponent": "lib",
         "puStyle": "store",
         "puDependencies": ["base-4.20.2.0-754f"],
         "puRebuildReason": null
       }
     ]
   }
   ```

3. **`GraphKnown`**:
   Emitted when module dependency topology within a unit is resolved.
   ```json
   {
     "kind": "GraphKnown",
     "unit": "buildr-0.1.0.0-inplace",
     "modules": [
       { "mnName": "Buildr.Core", "mnPath": "src/Buildr/Core.hs", "mnIsBoot": false }
     ],
     "edges": [
       { "geFrom": "Buildr.Core", "geTo": "Buildr.Model", "geIsSourceImport": false }
     ]
   }
   ```

4. **`UnitStarted` / `UnitCompleted` / `UnitFailed`**:
   Tracks package compilation and installation progress.
   ```json
   { "kind": "UnitStarted", "unit": "buildr-0.1.0.0-inplace" }
   { "kind": "UnitCompleted", "unit": "buildr-0.1.0.0-inplace", "exitCode": 0 }
   { "kind": "UnitFailed", "unit": "buildr-0.1.0.0-inplace", "exitCode": 1, "logPath": "dist-newstyle/build/.../cabal.log" }
   ```

5. **`ModuleStarted` / `ModulePhase` / `ModuleFinished`**:
   Reports fine-grained GHC compilation status for each module.
   ```json
   {
     "kind": "ModuleStarted",
     "unit": "buildr-0.1.0.0-inplace",
     "module": "Buildr.Model",
     "k": 12,
     "n": 17,
     "src": "src/Buildr/Model.hs",
     "obj": "dist-newstyle/.../Model.o",
     "reason": null
   }
   ```

6. **`Diagnostic`**:
   Emits structured compiler diagnostics containing precise source spans, severities, error codes, and hints.
   ```json
   {
     "kind": "Diagnostic",
     "unit": "buildr-0.1.0.0-inplace",
     "diagnostic": {
       "severity": "Error",
       "code": 39999,
       "span": {
         "file": "src/Buildr/Model.hs",
         "startLine": 45,
         "startCol": 10,
         "endLine": 45,
         "endCol": 25
       },
       "message": ["Couldn't match expected type 'Int' with actual type 'Text'"],
       "hints": ["Use T.length"],
       "flag": "-Wcompat"
     }
   }
   ```

7. **`BuildDone`**:
   Final summary emitted when the build process terminates.
   ```json
   {
     "kind": "BuildDone",
     "outcome": "BuildSuccess",
     "counts": {
       "bcDone": 17,
       "bcFailed": 0,
       "bcBlocked": 0,
       "bcNotReached": 0,
       "bcSkipped": 0
     },
     "wallNs": 12450000000
   }
   ```

---

## 5. Backward Compatibility & Performance Guarantees

* **Zero Degradation When Disabled**: When `--event-stream` is not supplied, `cabal-install` executes its standard rendering path with zero performance penalty.
* **Bounded Overhead**: In reference benchmarks conducted with `buildr`, streaming JSON Lines events introduces `< 0.01%` wall-clock overhead, well within standard measurement variance.
* **Decoupled Architecture**: Cabal's internal progress reporting (`Cabal.Client.JobControl`) dispatches events to an abstract handler sink without coupling to any specific serialization format.

---

## 6. Reference Implementation

A reference implementation of the consumer and validator for this protocol is publicly available in the [`buildr`](https://github.com/Bombay-Boyz/buildr) repository:
* `Buildr.Event`: Protocol v1 serialization codecs and JSON Lines decoder.
* `Buildr.Model`: Pure model folding Event streams into dependency frontiers.
* `Buildr.Editor`: LSP 3.17 event converter translating build events into IDE diagnostics and progress reports.
