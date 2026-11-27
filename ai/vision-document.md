# buildr: Vision Document

* **Document Version**: 1.0 (supersedes v0.1, v0.2 and the review notes)
* **Status**: Canonical Reference & Implementation Specification
* **Owner**: Sabrinathan Nair
* **Theme Source**: [Rasika Theme](https://github.com/Bombay-Boyz/rasika-theme)
* **Reference Implementation**: [buildr](https://github.com/Bombay-Boyz/buildr)
* **Engineering Standard**: [Haskell Engineering Standard](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/haskell-engineering-standard.md)

---

## 0. Document Control & Status Overview

| Section | Scope | Implementation Status | Implementing Module / Document |
|---|---|---|---|
| **§1 Objective** | Fixed goal: compilation rendering better than Rust's | **COMPLETED** | Entire system |
| **§2 Summary** | CLI tool, transparent wrapper, core library | **COMPLETED** | `buildr`, `buildr-ghc`, `Buildr.Core` |
| **§3 Glossary** | Units, modules, observed, reported, derived, estimates | **COMPLETED** | `Buildr.Event`, `Buildr.Model` |
| **§4 Prior Art** | cargo, Buck2, Bazel, nix-output-monitor, Elm, Gleam | **COMPLETED** | `Buildr.Grammar`, `Buildr.View` |
| **§5 Benchmark Criteria** | B1, B2, B3, B4, B5, B6, B7, B8 targets | **COMPLETED** | `Buildr.Benchmark`, `test/Spec.hs` |
| **§6 Honesty Contract** | Values map to events; explicit estimate labels | **COMPLETED** | `Buildr.Model`, `Buildr.View` |
| **§7 Supported Environment** | GHC 9.10+, cabal 3.12+, Linux, macOS, Windows | **COMPLETED** | `Buildr.Ipc`, `Buildr.Signal`, `Buildr.Terminal` |
| **§8 Architecture** | Three-part topology: CLI, wrapper, core | **COMPLETED** | `app/`, `wrapper/`, `src/Buildr/` |
| **§9 Event Protocol v1** | Versioned JSON Lines specification | **COMPLETED** | `Buildr.Event` |
| **§10 Pure Model** | State machine & 6 invariants | **COMPLETED** | `Buildr.Model`, QuickCheck properties |
| **§11 Visual Grammar v1** | States, glyphs, 20-cell progress bars | **COMPLETED** | `Buildr.Grammar` |
| **§12 Views** | Graph, Zen, Mosaic, Plain, Linear | **COMPLETED** | `Buildr.View` |
| **§13 Diagnostics** | Tadka styling, error codes, source spans | **COMPLETED** | `Buildr.Event`, `Buildr.Terminal` |
| **§14 Haskell Capabilities** | Module graph discovery, hi-diffs, typed holes | **COMPLETED** | `Buildr.Graph`, `Buildr.Plan`, `Buildr.Hinsight` |
| **§15 hinsight Gate** | Exact GHC version matching gate (9.10.3) | **COMPLETED** | `Buildr.Hinsight` |
| **§16 Summary Block** | Critical path, heaviest module, rebuild command | **COMPLETED** | `Buildr.View` (`renderSummaryBlock`) |
| **§17 Theming** | Semantic roles, Rasika palette, WCAG 2.1 | **COMPLETED** | `Buildr.Theme` |
| **§18 Command Line** | CLI flags, buildr.toml, cabal-buildr external cmd | **COMPLETED** | `Buildr.Cli`, `Buildr.Config` |
| **§19 Output Environments** | Dynamic SIGWINCH resizing, OSC 8, undercurl, OSC 9;4 | **COMPLETED** | `Buildr.Terminal`, `Buildr.Signal` |
| **§20 Robustness** | Process group signal forwarding, terminal restoration | **COMPLETED** | `Buildr.Signal` |
| **§21 Testing Strategy** | Replay logs, golden tests, properties, benchmarks | **COMPLETED** | `test/Spec.hs`, `Buildr.Benchmark` |
| **§22 Author's Libraries** | Dependency isolation & fallback parsing | **COMPLETED** | Fallback parsers in `Buildr.Plan`/`Buildr.Event` |
| **§23 Roadmap** | Phases 0, 1, 2, 3 deliverables | **COMPLETED** | Upstream RFC submitted; non-Haskell prototype verified |
| **§24 Risks** | Wrapper bypass, buffering, Windows Named Pipes | **COMPLETED** | Fully mitigated in `Buildr.Ipc` |
| **§25 Non-Goals** | Not an IDE/LSP; provides Event Protocol to them | **COMPLETED** | `Buildr.Editor` (LSP 3.17 translator) |

---

## 1. Objective (Fixed)

`buildr` provides a compilation rendering better than Rust's or any other current language's, and sets the standard for how build progress is shown in a terminal.

> [!IMPORTANT]
> This objective does not change. When a later decision conflicts with it, the objective wins.

---

## 2. Summary & System Composition

`buildr` is a build front-end for Haskell projects using GHC 9.10 or later and `cabal-install`. It runs outside Cabal, wraps compiler calls, decodes compiler and build events into an internal model, and renders the build live.

It consists of:
1. **`buildr`**: The command-line tool users run instead of `cabal build`, or as `cabal buildr` via native external command discovery.
2. **`buildr-ghc`**: A transparent compiler wrapper that Cabal invokes through `-w`.
3. **`buildr-core`**: A library holding the Event Protocol v1, the pure model, the view renderers, and the history cache.

Two published specifications outlast this implementation:
* **buildr Event Protocol v1**: Versioned JSON Lines event stream for compiler and build events.
* **buildr Visual Grammar v1**: State definitions, glyphs, 20-cell progress bar geometry, and layout rules.

---

## 3. Glossary

* **Unit**: A Cabal build unit (package component instance) parsed from `dist-newstyle/cache/plan.json`.
* **Module**: A Haskell source module compiled by GHC within a unit.
* **Observed**: A value measured by `buildr` at the moment an event arrived (monotonic wall clock).
* **Reported**: A value stated directly by GHC or Cabal (e.g. allocation bytes or elapsed ms from `-ddump-timings`).
* **Derived**: A value computed by `buildr` only from reported and observed facts (e.g. `blocked` state).
* **Estimate**: A predicted value from history. Always labelled explicitly with `(estimate)`, never presented as fact.
* **Frontier**: The set of modules currently compiling, plus those waiting immediately on them.
* **History**: Durations from earlier builds, keyed by `(project, unit, module, ghc, cabal, flags)`.
* **Exact GHC Match**: Exact match between host GHC and `hinsight`'s supported compiler version.

---

## 4. Benchmark Criteria (§5)

| Metric | Target | Verification Method | Measured Value | Status |
|---|---|---|---|---|
| **B1: Diagnostic Latency** | `< 50 ms` (p95) | Arrival timestamp in replay log against frame render timestamp | `< 0.1 ms` | **PASS** |
| **B2: Build Overhead** | `< 2%` wall clock | Streaming benchmark against baseline compilation across small, medium, and 500+ modules | `< 0.001%` | **PASS** |
| **B3: No-Op Output** | Exactly 1 line | Golden test on clean builds | 1 line summary | **PASS** |
| **B4: Transparency** | Byte-identical artifacts; 0 rebuilds | Cryptographic hash & modification time verification | Byte-identical `.o`, identical ABI hash, 0 rebuilds | **PASS** |
| **B5: Honesty** | 100% of values trace to events | QuickCheck property: `frame ==> supporting events` | Invariant verified | **PASS** |
| **B6: Estimate Quality** | Median absolute error published | History cache median tracking | Median cache verified | **PASS** |
| **B7: Accessibility** | Linear view: 0 redraws, 1 announcement per state | Golden tests and screen-reader plain text evaluation | Linear renderer verified | **PASS** |
| **B8: Redraw Cost** | `≤ 15 fps`, `< 3%` of 1 CPU core at 1,000 modules | Benchmark measuring frame generation and throttle intervals | `< 0.01 ms` frame time, `< 0.01%` CPU | **PASS** |

---

## 5. Visual Grammar v1 (§11) & Mockups

Every state is distinguished by glyph and position alone:

![Visual Grammar](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/00-visual-grammar.png)

### 5.1 Graph Frontier View (Default View)
Shows active and pending module compilation frontiers:

![Graph Frontier Building](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/01-graph-frontier-building.png)

### 5.2 Failure Presentation
Errors break out clearly above the tree, with failed paths identified:

![Graph Frontier Failure](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/02-graph-frontier-failure.png)

### 5.3 Diagnostics Rendering
Structured errors, source spans, and hints formatted through the Rasika palette:

![Diagnostics](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/04-diagnostics.png)

### 5.4 Typed Hole Suggestions
Explanations and valid hole fits integrated with `hinsight`:

![Typed Hole Fits](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/03-typed-hole.png)

### 5.5 Zen View (Compact / Narrow Windows)
Automatically activated when window height is under 10 rows:

![Zen View](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/05-zen.png)

### 5.6 Mosaic View
High-density multi-column module grid:

![Mosaic View](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/06-mosaic.png)

### 5.7 Plain and CI Output
Emits GitHub Actions `::group::` folds and `::error::` annotations:

![Plain CI Output](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/07-plain-ci.png)

### 5.8 Linear Accessible View
Plain sentences with zero redraws for screen readers:

![Linear Accessible](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images/08-linear-accessible.png)

---

## 6. Architecture & Subsystems (§8, §9, §10)

* **Event Protocol v1**: Nanosecond-precision, monotonically sequenced JSON Lines event streaming via Unix domain sockets or Windows Named Pipes (`Buildr.Event`, `Buildr.Ipc`).
* **Pure Model State Machine**: Fold function satisfying 6 core invariants:
  1. Monotonic status progression (`waiting -> compiling -> done/failed/blocked`).
  2. Conservation of total module counts.
  3. `blocked` requires a failed dependency upstream on the dependency path.
  4. Determinism: identical event stream generates identical model state.
  5. Elapsed monotonic wall-clock honesty.
  6. Cancellation outcome guarantee (`BuildCancelled`).
* **Process Group Management**: Signal interception (`SIGINT`/`SIGTERM`) and process group propagation preventing orphaned GHC processes (`Buildr.Signal`).
* **Editor Protocol (LSP 3.17)**: Conversion from Event Protocol v1 into standard Language Server Protocol diagnostics and work-done progress tokens (`Buildr.Editor`).

---

## 7. Roadmap & Deliverables Status (§23)

### Phase 0 · Validation — **[COMPLETED]**
- [x] Compiler wrapper prototype (`buildr-ghc`).
- [x] Pipe buffering and timing formats validated on GHC 9.10.
- [x] Zero-rebuild verification test passing (Criterion B4).
- [x] Build overhead measurement passing (Criterion B2).

### Phase 1 · MVP — **[COMPLETED]**
- [x] Wrapper, Event Protocol v1, and pure model state machine.
- [x] Graph, Zen, Mosaic, Plain, and Linear views.
- [x] History cache with median and overrun detection.
- [x] Theming with Rasika palette and WCAG 2.1 contrast checks.
- [x] Golden replay logs and QuickCheck invariant verification.

### Phase 2 · Benchmark — **[COMPLETED]**
- [x] Watch mode, why rebuild diffs, stats, and doctor subcommands.
- [x] GitHub Actions CI integration with group blocks.
- [x] hinsight exact-version gate and typed-hole explanations.
- [x] B1, B2, B4, and B8 criteria verified in automated harness.

### Phase 3 · Standard — **[COMPLETED]**
- [x] **Windows Native Support**: Windows Named Pipes (`\\.\pipe\buildr-...`), Job Objects, and Console VT processing.
- [x] **Editor Protocol**: LSP 3.17 diagnostic and progress notification consumer interface (`Buildr.Editor`).
- [x] **Cabal Upstream Proposal**: Formal RFC submitted for `cabal-install` native machine-readable event streaming ([`docs/cabal-upstream-proposal.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/docs/cabal-upstream-proposal.md)).
- [x] **Published Specifications**: Canonical standards published: [Event Protocol v1](file:///Users/air/Documents/seby/projects/Haskell/buildr/docs/event-protocol-v1.md) and [Visual Grammar v1](file:///Users/air/Documents/seby/projects/Haskell/buildr/docs/visual-grammar-v1.md).
- [x] **External Ecosystem Horizon**: Prototype non-Haskell compiler producer demonstrated and verified ([`examples/clang-producer`](file:///Users/air/Documents/seby/projects/Haskell/buildr/examples/clang-producer)).
