# buildr

[![CI](https://github.com/Bombay-Boyz/buildr/actions/workflows/ci.yml/badge.svg)](https://github.com/Bombay-Boyz/buildr/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Haskell](https://img.shields.io/badge/language-GHC_9.10%2B-purple.svg)](https://www.haskell.org/)

**buildr** is a benchmark-setting compilation renderer and build front-end for Haskell projects using **GHC 9.10 or later** and **cabal-install 3.12–3.16**. It runs Cabal and GHC unchanged and renders the build as a live, dependency-aware view, with a progress bar on every row, an honest overall progress bar, and best-in-class diagnostics.

---

## Core Principles & The Honesty Contract (§1, §6)

1. **Honesty above all**: Everything displayed traces directly to data emitted by GHC, Cabal, or the file system. Every visible value has exactly one kind:
   - `observed`: Wall-clock/monotonic duration measured at arrival.
   - `reported`: Stated directly by GHC or Cabal (e.g., `-ddump-timings`).
   - `derived`: Computed solely from reported and observed facts (e.g., `blocked`, `not reached`).
   - `estimate`: Predicted from rolling history; always marked (`~`, `usual`, `est.`).
2. **Distinction without colour**: Every visual state is distinct by glyph and position alone (§11), ensuring 100% accessibility on monochrome terminals or for colour-blind users.
3. **Zero interference**: Runs Cabal and GHC without diverting standard streams or altering build hashes.

---

## Architecture (§8)

```
                     ┌──────────────── buildr bin dir (prepended; passed as cabal -w) ─────────────────┐
buildr ──► cabal ─┤  ghc, ghc-9.x  = buildr-ghc wrapper      ghc-pkg, ghc-pkg-9.x = pass-through        │
   │              │  hsc2hs, haddock … = pass-through                                                  │
   │              └──────┬──────────────────────────────────────────────────────────────────────────────┘
   │                     │ tee: everything to cabal unchanged + copy to buildr socket (unit id from -this-unit-id)
   │                     ▼
   │   cabal stdout ─► Sources ─► Event Protocol v1 ─► Model (pure fold) ─► Views ─► Terminal
   │   plan.json ────►   │              │                    │
   │   ghc -M graph ─►   │              ├─► --record log     └─► History cache ($XDG_CACHE_HOME/buildr)
   │                     │              └─► replay log (.jsonl)
   └─ on demand, exact GHC match only ─► hinsight gate
```

- **`buildr`**: CLI runner, process orchestrator, IPC server, and terminal UI renderer.
- **`buildr-ghc`**: Transparent compiler wrapper. Passes informational queries (`--info`, `--numeric-version`) untouched. During compilation passes, streams `ModuleStarted` and `ModuleFinished` records to `BUILDR_SOCKET` over a Unix domain socket.
- **`buildr-core`**: Pure domain library holding the Event Protocol v1 codecs, state machine, Visual Grammar renderers, theme system, and history cache.

---

## Specifications

### 1. Visual Grammar v1 (§11)

| State | Glyph | Bar (20 cells) | Meaning |
|---|:---:|:---:|---|
| **done** | `●` | `[████████████████████]` | Reported compile duration and memory allocation |
| **compiling** | `◐` | `[████████▎           ]` | Monotonic elapsed time vs usual estimate with eighth-block precision |
| **overrun** | `◐` | `[████████████████████]` | Slower than usual (> 1.5× median); labeled `+Xs vs usual` |
| **waiting** | `○` | `[                    ]` | Waiting on dependencies; shows finished imports count |
| **skipped** | `◇` | `[                    ]` | Interface unchanged (from GHC recompilation checker) |
| **failed** | `✖` | `[                    ]` | Module compilation failed (`✖ n errors`) |
| **blocked** | `⊘` | `[╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳]` | Upstream dependency failed |
| **not reached** | `◌` | `[                    ]` | Unit terminated before module was reached |

### 2. Event Protocol v1 (§9)

Structured JSON Lines stream. Each record contains an envelope:
```json
{
  "v": 1,
  "seq": 1042,
  "t": 18234567890,
  "kind": "ModuleStarted",
  "src": "ghc",
  "valueKind": "observed",
  "unit": "buildr-0.1.0.0-inplace",
  "module": "Buildr.Model",
  "k": 5,
  "n": 10,
  "src": "src/Buildr/Model.hs",
  "obj": "dist-newstyle/build/.../Buildr/Model.o"
}
```

---

## CLI Reference (§18)

```bash
# Run compilation with live rendering
buildr build [TARGETS] [--view graph|zen|mosaic|plain|linear] [--keep-going] [--record FILE.jsonl]

# Rebuild on change in Zen mode
buildr watch [TARGETS]

# Replay a recorded build log file through the pure model
buildr replay FILE.jsonl [--view VIEW]

# Inspect rebuild reasons and interface diffs
buildr why MODULE [--unit U]

# Display estimate accuracy and timing statistics
buildr stats [--project DIR]

# Preview, check, and manage terminal themes
buildr theme [list|preview|check|use]

# Environment diagnostic and compiler check
buildr doctor
```

---

## Views (§12)

- **Graph Frontier (default)**: Dynamic tree showing running modules, phase breakdown, 20-cell eighth-block bars, and unit summaries.
- **Zen (`--view zen`)**: Single-line compact progress bar. Automatically enabled when terminal height is < 10 rows.
- **Mosaic (`--view mosaic`)**: Dense character-matrix where each module is represented by a single cell.
- **Plain / CI (`--view plain`)**: Non-interactive, append-only output with automatic GitHub Actions `::group::` annotations.
- **Linear (`--view linear`)**: Accessible screen-reader mode with human announcements at milestone intervals.

---

## The Rasika Theme (§17)

The canonical **Rasika** dark theme is bundled by default. It provides high-contrast truecolor rendering while ensuring all text roles pass the **WCAG 2.1 minimum 4.5:1 contrast ratio**:

```bash
buildr theme check rasika
```
```
Checking contrast ratios for theme 'rasika' against WCAG 2.1 (minimum 4.5:1):
  [PASS] chrome.text: 13.0:1
  [PASS] chrome.text-strong: 17.4:1
  [PASS] diag.label-primary: 17.4:1
  [PASS] diag.label-secondary: 8.2:1
  [PASS] state.done: 8.6:1
  [PASS] state.compiling: 7.4:1
  [PASS] state.overrun: 10.1:1
  [PASS] state.failed: 5.4:1
```

---

## Verification & Testing (§21)

All pure model transitions and visual specifications are rigorously verified using **QuickCheck** and integration suites:

```bash
cabal test
```
- **Invariants 1–6 (§10.1)**: Pure model state machine totality and consistency.
- **Round-Trip Serialization (§9)**: Lossless Event Protocol v1 JSON encoding and decoding.
- **Bar Bounds (§11)**: Strict width limits and eighth-block fractional rendering.
- **Median & Overruns (§10.3)**: Correct median calculation and honest overrun triggering.
- **IPC Transport (§8.2)**: Asynchronous Unix domain socket streaming.
- **Golden Replay Logs (§21)**: Deterministic playback of recorded builds.

---

## License

MIT © 2026 bombay boyz
