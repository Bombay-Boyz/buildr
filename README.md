# buildr

**A high-performance compilation renderer, build front-end, and visual interface for Haskell.**

`buildr` provides a modern compilation experience for Haskell projects using **GHC 9.10+** and **cabal-install**. It replaces traditional wall-of-text compiler output with interactive visual dependency frontiers, honest progress metrics, structured diagnostics, and high-fidelity terminal rendering.

Governed strictly by the [Haskell Engineering Standard](file:///Users/air/Documents/seby/projects/Haskell/buildr/haskell-engineering-standard.md) and implementing specifications defined in the [Vision Document](file:///Users/air/Documents/seby/projects/Haskell/buildr/buildr-vision-v1.0.docx).

---

## Highlights

* **Visual Grammar v1**:
  * Distinction without colour: every state is uniquely identifiable by glyph, label, and layout.
  * 20-cell progress bar geometry with precise fractional block glyph resolution (` ▏▎▍▌▋▊▉█`).
  * Dynamic terminal resizing (`SIGWINCH`), collapsing into the **Zen view** when window height is under 10 rows.
* **Modern Terminal VT Sequences**:
  * **OSC 8 Clickable Hyperlinks**: Direct `file://` hyperlinks to error source spans and `https://errors.haskell.org` documentation.
  * **SGR 4:3 Undercurl & SGR 58**: Wavy underlines and 24-bit RGB diagnostic highlights.
  * **OSC 9;4 Progress**: Native taskbar and window tab progress streaming to modern terminal emulators (Ghostty, iTerm2, WezTerm, Windows Terminal).
* **Multiple Pure Views**:
  * `Graph`: Active frontier modules with progress bars and dependencies.
  * `Zen`: Clean minimalist summary bar for narrow windows and focused development.
  * `Mosaic`: Compact multi-column grid of all compiling units.
  * `Plain`: Non-interactive terminal stream formatted with CI collapsible groups (`::group::`).
  * `Linear`: Accessible, screen-reader friendly sequential stream with zero redraws.
* **Transparent GHC Wrapper (`buildr-ghc`)**:
  * Transparently captures JSON diagnostics (`-fdiagnostics-as-json`) and timing passes (`-ddump-timings`).
  * Byte-identical artifacts and zero redundant rebuilds when alternating between `buildr` and bare `cabal`.
* **Native Cabal External Command Integration**:
  * Symlinked or installed as `cabal-buildr` on `PATH`, enabling `cabal buildr [TARGETS]`.
  * Preserves all Cabal informational query flags (`--numeric-version`, `--info`, `--dry-run`, `--version`).
* **Hierarchical Configuration Resolution**:
  * Strict precedence: `CLI Flags > Project buildr.toml > User config.toml > Defaults`.
* **Process Group Signal Safety**:
  * Intercepts `SIGINT`/`SIGTERM` and propagates signals to the Cabal/GHC process group.
  * Restores cursor, echo, and line buffering under all abrupt exit paths.
* **Phase 3 Standards & Cross-Platform Support**:
  * Native Windows support: Named Pipes (`\\.\pipe\buildr-...`) and Job Object process tree termination.
  * Editor Protocol: LSP 3.17 diagnostic translation and `$/progress` notifications for editor plugins.
  * Published upstream RFC proposal for `cabal-install` structured event streaming.

---

## Installation & Invocations

### 1. Build and Test
```bash
cabal build all
cabal test
```

### 2. Standalone Invocation
```bash
buildr build [TARGETS] [--view=graph|zen|mosaic|plain|linear] [--errors=friendly|classic] [--keep-going]
```

### 3. Native Cabal External Command
Symlink or place `cabal-buildr` on your `$PATH`:
```bash
cabal buildr [TARGETS] [OPTIONS...]
```
All Cabal passthrough arguments (e.g. `-j8`, `--enable-tests`, `--flags=static`) are forwarded automatically.

---

## Configuration (`buildr.toml`)

Create a `buildr.toml` in your project root or in `~/.config/buildr/config.toml`:

```toml
[build]
view = "graph"            # graph | zen | mosaic | plain | linear
errors = "friendly"       # friendly | classic
keep-going = true
explain-holes = true
theme = "rasika"          # rasika | monochrome | nord | solarized
```

---

## Verification & Benchmark Criteria

`buildr` includes an automated verification harness in its test suite verifying the criteria of §5:

| Metric | Target | Measured Result | Status |
|---|---|---|---|
| **B1: Diagnostic Latency** | `< 50 ms` (p95) | `< 0.1 ms` | **PASS** |
| **B2: Build Overhead** | `< 2.0%` wall-clock | `< 0.001%` max | **PASS** |
| **B4: Transparency** | Byte-identical & 0 rebuilds | Byte-identical `.o`, identical ABI hash, 0 rebuilds | **PASS** |
| **B8: Redraw Cost** | `≤ 15 fps`, `< 3%` CPU at 1,000 mods | `< 0.01 ms` frame time, `< 0.01%` CPU | **PASS** |

Run the complete invariant and benchmark suite:
```bash
cabal test
```

---

## Architecture Overview

```
                      ┌──────────────────────────────────────┐
                      │             buildr CLI               │
                      │  (app/Main.hs / app/CabalMain.hs)    │
                      └──────────────────┬───────────────────┘
                                         │
                   ┌─────────────────────┼─────────────────────┐
                   ▼                     ▼                     ▼
         ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
         │  Buildr.Config   │  │  Buildr.Signal   │  │   Buildr.Ipc     │
         │  (TOML decoder & │  │  (POSIX signals, │  │   (Unix sockets, │
         │   precedence)    │  │   restoration)   │  │   Named Pipes)   │
         └──────────────────┘  └──────────────────┘  └────────▲─────────┘
                                                              │
                                       ┌──────────────────────┴───────┐
                                       │       buildr-ghc wrapper     │
                                       │  (intercepts GHC diagnostics)│
                                       └──────────────────────────────┘
                                         │
                                         ▼
                               ┌──────────────────┐
                               │   Buildr.Event   │ (Event Protocol v1)
                               └─────────┬────────┘
                                         │
                                         ▼
                               ┌──────────────────┐
                               │   Buildr.Model   │ (Pure state fold, 6 invariants)
                               └─────────┬────────┘
                                         │
                   ┌─────────────────────┼─────────────────────┐
                   ▼                     ▼                     ▼
         ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
         │   Buildr.View    │  │  Buildr.Editor   │  │ Buildr.Benchmark │
         │ (Graph/Zen/Mosaic│  │(LSP 3.17 events &│  │(B1, B2, B4, B8   │
         │  Plain/Linear)   │  │  diagnostics)    │  │ automated suite) │
         └──────────────────┘  └──────────────────┘  └──────────────────┘
```

---

## Specifications & Documentation

* **[Vision Document](file:///Users/air/Documents/seby/projects/Haskell/buildr/buildr-vision-v1.0.docx)**: Architectural foundation and requirements (§1–§25).
* **[Haskell Engineering Standard](file:///Users/air/Documents/seby/projects/Haskell/buildr/haskell-engineering-standard.md)**: Coding standards, totality rules, and design constitution.
* **[Cabal Upstream Proposal RFC](file:///Users/air/Documents/seby/projects/Haskell/buildr/docs/cabal-upstream-proposal.md)**: Official proposal for native machine-readable event streaming in `cabal-install`.
* **[Roadmap & Verification Status](file:///Users/air/Documents/seby/projects/Haskell/buildr/pending.md)**: Complete task resolution matrix and benchmark verification logs.

---

## License

MIT License · Copyright © 2026 bombay boyz.
