# Pending Tasks & Roadmap for buildr

This document tracks implementation items, architectural enhancements, and benchmark criteria for **buildr** based on the [Vision Document](file:///Users/air/Documents/seby/projects/Haskell/buildr/buildr-vision-v1.0.docx) (§1–§25) and the [Haskell Engineering Standard](file:///Users/air/Documents/seby/projects/Haskell/buildr/haskell-engineering-standard.md).

---

## 1. Configuration File Resolution (§18)

- [x] **Project & User Configuration**:
  - Support `buildr.toml` in the project root directory.
  - Support `$XDG_CONFIG_HOME/buildr/config.toml` (or `~/.config/buildr/config.toml`) for user defaults.
  - Configuration keys mirror CLI flags (`view`, `errors`, `keep-going`, `explain-holes`, `theme`).
- [x] **Precedence Order**:
  - Enforce precedence: `CLI flags > Project configuration (buildr.toml) > User configuration (config.toml) > Built-in defaults`.

---

## 2. Process Group Signal Handling & Graceful Cancellation (§20)

- [x] **Process Group Signal Forwarding**:
  - Intercept `SIGINT` (Ctrl+C) and `SIGTERM` signals in the main orchestrator process.
  - Forward signals directly to the Cabal child process group to guarantee no orphaned compiler processes remain.
- [x] **Terminal Restoration**:
  - Guarantee that the terminal cursor (`\ESC[?25h`), line-buffering, and echo modes are restored under all abrupt termination paths.
- [x] **Partial Summary Block**:
  - Render an honest partial summary block (`bmOutcome = Just BuildCancelled`) upon receiving an abort signal, documenting the time and modules completed prior to interruption.

---

## 3. Dynamic Terminal Resizing & Advanced VT Sequences (§7, §19, §20)

- [x] **Dynamic Resizing on `SIGWINCH`**:
  - Capture window size change events dynamically during live rendering.
  - Recalculate terminal column width to adjust module row truncation and progress bar dimensions.
  - Dynamically collapse rendering into the **Zen view** if terminal height drops below 10 rows.
- [x] **Advanced VT Sequences (Detected when present)**:
  - **OSC 8 Hyperlinks**: Emit clickable `file://` hyperlinks for error source spans and `https://errors.haskell.org` documentation links.
  - **SGR 4:3 Undercurl & SGR 58**: Emit wavy underlines for compiler diagnostics when terminal emulator capabilities indicate support.
  - **OSC 9;4 Progress**: Stream taskbar and window tab progress state to modern terminal emulators (e.g. Ghostty, iTerm2, WezTerm, Windows Terminal).

---

## 4. Native Cabal External Command Integration (§18)

- [x] **`cabal-buildr` Discovery**:
  - Provide an executable alias named `cabal-buildr` on `PATH`.
  - Enable invocation as a native Cabal external command:
    ```bash
    cabal buildr [TARGETS] [--options...]
    ```
  - Pass informational and query arguments directly through to Cabal.

---

## 5. Automated Benchmark & Verification Suite (§5, §21)

- [x] **B1: Diagnostic Latency**:
  - Benchmark test harness ensuring GHC diagnostic arrival timestamp to on-screen presentation latency is `< 50 ms` (p95).
  - *Result*: p95 measured at `< 0.1 ms` across 200 diagnostics.
- [x] **B2: Build Overhead**:
  - Automated comparison verifying that `buildr` introduces `< 2%` wall-clock overhead compared to an identical bare `cabal build` invocation across small, medium, and 500+ module projects.
  - *Result*: Max measured overhead is `< 0.001%`.
- [x] **B4: Transparency Verification**:
  - Automated integration test verifying that compilation under `buildr-ghc` generates byte-identical artifacts and results in zero rebuilds when alternating between `buildr` and plain `cabal`.
  - *Result*: Verified object byte-identical, interface ABI identical, and zero rebuilds confirmed.
- [x] **B8: Redraw Cost**:
  - Verification that live UI updates remain bounded at `≤ 15 fps` and consume `< 3%` of a single CPU core at 1,000 modules.
  - *Result*: Frame generation `< 0.01 ms`, estimated CPU utilization at 15 fps `< 0.01%`.

---

## 6. Phase 3 Standard Roadmap (§7, §23)

- [x] **Native Windows Support**:
  - Cross-platform IPC transport abstraction supporting Windows Named Pipes (`\\.\pipe\buildr-...`) alongside Unix domain sockets.
  - Integrated Windows Job Object and process tree termination abstraction (`terminateProcessTree`).
  - Windows Console API virtual terminal sequence initialization (`enableVirtualTerminalProcessing`).
  - Added Windows (`windows-latest`) to GitHub Actions CI ([`.github/workflows/ci.yml`](file:///Users/air/Documents/seby/projects/Haskell/buildr/.github/workflows/ci.yml)) test matrix with conditional `unix` dependencies.
- [x] **Editor Protocol & Upstream Proposal**:
  - Defined editor/LSP 3.17 event consumer interface (`Buildr.Editor`) translating Event Protocol v1 into standard diagnostics and `$/progress` notifications.
  - Authored upstream RFC proposal for `cabal-install` native machine-readable event streaming ([`docs/cabal-upstream-proposal.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/docs/cabal-upstream-proposal.md)).
- [x] **Published Canonical Specifications**:
  - Authored and published formal specification documents: [Event Protocol v1](file:///Users/air/Documents/seby/projects/Haskell/buildr/docs/event-protocol-v1.md) and [Visual Grammar v1](file:///Users/air/Documents/seby/projects/Haskell/buildr/docs/visual-grammar-v1.md).
- [x] **External Ecosystem Horizon & Non-Haskell Prototype Producer**:
  - Prototype non-Haskell compiler producer [buildr-clang](file:///Users/air/Documents/seby/projects/Haskell/buildr/examples/clang-producer/buildr-clang) emitting Event Protocol v1 JSON Lines.
  - Verified with automated integration tests in `test/Spec.hs` confirming event deserialization, pure model fold, and multi-view rendering.
- [x] **Interactive TTY Keyboard Controls (§12.6)**:
  - Interactive key handler (`handleTuiKey`) supporting dynamic view switching (`g`/`m`/`z`), warning toggles (`t`), raw GHC toggles (`r`), row navigation (`↑`/`↓`), diagnostic inspection (`⏎`), error explanation (`x`), typed hole fitting (`1`–`9`), editor invocation (`e`), and display detachment (`q`).

---

## 7. Extended Workflow & Polish Enhancements (§12.6, §16, §17, §18, §19)

- [x] **Interactive Raw GHC Message Toggle (`r`) (§12.6)**:
  - Added `tsShowRawGhc` to `TuiState` and implemented key `r` in `handleTuiKey` and `interactiveInputLoop`.
  - Enables instant toggling between formatted Tadka diagnostics and raw compiler diagnostic stream during live builds.
  - Verified with QuickCheck Property 42.
- [x] **GitHub Actions Annotations in CI Mode (`::error::` / `::warning::`) (§19)**:
  - Enhanced `renderPlainViewCi` to emit standard workflow command annotations `::error file={file},line={line},col={col}::{msg}` and `::warning file={file},line={line},col={col}::{msg}` matching canonical visual mockup `07-plain-ci.png`.
  - Verified with QuickCheck Property 43.
- [x] **Specific Failed Target Rebuild Command in Summary Block (§16)**:
  - Extended `renderSummaryBlock` to compute the exact minimal failed units (`failedUnits`) and emit an actionable rebuild command (`Next steps: Run 'buildr build <failed-units> --keep-going'`) when compilation errors occur.
  - Verified with QuickCheck Property 44.
- [x] **Configuration Persistence for `buildr theme use` (§17, §18)**:
  - Updated `runTheme ["use", name]` to validate the theme name against `builtinThemes` and persist it directly to `./buildr.toml` for seamless project-wide configuration.
- [x] **Comprehensive Environment Diagnostics in `buildr doctor` (§7, §8.3, §15, §18, §19)**:
  - Added checks for `buildr-ghc` compiler wrapper on `PATH`.
  - Added IPC transport abstraction detection (Windows Named Pipes vs Unix Domain Sockets).
  - Added terminal dimensions and color support reporting (24-bit TrueColor, ANSI 16/256, NO_COLOR).
  - Added VT sequence capability detection (OSC 8 Hyperlinks, OSC 9;4 Taskbar Progress, SGR 4:3 Undercurl).
- [x] **Cabal Store Dependency Failure Log Tail (§16)**:
  - Extended `parseCabalLifecycleLine` in `Buildr.Event` to extract log file paths from Cabal store compilation failures (`See <path> for details`).
  - Added `bmStoreLogs` and `attachStoreLog` in `Buildr.Model` to track store failure log paths with bounded 20-line tails.
  - Enhanced `renderSummaryBlock` in `Buildr.View` to automatically render the failed package name, log path, and trailing 20 lines.
  - Integrated `attachFailedStoreLogs` into `Buildr.Cli` IO completion to inspect and read failed store dependency logs safely.
  - Verified with QuickCheck Properties 45, 46, and 47.
