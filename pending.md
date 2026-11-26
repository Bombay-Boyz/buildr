# Pending Tasks & Roadmap for buildr

This document tracks pending implementation items, architectural enhancements, and benchmark criteria for **buildr** based on the [Vision Document](file:///Users/air/Documents/seby/projects/Haskell/buildr/buildr-vision-v1.0.docx) (§1–§25) and the [Haskell Engineering Standard](file:///Users/air/Documents/seby/projects/Haskell/buildr/haskell-engineering-standard.md).

---

## 1. Configuration File Resolution (§18)

- [ ] **Project & User Configuration**:
  - Support `buildr.toml` in the project root directory.
  - Support `$XDG_CONFIG_HOME/buildr/config.toml` (or `~/.config/buildr/config.toml`) for user defaults.
  - Configuration keys should mirror CLI flags (`view`, `errors`, `keep-going`, `explain-holes`, `theme`).
- [ ] **Precedence Order**:
  - Enforce precedence: `CLI flags > Project configuration (buildr.toml) > User configuration (config.toml) > Built-in defaults`.

---

## 2. Process Group Signal Handling & Graceful Cancellation (§20)

- [ ] **Process Group Signal Forwarding**:
  - Intercept `SIGINT` (Ctrl+C) and `SIGTERM` signals in the main orchestrator process.
  - Forward signals directly to the Cabal child process group to guarantee no orphaned compiler processes remain.
- [ ] **Terminal Restoration**:
  - Guarantee that the terminal cursor (`\ESC[?25h`), line-buffering, and echo modes are restored under all abrupt termination paths.
- [ ] **Partial Summary Block**:
  - Render an honest partial summary block (`bmOutcome = Just BuildCancelled`) upon receiving an abort signal, documenting the time and modules completed prior to interruption.

---

## 3. Dynamic Terminal Resizing & Advanced VT Sequences (§7, §19, §20)

- [ ] **Dynamic Resizing on `SIGWINCH`**:
  - Capture window size change events dynamically during live rendering.
  - Recalculate terminal column width to adjust module row truncation and progress bar dimensions.
  - Dynamically collapse rendering into the **Zen view** if terminal height drops below 10 rows.
- [ ] **Advanced VT Sequences (Detected when present)**:
  - **OSC 8 Hyperlinks**: Emit clickable `file://` hyperlinks for error source spans and `https://errors.haskell.org` documentation links.
  - **SGR 4:3 Undercurl & SGR 58**: Emit wavy underlines for compiler diagnostics when terminal emulator capabilities indicate support.
  - **OSC 9;4 Progress**: Stream taskbar and window tab progress state to modern terminal emulators (e.g. Ghostty, iTerm2, WezTerm, Windows Terminal).

---

## 4. Native Cabal External Command Integration (§18)

- [ ] **`cabal-buildr` Discovery**:
  - Provide an executable alias or symlink named `cabal-buildr` on `PATH`.
  - Enable invocation as a native Cabal external command:
    ```bash
    cabal buildr [TARGETS] [--options...]
    ```
  - Pass informational and query arguments directly through to Cabal.

---

## 5. Automated Benchmark & Verification Suite (§5, §21)

- [ ] **B1: Diagnostic Latency**:
  - Benchmark test harness ensuring GHC diagnostic arrival timestamp to on-screen presentation latency is `< 50 ms` (p95).
- [ ] **B2: Build Overhead**:
  - Automated comparison verifying that `buildr` introduces `< 2%` wall-clock overhead compared to an identical bare `cabal build` invocation across small, medium, and 500+ module projects.
- [ ] **B4: Transparency Verification**:
  - Automated integration test verifying that compilation under `buildr-ghc` generates byte-identical artifacts and results in zero rebuilds when alternating between `buildr` and plain `cabal`.
- [ ] **B8: Redraw Cost**:
  - Verification that live UI updates remain bounded at `≤ 15 fps` and consume `< 3%` of a single CPU core at 1,000 modules.

---

## 6. Phase 3 Standard Roadmap (§7, §23)

- [ ] **Native Windows Support**:
  - Replace Unix domain socket IPC with Windows Named Pipes.
  - Integrate Windows Job Objects for resilient process tree termination.
  - Implement Windows Console API and virtual terminal escape sequence processing.
- [ ] **Editor Protocol & Upstream Proposal**:
  - Define an editor/LSP event consumer interface based on Event Protocol v1.
  - Submit upstream proposal to `cabal-install` for native machine-readable event streaming.
