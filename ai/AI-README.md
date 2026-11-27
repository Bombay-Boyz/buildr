# AI-README: Mandatory Instructions & Protocols for AI Agents

**Status: Binding on all AI agents, assistants, and automated systems editing this repository.**

This document defines the strict operational rules, architectural constraints, and post-commit update protocols that every AI assistant **must** follow when reading, modifying, or extending the **buildr** codebase.

---

## 1. Governance & Core Documents

Before proposing, generating, or committing any code, you **must** read and adhere to the governing documents located in the [`ai/`](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai) directory:

1. **[Haskell Engineering Standard](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/haskell-engineering-standard.md)**:
   * **Constitutional authority**. All code must satisfy Part 0 (Design Principles), Part 1 (Language Principles), Part 2 (Architecture), and Part 5 (Verification).
   * **Zero partiality**: Never use `head`, `tail`, `fromJust`, `(!!)`, `error`, `undefined`, or non-exhaustive pattern matches.
   * **Compiler flags**: Mandatory `-Wall -Wcompat -Werror -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-export-lists -Wunused-imports -Wunused-top-binds -Wunused-local-binds -Wunused-matches -Wredundant-constraints -Wpartial-fields -Widentities`.
2. **[Vision Document](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/vision-document.md)**:
   * The canonical specification (§1 through §25).
   * Objective (§1) is immutable: *compilation rendering better than Rust's*.
3. **[Module Dependency & Architecture Graph](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/graph.md)**:
   * The structural map of all modules, data flows, and IPC connections.
4. **[Pending Tasks & Roadmap Matrix](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/pending.md)**:
   * Current task checklist and benchmark criteria status.
5. **[Intended Visual Mockups](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/images)**:
   * Canonical rendering output references (`00-visual-grammar.png` through `08-linear-accessible.png`).

---

## 2. Mandatory Post-Commit / Post-Edit Update Protocol

Whenever you perform an edit or prepare a commit in this repository, you **MUST** execute the following update sequence without fail:

### Step 1: Run Full Verification Suite
```bash
cabal test
```
* **Exit code must be 0**.
* **Zero compiler warnings**. All warnings are treated as fatal errors (`-Werror`).
* All QuickCheck properties, boundary invariants, IPC tests, golden replay tests, and benchmark criteria (**B1, B2, B4, B8**) must pass.

### Step 2: Synchronize Pending List Documents
* Update **both** [`pending.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/pending.md) (root) and [`ai/pending.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/pending.md).
* If a pending task was addressed, mark it `[x]` and document the measured result or implementing module.
* If a new sub-task or requirement was identified, add it under the appropriate section.
* Keep `pending.md` and `ai/pending.md` 100% synchronized.

### Step 3: Update Vision Document Status Matrix
* Check [`ai/vision-document.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/vision-document.md).
* If you modified or implemented functionality relating to any section (§1 to §25), update the status table in **§0** and the roadmap checklist in **§7** with exact module references.

### Step 4: Update the Architectural Graph
* Check [`ai/graph.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/graph.md).
* If you added a new module, renamed a module, altered imports, or modified the IPC/event data flow, update the Mermaid diagram and the module table in `ai/graph.md`.

---

## 3. Haskell Architecture & Implementation Rules

When authoring Haskell code in this codebase:

1. **Hexagonal Dependency Direction**:
   * Pure Core modules (`Buildr.Model`, `Buildr.Event`, `Buildr.Grammar`, `Buildr.Theme`, `Buildr.Editor`) must **never** import IO boundary adapters (`Buildr.Ipc`, `Buildr.Signal`, `Buildr.Terminal`, `Buildr.Config`, `Buildr.Cli`).
   * Core modules must remain 100% pure, total, and deterministic.
2. **Closed Error Taxonomy**:
   * All failure modes are declared in [`Buildr.Error`](file:///Users/air/Documents/seby/projects/Haskell/buildr/src/Buildr/Error.hs) as layered, documented sum types.
   * Never introduce string-based errors, catch-all wildcards (`_ -> error ...`), or uninformative messages.
3. **Property-Based Verification**:
   * Any new pure function or state transformation must ship with a QuickCheck property test in [`test/Spec.hs`](file:///Users/air/Documents/seby/projects/Haskell/buildr/test/Spec.hs).
4. **Platform Portability**:
   * When dealing with system resources (IPC, signals, console), use CPP (`#if defined(mingw32_HOST_OS)`) to guarantee portability across Linux, macOS, and Windows.
   * On Windows, support Windows Named Pipes (`\\.\pipe\buildr-...`) and Job Object process tree termination.

---

## 4. Verification Checklist for AI Turns

Before replying to the user or finalizing changes, confirm each item:

- [ ] Does `cabal test` pass cleanly with zero warnings?
- [ ] Are all functions total with no partial record fields or non-exhaustive matches?
- [ ] Has [`pending.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/pending.md) been updated?
- [ ] Has [`ai/pending.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/pending.md) been updated?
- [ ] Has [`ai/vision-document.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/vision-document.md) been updated?
- [ ] Has [`ai/graph.md`](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/graph.md) been updated if architecture changed?
- [ ] Are all references using clickable `file:///` markdown links?
