# Non-Haskell Prototype Compiler Producer: buildr-clang

This example demonstrates that the **buildr Event Protocol v1** is a language-agnostic specification, fulfilling the exit criteria of Phase 3 of the [Vision Document](file:///Users/air/Documents/seby/projects/Haskell/buildr/ai/vision-document.md) (§23).

## How it works

1. `buildr-clang` wraps compilation commands (e.g. `clang -c main.c -o main.o`).
2. As it invokes the C/C++ compiler, it emits Event Protocol v1 JSON Lines (`ModuleStarted`, `ModulePhase`, `Diagnostic`, `ModuleFinished`, `BuildDone`).
3. If `BUILDR_SOCKET` is set, events stream directly to `buildr`'s live TUI orchestrator. Otherwise, events stream to `stdout` or a log file for replay.

## Running the demonstration

```bash
# Generate Event Protocol v1 stream to stdout
python3 buildr-clang main.c engine.c render.c

# Or pipe into buildr replay (or socket)
python3 buildr-clang main.c engine.c render.c > c-build.jsonl
```
