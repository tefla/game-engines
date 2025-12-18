# Oort Engine (CodexVersion)

This folder contains a minimal implementation of the **Slate** language + a **virtual filesystem (VFS)** + **signals** and a tiny **VCS** for VFS snapshots/undo.

## Commands

- Run tests: `bun test`
- Run a Slate file from the seeded VFS: `bun run slate -- /puzzles/door.sl`
- Run a Slate file from disk: `bun run slate -- path/to/file.sl`
- Run the puzzle demo (shows hot-reload): `bun run demo`
- Run the 3D web demo (Slate + Three.js): `bun run demo:web`

## Notes

- The VFS is in-memory (seeded in `packages/slate/src/stdlib.ts:40`).
- `write "/some/file.sl", "..."`
  hot-reloads that file (updates `fn`/`on`/top-level `entity` calls) after the write.
- `import "/path.sl"` uses VFS execute permission (`x`) via `vfs.exec(...)` (use `chmod` to enable).
- The Three.js scene adapter lives in `packages/renderer/src/slateThreeScene.ts:1`.
