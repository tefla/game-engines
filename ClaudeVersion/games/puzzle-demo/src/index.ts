// Puzzle Demo Game - Example puzzle game using Oort Engine
// This demonstrates all features: VFS, signals, VCS, and the editor

import { VirtualFileSystem, READ_ONLY, READ_WRITE, SignalBus } from "@oort/engine";
import { History, createHistory } from "@oort/vcs";
import { createEditor, SLATE_HIGHLIGHT_CSS_DARK } from "@oort/editor";
import { runSlate, stringify } from "@oort/slate";
import * as fs from "fs";
import * as path from "path";

// Load Slate source files
function loadSlateFile(filePath: string): string {
  const fullPath = path.join(__dirname, "..", filePath);
  return fs.readFileSync(fullPath, "utf-8");
}

// Create the puzzle game
export function createPuzzleGame() {
  // Initialize VFS with puzzle game structure
  const vfs = new VirtualFileSystem();

  // Create directories
  vfs.mkdir("/engine");
  vfs.mkdir("/puzzles");
  vfs.mkdir("/player");

  // Load engine files (read-only)
  const coreEngine = loadSlateFile("engine/core.sl");
  vfs.write("/engine/core.sl", coreEngine);
  vfs.chmod("/engine/core.sl", READ_ONLY);
  vfs.chmod("/engine", READ_ONLY);

  // Load puzzle files (read-write so player can edit)
  const brokenDoor = loadSlateFile("puzzles/broken_door.sl");
  const leverSequence = loadSlateFile("puzzles/lever_sequence.sl");
  const mathLock = loadSlateFile("puzzles/math_lock.sl");

  vfs.write("/puzzles/broken_door.sl", brokenDoor);
  vfs.write("/puzzles/lever_sequence.sl", leverSequence);
  vfs.write("/puzzles/math_lock.sl", mathLock);

  // Load player notes (read-write)
  const notes = loadSlateFile("player/notes.sl");
  vfs.write("/player/notes.sl", notes);

  // Initialize VCS for undo/redo
  const history = createHistory(vfs);

  // Initialize signal bus for game events
  const signalBus = new SignalBus();

  // Set up game event handlers
  signalBus.on(["game", "message"], (data) => {
    console.log(`[GAME] ${(data as any).fields?.get("text")?.value || data}`);
  });

  signalBus.on(["game", "notification"], (data) => {
    console.log(`[!] ${(data as any).fields?.get("text")?.value || data}`);
  });

  signalBus.on(["puzzle", "solved"], (data) => {
    const puzzleName = typeof data === "string" ? data : (data as any).value || "unknown";
    console.log(`\n*** PUZZLE SOLVED: ${puzzleName} ***\n`);
  });

  signalBus.on(["player", "got_item"], (data) => {
    const item = (data as any).fields?.get("item")?.value || "something";
    console.log(`[+] Got item: ${item}`);
  });

  // Create editor for editing puzzle files
  const editor = createEditor({
    onChange: (content) => {
      console.log("[Editor] Content changed");
    },
    onSave: (path, content) => {
      console.log(`[Editor] Saved: ${path}`);
    },
    onError: (error) => {
      console.error(`[Editor Error] ${error.message}`);
    },
    onDiagnostics: (diags) => {
      if (diags.length > 0) {
        console.log("[Editor] Diagnostics:");
        diags.forEach(d => {
          console.log(`  ${d.severity}: ${d.message} at line ${d.line}`);
        });
      }
    }
  });

  editor.connectVfs(vfs);
  editor.connectHistory(history);

  return {
    vfs,
    history,
    signalBus,
    editor,

    // Run a Slate file from VFS
    runFile(filePath: string) {
      try {
        const source = vfs.read(filePath);
        const result = runSlate(source);
        return result;
      } catch (e: any) {
        console.error(`Error running ${filePath}:`, e.message);
        return null;
      }
    },

    // Run custom Slate code
    run(source: string) {
      try {
        const result = runSlate(source);
        console.log("Result:", stringify(result));
        return result;
      } catch (e: any) {
        console.error("Error:", e.message);
        return null;
      }
    },

    // List files in a directory
    ls(dirPath: string = "/") {
      const files = vfs.ls(dirPath);
      console.log(`\nContents of ${dirPath}:`);
      files.forEach(f => {
        const type = f.isDirectory ? "[DIR]" : "[FILE]";
        console.log(`  ${f.permissions} ${type} ${f.name}`);
      });
    },

    // View a file
    cat(filePath: string) {
      try {
        const content = vfs.read(filePath);
        console.log(`\n=== ${filePath} ===`);
        console.log(content);
        console.log(`=== END ===\n`);
      } catch (e: any) {
        console.error(`Cannot read ${filePath}: ${e.message}`);
      }
    },

    // Edit a file
    edit(filePath: string) {
      if (editor.open(filePath)) {
        console.log(`Opened ${filePath} in editor`);
        console.log("Use editor.setContent(newContent) to modify");
        console.log("Use editor.save() to save changes");
      }
    },

    // Save snapshot
    snapshot(name: string) {
      history.saveSnapshot(name);
      console.log(`Saved snapshot: ${name}`);
    },

    // Restore snapshot
    restore(name: string) {
      if (history.loadSnapshot(name)) {
        console.log(`Restored snapshot: ${name}`);
      } else {
        console.log(`Snapshot not found: ${name}`);
      }
    },

    // Undo
    undo() {
      if (history.undo()) {
        console.log("Undone");
      } else {
        console.log("Nothing to undo");
      }
    },

    // Redo
    redo() {
      if (history.redo()) {
        console.log("Redone");
      } else {
        console.log("Nothing to redo");
      }
    },

    // Get help
    help() {
      console.log(`
========================================
        OORT PUZZLE GAME - HELP
========================================

GOAL: Fix the broken puzzles by editing their code!

COMMANDS:
  game.ls(path)      - List files in directory
  game.cat(path)     - View a file's contents
  game.edit(path)    - Open file in editor
  game.runFile(path) - Run a Slate file
  game.run(code)     - Run Slate code directly

VERSION CONTROL:
  game.snapshot(name) - Save current state
  game.restore(name)  - Restore saved state
  game.undo()         - Undo last change
  game.redo()         - Redo undone change

EDITOR:
  game.editor.setContent(text) - Set editor content
  game.editor.save()           - Save editor content
  game.editor.run()            - Run editor content

FILES:
  /engine/       - Read-only engine code (learn from it!)
  /puzzles/      - Puzzle files (fix these!)
  /player/       - Your scratchpad (experiment here!)

PUZZLES TO FIX:
  1. /puzzles/broken_door.sl    - Logic bug
  2. /puzzles/lever_sequence.sl - Incomplete code
  3. /puzzles/math_lock.sl      - Operator bug

Good luck, puzzle solver!
========================================
`);
    }
  };
}

// Export for testing
export const PuzzleGame = createPuzzleGame;

// Main entry point
if (require.main === module) {
  const game = createPuzzleGame();
  game.help();

  // Make game available globally for REPL
  (global as any).game = game;

  console.log("\nGame loaded! Try: game.ls('/puzzles')");
}
