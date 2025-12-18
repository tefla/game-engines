// History Manager - Undo/Redo with named snapshots

import type { VirtualFileSystem } from "@oort/engine";
import {
  type Snapshot,
  createSnapshot,
  restoreSnapshot,
  getChangedFiles,
} from "./snapshot";

// Options for the history manager
export interface HistoryOptions {
  maxHistory?: number; // Maximum number of snapshots to keep
  autoSnapshot?: boolean; // Automatically snapshot on changes
}

// A history entry with optional name
export interface HistoryEntry {
  snapshot: Snapshot;
  description?: string;
}

// History manager class
export class History {
  private entries: HistoryEntry[] = [];
  private currentIndex: number = -1;
  private namedSnapshots: Map<string, Snapshot> = new Map();
  private maxHistory: number;
  private autoSnapshot: boolean;
  private vfs: VirtualFileSystem;

  constructor(vfs: VirtualFileSystem, options: HistoryOptions = {}) {
    this.vfs = vfs;
    this.maxHistory = options.maxHistory ?? 100;
    this.autoSnapshot = options.autoSnapshot ?? true;

    // Create initial snapshot
    this.commit("Initial state");
  }

  // Get the VFS
  getVFS(): VirtualFileSystem {
    return this.vfs;
  }

  // Create a new snapshot and add to history
  commit(description?: string): Snapshot {
    // If we're not at the end of history, truncate forward history
    if (this.currentIndex < this.entries.length - 1) {
      this.entries = this.entries.slice(0, this.currentIndex + 1);
    }

    const snapshot = createSnapshot(this.vfs);
    const entry: HistoryEntry = { snapshot, description };

    this.entries.push(entry);
    this.currentIndex = this.entries.length - 1;

    // Trim history if it exceeds max
    while (this.entries.length > this.maxHistory) {
      this.entries.shift();
      this.currentIndex--;
    }

    return snapshot;
  }

  // Undo - go back one step
  undo(): boolean {
    if (!this.canUndo()) {
      return false;
    }

    this.currentIndex--;
    const entry = this.entries[this.currentIndex];
    restoreSnapshot(this.vfs, entry.snapshot);
    return true;
  }

  // Redo - go forward one step
  redo(): boolean {
    if (!this.canRedo()) {
      return false;
    }

    this.currentIndex++;
    const entry = this.entries[this.currentIndex];
    restoreSnapshot(this.vfs, entry.snapshot);
    return true;
  }

  // Check if undo is available
  canUndo(): boolean {
    return this.currentIndex > 0;
  }

  // Check if redo is available
  canRedo(): boolean {
    return this.currentIndex < this.entries.length - 1;
  }

  // Get number of undo steps available
  undoSteps(): number {
    return this.currentIndex;
  }

  // Get number of redo steps available
  redoSteps(): number {
    return this.entries.length - 1 - this.currentIndex;
  }

  // Create a named snapshot
  saveSnapshot(name: string): Snapshot {
    const snapshot = createSnapshot(this.vfs, name);
    this.namedSnapshots.set(name, snapshot);
    return snapshot;
  }

  // Restore a named snapshot
  loadSnapshot(name: string): boolean {
    const snapshot = this.namedSnapshots.get(name);
    if (!snapshot) {
      return false;
    }

    // Commit current state before restoring
    if (this.autoSnapshot) {
      this.commit(`Before restoring "${name}"`);
    }

    restoreSnapshot(this.vfs, snapshot);

    // Commit the restored state
    this.commit(`Restored "${name}"`);

    return true;
  }

  // Delete a named snapshot
  deleteSnapshot(name: string): boolean {
    return this.namedSnapshots.delete(name);
  }

  // List all named snapshots
  listSnapshots(): Array<{ name: string; timestamp: number }> {
    return Array.from(this.namedSnapshots.entries()).map(([name, snapshot]) => ({
      name,
      timestamp: snapshot.timestamp,
    }));
  }

  // Check if a named snapshot exists
  hasSnapshot(name: string): boolean {
    return this.namedSnapshots.has(name);
  }

  // Get the current snapshot
  current(): Snapshot | null {
    if (this.currentIndex < 0 || this.currentIndex >= this.entries.length) {
      return null;
    }
    return this.entries[this.currentIndex].snapshot;
  }

  // Get history list
  getHistory(): Array<{
    index: number;
    description?: string;
    timestamp: number;
    isCurrent: boolean;
  }> {
    return this.entries.map((entry, index) => ({
      index,
      description: entry.description,
      timestamp: entry.snapshot.timestamp,
      isCurrent: index === this.currentIndex,
    }));
  }

  // Jump to a specific history index
  goto(index: number): boolean {
    if (index < 0 || index >= this.entries.length) {
      return false;
    }

    this.currentIndex = index;
    restoreSnapshot(this.vfs, this.entries[index].snapshot);
    return true;
  }

  // Get changes since last commit
  getUncommittedChanges(): {
    added: string[];
    modified: string[];
    removed: string[];
  } | null {
    const current = this.current();
    if (!current) {
      return null;
    }

    const now = createSnapshot(this.vfs);
    return getChangedFiles(current, now);
  }

  // Check if there are uncommitted changes
  hasChanges(): boolean {
    const changes = this.getUncommittedChanges();
    if (!changes) return false;
    return (
      changes.added.length > 0 ||
      changes.modified.length > 0 ||
      changes.removed.length > 0
    );
  }

  // Reset to a clean state (clears all history)
  reset(): void {
    this.entries = [];
    this.currentIndex = -1;
    this.commit("Reset");
  }

  // Clear all named snapshots
  clearSnapshots(): void {
    this.namedSnapshots.clear();
  }

  // Get diff between two history entries
  diff(
    fromIndex: number,
    toIndex: number
  ): { added: string[]; modified: string[]; removed: string[] } | null {
    if (
      fromIndex < 0 ||
      fromIndex >= this.entries.length ||
      toIndex < 0 ||
      toIndex >= this.entries.length
    ) {
      return null;
    }

    return getChangedFiles(
      this.entries[fromIndex].snapshot,
      this.entries[toIndex].snapshot
    );
  }

  // Export all history to a serializable format
  exportHistory(): string {
    return JSON.stringify({
      entries: this.entries.map((e) => ({
        snapshot: {
          ...e.snapshot,
          metadata: Array.from(e.snapshot.metadata.entries()),
        },
        description: e.description,
      })),
      currentIndex: this.currentIndex,
      namedSnapshots: Array.from(this.namedSnapshots.entries()).map(
        ([name, snapshot]) => [
          name,
          { ...snapshot, metadata: Array.from(snapshot.metadata.entries()) },
        ]
      ),
    });
  }

  // Import history from serialized format
  importHistory(data: string): void {
    const parsed = JSON.parse(data);

    this.entries = parsed.entries.map((e: any) => ({
      snapshot: {
        ...e.snapshot,
        metadata: new Map(e.snapshot.metadata),
      },
      description: e.description,
    }));

    this.currentIndex = parsed.currentIndex;

    this.namedSnapshots = new Map(
      parsed.namedSnapshots.map(([name, snapshot]: [string, any]) => [
        name,
        { ...snapshot, metadata: new Map(snapshot.metadata) },
      ])
    );

    // Restore to current position
    if (this.currentIndex >= 0 && this.currentIndex < this.entries.length) {
      restoreSnapshot(this.vfs, this.entries[this.currentIndex].snapshot);
    }
  }
}

// Create a history manager for a VFS
export function createHistory(
  vfs: VirtualFileSystem,
  options?: HistoryOptions
): History {
  return new History(vfs, options);
}
