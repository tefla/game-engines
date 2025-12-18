// Centralized state store with subscription support
// Lightweight alternative to Redux/Zustand with path-based access

import { messageBus } from "./message-bus";
import type { EditorState, ProjectConfig } from "@shared/types";

type StateListener = (value: any, path: string) => void;

// Initial state
const initialState: EditorState = {
  project: {
    path: null,
    name: "Untitled",
    config: null,
  },
  files: {
    current: null,
    open: [],
    modified: new Set(),
  },
  ui: {
    theme: "dark",
    sidebarWidth: 250,
    bottomPanelHeight: 200,
    activePanel: null,
  },
};

class StateStore {
  private state: EditorState;
  private listeners: Map<string, Set<StateListener>> = new Map();
  private wildcardListeners: Map<string, Set<StateListener>> = new Map();

  constructor() {
    this.state = this.deepClone(initialState);
  }

  /**
   * Get a value from state by path
   * Example: store.get("project.name") returns "Untitled"
   */
  get<T = any>(path: string): T {
    return this.getByPath(this.state, path);
  }

  /**
   * Set a value in state by path
   * Example: store.set("project.name", "My Game")
   */
  set(path: string, value: any): void {
    const oldValue = this.getByPath(this.state, path);
    if (oldValue === value) return;

    this.setByPath(this.state, path, value);
    this.notifyListeners(path, value);
  }

  /**
   * Update multiple values at once
   */
  update(updates: Record<string, any>): void {
    Object.entries(updates).forEach(([path, value]) => {
      this.setByPath(this.state, path, value);
    });

    // Notify all updated paths
    Object.entries(updates).forEach(([path, value]) => {
      this.notifyListeners(path, value);
    });
  }

  /**
   * Subscribe to state changes
   * Supports wildcards: "project.*" matches all project changes
   */
  subscribe(path: string, listener: StateListener): () => void {
    if (path.includes("*")) {
      const pattern = path.replace("*", "");
      if (!this.wildcardListeners.has(pattern)) {
        this.wildcardListeners.set(pattern, new Set());
      }
      this.wildcardListeners.get(pattern)!.add(listener);

      return () => {
        this.wildcardListeners.get(pattern)?.delete(listener);
      };
    } else {
      if (!this.listeners.has(path)) {
        this.listeners.set(path, new Set());
      }
      this.listeners.get(path)!.add(listener);

      return () => {
        this.listeners.get(path)?.delete(listener);
      };
    }
  }

  /**
   * Get entire state (for debugging/persistence)
   */
  getState(): EditorState {
    return this.deepClone(this.state);
  }

  /**
   * Reset state to initial values
   */
  reset(): void {
    this.state = this.deepClone(initialState);
    this.notifyListeners("", this.state);
  }

  /**
   * Load state from persistence
   */
  loadState(saved: Partial<EditorState>): void {
    this.state = {
      ...this.deepClone(initialState),
      ...saved,
      files: {
        ...initialState.files,
        ...saved.files,
        modified: new Set(saved.files?.modified || []),
      },
    };
  }

  /**
   * Get serializable state for persistence
   */
  toJSON(): any {
    return {
      ...this.state,
      files: {
        ...this.state.files,
        modified: Array.from(this.state.files.modified),
      },
    };
  }

  // === File-specific helpers ===

  openFile(path: string): void {
    const open = this.get<string[]>("files.open");
    if (!open.includes(path)) {
      this.set("files.open", [...open, path]);
    }
    this.set("files.current", path);
    messageBus.emit("file:open", { path });
  }

  closeFile(path: string): void {
    const open = this.get<string[]>("files.open");
    const current = this.get<string | null>("files.current");

    this.set(
      "files.open",
      open.filter((p) => p !== path)
    );

    // If closing current file, switch to another open file
    if (current === path) {
      const remaining = open.filter((p) => p !== path);
      this.set("files.current", remaining[remaining.length - 1] || null);
    }

    // Remove from modified
    const modified = this.get<Set<string>>("files.modified");
    modified.delete(path);
    this.set("files.modified", modified);

    messageBus.emit("file:close", { path });
  }

  markModified(path: string, isModified = true): void {
    const modified = this.get<Set<string>>("files.modified");
    if (isModified) {
      modified.add(path);
    } else {
      modified.delete(path);
    }
    this.set("files.modified", modified);
  }

  isModified(path: string): boolean {
    return this.get<Set<string>>("files.modified").has(path);
  }

  // === Project helpers ===

  setProject(path: string, config: ProjectConfig): void {
    this.update({
      "project.path": path,
      "project.name": config.name,
      "project.config": config,
    });
    messageBus.emit("project:opened", { path, config });
  }

  closeProject(): void {
    this.update({
      "project.path": null,
      "project.name": "Untitled",
      "project.config": null,
      "files.current": null,
      "files.open": [],
      "files.modified": new Set(),
    });
    messageBus.emit("project:closed", {});
  }

  // === Private helpers ===

  private getByPath(obj: any, path: string): any {
    if (!path) return obj;
    return path.split(".").reduce((current, key) => current?.[key], obj);
  }

  private setByPath(obj: any, path: string, value: any): void {
    const keys = path.split(".");
    const lastKey = keys.pop()!;
    const target = keys.reduce((current, key) => {
      if (current[key] === undefined) {
        current[key] = {};
      }
      return current[key];
    }, obj);
    target[lastKey] = value;
  }

  private notifyListeners(path: string, value: any): void {
    // Exact match listeners
    const listeners = this.listeners.get(path);
    if (listeners) {
      listeners.forEach((listener) => {
        try {
          listener(value, path);
        } catch (error) {
          console.error(`Error in state listener for ${path}:`, error);
        }
      });
    }

    // Wildcard listeners
    this.wildcardListeners.forEach((wildcardListeners, pattern) => {
      if (path.startsWith(pattern)) {
        wildcardListeners.forEach((listener) => {
          try {
            listener(value, path);
          } catch (error) {
            console.error(`Error in wildcard listener for ${pattern}*:`, error);
          }
        });
      }
    });

    // Also notify parent paths for deep updates
    const parts = path.split(".");
    while (parts.length > 1) {
      parts.pop();
      const parentPath = parts.join(".");
      const parentListeners = this.listeners.get(parentPath);
      if (parentListeners) {
        const parentValue = this.getByPath(this.state, parentPath);
        parentListeners.forEach((listener) => {
          try {
            listener(parentValue, parentPath);
          } catch (error) {
            console.error(`Error in state listener for ${parentPath}:`, error);
          }
        });
      }
    }
  }

  private deepClone<T>(obj: T): T {
    if (obj === null || typeof obj !== "object") {
      return obj;
    }
    if (obj instanceof Set) {
      return new Set(obj) as any;
    }
    if (obj instanceof Map) {
      return new Map(obj) as any;
    }
    if (Array.isArray(obj)) {
      return obj.map((item) => this.deepClone(item)) as any;
    }
    const cloned: any = {};
    for (const key in obj) {
      if (Object.prototype.hasOwnProperty.call(obj, key)) {
        cloned[key] = this.deepClone((obj as any)[key]);
      }
    }
    return cloned;
  }
}

// Singleton instance
export const store = new StateStore();
