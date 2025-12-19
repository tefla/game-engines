// Runtime Service - Manages game runtime and VFS sync with real filesystem
// This service bridges the Electron filesystem with the in-memory VFS used by the game runtime

import { VirtualFileSystem } from "@oort/engine";
import { Runtime } from "@oort/engine";
import { History, createHistory } from "@oort/vcs";
import { messageBus } from "./message-bus";

type OutputHandler = (message: string) => void;

class RuntimeService {
  private runtime: Runtime | null = null;
  private history: History | null = null;
  private projectPath: string | null = null;
  private outputHandler: OutputHandler = console.log;
  private isLoading = false;

  /**
   * Initialize or reinitialize the runtime for a project
   */
  async initializeForProject(projectPath: string): Promise<void> {
    this.projectPath = projectPath;

    // Create fresh VFS
    const vfs = new VirtualFileSystem();

    // Load project files into VFS
    await this.loadProjectIntoVfs(vfs, projectPath);

    // Create runtime with the VFS
    this.runtime = new Runtime({
      vfs,
      onOutput: (msg) => this.outputHandler(msg),
    });

    // Create history manager for VFS snapshots
    this.history = createHistory(vfs, { maxHistory: 50 });

    messageBus.emit("runtime:initialized", { projectPath });
  }

  /**
   * Load all project files into the VFS
   */
  private async loadProjectIntoVfs(vfs: VirtualFileSystem, basePath: string): Promise<void> {
    this.isLoading = true;

    try {
      // Recursively load all files from the project directory
      await this.loadDirectoryIntoVfs(vfs, basePath, "/");
    } finally {
      this.isLoading = false;
    }
  }

  /**
   * Recursively load a directory and its contents into the VFS
   */
  private async loadDirectoryIntoVfs(
    vfs: VirtualFileSystem,
    realPath: string,
    vfsPath: string
  ): Promise<void> {
    const result = await window.electronAPI?.listFiles(realPath);

    if (!result?.success || !result.data) {
      return;
    }

    for (const entry of result.data) {
      const entryVfsPath = vfsPath === "/" ? `/${entry.name}` : `${vfsPath}/${entry.name}`;

      if (entry.type === "directory") {
        // Skip node_modules, .git, etc.
        if (this.shouldSkipDirectory(entry.name)) {
          continue;
        }

        // Create directory in VFS
        try {
          vfs.mkdir(entryVfsPath);
        } catch {
          // Directory might already exist
        }

        // Recursively load contents
        await this.loadDirectoryIntoVfs(vfs, entry.path, entryVfsPath);
      } else {
        // Load file content
        const fileResult = await window.electronAPI?.readFile(entry.path);
        if (fileResult?.success && fileResult.data !== undefined) {
          try {
            vfs.write(entryVfsPath, fileResult.data);
          } catch (err) {
            console.warn(`Failed to write ${entryVfsPath} to VFS:`, err);
          }
        }
      }
    }
  }

  /**
   * Check if a directory should be skipped during VFS sync
   */
  private shouldSkipDirectory(name: string): boolean {
    const skipDirs = ["node_modules", ".git", ".vscode", ".idea", "dist", "build"];
    return skipDirs.includes(name) || name.startsWith(".");
  }

  /**
   * Sync a single file from the real filesystem to VFS
   */
  async syncFile(realPath: string): Promise<void> {
    if (!this.runtime || !this.projectPath) return;

    // Convert real path to VFS path
    const vfsPath = this.realPathToVfsPath(realPath);
    if (!vfsPath) return;

    const vfs = this.runtime.getVfs();
    const result = await window.electronAPI?.readFile(realPath);

    if (result?.success && result.data !== undefined) {
      try {
        vfs.write(vfsPath, result.data);
        messageBus.emit("vfs:file-synced", { path: vfsPath });
      } catch (err) {
        console.warn(`Failed to sync ${vfsPath} to VFS:`, err);
      }
    }
  }

  /**
   * Handle file deletion - remove from VFS
   */
  async handleFileDeletion(realPath: string): Promise<void> {
    if (!this.runtime || !this.projectPath) return;

    const vfsPath = this.realPathToVfsPath(realPath);
    if (!vfsPath) return;

    const vfs = this.runtime.getVfs();
    try {
      vfs.rm(vfsPath);
      messageBus.emit("vfs:file-removed", { path: vfsPath });
    } catch {
      // File might not exist in VFS
    }
  }

  /**
   * Handle directory creation
   */
  async handleDirectoryCreation(realPath: string): Promise<void> {
    if (!this.runtime || !this.projectPath) return;

    const vfsPath = this.realPathToVfsPath(realPath);
    if (!vfsPath) return;

    const vfs = this.runtime.getVfs();
    try {
      vfs.mkdir(vfsPath);
      messageBus.emit("vfs:dir-created", { path: vfsPath });
    } catch {
      // Directory might already exist
    }
  }

  /**
   * Convert a real filesystem path to a VFS path
   */
  private realPathToVfsPath(realPath: string): string | null {
    if (!this.projectPath) return null;

    if (!realPath.startsWith(this.projectPath)) {
      return null;
    }

    const relativePath = realPath.substring(this.projectPath.length);
    return relativePath || "/";
  }

  /**
   * Set the output handler for print/say statements
   */
  setOutputHandler(handler: OutputHandler): void {
    this.outputHandler = handler;

    // Update runtime if it exists
    if (this.runtime) {
      // We need to reinitialize with the new output handler
      // For now, just store it and it will be used on next init
    }
  }

  /**
   * Get the current runtime instance
   */
  getRuntime(): Runtime | null {
    return this.runtime;
  }

  /**
   * Get the VFS from the runtime
   */
  getVfs(): VirtualFileSystem | null {
    return this.runtime?.getVfs() ?? null;
  }

  /**
   * Check if runtime is initialized
   */
  isInitialized(): boolean {
    return this.runtime !== null;
  }

  /**
   * Check if VFS sync is in progress
   */
  isSyncing(): boolean {
    return this.isLoading;
  }

  /**
   * Run Slate code using the runtime
   */
  run(source: string): any {
    if (!this.runtime) {
      throw new Error("Runtime not initialized. Open a project first.");
    }
    return this.runtime.run(source);
  }

  /**
   * Run a file from the VFS
   */
  runFile(vfsPath: string): any {
    if (!this.runtime) {
      throw new Error("Runtime not initialized. Open a project first.");
    }
    return this.runtime.runFile(vfsPath);
  }

  /**
   * Close the current runtime
   */
  close(): void {
    this.runtime = null;
    this.history = null;
    this.projectPath = null;
    messageBus.emit("runtime:closed", {});
  }

  /**
   * Get the interpreter for direct access
   */
  getInterpreter() {
    return this.runtime?.getInterpreter() ?? null;
  }

  /**
   * Get the history manager
   */
  getHistory(): History | null {
    return this.history;
  }

  // === VCS History Methods ===

  /**
   * Commit current VFS state to history
   */
  commit(description?: string): boolean {
    if (!this.history) return false;
    this.history.commit(description);
    messageBus.emit("vcs:commit", { description });
    return true;
  }

  /**
   * Undo to previous VFS state
   */
  undo(): boolean {
    if (!this.history) return false;
    const result = this.history.undo();
    if (result) {
      messageBus.emit("vcs:undo", {});
    }
    return result;
  }

  /**
   * Redo to next VFS state
   */
  redo(): boolean {
    if (!this.history) return false;
    const result = this.history.redo();
    if (result) {
      messageBus.emit("vcs:redo", {});
    }
    return result;
  }

  /**
   * Check if undo is available
   */
  canUndo(): boolean {
    return this.history?.canUndo() ?? false;
  }

  /**
   * Check if redo is available
   */
  canRedo(): boolean {
    return this.history?.canRedo() ?? false;
  }

  /**
   * Save a named snapshot (checkpoint)
   */
  saveCheckpoint(name: string): boolean {
    if (!this.history) return false;
    this.history.saveSnapshot(name);
    messageBus.emit("vcs:checkpoint-saved", { name });
    return true;
  }

  /**
   * Load a named snapshot (checkpoint)
   */
  loadCheckpoint(name: string): boolean {
    if (!this.history) return false;
    const result = this.history.loadSnapshot(name);
    if (result) {
      messageBus.emit("vcs:checkpoint-loaded", { name });
    }
    return result;
  }

  /**
   * List all saved checkpoints
   */
  listCheckpoints(): Array<{ name: string; timestamp: number }> {
    return this.history?.listSnapshots() ?? [];
  }

  /**
   * Delete a checkpoint
   */
  deleteCheckpoint(name: string): boolean {
    if (!this.history) return false;
    return this.history.deleteSnapshot(name);
  }

  /**
   * Get current history entries
   */
  getHistoryEntries() {
    return this.history?.getHistory() ?? [];
  }
}

// Singleton instance
export const runtimeService = new RuntimeService();
