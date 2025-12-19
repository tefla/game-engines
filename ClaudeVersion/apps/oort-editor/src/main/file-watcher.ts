// File watcher - monitors project files for external changes
import { watch, FSWatcher } from "chokidar";
import { BrowserWindow } from "electron";
import { IPC } from "../shared/ipc-channels";

class FileWatcher {
  private watcher: FSWatcher | null = null;
  private projectPath: string | null = null;
  private mainWindow: BrowserWindow | null = null;

  setMainWindow(window: BrowserWindow) {
    this.mainWindow = window;
  }

  start(projectPath: string) {
    // Stop any existing watcher
    this.stop();

    this.projectPath = projectPath;

    // Initialize watcher
    this.watcher = watch(projectPath, {
      ignored: [
        /(^|[\/\\])\../, // Ignore dotfiles
        /node_modules/,
        /\.git/,
      ],
      persistent: true,
      ignoreInitial: true, // Don't fire events for existing files
      awaitWriteFinish: {
        stabilityThreshold: 300,
        pollInterval: 100,
      },
    });

    // Set up event handlers
    this.watcher
      .on("add", (path) => this.notifyChange("add", path))
      .on("change", (path) => this.notifyChange("change", path))
      .on("unlink", (path) => this.notifyChange("unlink", path))
      .on("addDir", (path) => this.notifyChange("addDir", path))
      .on("unlinkDir", (path) => this.notifyChange("unlinkDir", path))
      .on("error", (error) => console.error("File watcher error:", error));

    console.log(`File watcher started for: ${projectPath}`);
  }

  stop() {
    if (this.watcher) {
      this.watcher.close();
      this.watcher = null;
      console.log("File watcher stopped");
    }
    this.projectPath = null;
  }

  private notifyChange(event: string, path: string) {
    if (!this.mainWindow || this.mainWindow.isDestroyed()) {
      return;
    }

    // Send event to renderer
    this.mainWindow.webContents.send(IPC.FILE_EXTERNAL_CHANGE, {
      event,
      path,
      relativePath: this.projectPath ? path.replace(this.projectPath, "") : path,
    });
  }

  isWatching(): boolean {
    return this.watcher !== null;
  }

  getProjectPath(): string | null {
    return this.projectPath;
  }
}

// Singleton instance
export const fileWatcher = new FileWatcher();
