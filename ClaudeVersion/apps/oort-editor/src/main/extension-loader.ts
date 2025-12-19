/**
 * Extension Loader - Discovers and manages editor extensions
 */

import * as fs from "fs";
import * as path from "path";
import { app, ipcMain, BrowserWindow } from "electron";

/**
 * Extension manifest from package.json
 */
export interface ExtensionManifest {
  id: string;
  name: string;
  version: string;
  description?: string;
  author?: string;
  type: "extension" | "theme";
  activationEvents?: string[];
  main: string;
  contributes?: {
    panels?: Array<{
      id: string;
      title: string;
      icon: string;
      defaultLocation?: "left" | "right" | "bottom" | "center";
      singleton?: boolean;
      category?: string;
    }>;
    commands?: Array<{
      id: string;
      title: string;
      category?: string;
      keybinding?: string;
      when?: string;
    }>;
    assetTypes?: Array<{
      extensions: string[];
      name: string;
      icon: string;
      editorId?: string;
    }>;
  };
}

/**
 * Loaded extension information
 */
export interface LoadedExtension {
  id: string;
  manifest: ExtensionManifest;
  extensionPath: string;
  active: boolean;
}

/**
 * Extension loader for discovering and managing extensions
 */
class ExtensionLoader {
  private extensions = new Map<string, LoadedExtension>();
  private extensionPaths: string[] = [];

  constructor() {
    this.setupExtensionPaths();
    this.setupIpcHandlers();
  }

  /**
   * Set up default extension search paths
   */
  private setupExtensionPaths(): void {
    // User extensions directory
    const userDataPath = app.getPath("userData");
    const userExtensionsPath = path.join(userDataPath, "extensions");

    // Built-in extensions (in app resources)
    const appPath = app.getAppPath();
    const builtinExtensionsPath = path.join(appPath, "extensions");

    // Development extensions (in project)
    const devExtensionsPath = path.join(appPath, "../../extensions");

    this.extensionPaths = [
      userExtensionsPath,
      builtinExtensionsPath,
      devExtensionsPath,
    ];

    // Ensure user extensions directory exists
    if (!fs.existsSync(userExtensionsPath)) {
      try {
        fs.mkdirSync(userExtensionsPath, { recursive: true });
      } catch (err) {
        console.error("Failed to create user extensions directory:", err);
      }
    }
  }

  /**
   * Set up IPC handlers for extension communication
   */
  private setupIpcHandlers(): void {
    // Get list of all extensions
    ipcMain.handle("extensions:getAll", () => {
      return Array.from(this.extensions.values()).map((ext) => ({
        id: ext.id,
        name: ext.manifest.name,
        version: ext.manifest.version,
        description: ext.manifest.description,
        active: ext.active,
        contributes: ext.manifest.contributes,
      }));
    });

    // Get specific extension
    ipcMain.handle("extensions:get", (_, id: string) => {
      const ext = this.extensions.get(id);
      if (!ext) return null;
      return {
        id: ext.id,
        name: ext.manifest.name,
        version: ext.manifest.version,
        description: ext.manifest.description,
        active: ext.active,
        contributes: ext.manifest.contributes,
      };
    });

    // Activate extension
    ipcMain.handle("extensions:activate", async (_, id: string) => {
      return this.activateExtension(id);
    });

    // Deactivate extension
    ipcMain.handle("extensions:deactivate", async (_, id: string) => {
      return this.deactivateExtension(id);
    });

    // Reload extensions
    ipcMain.handle("extensions:reload", async () => {
      await this.discoverExtensions();
      return this.getExtensionList();
    });
  }

  /**
   * Discover all extensions in configured paths
   */
  async discoverExtensions(): Promise<void> {
    console.log("[ExtensionLoader] Discovering extensions...");

    for (const searchPath of this.extensionPaths) {
      if (!fs.existsSync(searchPath)) {
        continue;
      }

      try {
        const entries = fs.readdirSync(searchPath, { withFileTypes: true });

        for (const entry of entries) {
          if (!entry.isDirectory()) continue;

          const extensionDir = path.join(searchPath, entry.name);
          const manifestPath = path.join(extensionDir, "package.json");

          if (!fs.existsSync(manifestPath)) continue;

          try {
            const manifestContent = fs.readFileSync(manifestPath, "utf-8");
            const pkg = JSON.parse(manifestContent);

            // Check if this is an Oort extension
            if (!pkg.oort) continue;

            const manifest: ExtensionManifest = {
              id: pkg.oort.id || pkg.name,
              name: pkg.oort.name || pkg.name,
              version: pkg.version,
              description: pkg.description,
              author: pkg.author,
              type: pkg.oort.type || "extension",
              activationEvents: pkg.oort.activationEvents,
              main: pkg.oort.main || pkg.main || "./dist/index.js",
              contributes: pkg.oort.contributes,
            };

            const extension: LoadedExtension = {
              id: manifest.id,
              manifest,
              extensionPath: extensionDir,
              active: false,
            };

            this.extensions.set(manifest.id, extension);
            console.log(`[ExtensionLoader] Found extension: ${manifest.name} (${manifest.id})`);
          } catch (err) {
            console.error(`[ExtensionLoader] Failed to load extension from ${extensionDir}:`, err);
          }
        }
      } catch (err) {
        console.error(`[ExtensionLoader] Failed to scan ${searchPath}:`, err);
      }
    }

    console.log(`[ExtensionLoader] Discovered ${this.extensions.size} extensions`);
  }

  /**
   * Activate an extension
   */
  async activateExtension(id: string): Promise<boolean> {
    const extension = this.extensions.get(id);
    if (!extension) {
      console.error(`[ExtensionLoader] Extension not found: ${id}`);
      return false;
    }

    if (extension.active) {
      console.log(`[ExtensionLoader] Extension already active: ${id}`);
      return true;
    }

    try {
      // For now, we just mark it as active
      // The actual module loading happens in the renderer process
      extension.active = true;

      // Notify renderer that extension is now active
      this.notifyRenderer("extension:activated", { id });

      console.log(`[ExtensionLoader] Activated extension: ${extension.manifest.name}`);
      return true;
    } catch (err) {
      console.error(`[ExtensionLoader] Failed to activate extension ${id}:`, err);
      return false;
    }
  }

  /**
   * Deactivate an extension
   */
  async deactivateExtension(id: string): Promise<boolean> {
    const extension = this.extensions.get(id);
    if (!extension) {
      console.error(`[ExtensionLoader] Extension not found: ${id}`);
      return false;
    }

    if (!extension.active) {
      console.log(`[ExtensionLoader] Extension already inactive: ${id}`);
      return true;
    }

    try {
      extension.active = false;

      // Notify renderer that extension is now inactive
      this.notifyRenderer("extension:deactivated", { id });

      console.log(`[ExtensionLoader] Deactivated extension: ${extension.manifest.name}`);
      return true;
    } catch (err) {
      console.error(`[ExtensionLoader] Failed to deactivate extension ${id}:`, err);
      return false;
    }
  }

  /**
   * Get extension list for IPC
   */
  private getExtensionList() {
    return Array.from(this.extensions.values()).map((ext) => ({
      id: ext.id,
      name: ext.manifest.name,
      version: ext.manifest.version,
      description: ext.manifest.description,
      active: ext.active,
      contributes: ext.manifest.contributes,
    }));
  }

  /**
   * Notify renderer process of extension events
   */
  private notifyRenderer(channel: string, data: unknown): void {
    const windows = BrowserWindow.getAllWindows();
    for (const window of windows) {
      window.webContents.send(channel, data);
    }
  }

  /**
   * Get a specific extension
   */
  getExtension(id: string): LoadedExtension | undefined {
    return this.extensions.get(id);
  }

  /**
   * Get all loaded extensions
   */
  getAllExtensions(): LoadedExtension[] {
    return Array.from(this.extensions.values());
  }

  /**
   * Add a custom extension path
   */
  addExtensionPath(extensionPath: string): void {
    if (!this.extensionPaths.includes(extensionPath)) {
      this.extensionPaths.push(extensionPath);
    }
  }
}

// Singleton instance
export const extensionLoader = new ExtensionLoader();
