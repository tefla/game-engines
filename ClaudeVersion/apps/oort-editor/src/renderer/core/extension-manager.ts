/**
 * Extension Manager - Renderer-side extension management
 *
 * Handles loading extension contributions (panels, commands, etc.)
 * and provides the extension context API for extensions to use.
 */

import { panelRegistry, PanelDefinition } from "./panel-registry";
import { commandRegistry, Command } from "./command-registry";
import { keybindingManager } from "./keybinding-manager";
import { messageBus } from "./message-bus";

/**
 * Extension info from main process
 */
export interface ExtensionInfo {
  id: string;
  name: string;
  version: string;
  description?: string;
  active: boolean;
  contributes?: ExtensionContributions;
}

/**
 * Extension contributions
 */
export interface ExtensionContributions {
  panels?: PanelContribution[];
  commands?: CommandContribution[];
  assetTypes?: AssetTypeContribution[];
}

export interface PanelContribution {
  id: string;
  title: string;
  icon: string;
  defaultLocation?: "left" | "right" | "bottom" | "center";
  singleton?: boolean;
  category?: string;
}

export interface CommandContribution {
  id: string;
  title: string;
  category?: string;
  keybinding?: string;
  when?: string;
}

export interface AssetTypeContribution {
  extensions: string[];
  name: string;
  icon: string;
  editorId?: string;
}

/**
 * Asset type handler
 */
export interface AssetTypeHandler {
  extensions: string[];
  name: string;
  icon: string;
  editorId?: string;
}

/**
 * Extension manager for the renderer process
 */
class ExtensionManager {
  private extensions = new Map<string, ExtensionInfo>();
  private assetTypes = new Map<string, AssetTypeHandler>();
  private initialized = false;

  /**
   * Initialize the extension manager
   */
  async initialize(): Promise<void> {
    if (this.initialized) return;

    // Load extensions from main process
    await this.loadExtensions();

    // Listen for extension events
    window.electronAPI?.on("extension:activated", (data: any) => {
      this.handleExtensionActivated(data.id);
    });

    window.electronAPI?.on("extension:deactivated", (data: any) => {
      this.handleExtensionDeactivated(data.id);
    });

    this.initialized = true;
    console.log("[ExtensionManager] Initialized");
  }

  /**
   * Load all extensions from main process
   */
  async loadExtensions(): Promise<void> {
    const extensions = await window.electronAPI?.getExtensions();
    if (!extensions) return;

    for (const ext of extensions) {
      this.extensions.set(ext.id, ext);

      // Register contributions if extension is active
      if (ext.active && ext.contributes) {
        this.registerContributions(ext.id, ext.contributes);
      }
    }

    console.log(`[ExtensionManager] Loaded ${extensions.length} extensions`);
  }

  /**
   * Register extension contributions
   */
  private registerContributions(extensionId: string, contributes: ExtensionContributions): void {
    // Register commands
    if (contributes.commands) {
      for (const cmd of contributes.commands) {
        this.registerCommand(extensionId, cmd);
      }
    }

    // Register asset types
    if (contributes.assetTypes) {
      for (const assetType of contributes.assetTypes) {
        this.registerAssetType(extensionId, assetType);
      }
    }

    // Note: Panel components need to be loaded separately
    // as they require React components that can't be serialized over IPC
    if (contributes.panels) {
      console.log(`[ExtensionManager] Extension ${extensionId} declares ${contributes.panels.length} panels`);
    }
  }

  /**
   * Register a command from extension
   */
  private registerCommand(extensionId: string, cmd: CommandContribution): void {
    const command: Command = {
      id: cmd.id,
      title: cmd.title,
      category: cmd.category || extensionId,
      keybinding: cmd.keybinding,
      handler: () => {
        // Emit event for extension to handle
        messageBus.emit(`extension:command:${cmd.id}`, { extensionId });
      },
    };

    commandRegistry.register(command);

    if (cmd.keybinding) {
      keybindingManager.bind(cmd.keybinding, cmd.id);
    }
  }

  /**
   * Register an asset type from extension
   */
  private registerAssetType(extensionId: string, assetType: AssetTypeContribution): void {
    const handler: AssetTypeHandler = {
      extensions: assetType.extensions,
      name: assetType.name,
      icon: assetType.icon,
      editorId: assetType.editorId,
    };

    // Register for each extension
    for (const ext of assetType.extensions) {
      this.assetTypes.set(ext.toLowerCase(), handler);
    }

    console.log(`[ExtensionManager] Registered asset type: ${assetType.name} (${assetType.extensions.join(", ")})`);
  }

  /**
   * Handle extension activated
   */
  private async handleExtensionActivated(id: string): Promise<void> {
    const ext = await window.electronAPI?.getExtension(id);
    if (!ext) return;

    this.extensions.set(id, ext);

    if (ext.contributes) {
      this.registerContributions(id, ext.contributes);
    }

    messageBus.emit("extension:activated", { id, extension: ext });
    console.log(`[ExtensionManager] Extension activated: ${ext.name}`);
  }

  /**
   * Handle extension deactivated
   */
  private handleExtensionDeactivated(id: string): void {
    const ext = this.extensions.get(id);
    if (!ext) return;

    ext.active = false;

    // Note: We don't unregister contributions for simplicity
    // In a full implementation, we'd track and unregister them

    messageBus.emit("extension:deactivated", { id });
    console.log(`[ExtensionManager] Extension deactivated: ${ext.name}`);
  }

  /**
   * Get all extensions
   */
  getExtensions(): ExtensionInfo[] {
    return Array.from(this.extensions.values());
  }

  /**
   * Get extension by ID
   */
  getExtension(id: string): ExtensionInfo | undefined {
    return this.extensions.get(id);
  }

  /**
   * Get asset type handler for a file extension
   */
  getAssetHandler(extension: string): AssetTypeHandler | undefined {
    return this.assetTypes.get(extension.toLowerCase());
  }

  /**
   * Get all registered asset types
   */
  getAssetTypes(): AssetTypeHandler[] {
    return Array.from(new Set(this.assetTypes.values()));
  }

  /**
   * Activate an extension
   */
  async activateExtension(id: string): Promise<boolean> {
    const result = await window.electronAPI?.activateExtension(id);
    return result ?? false;
  }

  /**
   * Deactivate an extension
   */
  async deactivateExtension(id: string): Promise<boolean> {
    const result = await window.electronAPI?.deactivateExtension(id);
    return result ?? false;
  }

  /**
   * Reload all extensions
   */
  async reloadExtensions(): Promise<void> {
    this.extensions.clear();
    await this.loadExtensions();
    messageBus.emit("extensions:reloaded", {});
  }
}

// Singleton instance
export const extensionManager = new ExtensionManager();
