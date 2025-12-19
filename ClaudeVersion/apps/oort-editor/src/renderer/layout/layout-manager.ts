// Layout Manager - Handles layout serialization, presets, and persistence

import { LayoutNode, generateNodeId } from "./DockingLayout";
import { panelRegistry } from "@/core/panel-registry";
import { messageBus } from "@/core/message-bus";

// Serialized layout format (no React components, just data)
export interface SerializedLayoutNode {
  type: "row" | "column" | "tabs" | "panel";
  children?: SerializedLayoutNode[];
  panelType?: string; // Panel definition ID (e.g., "project-explorer")
  activeTab?: number;
  size?: number;
}

export interface LayoutPreset {
  id: string;
  name: string;
  description?: string;
  layout: SerializedLayoutNode;
}

// Built-in layout presets
export const LAYOUT_PRESETS: LayoutPreset[] = [
  {
    id: "default",
    name: "Default",
    description: "Standard layout with project explorer, editor, console, and inspector",
    layout: {
      type: "row",
      children: [
        { type: "panel", panelType: "project-explorer", size: 1 },
        {
          type: "column",
          size: 3,
          children: [
            {
              type: "tabs",
              size: 3,
              activeTab: 0,
              children: [{ type: "panel", panelType: "code-editor" }],
            },
            { type: "panel", panelType: "console", size: 1 },
          ],
        },
        { type: "panel", panelType: "inspector", size: 1 },
      ],
    },
  },
  {
    id: "editor-focus",
    name: "Editor Focus",
    description: "Maximized editor with minimal panels",
    layout: {
      type: "row",
      children: [
        { type: "panel", panelType: "project-explorer", size: 1 },
        {
          type: "tabs",
          size: 5,
          activeTab: 0,
          children: [{ type: "panel", panelType: "code-editor" }],
        },
      ],
    },
  },
  {
    id: "debugging",
    name: "Debugging",
    description: "Layout optimized for debugging with larger console",
    layout: {
      type: "row",
      children: [
        { type: "panel", panelType: "project-explorer", size: 1 },
        {
          type: "column",
          size: 3,
          children: [
            {
              type: "tabs",
              size: 2,
              activeTab: 0,
              children: [{ type: "panel", panelType: "code-editor" }],
            },
            { type: "panel", panelType: "console", size: 2 },
          ],
        },
        { type: "panel", panelType: "inspector", size: 1 },
      ],
    },
  },
  {
    id: "wide",
    name: "Wide Layout",
    description: "Horizontal layout with console on the right",
    layout: {
      type: "row",
      children: [
        { type: "panel", panelType: "project-explorer", size: 1 },
        {
          type: "tabs",
          size: 3,
          activeTab: 0,
          children: [{ type: "panel", panelType: "code-editor" }],
        },
        {
          type: "column",
          size: 1.5,
          children: [
            { type: "panel", panelType: "inspector", size: 1 },
            { type: "panel", panelType: "console", size: 1 },
          ],
        },
      ],
    },
  },
];

const STORAGE_KEY = "oort-editor-layout";
const CUSTOM_PRESETS_KEY = "oort-editor-custom-presets";

class LayoutManager {
  private customPresets: LayoutPreset[] = [];

  constructor() {
    this.loadCustomPresets();
  }

  /**
   * Serialize a live layout to a storable format
   */
  serializeLayout(layout: LayoutNode): SerializedLayoutNode {
    const serialize = (node: LayoutNode): SerializedLayoutNode => {
      const serialized: SerializedLayoutNode = {
        type: node.type,
      };

      if (node.size !== undefined) {
        serialized.size = node.size;
      }

      if (node.activeTab !== undefined) {
        serialized.activeTab = node.activeTab;
      }

      if (node.type === "panel" && node.instanceId) {
        // Get the panel type from the instance
        const instance = panelRegistry.getInstance(node.instanceId);
        if (instance) {
          serialized.panelType = instance.panelId;
        }
      }

      if (node.children) {
        serialized.children = node.children.map(serialize);
      }

      return serialized;
    };

    return serialize(layout);
  }

  /**
   * Deserialize a stored layout back to a live layout
   */
  deserializeLayout(serialized: SerializedLayoutNode): LayoutNode {
    const deserialize = (node: SerializedLayoutNode): LayoutNode => {
      const result: LayoutNode = {
        id: generateNodeId(),
        type: node.type,
      };

      if (node.size !== undefined) {
        result.size = node.size;
      }

      if (node.activeTab !== undefined) {
        result.activeTab = node.activeTab;
      }

      if (node.type === "panel" && node.panelType) {
        // Create a new panel instance
        const instance = panelRegistry.createInstance(node.panelType);
        if (instance) {
          result.instanceId = instance.id;
        }
      }

      if (node.children) {
        result.children = node.children.map(deserialize);
      }

      return result;
    };

    return deserialize(serialized);
  }

  /**
   * Save the current layout to localStorage
   */
  saveLayout(layout: LayoutNode): void {
    try {
      const serialized = this.serializeLayout(layout);
      localStorage.setItem(STORAGE_KEY, JSON.stringify(serialized));
      messageBus.emit("layout:saved", {});
    } catch (error) {
      console.error("Failed to save layout:", error);
    }
  }

  /**
   * Load layout from localStorage
   */
  loadLayout(): LayoutNode | null {
    try {
      const stored = localStorage.getItem(STORAGE_KEY);
      if (stored) {
        const serialized = JSON.parse(stored) as SerializedLayoutNode;
        return this.deserializeLayout(serialized);
      }
    } catch (error) {
      console.error("Failed to load layout:", error);
    }
    return null;
  }

  /**
   * Get all available presets (built-in + custom)
   */
  getPresets(): LayoutPreset[] {
    return [...LAYOUT_PRESETS, ...this.customPresets];
  }

  /**
   * Get a preset by ID
   */
  getPreset(id: string): LayoutPreset | undefined {
    return this.getPresets().find((p) => p.id === id);
  }

  /**
   * Apply a preset
   */
  applyPreset(id: string): LayoutNode | null {
    const preset = this.getPreset(id);
    if (!preset) {
      console.warn(`Preset "${id}" not found`);
      return null;
    }

    // Clear existing panel instances
    panelRegistry.clearInstances();

    // Deserialize the preset
    const layout = this.deserializeLayout(preset.layout);
    messageBus.emit("layout:preset-applied", { id, name: preset.name });
    return layout;
  }

  /**
   * Save current layout as a custom preset
   */
  saveAsPreset(layout: LayoutNode, name: string, description?: string): LayoutPreset {
    const id = `custom-${Date.now()}`;
    const preset: LayoutPreset = {
      id,
      name,
      description,
      layout: this.serializeLayout(layout),
    };

    this.customPresets.push(preset);
    this.saveCustomPresets();

    messageBus.emit("layout:preset-saved", preset);
    return preset;
  }

  /**
   * Delete a custom preset
   */
  deletePreset(id: string): boolean {
    const index = this.customPresets.findIndex((p) => p.id === id);
    if (index >= 0) {
      this.customPresets.splice(index, 1);
      this.saveCustomPresets();
      messageBus.emit("layout:preset-deleted", { id });
      return true;
    }
    return false;
  }

  /**
   * Reset to default layout
   */
  resetToDefault(): LayoutNode {
    localStorage.removeItem(STORAGE_KEY);
    return this.applyPreset("default")!;
  }

  // Private methods

  private loadCustomPresets(): void {
    try {
      const stored = localStorage.getItem(CUSTOM_PRESETS_KEY);
      if (stored) {
        this.customPresets = JSON.parse(stored);
      }
    } catch (error) {
      console.error("Failed to load custom presets:", error);
      this.customPresets = [];
    }
  }

  private saveCustomPresets(): void {
    try {
      localStorage.setItem(CUSTOM_PRESETS_KEY, JSON.stringify(this.customPresets));
    } catch (error) {
      console.error("Failed to save custom presets:", error);
    }
  }
}

// Singleton instance
export const layoutManager = new LayoutManager();
