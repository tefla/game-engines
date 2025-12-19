// Panel registry - manages available panel types
import React from "react";
import { messageBus, Channels } from "./message-bus";

export interface PanelProps {
  panelId: string;
  instanceId: string;
}

export interface PanelDefinition {
  id: string;
  title: string;
  icon?: string;
  component: React.ComponentType<PanelProps>;
  defaultLocation?: "left" | "right" | "bottom" | "center";
  singleton?: boolean; // Only one instance allowed
  closeable?: boolean; // Can be closed by user
  minWidth?: number;
  minHeight?: number;
}

export interface PanelInstance {
  id: string; // Unique instance ID
  panelId: string; // Reference to PanelDefinition.id
  title: string;
  state?: any; // Panel-specific state
}

class PanelRegistry {
  private definitions: Map<string, PanelDefinition> = new Map();
  private instances: Map<string, PanelInstance> = new Map();
  private instanceCounter = 0;

  /**
   * Register a new panel type
   * Note: Silently skips if already registered (expected in React StrictMode)
   */
  register(definition: PanelDefinition): void {
    // Skip if already registered with same component (React StrictMode re-runs effects)
    if (this.definitions.has(definition.id)) {
      return;
    }

    this.definitions.set(definition.id, {
      closeable: true,
      ...definition,
    });

    messageBus.emit(Channels.PANEL_REGISTERED, definition);
  }

  /**
   * Unregister a panel type
   */
  unregister(id: string): void {
    this.definitions.delete(id);
  }

  /**
   * Get a panel definition by ID
   */
  getDefinition(id: string): PanelDefinition | undefined {
    return this.definitions.get(id);
  }

  /**
   * Get all registered panel definitions
   */
  getAllDefinitions(): PanelDefinition[] {
    return Array.from(this.definitions.values());
  }

  /**
   * Create a new panel instance
   */
  createInstance(panelId: string, initialState?: any): PanelInstance | null {
    const definition = this.definitions.get(panelId);
    if (!definition) {
      console.error(`Panel ${panelId} not found`);
      return null;
    }

    // Check singleton constraint
    if (definition.singleton) {
      const existing = this.findInstanceByPanelId(panelId);
      if (existing) {
        return existing;
      }
    }

    const instanceId = `${panelId}-${++this.instanceCounter}`;
    const instance: PanelInstance = {
      id: instanceId,
      panelId,
      title: definition.title,
      state: initialState,
    };

    this.instances.set(instanceId, instance);
    messageBus.emit(Channels.PANEL_OPEN, instance);

    return instance;
  }

  /**
   * Remove a panel instance
   */
  removeInstance(instanceId: string): void {
    const instance = this.instances.get(instanceId);
    if (instance) {
      this.instances.delete(instanceId);
      messageBus.emit(Channels.PANEL_CLOSE, instance);
    }
  }

  /**
   * Get a panel instance by ID
   */
  getInstance(instanceId: string): PanelInstance | undefined {
    return this.instances.get(instanceId);
  }

  /**
   * Find instance by panel type ID
   */
  findInstanceByPanelId(panelId: string): PanelInstance | undefined {
    for (const instance of this.instances.values()) {
      if (instance.panelId === panelId) {
        return instance;
      }
    }
    return undefined;
  }

  /**
   * Get all instances of a panel type
   */
  getInstancesByPanelId(panelId: string): PanelInstance[] {
    return Array.from(this.instances.values()).filter(
      (instance) => instance.panelId === panelId
    );
  }

  /**
   * Get all panel instances
   */
  getAllInstances(): PanelInstance[] {
    return Array.from(this.instances.values());
  }

  /**
   * Clear all panel instances (used when switching layouts)
   */
  clearInstances(): void {
    this.instances.clear();
    this.instanceCounter = 0;
  }

  /**
   * Update panel instance state
   */
  updateInstanceState(instanceId: string, state: any): void {
    const instance = this.instances.get(instanceId);
    if (instance) {
      instance.state = { ...instance.state, ...state };
    }
  }

  /**
   * Update panel instance title
   */
  updateInstanceTitle(instanceId: string, title: string): void {
    const instance = this.instances.get(instanceId);
    if (instance) {
      instance.title = title;
    }
  }
}

// Singleton instance
export const panelRegistry = new PanelRegistry();

// Hook for accessing panel registry
export function usePanelRegistry() {
  return panelRegistry;
}
