/**
 * Extension API Types - Core type definitions for Oort Editor extensions
 */

import type { ComponentType } from "react";

// ============================================================================
// Extension Manifest
// ============================================================================

/**
 * Extension manifest (defined in extension's package.json under "oort" key)
 */
export interface ExtensionManifest {
  /** Unique extension identifier */
  id: string;
  /** Display name */
  name: string;
  /** Version string */
  version: string;
  /** Extension description */
  description?: string;
  /** Author name */
  author?: string;
  /** Extension type */
  type: "extension" | "theme";
  /** Events that trigger extension activation */
  activationEvents?: string[];
  /** Main entry point */
  main: string;
  /** Extension contributions (panels, commands, etc.) */
  contributes?: ExtensionContributions;
}

/**
 * Declarative contributions from an extension
 */
export interface ExtensionContributions {
  /** Panel definitions */
  panels?: PanelContribution[];
  /** Command definitions */
  commands?: CommandContribution[];
  /** Menu items */
  menus?: MenuContribution[];
  /** Asset type handlers */
  assetTypes?: AssetTypeContribution[];
  /** Keybindings */
  keybindings?: KeybindingContribution[];
  /** Configuration schema */
  configuration?: ConfigurationContribution;
}

// ============================================================================
// Panel API
// ============================================================================

/**
 * Panel definition for registration
 */
export interface PanelDefinition {
  /** Unique panel identifier */
  id: string;
  /** Display title */
  title: string;
  /** Icon identifier or emoji */
  icon: string;
  /** React component to render */
  component: ComponentType<PanelProps>;
  /** Default dock location */
  defaultLocation?: "left" | "right" | "bottom" | "center";
  /** Only allow one instance */
  singleton?: boolean;
  /** Panel category for grouping */
  category?: string;
}

/**
 * Props passed to panel components
 */
export interface PanelProps {
  /** Panel instance ID */
  panelId: string;
  /** Unique instance identifier (for non-singleton panels) */
  instanceId: string;
  /** Panel context data */
  context?: Record<string, unknown>;
}

/**
 * Declarative panel contribution
 */
export interface PanelContribution {
  id: string;
  title: string;
  icon: string;
  defaultLocation?: "left" | "right" | "bottom" | "center";
  singleton?: boolean;
  category?: string;
}

// ============================================================================
// Command API
// ============================================================================

/**
 * Command definition for registration
 */
export interface CommandDefinition {
  /** Unique command identifier */
  id: string;
  /** Display title */
  title: string;
  /** Category for grouping in palette */
  category?: string;
  /** Default keybinding */
  keybinding?: string;
  /** Condition for when command is available */
  when?: string | (() => boolean);
  /** Command handler */
  handler: () => void | Promise<void>;
}

/**
 * Declarative command contribution
 */
export interface CommandContribution {
  id: string;
  title: string;
  category?: string;
  keybinding?: string;
  when?: string;
}

// ============================================================================
// Menu API
// ============================================================================

/**
 * Menu location identifiers
 */
export type MenuLocation =
  | "menubar.file"
  | "menubar.edit"
  | "menubar.view"
  | "menubar.run"
  | "menubar.help"
  | "context.editor"
  | "context.explorer"
  | "context.inspector";

/**
 * Menu item definition
 */
export interface MenuItemDefinition {
  /** Command to execute */
  commandId: string;
  /** Display label (overrides command title) */
  label?: string;
  /** Ordering group */
  group?: string;
  /** Order within group */
  order?: number;
  /** Condition for visibility */
  when?: string | (() => boolean);
}

/**
 * Declarative menu contribution
 */
export interface MenuContribution {
  location: MenuLocation;
  items: MenuItemDefinition[];
}

// ============================================================================
// Asset API
// ============================================================================

/**
 * Asset type definition
 */
export interface AssetTypeDefinition {
  /** File extensions (e.g., [".png", ".jpg"]) */
  extensions: string[];
  /** Display name for the type */
  name: string;
  /** Icon identifier */
  icon: string;
  /** Panel ID to open this asset type */
  editorId?: string;
  /** Generate thumbnail for asset browser */
  thumbnail?: (path: string) => Promise<string | null>;
  /** Preview component for inspector */
  preview?: ComponentType<{ path: string }>;
}

/**
 * Declarative asset type contribution
 */
export interface AssetTypeContribution {
  extensions: string[];
  name: string;
  icon: string;
  editorId?: string;
}

// ============================================================================
// Keybinding API
// ============================================================================

/**
 * Keybinding definition
 */
export interface KeybindingDefinition {
  /** Key combination (e.g., "Cmd+S", "Ctrl+Shift+P") */
  key: string;
  /** Command to execute */
  commandId: string;
  /** Platform-specific override */
  mac?: string;
  /** Condition for when binding is active */
  when?: string | (() => boolean);
}

/**
 * Declarative keybinding contribution
 */
export interface KeybindingContribution {
  key: string;
  commandId: string;
  mac?: string;
  when?: string;
}

// ============================================================================
// Configuration API
// ============================================================================

/**
 * Configuration schema for extension settings
 */
export interface ConfigurationContribution {
  /** Configuration title */
  title: string;
  /** Configuration properties */
  properties: Record<string, ConfigurationProperty>;
}

/**
 * Single configuration property
 */
export interface ConfigurationProperty {
  type: "string" | "number" | "boolean" | "array" | "object";
  default?: unknown;
  description?: string;
  enum?: unknown[];
  enumDescriptions?: string[];
  minimum?: number;
  maximum?: number;
}

// ============================================================================
// State API
// ============================================================================

/**
 * Extension state storage
 */
export interface ExtensionState {
  /** Get value from extension state */
  get<T>(key: string): T | undefined;
  /** Set value in extension state */
  set<T>(key: string, value: T): void;
  /** Get value from global editor state */
  getGlobal<T>(path: string): T | undefined;
  /** Subscribe to state changes */
  subscribe(pattern: string, callback: (value: unknown) => void): () => void;
}

// ============================================================================
// Message Bus API
// ============================================================================

/**
 * Event emitter interface for extension communication
 */
export interface MessageBusAPI {
  /** Emit an event */
  emit(channel: string, data?: unknown): void;
  /** Subscribe to an event */
  on(channel: string, handler: (data: unknown) => void): () => void;
  /** Subscribe to an event (once) */
  once(channel: string, handler: (data: unknown) => void): void;
}

// ============================================================================
// Extension Context
// ============================================================================

/**
 * Context object passed to extension activate function
 */
export interface ExtensionContext {
  /** Extension ID */
  extensionId: string;
  /** Extension directory path */
  extensionPath: string;
  /** Panel registration API */
  panels: PanelAPI;
  /** Command registration API */
  commands: CommandAPI;
  /** Menu registration API */
  menus: MenuAPI;
  /** Asset type registration API */
  assets: AssetAPI;
  /** Keybinding registration API */
  keybindings: KeybindingAPI;
  /** State management API */
  state: ExtensionState;
  /** Message bus API */
  messageBus: MessageBusAPI;
  /** Disposables to clean up on deactivation */
  subscriptions: Disposable[];
}

/**
 * Disposable resource
 */
export interface Disposable {
  dispose(): void;
}

// ============================================================================
// API Interfaces
// ============================================================================

export interface PanelAPI {
  /** Register a panel type */
  register(definition: PanelDefinition): Disposable;
  /** Open a panel */
  open(id: string, context?: Record<string, unknown>): void;
  /** Close a panel instance */
  close(instanceId: string): void;
  /** Check if panel is open */
  isOpen(id: string): boolean;
  /** Get all registered panel IDs */
  getRegistered(): string[];
}

export interface CommandAPI {
  /** Register a command */
  register(definition: CommandDefinition): Disposable;
  /** Execute a command */
  execute(id: string, ...args: unknown[]): Promise<unknown>;
  /** Check if command exists */
  exists(id: string): boolean;
  /** Get all commands */
  getAll(): CommandDefinition[];
}

export interface MenuAPI {
  /** Add items to a menu location */
  addItems(location: MenuLocation, items: MenuItemDefinition[]): Disposable;
  /** Remove items from a menu location */
  removeItems(location: MenuLocation, commandIds: string[]): void;
}

export interface AssetAPI {
  /** Register an asset type handler */
  register(definition: AssetTypeDefinition): Disposable;
  /** Get handler for a file extension */
  getHandler(extension: string): AssetTypeDefinition | undefined;
  /** Get all registered asset types */
  getAll(): AssetTypeDefinition[];
}

export interface KeybindingAPI {
  /** Register a keybinding */
  register(definition: KeybindingDefinition): Disposable;
  /** Get keybinding for a command */
  getForCommand(commandId: string): string | undefined;
}

// ============================================================================
// Extension Entry Point
// ============================================================================

/**
 * Extension module interface
 */
export interface ExtensionModule {
  /** Called when extension is activated */
  activate(context: ExtensionContext): void | Promise<void>;
  /** Called when extension is deactivated */
  deactivate?(): void | Promise<void>;
}
