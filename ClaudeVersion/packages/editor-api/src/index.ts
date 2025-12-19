/**
 * @oort/editor-api - Extension API for Oort Game Editor
 *
 * This package provides the types and utilities needed to create
 * extensions for the Oort Game Editor.
 *
 * @example
 * ```typescript
 * import { ExtensionContext, PanelDefinition } from "@oort/editor-api";
 *
 * export function activate(ctx: ExtensionContext) {
 *   ctx.panels.register({
 *     id: "my-panel",
 *     title: "My Panel",
 *     icon: "🎨",
 *     component: MyPanelComponent,
 *   });
 * }
 * ```
 */

// Export all types
export type {
  // Extension Manifest
  ExtensionManifest,
  ExtensionContributions,

  // Panel API
  PanelDefinition,
  PanelProps,
  PanelContribution,
  PanelAPI,

  // Command API
  CommandDefinition,
  CommandContribution,
  CommandAPI,

  // Menu API
  MenuLocation,
  MenuItemDefinition,
  MenuContribution,
  MenuAPI,

  // Asset API
  AssetTypeDefinition,
  AssetTypeContribution,
  AssetAPI,

  // Keybinding API
  KeybindingDefinition,
  KeybindingContribution,
  KeybindingAPI,

  // Configuration API
  ConfigurationContribution,
  ConfigurationProperty,

  // State API
  ExtensionState,

  // Message Bus API
  MessageBusAPI,

  // Extension Context
  ExtensionContext,
  Disposable,

  // Extension Module
  ExtensionModule,
} from "./types.js";

// Export utility functions
export { createDisposable, DisposableStore } from "./disposable.js";
export { defineExtension } from "./define.js";
