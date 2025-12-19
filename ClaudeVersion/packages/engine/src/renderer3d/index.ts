/**
 * Oort 3D Renderer Module
 *
 * Provides scene format types, loaders, and prefab system for 3D games.
 */

// Types
export * from "./types";

// Loaders
export {
  loadScene,
  serializeScene,
  createEmptyScene,
  createEntity,
  type LoadSceneOptions,
  type LoadedScene,
  type ResolvedEntity,
  type PrefabLoaderInterface,
} from "./scene-loader";

export {
  PrefabLoader,
  createEditorPrefabLoader,
  createRuntimePrefabLoader,
  type PrefabLoaderOptions,
  type InstantiateOptions,
} from "./prefab-loader";

// Scene Stdlib for Slate scripts
export {
  sceneStdlib,
  setEntityCallbacks,
  getEntity,
  getAllEntities,
  clearEntities,
  type Entity3D,
} from "./scene-stdlib";
