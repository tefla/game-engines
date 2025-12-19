/**
 * Scene Loader - Loads and parses .oort-scene files
 *
 * Handles:
 * - JSON parsing and validation
 * - Entity hierarchy building
 * - Component processing
 * - Prefab resolution (delegates to prefab-loader)
 */

import {
  OortScene,
  SceneEntity,
  SceneSettings,
  Transform3D,
  Component,
  DEFAULT_TRANSFORM,
  isValidScene,
  mergeTransforms,
  EntityOverrides,
} from "./types";

// ============================================================================
// Scene Loader
// ============================================================================

export interface LoadSceneOptions {
  /** Base path for resolving relative asset paths */
  basePath?: string;

  /** Prefab loader for resolving prefab references */
  prefabLoader?: PrefabLoaderInterface;

  /** Callback for progress updates */
  onProgress?: (progress: number, message: string) => void;
}

export interface PrefabLoaderInterface {
  loadPrefab(path: string): Promise<OortScene>;
  isLoaded(path: string): boolean;
}

export interface LoadedScene {
  /** Original scene data */
  scene: OortScene;

  /** Flattened entity map (includes resolved prefabs) */
  entities: Map<string, ResolvedEntity>;

  /** Root entity IDs (no parent) */
  rootIds: string[];
}

export interface ResolvedEntity {
  /** Original entity data (with prefab already merged) */
  entity: SceneEntity;

  /** Resolved absolute transform (includes parent transforms) */
  worldTransform: Transform3D;

  /** Parent entity ID */
  parentId: string | null;

  /** Child entity IDs */
  childIds: string[];

  /** Source prefab path (if from prefab) */
  prefabSource?: string;
}

/**
 * Load and parse a scene from JSON content
 */
export async function loadScene(
  content: string,
  options: LoadSceneOptions = {}
): Promise<LoadedScene> {
  const { basePath = "", onProgress } = options;

  onProgress?.(0, "Parsing scene...");

  // Parse JSON
  let sceneData: unknown;
  try {
    sceneData = JSON.parse(content);
  } catch (e) {
    throw new Error(`Invalid scene JSON: ${e}`);
  }

  // Validate structure
  if (!isValidScene(sceneData)) {
    throw new Error("Invalid scene format: missing required fields");
  }

  const scene = sceneData as OortScene;
  onProgress?.(10, "Validating scene...");

  // Build entity map
  const entities = new Map<string, ResolvedEntity>();
  const rootIds: string[] = [];

  // First pass: resolve prefabs and collect all entities
  onProgress?.(20, "Resolving entities...");
  await resolveEntities(scene.entities, null, entities, rootIds, options);

  // Second pass: calculate world transforms
  onProgress?.(80, "Calculating transforms...");
  calculateWorldTransforms(entities, rootIds);

  onProgress?.(100, "Scene loaded");

  return { scene, entities, rootIds };
}

/**
 * Recursively resolve entities, including prefabs
 */
async function resolveEntities(
  entities: SceneEntity[],
  parentId: string | null,
  result: Map<string, ResolvedEntity>,
  rootIds: string[],
  options: LoadSceneOptions
): Promise<void> {
  for (const entity of entities) {
    let resolvedEntity = entity;

    // Handle prefab
    if (entity.prefab && options.prefabLoader) {
      resolvedEntity = await resolvePrefabEntity(entity, options.prefabLoader);
    }

    // Create resolved entry
    const resolved: ResolvedEntity = {
      entity: resolvedEntity,
      worldTransform: { ...resolvedEntity.transform },
      parentId,
      childIds: [],
      prefabSource: entity.prefab,
    };

    result.set(resolvedEntity.id, resolved);

    // Track root entities
    if (!parentId) {
      rootIds.push(resolvedEntity.id);
    } else {
      // Add to parent's children
      const parent = result.get(parentId);
      if (parent) {
        parent.childIds.push(resolvedEntity.id);
      }
    }

    // Process children (inline or from parent reference)
    if (resolvedEntity.children && resolvedEntity.children.length > 0) {
      await resolveEntities(
        resolvedEntity.children,
        resolvedEntity.id,
        result,
        rootIds,
        options
      );
    }
  }

  // Handle parent references (entities with parent but not inline children)
  for (const entity of entities) {
    if (entity.parent && !result.get(entity.parent)?.childIds.includes(entity.id)) {
      const parent = result.get(entity.parent);
      if (parent) {
        parent.childIds.push(entity.id);
        const resolved = result.get(entity.id);
        if (resolved) {
          resolved.parentId = entity.parent;
        }
      }
    }
  }
}

/**
 * Resolve a prefab entity by loading the prefab and applying overrides
 */
async function resolvePrefabEntity(
  entity: SceneEntity,
  prefabLoader: PrefabLoaderInterface
): Promise<SceneEntity> {
  // Load prefab scene
  const prefabScene = await prefabLoader.loadPrefab(entity.prefab!);

  // Get the root entity from prefab (first entity)
  const prefabRoot = prefabScene.entities[0];
  if (!prefabRoot) {
    throw new Error(`Prefab ${entity.prefab} has no root entity`);
  }

  // Deep merge entity with overrides
  const merged = deepMergeEntity(prefabRoot, entity, entity.overrides);

  // Use instance ID, not prefab ID
  merged.id = entity.id;
  merged.name = entity.name || prefabRoot.name;

  return merged;
}

/**
 * Deep merge a prefab entity with instance overrides
 */
function deepMergeEntity(
  prefab: SceneEntity,
  instance: SceneEntity,
  overrides?: EntityOverrides
): SceneEntity {
  const result: SceneEntity = {
    ...prefab,
    id: instance.id,
    name: instance.name || prefab.name,
    enabled: instance.enabled ?? prefab.enabled,
    transform: mergeTransforms(
      prefab.transform,
      overrides?.transform || instance.transform || {}
    ),
    components: mergeComponents(prefab.components, overrides?.components),
    properties: {
      ...prefab.properties,
      ...overrides?.properties,
      ...instance.properties,
    },
  };

  // Merge children if both have them
  if (prefab.children && overrides?.children) {
    result.children = prefab.children.map((child) => {
      const childOverride = overrides.children?.[child.id];
      if (childOverride) {
        return deepMergeEntity(child, child, childOverride);
      }
      return child;
    });
  }

  return result;
}

/**
 * Merge component overrides into prefab components
 */
function mergeComponents(
  prefabComponents: Component[],
  overrides?: import("./types").ComponentOverride[]
): Component[] {
  if (!overrides || overrides.length === 0) {
    return [...prefabComponents];
  }

  const result = prefabComponents.map((comp, index) => {
    // Find matching override
    const override = overrides.find((o) => {
      if (o.type !== comp.type) return false;
      if (o.index !== undefined) return o.index === index;
      return true; // Match first of type
    });

    if (override) {
      // Merge override into component
      const { type, index: _, ...overrideProps } = override;
      return { ...comp, ...overrideProps } as Component;
    }

    return comp;
  });

  return result;
}

/**
 * Calculate world transforms by traversing hierarchy
 */
function calculateWorldTransforms(
  entities: Map<string, ResolvedEntity>,
  rootIds: string[]
): void {
  function processEntity(entityId: string, parentWorldTransform: Transform3D): void {
    const resolved = entities.get(entityId);
    if (!resolved) return;

    // Calculate world transform
    resolved.worldTransform = combineTransforms(
      parentWorldTransform,
      resolved.entity.transform
    );

    // Process children
    for (const childId of resolved.childIds) {
      processEntity(childId, resolved.worldTransform);
    }
  }

  // Process all root entities
  for (const rootId of rootIds) {
    processEntity(rootId, DEFAULT_TRANSFORM);
  }
}

/**
 * Combine parent and child transforms
 */
function combineTransforms(parent: Transform3D, child: Transform3D): Transform3D {
  // For now, simple addition (proper implementation would use matrices)
  return {
    position: [
      parent.position[0] + child.position[0] * parent.scale[0],
      parent.position[1] + child.position[1] * parent.scale[1],
      parent.position[2] + child.position[2] * parent.scale[2],
    ],
    rotation: [
      parent.rotation[0] + child.rotation[0],
      parent.rotation[1] + child.rotation[1],
      parent.rotation[2] + child.rotation[2],
    ],
    scale: [
      parent.scale[0] * child.scale[0],
      parent.scale[1] * child.scale[1],
      parent.scale[2] * child.scale[2],
    ],
  };
}

// ============================================================================
// Scene Serialization
// ============================================================================

/**
 * Serialize a scene to JSON string
 */
export function serializeScene(scene: OortScene, pretty = true): string {
  return JSON.stringify(scene, null, pretty ? 2 : 0);
}

/**
 * Create an empty scene with defaults
 */
export function createEmptyScene(name: string): OortScene {
  return {
    version: "1.0",
    name,
    settings: {
      background: "#1a1a2e",
      ambientLight: {
        color: "#ffffff",
        intensity: 0.4,
      },
    },
    entities: [],
  };
}

/**
 * Create a default entity
 */
export function createEntity(
  id: string,
  name: string,
  options: Partial<SceneEntity> = {}
): SceneEntity {
  return {
    id,
    name,
    enabled: true,
    transform: { ...DEFAULT_TRANSFORM },
    components: [],
    ...options,
  };
}
