/**
 * Prefab Loader - Loads and caches prefab scenes (like Godot's PackedScene)
 *
 * Features:
 * - Load prefabs from .oort-scene files
 * - Cache loaded prefabs for efficient instantiation
 * - Deep-merge overrides when instantiating
 * - Handle nested prefabs recursively
 */

import {
  OortScene,
  SceneEntity,
  Transform3D,
  EntityOverrides,
  PrefabCacheEntry,
  DEFAULT_TRANSFORM,
  isValidScene,
  mergeTransforms,
} from "./types";
import { PrefabLoaderInterface } from "./scene-loader";

// ============================================================================
// Prefab Loader
// ============================================================================

export interface PrefabLoaderOptions {
  /** Function to read file contents */
  readFile: (path: string) => Promise<string>;

  /** Base path for resolving relative paths */
  basePath?: string;

  /** Maximum cache size (number of prefabs) */
  maxCacheSize?: number;

  /** Cache TTL in milliseconds (0 = no expiry) */
  cacheTTL?: number;
}

export class PrefabLoader implements PrefabLoaderInterface {
  private cache: Map<string, PrefabCacheEntry> = new Map();
  private loading: Map<string, Promise<OortScene>> = new Map();
  private options: Required<PrefabLoaderOptions>;

  constructor(options: PrefabLoaderOptions) {
    this.options = {
      basePath: "",
      maxCacheSize: 100,
      cacheTTL: 0,
      ...options,
    };
  }

  /**
   * Load a prefab scene from file
   */
  async loadPrefab(path: string): Promise<OortScene> {
    const fullPath = this.resolvePath(path);

    // Check cache
    const cached = this.cache.get(fullPath);
    if (cached && !this.isCacheExpired(cached)) {
      return cached.scene;
    }

    // Check if already loading
    const existing = this.loading.get(fullPath);
    if (existing) {
      return existing;
    }

    // Start loading
    const loadPromise = this.loadPrefabInternal(fullPath);
    this.loading.set(fullPath, loadPromise);

    try {
      const scene = await loadPromise;

      // Cache the result
      this.addToCache(fullPath, scene);

      return scene;
    } finally {
      this.loading.delete(fullPath);
    }
  }

  /**
   * Check if a prefab is loaded in cache
   */
  isLoaded(path: string): boolean {
    const fullPath = this.resolvePath(path);
    const cached = this.cache.get(fullPath);
    return cached !== undefined && !this.isCacheExpired(cached);
  }

  /**
   * Preload multiple prefabs
   */
  async preload(paths: string[]): Promise<void> {
    await Promise.all(paths.map((p) => this.loadPrefab(p)));
  }

  /**
   * Instantiate a prefab with overrides
   */
  async instantiate(
    prefabPath: string,
    instanceId: string,
    overrides?: InstantiateOptions
  ): Promise<SceneEntity> {
    const prefab = await this.loadPrefab(prefabPath);

    // Get root entity from prefab
    const root = prefab.entities[0];
    if (!root) {
      throw new Error(`Prefab ${prefabPath} has no root entity`);
    }

    // Create instance with overrides
    return this.createInstance(root, instanceId, prefabPath, overrides);
  }

  /**
   * Create an instance from a prefab entity
   */
  private createInstance(
    prefabEntity: SceneEntity,
    instanceId: string,
    prefabPath: string,
    options?: InstantiateOptions
  ): SceneEntity {
    // Start with prefab as base
    const instance: SceneEntity = {
      ...structuredClone(prefabEntity),
      id: instanceId,
      prefab: prefabPath,
    };

    // Apply transform overrides
    if (options?.position || options?.rotation || options?.scale) {
      instance.transform = mergeTransforms(instance.transform, {
        position: options.position,
        rotation: options.rotation,
        scale: options.scale,
      });
    }

    // Apply entity overrides (deep merge)
    if (options?.overrides) {
      this.applyOverrides(instance, options.overrides);
    }

    // Process children recursively
    if (instance.children) {
      instance.children = instance.children.map((child, index) => {
        const childId = `${instanceId}_child_${index}`;
        const childOverrides = options?.overrides?.children?.[child.id];
        return this.createInstance(
          child,
          childId,
          prefabPath,
          childOverrides ? { overrides: childOverrides } : undefined
        );
      });
    }

    return instance;
  }

  /**
   * Apply deep-merge overrides to an entity
   */
  private applyOverrides(entity: SceneEntity, overrides: EntityOverrides): void {
    // Override transform
    if (overrides.transform) {
      entity.transform = mergeTransforms(entity.transform, overrides.transform);
    }

    // Override properties
    if (overrides.properties) {
      entity.properties = {
        ...entity.properties,
        ...overrides.properties,
      };
    }

    // Override components
    if (overrides.components) {
      for (const compOverride of overrides.components) {
        const targetIndex =
          compOverride.index ??
          entity.components.findIndex((c) => c.type === compOverride.type);

        if (targetIndex >= 0 && targetIndex < entity.components.length) {
          const { type, index, ...props } = compOverride;
          entity.components[targetIndex] = {
            ...entity.components[targetIndex],
            ...props,
          } as typeof entity.components[number];
        }
      }
    }
  }

  /**
   * Internal prefab loading
   */
  private async loadPrefabInternal(path: string): Promise<OortScene> {
    // Read file
    const content = await this.options.readFile(path);

    // Parse JSON
    let data: unknown;
    try {
      data = JSON.parse(content);
    } catch (e) {
      throw new Error(`Invalid prefab JSON at ${path}: ${e}`);
    }

    // Validate
    if (!isValidScene(data)) {
      throw new Error(`Invalid prefab format at ${path}`);
    }

    const scene = data as OortScene;

    // Resolve nested prefabs
    await this.resolveNestedPrefabs(scene);

    return scene;
  }

  /**
   * Resolve any nested prefab references in a scene
   */
  private async resolveNestedPrefabs(scene: OortScene): Promise<void> {
    const resolveEntity = async (entity: SceneEntity): Promise<void> => {
      // If this entity references a prefab, load it
      if (entity.prefab) {
        const nestedPrefab = await this.loadPrefab(entity.prefab);
        const root = nestedPrefab.entities[0];

        if (root) {
          // Merge nested prefab into this entity
          const merged = this.mergeWithPrefab(entity, root);
          Object.assign(entity, merged);
        }
      }

      // Process children
      if (entity.children) {
        await Promise.all(entity.children.map(resolveEntity));
      }
    };

    await Promise.all(scene.entities.map(resolveEntity));
  }

  /**
   * Merge an entity with its prefab base
   */
  private mergeWithPrefab(
    instance: SceneEntity,
    prefab: SceneEntity
  ): SceneEntity {
    return {
      ...prefab,
      id: instance.id,
      name: instance.name || prefab.name,
      enabled: instance.enabled ?? prefab.enabled,
      transform: mergeTransforms(
        prefab.transform,
        instance.overrides?.transform || {}
      ),
      components: this.mergeComponents(
        prefab.components,
        instance.overrides?.components
      ),
      properties: {
        ...prefab.properties,
        ...instance.overrides?.properties,
        ...instance.properties,
      },
      children: prefab.children,
      prefab: instance.prefab,
      overrides: instance.overrides,
    };
  }

  /**
   * Merge component overrides
   */
  private mergeComponents(
    base: SceneEntity["components"],
    overrides?: EntityOverrides["components"]
  ): SceneEntity["components"] {
    if (!overrides) return [...base];

    return base.map((comp, index) => {
      const override = overrides.find(
        (o) => o.type === comp.type && (o.index === undefined || o.index === index)
      );

      if (override) {
        const { type, index: _, ...props } = override;
        return { ...comp, ...props } as typeof comp;
      }

      return comp;
    });
  }

  /**
   * Resolve a path relative to base path
   */
  private resolvePath(path: string): string {
    if (path.startsWith("/") || path.startsWith("http")) {
      return path;
    }
    return `${this.options.basePath}/${path}`.replace(/\/+/g, "/");
  }

  /**
   * Check if a cache entry has expired
   */
  private isCacheExpired(entry: PrefabCacheEntry): boolean {
    if (this.options.cacheTTL === 0) return false;
    return Date.now() - entry.loadedAt > this.options.cacheTTL;
  }

  /**
   * Add a scene to the cache
   */
  private addToCache(path: string, scene: OortScene): void {
    // Evict oldest if at capacity
    if (this.cache.size >= this.options.maxCacheSize) {
      const oldest = this.findOldestCacheEntry();
      if (oldest) {
        this.cache.delete(oldest);
      }
    }

    this.cache.set(path, {
      scene,
      loadedAt: Date.now(),
    });
  }

  /**
   * Find the oldest cache entry
   */
  private findOldestCacheEntry(): string | null {
    let oldest: string | null = null;
    let oldestTime = Infinity;

    for (const [path, entry] of this.cache) {
      if (entry.loadedAt < oldestTime) {
        oldestTime = entry.loadedAt;
        oldest = path;
      }
    }

    return oldest;
  }

  /**
   * Clear the cache
   */
  clearCache(): void {
    this.cache.clear();
  }

  /**
   * Get cache statistics
   */
  getCacheStats(): { size: number; maxSize: number; paths: string[] } {
    return {
      size: this.cache.size,
      maxSize: this.options.maxCacheSize,
      paths: Array.from(this.cache.keys()),
    };
  }
}

// ============================================================================
// Types
// ============================================================================

export interface InstantiateOptions {
  /** Override position */
  position?: [number, number, number];

  /** Override rotation (degrees) */
  rotation?: [number, number, number];

  /** Override scale */
  scale?: [number, number, number];

  /** Deep-merge overrides */
  overrides?: EntityOverrides;
}

// ============================================================================
// Factory
// ============================================================================

/**
 * Create a prefab loader for use in the editor
 */
export function createEditorPrefabLoader(
  readFile: (path: string) => Promise<string>,
  projectPath: string
): PrefabLoader {
  return new PrefabLoader({
    readFile,
    basePath: projectPath,
    maxCacheSize: 50,
    cacheTTL: 0, // No expiry in editor
  });
}

/**
 * Create a prefab loader for use in runtime
 */
export function createRuntimePrefabLoader(
  readFile: (path: string) => Promise<string>,
  basePath: string
): PrefabLoader {
  return new PrefabLoader({
    readFile,
    basePath,
    maxCacheSize: 100,
    cacheTTL: 5 * 60 * 1000, // 5 minute cache
  });
}
