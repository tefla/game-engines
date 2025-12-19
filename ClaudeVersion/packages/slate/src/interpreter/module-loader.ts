// Module Loader - Handles loading and caching Slate modules

import type { SlateValue, SlateRecord } from "@oort/core";
import { Record } from "@oort/core";

/**
 * Interface for resolving module source code.
 * Implementations can read from VFS, filesystem, or other sources.
 */
export interface ModuleResolver {
  /**
   * Read the source code for a module.
   * @param path The module path (e.g., "math", "game/utils")
   * @returns The source code of the module
   * @throws Error if module not found
   */
  readModule(path: string): string;

  /**
   * Resolve a module path to its full path.
   * @param path The module path
   * @param fromPath The path of the importing module (for relative imports)
   * @returns The resolved full path
   */
  resolvePath(path: string, fromPath?: string): string;
}

/**
 * Module cache entry containing the exported bindings.
 */
interface CacheEntry {
  exports: SlateRecord;
  dependencies: Set<string>;
}

/**
 * ModuleLoader handles loading, parsing, executing, and caching Slate modules.
 * It works with a ModuleResolver to read source code and an interpreter factory
 * to execute modules in isolated scopes.
 */
export class ModuleLoader {
  private cache: Map<string, CacheEntry> = new Map();
  private resolver: ModuleResolver;
  private interpreterFactory: (
    moduleLoader: ModuleLoader
  ) => ModuleInterpreter;

  constructor(
    resolver: ModuleResolver,
    interpreterFactory: (moduleLoader: ModuleLoader) => ModuleInterpreter
  ) {
    this.resolver = resolver;
    this.interpreterFactory = interpreterFactory;
  }

  /**
   * Load a module and return its exports.
   * Uses cache if available.
   */
  load(path: string, fromPath?: string): SlateRecord {
    const fullPath = this.resolver.resolvePath(path, fromPath);

    // Check cache
    const cached = this.cache.get(fullPath);
    if (cached) {
      return cached.exports;
    }

    // Read source
    const source = this.resolver.readModule(fullPath);

    // Create interpreter for this module
    const interpreter = this.interpreterFactory(this);

    // Execute and get exports
    const exports = interpreter.executeModule(source, fullPath);

    // Cache the result
    this.cache.set(fullPath, {
      exports,
      dependencies: new Set(),
    });

    return exports;
  }

  /**
   * Invalidate a cached module (for hot reload).
   * Also invalidates modules that depend on it.
   */
  invalidate(path: string): void {
    const fullPath = this.resolver.resolvePath(path);
    this.cache.delete(fullPath);

    // Also invalidate dependents (TODO: track dependencies)
    // For now, just clear all cache on any invalidation
    this.cache.clear();
  }

  /**
   * Clear all cached modules.
   */
  clearCache(): void {
    this.cache.clear();
  }
}

/**
 * Interface for the interpreter used by ModuleLoader.
 * This is separate from the main Interpreter to avoid circular dependencies.
 */
export interface ModuleInterpreter {
  /**
   * Execute a module and return its exported bindings.
   * @param source The module source code
   * @param path The module path (for error reporting)
   * @returns A record containing all exported bindings
   */
  executeModule(source: string, path: string): SlateRecord;
}

/**
 * Simple in-memory module resolver for testing.
 */
export class InMemoryModuleResolver implements ModuleResolver {
  private modules: Map<string, string> = new Map();

  constructor(modules?: Record<string, string>) {
    if (modules) {
      for (const [path, source] of Object.entries(modules)) {
        this.modules.set(path, source);
      }
    }
  }

  addModule(path: string, source: string): void {
    this.modules.set(path, source);
  }

  readModule(path: string): string {
    const source = this.modules.get(path);
    if (source === undefined) {
      throw new Error(`Module not found: ${path}`);
    }
    return source;
  }

  resolvePath(path: string, _fromPath?: string): string {
    // For in-memory resolver, path is already the full path
    return path;
  }
}
