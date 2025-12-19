/**
 * Disposable utilities for managing cleanup
 */

import type { Disposable } from "./types.js";

/**
 * Create a disposable from a cleanup function
 */
export function createDisposable(cleanup: () => void): Disposable {
  let disposed = false;
  return {
    dispose() {
      if (!disposed) {
        disposed = true;
        cleanup();
      }
    },
  };
}

/**
 * Store for managing multiple disposables
 */
export class DisposableStore implements Disposable {
  private disposables: Disposable[] = [];
  private disposed = false;

  /**
   * Add a disposable to the store
   */
  add<T extends Disposable>(disposable: T): T {
    if (this.disposed) {
      disposable.dispose();
    } else {
      this.disposables.push(disposable);
    }
    return disposable;
  }

  /**
   * Add a cleanup function as a disposable
   */
  addCleanup(cleanup: () => void): Disposable {
    return this.add(createDisposable(cleanup));
  }

  /**
   * Dispose all stored disposables
   */
  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;

    for (const disposable of this.disposables) {
      try {
        disposable.dispose();
      } catch (err) {
        console.error("Error disposing:", err);
      }
    }
    this.disposables = [];
  }

  /**
   * Clear all disposables without disposing them
   */
  clear(): Disposable[] {
    const items = this.disposables;
    this.disposables = [];
    return items;
  }

  /**
   * Check if the store is disposed
   */
  get isDisposed(): boolean {
    return this.disposed;
  }
}
