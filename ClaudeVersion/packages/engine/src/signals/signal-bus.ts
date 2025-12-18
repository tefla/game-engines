// Signal Bus - Event Management System

import { type SlateValue, Null, stringify } from "@oort/core";

export type SignalPath = string[];

export interface SignalHandler {
  id: number;
  signal: SignalPath;
  pattern: string; // For matching (including wildcards)
  filter?: SlateValue;
  handler: (data: SlateValue, signal: SignalPath) => void;
  once: boolean;
}

export interface SignalEvent {
  signal: SignalPath;
  data: SlateValue;
  timestamp: number;
}

export class SignalBus {
  private handlers: Map<number, SignalHandler> = new Map();
  private nextHandlerId = 1;
  private history: SignalEvent[] = [];
  private historyLimit = 100;
  private paused = false;
  private queuedEmissions: Array<{ signal: SignalPath; data: SlateValue }> = [];

  // Register a handler for a signal
  on(
    signal: SignalPath,
    handler: (data: SlateValue, signal: SignalPath) => void,
    options?: { filter?: SlateValue; once?: boolean }
  ): number {
    const id = this.nextHandlerId++;
    const pattern = this.createPattern(signal);

    this.handlers.set(id, {
      id,
      signal,
      pattern,
      filter: options?.filter,
      handler,
      once: options?.once ?? false,
    });

    return id;
  }

  // Register a one-time handler
  once(
    signal: SignalPath,
    handler: (data: SlateValue, signal: SignalPath) => void,
    filter?: SlateValue
  ): number {
    return this.on(signal, handler, { filter, once: true });
  }

  // Remove a handler by ID
  off(handlerId: number): boolean {
    return this.handlers.delete(handlerId);
  }

  // Remove all handlers for a specific signal
  offAll(signal?: SignalPath): void {
    if (!signal) {
      this.handlers.clear();
      return;
    }

    const pattern = signal.join(".");
    for (const [id, handler] of this.handlers) {
      if (handler.signal.join(".") === pattern) {
        this.handlers.delete(id);
      }
    }
  }

  // Emit a signal
  emit(signal: SignalPath, data: SlateValue = Null()): void {
    if (this.paused) {
      this.queuedEmissions.push({ signal, data });
      return;
    }

    // Record in history
    this.recordEvent(signal, data);

    const signalStr = signal.join(".");
    const handlersToRemove: number[] = [];

    for (const [id, handler] of this.handlers) {
      if (this.matchesPattern(signalStr, handler.pattern)) {
        // Check filter if present
        if (handler.filter && !this.matchesFilter(data, handler.filter)) {
          continue;
        }

        try {
          handler.handler(data, signal);
        } catch (error) {
          console.error(`Error in signal handler for ${signalStr}:`, error);
        }

        if (handler.once) {
          handlersToRemove.push(id);
        }
      }
    }

    // Remove one-time handlers
    for (const id of handlersToRemove) {
      this.handlers.delete(id);
    }
  }

  // Pause signal processing (queue emissions)
  pause(): void {
    this.paused = true;
  }

  // Resume signal processing and flush queue
  resume(): void {
    this.paused = false;
    const queued = [...this.queuedEmissions];
    this.queuedEmissions = [];

    for (const { signal, data } of queued) {
      this.emit(signal, data);
    }
  }

  // Check if paused
  isPaused(): boolean {
    return this.paused;
  }

  // Get signal history
  getHistory(limit?: number): SignalEvent[] {
    const count = limit ?? this.history.length;
    return this.history.slice(-count);
  }

  // Clear history
  clearHistory(): void {
    this.history = [];
  }

  // Set history limit
  setHistoryLimit(limit: number): void {
    this.historyLimit = limit;
    if (this.history.length > limit) {
      this.history = this.history.slice(-limit);
    }
  }

  // Get all registered handlers (for debugging)
  getHandlers(): SignalHandler[] {
    return Array.from(this.handlers.values());
  }

  // Get handler count
  getHandlerCount(): number {
    return this.handlers.size;
  }

  // Check if any handlers exist for a signal
  hasHandlers(signal: SignalPath): boolean {
    const signalStr = signal.join(".");
    for (const handler of this.handlers.values()) {
      if (this.matchesPattern(signalStr, handler.pattern)) {
        return true;
      }
    }
    return false;
  }

  // Create a pattern from a signal path (supporting wildcards)
  private createPattern(signal: SignalPath): string {
    return signal
      .map((part) => {
        if (part === "*") return "[^.]+";
        if (part === "**") return ".*";
        return part.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      })
      .join("\\.");
  }

  // Check if a signal matches a pattern
  private matchesPattern(signal: string, pattern: string): boolean {
    const regex = new RegExp(`^${pattern}$`);
    return regex.test(signal);
  }

  // Check if data matches a filter
  private matchesFilter(data: SlateValue, filter: SlateValue): boolean {
    return this.valuesEqual(data, filter);
  }

  // Deep equality check for Slate values
  private valuesEqual(a: SlateValue, b: SlateValue): boolean {
    if (a.type !== b.type) return false;

    switch (a.type) {
      case "number":
        return a.value === (b as any).value;
      case "string":
        return a.value === (b as any).value;
      case "bool":
        return a.value === (b as any).value;
      case "null":
        return true;
      case "list": {
        const bList = b as any;
        if (a.elements.length !== bList.elements.length) return false;
        return a.elements.every((e, i) =>
          this.valuesEqual(e, bList.elements[i])
        );
      }
      case "record": {
        const bRecord = b as any;
        if (a.fields.size !== bRecord.fields.size) return false;
        for (const [key, value] of a.fields) {
          const bValue = bRecord.fields.get(key);
          if (bValue === undefined || !this.valuesEqual(value, bValue)) {
            return false;
          }
        }
        return true;
      }
      default:
        return false;
    }
  }

  // Record an event in history
  private recordEvent(signal: SignalPath, data: SlateValue): void {
    this.history.push({
      signal,
      data,
      timestamp: Date.now(),
    });

    // Trim history if over limit
    if (this.history.length > this.historyLimit) {
      this.history = this.history.slice(-this.historyLimit);
    }
  }
}

// Built-in signal paths for common game events
export const GameSignals = {
  // Lifecycle signals
  TICK: ["game", "tick"],
  START: ["game", "start"],
  STOP: ["game", "stop"],
  PAUSE: ["game", "pause"],
  RESUME: ["game", "resume"],

  // Player signals
  PLAYER_SPAWN: ["player", "spawn"],
  PLAYER_DEATH: ["player", "death"],
  PLAYER_INPUT: ["player", "input"],
  PLAYER_MOVE: ["player", "move"],

  // Entity signals
  ENTITY_SPAWN: ["entity", "spawn"],
  ENTITY_DESTROY: ["entity", "destroy"],
  ENTITY_COLLISION: ["entity", "collision"],

  // VFS signals
  FILE_READ: ["vfs", "read"],
  FILE_WRITE: ["vfs", "write"],
  FILE_CHMOD: ["vfs", "chmod"],
  PERMISSION_DENIED: ["vfs", "permission", "denied"],

  // Puzzle signals
  PUZZLE_START: ["puzzle", "start"],
  PUZZLE_COMPLETE: ["puzzle", "complete"],
  PUZZLE_FAIL: ["puzzle", "fail"],

  // Console signals
  CONSOLE_INPUT: ["console", "input"],
  CONSOLE_OUTPUT: ["console", "output"],
};
