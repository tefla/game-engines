/**
 * Keybinding Manager - Handles keyboard shortcuts
 *
 * Supports:
 * - Standard modifiers: Cmd/Ctrl, Shift, Alt, Meta
 * - Key sequences: "Cmd+K Cmd+S"
 * - Context-aware bindings
 */

import { commandRegistry } from "./command-registry";
import { messageBus } from "./message-bus";

export interface Keybinding {
  key: string;
  commandId: string;
  when?: () => boolean;
}

interface ParsedKeybinding {
  ctrl: boolean;
  shift: boolean;
  alt: boolean;
  meta: boolean;
  key: string;
}

class KeybindingManager {
  private keybindings = new Map<string, Keybinding>();
  private pendingSequence: string | null = null;
  private sequenceTimeout: ReturnType<typeof setTimeout> | null = null;
  private enabled = true;

  constructor() {
    this.setupGlobalListener();
  }

  /**
   * Set up global keyboard event listener
   */
  private setupGlobalListener(): void {
    document.addEventListener("keydown", this.handleKeyDown.bind(this));
  }

  /**
   * Handle keydown events
   */
  private handleKeyDown(event: KeyboardEvent): void {
    if (!this.enabled) return;

    // Ignore if typing in an input (unless it's a global shortcut)
    const target = event.target as HTMLElement;
    const isInput = target.tagName === "INPUT" ||
                    target.tagName === "TEXTAREA" ||
                    target.isContentEditable;

    const keyString = this.eventToKeyString(event);

    // Handle key sequences (e.g., "Cmd+K Cmd+S")
    if (this.pendingSequence) {
      const fullSequence = `${this.pendingSequence} ${keyString}`;
      this.clearSequence();

      const binding = this.keybindings.get(this.normalizeKeybinding(fullSequence));
      if (binding) {
        event.preventDefault();
        this.executeBinding(binding);
        return;
      }
    }

    // Check for direct binding
    const binding = this.keybindings.get(this.normalizeKeybinding(keyString));
    if (binding) {
      // Check if this is the start of a sequence
      const possibleSequences = this.findSequencesStartingWith(keyString);
      if (possibleSequences.length > 0) {
        event.preventDefault();
        this.startSequence(keyString);
        return;
      }

      // Don't trigger shortcuts when typing in inputs (except for specific global ones)
      if (isInput && !this.isGlobalShortcut(keyString)) {
        return;
      }

      event.preventDefault();
      this.executeBinding(binding);
      return;
    }

    // Check if this could be the start of a sequence
    const possibleSequences = this.findSequencesStartingWith(keyString);
    if (possibleSequences.length > 0 && !isInput) {
      event.preventDefault();
      this.startSequence(keyString);
    }
  }

  /**
   * Convert keyboard event to string representation
   */
  private eventToKeyString(event: KeyboardEvent): string {
    const parts: string[] = [];

    if (event.metaKey) parts.push("Cmd");
    if (event.ctrlKey) parts.push("Ctrl");
    if (event.altKey) parts.push("Alt");
    if (event.shiftKey) parts.push("Shift");

    // Get the key name
    let key = event.key;

    // Normalize special keys
    const keyMap: Record<string, string> = {
      " ": "Space",
      "ArrowUp": "Up",
      "ArrowDown": "Down",
      "ArrowLeft": "Left",
      "ArrowRight": "Right",
      "Escape": "Escape",
      "Enter": "Enter",
      "Tab": "Tab",
      "Backspace": "Backspace",
      "Delete": "Delete",
    };

    key = keyMap[key] || key;

    // Don't add modifier keys as the main key
    if (!["Control", "Shift", "Alt", "Meta"].includes(key)) {
      // Capitalize single letters
      if (key.length === 1) {
        key = key.toUpperCase();
      }
      parts.push(key);
    }

    return parts.join("+");
  }

  /**
   * Normalize keybinding string for consistent comparison
   */
  private normalizeKeybinding(keybinding: string): string {
    return keybinding
      .split(" ")
      .map(part => {
        const parts = part.split("+").map(p => {
          const lower = p.toLowerCase();
          if (lower === "cmd" || lower === "command" || lower === "meta") return "cmd";
          if (lower === "ctrl" || lower === "control") return "ctrl";
          if (lower === "alt" || lower === "option") return "alt";
          if (lower === "shift") return "shift";
          return p.toUpperCase();
        });

        // Sort modifiers consistently
        const modifiers = parts.filter(p => ["cmd", "ctrl", "alt", "shift"].includes(p)).sort();
        const key = parts.find(p => !["cmd", "ctrl", "alt", "shift"].includes(p)) || "";

        return [...modifiers, key].join("+");
      })
      .join(" ");
  }

  /**
   * Register a keybinding
   */
  bind(key: string, commandId: string, when?: () => boolean): () => void {
    const normalized = this.normalizeKeybinding(key);
    const binding: Keybinding = { key: normalized, commandId, when };

    // Check for conflicts
    const existing = this.keybindings.get(normalized);
    if (existing) {
      console.warn(`Keybinding "${key}" already bound to "${existing.commandId}", rebinding to "${commandId}"`);
    }

    this.keybindings.set(normalized, binding);
    messageBus.emit("keybinding:bound", { key: normalized, commandId });

    return () => this.unbind(key);
  }

  /**
   * Remove a keybinding
   */
  unbind(key: string): boolean {
    const normalized = this.normalizeKeybinding(key);
    const existed = this.keybindings.delete(normalized);
    if (existed) {
      messageBus.emit("keybinding:unbound", { key: normalized });
    }
    return existed;
  }

  /**
   * Get all keybindings
   */
  getAll(): Keybinding[] {
    return Array.from(this.keybindings.values());
  }

  /**
   * Get keybinding for a command
   */
  getForCommand(commandId: string): string | undefined {
    for (const binding of this.keybindings.values()) {
      if (binding.commandId === commandId) {
        return binding.key;
      }
    }
    return undefined;
  }

  /**
   * Execute a keybinding
   */
  private async executeBinding(binding: Keybinding): Promise<void> {
    // Check context condition
    if (binding.when && !binding.when()) {
      return;
    }

    messageBus.emit("keybinding:triggered", { key: binding.key, commandId: binding.commandId });
    await commandRegistry.execute(binding.commandId);
  }

  /**
   * Start a key sequence
   */
  private startSequence(key: string): void {
    this.pendingSequence = key;
    messageBus.emit("keybinding:sequence-started", { key });

    // Clear sequence after timeout
    this.sequenceTimeout = setTimeout(() => {
      this.clearSequence();
    }, 2000);
  }

  /**
   * Clear pending key sequence
   */
  private clearSequence(): void {
    if (this.sequenceTimeout) {
      clearTimeout(this.sequenceTimeout);
      this.sequenceTimeout = null;
    }
    if (this.pendingSequence) {
      messageBus.emit("keybinding:sequence-cancelled", { key: this.pendingSequence });
      this.pendingSequence = null;
    }
  }

  /**
   * Find keybindings that start with a given key
   */
  private findSequencesStartingWith(key: string): Keybinding[] {
    const normalized = this.normalizeKeybinding(key);
    const results: Keybinding[] = [];

    for (const binding of this.keybindings.values()) {
      if (binding.key.startsWith(normalized + " ")) {
        results.push(binding);
      }
    }

    return results;
  }

  /**
   * Check if a keybinding is a global shortcut (works in inputs)
   */
  private isGlobalShortcut(key: string): boolean {
    const globalShortcuts = [
      "cmd+shift+p", // Command palette
      "cmd+s",       // Save
      "cmd+w",       // Close
      "cmd+n",       // New
      "cmd+o",       // Open
      "cmd+z",       // Undo
      "cmd+shift+z", // Redo
      "cmd+y",       // Redo (alternative)
    ];

    return globalShortcuts.includes(this.normalizeKeybinding(key).toLowerCase());
  }

  /**
   * Enable/disable keybinding handling
   */
  setEnabled(enabled: boolean): void {
    this.enabled = enabled;
    messageBus.emit("keybinding:enabled-changed", { enabled });
  }

  /**
   * Check if keybinding handling is enabled
   */
  isEnabled(): boolean {
    return this.enabled;
  }

  /**
   * Format keybinding for display
   */
  formatForDisplay(keybinding: string): string {
    const isMac = navigator.platform.toUpperCase().indexOf("MAC") >= 0;

    return keybinding
      .split(" ")
      .map(part => {
        return part
          .split("+")
          .map(key => {
            const lower = key.toLowerCase();
            if (lower === "cmd") return isMac ? "⌘" : "Ctrl";
            if (lower === "ctrl") return isMac ? "⌃" : "Ctrl";
            if (lower === "alt") return isMac ? "⌥" : "Alt";
            if (lower === "shift") return isMac ? "⇧" : "Shift";
            if (lower === "enter") return "↵";
            if (lower === "escape") return "Esc";
            if (lower === "space") return "Space";
            if (lower === "backspace") return "⌫";
            if (lower === "delete") return "Del";
            if (lower === "up") return "↑";
            if (lower === "down") return "↓";
            if (lower === "left") return "←";
            if (lower === "right") return "→";
            return key;
          })
          .join(isMac ? "" : "+");
      })
      .join(" ");
  }
}

// Singleton instance
export const keybindingManager = new KeybindingManager();
