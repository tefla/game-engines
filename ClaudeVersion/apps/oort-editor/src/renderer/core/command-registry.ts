/**
 * Command Registry - Central registry for all editor commands
 *
 * Commands are actions that can be:
 * - Executed via keyboard shortcuts
 * - Triggered from command palette
 * - Called from menus
 * - Invoked programmatically
 */

import { messageBus } from "./message-bus";

export interface Command {
  id: string;
  title: string;
  category?: string;
  description?: string;
  keybinding?: string;
  icon?: string;
  when?: () => boolean; // Context condition for when command is available
  handler: () => void | Promise<void>;
}

export interface CommandGroup {
  category: string;
  commands: Command[];
}

class CommandRegistry {
  private commands = new Map<string, Command>();
  private recentCommands: string[] = [];
  private maxRecent = 10;

  /**
   * Register a command
   * Note: Silently skips if already registered (expected in React StrictMode)
   */
  register(command: Command): () => void {
    // Skip if already registered (React StrictMode re-runs effects)
    if (this.commands.has(command.id)) {
      return () => this.unregister(command.id);
    }

    this.commands.set(command.id, command);
    messageBus.emit("command:registered", { command });

    // Return unregister function
    return () => this.unregister(command.id);
  }

  /**
   * Register multiple commands at once
   */
  registerMany(commands: Command[]): () => void {
    const unregisters = commands.map(cmd => this.register(cmd));
    return () => unregisters.forEach(fn => fn());
  }

  /**
   * Unregister a command
   */
  unregister(id: string): boolean {
    const existed = this.commands.delete(id);
    if (existed) {
      messageBus.emit("command:unregistered", { id });
    }
    return existed;
  }

  /**
   * Get a command by ID
   */
  get(id: string): Command | undefined {
    return this.commands.get(id);
  }

  /**
   * Get all registered commands
   */
  getAll(): Command[] {
    return Array.from(this.commands.values());
  }

  /**
   * Get commands grouped by category
   */
  getGrouped(): CommandGroup[] {
    const groups = new Map<string, Command[]>();

    for (const command of this.commands.values()) {
      const category = command.category || "General";
      if (!groups.has(category)) {
        groups.set(category, []);
      }
      groups.get(category)!.push(command);
    }

    return Array.from(groups.entries())
      .map(([category, commands]) => ({
        category,
        commands: commands.sort((a, b) => a.title.localeCompare(b.title)),
      }))
      .sort((a, b) => a.category.localeCompare(b.category));
  }

  /**
   * Execute a command by ID
   */
  async execute(id: string): Promise<boolean> {
    const command = this.commands.get(id);

    if (!command) {
      console.warn(`Command "${id}" not found`);
      return false;
    }

    // Check if command is available
    if (command.when && !command.when()) {
      console.warn(`Command "${id}" is not available in current context`);
      return false;
    }

    try {
      messageBus.emit("command:executing", { id, command });
      await command.handler();
      messageBus.emit("command:executed", { id, command });

      // Track recent commands
      this.trackRecent(id);

      return true;
    } catch (error) {
      console.error(`Error executing command "${id}":`, error);
      messageBus.emit("command:error", { id, command, error });
      return false;
    }
  }

  /**
   * Search commands by query (fuzzy matching)
   */
  search(query: string): Command[] {
    if (!query.trim()) {
      // Return recent commands first, then all
      const recent = this.recentCommands
        .map(id => this.commands.get(id))
        .filter((cmd): cmd is Command => cmd !== undefined);

      const others = this.getAll()
        .filter(cmd => !this.recentCommands.includes(cmd.id))
        .filter(cmd => !cmd.when || cmd.when());

      return [...recent, ...others];
    }

    const lowerQuery = query.toLowerCase();
    const results: Array<{ command: Command; score: number }> = [];

    for (const command of this.commands.values()) {
      // Skip unavailable commands
      if (command.when && !command.when()) continue;

      const score = this.matchScore(command, lowerQuery);
      if (score > 0) {
        results.push({ command, score });
      }
    }

    return results
      .sort((a, b) => b.score - a.score)
      .map(r => r.command);
  }

  /**
   * Calculate match score for fuzzy search
   */
  private matchScore(command: Command, query: string): number {
    const title = command.title.toLowerCase();
    const category = (command.category || "").toLowerCase();
    const id = command.id.toLowerCase();

    let score = 0;

    // Exact match in title
    if (title === query) return 100;

    // Starts with query
    if (title.startsWith(query)) score += 50;

    // Contains query
    if (title.includes(query)) score += 30;

    // Category match
    if (category.includes(query)) score += 20;

    // ID match
    if (id.includes(query)) score += 10;

    // Fuzzy match (all characters present in order)
    if (this.fuzzyMatch(title, query)) score += 15;

    // Boost recent commands
    const recentIndex = this.recentCommands.indexOf(command.id);
    if (recentIndex !== -1) {
      score += (this.maxRecent - recentIndex) * 2;
    }

    return score;
  }

  /**
   * Check if all characters in query appear in text in order
   */
  private fuzzyMatch(text: string, query: string): boolean {
    let queryIndex = 0;
    for (const char of text) {
      if (char === query[queryIndex]) {
        queryIndex++;
        if (queryIndex === query.length) return true;
      }
    }
    return false;
  }

  /**
   * Track recently used commands
   */
  private trackRecent(id: string): void {
    // Remove if already in list
    const index = this.recentCommands.indexOf(id);
    if (index !== -1) {
      this.recentCommands.splice(index, 1);
    }

    // Add to front
    this.recentCommands.unshift(id);

    // Trim to max
    if (this.recentCommands.length > this.maxRecent) {
      this.recentCommands.pop();
    }
  }

  /**
   * Get commands with a specific keybinding
   */
  getByKeybinding(keybinding: string): Command | undefined {
    const normalized = this.normalizeKeybinding(keybinding);
    for (const command of this.commands.values()) {
      if (command.keybinding && this.normalizeKeybinding(command.keybinding) === normalized) {
        return command;
      }
    }
    return undefined;
  }

  /**
   * Normalize keybinding string for comparison
   */
  private normalizeKeybinding(keybinding: string): string {
    return keybinding
      .toLowerCase()
      .replace(/cmd/g, "meta")
      .replace(/command/g, "meta")
      .replace(/ctrl/g, "control")
      .replace(/\s+/g, "")
      .split("+")
      .sort()
      .join("+");
  }
}

// Singleton instance
export const commandRegistry = new CommandRegistry();
