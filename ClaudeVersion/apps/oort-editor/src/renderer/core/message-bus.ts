// Message bus for communication between panels and components
// Built on a simple pub/sub pattern, extensible via the same patterns as SignalBus

type MessageHandler = (data: any) => void;

interface Subscription {
  channel: string;
  handler: MessageHandler;
}

class MessageBus {
  private handlers: Map<string, Set<MessageHandler>> = new Map();
  private wildcardHandlers: Map<string, Set<MessageHandler>> = new Map();
  private history: Array<{ channel: string; data: any; timestamp: number }> = [];
  private maxHistory = 100;

  /**
   * Subscribe to a channel
   * Supports wildcards: "file:*" matches "file:open", "file:save", etc.
   */
  on(channel: string, handler: MessageHandler): () => void {
    if (channel.includes("*")) {
      // Wildcard subscription
      const pattern = channel.replace("*", "");
      if (!this.wildcardHandlers.has(pattern)) {
        this.wildcardHandlers.set(pattern, new Set());
      }
      this.wildcardHandlers.get(pattern)!.add(handler);

      return () => {
        this.wildcardHandlers.get(pattern)?.delete(handler);
      };
    } else {
      // Exact subscription
      if (!this.handlers.has(channel)) {
        this.handlers.set(channel, new Set());
      }
      this.handlers.get(channel)!.add(handler);

      return () => {
        this.handlers.get(channel)?.delete(handler);
      };
    }
  }

  /**
   * Subscribe to a channel once
   */
  once(channel: string, handler: MessageHandler): () => void {
    const wrapper: MessageHandler = (data) => {
      unsubscribe();
      handler(data);
    };
    const unsubscribe = this.on(channel, wrapper);
    return unsubscribe;
  }

  /**
   * Emit a message to a channel
   */
  emit(channel: string, data?: any): void {
    // Record in history
    this.history.push({ channel, data, timestamp: Date.now() });
    if (this.history.length > this.maxHistory) {
      this.history.shift();
    }

    // Notify exact subscribers
    const handlers = this.handlers.get(channel);
    if (handlers) {
      handlers.forEach((handler) => {
        try {
          handler(data);
        } catch (error) {
          console.error(`Error in message handler for ${channel}:`, error);
        }
      });
    }

    // Notify wildcard subscribers
    this.wildcardHandlers.forEach((wildcardHandlers, pattern) => {
      if (channel.startsWith(pattern)) {
        wildcardHandlers.forEach((handler) => {
          try {
            handler(data);
          } catch (error) {
            console.error(`Error in wildcard handler for ${pattern}*:`, error);
          }
        });
      }
    });
  }

  /**
   * Request-response pattern
   * Emits a request and waits for a response on a derived channel
   */
  request<T>(channel: string, data?: any, timeout = 5000): Promise<T> {
    return new Promise((resolve, reject) => {
      const responseChannel = `${channel}:response:${Date.now()}`;

      const timer = setTimeout(() => {
        unsubscribe();
        reject(new Error(`Request timeout: ${channel}`));
      }, timeout);

      const unsubscribe = this.once(responseChannel, (response) => {
        clearTimeout(timer);
        resolve(response);
      });

      this.emit(channel, { ...data, responseChannel });
    });
  }

  /**
   * Get message history for debugging
   */
  getHistory(): Array<{ channel: string; data: any; timestamp: number }> {
    return [...this.history];
  }

  /**
   * Clear all subscriptions
   */
  clear(): void {
    this.handlers.clear();
    this.wildcardHandlers.clear();
  }
}

// Singleton instance
export const messageBus = new MessageBus();

// Common channel types
export const Channels = {
  // File operations
  FILE_OPEN: "file:open",
  FILE_SAVE: "file:save",
  FILE_CLOSE: "file:close",
  FILE_CHANGED: "file:changed",
  FILE_SELECTED: "file:selected",

  // Editor operations
  EDITOR_CONTENT_CHANGED: "editor:content-changed",
  EDITOR_CURSOR_CHANGED: "editor:cursor-changed",
  EDITOR_SELECTION_CHANGED: "editor:selection-changed",

  // Panel operations
  PANEL_OPEN: "panel:open",
  PANEL_CLOSE: "panel:close",
  PANEL_FOCUS: "panel:focus",
  PANEL_REGISTERED: "panel:registered",

  // Selection
  SELECTION_CHANGED: "selection:changed",

  // Console
  CONSOLE_LOG: "console:log",
  CONSOLE_ERROR: "console:error",
  CONSOLE_WARN: "console:warn",
  CONSOLE_CLEAR: "console:clear",

  // VCS
  VCS_COMMITTED: "vcs:committed",
  VCS_UNDONE: "vcs:undone",
  VCS_REDONE: "vcs:redone",

  // Project
  PROJECT_OPENED: "project:opened",
  PROJECT_CLOSED: "project:closed",

  // Theme
  THEME_CHANGED: "theme:changed",

  // Commands
  COMMAND_EXECUTE: "command:execute",
  COMMAND_PALETTE_OPEN: "command-palette:open",
} as const;

export type Channel = (typeof Channels)[keyof typeof Channels];
