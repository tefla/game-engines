/**
 * Game Stats Extension - Entry Point
 *
 * This extension provides game statistics and performance monitoring.
 */

export interface ExtensionContext {
  extensionPath: string;
  subscriptions: { dispose: () => void }[];
}

export interface GameStats {
  fps: number;
  frameTime: number;
  entityCount: number;
  drawCalls: number;
  memoryUsage: number;
  updateTime: number;
  renderTime: number;
}

// Stats tracking
let stats: GameStats = {
  fps: 0,
  frameTime: 0,
  entityCount: 0,
  drawCalls: 0,
  memoryUsage: 0,
  updateTime: 0,
  renderTime: 0,
};

let isActive = false;

/**
 * Called when the extension is activated
 */
export function activate(context: ExtensionContext): void {
  console.log("[GameStats] Extension activated");
  isActive = true;

  // Register command handlers
  // In a full implementation, this would use the editor API
  // For now, we export functions that the editor can call
}

/**
 * Called when the extension is deactivated
 */
export function deactivate(): void {
  console.log("[GameStats] Extension deactivated");
  isActive = false;
}

/**
 * Update stats (called by the game engine)
 */
export function updateStats(newStats: Partial<GameStats>): void {
  stats = { ...stats, ...newStats };
}

/**
 * Get current stats
 */
export function getStats(): GameStats {
  return { ...stats };
}

/**
 * Reset all statistics
 */
export function resetStats(): void {
  stats = {
    fps: 0,
    frameTime: 0,
    entityCount: 0,
    drawCalls: 0,
    memoryUsage: 0,
    updateTime: 0,
    renderTime: 0,
  };
  console.log("[GameStats] Statistics reset");
}

/**
 * Check if extension is active
 */
export function isExtensionActive(): boolean {
  return isActive;
}
