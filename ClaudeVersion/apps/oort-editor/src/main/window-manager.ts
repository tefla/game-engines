import { BrowserWindow, screen } from "electron";

export interface WindowState {
  width: number;
  height: number;
  x?: number;
  y?: number;
  isMaximized: boolean;
}

export class WindowManager {
  private mainWindow: BrowserWindow | null = null;
  private floatingPanels: Map<string, BrowserWindow> = new Map();

  setMainWindow(window: BrowserWindow): void {
    this.mainWindow = window;

    window.on("closed", () => {
      this.mainWindow = null;
    });
  }

  getMainWindow(): BrowserWindow | null {
    return this.mainWindow;
  }

  // Send message to renderer
  send(channel: string, data?: unknown): void {
    if (this.mainWindow && !this.mainWindow.isDestroyed()) {
      this.mainWindow.webContents.send(channel, data);
    }
  }

  // Create a floating panel window
  createFloatingPanel(
    panelId: string,
    options: { title: string; width?: number; height?: number }
  ): BrowserWindow {
    const { width = 400, height = 300, title } = options;

    // Position near main window
    let x: number | undefined;
    let y: number | undefined;

    if (this.mainWindow) {
      const [mainX, mainY] = this.mainWindow.getPosition();
      const [mainW] = this.mainWindow.getSize();
      x = mainX + mainW + 20;
      y = mainY;
    }

    const panel = new BrowserWindow({
      width,
      height,
      x,
      y,
      title,
      parent: this.mainWindow || undefined,
      backgroundColor: "#1e1e2e",
      webPreferences: {
        contextIsolation: true,
        nodeIntegration: false,
      },
    });

    this.floatingPanels.set(panelId, panel);

    panel.on("closed", () => {
      this.floatingPanels.delete(panelId);
    });

    return panel;
  }

  closeFloatingPanel(panelId: string): void {
    const panel = this.floatingPanels.get(panelId);
    if (panel && !panel.isDestroyed()) {
      panel.close();
    }
  }

  // Get window state for persistence
  getWindowState(): WindowState | null {
    if (!this.mainWindow) return null;

    const [width, height] = this.mainWindow.getSize();
    const [x, y] = this.mainWindow.getPosition();
    const isMaximized = this.mainWindow.isMaximized();

    return { width, height, x, y, isMaximized };
  }

  // Restore window state
  restoreWindowState(state: WindowState): void {
    if (!this.mainWindow) return;

    // Validate position is on screen
    const displays = screen.getAllDisplays();
    const onScreen = displays.some((display) => {
      const { x, y, width, height } = display.bounds;
      return (
        state.x !== undefined &&
        state.y !== undefined &&
        state.x >= x &&
        state.x < x + width &&
        state.y >= y &&
        state.y < y + height
      );
    });

    if (onScreen && state.x !== undefined && state.y !== undefined) {
      this.mainWindow.setPosition(state.x, state.y);
    }

    this.mainWindow.setSize(state.width, state.height);

    if (state.isMaximized) {
      this.mainWindow.maximize();
    }
  }

  // Minimize main window
  minimize(): void {
    this.mainWindow?.minimize();
  }

  // Maximize/restore main window
  toggleMaximize(): void {
    if (!this.mainWindow) return;

    if (this.mainWindow.isMaximized()) {
      this.mainWindow.unmaximize();
    } else {
      this.mainWindow.maximize();
    }
  }

  // Close main window
  close(): void {
    this.mainWindow?.close();
  }

  // Toggle dev tools
  toggleDevTools(): void {
    this.mainWindow?.webContents.toggleDevTools();
  }
}
