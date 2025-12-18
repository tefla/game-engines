import { app, BrowserWindow, ipcMain } from "electron";
import * as path from "path";
import { setupIpcHandlers } from "./ipc-handlers";
import { WindowManager } from "./window-manager";

// Handle creating/removing shortcuts on Windows when installing/uninstalling
if (require("electron-squirrel-startup")) {
  app.quit();
}

const isDev = process.env.NODE_ENV === "development" || !app.isPackaged;

let windowManager: WindowManager;

async function createMainWindow(): Promise<BrowserWindow> {
  const mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    minWidth: 800,
    minHeight: 600,
    backgroundColor: "#1e1e2e",
    titleBarStyle: process.platform === "darwin" ? "hiddenInset" : "default",
    trafficLightPosition: { x: 16, y: 16 },
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false,
    },
    show: false, // Show when ready
  });

  // Load the app
  if (isDev) {
    await mainWindow.loadURL("http://localhost:5173");
    mainWindow.webContents.openDevTools();
  } else {
    await mainWindow.loadFile(path.join(__dirname, "../renderer/index.html"));
  }

  // Show window when ready
  mainWindow.once("ready-to-show", () => {
    mainWindow.show();
  });

  return mainWindow;
}

app.whenReady().then(async () => {
  // Set up IPC handlers
  setupIpcHandlers();

  // Create window manager
  windowManager = new WindowManager();

  // Create main window
  const mainWindow = await createMainWindow();
  windowManager.setMainWindow(mainWindow);

  // macOS: Re-create window when dock icon clicked
  app.on("activate", async () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      const window = await createMainWindow();
      windowManager.setMainWindow(window);
    }
  });
});

// Quit when all windows are closed (except macOS)
app.on("window-all-closed", () => {
  if (process.platform !== "darwin") {
    app.quit();
  }
});

// Export for testing
export { windowManager };
