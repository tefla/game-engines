import { app, BrowserWindow, ipcMain } from "electron";
import * as path from "path";
import { setupIpcHandlers } from "./ipc-handlers";
import { WindowManager } from "./window-manager";
import { fileWatcher } from "./file-watcher";
import { extensionLoader } from "./extension-loader";

// Handle creating/removing shortcuts on Windows when installing/uninstalling
try {
  if (require("electron-squirrel-startup")) {
    app.quit();
  }
} catch {
  // electron-squirrel-startup not available (dev mode or non-Windows)
}

const isDev = process.env.NODE_ENV === "development";
const isTest = process.env.NODE_ENV === "test";

// Hot reload main process in development
if (isDev) {
  try {
    require("electron-reload")(__dirname, {
      electron: path.join(__dirname, "../../node_modules/.bin/electron"),
      forceHardReset: true,
      hardResetMethod: "exit",
    });
  } catch (e) {
    console.log("electron-reload not available");
  }
}

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
    // Retry loading dev server with backoff
    const devUrl = "http://localhost:5173";
    let retries = 5;
    while (retries > 0) {
      try {
        await mainWindow.loadURL(devUrl);
        break;
      } catch (err) {
        retries--;
        if (retries === 0) {
          console.error("Failed to connect to dev server after retries:", err);
          // Fall back to built files if dev server isn't available
          await mainWindow.loadFile(path.join(__dirname, "../../renderer/index.html"));
          break;
        }
        console.log(`Dev server not ready, retrying... (${retries} left)`);
        await new Promise(resolve => setTimeout(resolve, 1000));
      }
    }
    mainWindow.webContents.openDevTools();
  } else {
    // Production or test mode - load built files
    await mainWindow.loadFile(path.join(__dirname, "../../renderer/index.html"));
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

  // Discover extensions
  await extensionLoader.discoverExtensions();

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
