import { ipcMain, dialog, app, BrowserWindow } from "electron";
import * as fs from "fs/promises";
import * as path from "path";
import { IPC } from "../shared/ipc-channels";
import { fileWatcher } from "./file-watcher";

export function setupIpcHandlers(): void {
  // File operations
  ipcMain.handle(IPC.FILE_READ, async (_event, filePath: string) => {
    try {
      const content = await fs.readFile(filePath, "utf-8");
      return { success: true, data: content };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  // Binary file reading (for glTF/GLB models, images, etc.)
  ipcMain.handle(IPC.FILE_READ_BINARY, async (_event, filePath: string) => {
    try {
      const buffer = await fs.readFile(filePath);
      // Return as Uint8Array for transfer
      return { success: true, data: Array.from(buffer) };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(
    IPC.FILE_WRITE,
    async (_event, filePath: string, content: string) => {
      try {
        await fs.writeFile(filePath, content, "utf-8");
        return { success: true };
      } catch (error: any) {
        return { success: false, error: error.message };
      }
    }
  );

  ipcMain.handle(IPC.FILE_DELETE, async (_event, filePath: string) => {
    try {
      await fs.unlink(filePath);
      return { success: true };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(
    IPC.FILE_RENAME,
    async (_event, oldPath: string, newPath: string) => {
      try {
        await fs.rename(oldPath, newPath);
        return { success: true };
      } catch (error: any) {
        return { success: false, error: error.message };
      }
    }
  );

  ipcMain.handle(IPC.FILE_LIST, async (_event, dirPath: string) => {
    try {
      const entries = await fs.readdir(dirPath, { withFileTypes: true });
      const files = entries.map((entry) => ({
        name: entry.name,
        path: path.join(dirPath, entry.name),
        type: entry.isDirectory() ? "directory" : "file",
      }));
      return { success: true, data: files };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.FILE_CREATE, async (_event, filePath: string, content: string = "") => {
    try {
      await fs.writeFile(filePath, content, "utf-8");
      return { success: true };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.FILE_MKDIR, async (_event, dirPath: string) => {
    try {
      await fs.mkdir(dirPath, { recursive: true });
      return { success: true };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.FILE_EXISTS, async (_event, filePath: string) => {
    try {
      await fs.access(filePath);
      return { success: true, data: true };
    } catch {
      return { success: true, data: false };
    }
  });

  // Dialog operations
  ipcMain.handle(IPC.DIALOG_OPEN_FILE, async (_event, options?: any) => {
    const window = BrowserWindow.getFocusedWindow();
    const result = await dialog.showOpenDialog(window!, {
      properties: ["openFile"],
      filters: options?.filters || [
        { name: "Slate Files", extensions: ["sl"] },
        { name: "All Files", extensions: ["*"] },
      ],
      ...options,
    });
    return result;
  });

  ipcMain.handle(IPC.DIALOG_OPEN_FOLDER, async (_event, options?: any) => {
    const window = BrowserWindow.getFocusedWindow();
    const result = await dialog.showOpenDialog(window!, {
      properties: ["openDirectory", "createDirectory"],
      ...options,
    });
    return result;
  });

  ipcMain.handle(IPC.DIALOG_SAVE_FILE, async (_event, options?: any) => {
    const window = BrowserWindow.getFocusedWindow();
    const result = await dialog.showSaveDialog(window!, {
      filters: options?.filters || [
        { name: "Slate Files", extensions: ["sl"] },
        { name: "All Files", extensions: ["*"] },
      ],
      ...options,
    });
    return result;
  });

  ipcMain.handle(
    IPC.DIALOG_MESSAGE,
    async (_event, options: Electron.MessageBoxOptions) => {
      const window = BrowserWindow.getFocusedWindow();
      const result = await dialog.showMessageBox(window!, options);
      return result;
    }
  );

  // Project operations
  ipcMain.handle(IPC.PROJECT_CREATE, async (_event, projectPath: string, name: string) => {
    try {
      // Create project structure
      await fs.mkdir(projectPath, { recursive: true });
      await fs.mkdir(path.join(projectPath, "scripts"));
      await fs.mkdir(path.join(projectPath, "assets"));
      await fs.mkdir(path.join(projectPath, "scenes"));

      // Create default scene
      const defaultScene = {
        name: "Main Scene",
        version: "1.0",
        entities: [],
        settings: {
          backgroundColor: "#1a1a2e",
          ambientLight: { color: "#ffffff", intensity: 0.4 },
          directionalLight: {
            color: "#ffffff",
            intensity: 0.8,
            position: [5, 10, 5],
          },
        },
      };
      await fs.writeFile(
        path.join(projectPath, "scenes", "main.oort-scene"),
        JSON.stringify(defaultScene, null, 2)
      );

      // Create project.json
      const config = {
        name,
        version: "1.0.0",
        entry: "/scripts/main.sl",
        assets: ["/assets"],
        mainScene: "/scenes/main.oort-scene",
        settings: {
          physics: false,
        },
      };
      await fs.writeFile(
        path.join(projectPath, "project.json"),
        JSON.stringify(config, null, 2)
      );

      // Create main.sl
      const mainContent = `# ${name} - Main Script
# This is the entry point for your game

fn main:
    say "Hello from ${name}!"

main()
`;
      await fs.writeFile(
        path.join(projectPath, "scripts", "main.sl"),
        mainContent
      );

      // Start file watcher for new project
      const mainWindow = BrowserWindow.getFocusedWindow();
      if (mainWindow) {
        fileWatcher.setMainWindow(mainWindow);
        fileWatcher.start(projectPath);
      }

      return { success: true, data: { path: projectPath, config } };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.PROJECT_OPEN, async (_event, projectPath: string) => {
    try {
      const configPath = path.join(projectPath, "project.json");
      const configContent = await fs.readFile(configPath, "utf-8");
      const config = JSON.parse(configContent);

      // Start file watcher for opened project
      const mainWindow = BrowserWindow.getFocusedWindow();
      if (mainWindow) {
        fileWatcher.setMainWindow(mainWindow);
        fileWatcher.start(projectPath);
      }

      return { success: true, data: { path: projectPath, config } };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.PROJECT_CLOSE, async () => {
    fileWatcher.stop();
    return { success: true };
  });

  // Scene operations
  ipcMain.handle(IPC.SCENE_CREATE, async (_event, scenePath: string, name: string) => {
    try {
      const sceneConfig = {
        name,
        version: "1.0",
        entities: [],
        settings: {
          backgroundColor: "#1a1a2e",
          ambientLight: { color: "#ffffff", intensity: 0.4 },
          directionalLight: {
            color: "#ffffff",
            intensity: 0.8,
            position: [5, 10, 5],
          },
        },
      };
      await fs.writeFile(scenePath, JSON.stringify(sceneConfig, null, 2));
      return { success: true, data: sceneConfig };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.SCENE_OPEN, async (_event, scenePath: string) => {
    try {
      const content = await fs.readFile(scenePath, "utf-8");
      const sceneConfig = JSON.parse(content);
      return { success: true, data: sceneConfig };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.SCENE_SAVE, async (_event, scenePath: string, sceneConfig: any) => {
    try {
      await fs.writeFile(scenePath, JSON.stringify(sceneConfig, null, 2));
      return { success: true };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  ipcMain.handle(IPC.SCENE_LIST, async (_event, projectPath: string) => {
    try {
      const scenesDir = path.join(projectPath, "scenes");
      const entries = await fs.readdir(scenesDir, { withFileTypes: true });
      const scenes = entries
        .filter(entry => entry.isFile() && entry.name.endsWith(".oort-scene"))
        .map(entry => ({
          name: entry.name.replace(".oort-scene", ""),
          path: path.join(scenesDir, entry.name),
        }));
      return { success: true, data: scenes };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
  });

  // File watch control
  ipcMain.handle(IPC.FILE_WATCH_START, async (_event, projectPath: string) => {
    const mainWindow = BrowserWindow.getFocusedWindow();
    if (mainWindow) {
      fileWatcher.setMainWindow(mainWindow);
      fileWatcher.start(projectPath);
    }
    return { success: true };
  });

  ipcMain.handle(IPC.FILE_WATCH_STOP, async () => {
    fileWatcher.stop();
    return { success: true };
  });

  // Window operations
  ipcMain.on(IPC.WINDOW_MINIMIZE, () => {
    BrowserWindow.getFocusedWindow()?.minimize();
  });

  ipcMain.on(IPC.WINDOW_MAXIMIZE, () => {
    const window = BrowserWindow.getFocusedWindow();
    if (window?.isMaximized()) {
      window.unmaximize();
    } else {
      window?.maximize();
    }
  });

  ipcMain.on(IPC.WINDOW_CLOSE, () => {
    BrowserWindow.getFocusedWindow()?.close();
  });

  ipcMain.on(IPC.WINDOW_TOGGLE_DEVTOOLS, () => {
    BrowserWindow.getFocusedWindow()?.webContents.toggleDevTools();
  });

  // App info
  ipcMain.handle(IPC.APP_GET_VERSION, () => {
    return app.getVersion();
  });

  ipcMain.handle(IPC.APP_GET_PATH, (_event, name: string) => {
    return app.getPath(name as any);
  });

  // Recent projects
  const getSettingsPath = () => {
    const userDataPath = app.getPath("userData");
    return path.join(userDataPath, "settings.json");
  };

  const loadSettings = async () => {
    try {
      const settingsPath = getSettingsPath();
      const content = await fs.readFile(settingsPath, "utf-8");
      return JSON.parse(content);
    } catch {
      return { recentProjects: [] };
    }
  };

  const saveSettings = async (settings: any) => {
    const settingsPath = getSettingsPath();
    await fs.writeFile(settingsPath, JSON.stringify(settings, null, 2));
  };

  ipcMain.handle("settings:getRecentProjects", async () => {
    const settings = await loadSettings();
    return settings.recentProjects || [];
  });

  ipcMain.handle("settings:addRecentProject", async (_event, projectPath: string, projectName: string) => {
    const settings = await loadSettings();
    const recentProjects = settings.recentProjects || [];

    // Remove if already exists
    const filtered = recentProjects.filter((p: any) => p.path !== projectPath);

    // Add to front
    filtered.unshift({
      path: projectPath,
      name: projectName,
      lastOpened: new Date().toISOString(),
    });

    // Keep only last 10
    settings.recentProjects = filtered.slice(0, 10);
    await saveSettings(settings);

    return settings.recentProjects;
  });

  ipcMain.handle("settings:removeRecentProject", async (_event, projectPath: string) => {
    const settings = await loadSettings();
    settings.recentProjects = (settings.recentProjects || []).filter(
      (p: any) => p.path !== projectPath
    );
    await saveSettings(settings);
    return settings.recentProjects;
  });

  ipcMain.handle("settings:clearRecentProjects", async () => {
    const settings = await loadSettings();
    settings.recentProjects = [];
    await saveSettings(settings);
    return [];
  });
}
