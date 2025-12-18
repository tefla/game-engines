import { ipcMain, dialog, app, BrowserWindow } from "electron";
import * as fs from "fs/promises";
import * as path from "path";
import { IPC } from "../shared/ipc-channels";

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

      // Create project.json
      const config = {
        name,
        version: "1.0.0",
        entry: "/scripts/main.sl",
        assets: ["/assets"],
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
      return { success: true, data: { path: projectPath, config } };
    } catch (error: any) {
      return { success: false, error: error.message };
    }
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
}
