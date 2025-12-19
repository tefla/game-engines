import { contextBridge, ipcRenderer } from "electron";
import { IPC } from "../shared/ipc-channels";

// Expose protected methods to renderer
contextBridge.exposeInMainWorld("electronAPI", {
  // File operations
  readFile: (path: string) => ipcRenderer.invoke(IPC.FILE_READ, path),
  readBinaryFile: (path: string) => ipcRenderer.invoke(IPC.FILE_READ_BINARY, path),
  writeFile: (path: string, content: string) =>
    ipcRenderer.invoke(IPC.FILE_WRITE, path, content),
  createFile: (path: string, content?: string) =>
    ipcRenderer.invoke(IPC.FILE_CREATE, path, content || ""),
  deleteFile: (path: string) => ipcRenderer.invoke(IPC.FILE_DELETE, path),
  renameFile: (oldPath: string, newPath: string) =>
    ipcRenderer.invoke(IPC.FILE_RENAME, oldPath, newPath),
  listFiles: (path: string) => ipcRenderer.invoke(IPC.FILE_LIST, path),
  mkdir: (path: string) => ipcRenderer.invoke(IPC.FILE_MKDIR, path),
  fileExists: (path: string) => ipcRenderer.invoke(IPC.FILE_EXISTS, path),

  // Dialog operations
  openFileDialog: (options?: any) =>
    ipcRenderer.invoke(IPC.DIALOG_OPEN_FILE, options),
  openFolderDialog: (options?: any) =>
    ipcRenderer.invoke(IPC.DIALOG_OPEN_FOLDER, options),
  saveFileDialog: (options?: any) =>
    ipcRenderer.invoke(IPC.DIALOG_SAVE_FILE, options),
  showMessage: (options: any) =>
    ipcRenderer.invoke(IPC.DIALOG_MESSAGE, options),

  // Project operations
  createProject: (path: string, name: string) =>
    ipcRenderer.invoke(IPC.PROJECT_CREATE, path, name),
  openProject: (path: string) => ipcRenderer.invoke(IPC.PROJECT_OPEN, path),

  // Scene operations
  createScene: (path: string, name: string) =>
    ipcRenderer.invoke(IPC.SCENE_CREATE, path, name),
  openScene: (path: string) => ipcRenderer.invoke(IPC.SCENE_OPEN, path),
  saveScene: (path: string, sceneConfig: any) =>
    ipcRenderer.invoke(IPC.SCENE_SAVE, path, sceneConfig),
  listScenes: (projectPath: string) =>
    ipcRenderer.invoke(IPC.SCENE_LIST, projectPath),

  // Window operations
  minimize: () => ipcRenderer.send(IPC.WINDOW_MINIMIZE),
  maximize: () => ipcRenderer.send(IPC.WINDOW_MAXIMIZE),
  close: () => ipcRenderer.send(IPC.WINDOW_CLOSE),
  toggleDevTools: () => ipcRenderer.send(IPC.WINDOW_TOGGLE_DEVTOOLS),

  // App info
  getVersion: () => ipcRenderer.invoke(IPC.APP_GET_VERSION),
  getPath: (name: string) => ipcRenderer.invoke(IPC.APP_GET_PATH, name),

  // Extension operations
  getExtensions: () => ipcRenderer.invoke("extensions:getAll"),
  getExtension: (id: string) => ipcRenderer.invoke("extensions:get", id),
  activateExtension: (id: string) => ipcRenderer.invoke("extensions:activate", id),
  deactivateExtension: (id: string) => ipcRenderer.invoke("extensions:deactivate", id),
  reloadExtensions: () => ipcRenderer.invoke("extensions:reload"),

  // Recent projects
  getRecentProjects: () => ipcRenderer.invoke("settings:getRecentProjects"),
  addRecentProject: (path: string, name: string) =>
    ipcRenderer.invoke("settings:addRecentProject", path, name),
  removeRecentProject: (path: string) =>
    ipcRenderer.invoke("settings:removeRecentProject", path),
  clearRecentProjects: () => ipcRenderer.invoke("settings:clearRecentProjects"),

  // Event listeners
  on: (channel: string, callback: (...args: any[]) => void) => {
    const validChannels = [
      IPC.PROJECT_OPENED,
      IPC.FILE_EXTERNAL_CHANGE,
      "extension:activated",
      "extension:deactivated",
    ];
    if (validChannels.includes(channel as any)) {
      const subscription = (_event: any, ...args: any[]) => callback(...args);
      ipcRenderer.on(channel, subscription);
      return () => ipcRenderer.removeListener(channel, subscription);
    }
    return () => {};
  },

  // One-time event listener
  once: (channel: string, callback: (...args: any[]) => void) => {
    ipcRenderer.once(channel, (_event, ...args) => callback(...args));
  },
});

// Extension info type
export interface ExtensionInfo {
  id: string;
  name: string;
  version: string;
  description?: string;
  active: boolean;
  contributes?: {
    panels?: Array<{
      id: string;
      title: string;
      icon: string;
      defaultLocation?: "left" | "right" | "bottom" | "center";
      singleton?: boolean;
      category?: string;
    }>;
    commands?: Array<{
      id: string;
      title: string;
      category?: string;
      keybinding?: string;
      when?: string;
    }>;
    assetTypes?: Array<{
      extensions: string[];
      name: string;
      icon: string;
      editorId?: string;
    }>;
  };
}

// Type declarations for renderer
export interface ElectronAPI {
  readFile: (path: string) => Promise<{ success: boolean; data?: string; error?: string }>;
  readBinaryFile: (path: string) => Promise<{ success: boolean; data?: number[]; error?: string }>;
  writeFile: (path: string, content: string) => Promise<{ success: boolean; error?: string }>;
  createFile: (path: string, content?: string) => Promise<{ success: boolean; error?: string }>;
  deleteFile: (path: string) => Promise<{ success: boolean; error?: string }>;
  renameFile: (oldPath: string, newPath: string) => Promise<{ success: boolean; error?: string }>;
  listFiles: (path: string) => Promise<{ success: boolean; data?: any[]; error?: string }>;
  mkdir: (path: string) => Promise<{ success: boolean; error?: string }>;
  fileExists: (path: string) => Promise<{ success: boolean; data?: boolean; error?: string }>;
  openFileDialog: (options?: any) => Promise<Electron.OpenDialogReturnValue>;
  openFolderDialog: (options?: any) => Promise<Electron.OpenDialogReturnValue>;
  saveFileDialog: (options?: any) => Promise<Electron.SaveDialogReturnValue>;
  showMessage: (options: any) => Promise<Electron.MessageBoxReturnValue>;
  createProject: (path: string, name: string) => Promise<{ success: boolean; data?: any; error?: string }>;
  openProject: (path: string) => Promise<{ success: boolean; data?: any; error?: string }>;
  createScene: (path: string, name: string) => Promise<{ success: boolean; data?: any; error?: string }>;
  openScene: (path: string) => Promise<{ success: boolean; data?: any; error?: string }>;
  saveScene: (path: string, sceneConfig: any) => Promise<{ success: boolean; error?: string }>;
  listScenes: (projectPath: string) => Promise<{ success: boolean; data?: any[]; error?: string }>;
  minimize: () => void;
  maximize: () => void;
  close: () => void;
  toggleDevTools: () => void;
  getVersion: () => Promise<string>;
  getPath: (name: string) => Promise<string>;
  getExtensions: () => Promise<ExtensionInfo[]>;
  getExtension: (id: string) => Promise<ExtensionInfo | null>;
  activateExtension: (id: string) => Promise<boolean>;
  deactivateExtension: (id: string) => Promise<boolean>;
  reloadExtensions: () => Promise<ExtensionInfo[]>;
  getRecentProjects: () => Promise<RecentProject[]>;
  addRecentProject: (path: string, name: string) => Promise<RecentProject[]>;
  removeRecentProject: (path: string) => Promise<RecentProject[]>;
  clearRecentProjects: () => Promise<RecentProject[]>;
  on: (channel: string, callback: (...args: any[]) => void) => () => void;
  once: (channel: string, callback: (...args: any[]) => void) => void;
}

export interface RecentProject {
  path: string;
  name: string;
  lastOpened: string;
}

declare global {
  interface Window {
    electronAPI: ElectronAPI;
  }
}
