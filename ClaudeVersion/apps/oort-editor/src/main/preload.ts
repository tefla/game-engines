import { contextBridge, ipcRenderer } from "electron";
import { IPC } from "../shared/ipc-channels";

// Expose protected methods to renderer
contextBridge.exposeInMainWorld("electronAPI", {
  // File operations
  readFile: (path: string) => ipcRenderer.invoke(IPC.FILE_READ, path),
  writeFile: (path: string, content: string) =>
    ipcRenderer.invoke(IPC.FILE_WRITE, path, content),
  deleteFile: (path: string) => ipcRenderer.invoke(IPC.FILE_DELETE, path),
  renameFile: (oldPath: string, newPath: string) =>
    ipcRenderer.invoke(IPC.FILE_RENAME, oldPath, newPath),
  listFiles: (path: string) => ipcRenderer.invoke(IPC.FILE_LIST, path),

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

  // Window operations
  minimize: () => ipcRenderer.send(IPC.WINDOW_MINIMIZE),
  maximize: () => ipcRenderer.send(IPC.WINDOW_MAXIMIZE),
  close: () => ipcRenderer.send(IPC.WINDOW_CLOSE),
  toggleDevTools: () => ipcRenderer.send(IPC.WINDOW_TOGGLE_DEVTOOLS),

  // App info
  getVersion: () => ipcRenderer.invoke(IPC.APP_GET_VERSION),
  getPath: (name: string) => ipcRenderer.invoke(IPC.APP_GET_PATH, name),

  // Event listeners
  on: (channel: string, callback: (...args: any[]) => void) => {
    const validChannels = [
      IPC.PROJECT_OPENED,
      IPC.FILE_EXTERNAL_CHANGE,
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

// Type declarations for renderer
export interface ElectronAPI {
  readFile: (path: string) => Promise<{ success: boolean; data?: string; error?: string }>;
  writeFile: (path: string, content: string) => Promise<{ success: boolean; error?: string }>;
  deleteFile: (path: string) => Promise<{ success: boolean; error?: string }>;
  renameFile: (oldPath: string, newPath: string) => Promise<{ success: boolean; error?: string }>;
  listFiles: (path: string) => Promise<{ success: boolean; data?: any[]; error?: string }>;
  openFileDialog: (options?: any) => Promise<Electron.OpenDialogReturnValue>;
  openFolderDialog: (options?: any) => Promise<Electron.OpenDialogReturnValue>;
  saveFileDialog: (options?: any) => Promise<Electron.SaveDialogReturnValue>;
  showMessage: (options: any) => Promise<Electron.MessageBoxReturnValue>;
  createProject: (path: string, name: string) => Promise<{ success: boolean; data?: any; error?: string }>;
  openProject: (path: string) => Promise<{ success: boolean; data?: any; error?: string }>;
  minimize: () => void;
  maximize: () => void;
  close: () => void;
  toggleDevTools: () => void;
  getVersion: () => Promise<string>;
  getPath: (name: string) => Promise<string>;
  on: (channel: string, callback: (...args: any[]) => void) => () => void;
  once: (channel: string, callback: (...args: any[]) => void) => void;
}

declare global {
  interface Window {
    electronAPI: ElectronAPI;
  }
}
