// Global type declarations for renderer process

interface ElectronAPI {
  readFile: (path: string) => Promise<{ success: boolean; data?: string; error?: string }>;
  writeFile: (path: string, content: string) => Promise<{ success: boolean; error?: string }>;
  createFile: (path: string, content?: string) => Promise<{ success: boolean; error?: string }>;
  deleteFile: (path: string) => Promise<{ success: boolean; error?: string }>;
  renameFile: (oldPath: string, newPath: string) => Promise<{ success: boolean; error?: string }>;
  listFiles: (path: string) => Promise<{ success: boolean; data?: any[]; error?: string }>;
  mkdir: (path: string) => Promise<{ success: boolean; error?: string }>;
  fileExists: (path: string) => Promise<{ success: boolean; data?: boolean; error?: string }>;
  openFileDialog: (options?: any) => Promise<{ canceled: boolean; filePaths: string[] }>;
  openFolderDialog: (options?: any) => Promise<{ canceled: boolean; filePaths: string[] }>;
  saveFileDialog: (options?: any) => Promise<{ canceled: boolean; filePath?: string }>;
  showMessage: (options: any) => Promise<{ response: number }>;
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
    electronAPI?: ElectronAPI;
  }
}

export {};
