// IPC channel names for communication between main and renderer

export const IPC = {
  // Project operations
  PROJECT_CREATE: "project:create",
  PROJECT_OPEN: "project:open",
  PROJECT_SAVE: "project:save",
  PROJECT_CLOSE: "project:close",
  PROJECT_OPENED: "project:opened",

  // Scene operations
  SCENE_CREATE: "scene:create",
  SCENE_OPEN: "scene:open",
  SCENE_SAVE: "scene:save",
  SCENE_LIST: "scene:list",

  // File operations
  FILE_READ: "file:read",
  FILE_READ_BINARY: "file:read-binary",
  FILE_WRITE: "file:write",
  FILE_CREATE: "file:create",
  FILE_DELETE: "file:delete",
  FILE_RENAME: "file:rename",
  FILE_LIST: "file:list",
  FILE_MKDIR: "file:mkdir",
  FILE_EXISTS: "file:exists",
  FILE_EXTERNAL_CHANGE: "file:external-change",
  FILE_WATCH_START: "file:watch-start",
  FILE_WATCH_STOP: "file:watch-stop",

  // Dialog operations
  DIALOG_OPEN_FILE: "dialog:open-file",
  DIALOG_OPEN_FOLDER: "dialog:open-folder",
  DIALOG_SAVE_FILE: "dialog:save-file",
  DIALOG_MESSAGE: "dialog:message",

  // Window operations
  WINDOW_MINIMIZE: "window:minimize",
  WINDOW_MAXIMIZE: "window:maximize",
  WINDOW_CLOSE: "window:close",
  WINDOW_TOGGLE_DEVTOOLS: "window:toggle-devtools",

  // App info
  APP_GET_VERSION: "app:get-version",
  APP_GET_PATH: "app:get-path",
} as const;

export type IPCChannel = (typeof IPC)[keyof typeof IPC];
