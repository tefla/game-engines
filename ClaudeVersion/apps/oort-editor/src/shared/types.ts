// Shared types between main and renderer processes

// Panel system types
export interface PanelDefinition {
  id: string;
  title: string;
  icon?: string;
  defaultLocation?: PanelLocation;
  singleton?: boolean;
}

export type PanelLocation = "left" | "right" | "bottom" | "center";

// Layout types
export interface LayoutNode {
  type: "row" | "column" | "tabs" | "panel";
  id: string;
  children?: LayoutNode[];
  panelId?: string;
  activeTab?: number;
  size?: number; // Flex ratio
}

// Project types
export interface ProjectConfig {
  name: string;
  version: string;
  entry: string;
  assets: string[];
}

export interface FileNode {
  name: string;
  path: string;
  type: "file" | "directory";
  children?: FileNode[];
}

// Editor state
export interface EditorState {
  project: {
    path: string | null;
    name: string;
    config: ProjectConfig | null;
  };
  files: {
    current: string | null;
    open: string[];
    modified: Set<string>;
  };
  ui: {
    theme: "dark" | "light";
    sidebarWidth: number;
    bottomPanelHeight: number;
    activePanel: string | null;
  };
}

// IPC message types
export interface IPCMessage<T = unknown> {
  channel: string;
  data: T;
}

// Console entry
export interface ConsoleEntry {
  id: string;
  type: "log" | "error" | "warn" | "info" | "result";
  text: string;
  timestamp: number;
  source?: string;
}
