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
  mainScene?: string;
  settings?: ProjectSettings;
}

export interface ProjectSettings {
  physics?: boolean;
  defaultBackgroundColor?: string;
}

// Scene types
export interface SceneConfig {
  name: string;
  version: string;
  entities: EntityConfig[];
  settings?: SceneSettings;
}

export interface SceneSettings {
  backgroundColor?: string;
  ambientLight?: { color: string; intensity: number };
  directionalLight?: {
    color: string;
    intensity: number;
    position: [number, number, number];
  };
}

export interface EntityConfig {
  id: string;
  name: string;
  parentId?: string;
  transform: TransformConfig;
  components: ComponentConfig[];
}

export interface TransformConfig {
  position: [number, number, number];
  rotation: [number, number, number];
  scale: [number, number, number];
}

export interface ComponentConfig {
  type: string;
  data: Record<string, any>;
}

// Entity hierarchy for Scene Hierarchy Panel
export interface EntityNode {
  id: string;
  name: string;
  children: EntityNode[];
  expanded?: boolean;
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
