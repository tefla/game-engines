# Oort Game Editor - Implementation Plan

## Vision

A modular, extensible game editor built on Electron that feels like a proper game development environment (Unity/Godot style) rather than a code IDE. The editor integrates seamlessly with the Slate language, VFS, VCS, and SignalBus systems we've already built.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Electron Main Process                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
│  │   Project   │  │    File     │  │   Window    │  │  Extension  │ │
│  │   Manager   │  │   Watcher   │  │   Manager   │  │   Loader    │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
                              │ IPC │
┌─────────────────────────────────────────────────────────────────────┐
│                       Electron Renderer Process                      │
│  ┌─────────────────────────────────────────────────────────────────┐│
│  │                         Panel System                             ││
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐   ││
│  │  │ Project │ │  Code   │ │ Scene   │ │Inspector│ │ Console │   ││
│  │  │Explorer │ │ Editor  │ │Viewport │ │  Panel  │ │  Panel  │   ││
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘   ││
│  └─────────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────────┐│
│  │                         Core Services                            ││
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐   ││
│  │  │ Message │ │  State  │ │  Theme  │ │ Command │ │ Keybind │   ││
│  │  │   Bus   │ │  Store  │ │ Manager │ │ Palette │ │ Manager │   ││
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘   ││
│  └─────────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────────┐│
│  │                      Oort Engine Packages                        ││
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐   ││
│  │  │  Slate  │ │   VFS   │ │   VCS   │ │ Signal  │ │ Editor  │   ││
│  │  │Language │ │ System  │ │ History │ │   Bus   │ │  Core   │   ││
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘   ││
│  └─────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────┘
```

## Core Design Principles

### 1. Panel-First Architecture
Everything is a panel. Panels can be:
- Docked (left, right, top, bottom, center)
- Tabbed (multiple panels in same area)
- Floated (separate window)
- Split (divide any area)

### 2. Extension Points
New features are added through well-defined extension points:
- `registerPanel(panelDefinition)` - Add new panel types
- `registerAssetType(assetDefinition)` - Handle new file types
- `registerCommand(commandDefinition)` - Add commands to palette
- `registerMenuItem(menuDefinition)` - Extend menus
- `registerInspector(inspectorDefinition)` - Custom property editors

### 3. Message-Driven Communication
Panels never talk directly. All communication via MessageBus:
```typescript
messageBus.emit("file:selected", { path: "/scripts/player.sl" });
messageBus.on("file:selected", (data) => { /* update inspector */ });
```

### 4. Centralized State
Single source of truth for app state:
```typescript
store.get("project.currentFile")
store.set("editor.theme", "dark")
store.subscribe("project.*", callback)
```

---

## Package Structure

```
apps/
  oort-editor/              # Electron app
    src/
      main/                 # Main process
        index.ts            # Entry point
        project-manager.ts  # Project loading/saving
        file-watcher.ts     # Watch for external changes
        window-manager.ts   # Window creation/management
        extension-loader.ts # Load extension packages
        ipc-handlers.ts     # IPC communication

      renderer/             # Renderer process
        index.tsx           # React entry point
        App.tsx             # Root component

        core/               # Core services
          message-bus.ts    # Event system (wraps SignalBus)
          state-store.ts    # Centralized state
          command-palette.ts# Command system
          keybind-manager.ts# Keyboard shortcuts
          theme-manager.ts  # Theme loading/switching
          extension-api.ts  # API exposed to extensions

        layout/             # Layout system
          DockingContainer.tsx  # Docking layout manager
          PanelContainer.tsx    # Single panel wrapper
          TabContainer.tsx      # Tabbed panels
          SplitContainer.tsx    # Split view
          FloatingPanel.tsx     # Floating window

        panels/             # Built-in panels
          project-explorer/ # File tree
          code-editor/      # Monaco + Slate
          scene-viewport/   # 3D/2D scene view
          inspector/        # Property editor
          console/          # Output/REPL
          asset-browser/    # Asset grid view

        components/         # Shared UI components
          TreeView.tsx
          PropertyGrid.tsx
          Toolbar.tsx
          StatusBar.tsx
          ContextMenu.tsx

        hooks/              # React hooks
          usePanel.ts
          useCommand.ts
          useStore.ts
          useTheme.ts

      shared/               # Shared between main/renderer
        types.ts
        constants.ts
        ipc-channels.ts

packages/
  @oort/editor-api/         # Extension API package
    src/
      index.ts
      panel-api.ts
      asset-api.ts
      command-api.ts
      menu-api.ts
```

---

## Phase 1: Foundation (Core Shell)

### Goals
- Electron app shell with main/renderer process
- Basic docking/panel system
- Message bus integration
- State management
- Theme support (dark/light)

### Deliverables

#### 1.1 Electron Shell
```typescript
// main/index.ts
import { app, BrowserWindow } from "electron";
import { setupIPC } from "./ipc-handlers";

app.whenReady().then(() => {
  const mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      contextIsolation: true,
    },
    titleBarStyle: "hiddenInset", // Native look on macOS
  });

  setupIPC(mainWindow);
  mainWindow.loadFile("index.html");
});
```

#### 1.2 Docking System
Using a library like `rc-dock` or `golden-layout`, or custom implementation:

```typescript
// layout/DockingContainer.tsx
interface DockLayout {
  type: "row" | "column" | "tabs" | "panel";
  children?: DockLayout[];
  panelId?: string;
  size?: number; // flex ratio
}

const defaultLayout: DockLayout = {
  type: "row",
  children: [
    { type: "panel", panelId: "project-explorer", size: 1 },
    {
      type: "column",
      size: 3,
      children: [
        { type: "tabs", children: [
          { type: "panel", panelId: "code-editor" },
          { type: "panel", panelId: "scene-viewport" },
        ]},
        { type: "panel", panelId: "console", size: 1 },
      ]
    },
    { type: "panel", panelId: "inspector", size: 1 },
  ]
};
```

#### 1.3 Panel Registry
```typescript
// core/panel-registry.ts
interface PanelDefinition {
  id: string;
  title: string;
  icon: string;
  component: React.ComponentType<PanelProps>;
  defaultLocation?: "left" | "right" | "bottom" | "center";
  singleton?: boolean; // Only one instance allowed
}

class PanelRegistry {
  private panels = new Map<string, PanelDefinition>();

  register(definition: PanelDefinition) {
    this.panels.set(definition.id, definition);
    messageBus.emit("panel:registered", definition);
  }

  get(id: string): PanelDefinition | undefined {
    return this.panels.get(id);
  }

  getAll(): PanelDefinition[] {
    return Array.from(this.panels.values());
  }
}
```

#### 1.4 Message Bus (wrapping SignalBus)
```typescript
// core/message-bus.ts
import { SignalBus } from "@oort/engine";

class EditorMessageBus {
  private bus = new SignalBus();

  emit(channel: string, data?: any) {
    const parts = channel.split(":");
    this.bus.emit(parts, data);
  }

  on(channel: string, handler: (data: any) => void): () => void {
    const parts = channel.split(":");
    this.bus.on(parts, handler);
    return () => this.bus.off(parts, handler);
  }

  once(channel: string, handler: (data: any) => void) {
    this.bus.once(parts, handler);
  }
}

export const messageBus = new EditorMessageBus();
```

#### 1.5 State Store
```typescript
// core/state-store.ts
interface EditorState {
  project: {
    path: string | null;
    name: string;
    currentFile: string | null;
    openFiles: string[];
  };
  editor: {
    theme: "dark" | "light";
    fontSize: number;
  };
  ui: {
    sidebarVisible: boolean;
    consoleVisible: boolean;
  };
}

class StateStore {
  private state: EditorState;
  private listeners = new Map<string, Set<Function>>();

  get<T>(path: string): T {
    return getByPath(this.state, path);
  }

  set(path: string, value: any) {
    setByPath(this.state, path, value);
    this.notify(path);
  }

  subscribe(pattern: string, callback: Function): () => void {
    // Support wildcards: "project.*" matches all project changes
  }
}
```

### Tests for Phase 1
- Panel registry add/remove/get
- Message bus emit/subscribe/unsubscribe
- State store get/set/subscribe
- Layout serialization/deserialization

---

## Phase 2: Essential Panels

### Goals
- Project Explorer (file tree)
- Code Editor (Monaco + Slate language)
- Console Panel (output + REPL)
- Basic Inspector (property grid)

### Deliverables

#### 2.1 Project Explorer
```typescript
// panels/project-explorer/ProjectExplorer.tsx
interface FileNode {
  name: string;
  path: string;
  type: "file" | "directory";
  children?: FileNode[];
  icon?: string;
}

function ProjectExplorer() {
  const [tree, setTree] = useState<FileNode | null>(null);
  const projectPath = useStore("project.path");

  // Load from VFS
  useEffect(() => {
    if (projectPath) {
      const tree = buildFileTree(vfs, "/");
      setTree(tree);
    }
  }, [projectPath]);

  const handleSelect = (node: FileNode) => {
    if (node.type === "file") {
      messageBus.emit("file:open", { path: node.path });
    }
  };

  return (
    <TreeView
      data={tree}
      onSelect={handleSelect}
      onContextMenu={handleContextMenu}
    />
  );
}
```

#### 2.2 Code Editor (Monaco)
```typescript
// panels/code-editor/CodeEditor.tsx
import * as monaco from "monaco-editor";
import { registerSlateLanguage } from "./slate-language";

function CodeEditor() {
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor>();
  const currentFile = useStore("project.currentFile");

  useEffect(() => {
    registerSlateLanguage(monaco);
  }, []);

  useEffect(() => {
    if (currentFile && editorRef.current) {
      const content = vfs.read(currentFile);
      editorRef.current.setValue(content);

      // Set language based on extension
      const lang = currentFile.endsWith(".sl") ? "slate" : "plaintext";
      monaco.editor.setModelLanguage(editorRef.current.getModel()!, lang);
    }
  }, [currentFile]);

  return (
    <div className="code-editor">
      <MonacoEditor
        ref={editorRef}
        theme={theme === "dark" ? "slate-dark" : "slate-light"}
        options={{
          minimap: { enabled: true },
          fontSize: 14,
          lineNumbers: "on",
        }}
        onChange={handleChange}
      />
    </div>
  );
}
```

#### 2.3 Slate Language for Monaco
```typescript
// panels/code-editor/slate-language.ts
export function registerSlateLanguage(monaco: typeof Monaco) {
  monaco.languages.register({ id: "slate" });

  monaco.languages.setMonarchTokensProvider("slate", {
    keywords: ["let", "var", "fn", "if", "else", "match", "on", "emit", "for", "in"],

    tokenizer: {
      root: [
        [/#[A-Fa-f0-9]{6}/, "color"],
        [/#.*$/, "comment"],
        [/@\w+/, "signal"],
        [/[a-z_]\w*/, { cases: { "@keywords": "keyword", "@default": "identifier" }}],
        [/[A-Z]\w*/, "type"],
        [/"[^"]*"/, "string"],
        [/\d+\.?\d*/, "number"],
        [/[+\-*\/=<>!]+/, "operator"],
      ],
    },
  });

  // Auto-completion
  monaco.languages.registerCompletionItemProvider("slate", {
    provideCompletionItems: (model, position) => {
      // Return Slate keywords, builtins, and project symbols
    },
  });

  // Diagnostics integration
  // Use our existing Slate parser to provide errors
}
```

#### 2.4 Console Panel
```typescript
// panels/console/ConsolePanel.tsx
interface ConsoleEntry {
  type: "log" | "error" | "warn" | "result";
  text: string;
  timestamp: Date;
}

function ConsolePanel() {
  const [entries, setEntries] = useState<ConsoleEntry[]>([]);
  const [input, setInput] = useState("");

  // Subscribe to console messages
  useEffect(() => {
    return messageBus.on("console:log", (data) => {
      setEntries(e => [...e, { type: "log", text: data.text, timestamp: new Date() }]);
    });
  }, []);

  const handleSubmit = () => {
    // Run Slate code in REPL
    try {
      const result = runSlate(input);
      setEntries(e => [...e, { type: "result", text: stringify(result), timestamp: new Date() }]);
    } catch (err) {
      setEntries(e => [...e, { type: "error", text: err.message, timestamp: new Date() }]);
    }
    setInput("");
  };

  return (
    <div className="console-panel">
      <div className="console-output">
        {entries.map((entry, i) => (
          <div key={i} className={`console-entry console-${entry.type}`}>
            {entry.text}
          </div>
        ))}
      </div>
      <div className="console-input">
        <input
          value={input}
          onChange={e => setInput(e.target.value)}
          onKeyDown={e => e.key === "Enter" && handleSubmit()}
          placeholder="Enter Slate code..."
        />
      </div>
    </div>
  );
}
```

#### 2.5 Inspector Panel
```typescript
// panels/inspector/InspectorPanel.tsx
function InspectorPanel() {
  const [target, setTarget] = useState<any>(null);

  useEffect(() => {
    return messageBus.on("selection:changed", (data) => {
      setTarget(data.target);
    });
  }, []);

  if (!target) {
    return <div className="inspector-empty">Nothing selected</div>;
  }

  return (
    <div className="inspector-panel">
      <PropertyGrid
        target={target}
        onChange={(key, value) => {
          messageBus.emit("property:changed", { target, key, value });
        }}
      />
    </div>
  );
}
```

---

## Phase 3: Project & File Management

### Goals
- Create/open/save projects
- File operations (create, rename, delete)
- VFS integration with real filesystem
- VCS integration (undo/redo in editor)

### Deliverables

#### 3.1 Project Manager
```typescript
// main/project-manager.ts
interface ProjectConfig {
  name: string;
  version: string;
  entry: string;
  assets: string[];
}

class ProjectManager {
  private projectPath: string | null = null;
  private config: ProjectConfig | null = null;

  async create(path: string, name: string): Promise<void> {
    // Create project structure
    await fs.mkdir(path);
    await fs.mkdir(path + "/scripts");
    await fs.mkdir(path + "/assets");
    await fs.mkdir(path + "/scenes");

    const config: ProjectConfig = {
      name,
      version: "1.0.0",
      entry: "/scripts/main.sl",
      assets: ["/assets"],
    };

    await fs.writeFile(path + "/project.json", JSON.stringify(config, null, 2));
    await this.open(path);
  }

  async open(path: string): Promise<void> {
    const configPath = path + "/project.json";
    this.config = JSON.parse(await fs.readFile(configPath, "utf-8"));
    this.projectPath = path;

    // Sync to VFS
    await this.syncToVfs(path);

    // Start file watcher
    fileWatcher.watch(path);

    // Notify renderer
    mainWindow.webContents.send("project:opened", { path, config: this.config });
  }

  private async syncToVfs(fsPath: string, vfsPath: string = "/") {
    // Recursively load filesystem into VFS
  }
}
```

#### 3.2 File Watcher
```typescript
// main/file-watcher.ts
import { watch } from "chokidar";

class FileWatcher {
  private watcher: FSWatcher | null = null;

  watch(projectPath: string) {
    this.watcher = watch(projectPath, {
      ignored: /(^|[\/\\])\../,  // Ignore dotfiles
      persistent: true,
    });

    this.watcher.on("change", (path) => {
      mainWindow.webContents.send("file:external-change", { path });
    });

    this.watcher.on("add", (path) => {
      mainWindow.webContents.send("file:external-add", { path });
    });

    this.watcher.on("unlink", (path) => {
      mainWindow.webContents.send("file:external-delete", { path });
    });
  }
}
```

#### 3.3 VCS Integration
```typescript
// renderer/core/vcs-integration.ts
import { History } from "@oort/vcs";

class EditorHistory {
  private history: History;

  constructor(vfs: VirtualFileSystem) {
    this.history = new History(vfs);
  }

  // Called when user saves a file
  commit(description: string) {
    this.history.commit(description);
    messageBus.emit("vcs:committed", { description });
  }

  undo() {
    if (this.history.undo()) {
      messageBus.emit("vcs:undone");
      return true;
    }
    return false;
  }

  redo() {
    if (this.history.redo()) {
      messageBus.emit("vcs:redone");
      return true;
    }
    return false;
  }

  // For time-travel debugging
  getHistory() {
    return this.history.getHistory();
  }
}
```

---

## Phase 4: Command System & Keybindings

### Goals
- Command palette (Cmd+Shift+P)
- Configurable keybindings
- Menu bar integration
- Context menus

### Deliverables

#### 4.1 Command Registry
```typescript
// core/command-palette.ts
interface Command {
  id: string;
  title: string;
  category?: string;
  keybinding?: string;
  when?: string; // Context expression
  handler: () => void;
}

class CommandRegistry {
  private commands = new Map<string, Command>();

  register(command: Command) {
    this.commands.set(command.id, command);

    if (command.keybinding) {
      keybindManager.bind(command.keybinding, command.id);
    }
  }

  execute(id: string) {
    const command = this.commands.get(id);
    if (command) {
      command.handler();
    }
  }

  search(query: string): Command[] {
    // Fuzzy search commands
  }
}

// Built-in commands
commands.register({
  id: "file.save",
  title: "Save File",
  category: "File",
  keybinding: "Cmd+S",
  handler: () => messageBus.emit("file:save"),
});

commands.register({
  id: "file.open",
  title: "Open File",
  category: "File",
  keybinding: "Cmd+O",
  handler: () => messageBus.emit("file:open-dialog"),
});

commands.register({
  id: "edit.undo",
  title: "Undo",
  category: "Edit",
  keybinding: "Cmd+Z",
  handler: () => editorHistory.undo(),
});
```

#### 4.2 Command Palette UI
```typescript
// components/CommandPalette.tsx
function CommandPalette() {
  const [open, setOpen] = useState(false);
  const [query, setQuery] = useState("");
  const [results, setResults] = useState<Command[]>([]);

  useEffect(() => {
    return keybindManager.on("Cmd+Shift+P", () => setOpen(true));
  }, []);

  useEffect(() => {
    setResults(commands.search(query));
  }, [query]);

  const handleSelect = (command: Command) => {
    commands.execute(command.id);
    setOpen(false);
    setQuery("");
  };

  if (!open) return null;

  return (
    <div className="command-palette-overlay">
      <div className="command-palette">
        <input
          autoFocus
          value={query}
          onChange={e => setQuery(e.target.value)}
          placeholder="Type a command..."
        />
        <div className="command-results">
          {results.map(cmd => (
            <div key={cmd.id} onClick={() => handleSelect(cmd)}>
              <span className="command-category">{cmd.category}</span>
              <span className="command-title">{cmd.title}</span>
              <span className="command-keybinding">{cmd.keybinding}</span>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
```

---

## Phase 5: Extension System

### Goals
- Extension package format
- Extension API
- Extension loading
- Built-in extension examples

### Deliverables

#### 5.1 Extension Format
```typescript
// Extension package.json
{
  "name": "@oort/shader-playground",
  "version": "1.0.0",
  "oort": {
    "type": "extension",
    "activationEvents": ["onCommand:shader.open"],
    "main": "./dist/index.js"
  }
}

// Extension entry point
import { OortExtension, ExtensionContext } from "@oort/editor-api";

export function activate(ctx: ExtensionContext) {
  // Register panel
  ctx.panels.register({
    id: "shader-playground",
    title: "Shader Playground",
    icon: "shader",
    component: ShaderPlaygroundPanel,
  });

  // Register command
  ctx.commands.register({
    id: "shader.open",
    title: "Open Shader Playground",
    handler: () => ctx.panels.open("shader-playground"),
  });

  // Register asset type
  ctx.assets.register({
    extension: ".glsl",
    icon: "shader-file",
    editor: "shader-playground",
  });
}

export function deactivate() {
  // Cleanup
}
```

#### 5.2 Extension API
```typescript
// packages/@oort/editor-api/src/index.ts
export interface ExtensionContext {
  panels: PanelAPI;
  commands: CommandAPI;
  assets: AssetAPI;
  menus: MenuAPI;
  state: StateAPI;
  messageBus: MessageBusAPI;
}

export interface PanelAPI {
  register(definition: PanelDefinition): void;
  open(id: string): void;
  close(id: string): void;
}

export interface CommandAPI {
  register(command: Command): void;
  execute(id: string): void;
}

export interface AssetAPI {
  register(definition: AssetTypeDefinition): void;
  getHandler(extension: string): AssetHandler | undefined;
}
```

#### 5.3 Built-in Extensions

**Asset Browser Extension:**
```typescript
// Grid view of assets with thumbnails
// Drag-and-drop support
// Asset preview on hover
```

**Scene Viewport Extension:**
```typescript
// 3D/2D scene rendering
// Entity selection
// Transform gizmos
// Camera controls
```

**Shader Playground Extension:**
```typescript
// Live shader editing
// Preview with 3D primitives
// Uniform controls
```

---

## Phase 6: Polish & Integration

### Goals
- Themes (dark/light + custom)
- Settings UI
- Welcome screen
- Recent projects
- Error boundaries
- Performance optimization

### Deliverables

#### 6.1 Theme System
```typescript
// core/theme-manager.ts
interface Theme {
  id: string;
  name: string;
  type: "dark" | "light";
  colors: {
    background: string;
    foreground: string;
    accent: string;
    panelBackground: string;
    border: string;
    // ... more
  };
  monaco: monaco.editor.IStandaloneThemeData;
}

class ThemeManager {
  private themes = new Map<string, Theme>();
  private current: Theme;

  register(theme: Theme) {
    this.themes.set(theme.id, theme);
    monaco.editor.defineTheme(theme.id, theme.monaco);
  }

  apply(id: string) {
    const theme = this.themes.get(id);
    if (theme) {
      this.current = theme;
      document.documentElement.style.setProperty("--bg", theme.colors.background);
      // Apply all CSS variables
      messageBus.emit("theme:changed", theme);
    }
  }
}
```

#### 6.2 Settings
```typescript
// Settings stored in user's home directory
// ~/.oort-editor/settings.json
{
  "editor.theme": "oort-dark",
  "editor.fontSize": 14,
  "editor.tabSize": 2,
  "editor.wordWrap": true,
  "terminal.fontSize": 12,
  "keybindings": {
    "file.save": "Cmd+S",
    "file.open": "Cmd+O"
  },
  "recentProjects": [
    "/Users/tim/games/my-game",
    "/Users/tim/games/puzzle-demo"
  ]
}
```

---

## Implementation Order

### Sprint 1: Foundation (Phase 1) - COMPLETE
- [x] Electron app shell with main/renderer
- [x] Basic window with React
- [x] Message bus implementation
- [x] State store implementation
- [x] Simple panel container (no docking yet)
- [x] Dark theme CSS foundation

### Sprint 2: Core Panels (Phase 2) - COMPLETE
- [x] Monaco editor integration
- [x] Slate language mode for Monaco (with autocomplete & snippets)
- [x] Project explorer (tree view)
- [x] Console panel with Slate REPL (fully wired up)
- [x] Basic inspector (read-only)

### Sprint 3: Docking & Layout (Phase 1 cont.) - COMPLETE
- [x] Full docking system
- [x] Layout serialization (auto-save/restore)
- [x] Panel drag-and-drop (with drop zone visual feedback)
- [x] Tab support (with drag reordering)
- [x] Layout presets (Default, Editor Focus, Debugging, Wide)

### Sprint 4: Project Management (Phase 3)
- [ ] Project create/open/save
- [ ] File operations
- [ ] VFS ↔ filesystem sync
- [ ] File watcher
- [ ] VCS integration

### Sprint 5: Commands & Keys (Phase 4)
- [ ] Command registry
- [ ] Command palette UI
- [ ] Keybinding manager
- [ ] Menu bar
- [ ] Context menus

### Sprint 6: Extensions (Phase 5)
- [ ] Extension API package
- [ ] Extension loader
- [ ] Asset browser extension
- [ ] Scene viewport extension (basic)

### Sprint 7: Polish (Phase 6)
- [ ] Theme system
- [ ] Settings UI
- [ ] Welcome screen
- [ ] Performance optimization
- [ ] Error handling

---

## Technology Stack

| Component | Technology | Reason |
|-----------|------------|--------|
| Shell | Electron | Cross-platform desktop app |
| UI | React + TypeScript | Component-based, type-safe |
| Editor | Monaco | VS Code's editor, excellent API |
| State | Zustand or custom | Lightweight, good DX |
| Styling | CSS Modules + CSS Variables | Themeable, scoped |
| Docking | rc-dock or custom | Flexible layout |
| Build | Vite + electron-builder | Fast builds |
| Testing | Vitest + Playwright | Unit + E2E |

---

## Success Criteria

1. **Extensible**: Adding a new panel type requires < 50 lines of code
2. **Responsive**: UI feels snappy, < 16ms frame times
3. **Integrated**: VFS, VCS, SignalBus work seamlessly with editor
4. **Familiar**: Devs coming from VS Code/Unity feel at home
5. **Testable**: Core systems have > 80% test coverage

---

## Next Steps

1. Create `apps/oort-editor/` directory structure
2. Set up Electron + React + Vite
3. Implement message bus and state store
4. Build first panel (code editor with Monaco)
5. Iterate from there!
