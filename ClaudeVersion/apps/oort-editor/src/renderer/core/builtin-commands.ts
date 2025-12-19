/**
 * Built-in Commands - Standard editor commands
 */

import { Command, commandRegistry } from "./command-registry";
import { keybindingManager } from "./keybinding-manager";
import { messageBus } from "./message-bus";
import { store } from "./state-store";

/**
 * Register all built-in commands
 */
export function registerBuiltinCommands(): void {
  const commands: Command[] = [
    // ============ File Commands ============
    {
      id: "file.newFile",
      title: "New File",
      category: "File",
      keybinding: "Cmd+N",
      icon: "file-plus",
      handler: () => {
        messageBus.emit("file:new", {});
      },
    },
    {
      id: "file.newFolder",
      title: "New Folder",
      category: "File",
      keybinding: "Cmd+Shift+N",
      icon: "folder-plus",
      handler: () => {
        messageBus.emit("folder:new", {});
      },
    },
    {
      id: "file.open",
      title: "Open File...",
      category: "File",
      keybinding: "Cmd+O",
      icon: "folder-open",
      handler: () => {
        messageBus.emit("file:open-dialog", {});
      },
    },
    {
      id: "file.save",
      title: "Save",
      category: "File",
      keybinding: "Cmd+S",
      icon: "save",
      when: () => store.getState().files.currentFile !== null,
      handler: () => {
        messageBus.emit("file:save", {});
      },
    },
    {
      id: "file.saveAs",
      title: "Save As...",
      category: "File",
      keybinding: "Cmd+Shift+S",
      icon: "save",
      when: () => store.getState().files.currentFile !== null,
      handler: () => {
        messageBus.emit("file:save-as", {});
      },
    },
    {
      id: "file.closeFile",
      title: "Close File",
      category: "File",
      keybinding: "Cmd+W",
      when: () => store.getState().files.currentFile !== null,
      handler: () => {
        messageBus.emit("file:close", {});
      },
    },

    // ============ Edit Commands ============
    {
      id: "edit.undo",
      title: "Undo",
      category: "Edit",
      keybinding: "Cmd+Z",
      icon: "undo",
      handler: () => {
        messageBus.emit("edit:undo", {});
      },
    },
    {
      id: "edit.redo",
      title: "Redo",
      category: "Edit",
      keybinding: "Cmd+Shift+Z",
      icon: "redo",
      handler: () => {
        messageBus.emit("edit:redo", {});
      },
    },
    {
      id: "edit.cut",
      title: "Cut",
      category: "Edit",
      keybinding: "Cmd+X",
      handler: () => {
        document.execCommand("cut");
      },
    },
    {
      id: "edit.copy",
      title: "Copy",
      category: "Edit",
      keybinding: "Cmd+C",
      handler: () => {
        document.execCommand("copy");
      },
    },
    {
      id: "edit.paste",
      title: "Paste",
      category: "Edit",
      keybinding: "Cmd+V",
      handler: () => {
        document.execCommand("paste");
      },
    },
    {
      id: "edit.selectAll",
      title: "Select All",
      category: "Edit",
      keybinding: "Cmd+A",
      handler: () => {
        document.execCommand("selectAll");
      },
    },
    {
      id: "edit.find",
      title: "Find",
      category: "Edit",
      keybinding: "Cmd+F",
      icon: "search",
      handler: () => {
        messageBus.emit("edit:find", {});
      },
    },
    {
      id: "edit.findReplace",
      title: "Find and Replace",
      category: "Edit",
      keybinding: "Cmd+Shift+F",
      icon: "search",
      handler: () => {
        messageBus.emit("edit:find-replace", {});
      },
    },

    // ============ View Commands ============
    {
      id: "view.commandPalette",
      title: "Command Palette",
      category: "View",
      keybinding: "Cmd+Shift+P",
      icon: "terminal",
      handler: () => {
        messageBus.emit("ui:toggle-command-palette", {});
      },
    },
    {
      id: "view.toggleSidebar",
      title: "Toggle Sidebar",
      category: "View",
      keybinding: "Cmd+B",
      icon: "sidebar",
      handler: () => {
        const current = store.getState().ui.sidebarVisible;
        store.getState().setSidebarVisible(!current);
      },
    },
    {
      id: "view.toggleConsole",
      title: "Toggle Console",
      category: "View",
      keybinding: "Cmd+J",
      icon: "terminal",
      handler: () => {
        messageBus.emit("ui:toggle-console", {});
      },
    },
    {
      id: "view.toggleInspector",
      title: "Toggle Inspector",
      category: "View",
      keybinding: "Cmd+I",
      icon: "info",
      handler: () => {
        messageBus.emit("ui:toggle-inspector", {});
      },
    },
    {
      id: "view.toggleAssetBrowser",
      title: "Toggle Asset Browser",
      category: "View",
      keybinding: "Cmd+Shift+A",
      icon: "package",
      handler: () => {
        messageBus.emit("ui:toggle-asset-browser", {});
      },
    },
    {
      id: "view.openSceneViewport",
      title: "Open Scene 2D Viewport",
      category: "View",
      keybinding: "Cmd+Shift+V",
      icon: "game",
      handler: () => {
        messageBus.emit("ui:open-scene-viewport", {});
      },
    },
    {
      id: "view.openSceneViewport3D",
      title: "Open Scene 3D Viewport",
      category: "View",
      keybinding: "Cmd+Shift+3",
      icon: "cube",
      handler: () => {
        messageBus.emit("ui:open-scene-viewport-3d", {});
      },
    },
    {
      id: "view.toggleGameStats",
      title: "Toggle Game Stats",
      category: "View",
      keybinding: "Cmd+Shift+G",
      icon: "activity",
      handler: () => {
        messageBus.emit("ui:toggle-game-stats", {});
      },
    },
    {
      id: "view.zoomIn",
      title: "Zoom In",
      category: "View",
      keybinding: "Cmd+=",
      handler: () => {
        messageBus.emit("view:zoom-in", {});
      },
    },
    {
      id: "view.zoomOut",
      title: "Zoom Out",
      category: "View",
      keybinding: "Cmd+-",
      handler: () => {
        messageBus.emit("view:zoom-out", {});
      },
    },
    {
      id: "view.resetZoom",
      title: "Reset Zoom",
      category: "View",
      keybinding: "Cmd+0",
      handler: () => {
        messageBus.emit("view:zoom-reset", {});
      },
    },

    // ============ Project Commands ============
    {
      id: "project.open",
      title: "Open Project...",
      category: "Project",
      keybinding: "Cmd+Shift+O",
      icon: "folder",
      handler: () => {
        messageBus.emit("project:open-dialog", {});
      },
    },
    {
      id: "project.close",
      title: "Close Project",
      category: "Project",
      when: () => store.getState().project.isOpen,
      handler: () => {
        messageBus.emit("project:close", {});
      },
    },
    {
      id: "project.newProject",
      title: "New Project...",
      category: "Project",
      icon: "folder-plus",
      handler: () => {
        messageBus.emit("project:new-dialog", {});
      },
    },
    {
      id: "project.refresh",
      title: "Refresh Project",
      category: "Project",
      keybinding: "Cmd+Shift+R",
      when: () => store.getState().project.isOpen,
      handler: () => {
        messageBus.emit("project:refresh", {});
      },
    },

    // ============ Run Commands ============
    {
      id: "run.play",
      title: "Play Game",
      category: "Run",
      keybinding: "Cmd+Enter",
      icon: "play",
      when: () => store.getState().project.isOpen,
      handler: () => {
        messageBus.emit("game:play", {});
      },
    },
    {
      id: "run.stop",
      title: "Stop Game",
      category: "Run",
      keybinding: "Cmd+.",
      icon: "stop",
      handler: () => {
        messageBus.emit("game:stop", {});
      },
    },
    {
      id: "run.restart",
      title: "Restart Game",
      category: "Run",
      keybinding: "Cmd+Shift+Enter",
      icon: "refresh",
      handler: () => {
        messageBus.emit("game:restart", {});
      },
    },

    // ============ VCS Commands ============
    {
      id: "vcs.checkpoint",
      title: "Create Checkpoint",
      category: "Version Control",
      keybinding: "Cmd+Shift+C",
      icon: "bookmark",
      when: () => store.getState().project.isOpen,
      handler: () => {
        messageBus.emit("vcs:checkpoint", {});
      },
    },
    {
      id: "vcs.listCheckpoints",
      title: "List Checkpoints",
      category: "Version Control",
      icon: "list",
      when: () => store.getState().project.isOpen,
      handler: () => {
        messageBus.emit("vcs:list-checkpoints", {});
      },
    },

    // ============ Help Commands ============
    {
      id: "help.about",
      title: "About Oort Editor",
      category: "Help",
      icon: "info",
      handler: () => {
        messageBus.emit("help:about", {});
      },
    },
    {
      id: "help.documentation",
      title: "Documentation",
      category: "Help",
      keybinding: "F1",
      icon: "book",
      handler: () => {
        messageBus.emit("help:docs", {});
      },
    },
    {
      id: "help.keyboardShortcuts",
      title: "Keyboard Shortcuts",
      category: "Help",
      keybinding: "Cmd+K Cmd+S",
      icon: "keyboard",
      handler: () => {
        messageBus.emit("help:shortcuts", {});
      },
    },

    // ============ Developer Commands ============
    {
      id: "dev.toggleDevTools",
      title: "Toggle Developer Tools",
      category: "Developer",
      keybinding: "Cmd+Alt+I",
      icon: "code",
      handler: () => {
        window.electronAPI?.toggleDevTools();
      },
    },
    {
      id: "dev.reloadWindow",
      title: "Reload Window",
      category: "Developer",
      keybinding: "Cmd+R",
      handler: () => {
        window.location.reload();
      },
    },

    // ============ Layout Commands ============
    {
      id: "layout.default",
      title: "Default Layout",
      category: "Layout",
      handler: () => {
        messageBus.emit("layout:apply-preset", { preset: "default" });
      },
    },
    {
      id: "layout.editorFocus",
      title: "Editor Focus Layout",
      category: "Layout",
      handler: () => {
        messageBus.emit("layout:apply-preset", { preset: "editor-focus" });
      },
    },
    {
      id: "layout.wide",
      title: "Wide Layout",
      category: "Layout",
      handler: () => {
        messageBus.emit("layout:apply-preset", { preset: "wide" });
      },
    },
    {
      id: "layout.resetLayout",
      title: "Reset Layout",
      category: "Layout",
      handler: () => {
        messageBus.emit("layout:reset", {});
      },
    },

    // ============ Navigation Commands ============
    {
      id: "nav.goToFile",
      title: "Go to File...",
      category: "Navigation",
      keybinding: "Cmd+P",
      icon: "file",
      when: () => store.getState().project.isOpen,
      handler: () => {
        messageBus.emit("nav:go-to-file", {});
      },
    },
    {
      id: "nav.goToLine",
      title: "Go to Line...",
      category: "Navigation",
      keybinding: "Cmd+G",
      when: () => store.getState().files.currentFile !== null,
      handler: () => {
        messageBus.emit("nav:go-to-line", {});
      },
    },
    {
      id: "nav.goToSymbol",
      title: "Go to Symbol...",
      category: "Navigation",
      keybinding: "Cmd+Shift+G",
      when: () => store.getState().files.currentFile !== null,
      handler: () => {
        messageBus.emit("nav:go-to-symbol", {});
      },
    },
  ];

  // Register all commands
  commands.forEach(command => {
    commandRegistry.register(command);

    // Bind keybinding if specified
    if (command.keybinding) {
      keybindingManager.bind(command.keybinding, command.id, command.when);
    }
  });

  console.log(`Registered ${commands.length} built-in commands`);
}
