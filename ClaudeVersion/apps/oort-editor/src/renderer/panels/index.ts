// Panel registration
import { panelRegistry } from "@/core/panel-registry";

// Import builtin panels
import { ProjectExplorer } from "./ProjectExplorer";
import { CodeEditor } from "./CodeEditor";
import { ConsolePanel } from "./ConsolePanel";
import { InspectorPanel } from "./InspectorPanel";
import { AssetBrowser } from "./AssetBrowser";
import { SceneViewport } from "./SceneViewport";
import { SceneViewport3D } from "./SceneViewport3D";
import { GameStatsPanel } from "./GameStatsPanel";
import { SceneHierarchyPanel } from "./SceneHierarchyPanel";

// Register all builtin panels
export function registerBuiltinPanels(): void {
  panelRegistry.register({
    id: "project-explorer",
    title: "Project",
    icon: "📁",
    component: ProjectExplorer,
    defaultLocation: "left",
    singleton: true,
    closeable: false,
  });

  panelRegistry.register({
    id: "code-editor",
    title: "Editor",
    icon: "📝",
    component: CodeEditor,
    defaultLocation: "center",
    singleton: false,
  });

  panelRegistry.register({
    id: "console",
    title: "Console",
    icon: "💬",
    component: ConsolePanel,
    defaultLocation: "bottom",
    singleton: true,
  });

  panelRegistry.register({
    id: "inspector",
    title: "Inspector",
    icon: "🔍",
    component: InspectorPanel,
    defaultLocation: "right",
    singleton: true,
  });

  panelRegistry.register({
    id: "scene-hierarchy",
    title: "Hierarchy",
    icon: "🌳",
    component: SceneHierarchyPanel,
    defaultLocation: "left",
    singleton: true,
  });

  panelRegistry.register({
    id: "asset-browser",
    title: "Assets",
    icon: "📦",
    component: AssetBrowser,
    defaultLocation: "bottom",
    singleton: true,
  });

  panelRegistry.register({
    id: "scene-viewport",
    title: "Scene 2D",
    icon: "🎮",
    component: SceneViewport,
    defaultLocation: "center",
    singleton: false,
  });

  panelRegistry.register({
    id: "scene-viewport-3d",
    title: "Scene 3D",
    icon: "🎲",
    component: SceneViewport3D,
    defaultLocation: "center",
    singleton: false,
  });

  // Extension-contributed panel (oort.game-stats)
  panelRegistry.register({
    id: "game-stats",
    title: "Game Stats",
    icon: "📊",
    component: GameStatsPanel,
    defaultLocation: "bottom",
    singleton: true,
  });
}

// Re-export panels
export { ProjectExplorer } from "./ProjectExplorer";
export { CodeEditor } from "./CodeEditor";
export { ConsolePanel } from "./ConsolePanel";
export { InspectorPanel } from "./InspectorPanel";
export { AssetBrowser } from "./AssetBrowser";
export { SceneViewport } from "./SceneViewport";
export { SceneViewport3D } from "./SceneViewport3D";
export { WelcomePanel } from "./WelcomePanel";
export { GameStatsPanel } from "./GameStatsPanel";
export { SceneHierarchyPanel } from "./SceneHierarchyPanel";
