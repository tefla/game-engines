// Panel registration
import { panelRegistry } from "@/core/panel-registry";

// Import builtin panels
import { ProjectExplorer } from "./ProjectExplorer";
import { CodeEditor } from "./CodeEditor";
import { ConsolePanel } from "./ConsolePanel";
import { InspectorPanel } from "./InspectorPanel";

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
}

// Re-export panels
export { ProjectExplorer } from "./ProjectExplorer";
export { CodeEditor } from "./CodeEditor";
export { ConsolePanel } from "./ConsolePanel";
export { InspectorPanel } from "./InspectorPanel";
export { WelcomePanel } from "./WelcomePanel";
