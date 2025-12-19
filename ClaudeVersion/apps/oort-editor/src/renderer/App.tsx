import React, { useState, useEffect, useCallback, useRef } from "react";
import { DockingLayout, LayoutNode, createDefaultLayout } from "@/layout/DockingLayout";
import { layoutManager } from "@/layout/layout-manager";
import { TitleBar } from "@/components/TitleBar";
import { StatusBar } from "@/components/StatusBar";
import { WelcomePanel } from "@/panels/WelcomePanel";
import { CommandPalette, useCommandPalette } from "@/components/CommandPalette";
import { panelRegistry } from "@/core/panel-registry";
import { useUI, useProject } from "@/hooks/useStore";
import { useMessageBus, useEmit } from "@/hooks/useMessageBus";
import { registerBuiltinPanels } from "@/panels";
import { registerBuiltinCommands } from "@/core/builtin-commands";
import { runtimeService } from "@/core/runtime-service";
import { extensionManager } from "@/core/extension-manager";
import { store } from "@/core/state-store";
import "./App.css";

// Expose store for e2e testing and debugging
declare global {
  interface Window {
    __oortStore?: typeof store;
  }
}
// Always expose store - this is an editor tool, not a public-facing app
window.__oortStore = store;

export function App() {
  const [layout, setLayout] = useState<LayoutNode | null>(null);
  const [initialized, setInitialized] = useState(false);
  const { theme } = useUI();
  const { isOpen: projectOpen, path: projectPath } = useProject();
  const saveTimeoutRef = useRef<number | null>(null);
  const emit = useEmit();
  const commandPalette = useCommandPalette();

  // Initialize panels, commands, and extensions on mount
  useEffect(() => {
    if (!initialized) {
      registerBuiltinPanels();
      registerBuiltinCommands();
      // Initialize extension manager (async but we don't block on it)
      extensionManager.initialize().catch((err) => {
        console.error("Failed to initialize extensions:", err);
      });
      setInitialized(true);
    }
  }, [initialized]);

  // Create layout once panels are registered
  useEffect(() => {
    if (initialized && projectOpen && !layout) {
      // Try to load saved layout, fall back to default
      const savedLayout = layoutManager.loadLayout();
      if (savedLayout) {
        setLayout(savedLayout);
      } else {
        setLayout(createDefaultLayout());
      }
    }
  }, [initialized, projectOpen, layout]);

  // Initialize runtime when project opens
  useEffect(() => {
    if (projectOpen && projectPath) {
      runtimeService.initializeForProject(projectPath).then(() => {
        emit("console:info", { text: "Runtime initialized with project files" });
      }).catch((err) => {
        emit("console:error", { text: `Failed to initialize runtime: ${err}` });
      });
    } else {
      runtimeService.close();
    }
  }, [projectOpen, projectPath, emit]);

  // Sync VFS when files change externally
  useMessageBus("file:external-change", async (data: { event: string; path: string }) => {
    if (!runtimeService.isInitialized()) return;

    switch (data.event) {
      case "add":
      case "change":
        await runtimeService.syncFile(data.path);
        break;
      case "unlink":
        await runtimeService.handleFileDeletion(data.path);
        break;
      case "addDir":
        await runtimeService.handleDirectoryCreation(data.path);
        break;
      case "unlinkDir":
        await runtimeService.handleFileDeletion(data.path);
        break;
    }
  });

  // Sync VFS when files are saved from the editor
  useMessageBus("file:saved", async (data: { path: string }) => {
    if (runtimeService.isInitialized()) {
      await runtimeService.syncFile(data.path);
    }
  });

  // Auto-save layout when it changes (debounced)
  const handleLayoutChange = useCallback((newLayout: LayoutNode) => {
    setLayout(newLayout);

    // Debounce layout save
    if (saveTimeoutRef.current) {
      clearTimeout(saveTimeoutRef.current);
    }
    saveTimeoutRef.current = window.setTimeout(() => {
      layoutManager.saveLayout(newLayout);
    }, 1000);
  }, []);

  // Listen for layout preset changes
  useMessageBus("layout:apply-preset", (data: { presetId: string }) => {
    const newLayout = layoutManager.applyPreset(data.presetId);
    if (newLayout) {
      setLayout(newLayout);
    }
  });

  // Listen for layout reset
  useMessageBus("layout:reset", () => {
    const newLayout = layoutManager.resetToDefault();
    setLayout(newLayout);
  });

  // Apply theme
  useEffect(() => {
    document.documentElement.setAttribute("data-theme", theme);
  }, [theme]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      if (saveTimeoutRef.current) {
        clearTimeout(saveTimeoutRef.current);
      }
    };
  }, []);

  return (
    <div className="app">
      <TitleBar />
      <div className="app-content">
        {projectOpen && layout ? (
          <DockingLayout layout={layout} onLayoutChange={handleLayoutChange} />
        ) : (
          <WelcomePanel />
        )}
      </div>
      <StatusBar />
      <CommandPalette isOpen={commandPalette.isOpen} onClose={commandPalette.close} />
    </div>
  );
}
