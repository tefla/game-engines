import React, { useState, useEffect } from "react";
import { DockingLayout, LayoutNode, createDefaultLayout } from "@/layout/DockingLayout";
import { TitleBar } from "@/components/TitleBar";
import { StatusBar } from "@/components/StatusBar";
import { WelcomePanel } from "@/panels/WelcomePanel";
import { panelRegistry } from "@/core/panel-registry";
import { useUI, useProject } from "@/hooks/useStore";
import { registerBuiltinPanels } from "@/panels";
import "./App.css";

export function App() {
  const [layout, setLayout] = useState<LayoutNode | null>(null);
  const [initialized, setInitialized] = useState(false);
  const { theme } = useUI();
  const { isOpen: projectOpen } = useProject();

  // Initialize panels on mount
  useEffect(() => {
    if (!initialized) {
      registerBuiltinPanels();
      setInitialized(true);
    }
  }, [initialized]);

  // Create layout once panels are registered
  useEffect(() => {
    if (initialized && projectOpen && !layout) {
      setLayout(createDefaultLayout());
    }
  }, [initialized, projectOpen, layout]);

  // Apply theme
  useEffect(() => {
    document.documentElement.setAttribute("data-theme", theme);
  }, [theme]);

  return (
    <div className="app">
      <TitleBar />
      <div className="app-content">
        {projectOpen && layout ? (
          <DockingLayout layout={layout} onLayoutChange={setLayout} />
        ) : (
          <WelcomePanel />
        )}
      </div>
      <StatusBar />
    </div>
  );
}
