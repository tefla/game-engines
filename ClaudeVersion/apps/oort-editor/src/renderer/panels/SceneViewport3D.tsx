import React, { useRef, useEffect, useState, useCallback } from "react";
import { PanelProps } from "@/core/panel-registry";
import { threeService } from "@/core/three-service";
import { useMessageBus } from "@/hooks/useMessageBus";
import "./SceneViewport3D.css";

type ViewMode = "perspective" | "top" | "front" | "right";

export function SceneViewport3D({ panelId, instanceId }: PanelProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const [isInitialized, setIsInitialized] = useState(false);
  const [viewMode, setViewMode] = useState<ViewMode>("perspective");
  const [showGrid, setShowGrid] = useState(true);
  const [showAxes, setShowAxes] = useState(true);

  // Initialize Three.js scene
  useEffect(() => {
    const canvas = canvasRef.current;
    const container = containerRef.current;
    if (!canvas || !container) return;

    // Create scene
    const state = threeService.createScene(instanceId, canvas);

    // Initial resize
    const rect = container.getBoundingClientRect();
    threeService.resize(instanceId, rect.width, rect.height);

    // Start render loop
    threeService.startRenderLoop(instanceId);

    // Add a test cube
    threeService.addPrimitive(instanceId, "box", {
      position: [0, 0.5, 0],
      color: 0x4a9eff,
    });

    setIsInitialized(true);

    return () => {
      threeService.disposeScene(instanceId);
    };
  }, [instanceId]);

  // Handle resize
  useEffect(() => {
    const container = containerRef.current;
    if (!container || !isInitialized) return;

    const resizeObserver = new ResizeObserver((entries) => {
      for (const entry of entries) {
        const { width, height } = entry.contentRect;
        if (width > 0 && height > 0) {
          threeService.resize(instanceId, width, height);
        }
      }
    });

    resizeObserver.observe(container);
    return () => resizeObserver.disconnect();
  }, [instanceId, isInitialized]);

  // Toggle grid visibility
  const toggleGrid = useCallback(() => {
    const state = threeService.getScene(instanceId);
    if (!state) return;

    const grid = state.scene.getObjectByName("__grid");
    if (grid) {
      grid.visible = !showGrid;
      setShowGrid(!showGrid);
    }
  }, [instanceId, showGrid]);

  // Toggle axes visibility
  const toggleAxes = useCallback(() => {
    const state = threeService.getScene(instanceId);
    if (!state) return;

    const axes = state.scene.getObjectByName("__axes");
    if (axes) {
      axes.visible = !showAxes;
      setShowAxes(!showAxes);
    }
  }, [instanceId, showAxes]);

  // Set camera to predefined view
  const setView = useCallback(
    (mode: ViewMode) => {
      const state = threeService.getScene(instanceId);
      if (!state) return;

      const distance = 10;
      const { camera, controls } = state;

      switch (mode) {
        case "perspective":
          camera.position.set(5, 5, 5);
          break;
        case "top":
          camera.position.set(0, distance, 0);
          break;
        case "front":
          camera.position.set(0, 0, distance);
          break;
        case "right":
          camera.position.set(distance, 0, 0);
          break;
      }

      controls.target.set(0, 0, 0);
      controls.update();
      setViewMode(mode);
    },
    [instanceId]
  );

  // Reset camera
  const resetCamera = useCallback(() => {
    setView("perspective");
  }, [setView]);

  // Add primitive shapes (for testing)
  const addShape = useCallback(
    (type: "box" | "sphere" | "cylinder") => {
      const x = (Math.random() - 0.5) * 6;
      const z = (Math.random() - 0.5) * 6;
      const color = Math.random() * 0xffffff;

      threeService.addPrimitive(instanceId, type, {
        position: [x, 0.5, z],
        color,
      });
    },
    [instanceId]
  );

  // Listen for entity spawn events
  useMessageBus("entity:spawn-3d", (data) => {
    if (data.viewportId === instanceId || !data.viewportId) {
      threeService.spawnEntity(instanceId, data);
    }
  });

  return (
    <div className="scene-viewport-3d" ref={containerRef}>
      <canvas ref={canvasRef} className="viewport-canvas" />

      {/* Toolbar */}
      <div className="viewport-toolbar">
        <div className="toolbar-group">
          <button
            className={`toolbar-btn ${viewMode === "perspective" ? "active" : ""}`}
            onClick={() => setView("perspective")}
            title="Perspective View"
          >
            3D
          </button>
          <button
            className={`toolbar-btn ${viewMode === "top" ? "active" : ""}`}
            onClick={() => setView("top")}
            title="Top View"
          >
            Top
          </button>
          <button
            className={`toolbar-btn ${viewMode === "front" ? "active" : ""}`}
            onClick={() => setView("front")}
            title="Front View"
          >
            Front
          </button>
          <button
            className={`toolbar-btn ${viewMode === "right" ? "active" : ""}`}
            onClick={() => setView("right")}
            title="Right View"
          >
            Right
          </button>
        </div>

        <div className="toolbar-separator" />

        <div className="toolbar-group">
          <button
            className={`toolbar-btn ${showGrid ? "active" : ""}`}
            onClick={toggleGrid}
            title="Toggle Grid"
          >
            Grid
          </button>
          <button
            className={`toolbar-btn ${showAxes ? "active" : ""}`}
            onClick={toggleAxes}
            title="Toggle Axes"
          >
            Axes
          </button>
        </div>

        <div className="toolbar-separator" />

        <div className="toolbar-group">
          <button
            className="toolbar-btn"
            onClick={() => addShape("box")}
            title="Add Box"
          >
            +Box
          </button>
          <button
            className="toolbar-btn"
            onClick={() => addShape("sphere")}
            title="Add Sphere"
          >
            +Sphere
          </button>
          <button
            className="toolbar-btn"
            onClick={resetCamera}
            title="Reset Camera"
          >
            Reset
          </button>
        </div>
      </div>

      {/* Info overlay */}
      <div className="viewport-info">
        <span className="info-label">
          {viewMode.charAt(0).toUpperCase() + viewMode.slice(1)}
        </span>
      </div>
    </div>
  );
}
