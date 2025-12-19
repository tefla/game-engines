/**
 * Scene Viewport Panel - Basic 2D scene viewer for game preview
 */

import React, { useState, useEffect, useRef, useCallback } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useProject } from "@/hooks/useStore";
import { useMessageBus, useEmit } from "@/hooks/useMessageBus";
import { runtimeService } from "@/core/runtime-service";
import "./SceneViewport.css";

interface ViewportState {
  zoom: number;
  panX: number;
  panY: number;
  gridVisible: boolean;
  playing: boolean;
}

interface SceneEntity {
  id: string;
  name: string;
  x: number;
  y: number;
  width: number;
  height: number;
  rotation: number;
  color: string;
  type: "rect" | "circle" | "sprite";
}

export function SceneViewport({ panelId, instanceId }: PanelProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const animationRef = useRef<number>(0);
  const { isOpen: projectOpen, path: projectPath } = useProject();
  const emit = useEmit();

  const [viewport, setViewport] = useState<ViewportState>({
    zoom: 1,
    panX: 0,
    panY: 0,
    gridVisible: true,
    playing: false,
  });

  const [entities, setEntities] = useState<SceneEntity[]>([
    // Demo entities
    {
      id: "player",
      name: "Player",
      x: 100,
      y: 100,
      width: 50,
      height: 50,
      rotation: 0,
      color: "#4A9EFF",
      type: "rect",
    },
    {
      id: "enemy",
      name: "Enemy",
      x: 300,
      y: 200,
      width: 40,
      height: 40,
      rotation: 45,
      color: "#FF4A4A",
      type: "rect",
    },
    {
      id: "collectible",
      name: "Coin",
      x: 200,
      y: 150,
      width: 30,
      height: 30,
      rotation: 0,
      color: "#FFD700",
      type: "circle",
    },
  ]);

  const [selectedEntity, setSelectedEntity] = useState<string | null>(null);
  const [isDragging, setIsDragging] = useState(false);
  const [dragStart, setDragStart] = useState({ x: 0, y: 0 });
  const [isPanning, setIsPanning] = useState(false);

  // Handle canvas resize
  useEffect(() => {
    const handleResize = () => {
      if (canvasRef.current && containerRef.current) {
        const rect = containerRef.current.getBoundingClientRect();
        canvasRef.current.width = rect.width;
        canvasRef.current.height = rect.height;
        render();
      }
    };

    handleResize();
    window.addEventListener("resize", handleResize);

    const resizeObserver = new ResizeObserver(handleResize);
    if (containerRef.current) {
      resizeObserver.observe(containerRef.current);
    }

    return () => {
      window.removeEventListener("resize", handleResize);
      resizeObserver.disconnect();
    };
  }, []);

  // Listen for play/stop events
  useMessageBus("run:play", () => {
    setViewport((v) => ({ ...v, playing: true }));
  });

  useMessageBus("run:stop", () => {
    setViewport((v) => ({ ...v, playing: false }));
  });

  // Notify selection changes
  useEffect(() => {
    if (selectedEntity) {
      const entity = entities.find((e) => e.id === selectedEntity);
      if (entity) {
        emit("selection:changed", { target: entity, type: "entity" });
      }
    } else {
      emit("selection:changed", { target: null, type: null });
    }
  }, [selectedEntity, entities, emit]);

  // Render loop
  const render = useCallback(() => {
    const canvas = canvasRef.current;
    const ctx = canvas?.getContext("2d");
    if (!canvas || !ctx) return;

    // Clear canvas
    ctx.fillStyle = "#1a1a2e";
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    // Apply viewport transform
    ctx.save();
    ctx.translate(canvas.width / 2 + viewport.panX, canvas.height / 2 + viewport.panY);
    ctx.scale(viewport.zoom, viewport.zoom);

    // Draw grid
    if (viewport.gridVisible) {
      drawGrid(ctx, canvas.width, canvas.height);
    }

    // Draw entities
    for (const entity of entities) {
      drawEntity(ctx, entity, entity.id === selectedEntity);
    }

    ctx.restore();

    // Draw viewport info
    drawViewportInfo(ctx, canvas);
  }, [viewport, entities, selectedEntity]);

  useEffect(() => {
    render();
  }, [render]);

  const drawGrid = (ctx: CanvasRenderingContext2D, width: number, height: number) => {
    const gridSize = 50;
    const gridExtent = Math.max(width, height) / viewport.zoom + 500;

    ctx.strokeStyle = "#2a2a4a";
    ctx.lineWidth = 1 / viewport.zoom;

    // Vertical lines
    for (let x = -gridExtent; x <= gridExtent; x += gridSize) {
      ctx.beginPath();
      ctx.moveTo(x, -gridExtent);
      ctx.lineTo(x, gridExtent);
      ctx.stroke();
    }

    // Horizontal lines
    for (let y = -gridExtent; y <= gridExtent; y += gridSize) {
      ctx.beginPath();
      ctx.moveTo(-gridExtent, y);
      ctx.lineTo(gridExtent, y);
      ctx.stroke();
    }

    // Origin lines
    ctx.strokeStyle = "#4a4a6a";
    ctx.lineWidth = 2 / viewport.zoom;

    ctx.beginPath();
    ctx.moveTo(-gridExtent, 0);
    ctx.lineTo(gridExtent, 0);
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(0, -gridExtent);
    ctx.lineTo(0, gridExtent);
    ctx.stroke();
  };

  const drawEntity = (
    ctx: CanvasRenderingContext2D,
    entity: SceneEntity,
    isSelected: boolean
  ) => {
    ctx.save();
    ctx.translate(entity.x, entity.y);
    ctx.rotate((entity.rotation * Math.PI) / 180);

    ctx.fillStyle = entity.color;

    if (entity.type === "circle") {
      ctx.beginPath();
      ctx.arc(0, 0, entity.width / 2, 0, Math.PI * 2);
      ctx.fill();
    } else {
      ctx.fillRect(
        -entity.width / 2,
        -entity.height / 2,
        entity.width,
        entity.height
      );
    }

    // Selection outline
    if (isSelected) {
      ctx.strokeStyle = "#00ff88";
      ctx.lineWidth = 2 / viewport.zoom;
      ctx.setLineDash([5 / viewport.zoom, 5 / viewport.zoom]);

      if (entity.type === "circle") {
        ctx.beginPath();
        ctx.arc(0, 0, entity.width / 2 + 4, 0, Math.PI * 2);
        ctx.stroke();
      } else {
        ctx.strokeRect(
          -entity.width / 2 - 4,
          -entity.height / 2 - 4,
          entity.width + 8,
          entity.height + 8
        );
      }

      ctx.setLineDash([]);

      // Draw handles
      drawHandles(ctx, entity);
    }

    ctx.restore();
  };

  const drawHandles = (ctx: CanvasRenderingContext2D, entity: SceneEntity) => {
    const handleSize = 8 / viewport.zoom;
    ctx.fillStyle = "#00ff88";

    const corners = [
      { x: -entity.width / 2, y: -entity.height / 2 },
      { x: entity.width / 2, y: -entity.height / 2 },
      { x: -entity.width / 2, y: entity.height / 2 },
      { x: entity.width / 2, y: entity.height / 2 },
    ];

    for (const corner of corners) {
      ctx.fillRect(
        corner.x - handleSize / 2,
        corner.y - handleSize / 2,
        handleSize,
        handleSize
      );
    }
  };

  const drawViewportInfo = (
    ctx: CanvasRenderingContext2D,
    canvas: HTMLCanvasElement
  ) => {
    ctx.fillStyle = "#888";
    ctx.font = "11px monospace";
    ctx.fillText(`Zoom: ${(viewport.zoom * 100).toFixed(0)}%`, 10, canvas.height - 10);

    if (viewport.playing) {
      ctx.fillStyle = "#00ff88";
      ctx.fillText("▶ PLAYING", canvas.width - 80, canvas.height - 10);
    }
  };

  // Screen to world coordinates
  const screenToWorld = (screenX: number, screenY: number) => {
    const canvas = canvasRef.current;
    if (!canvas) return { x: 0, y: 0 };

    const rect = canvas.getBoundingClientRect();
    const canvasX = screenX - rect.left;
    const canvasY = screenY - rect.top;

    const worldX = (canvasX - canvas.width / 2 - viewport.panX) / viewport.zoom;
    const worldY = (canvasY - canvas.height / 2 - viewport.panY) / viewport.zoom;

    return { x: worldX, y: worldY };
  };

  // Hit test entities
  const hitTestEntities = (worldX: number, worldY: number): SceneEntity | null => {
    // Check in reverse order (top-most first)
    for (let i = entities.length - 1; i >= 0; i--) {
      const entity = entities[i];
      const dx = worldX - entity.x;
      const dy = worldY - entity.y;

      if (entity.type === "circle") {
        if (Math.sqrt(dx * dx + dy * dy) <= entity.width / 2) {
          return entity;
        }
      } else {
        if (
          Math.abs(dx) <= entity.width / 2 &&
          Math.abs(dy) <= entity.height / 2
        ) {
          return entity;
        }
      }
    }
    return null;
  };

  // Mouse handlers
  const handleMouseDown = (e: React.MouseEvent) => {
    if (e.button === 1 || (e.button === 0 && e.altKey)) {
      // Middle click or Alt+click for panning
      setIsPanning(true);
      setDragStart({ x: e.clientX, y: e.clientY });
      e.preventDefault();
      return;
    }

    if (e.button === 0) {
      const { x, y } = screenToWorld(e.clientX, e.clientY);
      const entity = hitTestEntities(x, y);

      if (entity) {
        setSelectedEntity(entity.id);
        setIsDragging(true);
        setDragStart({ x: x - entity.x, y: y - entity.y });
      } else {
        setSelectedEntity(null);
      }
    }
  };

  const handleMouseMove = (e: React.MouseEvent) => {
    if (isPanning) {
      const dx = e.clientX - dragStart.x;
      const dy = e.clientY - dragStart.y;
      setViewport((v) => ({
        ...v,
        panX: v.panX + dx,
        panY: v.panY + dy,
      }));
      setDragStart({ x: e.clientX, y: e.clientY });
      return;
    }

    if (isDragging && selectedEntity) {
      const { x, y } = screenToWorld(e.clientX, e.clientY);
      setEntities((entities) =>
        entities.map((entity) =>
          entity.id === selectedEntity
            ? { ...entity, x: x - dragStart.x, y: y - dragStart.y }
            : entity
        )
      );
    }
  };

  const handleMouseUp = () => {
    setIsDragging(false);
    setIsPanning(false);
  };

  const handleWheel = (e: React.WheelEvent) => {
    e.preventDefault();
    const delta = e.deltaY > 0 ? 0.9 : 1.1;
    setViewport((v) => ({
      ...v,
      zoom: Math.min(Math.max(v.zoom * delta, 0.1), 5),
    }));
  };

  // Toolbar actions
  const resetView = () => {
    setViewport((v) => ({ ...v, zoom: 1, panX: 0, panY: 0 }));
  };

  const toggleGrid = () => {
    setViewport((v) => ({ ...v, gridVisible: !v.gridVisible }));
  };

  const togglePlay = () => {
    if (viewport.playing) {
      emit("run:stop", {});
    } else {
      emit("run:play", {});
    }
  };

  if (!projectOpen) {
    return (
      <div className="scene-viewport empty">
        <p>No project open</p>
      </div>
    );
  }

  return (
    <div className="scene-viewport">
      {/* Toolbar */}
      <div className="viewport-toolbar">
        <button
          className={`toolbar-btn ${viewport.playing ? "active" : ""}`}
          onClick={togglePlay}
          title={viewport.playing ? "Stop" : "Play"}
        >
          {viewport.playing ? "⏹" : "▶️"}
        </button>
        <div className="toolbar-separator" />
        <button className="toolbar-btn" onClick={resetView} title="Reset View">
          🎯
        </button>
        <button
          className={`toolbar-btn ${viewport.gridVisible ? "active" : ""}`}
          onClick={toggleGrid}
          title="Toggle Grid"
        >
          #
        </button>
        <div className="toolbar-spacer" />
        <span className="zoom-label">{(viewport.zoom * 100).toFixed(0)}%</span>
      </div>

      {/* Canvas */}
      <div className="viewport-canvas" ref={containerRef}>
        <canvas
          ref={canvasRef}
          onMouseDown={handleMouseDown}
          onMouseMove={handleMouseMove}
          onMouseUp={handleMouseUp}
          onMouseLeave={handleMouseUp}
          onWheel={handleWheel}
        />
      </div>

      {/* Entity List (mini) */}
      <div className="entity-list">
        {entities.map((entity) => (
          <div
            key={entity.id}
            className={`entity-item ${selectedEntity === entity.id ? "selected" : ""}`}
            onClick={() => setSelectedEntity(entity.id)}
          >
            <span
              className="entity-color"
              style={{ backgroundColor: entity.color }}
            />
            {entity.name}
          </div>
        ))}
      </div>
    </div>
  );
}
