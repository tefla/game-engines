import React, { useState, useCallback } from "react";
import { PanelProps } from "@/core/panel-registry";
import { messageBus } from "@/core/message-bus";
import { threeService } from "@/core/three-service";
import { useMessageBus } from "@/hooks/useMessageBus";
import { useFiles } from "@/hooks/useStore";
import "./InspectorPanel.css";

interface InspectedObject {
  type: string;
  name: string;
  properties: Record<string, any>;
  entityId?: string;
}

interface TransformValues {
  position: [number, number, number];
  rotation: [number, number, number];
  scale: [number, number, number];
}

export function InspectorPanel({ panelId, instanceId }: PanelProps) {
  const [inspected, setInspected] = useState<InspectedObject | null>(null);
  const [transform, setTransform] = useState<TransformValues | null>(null);
  const { currentFile } = useFiles();

  // Get active viewport
  const getActiveViewport = useCallback(() => {
    const viewports = threeService.getActiveViewports();
    return viewports[0] || null;
  }, []);

  // Listen for selection changes
  useMessageBus("selection:changed", (data) => {
    if (data.target) {
      setInspected({
        ...data.target,
        entityId: data.target.type === "Entity" ? data.target.name : undefined,
      });

      // If entity, get transform
      if (data.target.type === "Entity") {
        const viewportId = getActiveViewport();
        if (viewportId) {
          const entity = threeService.getEntity(viewportId, data.target.name);
          if (entity) {
            setTransform({
              position: [entity.position.x, entity.position.y, entity.position.z],
              rotation: [
                entity.rotation.x * 180 / Math.PI,
                entity.rotation.y * 180 / Math.PI,
                entity.rotation.z * 180 / Math.PI,
              ],
              scale: [entity.scale.x, entity.scale.y, entity.scale.z],
            });
          }
        }
      } else {
        setTransform(null);
      }
    } else {
      setInspected(null);
      setTransform(null);
    }
  });

  // Listen for transform updates from gizmo
  useMessageBus("entity:transform-changed", (data) => {
    if (inspected?.entityId === data.entityId) {
      setTransform({
        position: data.position,
        rotation: data.rotation,
        scale: data.scale,
      });
    }
  });

  // Handle transform value change
  const handleTransformChange = useCallback(
    (component: "position" | "rotation" | "scale", index: number, value: number) => {
      if (!inspected?.entityId || !transform) return;

      const viewportId = getActiveViewport();
      if (!viewportId) return;

      const newTransform = { ...transform };
      newTransform[component][index] = value;
      setTransform(newTransform);

      // Update entity in Three.js
      threeService.updateEntityTransform(viewportId, inspected.entityId, {
        [component]: newTransform[component],
      });
    },
    [inspected, transform, getActiveViewport]
  );

  // Show file info if a file is selected and no entity is selected
  const fileInfo = currentFile && !inspected
    ? {
        type: "File",
        name: currentFile.split("/").pop() || "Unknown",
        properties: {
          path: currentFile,
          extension: currentFile.split(".").pop() || "none",
        },
      }
    : null;

  const target = inspected || fileInfo;

  if (!target) {
    return (
      <div className="inspector-panel empty">
        <p>Nothing selected</p>
        <p className="hint">Select a file or object to inspect</p>
      </div>
    );
  }

  return (
    <div className="inspector-panel">
      <div className="inspector-header">
        <span className="inspector-type">{target.type}</span>
        <span className="inspector-name">{target.name}</span>
      </div>
      <div className="inspector-content">
        {/* Transform section for entities */}
        {transform && (
          <div className="property-section">
            <div className="section-title">Transform</div>
            <div className="transform-group">
              <TransformRow
                label="Position"
                values={transform.position}
                onChange={(i, v) => handleTransformChange("position", i, v)}
              />
              <TransformRow
                label="Rotation"
                values={transform.rotation}
                onChange={(i, v) => handleTransformChange("rotation", i, v)}
                step={1}
              />
              <TransformRow
                label="Scale"
                values={transform.scale}
                onChange={(i, v) => handleTransformChange("scale", i, v)}
                step={0.1}
              />
            </div>
          </div>
        )}

        {/* Properties section */}
        {!transform && (
          <div className="property-section">
            <div className="section-title">Properties</div>
            <div className="property-list">
              {Object.entries(target.properties).map(([key, value]) => (
                <div key={key} className="property-row">
                  <span className="property-key">{key}</span>
                  <span className="property-value">
                    {formatValue(value)}
                  </span>
                </div>
              ))}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

interface TransformRowProps {
  label: string;
  values: [number, number, number];
  onChange: (index: number, value: number) => void;
  step?: number;
}

function TransformRow({ label, values, onChange, step = 0.1 }: TransformRowProps) {
  const axes = ["X", "Y", "Z"];
  const colors = ["#ff6b6b", "#4ecdc4", "#6b9fff"];

  return (
    <div className="transform-row">
      <span className="transform-label">{label}</span>
      <div className="transform-inputs">
        {values.map((value, i) => (
          <div key={i} className="transform-input-wrapper">
            <span className="axis-label" style={{ color: colors[i] }}>
              {axes[i]}
            </span>
            <input
              type="number"
              className="transform-input"
              value={value.toFixed(2)}
              step={step}
              onChange={(e) => onChange(i, parseFloat(e.target.value) || 0)}
            />
          </div>
        ))}
      </div>
    </div>
  );
}

function formatValue(value: any): string {
  if (value === null) return "null";
  if (value === undefined) return "undefined";
  if (typeof value === "object") {
    return JSON.stringify(value, null, 2);
  }
  return String(value);
}
