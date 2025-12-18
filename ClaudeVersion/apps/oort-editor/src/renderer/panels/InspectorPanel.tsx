import React, { useState } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useMessageBus } from "@/hooks/useMessageBus";
import { useFiles } from "@/hooks/useStore";
import "./InspectorPanel.css";

interface InspectedObject {
  type: string;
  name: string;
  properties: Record<string, any>;
}

export function InspectorPanel({ panelId, instanceId }: PanelProps) {
  const [inspected, setInspected] = useState<InspectedObject | null>(null);
  const { currentFile } = useFiles();

  // Listen for selection changes
  useMessageBus("selection:changed", (data) => {
    if (data.target) {
      setInspected(data.target);
    } else {
      setInspected(null);
    }
  });

  // Show file info if a file is selected
  const fileInfo = currentFile
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
