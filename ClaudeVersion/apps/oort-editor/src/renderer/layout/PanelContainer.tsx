import React from "react";
import { panelRegistry, PanelInstance } from "@/core/panel-registry";
import { messageBus, Channels } from "@/core/message-bus";
import "./PanelContainer.css";

interface PanelContainerProps {
  instance: PanelInstance;
  onClose?: () => void;
  onFocus?: () => void;
  isFocused?: boolean;
}

export function PanelContainer({
  instance,
  onClose,
  onFocus,
  isFocused = false,
}: PanelContainerProps) {
  const definition = panelRegistry.getDefinition(instance.panelId);

  if (!definition) {
    return (
      <div className="panel-container panel-error">
        <div className="panel-header">
          <span className="panel-title">Error</span>
        </div>
        <div className="panel-content">
          <p>Panel type "{instance.panelId}" not found</p>
        </div>
      </div>
    );
  }

  const Component = definition.component;

  const handleClose = () => {
    if (definition.closeable !== false) {
      panelRegistry.removeInstance(instance.id);
      onClose?.();
    }
  };

  const handleFocus = () => {
    onFocus?.();
    messageBus.emit(Channels.PANEL_FOCUS, instance);
  };

  return (
    <div
      className={`panel-container ${isFocused ? "panel-focused" : ""}`}
      onClick={handleFocus}
    >
      <div className="panel-header">
        {definition.icon && (
          <span className="panel-icon">{definition.icon}</span>
        )}
        <span className="panel-title">{instance.title}</span>
        <div className="panel-actions">
          {definition.closeable !== false && (
            <button
              className="panel-action panel-close"
              onClick={handleClose}
              title="Close"
            >
              ×
            </button>
          )}
        </div>
      </div>
      <div className="panel-content">
        <Component panelId={instance.panelId} instanceId={instance.id} />
      </div>
    </div>
  );
}

// CSS styles are in PanelContainer.css
