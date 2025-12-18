import React from "react";
import { useProject } from "@/hooks/useStore";
import "./TitleBar.css";

export function TitleBar() {
  const { name, isOpen } = useProject();
  const isMac = navigator.platform.toLowerCase().includes("mac");

  const handleMinimize = () => window.electronAPI?.minimize();
  const handleMaximize = () => window.electronAPI?.maximize();
  const handleClose = () => window.electronAPI?.close();

  return (
    <div className={`title-bar ${isMac ? "mac" : "windows"}`}>
      {/* macOS: Leave space for traffic lights */}
      {isMac && <div className="traffic-light-space" />}

      {/* Title */}
      <div className="title-bar-title drag-region">
        <span className="title-text">
          {isOpen ? name : "Oort Editor"}
        </span>
      </div>

      {/* Windows: Window controls */}
      {!isMac && (
        <div className="window-controls no-drag">
          <button className="window-control" onClick={handleMinimize}>
            <svg width="10" height="1" viewBox="0 0 10 1">
              <rect width="10" height="1" fill="currentColor" />
            </svg>
          </button>
          <button className="window-control" onClick={handleMaximize}>
            <svg width="10" height="10" viewBox="0 0 10 10">
              <rect
                x="0.5"
                y="0.5"
                width="9"
                height="9"
                fill="none"
                stroke="currentColor"
              />
            </svg>
          </button>
          <button
            className="window-control window-close"
            onClick={handleClose}
          >
            <svg width="10" height="10" viewBox="0 0 10 10">
              <line x1="0" y1="0" x2="10" y2="10" stroke="currentColor" />
              <line x1="10" y1="0" x2="0" y2="10" stroke="currentColor" />
            </svg>
          </button>
        </div>
      )}
    </div>
  );
}
