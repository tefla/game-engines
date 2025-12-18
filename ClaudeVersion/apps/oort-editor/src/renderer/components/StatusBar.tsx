import React, { useState, useEffect } from "react";
import { useMessageBus } from "@/hooks/useMessageBus";
import { useFiles, useUI } from "@/hooks/useStore";
import "./StatusBar.css";

export function StatusBar() {
  const { currentFile, isModified } = useFiles();
  const { theme, setTheme } = useUI();
  const [status, setStatus] = useState<string>("");

  // Listen for status updates
  useMessageBus("status:update", (data: { text: string }) => {
    setStatus(data.text);
    // Clear after 5 seconds
    setTimeout(() => setStatus(""), 5000);
  });

  const toggleTheme = () => {
    setTheme(theme === "dark" ? "light" : "dark");
  };

  return (
    <div className="status-bar">
      <div className="status-left">
        {currentFile && (
          <span className="status-item">
            {currentFile}
            {isModified(currentFile) && <span className="modified-dot" />}
          </span>
        )}
        {status && <span className="status-item status-message">{status}</span>}
      </div>
      <div className="status-right">
        <button className="status-item status-button" onClick={toggleTheme}>
          {theme === "dark" ? "🌙" : "☀️"}
        </button>
        <span className="status-item">Oort Editor v0.1.0</span>
      </div>
    </div>
  );
}
