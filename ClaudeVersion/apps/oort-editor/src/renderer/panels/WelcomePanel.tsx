import React, { useState } from "react";
import { useProject } from "@/hooks/useStore";
import { messageBus } from "@/core/message-bus";
import "./WelcomePanel.css";

export function WelcomePanel() {
  const { setProject } = useProject();
  const [isCreating, setIsCreating] = useState(false);
  const [newProjectName, setNewProjectName] = useState("");

  const handleOpenProject = async () => {
    const result = await window.electronAPI?.openFolderDialog({
      title: "Open Project",
    });

    if (result && !result.canceled && result.filePaths[0]) {
      const projectPath = result.filePaths[0];
      const openResult = await window.electronAPI?.openProject(projectPath);

      if (openResult?.success) {
        setProject(projectPath, openResult.data.config);
      } else {
        messageBus.emit("status:update", {
          text: `Failed to open project: ${openResult?.error}`,
        });
      }
    }
  };

  const handleCreateProject = async () => {
    if (!newProjectName.trim()) return;

    const result = await window.electronAPI?.openFolderDialog({
      title: "Choose Project Location",
    });

    if (result && !result.canceled && result.filePaths[0]) {
      const basePath = result.filePaths[0];
      const projectPath = `${basePath}/${newProjectName}`;

      const createResult = await window.electronAPI?.createProject(
        projectPath,
        newProjectName
      );

      if (createResult?.success) {
        setProject(projectPath, createResult.data.config);
        setIsCreating(false);
        setNewProjectName("");
      } else {
        messageBus.emit("status:update", {
          text: `Failed to create project: ${createResult?.error}`,
        });
      }
    }
  };

  return (
    <div className="welcome-panel">
      <div className="welcome-content">
        <div className="welcome-logo">
          <span className="logo-icon">🎮</span>
          <h1 className="logo-text">Oort Editor</h1>
          <p className="logo-tagline">Game Development Environment</p>
        </div>

        <div className="welcome-actions">
          {isCreating ? (
            <div className="create-form">
              <input
                type="text"
                placeholder="Project name"
                value={newProjectName}
                onChange={(e) => setNewProjectName(e.target.value)}
                onKeyDown={(e) => e.key === "Enter" && handleCreateProject()}
                autoFocus
              />
              <div className="create-buttons">
                <button
                  className="btn btn-primary"
                  onClick={handleCreateProject}
                  disabled={!newProjectName.trim()}
                >
                  Create
                </button>
                <button
                  className="btn btn-secondary"
                  onClick={() => {
                    setIsCreating(false);
                    setNewProjectName("");
                  }}
                >
                  Cancel
                </button>
              </div>
            </div>
          ) : (
            <>
              <button
                className="welcome-btn welcome-btn-primary"
                onClick={() => setIsCreating(true)}
              >
                <span className="btn-icon">✨</span>
                <span className="btn-text">
                  <span className="btn-title">New Project</span>
                  <span className="btn-desc">Create a new game project</span>
                </span>
              </button>

              <button
                className="welcome-btn welcome-btn-secondary"
                onClick={handleOpenProject}
              >
                <span className="btn-icon">📂</span>
                <span className="btn-text">
                  <span className="btn-title">Open Project</span>
                  <span className="btn-desc">Open an existing project</span>
                </span>
              </button>
            </>
          )}
        </div>

        <div className="welcome-footer">
          <p>
            Built with <span className="heart">♥</span> using Slate language
          </p>
        </div>
      </div>
    </div>
  );
}
