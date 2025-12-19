import React, { useState, useEffect } from "react";
import { useProject } from "@/hooks/useStore";
import { messageBus } from "@/core/message-bus";
import "./WelcomePanel.css";

interface RecentProject {
  path: string;
  name: string;
  lastOpened: string;
}

export function WelcomePanel() {
  const { setProject } = useProject();
  const [isCreating, setIsCreating] = useState(false);
  const [newProjectName, setNewProjectName] = useState("");
  const [recentProjects, setRecentProjects] = useState<RecentProject[]>([]);
  const [loading, setLoading] = useState(true);

  // Load recent projects on mount
  useEffect(() => {
    loadRecentProjects();
  }, []);

  const loadRecentProjects = async () => {
    setLoading(true);
    try {
      const projects = await window.electronAPI?.getRecentProjects();
      setRecentProjects(projects || []);
    } catch (err) {
      console.error("Failed to load recent projects:", err);
    }
    setLoading(false);
  };

  const handleOpenProject = async () => {
    const result = await window.electronAPI?.openFolderDialog({
      title: "Open Project",
    });

    if (result && !result.canceled && result.filePaths[0]) {
      const projectPath = result.filePaths[0];
      await openProjectAtPath(projectPath);
    }
  };

  const openProjectAtPath = async (projectPath: string) => {
    const openResult = await window.electronAPI?.openProject(projectPath);

    if (openResult?.success) {
      setProject(projectPath, openResult.data.config);
      // Add to recent projects
      await window.electronAPI?.addRecentProject(
        projectPath,
        openResult.data.config.name
      );
    } else {
      messageBus.emit("status:update", {
        text: `Failed to open project: ${openResult?.error}`,
      });
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
        // Add to recent projects
        await window.electronAPI?.addRecentProject(projectPath, newProjectName);
        setIsCreating(false);
        setNewProjectName("");
      } else {
        messageBus.emit("status:update", {
          text: `Failed to create project: ${createResult?.error}`,
        });
      }
    }
  };

  const handleRemoveRecent = async (e: React.MouseEvent, projectPath: string) => {
    e.stopPropagation();
    const updated = await window.electronAPI?.removeRecentProject(projectPath);
    setRecentProjects(updated || []);
  };

  const formatLastOpened = (dateString: string) => {
    const date = new Date(dateString);
    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffDays = Math.floor(diffMs / (1000 * 60 * 60 * 24));

    if (diffDays === 0) {
      return "Today";
    } else if (diffDays === 1) {
      return "Yesterday";
    } else if (diffDays < 7) {
      return `${diffDays} days ago`;
    } else {
      return date.toLocaleDateString();
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

        <div className="welcome-main">
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

          {/* Recent Projects */}
          {!isCreating && recentProjects.length > 0 && (
            <div className="recent-projects">
              <h3 className="recent-title">Recent Projects</h3>
              <div className="recent-list">
                {recentProjects.map((project) => (
                  <button
                    key={project.path}
                    className="recent-item"
                    onClick={() => openProjectAtPath(project.path)}
                    title={project.path}
                  >
                    <span className="recent-icon">📁</span>
                    <span className="recent-info">
                      <span className="recent-name">{project.name}</span>
                      <span className="recent-path">{project.path}</span>
                    </span>
                    <span className="recent-date">
                      {formatLastOpened(project.lastOpened)}
                    </span>
                    <button
                      className="recent-remove"
                      onClick={(e) => handleRemoveRecent(e, project.path)}
                      title="Remove from recent"
                    >
                      ×
                    </button>
                  </button>
                ))}
              </div>
            </div>
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
