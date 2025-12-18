import React, { useState, useEffect } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useProject, useFiles } from "@/hooks/useStore";
import { useMessageBus } from "@/hooks/useMessageBus";
import { FileNode } from "@shared/types";
import "./ProjectExplorer.css";

export function ProjectExplorer({ panelId, instanceId }: PanelProps) {
  const { path: projectPath, isOpen } = useProject();
  const { openFile, currentFile } = useFiles();
  const [tree, setTree] = useState<FileNode | null>(null);
  const [expanded, setExpanded] = useState<Set<string>>(new Set([""]));

  // Load file tree when project opens
  useEffect(() => {
    if (projectPath) {
      loadFileTree(projectPath);
    } else {
      setTree(null);
    }
  }, [projectPath]);

  // Refresh tree on file changes
  useMessageBus("file:changed", () => {
    if (projectPath) {
      loadFileTree(projectPath);
    }
  });

  const loadFileTree = async (basePath: string) => {
    const buildTree = async (dirPath: string, name: string): Promise<FileNode> => {
      const result = await window.electronAPI?.listFiles(dirPath);
      const children: FileNode[] = [];

      if (result?.success && result.data) {
        for (const entry of result.data) {
          if (entry.type === "directory") {
            children.push(await buildTree(entry.path, entry.name));
          } else {
            children.push({
              name: entry.name,
              path: entry.path,
              type: "file",
            });
          }
        }
      }

      // Sort: directories first, then alphabetically
      children.sort((a, b) => {
        if (a.type !== b.type) {
          return a.type === "directory" ? -1 : 1;
        }
        return a.name.localeCompare(b.name);
      });

      return {
        name,
        path: dirPath,
        type: "directory",
        children,
      };
    };

    const rootName = basePath.split("/").pop() || "Project";
    const tree = await buildTree(basePath, rootName);
    setTree(tree);
    // Expand root by default
    setExpanded(new Set([basePath]));
  };

  const toggleExpanded = (path: string) => {
    setExpanded((prev) => {
      const next = new Set(prev);
      if (next.has(path)) {
        next.delete(path);
      } else {
        next.add(path);
      }
      return next;
    });
  };

  const handleFileClick = (node: FileNode) => {
    if (node.type === "directory") {
      toggleExpanded(node.path);
    } else {
      openFile(node.path);
    }
  };

  const renderNode = (node: FileNode, depth: number = 0): React.ReactNode => {
    const isExpanded = expanded.has(node.path);
    const isSelected = currentFile === node.path;
    const isDir = node.type === "directory";

    return (
      <div key={node.path}>
        <div
          className={`tree-item ${isSelected ? "selected" : ""}`}
          style={{ paddingLeft: depth * 16 + 8 }}
          onClick={() => handleFileClick(node)}
        >
          <span className="tree-icon">
            {isDir ? (isExpanded ? "📂" : "📁") : getFileIcon(node.name)}
          </span>
          <span className="tree-name">{node.name}</span>
        </div>
        {isDir && isExpanded && node.children && (
          <div className="tree-children">
            {node.children.map((child) => renderNode(child, depth + 1))}
          </div>
        )}
      </div>
    );
  };

  if (!isOpen) {
    return (
      <div className="project-explorer empty">
        <p>No project open</p>
      </div>
    );
  }

  return (
    <div className="project-explorer">
      {tree && renderNode(tree)}
    </div>
  );
}

// Get icon based on file extension
function getFileIcon(filename: string): string {
  const ext = filename.split(".").pop()?.toLowerCase();
  switch (ext) {
    case "sl":
      return "📜";
    case "json":
      return "📋";
    case "md":
      return "📄";
    case "png":
    case "jpg":
    case "jpeg":
    case "gif":
      return "🖼️";
    case "glsl":
    case "frag":
    case "vert":
      return "🎨";
    default:
      return "📄";
  }
}
