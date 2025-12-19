import React, { useState, useEffect, useRef, useCallback } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useProject, useFiles } from "@/hooks/useStore";
import { useMessageBus, useEmit } from "@/hooks/useMessageBus";
import { FileNode } from "@shared/types";
import { ContextMenu, ContextMenuItem, useContextMenu } from "@/components/ContextMenu";
import "./ProjectExplorer.css";

interface InputState {
  type: "new-file" | "new-folder" | "rename";
  parentPath: string;
  originalName?: string;
}

export function ProjectExplorer({ panelId, instanceId }: PanelProps) {
  const { path: projectPath, isOpen } = useProject();
  const { openFile, currentFile, closeFile } = useFiles();
  const [tree, setTree] = useState<FileNode | null>(null);
  const [expanded, setExpanded] = useState<Set<string>>(new Set([""]));
  const contextMenu = useContextMenu<FileNode>();
  const [inputState, setInputState] = useState<InputState | null>(null);
  const [inputValue, setInputValue] = useState("");
  const inputRef = useRef<HTMLInputElement>(null);
  const emit = useEmit();

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

  // Listen for external file changes from the file watcher
  useEffect(() => {
    if (!projectPath) return;

    const unsubscribe = window.electronAPI?.on(
      "file:external-change",
      (data: { event: string; path: string; relativePath: string }) => {
        console.log("External file change:", data);
        // Refresh the tree on any external change
        loadFileTree(projectPath);
        // Emit to notify other components
        emit("file:external-change", data);
      }
    );

    return () => {
      unsubscribe?.();
    };
  }, [projectPath, emit]);

  // Focus input when it appears
  useEffect(() => {
    if (inputState && inputRef.current) {
      inputRef.current.focus();
      inputRef.current.select();
    }
  }, [inputState]);


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

  const handleNewFile = useCallback((parentPath: string) => {
    contextMenu.close();
    setInputState({ type: "new-file", parentPath });
    setInputValue("untitled.sl");
    // Expand the parent
    setExpanded((prev) => new Set(prev).add(parentPath));
  }, [contextMenu]);

  const handleNewFolder = useCallback((parentPath: string) => {
    contextMenu.close();
    setInputState({ type: "new-folder", parentPath });
    setInputValue("new-folder");
    // Expand the parent
    setExpanded((prev) => new Set(prev).add(parentPath));
  }, [contextMenu]);

  const handleRename = useCallback((node: FileNode) => {
    contextMenu.close();
    const parentPath = node.path.substring(0, node.path.lastIndexOf("/"));
    setInputState({ type: "rename", parentPath, originalName: node.name });
    setInputValue(node.name);
  }, [contextMenu]);

  const handleDelete = useCallback(async (node: FileNode) => {
    contextMenu.close();

    const result = await window.electronAPI?.showMessage({
      type: "warning",
      message: `Delete "${node.name}"?`,
      detail: node.type === "directory"
        ? "This will delete the folder and all its contents."
        : "This file will be permanently deleted.",
      buttons: ["Delete", "Cancel"],
      defaultId: 1,
      cancelId: 1,
    });

    if (result?.response === 0) {
      const deleteResult = await window.electronAPI?.deleteFile(node.path);
      if (deleteResult?.success) {
        // Close file if it was open
        if (node.type === "file") {
          closeFile(node.path);
        }
        emit("file:changed", {});
        emit("status:update", { text: `Deleted ${node.name}` });
      } else {
        emit("console:error", { text: `Failed to delete: ${deleteResult?.error}` });
      }
    }
  }, [contextMenu, closeFile, emit]);

  // Build context menu items based on the selected node
  const getContextMenuItems = useCallback((node: FileNode): ContextMenuItem[] => {
    const items: ContextMenuItem[] = [];

    if (node.type === "directory") {
      items.push(
        { label: "New File", icon: "📄", onClick: () => handleNewFile(node.path) },
        { label: "New Folder", icon: "📁", onClick: () => handleNewFolder(node.path) },
        { type: "separator", label: "" }
      );
    }

    items.push(
      { label: "Rename", icon: "✏️", onClick: () => handleRename(node) },
      { label: "Delete", icon: "🗑️", danger: true, onClick: () => handleDelete(node) }
    );

    return items;
  }, [handleNewFile, handleNewFolder, handleRename, handleDelete]);

  const handleInputSubmit = async () => {
    if (!inputState || !inputValue.trim()) {
      setInputState(null);
      return;
    }

    const newPath = `${inputState.parentPath}/${inputValue.trim()}`;

    try {
      if (inputState.type === "new-file") {
        const result = await window.electronAPI?.createFile(newPath);
        if (result?.success) {
          emit("file:changed", {});
          openFile(newPath);
        } else {
          emit("console:error", { text: `Failed to create file: ${result?.error}` });
        }
      } else if (inputState.type === "new-folder") {
        const result = await window.electronAPI?.mkdir(newPath);
        if (result?.success) {
          emit("file:changed", {});
        } else {
          emit("console:error", { text: `Failed to create folder: ${result?.error}` });
        }
      } else if (inputState.type === "rename" && inputState.originalName) {
        const oldPath = `${inputState.parentPath}/${inputState.originalName}`;
        const result = await window.electronAPI?.renameFile(oldPath, newPath);
        if (result?.success) {
          emit("file:changed", {});
          // Update current file if it was renamed
          if (currentFile === oldPath) {
            closeFile(oldPath);
            openFile(newPath);
          }
        } else {
          emit("console:error", { text: `Failed to rename: ${result?.error}` });
        }
      }
    } catch (err) {
      emit("console:error", { text: `Error: ${err}` });
    }

    setInputState(null);
    setInputValue("");
  };

  const handleInputKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      handleInputSubmit();
    } else if (e.key === "Escape") {
      setInputState(null);
      setInputValue("");
    }
  };

  const renderNode = (node: FileNode, depth: number = 0): React.ReactNode => {
    const isExpanded = expanded.has(node.path);
    const isSelected = currentFile === node.path;
    const isDir = node.type === "directory";

    // Check if we need to show input for this node's children
    const showInput =
      inputState &&
      inputState.parentPath === node.path &&
      (inputState.type === "new-file" || inputState.type === "new-folder");

    // Check if this node is being renamed
    const isRenaming =
      inputState &&
      inputState.type === "rename" &&
      inputState.originalName === node.name &&
      inputState.parentPath === node.path.substring(0, node.path.lastIndexOf("/"));

    return (
      <div key={node.path}>
        <div
          className={`tree-item ${isSelected ? "selected" : ""}`}
          style={{ paddingLeft: depth * 16 + 8 }}
          onClick={() => handleFileClick(node)}
          onContextMenu={(e) => contextMenu.handleContextMenu(e, node)}
        >
          <span className="tree-icon">
            {isDir ? (isExpanded ? "📂" : "📁") : getFileIcon(node.name)}
          </span>
          {isRenaming ? (
            <input
              ref={inputRef}
              className="tree-input"
              value={inputValue}
              onChange={(e) => setInputValue(e.target.value)}
              onKeyDown={handleInputKeyDown}
              onBlur={handleInputSubmit}
              onClick={(e) => e.stopPropagation()}
            />
          ) : (
            <span className="tree-name">{node.name}</span>
          )}
        </div>
        {isDir && isExpanded && (
          <div className="tree-children">
            {showInput && (
              <div
                className="tree-item tree-input-item"
                style={{ paddingLeft: (depth + 1) * 16 + 8 }}
              >
                <span className="tree-icon">
                  {inputState.type === "new-file" ? "📄" : "📁"}
                </span>
                <input
                  ref={inputRef}
                  className="tree-input"
                  value={inputValue}
                  onChange={(e) => setInputValue(e.target.value)}
                  onKeyDown={handleInputKeyDown}
                  onBlur={handleInputSubmit}
                />
              </div>
            )}
            {node.children?.map((child) => renderNode(child, depth + 1))}
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
      <div className="explorer-toolbar">
        <button
          className="toolbar-btn"
          onClick={() => projectPath && handleNewFile(projectPath)}
          title="New File"
        >
          📄+
        </button>
        <button
          className="toolbar-btn"
          onClick={() => projectPath && handleNewFolder(projectPath)}
          title="New Folder"
        >
          📁+
        </button>
        <button
          className="toolbar-btn"
          onClick={() => projectPath && loadFileTree(projectPath)}
          title="Refresh"
        >
          🔄
        </button>
      </div>
      <div className="explorer-tree">
        {tree && renderNode(tree)}
      </div>

      {/* Context Menu */}
      {contextMenu.isOpen && contextMenu.data && (
        <ContextMenu
          x={contextMenu.x}
          y={contextMenu.y}
          items={getContextMenuItems(contextMenu.data)}
          onClose={contextMenu.close}
        />
      )}
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
