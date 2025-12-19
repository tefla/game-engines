/**
 * Asset Browser Panel - Grid view of project assets with thumbnails
 */

import React, { useState, useEffect, useCallback } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useProject, useFiles } from "@/hooks/useStore";
import { useMessageBus, useEmit } from "@/hooks/useMessageBus";
import { extensionManager } from "@/core/extension-manager";
import { ContextMenu, ContextMenuItem, useContextMenu } from "@/components/ContextMenu";
import "./AssetBrowser.css";

interface Asset {
  name: string;
  path: string;
  type: "file" | "directory";
  extension: string;
  icon: string;
  thumbnail?: string;
}

type ViewMode = "grid" | "list";
type SortBy = "name" | "type" | "modified";

export function AssetBrowser({ panelId, instanceId }: PanelProps) {
  const { path: projectPath, isOpen } = useProject();
  const { openFile } = useFiles();
  const [assets, setAssets] = useState<Asset[]>([]);
  const [currentPath, setCurrentPath] = useState<string>("");
  const [viewMode, setViewMode] = useState<ViewMode>("grid");
  const [sortBy, setSortBy] = useState<SortBy>("name");
  const [filter, setFilter] = useState("");
  const [selected, setSelected] = useState<Set<string>>(new Set());
  const contextMenu = useContextMenu<Asset>();
  const emit = useEmit();

  // Initialize current path when project opens
  useEffect(() => {
    if (projectPath && isOpen) {
      setCurrentPath(projectPath);
    } else {
      setCurrentPath("");
      setAssets([]);
    }
  }, [projectPath, isOpen]);

  // Load assets when path changes
  useEffect(() => {
    if (currentPath) {
      loadAssets(currentPath);
    }
  }, [currentPath]);

  // Refresh on file changes
  useMessageBus("file:changed", () => {
    if (currentPath) {
      loadAssets(currentPath);
    }
  });

  useMessageBus("file:external-change", () => {
    if (currentPath) {
      loadAssets(currentPath);
    }
  });

  const loadAssets = async (dirPath: string) => {
    const result = await window.electronAPI?.listFiles(dirPath);
    if (!result?.success || !result.data) {
      setAssets([]);
      return;
    }

    const assetList: Asset[] = result.data.map((entry: any) => {
      const ext = entry.name.includes(".")
        ? `.${entry.name.split(".").pop()?.toLowerCase()}`
        : "";

      return {
        name: entry.name,
        path: entry.path,
        type: entry.type,
        extension: ext,
        icon: getAssetIcon(entry.name, entry.type),
        thumbnail: getThumbnail(entry.name, ext),
      };
    });

    // Sort assets
    assetList.sort((a, b) => {
      // Directories first
      if (a.type !== b.type) {
        return a.type === "directory" ? -1 : 1;
      }

      switch (sortBy) {
        case "type":
          return a.extension.localeCompare(b.extension) || a.name.localeCompare(b.name);
        case "name":
        default:
          return a.name.localeCompare(b.name);
      }
    });

    setAssets(assetList);
    setSelected(new Set());
  };

  const getAssetIcon = (name: string, type: string): string => {
    if (type === "directory") return "📁";

    const ext = name.split(".").pop()?.toLowerCase();

    // Check extension manager for registered asset types
    const handler = extensionManager.getAssetHandler(`.${ext}`);
    if (handler) return handler.icon;

    // Default icons
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
      case "webp":
        return "🖼️";
      case "mp3":
      case "wav":
      case "ogg":
        return "🔊";
      case "mp4":
      case "webm":
        return "🎬";
      case "glsl":
      case "frag":
      case "vert":
        return "🎨";
      case "ttf":
      case "otf":
      case "woff":
        return "🔤";
      default:
        return "📄";
    }
  };

  const getThumbnail = (name: string, ext: string): string | undefined => {
    // For images, we could generate thumbnails
    // For now, return undefined (will use icon)
    const imageExts = [".png", ".jpg", ".jpeg", ".gif", ".webp"];
    if (imageExts.includes(ext.toLowerCase())) {
      // Return the file path as thumbnail for images
      // In a real implementation, we'd generate actual thumbnails
      return undefined; // For now, use icon
    }
    return undefined;
  };

  const handleAssetClick = (asset: Asset, event: React.MouseEvent) => {
    if (event.ctrlKey || event.metaKey) {
      // Multi-select
      setSelected((prev) => {
        const next = new Set(prev);
        if (next.has(asset.path)) {
          next.delete(asset.path);
        } else {
          next.add(asset.path);
        }
        return next;
      });
    } else {
      setSelected(new Set([asset.path]));
    }
  };

  const handleAssetDoubleClick = (asset: Asset) => {
    if (asset.type === "directory") {
      setCurrentPath(asset.path);
    } else {
      openFile(asset.path);
    }
  };

  const navigateUp = () => {
    if (!currentPath || currentPath === projectPath) return;
    const parentPath = currentPath.substring(0, currentPath.lastIndexOf("/"));
    if (parentPath.startsWith(projectPath!)) {
      setCurrentPath(parentPath);
    }
  };

  const navigateToPath = (path: string) => {
    if (path.startsWith(projectPath!)) {
      setCurrentPath(path);
    }
  };

  const getBreadcrumbs = (): { name: string; path: string }[] => {
    if (!currentPath || !projectPath) return [];

    const relativePath = currentPath.substring(projectPath.length);
    const parts = relativePath.split("/").filter(Boolean);

    const breadcrumbs: { name: string; path: string }[] = [
      { name: projectPath.split("/").pop() || "Project", path: projectPath },
    ];

    let buildPath = projectPath;
    for (const part of parts) {
      buildPath = `${buildPath}/${part}`;
      breadcrumbs.push({ name: part, path: buildPath });
    }

    return breadcrumbs;
  };

  const filteredAssets = assets.filter((asset) =>
    asset.name.toLowerCase().includes(filter.toLowerCase())
  );

  const getContextMenuItems = useCallback((asset: Asset): ContextMenuItem[] => {
    const items: ContextMenuItem[] = [];

    if (asset.type === "directory") {
      items.push({
        label: "Open Folder",
        icon: "📂",
        onClick: () => setCurrentPath(asset.path),
      });
    } else {
      items.push({
        label: "Open",
        icon: "📝",
        onClick: () => openFile(asset.path),
      });
    }

    items.push(
      { type: "separator", label: "" },
      {
        label: "Rename",
        icon: "✏️",
        onClick: () => emit("asset:rename", { path: asset.path }),
      },
      {
        label: "Delete",
        icon: "🗑️",
        danger: true,
        onClick: () => handleDelete(asset),
      }
    );

    return items;
  }, [openFile, emit]);

  const handleDelete = async (asset: Asset) => {
    const result = await window.electronAPI?.showMessage({
      type: "warning",
      message: `Delete "${asset.name}"?`,
      detail:
        asset.type === "directory"
          ? "This will delete the folder and all its contents."
          : "This file will be permanently deleted.",
      buttons: ["Delete", "Cancel"],
      defaultId: 1,
      cancelId: 1,
    });

    if (result?.response === 0) {
      const deleteResult = await window.electronAPI?.deleteFile(asset.path);
      if (deleteResult?.success) {
        emit("file:changed", {});
        emit("status:update", { text: `Deleted ${asset.name}` });
      } else {
        emit("console:error", { text: `Failed to delete: ${deleteResult?.error}` });
      }
    }
  };

  if (!isOpen) {
    return (
      <div className="asset-browser empty">
        <p>No project open</p>
      </div>
    );
  }

  return (
    <div className="asset-browser">
      {/* Toolbar */}
      <div className="asset-toolbar">
        <button
          className="toolbar-btn"
          onClick={navigateUp}
          disabled={currentPath === projectPath}
          title="Go Up"
        >
          ⬆️
        </button>
        <div className="breadcrumb-nav">
          {getBreadcrumbs().map((crumb, index, arr) => (
            <React.Fragment key={crumb.path}>
              <button
                className="breadcrumb-item"
                onClick={() => navigateToPath(crumb.path)}
              >
                {crumb.name}
              </button>
              {index < arr.length - 1 && <span className="breadcrumb-sep">/</span>}
            </React.Fragment>
          ))}
        </div>
        <div className="toolbar-spacer" />
        <input
          type="text"
          className="filter-input"
          placeholder="Filter..."
          value={filter}
          onChange={(e) => setFilter(e.target.value)}
        />
        <div className="view-toggle">
          <button
            className={`view-btn ${viewMode === "grid" ? "active" : ""}`}
            onClick={() => setViewMode("grid")}
            title="Grid View"
          >
            ▦
          </button>
          <button
            className={`view-btn ${viewMode === "list" ? "active" : ""}`}
            onClick={() => setViewMode("list")}
            title="List View"
          >
            ☰
          </button>
        </div>
      </div>

      {/* Asset Grid/List */}
      <div className={`asset-content ${viewMode}`}>
        {filteredAssets.length === 0 ? (
          <div className="asset-empty">
            {filter ? "No matching assets" : "This folder is empty"}
          </div>
        ) : (
          filteredAssets.map((asset) => (
            <div
              key={asset.path}
              className={`asset-item ${selected.has(asset.path) ? "selected" : ""}`}
              onClick={(e) => handleAssetClick(asset, e)}
              onDoubleClick={() => handleAssetDoubleClick(asset)}
              onContextMenu={(e) => contextMenu.handleContextMenu(e, asset)}
            >
              <div className="asset-icon">
                {asset.thumbnail ? (
                  <img src={asset.thumbnail} alt={asset.name} />
                ) : (
                  <span className="icon-emoji">{asset.icon}</span>
                )}
              </div>
              <div className="asset-name" title={asset.name}>
                {asset.name}
              </div>
            </div>
          ))
        )}
      </div>

      {/* Status bar */}
      <div className="asset-status">
        {filteredAssets.length} items
        {selected.size > 0 && ` • ${selected.size} selected`}
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
