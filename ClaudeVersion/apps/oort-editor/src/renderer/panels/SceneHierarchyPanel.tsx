import React, { useState, useCallback, useEffect, useRef } from "react";
import { PanelProps } from "@/core/panel-registry";
import { messageBus } from "@/core/message-bus";
import { threeService } from "@/core/three-service";
import { useMessageBus } from "@/hooks/useMessageBus";
import type { EntityNode } from "@shared/types";
import "./SceneHierarchyPanel.css";

interface EntityTreeItem {
  id: string;
  name: string;
  children: EntityTreeItem[];
  expanded: boolean;
  visible: boolean;
  parentId?: string;
}

interface ContextMenuState {
  visible: boolean;
  x: number;
  y: number;
  entityId: string | null;
}

export function SceneHierarchyPanel({ panelId, instanceId }: PanelProps) {
  const [entities, setEntities] = useState<EntityTreeItem[]>([]);
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [searchQuery, setSearchQuery] = useState("");
  const [editingId, setEditingId] = useState<string | null>(null);
  const [editingName, setEditingName] = useState("");
  const [contextMenu, setContextMenu] = useState<ContextMenuState>({
    visible: false,
    x: 0,
    y: 0,
    entityId: null,
  });
  const inputRef = useRef<HTMLInputElement>(null);

  // Get active viewport ID (for now, use first available)
  const getActiveViewport = useCallback(() => {
    const viewports = threeService.getActiveViewports();
    return viewports[0] || null;
  }, []);

  // Sync entities from ThreeService
  const syncEntities = useCallback(() => {
    const viewportId = getActiveViewport();
    if (!viewportId) {
      setEntities([]);
      return;
    }

    const state = threeService.getScene(viewportId);
    if (!state) {
      setEntities([]);
      return;
    }

    // Build entity tree from Three.js entities
    const entityMap = new Map<string, EntityTreeItem>();
    const rootEntities: EntityTreeItem[] = [];

    // First pass: create all entity items
    for (const [id, object] of state.entities) {
      const item: EntityTreeItem = {
        id,
        name: object.name || id,
        children: [],
        expanded: true,
        visible: object.visible,
        parentId: undefined,
      };
      entityMap.set(id, item);
    }

    // Second pass: build hierarchy
    for (const [id, object] of state.entities) {
      const item = entityMap.get(id)!;
      const parentEntity = object.parent?.userData?.entityId;

      if (parentEntity && entityMap.has(parentEntity)) {
        item.parentId = parentEntity;
        entityMap.get(parentEntity)!.children.push(item);
      } else {
        rootEntities.push(item);
      }
    }

    setEntities(rootEntities);
  }, [getActiveViewport]);

  // Initial sync and listen for entity changes
  useEffect(() => {
    syncEntities();
  }, [syncEntities]);

  useMessageBus("entity:spawned", syncEntities);
  useMessageBus("entity:removed", syncEntities);
  useMessageBus("scene3d:created", syncEntities);

  // Handle entity selection
  const handleSelect = useCallback((entityId: string) => {
    setSelectedId(entityId);
    messageBus.emit("entity:selected", { entityId });
    messageBus.emit("selection:changed", {
      target: {
        type: "Entity",
        name: entityId,
        properties: getEntityProperties(entityId),
      },
    });
  }, []);

  // Get entity properties for inspector
  const getEntityProperties = (entityId: string): Record<string, any> => {
    const viewportId = getActiveViewport();
    if (!viewportId) return {};

    const entity = threeService.getEntity(viewportId, entityId);
    if (!entity) return {};

    return {
      position: `(${entity.position.x.toFixed(2)}, ${entity.position.y.toFixed(2)}, ${entity.position.z.toFixed(2)})`,
      rotation: `(${(entity.rotation.x * 180 / Math.PI).toFixed(1)}°, ${(entity.rotation.y * 180 / Math.PI).toFixed(1)}°, ${(entity.rotation.z * 180 / Math.PI).toFixed(1)}°)`,
      scale: `(${entity.scale.x.toFixed(2)}, ${entity.scale.y.toFixed(2)}, ${entity.scale.z.toFixed(2)})`,
      visible: entity.visible,
      children: entity.children.length,
    };
  };

  // Toggle entity expansion
  const toggleExpand = useCallback((entityId: string, e: React.MouseEvent) => {
    e.stopPropagation();
    setEntities((prev) =>
      toggleEntityExpanded(prev, entityId)
    );
  }, []);

  // Toggle entity visibility
  const toggleVisibility = useCallback((entityId: string, e: React.MouseEvent) => {
    e.stopPropagation();
    const viewportId = getActiveViewport();
    if (!viewportId) return;

    const entity = threeService.getEntity(viewportId, entityId);
    if (entity) {
      entity.visible = !entity.visible;
      syncEntities();
    }
  }, [getActiveViewport, syncEntities]);

  // Start renaming
  const startRename = useCallback((entityId: string) => {
    const viewportId = getActiveViewport();
    if (!viewportId) return;

    const entity = threeService.getEntity(viewportId, entityId);
    if (entity) {
      setEditingId(entityId);
      setEditingName(entity.name || entityId);
      setTimeout(() => inputRef.current?.select(), 0);
    }
  }, [getActiveViewport]);

  // Finish renaming
  const finishRename = useCallback(() => {
    if (!editingId || !editingName.trim()) {
      setEditingId(null);
      return;
    }

    const viewportId = getActiveViewport();
    if (viewportId) {
      const entity = threeService.getEntity(viewportId, editingId);
      if (entity) {
        entity.name = editingName.trim();
        messageBus.emit("entity:renamed", { entityId: editingId, name: editingName.trim() });
      }
    }

    setEditingId(null);
    syncEntities();
  }, [editingId, editingName, getActiveViewport, syncEntities]);

  // Handle key events during rename
  const handleRenameKeyDown = useCallback((e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      finishRename();
    } else if (e.key === "Escape") {
      setEditingId(null);
    }
  }, [finishRename]);

  // Context menu handlers
  const handleContextMenu = useCallback((e: React.MouseEvent, entityId: string | null) => {
    e.preventDefault();
    setContextMenu({
      visible: true,
      x: e.clientX,
      y: e.clientY,
      entityId,
    });
  }, []);

  const closeContextMenu = useCallback(() => {
    setContextMenu((prev) => ({ ...prev, visible: false }));
  }, []);

  // Close context menu on click outside
  useEffect(() => {
    const handleClick = () => closeContextMenu();
    if (contextMenu.visible) {
      document.addEventListener("click", handleClick);
      return () => document.removeEventListener("click", handleClick);
    }
  }, [contextMenu.visible, closeContextMenu]);

  // Add new entity
  const addEntity = useCallback(async (type: "empty" | "box" | "sphere" | "cylinder") => {
    const viewportId = getActiveViewport();
    if (!viewportId) return;

    const id = `entity_${Date.now()}`;

    if (type === "empty") {
      await threeService.spawnEntity(viewportId, {
        id,
        position: [0, 0, 0],
      });
    } else {
      threeService.addPrimitive(viewportId, type, {
        position: [(Math.random() - 0.5) * 4, 0.5, (Math.random() - 0.5) * 4],
      });
    }

    closeContextMenu();
    syncEntities();
  }, [getActiveViewport, closeContextMenu, syncEntities]);

  // Delete entity
  const deleteEntity = useCallback((entityId: string) => {
    const viewportId = getActiveViewport();
    if (!viewportId) return;

    threeService.removeEntity(viewportId, entityId);
    if (selectedId === entityId) {
      setSelectedId(null);
    }
    closeContextMenu();
    syncEntities();
  }, [getActiveViewport, selectedId, closeContextMenu, syncEntities]);

  // Duplicate entity
  const duplicateEntity = useCallback(async (entityId: string) => {
    const viewportId = getActiveViewport();
    if (!viewportId) return;

    const source = threeService.getEntity(viewportId, entityId);
    if (!source) return;

    const newId = `${entityId}_copy_${Date.now()}`;
    await threeService.spawnEntity(viewportId, {
      id: newId,
      position: [
        source.position.x + 1,
        source.position.y,
        source.position.z,
      ],
      rotation: [
        source.rotation.x * 180 / Math.PI,
        source.rotation.y * 180 / Math.PI,
        source.rotation.z * 180 / Math.PI,
      ],
      scale: [source.scale.x, source.scale.y, source.scale.z],
    });

    closeContextMenu();
    syncEntities();
  }, [getActiveViewport, closeContextMenu, syncEntities]);

  // Filter entities by search
  const filteredEntities = searchQuery
    ? filterEntities(entities, searchQuery.toLowerCase())
    : entities;

  // Render entity tree node
  const renderEntity = (entity: EntityTreeItem, depth: number = 0): React.ReactNode => {
    const hasChildren = entity.children.length > 0;
    const isSelected = selectedId === entity.id;
    const isEditing = editingId === entity.id;

    return (
      <div key={entity.id} className="entity-node" style={{ marginLeft: depth > 0 ? 0 : undefined }}>
        <div
          className={`entity-row ${isSelected ? "selected" : ""}`}
          onClick={() => handleSelect(entity.id)}
          onDoubleClick={() => startRename(entity.id)}
          onContextMenu={(e) => handleContextMenu(e, entity.id)}
        >
          <span
            className={`entity-expand ${hasChildren ? (entity.expanded ? "expanded" : "") : "placeholder"}`}
            onClick={(e) => hasChildren && toggleExpand(entity.id, e)}
          >
            {hasChildren ? "▶" : ""}
          </span>
          <span className="entity-icon">
            {getEntityIcon(entity)}
          </span>
          {isEditing ? (
            <input
              ref={inputRef}
              className="entity-name-input"
              value={editingName}
              onChange={(e) => setEditingName(e.target.value)}
              onBlur={finishRename}
              onKeyDown={handleRenameKeyDown}
              autoFocus
            />
          ) : (
            <span className="entity-name">{entity.name}</span>
          )}
          <span
            className={`entity-visibility ${!entity.visible ? "hidden" : ""}`}
            onClick={(e) => toggleVisibility(entity.id, e)}
            title={entity.visible ? "Hide" : "Show"}
          >
            {entity.visible ? "👁" : "👁‍🗨"}
          </span>
        </div>
        {hasChildren && entity.expanded && (
          <div className="entity-children">
            {entity.children.map((child) => renderEntity(child, depth + 1))}
          </div>
        )}
      </div>
    );
  };

  return (
    <div className="scene-hierarchy-panel">
      <div className="hierarchy-toolbar">
        <button
          className="hierarchy-toolbar-btn"
          onClick={() => addEntity("empty")}
          title="Add Empty Entity"
        >
          +
        </button>
        <input
          type="text"
          className="hierarchy-search"
          placeholder="Search entities..."
          value={searchQuery}
          onChange={(e) => setSearchQuery(e.target.value)}
        />
      </div>

      <div
        className="hierarchy-content"
        onContextMenu={(e) => handleContextMenu(e, null)}
      >
        {filteredEntities.length === 0 ? (
          <div className="hierarchy-empty">
            <p>No entities in scene</p>
            <p className="hint">Right-click to add entities</p>
          </div>
        ) : (
          <div className="hierarchy-tree">
            {filteredEntities.map((entity) => renderEntity(entity))}
          </div>
        )}
      </div>

      {/* Context Menu */}
      {contextMenu.visible && (
        <div
          className="hierarchy-context-menu"
          style={{ left: contextMenu.x, top: contextMenu.y }}
        >
          <div className="context-menu-item" onClick={() => addEntity("empty")}>
            <span>➕</span> Add Empty
          </div>
          <div className="context-menu-item" onClick={() => addEntity("box")}>
            <span>📦</span> Add Box
          </div>
          <div className="context-menu-item" onClick={() => addEntity("sphere")}>
            <span>🔮</span> Add Sphere
          </div>
          <div className="context-menu-item" onClick={() => addEntity("cylinder")}>
            <span>🛢</span> Add Cylinder
          </div>
          {contextMenu.entityId && (
            <>
              <div className="context-menu-separator" />
              <div className="context-menu-item" onClick={() => startRename(contextMenu.entityId!)}>
                <span>✏️</span> Rename
                <span className="context-menu-shortcut">F2</span>
              </div>
              <div className="context-menu-item" onClick={() => duplicateEntity(contextMenu.entityId!)}>
                <span>📋</span> Duplicate
                <span className="context-menu-shortcut">Ctrl+D</span>
              </div>
              <div className="context-menu-separator" />
              <div
                className="context-menu-item danger"
                onClick={() => deleteEntity(contextMenu.entityId!)}
              >
                <span>🗑</span> Delete
                <span className="context-menu-shortcut">Del</span>
              </div>
            </>
          )}
        </div>
      )}
    </div>
  );
}

// Helper functions

function toggleEntityExpanded(entities: EntityTreeItem[], targetId: string): EntityTreeItem[] {
  return entities.map((entity) => {
    if (entity.id === targetId) {
      return { ...entity, expanded: !entity.expanded };
    }
    if (entity.children.length > 0) {
      return {
        ...entity,
        children: toggleEntityExpanded(entity.children, targetId),
      };
    }
    return entity;
  });
}

function filterEntities(entities: EntityTreeItem[], query: string): EntityTreeItem[] {
  const result: EntityTreeItem[] = [];

  for (const entity of entities) {
    const nameMatches = entity.name.toLowerCase().includes(query);
    const filteredChildren = filterEntities(entity.children, query);

    if (nameMatches || filteredChildren.length > 0) {
      result.push({
        ...entity,
        expanded: true,
        children: filteredChildren,
      });
    }
  }

  return result;
}

function getEntityIcon(entity: EntityTreeItem): string {
  // Could be enhanced to show different icons based on entity type
  if (entity.children.length > 0) {
    return "📁";
  }
  return "🎮";
}
