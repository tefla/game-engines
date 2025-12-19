import React, { useState, useCallback, useRef, useEffect } from "react";
import { PanelContainer } from "./PanelContainer";
import { panelRegistry, PanelInstance } from "@/core/panel-registry";
import { useMessageBus } from "@/hooks/useMessageBus";
import { Channels } from "@/core/message-bus";
import "./DockingLayout.css";

// Layout node types
export type LayoutNodeType = "row" | "column" | "tabs" | "panel";

export interface LayoutNode {
  id: string;
  type: LayoutNodeType;
  children?: LayoutNode[];
  instanceId?: string; // For panel nodes
  activeTab?: number; // For tabs nodes
  size?: number; // Flex ratio (default 1)
}

interface DockingLayoutProps {
  layout: LayoutNode;
  onLayoutChange?: (layout: LayoutNode) => void;
}

// Drag state
interface DragState {
  instanceId: string;
  sourceNodeId: string;
  title: string;
}

type DropZone = "left" | "right" | "top" | "bottom" | "center";

// Generate unique IDs
let nodeIdCounter = 0;
export function generateNodeId(): string {
  return `node-${++nodeIdCounter}`;
}

// Create initial layout with panel instances
export function createDefaultLayout(): LayoutNode {
  return {
    id: generateNodeId(),
    type: "row",
    children: [
      {
        id: generateNodeId(),
        type: "panel",
        instanceId: panelRegistry.createInstance("project-explorer")?.id,
        size: 1,
      },
      {
        id: generateNodeId(),
        type: "column",
        size: 3,
        children: [
          {
            id: generateNodeId(),
            type: "tabs",
            size: 3,
            activeTab: 0,
            children: [
              {
                id: generateNodeId(),
                type: "panel",
                instanceId: panelRegistry.createInstance("code-editor")?.id,
              },
            ],
          },
          {
            id: generateNodeId(),
            type: "panel",
            instanceId: panelRegistry.createInstance("console")?.id,
            size: 1,
          },
        ],
      },
      {
        id: generateNodeId(),
        type: "panel",
        instanceId: panelRegistry.createInstance("inspector")?.id,
        size: 1,
      },
    ],
  };
}

// Context for drag state
const DragContext = React.createContext<{
  dragState: DragState | null;
  setDragState: (state: DragState | null) => void;
  dropZone: { nodeId: string; zone: DropZone } | null;
  setDropZone: (zone: { nodeId: string; zone: DropZone } | null) => void;
}>({
  dragState: null,
  setDragState: () => {},
  dropZone: null,
  setDropZone: () => {},
});

// Render a layout node
function LayoutNodeRenderer({
  node,
  onUpdate,
  focusedPanel,
  onFocus,
  onDrop,
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
  onDrop: (targetNodeId: string, zone: DropZone, draggedInstanceId: string) => void;
}) {
  switch (node.type) {
    case "row":
      return (
        <RowLayout
          node={node}
          onUpdate={onUpdate}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
          onDrop={onDrop}
        />
      );
    case "column":
      return (
        <ColumnLayout
          node={node}
          onUpdate={onUpdate}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
          onDrop={onDrop}
        />
      );
    case "tabs":
      return (
        <TabsLayout
          node={node}
          onUpdate={onUpdate}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
          onDrop={onDrop}
        />
      );
    case "panel":
      return (
        <PanelNode
          node={node}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
          onDrop={onDrop}
        />
      );
    default:
      return <div>Unknown node type</div>;
  }
}

// Drop zone overlay component
function DropZoneOverlay({
  nodeId,
  onDrop,
}: {
  nodeId: string;
  onDrop: (targetNodeId: string, zone: DropZone, draggedInstanceId: string) => void;
}) {
  const { dragState, dropZone, setDropZone } = React.useContext(DragContext);

  if (!dragState) return null;

  const zones: DropZone[] = ["left", "right", "top", "bottom", "center"];

  const handleDragOver = (e: React.DragEvent, zone: DropZone) => {
    e.preventDefault();
    e.stopPropagation();
    setDropZone({ nodeId, zone });
  };

  const handleDragLeave = () => {
    setDropZone(null);
  };

  const handleDrop = (e: React.DragEvent, zone: DropZone) => {
    e.preventDefault();
    e.stopPropagation();
    if (dragState) {
      onDrop(nodeId, zone, dragState.instanceId);
    }
    setDropZone(null);
  };

  return (
    <div className="drop-zone-overlay">
      {zones.map((zone) => (
        <div
          key={zone}
          className={`drop-zone drop-zone-${zone} ${
            dropZone?.nodeId === nodeId && dropZone?.zone === zone ? "active" : ""
          }`}
          onDragOver={(e) => handleDragOver(e, zone)}
          onDragLeave={handleDragLeave}
          onDrop={(e) => handleDrop(e, zone)}
        />
      ))}
    </div>
  );
}

// Row layout (horizontal split)
function RowLayout({
  node,
  onUpdate,
  focusedPanel,
  onFocus,
  onDrop,
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
  onDrop: (targetNodeId: string, zone: DropZone, draggedInstanceId: string) => void;
}) {
  if (!node.children) return null;

  return (
    <div className="layout-row">
      {node.children.map((child, index) => (
        <React.Fragment key={child.id}>
          <div
            className="layout-item"
            style={{ flex: child.size || 1 }}
          >
            <LayoutNodeRenderer
              node={child}
              onUpdate={(updated) => {
                const newChildren = [...node.children!];
                newChildren[index] = updated;
                onUpdate({ ...node, children: newChildren });
              }}
              focusedPanel={focusedPanel}
              onFocus={onFocus}
              onDrop={onDrop}
            />
          </div>
          {index < node.children!.length - 1 && (
            <Resizer
              direction="vertical"
              onResize={(delta) => {
                const children = [...node.children!];
                const deltaRatio = delta / 400;
                children[index] = { ...children[index], size: Math.max(0.1, (children[index].size || 1) + deltaRatio) };
                children[index + 1] = { ...children[index + 1], size: Math.max(0.1, (children[index + 1].size || 1) - deltaRatio) };
                onUpdate({ ...node, children });
              }}
            />
          )}
        </React.Fragment>
      ))}
    </div>
  );
}

// Column layout (vertical split)
function ColumnLayout({
  node,
  onUpdate,
  focusedPanel,
  onFocus,
  onDrop,
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
  onDrop: (targetNodeId: string, zone: DropZone, draggedInstanceId: string) => void;
}) {
  if (!node.children) return null;

  return (
    <div className="layout-column">
      {node.children.map((child, index) => (
        <React.Fragment key={child.id}>
          <div
            className="layout-item"
            style={{ flex: child.size || 1 }}
          >
            <LayoutNodeRenderer
              node={child}
              onUpdate={(updated) => {
                const newChildren = [...node.children!];
                newChildren[index] = updated;
                onUpdate({ ...node, children: newChildren });
              }}
              focusedPanel={focusedPanel}
              onFocus={onFocus}
              onDrop={onDrop}
            />
          </div>
          {index < node.children!.length - 1 && (
            <Resizer
              direction="horizontal"
              onResize={(delta) => {
                const children = [...node.children!];
                const deltaRatio = delta / 300;
                children[index] = { ...children[index], size: Math.max(0.1, (children[index].size || 1) + deltaRatio) };
                children[index + 1] = { ...children[index + 1], size: Math.max(0.1, (children[index + 1].size || 1) - deltaRatio) };
                onUpdate({ ...node, children });
              }}
            />
          )}
        </React.Fragment>
      ))}
    </div>
  );
}

// Tabs layout
function TabsLayout({
  node,
  onUpdate,
  focusedPanel,
  onFocus,
  onDrop,
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
  onDrop: (targetNodeId: string, zone: DropZone, draggedInstanceId: string) => void;
}) {
  const { dragState, setDragState } = React.useContext(DragContext);

  if (!node.children || node.children.length === 0) {
    return <div className="layout-tabs-empty">No tabs</div>;
  }

  const activeIndex = node.activeTab || 0;
  const activeChild = node.children[activeIndex];

  const handleDragStart = (e: React.DragEvent, child: LayoutNode, index: number) => {
    if (!child.instanceId) return;
    const instance = panelRegistry.getInstance(child.instanceId);
    if (!instance) return;

    e.dataTransfer.effectAllowed = "move";
    e.dataTransfer.setData("text/plain", child.instanceId);

    setDragState({
      instanceId: child.instanceId,
      sourceNodeId: node.id,
      title: instance.title,
    });
  };

  const handleDragEnd = () => {
    setDragState(null);
  };

  const handleTabDrop = (e: React.DragEvent, targetIndex: number) => {
    e.preventDefault();
    e.stopPropagation();

    if (!dragState) return;

    // If dropping from another tabs container, add to this one
    if (dragState.sourceNodeId !== node.id) {
      const instance = panelRegistry.getInstance(dragState.instanceId);
      if (instance) {
        const newChild: LayoutNode = {
          id: generateNodeId(),
          type: "panel",
          instanceId: dragState.instanceId,
        };
        const newChildren = [...node.children!];
        newChildren.splice(targetIndex, 0, newChild);
        onUpdate({ ...node, children: newChildren, activeTab: targetIndex });
      }
    } else {
      // Reordering within same tabs container
      const currentIndex = node.children!.findIndex((c) => c.instanceId === dragState.instanceId);
      if (currentIndex >= 0 && currentIndex !== targetIndex) {
        const newChildren = [...node.children!];
        const [moved] = newChildren.splice(currentIndex, 1);
        newChildren.splice(targetIndex > currentIndex ? targetIndex - 1 : targetIndex, 0, moved);
        onUpdate({ ...node, children: newChildren, activeTab: targetIndex > currentIndex ? targetIndex - 1 : targetIndex });
      }
    }

    setDragState(null);
  };

  const handleCloseTab = (index: number, e: React.MouseEvent) => {
    e.stopPropagation();
    const child = node.children![index];
    if (child.instanceId) {
      const def = panelRegistry.getDefinition(panelRegistry.getInstance(child.instanceId)?.panelId || "");
      if (def?.closeable === false) return;
    }

    const newChildren = node.children!.filter((_, i) => i !== index);
    const newActiveTab = activeIndex >= newChildren.length ? newChildren.length - 1 : activeIndex;
    onUpdate({ ...node, children: newChildren, activeTab: Math.max(0, newActiveTab) });
  };

  return (
    <div className="layout-tabs">
      <div className="tabs-header">
        {node.children.map((child, index) => {
          const instance = child.instanceId
            ? panelRegistry.getInstance(child.instanceId)
            : null;
          const def = instance ? panelRegistry.getDefinition(instance.panelId) : null;
          return (
            <div
              key={child.id}
              className={`tab-button ${index === activeIndex ? "active" : ""}`}
              onClick={() => onUpdate({ ...node, activeTab: index })}
              draggable
              onDragStart={(e) => handleDragStart(e, child, index)}
              onDragEnd={handleDragEnd}
              onDragOver={(e) => e.preventDefault()}
              onDrop={(e) => handleTabDrop(e, index)}
            >
              <span className="tab-title">{instance?.title || "Tab"}</span>
              {def?.closeable !== false && (
                <button
                  className="tab-close"
                  onClick={(e) => handleCloseTab(index, e)}
                  title="Close"
                >
                  ×
                </button>
              )}
            </div>
          );
        })}
        <div
          className="tab-drop-zone"
          onDragOver={(e) => e.preventDefault()}
          onDrop={(e) => handleTabDrop(e, node.children!.length)}
        />
      </div>
      <div className="tabs-content">
        <LayoutNodeRenderer
          node={activeChild}
          onUpdate={(updated) => {
            const newChildren = [...node.children!];
            newChildren[activeIndex] = updated;
            onUpdate({ ...node, children: newChildren });
          }}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
          onDrop={onDrop}
        />
      </div>
    </div>
  );
}

// Panel node
function PanelNode({
  node,
  focusedPanel,
  onFocus,
  onDrop,
}: {
  node: LayoutNode;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
  onDrop: (targetNodeId: string, zone: DropZone, draggedInstanceId: string) => void;
}) {
  const { dragState } = React.useContext(DragContext);

  if (!node.instanceId) {
    return <div className="panel-empty">Empty panel slot</div>;
  }

  const instance = panelRegistry.getInstance(node.instanceId);
  if (!instance) {
    return <div className="panel-empty">Panel not found</div>;
  }

  return (
    <div className="panel-node-wrapper">
      <PanelContainer
        instance={instance}
        isFocused={focusedPanel === node.instanceId}
        onFocus={() => onFocus(node.instanceId!)}
      />
      {dragState && dragState.instanceId !== node.instanceId && (
        <DropZoneOverlay nodeId={node.id} onDrop={onDrop} />
      )}
    </div>
  );
}

// Resizer component
function Resizer({
  direction,
  onResize,
}: {
  direction: "horizontal" | "vertical";
  onResize: (delta: number) => void;
}) {
  const [isDragging, setIsDragging] = useState(false);
  const startPos = useRef(0);

  const handleMouseDown = (e: React.MouseEvent) => {
    e.preventDefault();
    setIsDragging(true);
    startPos.current = direction === "horizontal" ? e.clientY : e.clientX;
  };

  useEffect(() => {
    if (!isDragging) return;

    const handleMouseMove = (e: MouseEvent) => {
      const currentPos = direction === "horizontal" ? e.clientY : e.clientX;
      const delta = currentPos - startPos.current;
      onResize(delta);
      startPos.current = currentPos;
    };

    const handleMouseUp = () => {
      setIsDragging(false);
    };

    document.addEventListener("mousemove", handleMouseMove);
    document.addEventListener("mouseup", handleMouseUp);

    return () => {
      document.removeEventListener("mousemove", handleMouseMove);
      document.removeEventListener("mouseup", handleMouseUp);
    };
  }, [isDragging, direction, onResize]);

  return (
    <div
      className={`resizer resizer-${direction} ${isDragging ? "dragging" : ""}`}
      onMouseDown={handleMouseDown}
    />
  );
}

// Helper function to remove a panel from layout
function removeFromLayout(layout: LayoutNode, instanceId: string): LayoutNode | null {
  if (layout.type === "panel") {
    if (layout.instanceId === instanceId) {
      return null;
    }
    return layout;
  }

  if (!layout.children) return layout;

  const newChildren = layout.children
    .map((child) => removeFromLayout(child, instanceId))
    .filter((child): child is LayoutNode => child !== null);

  // If tabs node is empty, remove it
  if (layout.type === "tabs" && newChildren.length === 0) {
    return null;
  }

  // If row/column has only one child, collapse it
  if ((layout.type === "row" || layout.type === "column") && newChildren.length === 1) {
    return { ...newChildren[0], size: layout.size };
  }

  // Adjust active tab if needed
  let activeTab = layout.activeTab;
  if (layout.type === "tabs" && activeTab !== undefined && activeTab >= newChildren.length) {
    activeTab = Math.max(0, newChildren.length - 1);
  }

  return { ...layout, children: newChildren, activeTab };
}

// Helper to add panel to a node
function addToLayout(
  layout: LayoutNode,
  targetNodeId: string,
  zone: DropZone,
  instanceId: string
): LayoutNode {
  if (layout.id === targetNodeId) {
    const newPanelNode: LayoutNode = {
      id: generateNodeId(),
      type: "panel",
      instanceId,
    };

    if (zone === "center") {
      // Wrap in tabs
      return {
        id: generateNodeId(),
        type: "tabs",
        activeTab: 1,
        size: layout.size,
        children: [layout, newPanelNode],
      };
    }

    // Create split
    const isHorizontal = zone === "left" || zone === "right";
    const isFirst = zone === "left" || zone === "top";

    const children = isFirst
      ? [{ ...newPanelNode, size: 1 }, { ...layout, size: 2 }]
      : [{ ...layout, size: 2 }, { ...newPanelNode, size: 1 }];

    return {
      id: generateNodeId(),
      type: isHorizontal ? "row" : "column",
      size: layout.size,
      children,
    };
  }

  if (!layout.children) return layout;

  return {
    ...layout,
    children: layout.children.map((child) =>
      addToLayout(child, targetNodeId, zone, instanceId)
    ),
  };
}

// Main DockingLayout component
export function DockingLayout({ layout, onLayoutChange }: DockingLayoutProps) {
  const [focusedPanel, setFocusedPanel] = useState<string | null>(null);
  const [dragState, setDragState] = useState<DragState | null>(null);
  const [dropZone, setDropZone] = useState<{ nodeId: string; zone: DropZone } | null>(null);

  // Listen for panel focus events
  useMessageBus(Channels.PANEL_FOCUS, (data: PanelInstance) => {
    setFocusedPanel(data.id);
  });

  const handleDrop = useCallback(
    (targetNodeId: string, zone: DropZone, draggedInstanceId: string) => {
      if (!onLayoutChange) return;

      // Remove from current location
      let newLayout = removeFromLayout(layout, draggedInstanceId);
      if (!newLayout) {
        // The entire layout was the removed panel - shouldn't happen normally
        return;
      }

      // Add to new location
      newLayout = addToLayout(newLayout, targetNodeId, zone, draggedInstanceId);
      onLayoutChange(newLayout);
      setDragState(null);
    },
    [layout, onLayoutChange]
  );

  return (
    <DragContext.Provider value={{ dragState, setDragState, dropZone, setDropZone }}>
      <div className={`docking-layout ${dragState ? "dragging" : ""}`}>
        <LayoutNodeRenderer
          node={layout}
          onUpdate={onLayoutChange || (() => {})}
          focusedPanel={focusedPanel}
          onFocus={setFocusedPanel}
          onDrop={handleDrop}
        />
      </div>
    </DragContext.Provider>
  );
}
