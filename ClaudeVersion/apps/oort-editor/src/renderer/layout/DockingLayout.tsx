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

// Render a layout node
function LayoutNodeRenderer({
  node,
  onUpdate,
  focusedPanel,
  onFocus,
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
}) {
  switch (node.type) {
    case "row":
      return (
        <RowLayout
          node={node}
          onUpdate={onUpdate}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
        />
      );
    case "column":
      return (
        <ColumnLayout
          node={node}
          onUpdate={onUpdate}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
        />
      );
    case "tabs":
      return (
        <TabsLayout
          node={node}
          onUpdate={onUpdate}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
        />
      );
    case "panel":
      return (
        <PanelNode
          node={node}
          focusedPanel={focusedPanel}
          onFocus={onFocus}
        />
      );
    default:
      return <div>Unknown node type</div>;
  }
}

// Row layout (horizontal split)
function RowLayout({
  node,
  onUpdate,
  focusedPanel,
  onFocus,
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
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
            />
          </div>
          {index < node.children!.length - 1 && (
            <Resizer
              direction="vertical"
              onResize={(delta) => {
                // Handle resize
                const children = [...node.children!];
                const totalSize = children.reduce((sum, c) => sum + (c.size || 1), 0);
                const deltaRatio = delta / 400; // Approximate pixel to ratio
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
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
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
}: {
  node: LayoutNode;
  onUpdate: (node: LayoutNode) => void;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
}) {
  if (!node.children || node.children.length === 0) {
    return <div className="layout-tabs-empty">No tabs</div>;
  }

  const activeIndex = node.activeTab || 0;
  const activeChild = node.children[activeIndex];

  return (
    <div className="layout-tabs">
      <div className="tabs-header">
        {node.children.map((child, index) => {
          const instance = child.instanceId
            ? panelRegistry.getInstance(child.instanceId)
            : null;
          return (
            <button
              key={child.id}
              className={`tab-button ${index === activeIndex ? "active" : ""}`}
              onClick={() => onUpdate({ ...node, activeTab: index })}
            >
              {instance?.title || "Tab"}
            </button>
          );
        })}
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
}: {
  node: LayoutNode;
  focusedPanel: string | null;
  onFocus: (instanceId: string) => void;
}) {
  if (!node.instanceId) {
    return <div className="panel-empty">Empty panel slot</div>;
  }

  const instance = panelRegistry.getInstance(node.instanceId);
  if (!instance) {
    return <div className="panel-empty">Panel not found</div>;
  }

  return (
    <PanelContainer
      instance={instance}
      isFocused={focusedPanel === node.instanceId}
      onFocus={() => onFocus(node.instanceId!)}
    />
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

// Main DockingLayout component
export function DockingLayout({ layout, onLayoutChange }: DockingLayoutProps) {
  const [focusedPanel, setFocusedPanel] = useState<string | null>(null);

  // Listen for panel focus events
  useMessageBus(Channels.PANEL_FOCUS, (data: PanelInstance) => {
    setFocusedPanel(data.id);
  });

  return (
    <div className="docking-layout">
      <LayoutNodeRenderer
        node={layout}
        onUpdate={onLayoutChange || (() => {})}
        focusedPanel={focusedPanel}
        onFocus={setFocusedPanel}
      />
    </div>
  );
}
