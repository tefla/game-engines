/**
 * Context Menu - Reusable context menu component with command integration
 */

import React, { useEffect, useRef, useCallback } from "react";
import { commandRegistry } from "@/core/command-registry";
import { keybindingManager } from "@/core/keybinding-manager";
import "./ContextMenu.css";

export interface ContextMenuItem {
  id?: string;           // Command ID to execute
  label: string;
  type?: "separator" | "item";
  icon?: string;
  disabled?: boolean;
  danger?: boolean;
  onClick?: () => void;  // Custom handler (if not using command)
}

interface ContextMenuProps {
  x: number;
  y: number;
  items: ContextMenuItem[];
  onClose: () => void;
  context?: any;         // Optional context data passed to command
}

export function ContextMenu({ x, y, items, onClose, context }: ContextMenuProps) {
  const menuRef = useRef<HTMLDivElement>(null);

  // Position adjustment to keep menu in viewport
  useEffect(() => {
    if (menuRef.current) {
      const rect = menuRef.current.getBoundingClientRect();
      const viewportWidth = window.innerWidth;
      const viewportHeight = window.innerHeight;

      // Adjust horizontal position
      if (rect.right > viewportWidth) {
        menuRef.current.style.left = `${viewportWidth - rect.width - 8}px`;
      }

      // Adjust vertical position
      if (rect.bottom > viewportHeight) {
        menuRef.current.style.top = `${viewportHeight - rect.height - 8}px`;
      }
    }
  }, [x, y]);

  // Close on escape
  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        onClose();
      }
    };

    document.addEventListener("keydown", handleKeyDown);
    return () => document.removeEventListener("keydown", handleKeyDown);
  }, [onClose]);

  // Close on click outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
        onClose();
      }
    };

    // Use mousedown to catch clicks before they propagate
    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, [onClose]);

  const handleItemClick = useCallback((item: ContextMenuItem) => {
    if (item.type === "separator" || item.disabled) return;

    onClose();

    // Use custom handler if provided
    if (item.onClick) {
      item.onClick();
      return;
    }

    // Otherwise execute command
    if (item.id) {
      commandRegistry.execute(item.id);
    }
  }, [onClose, context]);

  return (
    <div
      ref={menuRef}
      className="context-menu"
      style={{ left: x, top: y }}
      onClick={(e) => e.stopPropagation()}
    >
      {items.map((item, index) => (
        <ContextMenuItemComponent
          key={item.id || `${item.label}-${index}`}
          item={item}
          onClick={() => handleItemClick(item)}
        />
      ))}
    </div>
  );
}

interface ContextMenuItemComponentProps {
  item: ContextMenuItem;
  onClick: () => void;
}

function ContextMenuItemComponent({ item, onClick }: ContextMenuItemComponentProps) {
  if (item.type === "separator") {
    return <div className="context-menu-separator" />;
  }

  const command = item.id ? commandRegistry.get(item.id) : undefined;
  const keybinding = item.id ? keybindingManager.getForCommand(item.id) : undefined;
  const displayKeybinding = keybinding ? keybindingManager.formatForDisplay(keybinding) : undefined;

  // Check if command is available
  const isDisabled = item.disabled ||
    (command?.when && !command.when());

  return (
    <button
      className={`context-menu-item ${isDisabled ? "disabled" : ""} ${item.danger ? "danger" : ""}`}
      onClick={!isDisabled ? onClick : undefined}
      disabled={isDisabled}
    >
      {item.icon && <span className="context-menu-icon">{item.icon}</span>}
      <span className="context-menu-label">{item.label}</span>
      {displayKeybinding && (
        <span className="context-menu-shortcut">{displayKeybinding}</span>
      )}
    </button>
  );
}

/**
 * Hook to manage context menu state
 */
export function useContextMenu<T = any>() {
  const [state, setState] = React.useState<{
    x: number;
    y: number;
    data: T;
  } | null>(null);

  const open = useCallback((x: number, y: number, data: T) => {
    setState({ x, y, data });
  }, []);

  const close = useCallback(() => {
    setState(null);
  }, []);

  const handleContextMenu = useCallback((event: React.MouseEvent, data: T) => {
    event.preventDefault();
    event.stopPropagation();
    open(event.clientX, event.clientY, data);
  }, [open]);

  return {
    isOpen: state !== null,
    x: state?.x ?? 0,
    y: state?.y ?? 0,
    data: state?.data,
    open,
    close,
    handleContextMenu,
  };
}
