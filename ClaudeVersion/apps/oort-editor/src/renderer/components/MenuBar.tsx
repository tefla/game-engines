/**
 * Menu Bar - Application menu bar with command integration
 */

import React, { useState, useRef, useEffect, useCallback } from "react";
import { commandRegistry } from "@/core/command-registry";
import { keybindingManager } from "@/core/keybinding-manager";
import "./MenuBar.css";

export interface MenuItem {
  id?: string;           // Command ID to execute
  label: string;
  type?: "separator" | "submenu" | "item";
  submenu?: MenuItem[];
  disabled?: boolean;
}

export interface MenuDefinition {
  label: string;
  items: MenuItem[];
}

const defaultMenus: MenuDefinition[] = [
  {
    label: "File",
    items: [
      { id: "file.newFile", label: "New File" },
      { id: "file.newFolder", label: "New Folder" },
      { type: "separator", label: "" },
      { id: "file.open", label: "Open File..." },
      { id: "project.open", label: "Open Project..." },
      { type: "separator", label: "" },
      { id: "file.save", label: "Save" },
      { id: "file.saveAs", label: "Save As..." },
      { type: "separator", label: "" },
      { id: "file.closeFile", label: "Close File" },
      { id: "project.close", label: "Close Project" },
    ],
  },
  {
    label: "Edit",
    items: [
      { id: "edit.undo", label: "Undo" },
      { id: "edit.redo", label: "Redo" },
      { type: "separator", label: "" },
      { id: "edit.cut", label: "Cut" },
      { id: "edit.copy", label: "Copy" },
      { id: "edit.paste", label: "Paste" },
      { id: "edit.selectAll", label: "Select All" },
      { type: "separator", label: "" },
      { id: "edit.find", label: "Find" },
      { id: "edit.findReplace", label: "Find and Replace" },
    ],
  },
  {
    label: "View",
    items: [
      { id: "view.commandPalette", label: "Command Palette" },
      { type: "separator", label: "" },
      { id: "view.toggleSidebar", label: "Toggle Sidebar" },
      { id: "view.toggleConsole", label: "Toggle Console" },
      { id: "view.toggleInspector", label: "Toggle Inspector" },
      { type: "separator", label: "" },
      {
        label: "Layout",
        type: "submenu",
        submenu: [
          { id: "layout.default", label: "Default" },
          { id: "layout.editorFocus", label: "Editor Focus" },
          { id: "layout.wide", label: "Wide" },
          { type: "separator", label: "" },
          { id: "layout.resetLayout", label: "Reset Layout" },
        ],
      },
      { type: "separator", label: "" },
      { id: "view.zoomIn", label: "Zoom In" },
      { id: "view.zoomOut", label: "Zoom Out" },
      { id: "view.resetZoom", label: "Reset Zoom" },
    ],
  },
  {
    label: "Run",
    items: [
      { id: "run.play", label: "Play Game" },
      { id: "run.stop", label: "Stop" },
      { id: "run.restart", label: "Restart" },
    ],
  },
  {
    label: "Help",
    items: [
      { id: "help.documentation", label: "Documentation" },
      { id: "help.keyboardShortcuts", label: "Keyboard Shortcuts" },
      { type: "separator", label: "" },
      { id: "dev.toggleDevTools", label: "Toggle Developer Tools" },
      { type: "separator", label: "" },
      { id: "help.about", label: "About Oort Editor" },
    ],
  },
];

interface MenuBarProps {
  menus?: MenuDefinition[];
}

export function MenuBar({ menus = defaultMenus }: MenuBarProps) {
  const [openMenuIndex, setOpenMenuIndex] = useState<number | null>(null);
  const menuBarRef = useRef<HTMLDivElement>(null);

  // Close menu when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (menuBarRef.current && !menuBarRef.current.contains(event.target as Node)) {
        setOpenMenuIndex(null);
      }
    };

    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  // Close menu on escape
  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        setOpenMenuIndex(null);
      }
    };

    document.addEventListener("keydown", handleKeyDown);
    return () => document.removeEventListener("keydown", handleKeyDown);
  }, []);

  const handleMenuClick = useCallback((index: number) => {
    setOpenMenuIndex(prev => prev === index ? null : index);
  }, []);

  const handleMenuHover = useCallback((index: number) => {
    if (openMenuIndex !== null) {
      setOpenMenuIndex(index);
    }
  }, [openMenuIndex]);

  const handleItemClick = useCallback((item: MenuItem) => {
    if (item.type === "separator" || item.type === "submenu") return;
    if (item.disabled) return;

    setOpenMenuIndex(null);

    if (item.id) {
      commandRegistry.execute(item.id);
    }
  }, []);

  return (
    <div className="menu-bar" ref={menuBarRef}>
      {menus.map((menu, index) => (
        <div
          key={menu.label}
          className={`menu-bar-item ${openMenuIndex === index ? "open" : ""}`}
        >
          <button
            className="menu-bar-button"
            onClick={() => handleMenuClick(index)}
            onMouseEnter={() => handleMenuHover(index)}
          >
            {menu.label}
          </button>
          {openMenuIndex === index && (
            <MenuDropdown
              items={menu.items}
              onItemClick={handleItemClick}
            />
          )}
        </div>
      ))}
    </div>
  );
}

interface MenuDropdownProps {
  items: MenuItem[];
  onItemClick: (item: MenuItem) => void;
}

function MenuDropdown({ items, onItemClick }: MenuDropdownProps) {
  return (
    <div className="menu-dropdown">
      {items.map((item, index) => (
        <MenuItemComponent
          key={item.id || `${item.label}-${index}`}
          item={item}
          onItemClick={onItemClick}
        />
      ))}
    </div>
  );
}

interface MenuItemComponentProps {
  item: MenuItem;
  onItemClick: (item: MenuItem) => void;
}

function MenuItemComponent({ item, onItemClick }: MenuItemComponentProps) {
  const [submenuOpen, setSubmenuOpen] = useState(false);
  const submenuTimeout = useRef<ReturnType<typeof setTimeout>>();

  if (item.type === "separator") {
    return <div className="menu-separator" />;
  }

  const command = item.id ? commandRegistry.get(item.id) : undefined;
  const keybinding = item.id ? keybindingManager.getForCommand(item.id) : undefined;
  const displayKeybinding = keybinding ? keybindingManager.formatForDisplay(keybinding) : undefined;

  // Check if command is available
  const isDisabled = item.disabled ||
    (command?.when && !command.when());

  if (item.type === "submenu" && item.submenu) {
    return (
      <div
        className={`menu-item submenu ${isDisabled ? "disabled" : ""}`}
        onMouseEnter={() => {
          clearTimeout(submenuTimeout.current);
          setSubmenuOpen(true);
        }}
        onMouseLeave={() => {
          submenuTimeout.current = setTimeout(() => setSubmenuOpen(false), 100);
        }}
      >
        <span className="menu-item-label">{item.label}</span>
        <span className="menu-item-arrow">▶</span>
        {submenuOpen && (
          <div className="menu-submenu">
            {item.submenu.map((subitem, index) => (
              <MenuItemComponent
                key={subitem.id || `${subitem.label}-${index}`}
                item={subitem}
                onItemClick={onItemClick}
              />
            ))}
          </div>
        )}
      </div>
    );
  }

  return (
    <button
      className={`menu-item ${isDisabled ? "disabled" : ""}`}
      onClick={() => !isDisabled && onItemClick(item)}
      disabled={isDisabled}
    >
      <span className="menu-item-label">{item.label}</span>
      {displayKeybinding && (
        <span className="menu-item-shortcut">{displayKeybinding}</span>
      )}
    </button>
  );
}
