/**
 * Command Palette - VS Code-style command search and execution
 */

import React, { useState, useEffect, useRef, useCallback } from "react";
import { Command, commandRegistry } from "@/core/command-registry";
import { keybindingManager } from "@/core/keybinding-manager";
import { useMessageBus } from "@/hooks/useMessageBus";
import "./CommandPalette.css";

interface CommandPaletteProps {
  isOpen: boolean;
  onClose: () => void;
}

export function CommandPalette({ isOpen, onClose }: CommandPaletteProps) {
  const [query, setQuery] = useState("");
  const [results, setResults] = useState<Command[]>([]);
  const [selectedIndex, setSelectedIndex] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);
  const listRef = useRef<HTMLDivElement>(null);

  // Search commands when query changes
  useEffect(() => {
    const results = commandRegistry.search(query);
    setResults(results);
    setSelectedIndex(0);
  }, [query]);

  // Focus input when opened
  useEffect(() => {
    if (isOpen) {
      setQuery("");
      setSelectedIndex(0);
      setTimeout(() => inputRef.current?.focus(), 50);
    }
  }, [isOpen]);

  // Scroll selected item into view
  useEffect(() => {
    if (listRef.current) {
      const selectedElement = listRef.current.children[selectedIndex] as HTMLElement;
      if (selectedElement) {
        selectedElement.scrollIntoView({ block: "nearest" });
      }
    }
  }, [selectedIndex]);

  const handleKeyDown = useCallback((event: React.KeyboardEvent) => {
    switch (event.key) {
      case "ArrowDown":
        event.preventDefault();
        setSelectedIndex(i => Math.min(i + 1, results.length - 1));
        break;

      case "ArrowUp":
        event.preventDefault();
        setSelectedIndex(i => Math.max(i - 1, 0));
        break;

      case "Enter":
        event.preventDefault();
        if (results[selectedIndex]) {
          executeCommand(results[selectedIndex]);
        }
        break;

      case "Escape":
        event.preventDefault();
        onClose();
        break;

      case "Tab":
        event.preventDefault();
        // Tab moves down, Shift+Tab moves up
        if (event.shiftKey) {
          setSelectedIndex(i => Math.max(i - 1, 0));
        } else {
          setSelectedIndex(i => Math.min(i + 1, results.length - 1));
        }
        break;
    }
  }, [results, selectedIndex, onClose]);

  const executeCommand = useCallback((command: Command) => {
    onClose();
    // Small delay to allow palette to close before executing
    setTimeout(() => {
      commandRegistry.execute(command.id);
    }, 50);
  }, [onClose]);

  const handleItemClick = useCallback((command: Command, index: number) => {
    setSelectedIndex(index);
    executeCommand(command);
  }, [executeCommand]);

  const handleBackdropClick = useCallback((event: React.MouseEvent) => {
    if (event.target === event.currentTarget) {
      onClose();
    }
  }, [onClose]);

  if (!isOpen) return null;

  return (
    <div className="command-palette-overlay" onClick={handleBackdropClick}>
      <div className="command-palette">
        <div className="command-palette-input-container">
          <span className="command-palette-icon">⌘</span>
          <input
            ref={inputRef}
            type="text"
            className="command-palette-input"
            placeholder="Type a command..."
            value={query}
            onChange={e => setQuery(e.target.value)}
            onKeyDown={handleKeyDown}
          />
        </div>

        <div className="command-palette-results" ref={listRef}>
          {results.length === 0 ? (
            <div className="command-palette-empty">
              No commands found
            </div>
          ) : (
            results.map((command, index) => (
              <CommandItem
                key={command.id}
                command={command}
                isSelected={index === selectedIndex}
                onClick={() => handleItemClick(command, index)}
                onMouseEnter={() => setSelectedIndex(index)}
              />
            ))
          )}
        </div>

        <div className="command-palette-footer">
          <span className="command-palette-hint">
            <kbd>↑↓</kbd> navigate
            <kbd>↵</kbd> select
            <kbd>esc</kbd> close
          </span>
        </div>
      </div>
    </div>
  );
}

interface CommandItemProps {
  command: Command;
  isSelected: boolean;
  onClick: () => void;
  onMouseEnter: () => void;
}

function CommandItem({ command, isSelected, onClick, onMouseEnter }: CommandItemProps) {
  const keybinding = keybindingManager.getForCommand(command.id);
  const displayKeybinding = keybinding ? keybindingManager.formatForDisplay(keybinding) : null;

  return (
    <div
      className={`command-palette-item ${isSelected ? "selected" : ""}`}
      onClick={onClick}
      onMouseEnter={onMouseEnter}
    >
      <div className="command-palette-item-content">
        {command.category && (
          <span className="command-palette-category">{command.category}</span>
        )}
        <span className="command-palette-title">{command.title}</span>
      </div>
      {displayKeybinding && (
        <span className="command-palette-keybinding">{displayKeybinding}</span>
      )}
    </div>
  );
}

/**
 * Hook to manage command palette state
 */
export function useCommandPalette() {
  const [isOpen, setIsOpen] = useState(false);

  // Listen for toggle event
  useMessageBus("ui:toggle-command-palette", () => {
    setIsOpen(prev => !prev);
  });

  // Close when command is executed
  useMessageBus("command:executed", () => {
    setIsOpen(false);
  });

  return {
    isOpen,
    open: () => setIsOpen(true),
    close: () => setIsOpen(false),
    toggle: () => setIsOpen(prev => !prev),
  };
}
