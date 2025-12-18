import React, { useState, useEffect, useRef } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useMessageBus, useEmit } from "@/hooks/useMessageBus";
import { ConsoleEntry } from "@shared/types";
import "./ConsolePanel.css";

let entryId = 0;

export function ConsolePanel({ panelId, instanceId }: PanelProps) {
  const [entries, setEntries] = useState<ConsoleEntry[]>([]);
  const [input, setInput] = useState("");
  const [history, setHistory] = useState<string[]>([]);
  const [historyIndex, setHistoryIndex] = useState(-1);
  const outputRef = useRef<HTMLDivElement>(null);
  const emit = useEmit();

  // Subscribe to console messages
  useMessageBus("console:log", (data) => {
    addEntry("log", data.text, data.source);
  });

  useMessageBus("console:error", (data) => {
    addEntry("error", data.text, data.source);
  });

  useMessageBus("console:warn", (data) => {
    addEntry("warn", data.text, data.source);
  });

  useMessageBus("console:info", (data) => {
    addEntry("info", data.text, data.source);
  });

  useMessageBus("console:clear", () => {
    setEntries([]);
  });

  const addEntry = (type: ConsoleEntry["type"], text: string, source?: string) => {
    setEntries((prev) => [
      ...prev,
      {
        id: `entry-${++entryId}`,
        type,
        text,
        timestamp: Date.now(),
        source,
      },
    ]);
  };

  // Auto-scroll to bottom
  useEffect(() => {
    if (outputRef.current) {
      outputRef.current.scrollTop = outputRef.current.scrollHeight;
    }
  }, [entries]);

  const handleSubmit = () => {
    const trimmed = input.trim();
    if (!trimmed) return;

    // Add to history
    setHistory((prev) => [...prev.filter((h) => h !== trimmed), trimmed]);
    setHistoryIndex(-1);

    // Echo input
    addEntry("info", `> ${trimmed}`);

    // For now, just echo back - Slate integration will come later
    try {
      // Simple expression evaluation for demo
      if (trimmed.startsWith("clear")) {
        setEntries([]);
      } else if (trimmed.startsWith("help")) {
        addEntry("log", "Available commands:\n  clear - Clear console\n  help - Show this help");
      } else {
        addEntry("log", `[Slate REPL coming soon] ${trimmed}`);
      }
    } catch (err: any) {
      addEntry("error", err.message);
    }

    setInput("");
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSubmit();
    } else if (e.key === "ArrowUp") {
      e.preventDefault();
      if (history.length > 0) {
        const newIndex = historyIndex < history.length - 1 ? historyIndex + 1 : historyIndex;
        setHistoryIndex(newIndex);
        setInput(history[history.length - 1 - newIndex] || "");
      }
    } else if (e.key === "ArrowDown") {
      e.preventDefault();
      if (historyIndex > 0) {
        const newIndex = historyIndex - 1;
        setHistoryIndex(newIndex);
        setInput(history[history.length - 1 - newIndex] || "");
      } else if (historyIndex === 0) {
        setHistoryIndex(-1);
        setInput("");
      }
    }
  };

  const formatTime = (timestamp: number): string => {
    const date = new Date(timestamp);
    return date.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", second: "2-digit" });
  };

  return (
    <div className="console-panel">
      <div className="console-toolbar">
        <button
          className="console-btn"
          onClick={() => setEntries([])}
          title="Clear"
        >
          🗑️
        </button>
      </div>
      <div className="console-output" ref={outputRef}>
        {entries.map((entry) => (
          <div key={entry.id} className={`console-entry console-${entry.type}`}>
            <span className="entry-time">{formatTime(entry.timestamp)}</span>
            <span className="entry-text">{entry.text}</span>
          </div>
        ))}
        {entries.length === 0 && (
          <div className="console-empty">
            Console ready. Type commands or view output here.
          </div>
        )}
      </div>
      <div className="console-input-container">
        <span className="input-prompt">&gt;</span>
        <input
          className="console-input"
          value={input}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={handleKeyDown}
          placeholder="Enter command..."
          spellCheck={false}
        />
      </div>
    </div>
  );
}
