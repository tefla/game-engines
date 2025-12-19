import React, { useState, useEffect, useRef, useCallback } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useMessageBus, useEmit } from "@/hooks/useMessageBus";
import { ConsoleEntry } from "@shared/types";
import {
  Lexer,
  Parser,
  Interpreter,
  stdlib,
  stringify,
  Str,
  Null,
  type SlateValue,
  type SlateNativeFunction,
  RuntimeError,
  ParseError,
} from "@oort/slate";
import { runtimeService } from "@/core/runtime-service";
import "./ConsolePanel.css";

let entryId = 0;

// Create a persistent interpreter with output capture
function createInterpreter(addOutput: (type: ConsoleEntry["type"], text: string) => void) {
  // Custom print/say that outputs to console panel
  const customPrint: SlateNativeFunction = {
    type: "native",
    name: "print",
    arity: "variadic",
    fn: (args: SlateValue[]) => {
      const text = args.map((a: SlateValue) => stringify(a)).join(" ");
      addOutput("log", text);
      return Null();
    },
  };

  const customSay: SlateNativeFunction = {
    type: "native",
    name: "say",
    arity: "variadic",
    fn: (args: SlateValue[]) => {
      const text = args.map((a: SlateValue) => stringify(a)).join(" ");
      addOutput("info", text);
      return Null();
    },
  };

  // Merge stdlib with custom I/O
  const globals = new Map<string, SlateValue>(stdlib);
  globals.set("print", customPrint);
  globals.set("say", customSay);

  return new Interpreter(globals);
}

export function ConsolePanel({ panelId, instanceId }: PanelProps) {
  const [entries, setEntries] = useState<ConsoleEntry[]>([]);
  const [input, setInput] = useState("");
  const [history, setHistory] = useState<string[]>([]);
  const [historyIndex, setHistoryIndex] = useState(-1);
  const [multiline, setMultiline] = useState(false);
  const outputRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLTextAreaElement>(null);
  const interpreterRef = useRef<Interpreter | null>(null);
  const emit = useEmit();

  const addEntry = useCallback((type: ConsoleEntry["type"], text: string, source?: string) => {
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
  }, []);

  // Initialize interpreter
  useEffect(() => {
    interpreterRef.current = createInterpreter(addEntry);
  }, [addEntry]);

  // Subscribe to console messages from other parts of the app
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

  // Auto-scroll to bottom
  useEffect(() => {
    if (outputRef.current) {
      outputRef.current.scrollTop = outputRef.current.scrollHeight;
    }
  }, [entries]);

  const runSlateCode = (code: string): { success: boolean; result?: string; error?: string } => {
    try {
      const lexer = new Lexer(code);
      const tokens = lexer.tokenize();
      const parser = new Parser(tokens);
      const ast = parser.parse();

      if (!interpreterRef.current) {
        interpreterRef.current = createInterpreter(addEntry);
      }

      const result = interpreterRef.current.run(ast);
      return { success: true, result: stringify(result) };
    } catch (err) {
      if (err instanceof ParseError) {
        return { success: false, error: `Parse Error: ${err.message}` };
      }
      if (err instanceof RuntimeError) {
        return { success: false, error: `Runtime Error: ${err.message}` };
      }
      return { success: false, error: String(err) };
    }
  };

  const handleSubmit = () => {
    const trimmed = input.trim();
    if (!trimmed) return;

    // Add to history
    setHistory((prev) => [...prev.filter((h) => h !== trimmed), trimmed]);
    setHistoryIndex(-1);

    // Echo input
    addEntry("info", `> ${trimmed.replace(/\n/g, "\n  ")}`);

    // Handle built-in commands
    if (trimmed === "clear") {
      setEntries([]);
      setInput("");
      return;
    }

    if (trimmed === "help") {
      addEntry("log", `Available commands:
  clear         - Clear console
  help          - Show this help
  reset         - Reset interpreter state

Checkpoints (VCS):
  save <name>   - Save current game state
  load <name>   - Load a saved checkpoint
  checkpoints   - List all checkpoints
  undo          - Undo last VFS change
  redo          - Redo VFS change

Slate REPL:
  Enter any Slate expression or statement.
  Use Shift+Enter for multiline input.

Examples:
  let x = 10
  x + 5
  fn double n: n * 2
  double(21)
  print("Hello, world!")
  [1, 2, 3] + [4, 5]`);
      setInput("");
      return;
    }

    // VCS commands
    if (trimmed.startsWith("save ")) {
      const name = trimmed.substring(5).trim();
      if (name) {
        if (runtimeService.saveCheckpoint(name)) {
          addEntry("info", `Checkpoint saved: ${name}`);
        } else {
          addEntry("error", "Failed to save checkpoint. Is a project open?");
        }
      } else {
        addEntry("error", "Usage: save <checkpoint-name>");
      }
      setInput("");
      return;
    }

    if (trimmed.startsWith("load ")) {
      const name = trimmed.substring(5).trim();
      if (name) {
        if (runtimeService.loadCheckpoint(name)) {
          addEntry("info", `Checkpoint loaded: ${name}`);
        } else {
          addEntry("error", `Checkpoint not found: ${name}`);
        }
      } else {
        addEntry("error", "Usage: load <checkpoint-name>");
      }
      setInput("");
      return;
    }

    if (trimmed === "checkpoints") {
      const checkpoints = runtimeService.listCheckpoints();
      if (checkpoints.length === 0) {
        addEntry("info", "No checkpoints saved. Use 'save <name>' to create one.");
      } else {
        const list = checkpoints.map(cp => {
          const date = new Date(cp.timestamp);
          return `  ${cp.name} (${date.toLocaleString()})`;
        }).join("\n");
        addEntry("log", `Saved checkpoints:\n${list}`);
      }
      setInput("");
      return;
    }

    if (trimmed === "undo") {
      if (runtimeService.undo()) {
        addEntry("info", "Undo successful");
      } else {
        addEntry("error", "Nothing to undo");
      }
      setInput("");
      return;
    }

    if (trimmed === "redo") {
      if (runtimeService.redo()) {
        addEntry("info", "Redo successful");
      } else {
        addEntry("error", "Nothing to redo");
      }
      setInput("");
      return;
    }

    if (trimmed === "reset") {
      interpreterRef.current = createInterpreter(addEntry);
      addEntry("info", "Interpreter state reset");
      setInput("");
      return;
    }

    // Run Slate code
    const { success, result, error } = runSlateCode(trimmed);

    if (success) {
      // Only show result if it's not null/undefined
      if (result && result !== "null") {
        addEntry("log", `← ${result}`);
      }
    } else {
      addEntry("error", error || "Unknown error");
    }

    setInput("");
    setMultiline(false);
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      if (e.shiftKey) {
        // Shift+Enter for multiline
        setMultiline(true);
        return;
      }
      e.preventDefault();
      handleSubmit();
    } else if (e.key === "ArrowUp" && !multiline) {
      e.preventDefault();
      if (history.length > 0) {
        const newIndex = historyIndex < history.length - 1 ? historyIndex + 1 : historyIndex;
        setHistoryIndex(newIndex);
        setInput(history[history.length - 1 - newIndex] || "");
      }
    } else if (e.key === "ArrowDown" && !multiline) {
      e.preventDefault();
      if (historyIndex > 0) {
        const newIndex = historyIndex - 1;
        setHistoryIndex(newIndex);
        setInput(history[history.length - 1 - newIndex] || "");
      } else if (historyIndex === 0) {
        setHistoryIndex(-1);
        setInput("");
      }
    } else if (e.key === "l" && (e.ctrlKey || e.metaKey)) {
      e.preventDefault();
      setEntries([]);
    }
  };

  const formatTime = (timestamp: number): string => {
    const date = new Date(timestamp);
    return date.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", second: "2-digit" });
  };

  const getEntryIcon = (type: ConsoleEntry["type"]): string => {
    switch (type) {
      case "error":
        return "❌";
      case "warn":
        return "⚠️";
      case "info":
        return "ℹ️";
      default:
        return "";
    }
  };

  return (
    <div className="console-panel">
      <div className="console-toolbar">
        <button
          className="console-btn"
          onClick={() => setEntries([])}
          title="Clear (Cmd+L)"
        >
          🗑️ Clear
        </button>
        <button
          className="console-btn"
          onClick={() => {
            interpreterRef.current = createInterpreter(addEntry);
            addEntry("info", "Interpreter state reset");
          }}
          title="Reset interpreter state"
        >
          🔄 Reset
        </button>
        <div className="console-toolbar-spacer" />
        <span className="console-language-badge">Slate REPL</span>
      </div>
      <div className="console-output" ref={outputRef}>
        {entries.map((entry) => (
          <div key={entry.id} className={`console-entry console-${entry.type}`}>
            <span className="entry-time">{formatTime(entry.timestamp)}</span>
            {entry.type !== "log" && (
              <span className="entry-icon">{getEntryIcon(entry.type)}</span>
            )}
            <span className="entry-text">{entry.text}</span>
          </div>
        ))}
        {entries.length === 0 && (
          <div className="console-empty">
            <p>Slate REPL ready. Type <code>help</code> for available commands.</p>
            <p className="hint">Tip: Use Shift+Enter for multiline input.</p>
          </div>
        )}
      </div>
      <div className={`console-input-container ${multiline ? "multiline" : ""}`}>
        <span className="input-prompt">{multiline ? "..." : ">"}</span>
        <textarea
          ref={inputRef}
          className="console-input"
          value={input}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={handleKeyDown}
          placeholder="Enter Slate code..."
          spellCheck={false}
          rows={multiline ? 3 : 1}
        />
        {multiline && (
          <button className="console-run-btn" onClick={handleSubmit} title="Run (Enter)">
            ▶
          </button>
        )}
      </div>
    </div>
  );
}
