import React, { useState, useEffect, useRef, useCallback } from "react";
import * as monaco from "monaco-editor";
import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import jsonWorker from "monaco-editor/esm/vs/language/json/json.worker?worker";
import cssWorker from "monaco-editor/esm/vs/language/css/css.worker?worker";
import htmlWorker from "monaco-editor/esm/vs/language/html/html.worker?worker";
import tsWorker from "monaco-editor/esm/vs/language/typescript/ts.worker?worker";
import { PanelProps } from "@/core/panel-registry";
import { useFiles, useUI } from "@/hooks/useStore";
import { useEmit, useMessageBus } from "@/hooks/useMessageBus";
import { registerSlateLanguage, registerSlateThemes } from "@/editor/slate-language";
import "./CodeEditor.css";

// Configure Monaco environment for Electron/Vite
self.MonacoEnvironment = {
  getWorker(_, label) {
    if (label === "json") {
      return new jsonWorker();
    }
    if (label === "css" || label === "scss" || label === "less") {
      return new cssWorker();
    }
    if (label === "html" || label === "handlebars" || label === "razor") {
      return new htmlWorker();
    }
    if (label === "typescript" || label === "javascript") {
      return new tsWorker();
    }
    return new editorWorker();
  },
};

// Track if Monaco has been configured
let monacoConfigured = false;

// Initialize Slate language and themes once
if (!monacoConfigured) {
  registerSlateLanguage(monaco);
  registerSlateThemes(monaco);
  monacoConfigured = true;
}

export function CodeEditor({ panelId, instanceId }: PanelProps) {
  const { currentFile, markModified, isModified } = useFiles();
  const { theme } = useUI();
  const [content, setContent] = useState("");
  const [originalContent, setOriginalContent] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const emit = useEmit();

  const loadFile = useCallback(async (path: string) => {
    setIsLoading(true);
    const result = await window.electronAPI?.readFile(path);
    if (result?.success && result.data !== undefined) {
      setContent(result.data);
      setOriginalContent(result.data);
      markModified(path, false);
    }
    setIsLoading(false);
  }, [markModified]);

  // Load file content when current file changes
  useEffect(() => {
    if (currentFile) {
      loadFile(currentFile);
    } else {
      setContent("");
      setOriginalContent("");
    }
  }, [currentFile, loadFile]);

  // Create Monaco editor
  useEffect(() => {
    if (!containerRef.current || !currentFile || isLoading) return;

    // Dispose existing editor
    if (editorRef.current) {
      editorRef.current.dispose();
    }

    // Create new editor
    const editor = monaco.editor.create(containerRef.current, {
      value: content,
      language: getLanguage(currentFile),
      theme: getEditorTheme(theme, currentFile),
      minimap: { enabled: true },
      fontSize: 14,
      lineNumbers: "on",
      tabSize: 2,
      insertSpaces: true,
      wordWrap: "on",
      automaticLayout: true,
      scrollBeyondLastLine: false,
      renderWhitespace: "selection",
      bracketPairColorization: { enabled: true },
      guides: {
        indentation: true,
        bracketPairs: true,
      },
      folding: true,
      foldingStrategy: "indentation",
      padding: { top: 8, bottom: 8 },
    });

    editorRef.current = editor;

    // Add save command
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, () => {
      handleSave();
    });

    // Handle content changes
    editor.onDidChangeModelContent(() => {
      const newContent = editor.getValue();
      setContent(newContent);
      if (currentFile) {
        markModified(currentFile, newContent !== originalContent);
      }
      emit("editor:content-changed", { path: currentFile, content: newContent });
    });

    // Focus the editor
    editor.focus();

    return () => {
      editor.dispose();
    };
  }, [containerRef.current, currentFile, isLoading]);

  // Update content when file is loaded
  useEffect(() => {
    if (editorRef.current && !isLoading) {
      const currentValue = editorRef.current.getValue();
      if (currentValue !== content) {
        editorRef.current.setValue(content);
      }
    }
  }, [content, isLoading]);

  // Update theme when it changes
  useEffect(() => {
    if (currentFile) {
      monaco.editor.setTheme(getEditorTheme(theme, currentFile));
    }
  }, [theme, currentFile]);

  // Handle external file changes
  useMessageBus("file:external-change", async (data: { event: string; path: string }) => {
    if (!currentFile || data.path !== currentFile) return;

    // File was deleted externally
    if (data.event === "unlink") {
      emit("status:update", { text: `File "${currentFile.split("/").pop()}" was deleted externally` });
      return;
    }

    // File was modified externally
    if (data.event === "change") {
      const hasLocalChanges = isModified(currentFile);

      if (hasLocalChanges) {
        // Prompt user to decide
        const result = await window.electronAPI?.showMessage({
          type: "question",
          message: "File Changed Externally",
          detail: `The file "${currentFile.split("/").pop()}" has been modified outside the editor. Do you want to reload it? Your unsaved changes will be lost.`,
          buttons: ["Reload", "Keep My Changes"],
          defaultId: 1,
          cancelId: 1,
        });

        if (result?.response === 0) {
          loadFile(currentFile);
        }
      } else {
        // No local changes, reload automatically
        loadFile(currentFile);
        emit("status:update", { text: "File reloaded (modified externally)" });
      }
    }
  });

  const handleSave = async () => {
    if (!currentFile) return;

    const currentContent = editorRef.current?.getValue() || content;
    const result = await window.electronAPI?.writeFile(currentFile, currentContent);
    if (result?.success) {
      setOriginalContent(currentContent);
      markModified(currentFile, false);
      emit("status:update", { text: `Saved ${currentFile}` });
      emit("file:saved", { path: currentFile });
    } else {
      emit("console:error", { text: `Failed to save: ${result?.error}` });
    }
  };

  if (!currentFile) {
    return (
      <div className="code-editor empty">
        <div className="empty-state">
          <span className="empty-icon">📝</span>
          <p>No file open</p>
          <p className="hint">Select a file from the Project panel</p>
        </div>
      </div>
    );
  }

  return (
    <div className="code-editor">
      <div className="editor-header">
        <span className="filename">
          {currentFile.split("/").pop()}
          {isModified(currentFile) && <span className="modified-indicator">●</span>}
        </span>
        <div className="editor-actions">
          <button className="editor-btn" onClick={handleSave} title="Save (Cmd+S)">
            💾
          </button>
        </div>
      </div>
      <div className="editor-content">
        {isLoading ? (
          <div className="editor-loading">Loading...</div>
        ) : (
          <div ref={containerRef} className="monaco-container" />
        )}
      </div>
    </div>
  );
}

// Helper functions
function getLanguage(path: string | null): string {
  if (!path) return "plaintext";
  const ext = path.split(".").pop()?.toLowerCase();
  switch (ext) {
    case "sl":
      return "slate";
    case "json":
      return "json";
    case "md":
      return "markdown";
    case "ts":
    case "tsx":
      return "typescript";
    case "js":
    case "jsx":
      return "javascript";
    case "css":
      return "css";
    case "html":
      return "html";
    case "glsl":
    case "frag":
    case "vert":
      return "glsl";
    default:
      return "plaintext";
  }
}

function getEditorTheme(theme: string, path: string | null): string {
  const lang = getLanguage(path);
  if (lang === "slate") {
    return theme === "dark" ? "slate-dark" : "slate-light";
  }
  return theme === "dark" ? "vs-dark" : "vs";
}
