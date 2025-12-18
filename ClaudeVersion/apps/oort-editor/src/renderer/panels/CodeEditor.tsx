import React, { useState, useEffect, useRef } from "react";
import { PanelProps } from "@/core/panel-registry";
import { useFiles } from "@/hooks/useStore";
import { useMessageBus, useEmit } from "@/hooks/useMessageBus";
import "./CodeEditor.css";

// Simple code editor (Monaco will be integrated later)
export function CodeEditor({ panelId, instanceId }: PanelProps) {
  const { currentFile, markModified, isModified } = useFiles();
  const [content, setContent] = useState("");
  const [originalContent, setOriginalContent] = useState("");
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const emit = useEmit();

  // Load file content when current file changes
  useEffect(() => {
    if (currentFile) {
      loadFile(currentFile);
    } else {
      setContent("");
      setOriginalContent("");
    }
  }, [currentFile]);

  const loadFile = async (path: string) => {
    const result = await window.electronAPI?.readFile(path);
    if (result?.success && result.data !== undefined) {
      setContent(result.data);
      setOriginalContent(result.data);
    }
  };

  const handleChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    const newContent = e.target.value;
    setContent(newContent);

    if (currentFile) {
      markModified(currentFile, newContent !== originalContent);
    }

    emit("editor:content-changed", { path: currentFile, content: newContent });
  };

  const handleSave = async () => {
    if (!currentFile) return;

    const result = await window.electronAPI?.writeFile(currentFile, content);
    if (result?.success) {
      setOriginalContent(content);
      markModified(currentFile, false);
      emit("status:update", { text: `Saved ${currentFile}` });
    } else {
      emit("console:error", { text: `Failed to save: ${result?.error}` });
    }
  };

  // Handle keyboard shortcuts
  const handleKeyDown = (e: React.KeyboardEvent) => {
    if ((e.metaKey || e.ctrlKey) && e.key === "s") {
      e.preventDefault();
      handleSave();
    }
  };

  // Handle tab key
  const handleTab = (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.key === "Tab") {
      e.preventDefault();
      const textarea = textareaRef.current;
      if (textarea) {
        const start = textarea.selectionStart;
        const end = textarea.selectionEnd;
        const newContent = content.substring(0, start) + "  " + content.substring(end);
        setContent(newContent);
        // Set cursor position after the tab
        setTimeout(() => {
          textarea.selectionStart = textarea.selectionEnd = start + 2;
        }, 0);
      }
    }
  };

  if (!currentFile) {
    return (
      <div className="code-editor empty">
        <p>No file open</p>
        <p className="hint">Select a file from the Project panel</p>
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
        <textarea
          ref={textareaRef}
          className="code-textarea"
          value={content}
          onChange={handleChange}
          onKeyDown={(e) => {
            handleKeyDown(e);
            handleTab(e);
          }}
          spellCheck={false}
          autoCapitalize="off"
          autoCorrect="off"
        />
      </div>
    </div>
  );
}
