// Slate Code Editor
// Provides an editor model that integrates with VFS and VCS

import type { VirtualFileSystem } from "@oort/engine";
import type { History } from "@oort/vcs";
import { runSlate } from "@oort/slate";
import type { SlateValue, SlateError, SourceLocation } from "@oort/core";
import { highlightSlate, highlightToHtml, type HighlightToken } from "./syntax";

// Editor state
export interface EditorState {
  content: string;
  cursorLine: number;
  cursorColumn: number;
  selectionStart?: { line: number; column: number };
  selectionEnd?: { line: number; column: number };
  isDirty: boolean;
}

// Editor diagnostics (errors, warnings)
export interface Diagnostic {
  severity: "error" | "warning" | "info";
  message: string;
  line: number;
  column: number;
  endLine?: number;
  endColumn?: number;
  hint?: string;
}

// Editor events
export interface EditorEvents {
  onChange?: (content: string) => void;
  onSave?: (path: string, content: string) => void;
  onRun?: (result: SlateValue) => void;
  onError?: (error: SlateError | Error) => void;
  onDiagnostics?: (diagnostics: Diagnostic[]) => void;
}

// Editor options
export interface EditorOptions {
  autoSave?: boolean;
  autoSaveDelay?: number;
  checkOnType?: boolean;
  checkDelay?: number;
  tabSize?: number;
  insertSpaces?: boolean;
}

const DEFAULT_OPTIONS: EditorOptions = {
  autoSave: false,
  autoSaveDelay: 1000,
  checkOnType: true,
  checkDelay: 500,
  tabSize: 4,
  insertSpaces: true,
};

// The Editor class
export class SlateEditor {
  private state: EditorState;
  private filePath: string | null = null;
  private vfs: VirtualFileSystem | null = null;
  private history: History | null = null;
  private events: EditorEvents;
  private options: EditorOptions;
  private checkTimer: ReturnType<typeof setTimeout> | null = null;
  private saveTimer: ReturnType<typeof setTimeout> | null = null;
  private diagnostics: Diagnostic[] = [];

  constructor(
    events: EditorEvents = {},
    options: Partial<EditorOptions> = {}
  ) {
    this.events = events;
    this.options = { ...DEFAULT_OPTIONS, ...options };
    this.state = {
      content: "",
      cursorLine: 1,
      cursorColumn: 1,
      isDirty: false,
    };
  }

  // Connect to VFS for file operations
  connectVfs(vfs: VirtualFileSystem): void {
    this.vfs = vfs;
  }

  // Connect to VCS for version control
  connectHistory(history: History): void {
    this.history = history;
  }

  // Open a file from VFS
  open(path: string): boolean {
    if (!this.vfs) {
      throw new Error("VFS not connected");
    }

    try {
      const content = this.vfs.read(path);
      this.state.content = content;
      this.state.cursorLine = 1;
      this.state.cursorColumn = 1;
      this.state.isDirty = false;
      this.filePath = path;
      this.diagnostics = [];
      this.scheduleCheck();
      return true;
    } catch (e) {
      this.events.onError?.(e as Error);
      return false;
    }
  }

  // Save current content to VFS
  save(path?: string): boolean {
    const targetPath = path ?? this.filePath;
    if (!targetPath) {
      throw new Error("No file path specified");
    }
    if (!this.vfs) {
      throw new Error("VFS not connected");
    }

    try {
      this.vfs.write(targetPath, this.state.content);
      this.state.isDirty = false;
      this.filePath = targetPath;

      // Commit to history if connected
      if (this.history) {
        this.history.commit(`Save ${targetPath}`);
      }

      this.events.onSave?.(targetPath, this.state.content);
      return true;
    } catch (e) {
      this.events.onError?.(e as Error);
      return false;
    }
  }

  // Get current content
  getContent(): string {
    return this.state.content;
  }

  // Set content directly
  setContent(content: string): void {
    this.state.content = content;
    this.state.isDirty = true;
    this.events.onChange?.(content);
    this.scheduleCheck();
    this.scheduleAutoSave();
  }

  // Insert text at cursor position
  insert(text: string): void {
    const lines = this.state.content.split("\n");
    const lineIndex = this.state.cursorLine - 1;
    const colIndex = this.state.cursorColumn - 1;

    if (lineIndex < lines.length) {
      const line = lines[lineIndex];
      lines[lineIndex] =
        line.slice(0, colIndex) + text + line.slice(colIndex);

      // Handle newlines in inserted text
      if (text.includes("\n")) {
        const insertedLines = text.split("\n");
        const before = line.slice(0, colIndex);
        const after = line.slice(colIndex);

        lines.splice(
          lineIndex,
          1,
          before + insertedLines[0],
          ...insertedLines.slice(1, -1),
          insertedLines[insertedLines.length - 1] + after
        );

        this.state.cursorLine += insertedLines.length - 1;
        this.state.cursorColumn = insertedLines[insertedLines.length - 1].length + 1;
      } else {
        this.state.cursorColumn += text.length;
      }
    }

    this.setContent(lines.join("\n"));
  }

  // Delete character before cursor
  backspace(): void {
    const lines = this.state.content.split("\n");
    const lineIndex = this.state.cursorLine - 1;
    const colIndex = this.state.cursorColumn - 1;

    if (colIndex > 0) {
      // Delete character on current line
      const line = lines[lineIndex];
      lines[lineIndex] = line.slice(0, colIndex - 1) + line.slice(colIndex);
      this.state.cursorColumn--;
    } else if (lineIndex > 0) {
      // Merge with previous line
      const currentLine = lines[lineIndex];
      const prevLine = lines[lineIndex - 1];
      lines[lineIndex - 1] = prevLine + currentLine;
      lines.splice(lineIndex, 1);
      this.state.cursorLine--;
      this.state.cursorColumn = prevLine.length + 1;
    }

    this.setContent(lines.join("\n"));
  }

  // Handle tab insertion
  tab(): void {
    if (this.options.insertSpaces) {
      this.insert(" ".repeat(this.options.tabSize!));
    } else {
      this.insert("\t");
    }
  }

  // Move cursor
  moveCursor(line: number, column: number): void {
    const lines = this.state.content.split("\n");
    this.state.cursorLine = Math.max(1, Math.min(line, lines.length));

    const currentLineLength = lines[this.state.cursorLine - 1]?.length ?? 0;
    this.state.cursorColumn = Math.max(1, Math.min(column, currentLineLength + 1));
  }

  // Get cursor position
  getCursor(): { line: number; column: number } {
    return {
      line: this.state.cursorLine,
      column: this.state.cursorColumn,
    };
  }

  // Get editor state
  getState(): Readonly<EditorState> {
    return { ...this.state };
  }

  // Get syntax-highlighted HTML
  getHighlightedHtml(): string {
    const tokens = highlightSlate(this.state.content);
    return highlightToHtml(this.state.content, tokens);
  }

  // Get syntax tokens
  getTokens(): HighlightToken[] {
    return highlightSlate(this.state.content);
  }

  // Run the current content
  run(): SlateValue | null {
    try {
      const result = runSlate(this.state.content);
      this.events.onRun?.(result);
      return result;
    } catch (e) {
      this.events.onError?.(e as Error);
      return null;
    }
  }

  // Check for errors
  check(): Diagnostic[] {
    this.diagnostics = [];

    try {
      // Try to parse and evaluate
      runSlate(this.state.content);
    } catch (e: any) {
      const diag: Diagnostic = {
        severity: "error",
        message: e.message || "Unknown error",
        line: e.line || 1,
        column: e.column || 1,
        hint: e.hint,
      };
      this.diagnostics.push(diag);
    }

    this.events.onDiagnostics?.(this.diagnostics);
    return this.diagnostics;
  }

  // Get current diagnostics
  getDiagnostics(): Diagnostic[] {
    return [...this.diagnostics];
  }

  // Is the content modified?
  isDirty(): boolean {
    return this.state.isDirty;
  }

  // Get current file path
  getFilePath(): string | null {
    return this.filePath;
  }

  // Undo (if connected to history)
  undo(): boolean {
    if (!this.history) return false;
    const success = this.history.undo();
    if (success && this.filePath && this.vfs) {
      try {
        this.state.content = this.vfs.read(this.filePath);
        this.state.isDirty = false;
        this.events.onChange?.(this.state.content);
        this.scheduleCheck();
      } catch {
        // File might not exist after undo
      }
    }
    return success;
  }

  // Redo (if connected to history)
  redo(): boolean {
    if (!this.history) return false;
    const success = this.history.redo();
    if (success && this.filePath && this.vfs) {
      try {
        this.state.content = this.vfs.read(this.filePath);
        this.state.isDirty = false;
        this.events.onChange?.(this.state.content);
        this.scheduleCheck();
      } catch {
        // File might not exist after redo
      }
    }
    return success;
  }

  // Schedule a syntax check
  private scheduleCheck(): void {
    if (!this.options.checkOnType) return;

    if (this.checkTimer) {
      clearTimeout(this.checkTimer);
    }

    this.checkTimer = setTimeout(() => {
      this.check();
      this.checkTimer = null;
    }, this.options.checkDelay);
  }

  // Schedule auto-save
  private scheduleAutoSave(): void {
    if (!this.options.autoSave || !this.filePath) return;

    if (this.saveTimer) {
      clearTimeout(this.saveTimer);
    }

    this.saveTimer = setTimeout(() => {
      this.save();
      this.saveTimer = null;
    }, this.options.autoSaveDelay);
  }

  // Clean up timers
  dispose(): void {
    if (this.checkTimer) clearTimeout(this.checkTimer);
    if (this.saveTimer) clearTimeout(this.saveTimer);
  }
}

// Create an editor instance
export function createEditor(
  events?: EditorEvents,
  options?: Partial<EditorOptions>
): SlateEditor {
  return new SlateEditor(events, options);
}
