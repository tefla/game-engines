// Slate Syntax Highlighting
// Provides token classification for syntax highlighting

import { TokenType, KEYWORDS } from "@oort/core";
import { Lexer } from "@oort/slate/src/lexer";

// Token categories for styling
export type TokenCategory =
  | "keyword"
  | "operator"
  | "number"
  | "string"
  | "identifier"
  | "function"
  | "comment"
  | "punctuation"
  | "signal"
  | "color"
  | "type"
  | "property"
  | "error"
  | "none";

// A highlighted token with position and category
export interface HighlightToken {
  text: string;
  category: TokenCategory;
  start: number;
  end: number;
  line: number;
  column: number;
}

// Map token types to highlight categories
function getCategory(type: TokenType, lexeme: string): TokenCategory {
  switch (type) {
    // Keywords
    case TokenType.LET:
    case TokenType.VAR:
    case TokenType.FN:
    case TokenType.IF:
    case TokenType.ELSE:
    case TokenType.MATCH:
    case TokenType.ON:
    case TokenType.EXTEND:
    case TokenType.TRUE:
    case TokenType.FALSE:
    case TokenType.NOT:
    case TokenType.AND:
    case TokenType.OR:
    case TokenType.EMIT:
    case TokenType.IMPORT:
    case TokenType.LOOP:
    case TokenType.FOR:
    case TokenType.IN:
    case TokenType.BY:
    case TokenType.YIELD:
    case TokenType.SPAWN:
    case TokenType.WAIT:
    case TokenType.TYPE:
    case TokenType.WITH:
      return "keyword";

    // Operators
    case TokenType.PLUS:
    case TokenType.MINUS:
    case TokenType.STAR:
    case TokenType.SLASH:
    case TokenType.PERCENT:
    case TokenType.EQUAL:
    case TokenType.EQUAL_EQUAL:
    case TokenType.BANG_EQUAL:
    case TokenType.LESS:
    case TokenType.LESS_EQUAL:
    case TokenType.GREATER:
    case TokenType.GREATER_EQUAL:
    case TokenType.ARROW:
    case TokenType.THIN_ARROW:
    case TokenType.DOT_DOT:
    case TokenType.PIPE:
      return "operator";

    // Literals
    case TokenType.NUMBER:
      return "number";

    case TokenType.STRING:
      return "string";

    case TokenType.HASH:
      return "color";

    // Signals
    case TokenType.AT:
      return "signal";

    // Punctuation
    case TokenType.LEFT_PAREN:
    case TokenType.RIGHT_PAREN:
    case TokenType.LEFT_BRACKET:
    case TokenType.RIGHT_BRACKET:
    case TokenType.LEFT_BRACE:
    case TokenType.RIGHT_BRACE:
    case TokenType.COMMA:
    case TokenType.COLON:
    case TokenType.DOT:
    case TokenType.UNDERSCORE:
      return "punctuation";

    // Identifier (could be function, type, or variable)
    case TokenType.IDENTIFIER:
      // Check if it looks like a type (PascalCase)
      if (lexeme[0] === lexeme[0].toUpperCase() && /^[A-Z]/.test(lexeme)) {
        return "type";
      }
      return "identifier";

    case TokenType.ERROR:
      return "error";

    default:
      return "none";
  }
}

// Tokenize source code and return highlight tokens
export function highlightSlate(source: string): HighlightToken[] {
  const lexer = new Lexer(source);
  const tokens = lexer.tokenize();
  const result: HighlightToken[] = [];

  // Track position in source for calculating offsets
  let currentPos = 0;

  for (const token of tokens) {
    if (token.type === TokenType.EOF) break;
    if (token.type === TokenType.INDENT || token.type === TokenType.DEDENT) continue;
    if (token.type === TokenType.NEWLINE) continue;

    // Find the token in source (handle whitespace gaps)
    const tokenStart = source.indexOf(token.lexeme, currentPos);
    if (tokenStart === -1) continue;

    const category = getCategory(token.type, token.lexeme);

    result.push({
      text: token.lexeme,
      category,
      start: tokenStart,
      end: tokenStart + token.lexeme.length,
      line: token.line,
      column: token.column,
    });

    currentPos = tokenStart + token.lexeme.length;
  }

  // Add comments (lexer skips them, so we need to find them manually)
  addComments(source, result);

  // Sort by position
  result.sort((a, b) => a.start - b.start);

  return result;
}

// Find and add comment tokens
function addComments(source: string, tokens: HighlightToken[]): void {
  const lines = source.split("\n");
  let pos = 0;

  for (let lineNum = 0; lineNum < lines.length; lineNum++) {
    const line = lines[lineNum];
    const commentIndex = findCommentStart(line);

    if (commentIndex !== -1) {
      const commentText = line.slice(commentIndex);
      tokens.push({
        text: commentText,
        category: "comment",
        start: pos + commentIndex,
        end: pos + line.length,
        line: lineNum + 1,
        column: commentIndex + 1,
      });
    }

    pos += line.length + 1; // +1 for newline
  }
}

// Find comment start, handling strings
function findCommentStart(line: string): number {
  let inString = false;
  for (let i = 0; i < line.length; i++) {
    if (line[i] === '"' && (i === 0 || line[i - 1] !== "\\")) {
      inString = !inString;
    }
    if (!inString && line[i] === "#") {
      // Make sure it's not a hex color
      const rest = line.slice(i + 1);
      if (!/^[0-9a-fA-F]{3,6}(?![0-9a-fA-F])/.test(rest)) {
        return i;
      }
    }
  }
  return -1;
}

// Convert highlighted tokens to HTML
export function highlightToHtml(source: string, tokens: HighlightToken[]): string {
  const parts: string[] = [];
  let lastEnd = 0;

  for (const token of tokens) {
    // Add any text between tokens (whitespace)
    if (token.start > lastEnd) {
      parts.push(escapeHtml(source.slice(lastEnd, token.start)));
    }

    // Add the token with its class
    if (token.category !== "none") {
      parts.push(`<span class="slate-${token.category}">${escapeHtml(token.text)}</span>`);
    } else {
      parts.push(escapeHtml(token.text));
    }

    lastEnd = token.end;
  }

  // Add any remaining text
  if (lastEnd < source.length) {
    parts.push(escapeHtml(source.slice(lastEnd)));
  }

  return parts.join("");
}

function escapeHtml(text: string): string {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

// CSS styles for syntax highlighting
export const SLATE_HIGHLIGHT_CSS = `
.slate-keyword { color: #c586c0; font-weight: bold; }
.slate-operator { color: #d4d4d4; }
.slate-number { color: #b5cea8; }
.slate-string { color: #ce9178; }
.slate-identifier { color: #9cdcfe; }
.slate-function { color: #dcdcaa; }
.slate-comment { color: #6a9955; font-style: italic; }
.slate-punctuation { color: #d4d4d4; }
.slate-signal { color: #4ec9b0; }
.slate-color { color: #ce9178; }
.slate-type { color: #4ec9b0; }
.slate-property { color: #9cdcfe; }
.slate-error { color: #f44747; text-decoration: wavy underline; }
`;

// Dark theme variant
export const SLATE_HIGHLIGHT_CSS_DARK = `
.slate-editor {
  background-color: #1e1e1e;
  color: #d4d4d4;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  font-size: 14px;
  line-height: 1.5;
  padding: 16px;
  border-radius: 4px;
}
${SLATE_HIGHLIGHT_CSS}
`;

// Light theme variant
export const SLATE_HIGHLIGHT_CSS_LIGHT = `
.slate-editor {
  background-color: #ffffff;
  color: #1e1e1e;
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  font-size: 14px;
  line-height: 1.5;
  padding: 16px;
  border-radius: 4px;
  border: 1px solid #e0e0e0;
}
.slate-keyword { color: #0000ff; font-weight: bold; }
.slate-operator { color: #1e1e1e; }
.slate-number { color: #098658; }
.slate-string { color: #a31515; }
.slate-identifier { color: #001080; }
.slate-function { color: #795e26; }
.slate-comment { color: #008000; font-style: italic; }
.slate-punctuation { color: #1e1e1e; }
.slate-signal { color: #267f99; }
.slate-color { color: #a31515; }
.slate-type { color: #267f99; }
.slate-property { color: #001080; }
.slate-error { color: #e51400; text-decoration: wavy underline; }
`;
