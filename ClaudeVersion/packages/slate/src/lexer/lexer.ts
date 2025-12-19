// Slate Lexer - Tokenizes Slate source code

import { Token, TokenType, KEYWORDS } from "@oort/core";

export class Lexer {
  private source: string;
  private tokens: Token[] = [];
  private start = 0;
  private current = 0;
  private line = 1;
  private column = 1;
  private lineStart = 0;
  private indentStack: number[] = [0];
  private atLineStart = true;
  private pendingTokens: Token[] = [];
  private bracketDepth = 0; // Track depth of (), [], {} for implicit line continuation

  // For string interpolation
  private interpStringStack: number[] = []; // Stack of brace depths when entering interpolation
  private interpBraceDepth = 0; // Brace depth within current interpolation expression

  constructor(source: string) {
    this.source = source;
  }

  tokenize(): Token[] {
    while (!this.isAtEnd()) {
      this.start = this.current;
      this.scanToken();
    }

    // Emit remaining DEDENTs at end of file
    while (this.indentStack.length > 1) {
      this.indentStack.pop();
      this.tokens.push(this.makeToken(TokenType.DEDENT, ""));
    }

    this.tokens.push(this.makeToken(TokenType.EOF, ""));
    return this.tokens;
  }

  private scanToken(): void {
    // Handle indentation at start of line
    if (this.atLineStart) {
      this.handleIndentation();
      if (this.isAtEnd()) return;
      // Reset start position after consuming indentation whitespace
      this.start = this.current;
    }

    const c = this.advance();

    switch (c) {
      case "(":
        this.bracketDepth++;
        this.addToken(TokenType.LEFT_PAREN);
        break;
      case ")":
        this.bracketDepth = Math.max(0, this.bracketDepth - 1);
        this.addToken(TokenType.RIGHT_PAREN);
        break;
      case "[":
        this.bracketDepth++;
        this.addToken(TokenType.LEFT_BRACKET);
        break;
      case "]":
        this.bracketDepth = Math.max(0, this.bracketDepth - 1);
        this.addToken(TokenType.RIGHT_BRACKET);
        break;
      case "{":
        this.bracketDepth++;
        // Track brace depth for interpolation
        if (this.interpStringStack.length > 0) {
          this.interpBraceDepth++;
        }
        this.addToken(TokenType.LEFT_BRACE);
        break;
      case "}":
        // Check if this closes an interpolation expression
        if (this.interpStringStack.length > 0 && this.interpBraceDepth === 0) {
          // Continue scanning the string from here
          this.continueInterpolatedString();
          break;
        }
        this.bracketDepth = Math.max(0, this.bracketDepth - 1);
        if (this.interpStringStack.length > 0) {
          this.interpBraceDepth = Math.max(0, this.interpBraceDepth - 1);
        }
        this.addToken(TokenType.RIGHT_BRACE);
        break;
      case ",":
        this.addToken(TokenType.COMMA);
        break;
      case ":":
        this.addToken(TokenType.COLON);
        break;
      case "+":
        this.addToken(TokenType.PLUS);
        break;
      case "*":
        this.addToken(TokenType.STAR);
        break;
      case "/":
        this.addToken(TokenType.SLASH);
        break;
      case "%":
        this.addToken(TokenType.PERCENT);
        break;
      case "|":
        this.addToken(TokenType.PIPE);
        break;
      case "_":
        if (this.isAlphaNumeric(this.peek())) {
          this.identifier();
        } else {
          this.addToken(TokenType.UNDERSCORE);
        }
        break;
      case "-":
        if (this.match(">")) {
          this.addToken(TokenType.THIN_ARROW);
        } else {
          this.addToken(TokenType.MINUS);
        }
        break;
      case ".":
        if (this.match(".")) {
          if (this.match("=")) {
            this.addToken(TokenType.DOT_DOT_EQUAL);
          } else {
            this.addToken(TokenType.DOT_DOT);
          }
        } else {
          this.addToken(TokenType.DOT);
        }
        break;
      case "=":
        if (this.match("=")) {
          this.addToken(TokenType.EQUAL_EQUAL);
        } else if (this.match(">")) {
          this.addToken(TokenType.ARROW);
        } else {
          this.addToken(TokenType.EQUAL);
        }
        break;
      case "!":
        if (this.match("=")) {
          this.addToken(TokenType.BANG_EQUAL);
        } else {
          this.error("Unexpected character '!'");
        }
        break;
      case "<":
        if (this.match("=")) {
          this.addToken(TokenType.LESS_EQUAL);
        } else {
          this.addToken(TokenType.LESS);
        }
        break;
      case ">":
        if (this.match("=")) {
          this.addToken(TokenType.GREATER_EQUAL);
        } else {
          this.addToken(TokenType.GREATER);
        }
        break;
      case "#":
        if (this.isHexDigit(this.peek())) {
          this.hexColor();
        } else {
          this.comment();
        }
        break;
      case "@":
        this.addToken(TokenType.AT);
        break;
      case "?":
        if (this.match("?")) {
          this.addToken(TokenType.QUESTION_QUESTION);
        } else if (this.match(".")) {
          this.addToken(TokenType.QUESTION_DOT);
        } else if (this.match("[")) {
          this.addToken(TokenType.QUESTION_BRACKET);
        } else if (this.match("(")) {
          this.addToken(TokenType.QUESTION_PAREN);
        } else {
          this.error("Unexpected character '?'");
        }
        break;
      case " ":
      case "\t":
      case "\r":
        // Ignore whitespace (not at line start)
        break;
      case "\n":
        this.newline();
        break;
      case '"':
        this.string();
        break;
      default:
        if (this.isDigit(c)) {
          this.number();
        } else if (this.isAlpha(c)) {
          this.identifier();
        } else {
          this.error(`Unexpected character '${c}'`);
        }
    }
  }

  private handleIndentation(): void {
    // Skip indentation handling when inside brackets (implicit line continuation)
    if (this.bracketDepth > 0) {
      this.atLineStart = false;
      // Still need to skip whitespace
      while (!this.isAtEnd() && (this.peek() === " " || this.peek() === "\t")) {
        this.advance();
      }
      return;
    }

    let indent = 0;

    // Skip blank lines and comments
    while (!this.isAtEnd()) {
      const lineIndent = this.countIndentation();

      // Check if this is a blank line or comment-only line
      if (this.peek() === "\n" || this.isAtEnd()) {
        this.advance(); // consume newline
        this.line++;
        this.column = 1;
        this.lineStart = this.current;
        continue;
      }
      if (this.peek() === "#" && !this.isHexDigit(this.peekNext())) {
        // Comment line - skip to end
        while (!this.isAtEnd() && this.peek() !== "\n") {
          this.advance();
        }
        if (this.peek() === "\n") {
          this.advance();
          this.line++;
          this.column = 1;
          this.lineStart = this.current;
        }
        continue;
      }

      indent = lineIndent;
      break;
    }

    this.atLineStart = false;

    const currentIndent = this.indentStack[this.indentStack.length - 1];

    if (indent > currentIndent) {
      this.indentStack.push(indent);
      this.tokens.push(this.makeToken(TokenType.INDENT, ""));
    } else if (indent < currentIndent) {
      while (
        this.indentStack.length > 1 &&
        this.indentStack[this.indentStack.length - 1] > indent
      ) {
        this.indentStack.pop();
        this.tokens.push(this.makeToken(TokenType.DEDENT, ""));
      }
    }
  }

  private countIndentation(): number {
    let count = 0;
    while (!this.isAtEnd() && (this.peek() === " " || this.peek() === "\t")) {
      if (this.peek() === "\t") {
        count += 4; // Tab = 4 spaces
      } else {
        count++;
      }
      this.advance();
    }
    return count;
  }

  private newline(): void {
    // Skip NEWLINE token when inside brackets (implicit line continuation)
    if (this.bracketDepth === 0) {
      this.tokens.push(this.makeToken(TokenType.NEWLINE, "\n"));
    }
    this.line++;
    this.column = 1;
    this.lineStart = this.current;
    this.atLineStart = true;
  }

  private comment(): void {
    while (!this.isAtEnd() && this.peek() !== "\n") {
      this.advance();
    }
  }

  private string(): void {
    const stringStart = this.start + 1; // After opening quote
    let hasInterpolation = false;

    while (!this.isAtEnd() && this.peek() !== '"') {
      if (this.peek() === "\n") {
        this.line++;
        this.column = 0;
      }
      // Check for escape sequence
      if (this.peek() === "\\" && this.peekNext() !== "\0") {
        this.advance(); // consume backslash
        this.advance(); // consume escaped char
        continue;
      }
      // Check for interpolation start
      if (this.peek() === "{") {
        hasInterpolation = true;
        // Extract the string part before {
        const raw = this.source.slice(stringStart, this.current);
        const value = this.unescapeString(raw);

        // Determine token type based on whether we're already in interpolation
        const tokenType = this.interpStringStack.length > 0
          ? TokenType.STRING_INTERP_MIDDLE
          : TokenType.STRING_INTERP_START;

        this.addTokenLiteral(tokenType, value);

        // Push to interpolation stack (store that we're in interpolation mode)
        this.interpStringStack.push(this.interpBraceDepth);
        this.interpBraceDepth = 0;

        this.advance(); // consume the '{'
        return; // Let the main loop lex the expression
      }
      this.advance();
    }

    if (this.isAtEnd()) {
      this.error("Unterminated string");
      return;
    }

    this.advance(); // closing quote

    // Extract the string value, handling escape sequences
    const raw = this.source.slice(stringStart, this.current - 1);
    const value = this.unescapeString(raw);

    // Determine token type
    if (this.interpStringStack.length > 0) {
      // This is the end of an interpolated string
      this.interpStringStack.pop();
      this.addTokenLiteral(TokenType.STRING_INTERP_END, value);
    } else {
      // Regular string
      this.addTokenLiteral(TokenType.STRING, value);
    }
  }

  // Continue scanning an interpolated string after a } closes an expression
  private continueInterpolatedString(): void {
    // We just consumed }, now continue scanning the string
    const stringStart = this.current;

    while (!this.isAtEnd() && this.peek() !== '"') {
      if (this.peek() === "\n") {
        this.line++;
        this.column = 0;
      }
      // Check for escape sequence
      if (this.peek() === "\\" && this.peekNext() !== "\0") {
        this.advance(); // consume backslash
        this.advance(); // consume escaped char
        continue;
      }
      // Check for another interpolation
      if (this.peek() === "{") {
        // Extract the string part between } and {
        const raw = this.source.slice(stringStart, this.current);
        const value = this.unescapeString(raw);

        this.addTokenLiteral(TokenType.STRING_INTERP_MIDDLE, value);

        // Don't need to push to stack, already in interpolation mode
        this.interpBraceDepth = 0;

        this.advance(); // consume the '{'
        return; // Let the main loop lex the expression
      }
      this.advance();
    }

    if (this.isAtEnd()) {
      this.error("Unterminated string");
      return;
    }

    this.advance(); // closing quote

    // Extract the final string part
    const raw = this.source.slice(stringStart, this.current - 1);
    const value = this.unescapeString(raw);

    // Pop from the interpolation stack
    this.interpStringStack.pop();
    this.addTokenLiteral(TokenType.STRING_INTERP_END, value);
  }

  private unescapeString(s: string): string {
    let result = "";
    let i = 0;
    while (i < s.length) {
      if (s[i] === "\\" && i + 1 < s.length) {
        switch (s[i + 1]) {
          case "n":
            result += "\n";
            i += 2;
            break;
          case "t":
            result += "\t";
            i += 2;
            break;
          case "r":
            result += "\r";
            i += 2;
            break;
          case '"':
            result += '"';
            i += 2;
            break;
          case "\\":
            result += "\\";
            i += 2;
            break;
          default:
            result += s[i];
            i++;
        }
      } else {
        result += s[i];
        i++;
      }
    }
    return result;
  }

  private number(): void {
    while (this.isDigit(this.peek())) {
      this.advance();
    }

    // Look for decimal part
    if (this.peek() === "." && this.isDigit(this.peekNext())) {
      this.advance(); // consume '.'
      while (this.isDigit(this.peek())) {
        this.advance();
      }
    }

    const value = parseFloat(this.source.slice(this.start, this.current));
    this.addTokenLiteral(TokenType.NUMBER, value);
  }

  private hexColor(): void {
    // Already consumed '#', now read hex digits
    while (this.isHexDigit(this.peek())) {
      this.advance();
    }

    const hex = this.source.slice(this.start, this.current);
    this.addTokenLiteral(TokenType.HASH, hex);
  }

  private identifier(): void {
    while (this.isAlphaNumeric(this.peek())) {
      this.advance();
    }

    const text = this.source.slice(this.start, this.current);
    const type = KEYWORDS[text] ?? TokenType.IDENTIFIER;
    this.addToken(type);
  }

  private match(expected: string): boolean {
    if (this.isAtEnd()) return false;
    if (this.source[this.current] !== expected) return false;
    this.current++;
    this.column++;
    return true;
  }

  private peek(): string {
    if (this.isAtEnd()) return "\0";
    return this.source[this.current];
  }

  private peekNext(): string {
    if (this.current + 1 >= this.source.length) return "\0";
    return this.source[this.current + 1];
  }

  private advance(): string {
    const c = this.source[this.current++];
    this.column++;
    return c;
  }

  private isAtEnd(): boolean {
    return this.current >= this.source.length;
  }

  private isDigit(c: string): boolean {
    return c >= "0" && c <= "9";
  }

  private isHexDigit(c: string): boolean {
    return (
      (c >= "0" && c <= "9") ||
      (c >= "a" && c <= "f") ||
      (c >= "A" && c <= "F")
    );
  }

  private isAlpha(c: string): boolean {
    return (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c === "_";
  }

  private isAlphaNumeric(c: string): boolean {
    return this.isAlpha(c) || this.isDigit(c);
  }

  private addToken(type: TokenType): void {
    this.addTokenLiteral(type, null);
  }

  private addTokenLiteral(type: TokenType, literal: unknown): void {
    const lexeme = this.source.slice(this.start, this.current);
    this.tokens.push({
      type,
      lexeme,
      literal,
      line: this.line,
      column: this.column - lexeme.length,
    });
  }

  private makeToken(type: TokenType, lexeme: string): Token {
    return {
      type,
      lexeme,
      literal: null,
      line: this.line,
      column: this.column,
    };
  }

  private error(message: string): void {
    this.tokens.push({
      type: TokenType.ERROR,
      lexeme: message,
      literal: null,
      line: this.line,
      column: this.column,
    });
  }
}
