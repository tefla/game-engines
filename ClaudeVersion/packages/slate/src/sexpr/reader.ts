/**
 * Minimal S-Expression Reader
 *
 * Parses S-expressions into a simple AST:
 * - Symbols: { type: "symbol", name: string }
 * - Numbers: { type: "number", value: number }
 * - Strings: { type: "string", value: string }
 * - Booleans: { type: "boolean", value: boolean }
 * - Lists: { type: "list", elements: SExpr[] }
 * - Nil: { type: "nil" }
 */

export type SExpr =
  | { type: "symbol"; name: string }
  | { type: "number"; value: number }
  | { type: "string"; value: string }
  | { type: "boolean"; value: boolean }
  | { type: "list"; elements: SExpr[] }
  | { type: "nil" };

// Constructors
export const sym = (name: string): SExpr => ({ type: "symbol", name });
export const num = (value: number): SExpr => ({ type: "number", value });
export const str = (value: string): SExpr => ({ type: "string", value });
export const bool = (value: boolean): SExpr => ({ type: "boolean", value });
export const list = (...elements: SExpr[]): SExpr => ({ type: "list", elements });
export const nil: SExpr = { type: "nil" };

// Type guards
export const isSymbol = (e: SExpr): e is { type: "symbol"; name: string } => e.type === "symbol";
export const isNumber = (e: SExpr): e is { type: "number"; value: number } => e.type === "number";
export const isString = (e: SExpr): e is { type: "string"; value: string } => e.type === "string";
export const isBoolean = (e: SExpr): e is { type: "boolean"; value: boolean } => e.type === "boolean";
export const isList = (e: SExpr): e is { type: "list"; elements: SExpr[] } => e.type === "list";
export const isNil = (e: SExpr): e is { type: "nil" } => e.type === "nil";

/**
 * Read S-expressions from source string
 */
export function read(source: string): SExpr[] {
  const reader = new Reader(source);
  return reader.readAll();
}

/**
 * Read a single S-expression
 */
export function readOne(source: string): SExpr {
  const reader = new Reader(source);
  return reader.readExpr();
}

class Reader {
  private pos = 0;
  private source: string;

  constructor(source: string) {
    this.source = source;
  }

  readAll(): SExpr[] {
    const exprs: SExpr[] = [];
    this.skipWhitespaceAndComments();
    while (!this.isAtEnd()) {
      exprs.push(this.readExpr());
      this.skipWhitespaceAndComments();
    }
    return exprs;
  }

  readExpr(): SExpr {
    this.skipWhitespaceAndComments();

    if (this.isAtEnd()) {
      throw new Error("Unexpected end of input");
    }

    const ch = this.peek();

    // List
    if (ch === "(") {
      return this.readList();
    }

    // Quote shorthand: 'x -> (quote x)
    if (ch === "'") {
      this.advance();
      return list(sym("quote"), this.readExpr());
    }

    // Quasiquote: `x -> (quasiquote x)
    if (ch === "`") {
      this.advance();
      return list(sym("quasiquote"), this.readExpr());
    }

    // Unquote: ,x -> (unquote x)
    if (ch === ",") {
      this.advance();
      if (this.peek() === "@") {
        this.advance();
        return list(sym("unquote-splicing"), this.readExpr());
      }
      return list(sym("unquote"), this.readExpr());
    }

    // String
    if (ch === '"') {
      return this.readString();
    }

    // Number or symbol starting with - or +
    if (ch === "-" || ch === "+") {
      const next = this.peekNext();
      if (next && /[0-9]/.test(next)) {
        return this.readNumber();
      }
      return this.readSymbol();
    }

    // Number
    if (/[0-9]/.test(ch)) {
      return this.readNumber();
    }

    // Boolean: #t, #f
    if (ch === "#") {
      return this.readHash();
    }

    // Symbol
    return this.readSymbol();
  }

  private readList(): SExpr {
    this.advance(); // consume '('
    const elements: SExpr[] = [];

    this.skipWhitespaceAndComments();
    while (!this.isAtEnd() && this.peek() !== ")") {
      elements.push(this.readExpr());
      this.skipWhitespaceAndComments();
    }

    if (this.isAtEnd()) {
      throw new Error("Unclosed list");
    }
    this.advance(); // consume ')'

    return list(...elements);
  }

  private readString(): SExpr {
    this.advance(); // consume opening "
    let value = "";

    while (!this.isAtEnd() && this.peek() !== '"') {
      if (this.peek() === "\\") {
        this.advance();
        if (this.isAtEnd()) {
          throw new Error("Unterminated string escape");
        }
        const escaped = this.advance();
        switch (escaped) {
          case "n": value += "\n"; break;
          case "t": value += "\t"; break;
          case "r": value += "\r"; break;
          case "\\": value += "\\"; break;
          case '"': value += '"'; break;
          default: value += escaped;
        }
      } else {
        value += this.advance();
      }
    }

    if (this.isAtEnd()) {
      throw new Error("Unterminated string");
    }
    this.advance(); // consume closing "

    return str(value);
  }

  private readNumber(): SExpr {
    let numStr = "";

    // Optional sign
    if (this.peek() === "-" || this.peek() === "+") {
      numStr += this.advance();
    }

    // Integer part
    while (!this.isAtEnd() && /[0-9]/.test(this.peek())) {
      numStr += this.advance();
    }

    // Decimal part
    if (!this.isAtEnd() && this.peek() === "." && /[0-9]/.test(this.peekNext() || "")) {
      numStr += this.advance(); // consume '.'
      while (!this.isAtEnd() && /[0-9]/.test(this.peek())) {
        numStr += this.advance();
      }
    }

    return num(parseFloat(numStr));
  }

  private readSymbol(): SExpr {
    let name = "";
    while (!this.isAtEnd() && this.isSymbolChar(this.peek())) {
      name += this.advance();
    }

    // Handle nil
    if (name === "nil") {
      return nil;
    }

    return sym(name);
  }

  private readHash(): SExpr {
    this.advance(); // consume '#'
    const ch = this.advance();

    if (ch === "t") return bool(true);
    if (ch === "f") return bool(false);

    throw new Error(`Unknown hash literal: #${ch}`);
  }

  private isSymbolChar(ch: string): boolean {
    return /[a-zA-Z0-9_+\-*/<>=!?.$%&^~]/.test(ch);
  }

  private skipWhitespaceAndComments(): void {
    while (!this.isAtEnd()) {
      const ch = this.peek();
      if (ch === " " || ch === "\t" || ch === "\n" || ch === "\r") {
        this.advance();
      } else if (ch === ";") {
        // Skip line comment
        while (!this.isAtEnd() && this.peek() !== "\n") {
          this.advance();
        }
      } else {
        break;
      }
    }
  }

  private peek(): string {
    return this.source[this.pos];
  }

  private peekNext(): string | undefined {
    return this.source[this.pos + 1];
  }

  private advance(): string {
    return this.source[this.pos++];
  }

  private isAtEnd(): boolean {
    return this.pos >= this.source.length;
  }
}

/**
 * Convert S-expression to string for debugging
 */
export function sexprToString(expr: SExpr): string {
  switch (expr.type) {
    case "symbol": return expr.name;
    case "number": return String(expr.value);
    case "string": return `"${expr.value.replace(/"/g, '\\"')}"`;
    case "boolean": return expr.value ? "#t" : "#f";
    case "nil": return "nil";
    case "list": return `(${expr.elements.map(sexprToString).join(" ")})`;
  }
}
