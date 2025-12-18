import type { CallExpr, Expr, Loc, Pattern, Program, Stmt } from "./ast";
import { lex, type Token, type TokenType } from "./lexer";

class ParseError extends Error {
  readonly name = "ParseError";
}

function locFrom(token: Token): Loc {
  return { line: token.line, col: token.col };
}

function decodeStringLiteral(token: Token): string {
  const raw = token.lexeme;
  if (!raw.startsWith('"') || !raw.endsWith('"')) return raw;
  let out = "";
  for (let i = 1; i < raw.length - 1; i++) {
    const ch = raw[i]!;
    if (ch !== "\\") {
      out += ch;
      continue;
    }
    const next = raw[i + 1];
    if (next === undefined) throw new ParseError(`Invalid escape at ${token.line}:${token.col}`);
    i++;
    switch (next) {
      case "n":
        out += "\n";
        break;
      case "t":
        out += "\t";
        break;
      case '"':
        out += '"';
        break;
      case "\\":
        out += "\\";
        break;
      default:
        out += next;
        break;
    }
  }
  return out;
}

function canStartExpr(type: TokenType): boolean {
  return (
    type === "IDENT" ||
    type === "NUMBER" ||
    type === "STRING" ||
    type === "COLOR" ||
    type === "TRUE" ||
    type === "FALSE" ||
    type === "NULL" ||
    type === "LPAREN" ||
    type === "LBRACE" ||
    type === "LBRACKET" ||
    type === "IF" ||
    type === "MATCH"
  );
}

export class Parser {
  private readonly tokens: Token[];
  private current = 0;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  parseProgram(): Program {
    const body: Stmt[] = [];
    this.skipNewlines();
    while (!this.isAtEnd()) {
      if (this.check("DEDENT")) break;
      body.push(this.parseStmt());
      this.skipNewlines();
    }
    return { type: "Program", body };
  }

  private parseStmt(): Stmt {
    if (this.match("LET")) return this.parseLetOrVar("Let");
    if (this.match("VAR")) return this.parseLetOrVar("Var");
    if (this.match("FN")) return this.parseFn();
    if (this.match("ON")) return this.parseOn();
    if (this.match("EXTEND")) return this.parseExtend();
    if (this.match("IMPORT")) return this.parseImport();
    if (this.match("EMIT")) return this.parseEmit();
    return this.parseExprStmt();
  }

  private parseLetOrVar(kind: "Let" | "Var"): Stmt {
    const keyword = this.previous();
    const name = this.consume("IDENT", `Expected identifier after '${keyword.lexeme}'`).lexeme;
    this.consume("EQ", "Expected '=' in binding");
    const expr = this.parseExpr();
    this.match("NEWLINE");
    return { type: kind, loc: locFrom(keyword), name, expr };
  }

  private parseFn(): Stmt {
    const fnTok = this.previous();
    const name = this.consume("IDENT", "Expected function name").lexeme;
    const params: string[] = [];
    while (this.check("IDENT")) {
      params.push(this.advance().lexeme);
    }
    this.consume("COLON", "Expected ':' after function signature");
    const body = this.parseBlockCode();
    return { type: "Fn", loc: locFrom(fnTok), name, params, body };
  }

  private parseOn(): Stmt {
    const onTok = this.previous();
    const signal = this.parseSignalPath();
    const filters: Expr[] = [];
    while (!this.check("COLON")) {
      if (!canStartExpr(this.peek().type)) {
        throw this.error(this.peek(), "Expected filter expression or ':'");
      }
      filters.push(this.parseUnary());
    }
    this.consume("COLON", "Expected ':' after signal");
    const body = this.parseBlockCode();
    return { type: "On", loc: locFrom(onTok), signal, filters, body };
  }

  private parseExtend(): Stmt {
    const extendTok = this.previous();
    this.consume("SYNTAX", "Expected 'syntax' after 'extend'");
    const name = this.consume("IDENT", "Expected syntax name").lexeme;
    const params: string[] = [];
    while (this.check("IDENT")) {
      params.push(this.advance().lexeme);
    }
    this.consume("COLON", "Expected ':' after extend signature");
    const body = this.parseBlockCode();
    return { type: "Extend", loc: locFrom(extendTok), name, params, body };
  }

  private parseImport(): Stmt {
    const importTok = this.previous();
    if (this.check("IDENT")) {
      const name = this.advance().lexeme;
      this.match("NEWLINE");
      return { type: "Import", loc: locFrom(importTok), kind: "module", name };
    }
    if (this.check("STRING")) {
      const t = this.advance();
      const name = decodeStringLiteral(t);
      this.match("NEWLINE");
      return { type: "Import", loc: locFrom(importTok), kind: "path", name };
    }
    throw this.error(this.peek(), "Expected module name or path");
  }

  private parseEmit(): Stmt {
    const emitTok = this.previous();
    const signal = this.parseSignalPath();
    let data: Expr | undefined;
    if (this.match("COMMA")) {
      data = this.parseExpr();
    }
    this.match("NEWLINE");
    return { type: "Emit", loc: locFrom(emitTok), signal, data };
  }

  private parseExprStmt(): Stmt {
    const start = this.peek();
    const expr = this.parseExpr();
    this.match("NEWLINE");
    return { type: "Expr", loc: locFrom(start), expr };
  }

  private parseBlockCode(): Stmt[] {
    this.consume("NEWLINE", "Expected newline before block");
    this.consume("INDENT", "Expected indented block");
    const body: Stmt[] = [];
    this.skipNewlines();
    while (!this.check("DEDENT") && !this.isAtEnd()) {
      body.push(this.parseStmt());
      this.skipNewlines();
    }
    this.consume("DEDENT", "Expected end of block");
    return body;
  }

  private parseExpr(): Expr {
    return this.parseAssignment();
  }

  private parseAssignment(): Expr {
    const expr = this.parseOr();
    if (this.match("EQ")) {
      const equals = this.previous();
      if (expr.type !== "Ident" && expr.type !== "Member") {
        throw this.error(equals, "Invalid assignment target");
      }
      const value = this.parseAssignment();
      return { type: "Assign", loc: locFrom(equals), target: expr, value };
    }
    return expr;
  }

  private parseOr(): Expr {
    let expr = this.parseAnd();
    while (this.match("OR")) {
      const op = this.previous();
      const right = this.parseAnd();
      expr = { type: "Binary", loc: locFrom(op), op: "or", left: expr, right };
    }
    return expr;
  }

  private parseAnd(): Expr {
    let expr = this.parseEquality();
    while (this.match("AND")) {
      const op = this.previous();
      const right = this.parseEquality();
      expr = { type: "Binary", loc: locFrom(op), op: "and", left: expr, right };
    }
    return expr;
  }

  private parseEquality(): Expr {
    let expr = this.parseComparison();
    while (this.match("EQEQ", "NEQ")) {
      const op = this.previous();
      const right = this.parseComparison();
      expr = {
        type: "Binary",
        loc: locFrom(op),
        op: op.type === "EQEQ" ? "==" : "!=",
        left: expr,
        right,
      };
    }
    return expr;
  }

  private parseComparison(): Expr {
    let expr = this.parseTerm();
    while (this.match("GT", "GTE", "LT", "LTE")) {
      const op = this.previous();
      const right = this.parseTerm();
      const opStr = op.type === "GT" ? ">" : op.type === "GTE" ? ">=" : op.type === "LT" ? "<" : "<=";
      expr = { type: "Binary", loc: locFrom(op), op: opStr, left: expr, right };
    }
    return expr;
  }

  private parseTerm(): Expr {
    let expr = this.parseFactor();
    while (this.match("PLUS", "MINUS")) {
      const op = this.previous();
      const right = this.parseFactor();
      expr = { type: "Binary", loc: locFrom(op), op: op.type === "PLUS" ? "+" : "-", left: expr, right };
    }
    return expr;
  }

  private parseFactor(): Expr {
    let expr = this.parseUnary();
    while (this.match("STAR", "SLASH")) {
      const op = this.previous();
      const right = this.parseUnary();
      expr = { type: "Binary", loc: locFrom(op), op: op.type === "STAR" ? "*" : "/", left: expr, right };
    }
    return expr;
  }

  private parseUnary(): Expr {
    if (this.match("NOT")) {
      const op = this.previous();
      const expr = this.parseUnary();
      return { type: "Unary", loc: locFrom(op), op: "not", expr };
    }
    if (this.match("MINUS")) {
      const op = this.previous();
      const expr = this.parseUnary();
      return { type: "Unary", loc: locFrom(op), op: "-", expr };
    }
    return this.parseCall();
  }

  private parseCall(): Expr {
    let expr = this.parsePrimary();
    while (true) {
      if (this.match("DOT")) {
        const dot = this.previous();
        const property = this.consume("IDENT", "Expected property name after '.'").lexeme;
        expr = { type: "Member", loc: locFrom(dot), object: expr, property };
        continue;
      }

      if (this.match("LPAREN")) {
        const lparen = this.previous();
        const args: Expr[] = [];
        this.skipNewlines();
        if (!this.check("RPAREN")) {
          do {
            this.skipNewlines();
            args.push(this.parseExpr());
            this.skipNewlines();
          } while (this.match("COMMA"));
        }
        this.consume("RPAREN", "Expected ')'");
        expr = { type: "Call", loc: locFrom(lparen), callee: expr, args };
        continue;
      }

      if (
        canStartExpr(this.peek().type) &&
        this.previous().type !== "NEWLINE" &&
        this.previous().type !== "INDENT" &&
        this.previous().type !== "DEDENT"
      ) {
        const startTok = this.peek();
        const args: Expr[] = [];
        while (true) {
          if (!canStartExpr(this.peek().type)) break;
          args.push(this.parseUnary());
          if (this.match("COMMA")) continue;
          if (!canStartExpr(this.peek().type)) break;
        }
        expr = { type: "Call", loc: locFrom(startTok), callee: expr, args };
        continue;
      }

      if (this.check("COLON")) {
        if (expr.type !== "Call") break;
        if (expr.block) throw this.error(this.peek(), "Call already has a block");
        if (!this.looksLikeDataBlock()) break;
        this.advance();
        const block = this.parseDataBlock();
        expr = { ...(expr as CallExpr), block };
        continue;
      }

      break;
    }
    return expr;
  }

  private parsePrimary(): Expr {
    const tok = this.peek();

    if (this.match("NUMBER")) {
      return { type: "Literal", loc: locFrom(this.previous()), value: Number(this.previous().lexeme) };
    }
    if (this.match("STRING")) {
      const t = this.previous();
      return { type: "Literal", loc: locFrom(t), value: decodeStringLiteral(t) };
    }
    if (this.match("COLOR")) {
      const t = this.previous();
      return { type: "Literal", loc: locFrom(t), value: t.lexeme };
    }
    if (this.match("TRUE")) {
      const t = this.previous();
      return { type: "Literal", loc: locFrom(t), value: true };
    }
    if (this.match("FALSE")) {
      const t = this.previous();
      return { type: "Literal", loc: locFrom(t), value: false };
    }
    if (this.match("NULL")) {
      const t = this.previous();
      return { type: "Literal", loc: locFrom(t), value: null };
    }
    if (this.match("IDENT")) {
      const t = this.previous();
      return { type: "Ident", loc: locFrom(t), name: t.lexeme };
    }
    if (this.match("IF")) return this.parseIfExpr();
    if (this.match("MATCH")) return this.parseMatchExpr();

    if (this.match("LPAREN")) {
      this.skipNewlines();
      const expr = this.parseExpr();
      this.skipNewlines();
      this.consume("RPAREN", "Expected ')'");
      return expr;
    }

    if (this.match("LBRACKET")) return this.parseListLiteral(tok);
    if (this.match("LBRACE")) return this.parseRecordLiteral(tok);

    throw this.error(tok, "Expected expression");
  }

  private parseIfExpr(): Expr {
    const ifTok = this.previous();
    const condition = this.parseExpr();
    this.consume("COLON", "Expected ':' after if condition");
    const thenBlock = this.parseBlockCode();

    let elseBlock: Stmt[] | undefined;
    if (this.match("ELSE")) {
      this.consume("COLON", "Expected ':' after else");
      elseBlock = this.parseBlockCode();
    }

    return { type: "If", loc: locFrom(ifTok), condition, thenBlock, elseBlock };
  }

  private parseMatchExpr(): Expr {
    const matchTok = this.previous();
    const expr = this.parseExpr();
    this.consume("COLON", "Expected ':' after match expression");
    this.consume("NEWLINE", "Expected newline before match cases");
    this.consume("INDENT", "Expected indented match cases");
    const cases: { pattern: Pattern; expr: Expr }[] = [];

    this.skipNewlines();
    while (!this.check("DEDENT") && !this.isAtEnd()) {
      const pattern = this.parsePattern();
      this.consume("FATARROW", "Expected '=>' after match pattern");
      const caseExpr = this.parseExpr();
      cases.push({ pattern, expr: caseExpr });
      this.match("NEWLINE");
      this.skipNewlines();
    }
    this.consume("DEDENT", "Expected end of match cases");
    return { type: "Match", loc: locFrom(matchTok), expr, cases };
  }

  private parsePattern(): Pattern {
    const tok = this.peek();

    if (this.match("IDENT")) {
      const t = this.previous();
      if (t.lexeme === "_") return { type: "Wildcard", loc: locFrom(t) };
      return { type: "PIdent", loc: locFrom(t), name: t.lexeme };
    }
    if (this.match("NUMBER")) {
      const t = this.previous();
      return { type: "PLiteral", loc: locFrom(t), value: Number(t.lexeme) };
    }
    if (this.match("STRING")) {
      const t = this.previous();
      return { type: "PLiteral", loc: locFrom(t), value: decodeStringLiteral(t) };
    }
    if (this.match("COLOR")) {
      const t = this.previous();
      return { type: "PLiteral", loc: locFrom(t), value: t.lexeme };
    }
    if (this.match("TRUE")) {
      const t = this.previous();
      return { type: "PLiteral", loc: locFrom(t), value: true };
    }
    if (this.match("FALSE")) {
      const t = this.previous();
      return { type: "PLiteral", loc: locFrom(t), value: false };
    }
    if (this.match("NULL")) {
      const t = this.previous();
      return { type: "PLiteral", loc: locFrom(t), value: null };
    }
    if (this.match("LBRACE")) return this.parseRecordPattern(tok);

    throw this.error(tok, "Expected pattern");
  }

  private parseRecordPattern(open: Token): Pattern {
    const fields: { key: string; pattern: Pattern }[] = [];
    this.skipNewlines();
    if (this.match("NEWLINE")) {
      this.consume("INDENT", "Expected indented record pattern");
      this.skipNewlines();
      while (!this.check("DEDENT") && !this.check("RBRACE") && !this.isAtEnd()) {
        fields.push(this.parsePatternField());
        this.match("COMMA");
        this.match("NEWLINE");
        this.skipNewlines();
      }
      if (this.match("DEDENT")) {
        this.skipNewlines();
      }
      this.consume("RBRACE", "Expected '}'");
      return { type: "PRecord", loc: locFrom(open), fields };
    }

    if (!this.check("RBRACE")) {
      do {
        fields.push(this.parsePatternField());
      } while (this.match("COMMA"));
    }
    this.consume("RBRACE", "Expected '}'");
    return { type: "PRecord", loc: locFrom(open), fields };
  }

  private parsePatternField(): { key: string; pattern: Pattern } {
    const keyTok = this.consume("IDENT", "Expected field name in pattern");
    const key = keyTok.lexeme;
    if (this.match("COLON")) {
      const pat = this.parsePattern();
      return { key, pattern: pat };
    }
    return { key, pattern: { type: "PIdent", loc: locFrom(keyTok), name: key } };
  }

  private parseListLiteral(open: Token): Expr {
    const items: Expr[] = [];
    this.skipNewlines();

    let multiline = false;
    if (this.match("NEWLINE")) {
      multiline = true;
      this.consume("INDENT", "Expected indented list");
      this.skipNewlines();
    }

    while (!this.check("RBRACKET") && !this.check("DEDENT") && !this.isAtEnd()) {
      items.push(this.parseExpr());
      this.skipNewlines();
      this.match("COMMA");
      this.skipNewlines();
    }

    if (multiline) {
      this.consume("DEDENT", "Expected end of list");
      this.skipNewlines();
    }

    this.consume("RBRACKET", "Expected ']'");
    return { type: "List", loc: locFrom(open), items };
  }

  private parseRecordLiteral(open: Token): Expr {
    const entries: { key: string; value: Expr }[] = [];
    this.skipNewlines();

    let multiline = false;
    if (this.match("NEWLINE")) {
      multiline = true;
      this.consume("INDENT", "Expected indented record");
      this.skipNewlines();
    }

    while (!this.check("RBRACE") && !this.check("DEDENT") && !this.isAtEnd()) {
      const keyTok = this.consume("IDENT", "Expected record key");
      const key = keyTok.lexeme;
      this.consume("COLON", "Expected ':' after record key");
      const value = this.parseExpr();
      entries.push({ key, value });
      this.skipNewlines();
      this.match("COMMA");
      this.skipNewlines();
    }

    if (multiline) {
      this.consume("DEDENT", "Expected end of record");
      this.skipNewlines();
    }

    this.consume("RBRACE", "Expected '}'");
    return { type: "Record", loc: locFrom(open), entries };
  }

  private parseDataBlock(): Expr {
    this.consume("NEWLINE", "Expected newline before data block");
    this.consume("INDENT", "Expected indented data block");
    this.skipNewlines();

    if (this.check("DASH")) {
      const list = this.parseDataList();
      this.consume("DEDENT", "Expected end of data block");
      return list;
    }
    if (this.check("IDENT") && this.peekNext().type === "COLON") {
      const record = this.parseDataRecord();
      this.consume("DEDENT", "Expected end of data block");
      return record;
    }

    throw this.error(this.peek(), "Expected list or record data block");
  }

  private parseDataRecord(): Expr {
    const start = this.peek();
    const entries: { key: string; value: Expr }[] = [];

    while (!this.check("DEDENT") && !this.isAtEnd()) {
      const key = this.consume("IDENT", "Expected key").lexeme;
      this.consume("COLON", "Expected ':' after key");

      let value: Expr;
      if (this.check("NEWLINE")) {
        value = this.parseDataBlock();
      } else {
        value = this.parseExpr();
      }
      entries.push({ key, value });
      this.match("NEWLINE");
      this.skipNewlines();
    }

    return { type: "Record", loc: locFrom(start), entries };
  }

  private parseDataList(): Expr {
    const start = this.peek();
    const items: Expr[] = [];

    while (!this.check("DEDENT") && !this.isAtEnd()) {
      this.consume("DASH", "Expected '-' list item");

      let value: Expr;
      if (this.check("NEWLINE")) {
        value = this.parseDataBlock();
      } else if (this.check("IDENT") && this.peekNext().type === "COLON") {
        const key = this.advance().lexeme;
        this.consume("COLON", "Expected ':' after key");
        const innerValue = this.check("NEWLINE") ? this.parseDataBlock() : this.parseExpr();
        value = { type: "Record", loc: locFrom(start), entries: [{ key, value: innerValue }] };
      } else {
        value = this.parseExpr();
      }
      items.push(value);
      this.match("NEWLINE");
      this.skipNewlines();
    }

    return { type: "List", loc: locFrom(start), items };
  }

  private parseSignalPath(): string {
    const first = this.consume("IDENT", "Expected signal").lexeme;
    const parts = [first];
    while (this.match("DOT")) {
      parts.push(this.consume("IDENT", "Expected identifier after '.'").lexeme);
    }
    return parts.join(".");
  }

  private skipNewlines(): void {
    while (this.match("NEWLINE")) {}
  }

  private match(...types: TokenType[]): boolean {
    for (const type of types) {
      if (this.check(type)) {
        this.advance();
        return true;
      }
    }
    return false;
  }

  private consume(type: TokenType, message: string): Token {
    if (this.check(type)) return this.advance();
    throw this.error(this.peek(), message);
  }

  private check(type: TokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.peek().type === type;
  }

  private advance(): Token {
    if (!this.isAtEnd()) this.current++;
    return this.previous();
  }

  private isAtEnd(): boolean {
    return this.peek().type === "EOF";
  }

  private peek(): Token {
    return this.tokens[this.current] ?? this.tokens[this.tokens.length - 1]!;
  }

  private peekNext(): Token {
    return this.tokens[this.current + 1] ?? this.tokens[this.tokens.length - 1]!;
  }

  private looksLikeDataBlock(): boolean {
    const t1 = this.tokens[this.current + 1];
    const t2 = this.tokens[this.current + 2];
    const t3 = this.tokens[this.current + 3];
    const t4 = this.tokens[this.current + 4];

    if (!t1 || t1.type !== "NEWLINE") return false;
    if (!t2 || t2.type !== "INDENT") return false;
    if (!t3) return false;
    if (t3.type === "DASH") return true;
    if (t3.type === "IDENT" && t4?.type === "COLON") return true;
    return false;
  }

  private previous(): Token {
    return this.tokens[this.current - 1] ?? this.tokens[0]!;
  }

  private error(token: Token, message: string): ParseError {
    return new ParseError(`${message} at ${token.line}:${token.col}`);
  }
}

export function parse(source: string): Program {
  return new Parser(lex(source)).parseProgram();
}
