/**
 * Reader - Converts token stream to Syntax Objects
 *
 * The reader preserves Slate's nice surface syntax but produces
 * a uniform syntax object representation suitable for macro expansion.
 *
 * Surface syntax:
 *   fn greet name:
 *       "Hello, " + name
 *
 * Becomes syntax object:
 *   (fn greet (name) (+ "Hello, " name))
 */

import { Token, TokenType } from "@oort/core";
import {
  Syntax,
  SyntaxList,
  SourceLoc,
  stx,
  stxSym,
  stxList,
  stxRecord,
  loc,
  isSymbol,
  symbolName,
} from "./syntax";

export class ReaderError extends Error {
  constructor(
    message: string,
    public readonly line: number,
    public readonly column: number
  ) {
    super(`${message} at ${line}:${column}`);
    this.name = "ReaderError";
  }
}

export class Reader {
  private tokens: Token[];
  private current = 0;
  private file?: string;

  constructor(tokens: Token[], file?: string) {
    // Keep all tokens - NEWLINEs are needed for statement boundaries
    this.tokens = tokens;
    this.file = file;
  }

  /**
   * Read the entire token stream into a program (list of top-level forms)
   */
  read(): SyntaxList {
    const forms: Syntax[] = [];
    const startLoc = this.currentLoc();

    while (!this.isAtEnd()) {
      // Skip newlines between statements
      this.skipNewlines();
      if (this.isAtEnd()) break;

      const form = this.readForm();
      if (form) {
        forms.push(form);
      }
    }

    return stxList(forms, startLoc);
  }

  /**
   * Skip any NEWLINE tokens
   */
  private skipNewlines(): void {
    while (this.check(TokenType.NEWLINE)) {
      this.advance();
    }
  }

  /**
   * Read a single form (statement or expression)
   */
  private readForm(): Syntax | null {
    // Skip any stray tokens
    while (this.check(TokenType.DEDENT)) {
      this.advance();
    }

    if (this.isAtEnd()) return null;

    // Handle different statement types
    if (this.check(TokenType.LET)) return this.readLet();
    if (this.check(TokenType.VAR)) return this.readVar();
    if (this.check(TokenType.FN)) return this.readFn();
    if (this.check(TokenType.IF)) return this.readIf();
    if (this.check(TokenType.MATCH)) return this.readMatch();
    if (this.check(TokenType.ON)) return this.readOn();
    if (this.check(TokenType.EMIT)) return this.readEmit();
    if (this.check(TokenType.EXTEND)) return this.readExtend();
    if (this.check(TokenType.IMPORT)) return this.readImport();
    if (this.check(TokenType.FOR)) return this.readFor();
    if (this.check(TokenType.LOOP)) return this.readLoop();
    if (this.check(TokenType.TYPE)) return this.readTypeDef();

    // Otherwise, it's an expression
    return this.readExpr();
  }

  // ============ Statement Readers ============

  private readLet(): Syntax {
    const letTok = this.advance();
    const name = this.readIdentifier();
    this.consume(TokenType.EQUAL, "Expected '=' after variable name");
    const value = this.readExpr();

    return stxList(
      [stxSym("let", this.tokLoc(letTok)), name, value],
      this.tokLoc(letTok)
    );
  }

  private readVar(): Syntax {
    const varTok = this.advance();
    const name = this.readIdentifier();
    this.consume(TokenType.EQUAL, "Expected '=' after variable name");
    const value = this.readExpr();

    return stxList(
      [stxSym("var", this.tokLoc(varTok)), name, value],
      this.tokLoc(varTok)
    );
  }

  private readFn(): Syntax {
    const fnTok = this.advance();
    const name = this.readIdentifier();

    // Read parameters until we hit ':'
    const params: Syntax[] = [];
    while (!this.check(TokenType.COLON) && !this.isAtEnd()) {
      if (this.check(TokenType.IDENTIFIER)) {
        params.push(this.readIdentifier());
      } else {
        break;
      }
    }

    this.consume(TokenType.COLON, "Expected ':' after function signature");
    const body = this.readBlock();

    return stxList(
      [
        stxSym("fn", this.tokLoc(fnTok)),
        name,
        stxList(params, name.loc),
        ...body,
      ],
      this.tokLoc(fnTok)
    );
  }

  private readIf(): Syntax {
    const ifTok = this.advance();
    const condition = this.readExpr();
    this.consume(TokenType.COLON, "Expected ':' after if condition");
    const thenBlock = this.readBlock();

    // Check for else
    if (this.check(TokenType.ELSE)) {
      this.advance();
      if (this.check(TokenType.IF)) {
        // else if
        const elseIf = this.readIf();
        return stxList(
          [
            stxSym("if", this.tokLoc(ifTok)),
            condition,
            stxList([stxSym("begin", this.tokLoc(ifTok)), ...thenBlock], this.tokLoc(ifTok)),
            elseIf,
          ],
          this.tokLoc(ifTok)
        );
      } else {
        this.consume(TokenType.COLON, "Expected ':' after else");
        const elseBlock = this.readBlock();
        return stxList(
          [
            stxSym("if", this.tokLoc(ifTok)),
            condition,
            stxList([stxSym("begin", this.tokLoc(ifTok)), ...thenBlock], this.tokLoc(ifTok)),
            stxList([stxSym("begin", this.tokLoc(ifTok)), ...elseBlock], this.tokLoc(ifTok)),
          ],
          this.tokLoc(ifTok)
        );
      }
    }

    return stxList(
      [
        stxSym("if", this.tokLoc(ifTok)),
        condition,
        stxList([stxSym("begin", this.tokLoc(ifTok)), ...thenBlock], this.tokLoc(ifTok)),
      ],
      this.tokLoc(ifTok)
    );
  }

  private readMatch(): Syntax {
    const matchTok = this.advance();
    const subject = this.readExpr();
    this.consume(TokenType.COLON, "Expected ':' after match subject");
    this.consume(TokenType.INDENT, "Expected indented match arms");

    const arms: Syntax[] = [];
    while (!this.check(TokenType.DEDENT) && !this.isAtEnd()) {
      const pattern = this.readPattern();
      this.consume(TokenType.ARROW, "Expected '=>' after pattern");
      const body = this.readExpr();
      arms.push(stxList([pattern, body], pattern.loc));
    }

    this.consume(TokenType.DEDENT, "Expected end of match block");

    return stxList(
      [stxSym("match", this.tokLoc(matchTok)), subject, ...arms],
      this.tokLoc(matchTok)
    );
  }

  private readOn(): Syntax {
    const onTok = this.advance();

    // Read signal path (possibly with @ prefix)
    this.match(TokenType.AT); // optional @
    const signal = this.readSignalPath();

    // Optional filter expression before ':'
    let filter: Syntax | null = null;
    if (!this.check(TokenType.COLON)) {
      filter = this.readExpr();
    }

    this.consume(TokenType.COLON, "Expected ':' after signal");
    const body = this.readBlock();

    const elements: Syntax[] = [stxSym("on", this.tokLoc(onTok)), signal];
    if (filter) {
      elements.push(filter);
    }
    elements.push(stxList([stxSym("begin", this.tokLoc(onTok)), ...body], this.tokLoc(onTok)));

    return stxList(elements, this.tokLoc(onTok));
  }

  private readEmit(): Syntax {
    const emitTok = this.advance();
    this.match(TokenType.AT); // optional @
    const signal = this.readSignalPath();

    // Optional data expression
    let data: Syntax | null = null;
    if (!this.isAtEnd() && !this.check(TokenType.DEDENT) && !this.isStatementStart()) {
      data = this.readExpr();
    }

    const elements: Syntax[] = [stxSym("emit", this.tokLoc(emitTok)), signal];
    if (data) {
      elements.push(data);
    }

    return stxList(elements, this.tokLoc(emitTok));
  }

  private readExtend(): Syntax {
    const extendTok = this.advance();

    // Expect 'syntax' keyword
    const syntaxKw = this.consume(TokenType.IDENTIFIER, "Expected 'syntax' after 'extend'");
    if (syntaxKw.lexeme !== "syntax") {
      throw new ReaderError("Expected 'syntax' after 'extend'", syntaxKw.line, syntaxKw.column);
    }

    const name = this.readIdentifier();

    // Read parameters
    const params: Syntax[] = [];
    while (!this.check(TokenType.COLON) && !this.isAtEnd()) {
      if (this.check(TokenType.IDENTIFIER)) {
        params.push(this.readIdentifier());
      } else {
        break;
      }
    }

    this.consume(TokenType.COLON, "Expected ':' after extend signature");
    const body = this.readBlock();

    return stxList(
      [
        stxSym("define-syntax", this.tokLoc(extendTok)),
        name,
        stxList(params, name.loc),
        stxList([stxSym("begin", this.tokLoc(extendTok)), ...body], this.tokLoc(extendTok)),
      ],
      this.tokLoc(extendTok)
    );
  }

  private readImport(): Syntax {
    const importTok = this.advance();
    const path = this.readIdentifier();

    return stxList(
      [stxSym("import", this.tokLoc(importTok)), path],
      this.tokLoc(importTok)
    );
  }

  private readFor(): Syntax {
    const forTok = this.advance();
    const variable = this.readIdentifier();
    this.consume(TokenType.IN, "Expected 'in' after variable");
    const iterable = this.readExpr();
    this.consume(TokenType.COLON, "Expected ':' after for expression");
    const body = this.readBlock();

    return stxList(
      [
        stxSym("for", this.tokLoc(forTok)),
        variable,
        iterable,
        stxList([stxSym("begin", this.tokLoc(forTok)), ...body], this.tokLoc(forTok)),
      ],
      this.tokLoc(forTok)
    );
  }

  private readLoop(): Syntax {
    const loopTok = this.advance();
    this.consume(TokenType.COLON, "Expected ':' after loop");
    const body = this.readBlock();

    return stxList(
      [
        stxSym("loop", this.tokLoc(loopTok)),
        stxList([stxSym("begin", this.tokLoc(loopTok)), ...body], this.tokLoc(loopTok)),
      ],
      this.tokLoc(loopTok)
    );
  }

  private readTypeDef(): Syntax {
    const typeTok = this.advance();
    const name = this.readIdentifier();
    this.consume(TokenType.EQUAL, "Expected '=' after type name");
    const definition = this.readTypeExpr();

    return stxList(
      [stxSym("define-type", this.tokLoc(typeTok)), name, definition],
      this.tokLoc(typeTok)
    );
  }

  // ============ Expression Readers ============

  private readExpr(): Syntax {
    return this.readAssignment();
  }

  private readAssignment(): Syntax {
    const expr = this.readOr();

    if (this.match(TokenType.EQUAL)) {
      const eqTok = this.previous();
      const value = this.readAssignment();
      return stxList([stxSym("set!", this.tokLoc(eqTok)), expr, value], expr.loc);
    }

    return expr;
  }

  private readOr(): Syntax {
    let left = this.readAnd();

    while (this.match(TokenType.OR)) {
      const opTok = this.previous();
      const right = this.readAnd();
      left = stxList([stxSym("or", this.tokLoc(opTok)), left, right], left.loc);
    }

    return left;
  }

  private readAnd(): Syntax {
    let left = this.readEquality();

    while (this.match(TokenType.AND)) {
      const opTok = this.previous();
      const right = this.readEquality();
      left = stxList([stxSym("and", this.tokLoc(opTok)), left, right], left.loc);
    }

    return left;
  }

  private readEquality(): Syntax {
    let left = this.readComparison();

    while (this.match(TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL)) {
      const opTok = this.previous();
      const op = opTok.type === TokenType.EQUAL_EQUAL ? "==" : "!=";
      const right = this.readComparison();
      left = stxList([stxSym(op, this.tokLoc(opTok)), left, right], left.loc);
    }

    return left;
  }

  private readComparison(): Syntax {
    let left = this.readTerm();

    while (this.match(TokenType.LESS, TokenType.LESS_EQUAL, TokenType.GREATER, TokenType.GREATER_EQUAL)) {
      const opTok = this.previous();
      const op = {
        [TokenType.LESS]: "<",
        [TokenType.LESS_EQUAL]: "<=",
        [TokenType.GREATER]: ">",
        [TokenType.GREATER_EQUAL]: ">=",
      }[opTok.type]!;
      const right = this.readTerm();
      left = stxList([stxSym(op, this.tokLoc(opTok)), left, right], left.loc);
    }

    return left;
  }

  private readTerm(): Syntax {
    let left = this.readFactor();

    while (this.match(TokenType.PLUS, TokenType.MINUS)) {
      const opTok = this.previous();
      const op = opTok.type === TokenType.PLUS ? "+" : "-";
      const right = this.readFactor();
      left = stxList([stxSym(op, this.tokLoc(opTok)), left, right], left.loc);
    }

    return left;
  }

  private readFactor(): Syntax {
    let left = this.readUnary();

    while (this.match(TokenType.STAR, TokenType.SLASH, TokenType.PERCENT)) {
      const opTok = this.previous();
      const op = opTok.type === TokenType.STAR ? "*" : opTok.type === TokenType.SLASH ? "/" : "%";
      const right = this.readUnary();
      left = stxList([stxSym(op, this.tokLoc(opTok)), left, right], left.loc);
    }

    return left;
  }

  private readUnary(): Syntax {
    if (this.match(TokenType.NOT)) {
      const opTok = this.previous();
      const operand = this.readUnary();
      return stxList([stxSym("not", this.tokLoc(opTok)), operand], this.tokLoc(opTok));
    }
    if (this.match(TokenType.MINUS)) {
      const opTok = this.previous();
      const operand = this.readUnary();
      return stxList([stxSym("-", this.tokLoc(opTok)), operand], this.tokLoc(opTok));
    }

    return this.readCall();
  }

  private readCall(): Syntax {
    let expr = this.readPrimary();

    while (true) {
      if (this.match(TokenType.LEFT_PAREN)) {
        expr = this.finishCall(expr);
      } else if (this.match(TokenType.DOT)) {
        const prop = this.readIdentifier();
        expr = stxList([stxSym(".", expr.loc), expr, prop], expr.loc);
      } else if (this.match(TokenType.LEFT_BRACKET)) {
        const index = this.readExpr();
        this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after index");
        expr = stxList([stxSym("index", expr.loc), expr, index], expr.loc);
      } else if (this.canStartJuxtapositionArg()) {
        // Juxtaposition call: fn arg1 arg2
        expr = this.readJuxtapositionCall(expr);
      } else {
        break;
      }
    }

    return expr;
  }

  private finishCall(callee: Syntax): Syntax {
    const args: Syntax[] = [];

    if (!this.check(TokenType.RIGHT_PAREN)) {
      do {
        args.push(this.readExpr());
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_PAREN, "Expected ')' after arguments");

    return stxList([callee, ...args], callee.loc);
  }

  private readJuxtapositionCall(callee: Syntax): Syntax {
    const args: Syntax[] = [];

    while (this.canStartJuxtapositionArg()) {
      args.push(this.readPrimary());
      this.match(TokenType.COMMA); // optional comma
    }

    return stxList([callee, ...args], callee.loc);
  }

  private canStartJuxtapositionArg(): boolean {
    // Don't continue across newlines
    if (this.check(TokenType.NEWLINE)) {
      return false;
    }

    return (
      this.check(TokenType.STRING) ||
      this.check(TokenType.NUMBER) ||
      this.check(TokenType.IDENTIFIER) ||
      this.check(TokenType.LEFT_BRACE) ||
      this.check(TokenType.LEFT_BRACKET) ||
      this.check(TokenType.HASH)
    );
  }

  private readPrimary(): Syntax {
    if (this.match(TokenType.NUMBER)) {
      const tok = this.previous();
      return stx(tok.literal as number, this.tokLoc(tok));
    }

    if (this.match(TokenType.STRING)) {
      const tok = this.previous();
      return stx(tok.literal as string, this.tokLoc(tok));
    }

    if (this.match(TokenType.TRUE)) {
      return stx(true, this.tokLoc(this.previous()));
    }

    if (this.match(TokenType.FALSE)) {
      return stx(false, this.tokLoc(this.previous()));
    }

    if (this.match(TokenType.HASH)) {
      const tok = this.previous();
      // Color literal
      return stxList(
        [stxSym("color", this.tokLoc(tok)), stx(tok.lexeme, this.tokLoc(tok))],
        this.tokLoc(tok)
      );
    }

    if (this.match(TokenType.IDENTIFIER)) {
      return stxSym(this.previous().lexeme, this.tokLoc(this.previous()));
    }

    if (this.match(TokenType.LEFT_BRACKET)) {
      return this.readList();
    }

    if (this.match(TokenType.LEFT_BRACE)) {
      return this.readRecord();
    }

    if (this.match(TokenType.LEFT_PAREN)) {
      const expr = this.readExpr();
      this.consume(TokenType.RIGHT_PAREN, "Expected ')' after expression");
      return expr;
    }

    if (this.check(TokenType.IF)) {
      return this.readIf();
    }

    if (this.check(TokenType.MATCH)) {
      return this.readMatch();
    }

    throw new ReaderError(
      `Unexpected token: ${this.peek().lexeme}`,
      this.peek().line,
      this.peek().column
    );
  }

  private readList(): Syntax {
    const startTok = this.previous();
    const elements: Syntax[] = [];

    if (!this.check(TokenType.RIGHT_BRACKET)) {
      do {
        elements.push(this.readExpr());
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after list");

    return stxList(
      [stxSym("list", this.tokLoc(startTok)), ...elements],
      this.tokLoc(startTok)
    );
  }

  private readRecord(): Syntax {
    const startTok = this.previous();
    const entries = new Map<string, Syntax>();

    if (!this.check(TokenType.RIGHT_BRACE)) {
      do {
        const key = this.consume(TokenType.IDENTIFIER, "Expected field name");
        this.consume(TokenType.COLON, "Expected ':' after field name");
        const value = this.readExpr();
        entries.set(key.lexeme, value);
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_BRACE, "Expected '}' after record");

    return stxRecord(entries, this.tokLoc(startTok));
  }

  private readIdentifier(): Syntax {
    const tok = this.consume(TokenType.IDENTIFIER, "Expected identifier");
    return stxSym(tok.lexeme, this.tokLoc(tok));
  }

  private readSignalPath(): Syntax {
    const parts: Syntax[] = [];
    const first = this.readIdentifier();
    parts.push(first);

    while (this.match(TokenType.DOT)) {
      parts.push(this.readIdentifier());
    }

    if (parts.length === 1) {
      return parts[0];
    }

    return stxList(
      [stxSym("signal-path", first.loc), ...parts],
      first.loc
    );
  }

  private readPattern(): Syntax {
    if (this.match(TokenType.UNDERSCORE)) {
      return stxSym("_", this.tokLoc(this.previous()));
    }

    if (this.match(TokenType.LEFT_BRACE)) {
      // Record pattern
      const startTok = this.previous();
      const fields: Syntax[] = [];

      if (!this.check(TokenType.RIGHT_BRACE)) {
        do {
          const key = this.consume(TokenType.IDENTIFIER, "Expected field name");
          if (this.match(TokenType.COLON)) {
            const pattern = this.readPattern();
            fields.push(stxList(
              [stxSym(key.lexeme, this.tokLoc(key)), pattern],
              this.tokLoc(key)
            ));
          } else {
            // Shorthand: {key} means {key: key}
            fields.push(stxSym(key.lexeme, this.tokLoc(key)));
          }
        } while (this.match(TokenType.COMMA));
      }

      this.consume(TokenType.RIGHT_BRACE, "Expected '}' after pattern");
      return stxList(
        [stxSym("record-pattern", this.tokLoc(startTok)), ...fields],
        this.tokLoc(startTok)
      );
    }

    if (this.match(TokenType.LEFT_BRACKET)) {
      // List pattern
      const startTok = this.previous();
      const elements: Syntax[] = [];
      let rest: Syntax | null = null;

      if (!this.check(TokenType.RIGHT_BRACKET)) {
        do {
          if (this.match(TokenType.DOT_DOT)) {
            rest = this.readIdentifier();
            break;
          }
          elements.push(this.readPattern());
        } while (this.match(TokenType.COMMA));
      }

      this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after pattern");

      const listPat: Syntax[] = [stxSym("list-pattern", this.tokLoc(startTok)), ...elements];
      if (rest) {
        listPat.push(stxList([stxSym("...", rest.loc), rest], rest.loc));
      }
      return stxList(listPat, this.tokLoc(startTok));
    }

    // Literal or identifier pattern
    if (this.match(TokenType.NUMBER)) {
      return stx(this.previous().literal as number, this.tokLoc(this.previous()));
    }
    if (this.match(TokenType.STRING)) {
      return stx(this.previous().literal as string, this.tokLoc(this.previous()));
    }
    if (this.match(TokenType.TRUE)) {
      return stx(true, this.tokLoc(this.previous()));
    }
    if (this.match(TokenType.FALSE)) {
      return stx(false, this.tokLoc(this.previous()));
    }
    if (this.match(TokenType.IDENTIFIER)) {
      return stxSym(this.previous().lexeme, this.tokLoc(this.previous()));
    }

    throw new ReaderError(
      `Expected pattern, got ${this.peek().lexeme}`,
      this.peek().line,
      this.peek().column
    );
  }

  private readTypeExpr(): Syntax {
    // Simplified type expression reading
    if (this.check(TokenType.PIPE)) {
      // Sum type
      const variants: Syntax[] = [];
      while (this.match(TokenType.PIPE)) {
        const name = this.readIdentifier();
        variants.push(name);
      }
      return stxList([stxSym("sum-type", variants[0]?.loc ?? this.currentLoc()), ...variants], variants[0]?.loc ?? this.currentLoc());
    }

    if (this.match(TokenType.LEFT_BRACE)) {
      // Record type
      const startTok = this.previous();
      const fields: Syntax[] = [];

      if (!this.check(TokenType.RIGHT_BRACE)) {
        do {
          const key = this.consume(TokenType.IDENTIFIER, "Expected field name");
          this.consume(TokenType.COLON, "Expected ':' after field name");
          const typeExpr = this.readTypeExpr();
          fields.push(stxList(
            [stxSym(key.lexeme, this.tokLoc(key)), typeExpr],
            this.tokLoc(key)
          ));
        } while (this.match(TokenType.COMMA));
      }

      this.consume(TokenType.RIGHT_BRACE, "Expected '}' after type");
      return stxList([stxSym("record-type", this.tokLoc(startTok)), ...fields], this.tokLoc(startTok));
    }

    // Named type
    return this.readIdentifier();
  }

  private readBlock(): Syntax[] {
    this.skipNewlines();
    this.consume(TokenType.INDENT, "Expected indented block");
    const statements: Syntax[] = [];

    while (!this.check(TokenType.DEDENT) && !this.isAtEnd()) {
      this.skipNewlines();
      if (this.check(TokenType.DEDENT)) break;

      const form = this.readForm();
      if (form) {
        statements.push(form);
      }
    }

    this.skipNewlines();
    this.consume(TokenType.DEDENT, "Expected end of block");
    return statements;
  }

  // ============ Helpers ============

  private isStatementStart(): boolean {
    return (
      this.check(TokenType.LET) ||
      this.check(TokenType.VAR) ||
      this.check(TokenType.FN) ||
      this.check(TokenType.IF) ||
      this.check(TokenType.MATCH) ||
      this.check(TokenType.ON) ||
      this.check(TokenType.EMIT) ||
      this.check(TokenType.EXTEND) ||
      this.check(TokenType.IMPORT) ||
      this.check(TokenType.FOR) ||
      this.check(TokenType.LOOP) ||
      this.check(TokenType.TYPE)
    );
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

  private check(type: TokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.peek().type === type;
  }

  private advance(): Token {
    if (!this.isAtEnd()) this.current++;
    return this.previous();
  }

  private consume(type: TokenType, message: string): Token {
    if (this.check(type)) return this.advance();
    throw new ReaderError(message, this.peek().line, this.peek().column);
  }

  private peek(): Token {
    return this.tokens[this.current];
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }

  private isAtEnd(): boolean {
    return this.peek().type === TokenType.EOF;
  }

  private tokLoc(tok: Token): SourceLoc {
    return loc(tok.line, tok.column, this.file);
  }

  private currentLoc(): SourceLoc {
    return this.tokLoc(this.peek());
  }
}

/**
 * Read source code into syntax objects
 */
export function read(tokens: Token[], file?: string): SyntaxList {
  return new Reader(tokens, file).read();
}
