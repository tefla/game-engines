// Slate Parser - Builds AST from tokens

import {
  Token,
  TokenType,
  ParseError,
  type Program,
  type Stmt,
  type Expr,
  type Block,
  type Pattern,
  type LetStmt,
  type VarStmt,
  type FnStmt,
  type OnStmt,
  type EmitStmt,
  type ExprStmt,
  type IfExpr,
  type MatchExpr,
  type BinaryExpr,
  type UnaryExpr,
  type LogicalExpr,
  type CallExpr,
  type MemberExpr,
  type IndexExpr,
  type RecordExpr,
  type ListExpr,
  type LiteralExpr,
  type IdentifierExpr,
  type AssignExpr,
  type ColorExpr,
  type SignalPath,
  type ImportStmt,
  type LoopStmt,
  type ForStmt,
  type BreakStmt,
  type ContinueStmt,
  type NullCoalesceExpr,
  type InterpolatedStringExpr,
  type OptionalMemberExpr,
  type OptionalIndexExpr,
  type OptionalCallExpr,
  type LambdaExpr,
  type TypeExpr,
} from "@oort/core";

export class Parser {
  private tokens: Token[];
  private current = 0;
  private allowJuxtaposition = true; // Disable inside lambda bodies

  constructor(tokens: Token[]) {
    // Filter out NEWLINE tokens except when they are significant
    this.tokens = tokens.filter((t) => t.type !== TokenType.NEWLINE);
  }

  parse(): Program {
    const statements: Stmt[] = [];

    while (!this.isAtEnd()) {
      const stmt = this.declaration();
      if (stmt) {
        statements.push(stmt);
      }
    }

    return {
      type: "Program",
      statements,
      line: 1,
      column: 1,
    };
  }

  private declaration(): Stmt | null {
    try {
      if (this.check(TokenType.LET)) return this.letStatement();
      if (this.check(TokenType.VAR)) return this.varStatement();
      if (this.check(TokenType.FN)) return this.fnStatement();
      if (this.check(TokenType.ON)) return this.onStatement();
      if (this.check(TokenType.EMIT)) return this.emitStatement();
      if (this.check(TokenType.IMPORT)) return this.importStatement();
      if (this.check(TokenType.LOOP)) return this.loopStatement();
      if (this.check(TokenType.FOR)) return this.forStatement();
      if (this.check(TokenType.BREAK)) return this.breakStatement();
      if (this.check(TokenType.CONTINUE)) return this.continueStatement();
      if (this.check(TokenType.TYPE)) return this.typeStatement();
      return this.expressionStatement();
    } catch (e) {
      if (e instanceof ParseError) {
        this.synchronize();
        return null;
      }
      throw e;
    }
  }

  private letStatement(): LetStmt {
    const keyword = this.advance(); // consume 'let'
    const name = this.consume(TokenType.IDENTIFIER, "Expected variable name");

    let typeAnnotation: TypeExpr | undefined;
    if (this.match(TokenType.COLON)) {
      typeAnnotation = this.typeExpression();
    }

    this.consume(TokenType.EQUAL, "Expected '=' after variable name");
    const value = this.expression();

    return {
      type: "Let",
      name: name.lexeme,
      typeAnnotation,
      value,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private varStatement(): VarStmt {
    const keyword = this.advance(); // consume 'var'
    const name = this.consume(TokenType.IDENTIFIER, "Expected variable name");

    let typeAnnotation: TypeExpr | undefined;
    if (this.match(TokenType.COLON)) {
      typeAnnotation = this.typeExpression();
    }

    this.consume(TokenType.EQUAL, "Expected '=' after variable name");
    const value = this.expression();

    return {
      type: "Var",
      name: name.lexeme,
      typeAnnotation,
      value,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private fnStatement(): FnStmt {
    const keyword = this.advance(); // consume 'fn'
    const name = this.consume(TokenType.IDENTIFIER, "Expected function name");

    const params: Array<{ name: string; typeAnnotation?: TypeExpr }> = [];

    // Parse parameters: either `a b c` or `a: Type, b: Type`
    while (this.check(TokenType.IDENTIFIER)) {
      const paramName = this.advance();
      let typeAnnotation: TypeExpr | undefined;

      // Check for type annotation on parameter
      // Only if we see `:` followed by an identifier (type name), not `:` followed by INDENT (body start)
      if (
        this.check(TokenType.COLON) &&
        this.peekNext()?.type === TokenType.IDENTIFIER
      ) {
        this.advance(); // consume ':'
        typeAnnotation = this.typeExpression();
        // Check for comma separator between params
        this.match(TokenType.COMMA);
      }

      params.push({ name: paramName.lexeme, typeAnnotation });
    }

    let returnType: TypeExpr | undefined;
    if (this.match(TokenType.THIN_ARROW)) {
      returnType = this.typeExpression();
    }

    this.consume(TokenType.COLON, "Expected ':' before function body");
    const body = this.block();

    return {
      type: "Fn",
      name: name.lexeme,
      params,
      returnType,
      body,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private onStatement(): OnStmt {
    const keyword = this.advance(); // consume 'on'
    const signal = this.signalPath();

    let filter: Expr | undefined;
    // Check for optional filter (e.g., on puzzle.solved "find_key":)
    if (!this.check(TokenType.COLON)) {
      filter = this.expression();
    }

    this.consume(TokenType.COLON, "Expected ':' before handler body");
    const body = this.block();

    return {
      type: "On",
      signal,
      filter,
      body,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private emitStatement(): EmitStmt {
    const keyword = this.advance(); // consume 'emit'
    const signal = this.signalPath();

    let data: Expr | undefined;
    // Data expression is optional, no comma required
    // Check if there's more on this line (not EOF, NEWLINE, DEDENT)
    // Note: Use isAtEnd() instead of check(EOF) since check() returns false at EOF
    if (
      !this.isAtEnd() &&
      !this.check(TokenType.NEWLINE) &&
      !this.check(TokenType.DEDENT)
    ) {
      data = this.expression();
    }

    return {
      type: "Emit",
      signal,
      data,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private importStatement(): ImportStmt {
    const keyword = this.advance(); // consume 'import'
    const path = this.consume(
      TokenType.IDENTIFIER,
      "Expected module path after 'import'"
    );

    return {
      type: "Import",
      path: path.lexeme,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private loopStatement(): LoopStmt {
    const keyword = this.advance(); // consume 'loop'
    this.consume(TokenType.COLON, "Expected ':' before loop body");
    const body = this.block();

    return {
      type: "Loop",
      body,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private forStatement(): ForStmt {
    const keyword = this.advance(); // consume 'for'
    const variable = this.consume(
      TokenType.IDENTIFIER,
      "Expected variable name"
    );
    this.consume(TokenType.IN, "Expected 'in' after variable");
    const iterable = this.expression();
    this.consume(TokenType.COLON, "Expected ':' before for body");
    const body = this.block();

    return {
      type: "For",
      variable: variable.lexeme,
      iterable,
      body,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private breakStatement(): BreakStmt {
    const keyword = this.advance(); // consume 'break'
    return {
      type: "Break",
      line: keyword.line,
      column: keyword.column,
    };
  }

  private continueStatement(): ContinueStmt {
    const keyword = this.advance(); // consume 'continue'
    return {
      type: "Continue",
      line: keyword.line,
      column: keyword.column,
    };
  }

  private typeStatement(): Stmt {
    const keyword = this.advance(); // consume 'type'
    const name = this.consume(TokenType.IDENTIFIER, "Expected type name");

    let typeParams: string[] | undefined;
    if (this.match(TokenType.LEFT_BRACKET)) {
      typeParams = [];
      do {
        const param = this.consume(TokenType.IDENTIFIER, "Expected type parameter");
        typeParams.push(param.lexeme);
      } while (this.match(TokenType.COMMA));
      this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after type parameters");
    }

    this.consume(TokenType.EQUAL, "Expected '=' after type name");
    const definition = this.typeExpression();

    return {
      type: "TypeDef",
      name: name.lexeme,
      typeParams,
      definition,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private typeExpression(): TypeExpr {
    // Sum type: | Variant1 | Variant2 { field: Type }
    if (this.check(TokenType.PIPE)) {
      const variants: Array<{ name: string; fields?: any }> = [];
      while (this.match(TokenType.PIPE)) {
        const variantName = this.consume(TokenType.IDENTIFIER, "Expected variant name");
        let fields: any | undefined;
        if (this.check(TokenType.LEFT_BRACE)) {
          fields = this.recordType();
        }
        variants.push({ name: variantName.lexeme, fields });
      }
      return {
        type: "SumType",
        variants,
        line: this.previous().line,
        column: this.previous().column,
      };
    }

    // Record type: { field: Type, ... }
    if (this.check(TokenType.LEFT_BRACE)) {
      return this.recordType();
    }

    // Named type: TypeName or TypeName[T]
    const name = this.consume(TokenType.IDENTIFIER, "Expected type name");
    let typeArgs: TypeExpr[] | undefined;

    if (this.match(TokenType.LEFT_BRACKET)) {
      typeArgs = [];
      do {
        typeArgs.push(this.typeExpression());
      } while (this.match(TokenType.COMMA));
      this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after type arguments");
    }

    return {
      type: "NamedType",
      name: name.lexeme,
      typeArgs,
      line: name.line,
      column: name.column,
    };
  }

  private recordType(): TypeExpr {
    const brace = this.advance(); // consume '{'
    const fields: Array<{ key: string; typeExpr: TypeExpr }> = [];

    if (!this.check(TokenType.RIGHT_BRACE)) {
      do {
        const key = this.consume(TokenType.IDENTIFIER, "Expected field name");
        this.consume(TokenType.COLON, "Expected ':' after field name");
        const typeExpr = this.typeExpression();
        fields.push({ key: key.lexeme, typeExpr });
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_BRACE, "Expected '}' after record type");

    return {
      type: "RecordType",
      fields,
      line: brace.line,
      column: brace.column,
    };
  }

  private signalPath(): SignalPath {
    // Consume optional @ prefix
    const at = this.match(TokenType.AT);
    const startToken = at ? this.previous() : this.peek();

    const parts: string[] = [];
    const first = this.consume(TokenType.IDENTIFIER, "Expected signal name");
    parts.push(first.lexeme);

    while (this.match(TokenType.DOT)) {
      const part = this.consume(TokenType.IDENTIFIER, "Expected signal path");
      parts.push(part.lexeme);
    }

    return {
      type: "SignalPath",
      parts,
      line: startToken.line,
      column: startToken.column,
    };
  }

  private block(): Block {
    const indent = this.consume(TokenType.INDENT, "Expected indented block");
    const statements: Stmt[] = [];

    while (!this.check(TokenType.DEDENT) && !this.isAtEnd()) {
      const stmt = this.declaration();
      if (stmt) {
        statements.push(stmt);
      }
    }

    this.consume(TokenType.DEDENT, "Expected end of block");

    return {
      type: "Block",
      statements,
      line: indent.line,
      column: indent.column,
    };
  }

  private expressionStatement(): ExprStmt {
    const expr = this.expression();
    return {
      type: "ExprStmt",
      expression: expr,
      line: expr.line,
      column: expr.column,
    };
  }

  // ============ Expressions ============

  private expression(): Expr {
    return this.assignment();
  }

  private assignment(): Expr {
    const expr = this.nullCoalesce();

    if (this.match(TokenType.EQUAL)) {
      const value = this.assignment();

      if (
        expr.type === "Identifier" ||
        expr.type === "Member" ||
        expr.type === "Index"
      ) {
        return {
          type: "Assign",
          target: expr as any,
          value,
          line: expr.line,
          column: expr.column,
        };
      }

      this.error(this.previous(), "Invalid assignment target");
    }

    return expr;
  }

  private nullCoalesce(): Expr {
    let expr = this.or();

    while (this.match(TokenType.QUESTION_QUESTION)) {
      const operator = this.previous();
      const right = this.or();
      expr = {
        type: "NullCoalesce",
        left: expr,
        right,
        line: operator.line,
        column: operator.column,
      } as NullCoalesceExpr;
    }

    return expr;
  }

  private or(): Expr {
    let left = this.and();

    while (this.match(TokenType.OR)) {
      const operator = this.previous();
      const right = this.and();
      left = {
        type: "Logical",
        left,
        operator,
        right,
        line: operator.line,
        column: operator.column,
      };
    }

    return left;
  }

  private and(): Expr {
    let left = this.equality();

    while (this.match(TokenType.AND)) {
      const operator = this.previous();
      const right = this.equality();
      left = {
        type: "Logical",
        left,
        operator,
        right,
        line: operator.line,
        column: operator.column,
      };
    }

    return left;
  }

  private equality(): Expr {
    let left = this.comparison();

    while (this.match(TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL)) {
      const operator = this.previous();
      const right = this.comparison();
      left = {
        type: "Binary",
        left,
        operator,
        right,
        line: operator.line,
        column: operator.column,
      };
    }

    return left;
  }

  private comparison(): Expr {
    let left = this.range();

    while (
      this.match(
        TokenType.LESS,
        TokenType.LESS_EQUAL,
        TokenType.GREATER,
        TokenType.GREATER_EQUAL
      )
    ) {
      const operator = this.previous();
      const right = this.range();
      left = {
        type: "Binary",
        left,
        operator,
        right,
        line: operator.line,
        column: operator.column,
      };
    }

    return left;
  }

  private range(): Expr {
    let expr = this.term();

    if (this.match(TokenType.DOT_DOT, TokenType.DOT_DOT_EQUAL)) {
      const inclusive = this.previous().type === TokenType.DOT_DOT_EQUAL;
      const end = this.term();
      expr = {
        type: "Range",
        start: expr,
        end,
        inclusive,
        line: expr.line,
        column: expr.column,
      };
    }

    return expr;
  }

  private term(): Expr {
    let left = this.factor();

    while (this.match(TokenType.PLUS, TokenType.MINUS)) {
      const operator = this.previous();
      const right = this.factor();
      left = {
        type: "Binary",
        left,
        operator,
        right,
        line: operator.line,
        column: operator.column,
      };
    }

    return left;
  }

  private factor(): Expr {
    let left = this.unary();

    while (this.match(TokenType.STAR, TokenType.SLASH, TokenType.PERCENT)) {
      const operator = this.previous();
      const right = this.unary();
      left = {
        type: "Binary",
        left,
        operator,
        right,
        line: operator.line,
        column: operator.column,
      };
    }

    return left;
  }

  private unary(): Expr {
    if (this.match(TokenType.MINUS, TokenType.NOT)) {
      const operator = this.previous();
      const operand = this.unary();
      return {
        type: "Unary",
        operator,
        operand,
        line: operator.line,
        column: operator.column,
      };
    }

    return this.call();
  }

  private call(): Expr {
    let expr = this.primary();

    while (true) {
      if (this.match(TokenType.LEFT_PAREN)) {
        expr = this.finishCall(expr);
      } else if (this.match(TokenType.DOT)) {
        const name = this.consume(
          TokenType.IDENTIFIER,
          "Expected property name"
        );
        expr = {
          type: "Member",
          object: expr,
          property: name.lexeme,
          line: name.line,
          column: name.column,
        };
      } else if (this.match(TokenType.LEFT_BRACKET)) {
        const index = this.expression();
        this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after index");
        expr = {
          type: "Index",
          object: expr,
          index,
          line: expr.line,
          column: expr.column,
        };
      } else if (this.match(TokenType.QUESTION_DOT)) {
        // Optional member access: obj?.prop
        const name = this.consume(
          TokenType.IDENTIFIER,
          "Expected property name after '?.'"
        );
        expr = {
          type: "OptionalMember",
          object: expr,
          property: name.lexeme,
          line: name.line,
          column: name.column,
        } as OptionalMemberExpr;
      } else if (this.match(TokenType.QUESTION_BRACKET)) {
        // Optional index access: obj?[index]
        const index = this.expression();
        this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after index");
        expr = {
          type: "OptionalIndex",
          object: expr,
          index,
          line: expr.line,
          column: expr.column,
        } as OptionalIndexExpr;
      } else if (this.match(TokenType.QUESTION_PAREN)) {
        // Optional call: func?()
        expr = this.finishOptionalCall(expr);
      } else {
        break;
      }
    }

    // Handle juxtaposition function calls (e.g., say "hello")
    // Disabled inside lambda bodies to prevent consuming next statement
    if (this.allowJuxtaposition && expr.type === "Identifier" && this.canBeArgument()) {
      const args = this.juxtapositionArgs();
      if (args.length > 0) {
        expr = {
          type: "Call",
          callee: expr,
          args,
          line: expr.line,
          column: expr.column,
        };
      }
    }

    return expr;
  }

  private canBeArgument(): boolean {
    return (
      this.check(TokenType.STRING) ||
      this.check(TokenType.NUMBER) ||
      this.check(TokenType.IDENTIFIER) ||
      this.check(TokenType.LEFT_BRACE) ||
      this.check(TokenType.LEFT_BRACKET) ||
      this.check(TokenType.HASH)
    );
  }

  private juxtapositionArgs(): Expr[] {
    const args: Expr[] = [];

    while (this.canBeArgument()) {
      args.push(this.primary());
      this.match(TokenType.COMMA); // optional comma
    }

    return args;
  }

  private finishCall(callee: Expr): CallExpr {
    const args: Expr[] = [];

    if (!this.check(TokenType.RIGHT_PAREN)) {
      do {
        // Support trailing commas - check for closing paren after comma
        if (this.check(TokenType.RIGHT_PAREN)) break;
        args.push(this.expression());
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_PAREN, "Expected ')' after arguments");

    return {
      type: "Call",
      callee,
      args,
      line: callee.line,
      column: callee.column,
    };
  }

  private finishOptionalCall(callee: Expr): OptionalCallExpr {
    const args: Expr[] = [];

    if (!this.check(TokenType.RIGHT_PAREN)) {
      do {
        // Support trailing commas - check for closing paren after comma
        if (this.check(TokenType.RIGHT_PAREN)) break;
        args.push(this.expression());
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_PAREN, "Expected ')' after arguments");

    return {
      type: "OptionalCall",
      callee,
      args,
      line: callee.line,
      column: callee.column,
    };
  }

  private lambdaExpression(): LambdaExpr {
    const startToken = this.previous(); // The opening PIPE
    const params: Array<{ name: string; typeAnnotation?: TypeExpr }> = [];

    // Parse parameters until closing |
    if (!this.check(TokenType.PIPE)) {
      do {
        const paramName = this.consume(
          TokenType.IDENTIFIER,
          "Expected parameter name"
        );
        // TODO: Optional type annotation with :Type syntax
        params.push({ name: paramName.lexeme });
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.PIPE, "Expected '|' after lambda parameters");

    // Parse the body expression with juxtaposition disabled
    // to prevent consuming tokens from the next statement
    const savedAllowJuxtaposition = this.allowJuxtaposition;
    this.allowJuxtaposition = false;
    const body = this.expression();
    this.allowJuxtaposition = savedAllowJuxtaposition;

    return {
      type: "Lambda",
      params,
      body,
      line: startToken.line,
      column: startToken.column,
    };
  }

  private primary(): Expr {
    // Literals
    if (this.match(TokenType.NUMBER)) {
      const token = this.previous();
      return {
        type: "Literal",
        value: token.literal as number,
        line: token.line,
        column: token.column,
      };
    }

    if (this.match(TokenType.STRING)) {
      const token = this.previous();
      return {
        type: "Literal",
        value: token.literal as string,
        line: token.line,
        column: token.column,
      };
    }

    // Interpolated string: "Hello, {name}!"
    if (this.match(TokenType.STRING_INTERP_START)) {
      return this.interpolatedString();
    }

    if (this.match(TokenType.TRUE)) {
      const token = this.previous();
      return {
        type: "Literal",
        value: true,
        line: token.line,
        column: token.column,
      };
    }

    if (this.match(TokenType.FALSE)) {
      const token = this.previous();
      return {
        type: "Literal",
        value: false,
        line: token.line,
        column: token.column,
      };
    }

    if (this.match(TokenType.NULL)) {
      const token = this.previous();
      return {
        type: "Literal",
        value: null,
        line: token.line,
        column: token.column,
      };
    }

    // Lambda expression: |x| x * 2, |a, b| a + b, || 42
    if (this.match(TokenType.PIPE)) {
      return this.lambdaExpression();
    }

    // Color literal
    if (this.match(TokenType.HASH)) {
      const token = this.previous();
      return {
        type: "Color",
        hex: token.lexeme,
        line: token.line,
        column: token.column,
      };
    }

    // If expression
    if (this.match(TokenType.IF)) {
      return this.ifExpression();
    }

    // Match expression
    if (this.match(TokenType.MATCH)) {
      return this.matchExpression();
    }

    // Record literal
    if (this.match(TokenType.LEFT_BRACE)) {
      return this.recordLiteral();
    }

    // List literal
    if (this.match(TokenType.LEFT_BRACKET)) {
      return this.listLiteral();
    }

    // Parenthesized expression
    if (this.match(TokenType.LEFT_PAREN)) {
      const expr = this.expression();
      this.consume(TokenType.RIGHT_PAREN, "Expected ')' after expression");
      return expr;
    }

    // Identifier
    if (this.match(TokenType.IDENTIFIER)) {
      const token = this.previous();

      // Check for `with` expression
      if (this.check(TokenType.WITH)) {
        this.advance(); // consume 'with'
        const updates = this.recordLiteral();
        return {
          type: "With",
          base: {
            type: "Identifier",
            name: token.lexeme,
            line: token.line,
            column: token.column,
          },
          updates,
          line: token.line,
          column: token.column,
        };
      }

      return {
        type: "Identifier",
        name: token.lexeme,
        line: token.line,
        column: token.column,
      };
    }

    throw this.error(this.peek(), "Expected expression");
  }

  private interpolatedString(): InterpolatedStringExpr {
    const startToken = this.previous();
    const parts: Array<
      { kind: "literal"; value: string } | { kind: "expr"; expr: Expr }
    > = [];

    // Add the first string part (from STRING_INTERP_START)
    const firstPart = startToken.literal as string;
    if (firstPart.length > 0) {
      parts.push({ kind: "literal", value: firstPart });
    }

    // Parse the expression
    parts.push({ kind: "expr", expr: this.expression() });

    // Continue parsing MIDDLE and END tokens
    while (this.match(TokenType.STRING_INTERP_MIDDLE)) {
      const middlePart = this.previous().literal as string;
      if (middlePart.length > 0) {
        parts.push({ kind: "literal", value: middlePart });
      }
      parts.push({ kind: "expr", expr: this.expression() });
    }

    // Expect the END token
    const endToken = this.consume(
      TokenType.STRING_INTERP_END,
      "Expected end of interpolated string"
    );
    const endPart = endToken.literal as string;
    if (endPart.length > 0) {
      parts.push({ kind: "literal", value: endPart });
    }

    return {
      type: "InterpolatedString",
      parts,
      line: startToken.line,
      column: startToken.column,
    };
  }

  private ifExpression(): IfExpr {
    const keyword = this.previous();
    const condition = this.expression();
    this.consume(TokenType.COLON, "Expected ':' after if condition");
    const thenBranch = this.block();

    let elseBranch: Block | IfExpr | undefined;
    if (this.match(TokenType.ELSE)) {
      if (this.match(TokenType.IF)) {
        elseBranch = this.ifExpression();
      } else {
        this.consume(TokenType.COLON, "Expected ':' after else");
        elseBranch = this.block();
      }
    }

    return {
      type: "If",
      condition,
      thenBranch,
      elseBranch,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private matchExpression(): MatchExpr {
    const keyword = this.previous();
    const subject = this.expression();
    this.consume(TokenType.COLON, "Expected ':' after match subject");
    this.consume(TokenType.INDENT, "Expected indented match arms");

    const arms: Array<{
      type: "MatchArm";
      pattern: Pattern;
      body: Expr;
      line: number;
      column: number;
    }> = [];

    while (!this.check(TokenType.DEDENT) && !this.isAtEnd()) {
      const pattern = this.pattern();
      this.consume(TokenType.ARROW, "Expected '=>' after pattern");
      const body = this.expression();
      arms.push({
        type: "MatchArm",
        pattern,
        body,
        line: pattern.line,
        column: pattern.column,
      });
    }

    this.consume(TokenType.DEDENT, "Expected end of match block");

    return {
      type: "Match",
      subject,
      arms,
      line: keyword.line,
      column: keyword.column,
    };
  }

  private pattern(): Pattern {
    // Wildcard
    if (this.match(TokenType.UNDERSCORE)) {
      const token = this.previous();
      return {
        type: "WildcardPattern",
        line: token.line,
        column: token.column,
      };
    }

    // Record pattern
    if (this.match(TokenType.LEFT_BRACE)) {
      return this.recordPattern();
    }

    // List pattern
    if (this.match(TokenType.LEFT_BRACKET)) {
      return this.listPattern();
    }

    // Literal patterns
    if (this.match(TokenType.NUMBER)) {
      const token = this.previous();
      return {
        type: "LiteralPattern",
        value: token.literal as number,
        line: token.line,
        column: token.column,
      };
    }

    if (this.match(TokenType.STRING)) {
      const token = this.previous();
      return {
        type: "LiteralPattern",
        value: token.literal as string,
        line: token.line,
        column: token.column,
      };
    }

    if (this.match(TokenType.TRUE)) {
      const token = this.previous();
      return {
        type: "LiteralPattern",
        value: true,
        line: token.line,
        column: token.column,
      };
    }

    if (this.match(TokenType.FALSE)) {
      const token = this.previous();
      return {
        type: "LiteralPattern",
        value: false,
        line: token.line,
        column: token.column,
      };
    }

    // Identifier pattern (binding)
    if (this.match(TokenType.IDENTIFIER)) {
      const token = this.previous();
      return {
        type: "IdentifierPattern",
        name: token.lexeme,
        line: token.line,
        column: token.column,
      };
    }

    throw this.error(this.peek(), "Expected pattern");
  }

  private recordPattern(): Pattern {
    const brace = this.previous();
    const fields: Array<{ key: string; pattern?: Pattern }> = [];

    if (!this.check(TokenType.RIGHT_BRACE)) {
      do {
        const key = this.consumeFieldName("Expected field name");
        let pattern: Pattern | undefined;
        if (this.match(TokenType.COLON)) {
          pattern = this.pattern();
        }
        fields.push({ key: key.lexeme, pattern });
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_BRACE, "Expected '}' after record pattern");

    return {
      type: "RecordPattern",
      fields,
      line: brace.line,
      column: brace.column,
    };
  }

  // Helper to consume a field name - accepts IDENTIFIER or keywords that can be field names
  private consumeFieldName(message: string): Token {
    // Keywords that are also valid field names
    const validFieldNames = [
      TokenType.IDENTIFIER,
      TokenType.TYPE,
      TokenType.MATCH,
      TokenType.IF,
      TokenType.FN,
      TokenType.LET,
      TokenType.VAR,
      TokenType.ON,
    ];

    if (validFieldNames.includes(this.peek().type)) {
      return this.advance();
    }
    throw this.error(this.peek(), message);
  }

  private listPattern(): Pattern {
    const bracket = this.previous();
    const elements: Pattern[] = [];
    let rest: string | undefined;

    if (!this.check(TokenType.RIGHT_BRACKET)) {
      do {
        if (this.match(TokenType.DOT_DOT)) {
          const restId = this.consume(
            TokenType.IDENTIFIER,
            "Expected identifier after '..'"
          );
          rest = restId.lexeme;
          break;
        }
        elements.push(this.pattern());
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after list pattern");

    return {
      type: "ListPattern",
      elements,
      rest,
      line: bracket.line,
      column: bracket.column,
    };
  }

  private recordLiteral(): RecordExpr {
    const brace = this.previous();
    const fields: Array<{ key: string; value: Expr }> = [];

    if (!this.check(TokenType.RIGHT_BRACE)) {
      do {
        // Support trailing commas - check for closing brace after comma
        if (this.check(TokenType.RIGHT_BRACE)) break;
        const key = this.consumeFieldName("Expected field name");
        this.consume(TokenType.COLON, "Expected ':' after field name");
        const value = this.expression();
        fields.push({ key: key.lexeme, value });
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_BRACE, "Expected '}' after record");

    return {
      type: "Record",
      fields,
      line: brace.line,
      column: brace.column,
    };
  }

  private listLiteral(): ListExpr {
    const bracket = this.previous();
    const elements: Expr[] = [];

    if (!this.check(TokenType.RIGHT_BRACKET)) {
      do {
        // Support trailing commas - check for closing bracket after comma
        if (this.check(TokenType.RIGHT_BRACKET)) break;
        elements.push(this.expression());
      } while (this.match(TokenType.COMMA));
    }

    this.consume(TokenType.RIGHT_BRACKET, "Expected ']' after list");

    return {
      type: "List",
      elements,
      line: bracket.line,
      column: bracket.column,
    };
  }

  // ============ Helpers ============

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

  private isAtEnd(): boolean {
    return this.peek().type === TokenType.EOF;
  }

  private peek(): Token {
    return this.tokens[this.current];
  }

  private peekNext(): Token | undefined {
    if (this.current + 1 >= this.tokens.length) return undefined;
    return this.tokens[this.current + 1];
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }

  private consume(type: TokenType, message: string): Token {
    if (this.check(type)) return this.advance();
    throw this.error(this.peek(), message);
  }

  private error(token: Token, message: string): ParseError {
    return new ParseError(message, token.line, token.column);
  }

  private synchronize(): void {
    this.advance();

    while (!this.isAtEnd()) {
      if (this.previous().type === TokenType.DEDENT) return;

      switch (this.peek().type) {
        case TokenType.LET:
        case TokenType.VAR:
        case TokenType.FN:
        case TokenType.ON:
        case TokenType.IF:
        case TokenType.MATCH:
        case TokenType.EMIT:
        case TokenType.IMPORT:
          return;
      }

      this.advance();
    }
  }
}
