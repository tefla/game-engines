import { Token, TokenType } from "./token";
import * as Stmt from "./ast";
import * as Expr from "./ast";

class ParseError extends Error { }

export class Parser {
    private current = 0;

    constructor(private tokens: Token[]) { }

    parse(): Stmt.Stmt[] {
        const statements: Stmt.Stmt[] = [];
        while (!this.isAtEnd()) {
            if (this.match(TokenType.NEWLINE)) continue;
            statements.push(this.declaration());
        }
        return statements;
    }

    private declaration(): Stmt.Stmt {
        try {
            let stmt: Stmt.Stmt;
            if (this.match(TokenType.VAR, TokenType.VAL)) stmt = this.varDeclaration();
            else if (this.match(TokenType.FUN)) stmt = this.function("function");
            else stmt = this.statement();

            // Consume optional trailing newline(s)
            while (this.match(TokenType.NEWLINE)); // Skip newlines

            return stmt;
        } catch (error) {
            this.synchronize();
            return null as any; // Should handle better in real compiler
        }
    }

    private function(kind: string): Stmt.Function {
        const name = this.consume(TokenType.IDENTIFIER, `Expect ${kind} name.`);
        this.consume(TokenType.LEFT_PAREN, `Expect '(' after ${kind} name.`);
        const parameters: { name: Token, typeName?: string }[] = [];

        if (!this.check(TokenType.RIGHT_PAREN)) {
            do {
                if (parameters.length >= 255) {
                    this.error(this.peek(), "Can't have more than 255 parameters.");
                }
                const paramName = this.consume(TokenType.IDENTIFIER, "Expect parameter name.");
                let typeName: string | undefined = undefined;
                if (this.match(TokenType.COLON)) {
                    typeName = this.consume(TokenType.IDENTIFIER, "Expect parameter type.").lexeme;
                }
                parameters.push({ name: paramName, typeName });
            } while (this.match(TokenType.COMMA));
        }
        this.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");

        let returnType: string | null = null;
        if (this.match(TokenType.COLON)) {
            returnType = this.consume(TokenType.IDENTIFIER, "Expect return type").lexeme;
        }

        while (this.match(TokenType.NEWLINE));
        const body = this.block();

        return new Stmt.Function(name, parameters, returnType, body);
    }

    private varDeclaration(): Stmt.Stmt {
        const name = this.consume(TokenType.IDENTIFIER, "Expect variable name.");
        let typeName: string | null = null;
        if (this.match(TokenType.COLON)) {
            typeName = this.consume(TokenType.IDENTIFIER, "Expect type name.").lexeme;
        }

        let initializer: Expr.Expr | null = null;
        if (this.match(TokenType.EQUAL)) {
            initializer = this.expression();
        }

        // this.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration."); 
        // Allowing defining vars without semicolon for scala-feel (newline check?)
        // For now assuming optional semicolon or just nothing.

        return new Stmt.Var(name, typeName, initializer);
    }

    private statement(): Stmt.Stmt {
        if (this.match(TokenType.IF)) return this.ifStatement();
        if (this.match(TokenType.WHILE)) return this.whileStatement();
        if (this.match(TokenType.FOR)) return this.forStatement(); // Todo
        if (this.match(TokenType.RETURN)) return this.returnStatement();
        // Assume INDENT starts a block statement? 
        // Or specific token? "Block" isn't a statement starter usually unless inside another structure.
        // But let's allow anonymous blocks?
        if (this.check(TokenType.INDENT)) return new Stmt.Block(this.block());

        return this.expressionStatement();
    }

    private ifStatement(): Stmt.Stmt {
        const condition = this.expression();
        while (this.match(TokenType.NEWLINE));
        const thenBranch = this.statement();
        let elseBranch: Stmt.Stmt | null = null;
        if (this.match(TokenType.ELSE)) {
            while (this.match(TokenType.NEWLINE));
            elseBranch = this.statement();
        }

        return new Stmt.If(condition, thenBranch, elseBranch);
    }

    private whileStatement(): Stmt.Stmt {
        const condition = this.expression();
        while (this.match(TokenType.NEWLINE));
        const body = this.statement();

        return new Stmt.While(condition, body);
    }

    private forStatement(): Stmt.Stmt {
        // Basic desugaring to while
        this.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
        // ... TODO simplify for now
        return null as any;
    }

    private returnStatement(): Stmt.Stmt {
        const keyword = this.previous();
        let value: Expr.Expr | null = null;
        if (!this.check(TokenType.DEDENT) && !this.check(TokenType.NEWLINE) && !this.isAtEnd()) {
            // Semicolons are tricky without them.
            // Let's assume if the next token is a value start
            value = this.expression();
        }
        // this.consume(TokenType.SEMICOLON, "Expect ';' after return value.");
        return new Stmt.Return(keyword, value);
    }

    private block(): Stmt.Stmt[] {
        const statements: Stmt.Stmt[] = [];
        // Optional newline logic handled by caller or just ignored if we look for INDENT
        // But INDENT follows NEWLINE usually.
        // Let's just consume INDENT. Caller must strip NEWLINE.
        this.consume(TokenType.INDENT, "Expect Indentation to start block.");

        while (!this.check(TokenType.DEDENT) && !this.isAtEnd()) {
            statements.push(this.declaration());
        }

        this.consume(TokenType.DEDENT, "Expect Dedentation to end block.");
        return statements;
    }

    private expressionStatement(): Stmt.Stmt {
        const expr = this.expression();
        // this.consume(TokenType.SEMICOLON, "Expect ';' after expression.");
        return new Stmt.Expression(expr);
    }

    private expression(): Expr.Expr {
        return this.assignment();
    }

    private assignment(): Expr.Expr {
        const expr = this.arrow(); // Check for lambda: (args) => ...

        if (this.match(TokenType.EQUAL)) {
            const equals = this.previous();
            const value = this.assignment();

            if (expr instanceof Expr.Variable) {
                const name = expr.name;
                return new Expr.Assign(name, value);
            } else if (expr instanceof Expr.Get) {
                // Setter: obj.prop = val
                // We can desugar this to obj.set(prop, val) or keep as Assign
                // Keeping as Assign is standard, interpreter handles it.
                // But wait, ast.ts Assign takes a token name.
                // We need SetExpr for property sets.
                // For now, let's limit assignment to variables.
                this.error(equals, "Invalid assignment target.");
            }

            this.error(equals, "Invalid assignment target.");
        }

        return expr;
    }

    private arrow(): Expr.Expr {
        // Lookahead is tricky. 
        // If we see '(', it could be logical grouping OR lambda params.
        // (a, b) => ...
        // If we are at '(', let's try to parse as logicOr, if we hit '=>', it was a lambda.
        // Actually simpler:
        // If matches '(', check contents.
        // Or we can just parse 'logicOr'.
        // If we hit '=>' after logicOr, then the logicOr MUST be valid params.

        // Let's defer full lambda parsing for a moment and prioritize basic logic.
        // Standard precedence:
        return this.or();
    }

    private or(): Expr.Expr {
        let expr = this.and();
        while (this.match(TokenType.OR)) {
            const operator = this.previous();
            const right = this.and();
            expr = new Expr.Logical(expr, operator, right);
        }
        return expr;
    }

    private and(): Expr.Expr {
        let expr = this.equality();
        while (this.match(TokenType.AND)) {
            const operator = this.previous();
            const right = this.equality();
            expr = new Expr.Logical(expr, operator, right);
        }
        return expr;
    }

    private equality(): Expr.Expr {
        let expr = this.comparison();
        while (this.match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
            const operator = this.previous();
            const right = this.comparison();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    private comparison(): Expr.Expr {
        let expr = this.term();
        while (this.match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
            const operator = this.previous();
            const right = this.term();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    private term(): Expr.Expr {
        let expr = this.factor();
        while (this.match(TokenType.MINUS, TokenType.PLUS)) {
            const operator = this.previous();
            const right = this.factor();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    private factor(): Expr.Expr {
        let expr = this.unary();
        while (this.match(TokenType.SLASH, TokenType.STAR)) {
            const operator = this.previous();
            const right = this.unary();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    private unary(): Expr.Expr {
        if (this.match(TokenType.BANG, TokenType.MINUS)) {
            const operator = this.previous();
            const right = this.unary();
            return new Expr.Unary(operator, right);
        }
        return this.infixCall();
    }

    private infixCall(): Expr.Expr {
        // Helper for infix sugar e.g. "player has Item" -> player.has(Item)
        // This is tricky.
        // For now, just standard call.
        return this.call();
    }

    private call(): Expr.Expr {
        let expr = this.primary();

        while (true) {
            if (this.match(TokenType.LEFT_PAREN)) {
                expr = this.finishCall(expr);
            } else if (this.match(TokenType.DOT)) {
                const name = this.consume(TokenType.IDENTIFIER, "Expect property name after '.'.");
                expr = new Expr.Get(expr, name);
            } else if ((this.check(TokenType.INDENT) || (this.check(TokenType.NEWLINE) && this.peekNext().type === TokenType.INDENT)) && expr instanceof Expr.Call) {
                // Trailing Lambda with INDENT (handle optional newline)
                while (this.match(TokenType.NEWLINE));
                const block = this.block();
                const lambda = new Expr.Lambda([], block);
                (expr as Expr.Call).args.push(lambda);
            } else if (expr instanceof Expr.Variable && !this.check(TokenType.RIGHT_PAREN) && !this.check(TokenType.COMMA)) {
                // COMMAND SYNTAX Check: func arg
                // IF the next token is a Literal or Ident, we can consume it as arg.
                // Only allow if we haven't already started a paren call? 
                // Actually this loop is "after primary".
                // If expr is Variable("UI") and next is String("Foo")
                if (this.match(TokenType.STRING, TokenType.NUMBER, TokenType.IDENTIFIER)) {
                    const arg = new Expr.Literal(this.previous().literal);
                    // If ident, it should be variable?
                    // Literal helper consumes literal value. 
                    // Fix: if IDENTIFIER, make Variable.
                    let argExpr: Expr.Expr;
                    if (this.previous().type === TokenType.IDENTIFIER) {
                        argExpr = new Expr.Variable(this.previous());
                    } else {
                        argExpr = new Expr.Literal(this.previous().literal);
                    }

                    // Convert 'expr' (Callee) into Call(Callee, [Arg])
                    expr = new Expr.Call(expr as Expr.Variable, this.previous(), [argExpr]);
                    // Continued loop allows chaining? UI "Foo" do ... end
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        return expr;
    }

    private finishCall(callee: Expr.Expr): Expr.Expr {
        const args: Expr.Expr[] = [];
        if (!this.check(TokenType.RIGHT_PAREN)) {
            do {
                if (args.length >= 255) {
                    this.error(this.peek(), "Can't have more than 255 arguments.");
                }

                // Check if lambda expression (x) => ...
                if (this.check(TokenType.LEFT_PAREN) || this.check(TokenType.IDENTIFIER)) {
                    // Peek ahead to see if '=>' exists?
                    // Or simpler: parse expression. 
                    // If we want inline lambdas `(x) => x+1`, we need expression() to handle it.
                }
                args.push(this.expression());
            } while (this.match(TokenType.COMMA));
        }

        const paren = this.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");

        // Check for trailing lambda IMMEDIATELY after parens?
        // Handled in the loop in `call()`

        return new Expr.Call(callee, paren, args);
    }

    private primary(): Expr.Expr {
        if (this.match(TokenType.FALSE)) return new Expr.Literal(false);
        if (this.match(TokenType.TRUE)) return new Expr.Literal(true);
        if (this.match(TokenType.NIL)) return new Expr.Literal(null);

        if (this.match(TokenType.NUMBER, TokenType.STRING)) {
            return new Expr.Literal(this.previous().literal);
        }

        if (this.match(TokenType.IDENTIFIER)) {
            return new Expr.Variable(this.previous());
        }

        if (this.match(TokenType.LEFT_PAREN)) {
            // Could be grouping (expr) OR lambda params (a,b) =>
            // Naive approach: Parse expression.
            const expr = this.expression();
            this.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");

            // Check for Arrow
            if (this.match(TokenType.ARROW)) {
                // It was a lambda!
                // We need to convert `expr` (which might be a Variable or Comma/Binary) into Params.
                // This is messy. Verification required.
                // For now, let's implement Grouping only.
            }
            return new Expr.Grouping(expr);
        }

        // Support parsing (x,y) => ... specifically at primary level?

        throw this.error(this.peek(), "Expect expression.");
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
        return this.peek().type === TokenType.EOF;
    }

    private peek(): Token {
        return this.tokens[this.current] || this.tokens[this.tokens.length - 1]; // Safe EOF fallback
    }

    private peekNext(): Token {
        return this.tokens[this.current + 1] || this.tokens[this.tokens.length - 1];
    }

    private previous(): Token {
        return this.tokens[this.current - 1] || this.tokens[0];
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

    private synchronize() {
        this.advance();

        while (!this.isAtEnd()) {
            if (this.previous().type === TokenType.NEWLINE) return;

            switch (this.peek().type) {
                case TokenType.CLASS:
                case TokenType.FUN:
                case TokenType.VAR:
                case TokenType.FOR:
                case TokenType.IF:
                case TokenType.WHILE:
                case TokenType.PRINT:
                case TokenType.RETURN:
                    return;
            }

            this.advance();
        }
    }

    private error(token: Token, message: string): ParseError {
        console.error(`[line ${token.line}] Error at '${token.lexeme}': ${message}`);
        return new ParseError();
    }
}
