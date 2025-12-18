import { Token, TokenType } from "./token";

const KEYWORDS: Record<string, TokenType> = {
    "let": TokenType.LET,
    "fn": TokenType.FN,
    "if": TokenType.IF,
    "else": TokenType.ELSE,
    "match": TokenType.MATCH,
    "use": TokenType.USE,
    "extend": TokenType.EXTEND,
    "on": TokenType.ON,
};

export class Lexer {
    private source: string;
    private tokens: Token[] = [];
    private start = 0;
    private current = 0;
    private line = 1;
    private column = 1;

    // Indentation tracking
    private indentStack: number[] = [0];
    private pendingDedents = 0;
    private atLineStart = true;

    constructor(source: string) {
        this.source = source;
    }

    scanTokens(): Token[] {
        while (!this.isAtEnd()) {
            this.start = this.current;

            // Emit pending dedents
            if (this.pendingDedents > 0) {
                this.addToken(TokenType.DEDENT);
                this.pendingDedents--;
                continue;
            }

            this.scanToken();
        }

        // Unwind indentation at EOF
        while (this.indentStack.length > 1) {
            this.addToken(TokenType.DEDENT);
            this.indentStack.pop();
        }

        this.tokens.push(new Token(TokenType.EOF, "", null, this.line, this.column));
        return this.tokens;
    }

    private scanToken() {
        const c = this.advance();

        switch (c) {
            // Single-char tokens
            case '(': this.addToken(TokenType.LEFT_PAREN); break;
            case ')': this.addToken(TokenType.RIGHT_PAREN); break;
            case '[': this.addToken(TokenType.LEFT_BRACKET); break;
            case ']': this.addToken(TokenType.RIGHT_BRACKET); break;
            case ',': this.addToken(TokenType.COMMA); break;
            case '.': this.addToken(TokenType.DOT); break;
            case '+': this.addToken(TokenType.PLUS); break;
            case '*': this.addToken(TokenType.STAR); break;
            case '/': this.addToken(TokenType.SLASH); break;

            // Minus or list item
            case '-':
                if (this.atLineStart) {
                    this.addToken(TokenType.DASH);
                } else {
                    this.addToken(TokenType.MINUS);
                }
                break;

            // Colon
            case ':': this.addToken(TokenType.COLON); break;

            // Operators with lookahead
            case '!':
                this.addToken(this.match('=') ? TokenType.BANG_EQUAL : TokenType.IDENTIFIER);
                break;
            case '=':
                if (this.match('>')) {
                    this.addToken(TokenType.ARROW);
                } else if (this.match('=')) {
                    this.addToken(TokenType.EQUAL_EQUAL);
                } else {
                    this.addToken(TokenType.EQUAL);
                }
                break;
            case '<':
                this.addToken(this.match('=') ? TokenType.LESS_EQUAL : TokenType.LESS);
                break;
            case '>':
                this.addToken(this.match('=') ? TokenType.GREATER_EQUAL : TokenType.GREATER);
                break;

            // Comments
            case '#':
                while (this.peek() !== '\n' && !this.isAtEnd()) this.advance();
                break;

            // Whitespace
            case ' ':
            case '\r':
            case '\t':
                // Ignore mid-line whitespace
                break;

            case '\n':
                this.handleNewline();
                break;

            // Strings
            case '"': this.string(); break;

            default:
                if (this.isDigit(c)) {
                    this.number();
                } else if (this.isAlpha(c)) {
                    this.identifier();
                } else {
                    console.error(`[line ${this.line}] Unexpected character: ${c}`);
                }
                break;
        }

        this.atLineStart = false;
    }

    private handleNewline() {
        this.addToken(TokenType.NEWLINE);
        this.line++;
        this.column = 1;
        this.atLineStart = true;

        // Calculate indentation
        let indent = 0;
        while (this.peek() === ' ' || this.peek() === '\t') {
            if (this.peek() === '\t') indent += 4;
            else indent += 1;
            this.advance();
        }

        // Skip blank lines
        if (this.peek() === '\n' || this.peek() === '#' || this.isAtEnd()) return;

        const currentIndent = this.indentStack[this.indentStack.length - 1] ?? 0;

        if (indent > currentIndent) {
            this.indentStack.push(indent);
            this.addToken(TokenType.INDENT);
        } else if (indent < currentIndent) {
            while (indent < (this.indentStack[this.indentStack.length - 1] ?? 0)) {
                this.indentStack.pop();
                this.pendingDedents++;
            }
            if (indent !== (this.indentStack[this.indentStack.length - 1] ?? 0)) {
                console.error(`[line ${this.line}] Indentation error`);
            }
        }
    }

    private string() {
        while (this.peek() !== '"' && !this.isAtEnd()) {
            if (this.peek() === '\n') this.line++;
            this.advance();
        }

        if (this.isAtEnd()) {
            console.error(`[line ${this.line}] Unterminated string`);
            return;
        }

        this.advance(); // Closing "
        const value = this.source.substring(this.start + 1, this.current - 1);
        this.addToken(TokenType.STRING, value);
    }

    private number() {
        while (this.isDigit(this.peek())) this.advance();

        if (this.peek() === '.' && this.isDigit(this.peekNext())) {
            this.advance(); // Consume '.'
            while (this.isDigit(this.peek())) this.advance();
        }

        const value = parseFloat(this.source.substring(this.start, this.current));
        this.addToken(TokenType.NUMBER, value);
    }

    private identifier() {
        while (this.isAlphaNumeric(this.peek())) this.advance();

        const text = this.source.substring(this.start, this.current);
        const type = KEYWORDS[text] ?? TokenType.IDENTIFIER;
        this.addToken(type);
    }

    // Helpers
    private advance(): string {
        this.column++;
        return this.source.charAt(this.current++);
    }

    private match(expected: string): boolean {
        if (this.isAtEnd()) return false;
        if (this.source.charAt(this.current) !== expected) return false;
        this.current++;
        this.column++;
        return true;
    }

    private peek(): string {
        if (this.isAtEnd()) return '\0';
        return this.source.charAt(this.current);
    }

    private peekNext(): string {
        if (this.current + 1 >= this.source.length) return '\0';
        return this.source.charAt(this.current + 1);
    }

    private isAtEnd(): boolean {
        return this.current >= this.source.length;
    }

    private isDigit(c: string): boolean {
        return c >= '0' && c <= '9';
    }

    private isAlpha(c: string): boolean {
        return (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c === '_';
    }

    private isAlphaNumeric(c: string): boolean {
        return this.isAlpha(c) || this.isDigit(c);
    }

    private addToken(type: TokenType, literal: any = null) {
        const text = this.source.substring(this.start, this.current);
        this.tokens.push(new Token(type, text, literal, this.line, this.column));
    }
}
