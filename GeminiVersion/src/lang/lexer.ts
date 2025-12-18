import { Token, TokenType } from "./token";

export class Lexer {
    private source: string;
    private tokens: Token[] = [];
    private start = 0;
    private current = 0;
    private line = 1;

    // Indentation State
    private indentStack: number[] = [0];
    private pendingDedents = 0;

    private static keywords: Record<string, TokenType> = {
        "and": TokenType.AND,
        "class": TokenType.CLASS,
        "do": TokenType.DO,
        "else": TokenType.ELSE,
        "end": TokenType.END,
        "false": TokenType.FALSE,
        "for": TokenType.FOR,
        "fun": TokenType.FUN,
        "if": TokenType.IF,
        "nil": TokenType.NIL,
        "or": TokenType.OR,
        "return": TokenType.RETURN,
        "super": TokenType.SUPER,
        "this": TokenType.THIS,
        "true": TokenType.TRUE,
        "var": TokenType.VAR,
        "val": TokenType.VAL,
        "while": TokenType.WHILE,
    };

    constructor(source: string) {
        this.source = source;
    }

    scanTokens(): Token[] {
        while (!this.isAtEnd()) {
            this.start = this.current;

            // If we have pending dedents, emit them before scanning next char
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

        this.tokens.push(new Token(TokenType.EOF, "", null, this.line));
        return this.tokens;
    }

    private scanToken() {
        const c = this.advance();
        switch (c) {
            case '(': this.addToken(TokenType.LEFT_PAREN); break;
            case ')': this.addToken(TokenType.RIGHT_PAREN); break;
            case '{': this.addToken(TokenType.LEFT_BRACE); break;
            case '}': this.addToken(TokenType.RIGHT_BRACE); break;
            case ',': this.addToken(TokenType.COMMA); break;
            case '.': this.addToken(TokenType.DOT); break;
            case '-': this.addToken(TokenType.MINUS); break;
            case '+': this.addToken(TokenType.PLUS); break;
            case '*': this.addToken(TokenType.STAR); break;
            case ':': this.addToken(TokenType.COLON); break;
            case '!':
                this.addToken(this.match('=') ? TokenType.BANG_EQUAL : TokenType.BANG);
                break;
            case '=':
                if (this.match('>')) {
                    this.addToken(TokenType.ARROW);
                } else {
                    this.addToken(this.match('=') ? TokenType.EQUAL_EQUAL : TokenType.EQUAL);
                }
                break;
            case '<':
                this.addToken(this.match('=') ? TokenType.LESS_EQUAL : TokenType.LESS);
                break;
            case '>':
                this.addToken(this.match('=') ? TokenType.GREATER_EQUAL : TokenType.GREATER);
                break;
            case '/':
                if (this.match('/')) {
                    while (this.peek() != '\n' && !this.isAtEnd()) this.advance();
                } else {
                    this.addToken(TokenType.SLASH);
                }
                break;

            case ' ':
            case '\r':
            case '\t':
                // Ignore whitespace inside lines
                break;

            case '\n':
                this.handleNewline();
                break;

            case '"': this.string(); break;

            default:
                if (this.isDigit(c)) {
                    this.number();
                } else if (this.isAlpha(c)) {
                    this.identifier();
                } else {
                    console.error(`Unexpected character: ${c} at line ${this.line}`);
                }
                break;
        }
    }

    private handleNewline() {
        this.line++;
        this.addToken(TokenType.NEWLINE);

        // Calculate next indent
        let indent = 0;
        while (this.peek() == ' ' || this.peek() == '\t') {
            // Tab = 4 spaces assumption? Or just raw chars.
            if (this.peek() == '\t') indent += 4;
            else indent += 1;
            this.advance();
        }

        // Ignore empty lines
        if (this.peek() == '\n' || this.isAtEnd()) return;

        // Check Indent
        const currentIndent = this.indentStack[this.indentStack.length - 1] ?? 0;

        if (indent > currentIndent) {
            this.indentStack.push(indent);
            this.addToken(TokenType.INDENT);
        } else if (indent < currentIndent) {
            while (indent < (this.indentStack[this.indentStack.length - 1] ?? 0)) {
                this.indentStack.pop();
                this.pendingDedents++;
            }
            if (indent != (this.indentStack[this.indentStack.length - 1] ?? 0)) {
                console.error("Indentation Error: mismatched levels");
            }
        }
    }

    private identifier() {
        while (this.isAlphaNumeric(this.peek())) this.advance();

        const text = this.source.substring(this.start, this.current);
        let type = Lexer.keywords[text];
        if (type == undefined) type = TokenType.IDENTIFIER;

        this.addToken(type);
    }

    private number() {
        while (this.isDigit(this.peek())) this.advance();

        if (this.peek() == '.' && this.isDigit(this.peekNext())) {
            this.advance(); // Consume the "."
            while (this.isDigit(this.peek())) this.advance();
        }

        this.addToken(TokenType.NUMBER, Number(this.source.substring(this.start, this.current)));
    }

    private string() {
        while (this.peek() != '"' && !this.isAtEnd()) {
            if (this.peek() == '\n') this.line++;
            this.advance();
        }

        if (this.isAtEnd()) {
            console.error("Unterminated string.");
            return;
        }

        this.advance(); // The closing "

        const value = this.source.substring(this.start + 1, this.current - 1);
        this.addToken(TokenType.STRING, value);
    }

    private match(expected: string): boolean {
        if (this.isAtEnd()) return false;
        if (this.source.charAt(this.current) != expected) return false;
        this.current++;
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

    private isAlpha(c: string): boolean {
        return (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c == '_';
    }

    private isAlphaNumeric(c: string): boolean {
        return this.isAlpha(c) || this.isDigit(c);
    }

    private isDigit(c: string): boolean {
        return c >= '0' && c <= '9';
    }

    private isAtEnd(): boolean {
        return this.current >= this.source.length;
    }

    private advance(): string {
        return this.source.charAt(this.current++);
    }

    private addToken(type: TokenType, literal: any = null) {
        const text = (type === TokenType.NEWLINE || type === TokenType.INDENT || type === TokenType.DEDENT)
            ? ""
            : this.source.substring(this.start, this.current);
        this.tokens.push(new Token(type, text, literal, this.line));
    }
}
