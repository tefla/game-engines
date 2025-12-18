
export enum TokenType {
    // Single-character tokens
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SLASH, STAR, COLON,

    // One or two character tokens
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,
    ARROW, // =>

    // Literals
    IDENTIFIER, STRING, NUMBER,

    // Keywords
    AND, CLASS, DO, ELSE, END, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, VAL, WHILE,
    SYSTEM, ON, // DSL specific

    // Significant Whitespace
    INDENT, DEDENT, NEWLINE,

    EOF
}

export class Token {
    constructor(
        public readonly type: TokenType,
        public readonly lexeme: string,
        public readonly literal: any,
        public readonly line: number
    ) { }

    toString(): string {
        return `${TokenType[this.type]} ${this.lexeme} ${this.literal}`;
    }
}
