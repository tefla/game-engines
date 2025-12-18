// Slate Token Types
export enum TokenType {
    // Literals
    NUMBER,
    STRING,
    IDENTIFIER,
    SYMBOL,         // :name

    // Keywords (7 total)
    LET,
    FN,
    IF,
    ELSE,
    MATCH,
    USE,
    EXTEND,
    ON,

    // Operators
    PLUS,
    MINUS,
    STAR,
    SLASH,
    EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    ARROW,          // =>
    DOT,
    COMMA,
    COLON,

    // Brackets
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,

    // Whitespace Control
    NEWLINE,
    INDENT,
    DEDENT,

    // Special
    DASH,           // - for list items
    EOF,
}

export class Token {
    constructor(
        public type: TokenType,
        public lexeme: string,
        public literal: any,
        public line: number,
        public column: number
    ) { }

    toString(): string {
        return `${TokenType[this.type]} '${this.lexeme}' ${this.literal}`;
    }
}
