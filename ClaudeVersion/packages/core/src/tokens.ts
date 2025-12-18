// Token types for the Slate lexer

export enum TokenType {
  // Literals
  NUMBER = "NUMBER",
  STRING = "STRING",
  IDENTIFIER = "IDENTIFIER",

  // Keywords (7 core)
  LET = "LET",
  VAR = "VAR",
  FN = "FN",
  IF = "IF",
  ELSE = "ELSE",
  MATCH = "MATCH",
  ON = "ON",
  EXTEND = "EXTEND",

  // Additional keywords
  TRUE = "TRUE",
  FALSE = "FALSE",
  NOT = "NOT",
  AND = "AND",
  OR = "OR",
  EMIT = "EMIT",
  IMPORT = "IMPORT",
  LOOP = "LOOP",
  FOR = "FOR",
  IN = "IN",
  BY = "BY",
  YIELD = "YIELD",
  SPAWN = "SPAWN",
  WAIT = "WAIT",
  TYPE = "TYPE",
  WITH = "WITH",

  // Operators
  PLUS = "PLUS",
  MINUS = "MINUS",
  STAR = "STAR",
  SLASH = "SLASH",
  PERCENT = "PERCENT",
  EQUAL = "EQUAL",
  EQUAL_EQUAL = "EQUAL_EQUAL",
  BANG_EQUAL = "BANG_EQUAL",
  LESS = "LESS",
  LESS_EQUAL = "LESS_EQUAL",
  GREATER = "GREATER",
  GREATER_EQUAL = "GREATER_EQUAL",
  ARROW = "ARROW",           // =>
  THIN_ARROW = "THIN_ARROW", // ->
  DOT = "DOT",
  DOT_DOT = "DOT_DOT",       // ..
  COMMA = "COMMA",
  COLON = "COLON",
  PIPE = "PIPE",
  UNDERSCORE = "UNDERSCORE",
  HASH = "HASH",

  // Brackets
  LEFT_PAREN = "LEFT_PAREN",
  RIGHT_PAREN = "RIGHT_PAREN",
  LEFT_BRACKET = "LEFT_BRACKET",
  RIGHT_BRACKET = "RIGHT_BRACKET",
  LEFT_BRACE = "LEFT_BRACE",
  RIGHT_BRACE = "RIGHT_BRACE",

  // Indentation
  INDENT = "INDENT",
  DEDENT = "DEDENT",
  NEWLINE = "NEWLINE",

  // Special
  AT = "AT", // @ for signal paths
  EOF = "EOF",
  ERROR = "ERROR",
}

export interface Token {
  type: TokenType;
  lexeme: string;
  literal: unknown;
  line: number;
  column: number;
}

export const KEYWORDS: Record<string, TokenType> = {
  "let": TokenType.LET,
  "var": TokenType.VAR,
  "fn": TokenType.FN,
  "if": TokenType.IF,
  "else": TokenType.ELSE,
  "match": TokenType.MATCH,
  "on": TokenType.ON,
  "extend": TokenType.EXTEND,
  "true": TokenType.TRUE,
  "false": TokenType.FALSE,
  "not": TokenType.NOT,
  "and": TokenType.AND,
  "or": TokenType.OR,
  "emit": TokenType.EMIT,
  "import": TokenType.IMPORT,
  "loop": TokenType.LOOP,
  "for": TokenType.FOR,
  "in": TokenType.IN,
  "by": TokenType.BY,
  "yield": TokenType.YIELD,
  "spawn": TokenType.SPAWN,
  "wait": TokenType.WAIT,
  "type": TokenType.TYPE,
  "with": TokenType.WITH,
};
