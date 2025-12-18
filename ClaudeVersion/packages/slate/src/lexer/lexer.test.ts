import { describe, it, expect } from "bun:test";
import { Lexer } from "./lexer";
import { TokenType } from "@oort/core";

describe("Lexer", () => {
  describe("basic tokens", () => {
    it("tokenizes numbers", () => {
      const lexer = new Lexer("42 3.14 100");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.NUMBER);
      expect(tokens[0].literal).toBe(42);
      expect(tokens[1].type).toBe(TokenType.NUMBER);
      expect(tokens[1].literal).toBe(3.14);
      expect(tokens[2].type).toBe(TokenType.NUMBER);
      expect(tokens[2].literal).toBe(100);
    });

    it("tokenizes strings", () => {
      const lexer = new Lexer('"hello" "world"');
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.STRING);
      expect(tokens[0].literal).toBe("hello");
      expect(tokens[1].type).toBe(TokenType.STRING);
      expect(tokens[1].literal).toBe("world");
    });

    it("tokenizes strings with escape sequences", () => {
      const lexer = new Lexer('"hello\\nworld" "tab\\there"');
      const tokens = lexer.tokenize();

      expect(tokens[0].literal).toBe("hello\nworld");
      expect(tokens[1].literal).toBe("tab\there");
    });

    it("tokenizes identifiers", () => {
      const lexer = new Lexer("foo bar_baz x1 _private");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[0].lexeme).toBe("foo");
      expect(tokens[1].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[1].lexeme).toBe("bar_baz");
      expect(tokens[2].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[2].lexeme).toBe("x1");
      expect(tokens[3].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[3].lexeme).toBe("_private");
    });
  });

  describe("keywords", () => {
    it("tokenizes all 7 core keywords", () => {
      const lexer = new Lexer("let var fn if match on extend");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.LET);
      expect(tokens[1].type).toBe(TokenType.VAR);
      expect(tokens[2].type).toBe(TokenType.FN);
      expect(tokens[3].type).toBe(TokenType.IF);
      expect(tokens[4].type).toBe(TokenType.MATCH);
      expect(tokens[5].type).toBe(TokenType.ON);
      expect(tokens[6].type).toBe(TokenType.EXTEND);
    });

    it("tokenizes additional keywords", () => {
      const lexer = new Lexer("true false not and or emit else");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.TRUE);
      expect(tokens[1].type).toBe(TokenType.FALSE);
      expect(tokens[2].type).toBe(TokenType.NOT);
      expect(tokens[3].type).toBe(TokenType.AND);
      expect(tokens[4].type).toBe(TokenType.OR);
      expect(tokens[5].type).toBe(TokenType.EMIT);
      expect(tokens[6].type).toBe(TokenType.ELSE);
    });
  });

  describe("operators", () => {
    it("tokenizes arithmetic operators", () => {
      const lexer = new Lexer("+ - * / %");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.PLUS);
      expect(tokens[1].type).toBe(TokenType.MINUS);
      expect(tokens[2].type).toBe(TokenType.STAR);
      expect(tokens[3].type).toBe(TokenType.SLASH);
      expect(tokens[4].type).toBe(TokenType.PERCENT);
    });

    it("tokenizes comparison operators", () => {
      const lexer = new Lexer("< <= > >= == !=");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.LESS);
      expect(tokens[1].type).toBe(TokenType.LESS_EQUAL);
      expect(tokens[2].type).toBe(TokenType.GREATER);
      expect(tokens[3].type).toBe(TokenType.GREATER_EQUAL);
      expect(tokens[4].type).toBe(TokenType.EQUAL_EQUAL);
      expect(tokens[5].type).toBe(TokenType.BANG_EQUAL);
    });

    it("tokenizes special operators", () => {
      const lexer = new Lexer("= => -> . .. :");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.EQUAL);
      expect(tokens[1].type).toBe(TokenType.ARROW);
      expect(tokens[2].type).toBe(TokenType.THIN_ARROW);
      expect(tokens[3].type).toBe(TokenType.DOT);
      expect(tokens[4].type).toBe(TokenType.DOT_DOT);
      expect(tokens[5].type).toBe(TokenType.COLON);
    });
  });

  describe("brackets", () => {
    it("tokenizes all bracket types", () => {
      const lexer = new Lexer("() [] {}");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.LEFT_PAREN);
      expect(tokens[1].type).toBe(TokenType.RIGHT_PAREN);
      expect(tokens[2].type).toBe(TokenType.LEFT_BRACKET);
      expect(tokens[3].type).toBe(TokenType.RIGHT_BRACKET);
      expect(tokens[4].type).toBe(TokenType.LEFT_BRACE);
      expect(tokens[5].type).toBe(TokenType.RIGHT_BRACE);
    });
  });

  describe("indentation", () => {
    it("generates INDENT and DEDENT tokens", () => {
      const lexer = new Lexer(`fn foo:
    let x = 1
    let y = 2
let z = 3`);
      const tokens = lexer.tokenize();

      const types = tokens.map((t) => t.type);
      expect(types).toContain(TokenType.INDENT);
      expect(types).toContain(TokenType.DEDENT);
    });

    it("handles nested indentation", () => {
      const lexer = new Lexer(`if true:
    if false:
        x
    y
z`);
      const tokens = lexer.tokenize();

      const indentCount = tokens.filter(
        (t) => t.type === TokenType.INDENT
      ).length;
      const dedentCount = tokens.filter(
        (t) => t.type === TokenType.DEDENT
      ).length;
      expect(indentCount).toBe(2);
      expect(dedentCount).toBe(2);
    });
  });

  describe("comments", () => {
    it("skips single-line comments", () => {
      const lexer = new Lexer(`let x = 1 # this is a comment
let y = 2`);
      const tokens = lexer.tokenize();

      // Should not contain any comment tokens
      const identifiers = tokens.filter(
        (t) => t.type === TokenType.IDENTIFIER
      );
      expect(identifiers.map((t) => t.lexeme)).toEqual(["x", "y"]);
    });
  });

  describe("color literals", () => {
    it("tokenizes hex colors", () => {
      const lexer = new Lexer("#FF0000 #00FF00 #808080");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.HASH);
      expect(tokens[0].lexeme).toBe("#FF0000");
      expect(tokens[1].type).toBe(TokenType.HASH);
      expect(tokens[1].lexeme).toBe("#00FF00");
      expect(tokens[2].type).toBe(TokenType.HASH);
      expect(tokens[2].lexeme).toBe("#808080");
    });
  });

  describe("edge cases", () => {
    it("handles empty input", () => {
      const lexer = new Lexer("");
      const tokens = lexer.tokenize();

      expect(tokens.length).toBe(1);
      expect(tokens[0].type).toBe(TokenType.EOF);
    });

    it("handles underscore as wildcard", () => {
      const lexer = new Lexer("_ _foo");
      const tokens = lexer.tokenize();

      expect(tokens[0].type).toBe(TokenType.UNDERSCORE);
      expect(tokens[1].type).toBe(TokenType.IDENTIFIER);
      expect(tokens[1].lexeme).toBe("_foo");
    });

    it("tracks line and column numbers", () => {
      const lexer = new Lexer(`let x = 1
let y = 2`);
      const tokens = lexer.tokenize();

      const letTokens = tokens.filter((t) => t.type === TokenType.LET);
      expect(letTokens[0].line).toBe(1);
      expect(letTokens[1].line).toBe(2);
    });
  });
});
