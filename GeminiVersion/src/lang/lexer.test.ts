import { describe, it, expect } from "bun:test";
import { Lexer } from "./lexer";
import { TokenType } from "./token";

describe("Lexer", () => {
    it("should tokenize variables", () => {
        const source = "val x: Int = 10";
        const lexer = new Lexer(source);
        const tokens = lexer.scanTokens();

        expect(tokens.length).toBe(7); // val, x, :, Int, =, 10, EOF
        expect(tokens[0].type).toBe(TokenType.VAL);
        expect(tokens[1].type).toBe(TokenType.IDENTIFIER);
        expect(tokens[5].literal).toBe(10);
    });

    it("should tokenize functions with braces", () => {
        const source = "fun foo() { }";
        const lexer = new Lexer(source);
        const tokens = lexer.scanTokens();

        expect(tokens[0].type).toBe(TokenType.FUN);
        expect(tokens.map(t => t.type)).toContain(TokenType.LEFT_BRACE);
    });

    it("should handle operators", () => {
        const source = "+ - * / =>";
        const lexer = new Lexer(source);
        const tokens = lexer.scanTokens();

        expect(tokens[4].type).toBe(TokenType.ARROW);
    });
});
