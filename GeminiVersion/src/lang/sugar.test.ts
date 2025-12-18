import { describe, it, expect } from "bun:test";
import { Lexer } from "./lexer";
import { TokenType } from "./token";

describe("Indentation Syntax", () => {
    it("should lex indent/dedent", () => {
        const source = `
if true
    print "hi"
print "end"
`;
        const lexer = new Lexer(source);
        const tokens = lexer.scanTokens();

        // Filter out newlines for easier checking
        const meaningful = tokens.filter(t => t.type !== TokenType.NEWLINE);

        // if, true, INDENT, print, "hi", DEDENT, print, "end", EOF
        let i = 0;
        expect(meaningful[i++].type).toBe(TokenType.IF);
        expect(meaningful[i++].type).toBe(TokenType.TRUE);
        expect(meaningful[i++].type).toBe(TokenType.INDENT);
        expect(meaningful[i++].type).toBe(TokenType.IDENTIFIER); // print
        expect(meaningful[i++].type).toBe(TokenType.STRING);
        expect(meaningful[i++].type).toBe(TokenType.DEDENT);
        expect(meaningful[i++].type).toBe(TokenType.IDENTIFIER); // print
    });
});
