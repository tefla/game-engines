import { describe, it, expect } from "bun:test";
import { Lexer } from "./lexer";
import { Parser } from "./parser";
import * as Stmt from "./ast";
import * as Expr from "./ast";

describe("Parser", () => {
    it("should parse variable declarations", () => {
        const source = "val x: Int = 10";
        const tokens = new Lexer(source).scanTokens();
        const parser = new Parser(tokens);
        const stmts = parser.parse();

        expect(stmts.length).toBe(1);
        const stmt = stmts[0] as Stmt.Var;
        expect(stmt).toBeInstanceOf(Stmt.Var);
        expect(stmt.name.lexeme).toBe("x");
        expect(stmt.typeName).toBe("Int");
    });

    it("should parse trailing lambdas in calls", () => {
        const source = `
System "Move"
    print "hi"
`;
        const tokens = new Lexer(source).scanTokens();
        const stmts = new Parser(tokens).parse();

        const exprStmt = stmts[0] as Stmt.Expression;
        const call = exprStmt.expression as Expr.Call;

        expect(call.args.length).toBe(2); // "Move", Lambda
        expect(call.args[1]).toBeInstanceOf(Expr.Lambda);
    });

    it("should parse operator overloading usage", () => {
        const source = "val res = vec + vec2";
        const stmts = new Parser(new Lexer(source).scanTokens()).parse();

        const stmt = stmts[0] as Stmt.Var;
        const bin = stmt.initializer as Expr.Binary;
        expect(bin).toBeInstanceOf(Expr.Binary);
    });
});
