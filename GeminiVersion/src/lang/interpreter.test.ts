import { describe, it, expect } from "bun:test";
import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { Interpreter } from "./interpreter";
import { Vector3 } from "../engine/vector";

function run(source: string) {
    const tokens = new Lexer(source).scanTokens();
    const stmts = new Parser(tokens).parse();
    const interpreter = new Interpreter();
    // Setup Native
    interpreter.globals.define("Vec3", {
        call: (_: any, args: any[]) => new Vector3(args[0], args[1], args[2])
    });

    interpreter.globals.define("res", null); // Output slot
    // Mock Game
    interpreter.globals.define("Game", {
        add: () => { },
        clear: () => { }
    });
    interpreter.globals.define("Input", {
        isKeyDown: () => false
    });
    interpreter.interpret(stmts);
    return interpreter;
}

describe("Interpreter", () => {
    it("should execute math", () => {
        const interpreter = run("val x = 10 + 20");
        expect(interpreter.globals.values["x"]).toBe(30);
    });

    it("should support operator overloading on native objects", () => {
        const source = `
val v1 = Vec3(1, 0, 0)
val v2 = Vec3(2, 0, 0)
val v3 = v1 + v2
`;
        const interpreter = run(source);
        const v3 = interpreter.globals.values["v3"] as Vector3;

        expect(v3).toBeInstanceOf(Vector3);
        expect(v3.x).toBe(3);
    });

    it("should handle closures", () => {
        const source = `
            var a = "global"
            var a = "global"
            // Anonymous block (if allowed by parser) or just if true
            if true
                fun show()
                    print(a)
                
                show()
                var a = "block"
                show()
        `;
        // Hard to test output without mocking print, but verifying it runs is good step 1
        expect(() => run(source)).not.toThrow();
    });
});
