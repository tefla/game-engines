/**
 * Tests for the self-hosted Slate runtime
 * These tests mirror the interpreter.test.ts tests to ensure feature parity
 */

import { describe, it, expect, beforeAll } from "bun:test";
import { createSlateRuntime, SlateRuntime } from "./index";

// Helper to dedent template strings
function dedent(str: string): string {
  const lines = str.split("\n");
  if (lines[0].trim() === "") lines.shift();
  if (lines[lines.length - 1].trim() === "") lines.pop();

  const minIndent = lines
    .filter((l) => l.trim().length > 0)
    .reduce((min, line) => {
      const indent = line.match(/^\s*/)?.[0].length ?? 0;
      return Math.min(min, indent);
    }, Infinity);

  return lines.map((l) => l.slice(minIndent)).join("\n");
}

describe("Self-hosted Slate Runtime", () => {
  let runtime: SlateRuntime;

  beforeAll(() => {
    runtime = createSlateRuntime();
  });

  describe("literals", () => {
    it("evaluates number literals", () => {
      const result = runtime.run("42");
      expect(result).toBe(42);
    });

    it("evaluates decimal number literals", () => {
      const result = runtime.run("3.14");
      expect(result).toBe(3.14);
    });

    it("evaluates string literals", () => {
      const result = runtime.run('"hello"');
      expect(result).toBe("hello");
    });

    it("evaluates boolean literals", () => {
      expect(runtime.run("true")).toBe(true);
      expect(runtime.run("false")).toBe(false);
    });

    it("evaluates null literal", () => {
      expect(runtime.run("null")).toBe(null);
    });

    it("evaluates list literals", () => {
      const result = runtime.run("[1, 2, 3]");
      expect(result).toEqual([1, 2, 3]);
    });

    it("evaluates empty list", () => {
      const result = runtime.run("[]");
      expect(result).toEqual([]);
    });

    it("evaluates record literals", () => {
      const result = runtime.run("{x: 1, y: 2}");
      expect(result).toEqual({ x: 1, y: 2 });
    });

    it("evaluates empty record", () => {
      const result = runtime.run("{}");
      expect(result).toEqual({});
    });
  });

  describe("arithmetic", () => {
    it("evaluates addition", () => {
      expect(runtime.run("1 + 2")).toBe(3);
    });

    it("evaluates subtraction", () => {
      expect(runtime.run("10 - 4")).toBe(6);
    });

    it("evaluates multiplication", () => {
      expect(runtime.run("3 * 4")).toBe(12);
    });

    it("evaluates division", () => {
      expect(runtime.run("10 / 2")).toBe(5);
    });

    it("evaluates modulo", () => {
      expect(runtime.run("10 % 3")).toBe(1);
    });

    it("respects operator precedence", () => {
      expect(runtime.run("2 + 3 * 4")).toBe(14);
      expect(runtime.run("(2 + 3) * 4")).toBe(20);
    });

    it("evaluates unary minus", () => {
      expect(runtime.run("-5")).toBe(-5);
    });

    it("evaluates power", () => {
      expect(runtime.run("2 ^ 3")).toBe(8);
    });
  });

  describe("comparison", () => {
    it("evaluates less than", () => {
      expect(runtime.run("1 < 2")).toBe(true);
      expect(runtime.run("2 < 1")).toBe(false);
    });

    it("evaluates less than or equal", () => {
      expect(runtime.run("1 <= 2")).toBe(true);
      expect(runtime.run("2 <= 2")).toBe(true);
      expect(runtime.run("3 <= 2")).toBe(false);
    });

    it("evaluates greater than", () => {
      expect(runtime.run("2 > 1")).toBe(true);
      expect(runtime.run("1 > 2")).toBe(false);
    });

    it("evaluates greater than or equal", () => {
      expect(runtime.run("2 >= 1")).toBe(true);
      expect(runtime.run("2 >= 2")).toBe(true);
      expect(runtime.run("1 >= 2")).toBe(false);
    });

    it("evaluates equality", () => {
      expect(runtime.run("1 == 1")).toBe(true);
      expect(runtime.run("1 == 2")).toBe(false);
      expect(runtime.run('"a" == "a"')).toBe(true);
    });

    it("evaluates inequality", () => {
      expect(runtime.run("1 != 2")).toBe(true);
      expect(runtime.run("1 != 1")).toBe(false);
    });
  });

  describe("logical operators", () => {
    it("evaluates and", () => {
      expect(runtime.run("true and true")).toBe(true);
      expect(runtime.run("true and false")).toBe(false);
      expect(runtime.run("false and true")).toBe(false);
    });

    it("evaluates or", () => {
      expect(runtime.run("false or true")).toBe(true);
      expect(runtime.run("false or false")).toBe(false);
      expect(runtime.run("true or false")).toBe(true);
    });

    it("evaluates not", () => {
      expect(runtime.run("not true")).toBe(false);
      expect(runtime.run("not false")).toBe(true);
    });

    it("short-circuits and", () => {
      // Should return false without evaluating second operand
      expect(runtime.run("false and true")).toBe(false);
    });

    it("short-circuits or", () => {
      // Should return true without evaluating second operand
      expect(runtime.run("true or false")).toBe(true);
    });
  });

  describe("variables", () => {
    it("binds let variables", () => {
      const result = runtime.run(dedent(`
        let x = 10
        x + 5
      `));
      expect(result).toBe(15);
    });

    it("binds multiple variables", () => {
      const result = runtime.run(dedent(`
        let x = 10
        let y = 20
        x + y
      `));
      expect(result).toBe(30);
    });

    it("shadows outer variables", () => {
      const result = runtime.run(dedent(`
        let x = 10
        let x = 20
        x
      `));
      expect(result).toBe(20);
    });
  });

  describe("functions", () => {
    it("defines and calls functions", () => {
      const result = runtime.run(dedent(`
        fn add a b:
            a + b
        add(2, 3)
      `));
      expect(result).toBe(5);
    });

    it("returns last expression", () => {
      const result = runtime.run(dedent(`
        fn test:
            1
            2
            3
        test()
      `));
      expect(result).toBe(3);
    });

    it("handles recursive functions", () => {
      const result = runtime.run(dedent(`
        fn factorial n:
            if n <= 1:
                1
            else:
                n * factorial(n - 1)
        factorial(5)
      `));
      expect(result).toBe(120);
    });

    it("captures closures", () => {
      const result = runtime.run(dedent(`
        fn makeAdder x:
            fn add y:
                x + y
            add
        let add5 = makeAdder(5)
        add5(3)
      `));
      expect(result).toBe(8);
    });
  });

  describe("conditionals", () => {
    it("evaluates if-then", () => {
      const result = runtime.run(dedent(`
        if true:
            42
      `));
      expect(result).toBe(42);
    });

    it("evaluates if-else (true branch)", () => {
      const result = runtime.run(dedent(`
        if true:
            1
        else:
            2
      `));
      expect(result).toBe(1);
    });

    it("evaluates if-else (false branch)", () => {
      const result = runtime.run(dedent(`
        if false:
            1
        else:
            2
      `));
      expect(result).toBe(2);
    });

    it("evaluates else-if chains", () => {
      const result = runtime.run(dedent(`
        let x = 2
        if x == 1:
            10
        else if x == 2:
            20
        else:
            30
      `));
      expect(result).toBe(20);
    });
  });

  describe("loops", () => {
    it("evaluates while loops", () => {
      const result = runtime.run(dedent(`
        let sum = 0
        let i = 1
        while i <= 5:
            sum = sum + i
            i = i + 1
        sum
      `));
      expect(result).toBe(15);
    });

    it("evaluates for loops", () => {
      const result = runtime.run(dedent(`
        let sum = 0
        for x in [1, 2, 3, 4, 5]:
            sum = sum + x
        sum
      `));
      expect(result).toBe(15);
    });

    it("handles break in while", () => {
      const result = runtime.run(dedent(`
        let sum = 0
        let i = 1
        while true:
            sum = sum + i
            i = i + 1
            if i > 5:
                break
        sum
      `));
      expect(result).toBe(15);
    });

    it("handles continue in for", () => {
      const result = runtime.run(dedent(`
        let sum = 0
        for x in [1, 2, 3, 4, 5]:
            if x == 3:
                continue
            sum = sum + x
        sum
      `));
      expect(result).toBe(12); // 1 + 2 + 4 + 5
    });
  });

  describe("member access", () => {
    it("accesses record fields", () => {
      const result = runtime.run(dedent(`
        let obj = {x: 10, y: 20}
        obj.x + obj.y
      `));
      expect(result).toBe(30);
    });

    it("accesses list length", () => {
      const result = runtime.run(dedent(`
        let items = [1, 2, 3, 4, 5]
        items.length
      `));
      expect(result).toBe(5);
    });
  });

  describe("index access", () => {
    it("indexes lists", () => {
      const result = runtime.run(dedent(`
        let items = [10, 20, 30]
        items[1]
      `));
      expect(result).toBe(20);
    });

    it("indexes records with strings", () => {
      const result = runtime.run(dedent(`
        let obj = {name: "test"}
        obj["name"]
      `));
      expect(result).toBe("test");
    });
  });

  describe("null coalescing", () => {
    it("returns left when not null", () => {
      const result = runtime.run("42 ?? 0");
      expect(result).toBe(42);
    });

    it("returns right when left is null", () => {
      const result = runtime.run("null ?? 99");
      expect(result).toBe(99);
    });
  });

  describe("ranges", () => {
    it("creates exclusive range", () => {
      const result = runtime.run("1..5");
      expect(result).toEqual([1, 2, 3, 4]);
    });

    it("creates inclusive range", () => {
      const result = runtime.run("1..=5");
      expect(result).toEqual([1, 2, 3, 4, 5]);
    });
  });

  describe("lambda expressions", () => {
    it("creates and calls lambda", () => {
      const result = runtime.run(dedent(`
        let double = |x| x * 2
        double(5)
      `));
      expect(result).toBe(10);
    });

    it("creates multi-param lambda", () => {
      const result = runtime.run(dedent(`
        let add = |a, b| a + b
        add(3, 4)
      `));
      expect(result).toBe(7);
    });
  });

  describe("return statement", () => {
    it("returns from function", () => {
      const result = runtime.run(dedent(`
        fn test:
            return 42
            100
        test()
      `));
      expect(result).toBe(42);
    });

    it("returns from nested block", () => {
      const result = runtime.run(dedent(`
        fn test x:
            if x > 0:
                return x
            0
        test(5)
      `));
      expect(result).toBe(5);
    });
  });
});
