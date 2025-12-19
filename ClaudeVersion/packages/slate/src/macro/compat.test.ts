/**
 * Compatibility Tests
 *
 * Run existing Slate tests against the macro-based interpreter
 * to verify feature parity.
 */

import { describe, it, expect } from "vitest";
import { createSlateRuntime } from "./index";

// Helper to run and get raw value
async function run(source: string) {
  const { run } = createSlateRuntime();
  return run(source);
}

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

describe("Macro System - Compatibility Tests", () => {
  describe("literals", () => {
    it("evaluates number literals", async () => {
      expect(await run("42")).toBe(42);
    });

    it("evaluates string literals", async () => {
      expect(await run('"hello"')).toBe("hello");
    });

    it("evaluates boolean literals", async () => {
      expect(await run("true")).toBe(true);
      expect(await run("false")).toBe(false);
    });

    it("evaluates list literals", async () => {
      const result = await run("[1, 2, 3]");
      expect(Array.isArray(result)).toBe(true);
      expect(result).toEqual([1, 2, 3]);
    });

    it("evaluates record literals", async () => {
      const result = await run("{x: 1, y: 2}") as any;
      expect(result.type).toBe("record");
      expect(result.fields.get("x")).toBe(1);
    });
  });

  describe("arithmetic", () => {
    it("evaluates addition", async () => {
      expect(await run("1 + 2")).toBe(3);
    });

    it("evaluates subtraction", async () => {
      expect(await run("10 - 4")).toBe(6);
    });

    it("evaluates multiplication", async () => {
      expect(await run("3 * 4")).toBe(12);
    });

    it("evaluates division", async () => {
      expect(await run("10 / 2")).toBe(5);
    });

    it("evaluates modulo", async () => {
      expect(await run("10 % 3")).toBe(1);
    });

    it("respects operator precedence", async () => {
      expect(await run("2 + 3 * 4")).toBe(14);
      expect(await run("(2 + 3) * 4")).toBe(20);
    });

    it("evaluates unary minus", async () => {
      expect(await run("-5")).toBe(-5);
    });
  });

  describe("comparison", () => {
    it("evaluates less than", async () => {
      expect(await run("1 < 2")).toBe(true);
      expect(await run("2 < 1")).toBe(false);
    });

    it("evaluates greater than", async () => {
      expect(await run("2 > 1")).toBe(true);
      expect(await run("1 > 2")).toBe(false);
    });

    it("evaluates equality", async () => {
      expect(await run("1 == 1")).toBe(true);
      expect(await run("1 == 2")).toBe(false);
      expect(await run('"a" == "a"')).toBe(true);
    });

    it("evaluates inequality", async () => {
      expect(await run("1 != 2")).toBe(true);
      expect(await run("1 != 1")).toBe(false);
    });
  });

  describe("logical operators", () => {
    it("evaluates and", async () => {
      expect(await run("true and true")).toBe(true);
      expect(await run("true and false")).toBe(false);
    });

    it("evaluates or", async () => {
      expect(await run("false or true")).toBe(true);
      expect(await run("false or false")).toBe(false);
    });

    it("evaluates not", async () => {
      expect(await run("not true")).toBe(false);
      expect(await run("not false")).toBe(true);
    });

    it("short-circuits and", async () => {
      const result = await run("false and (1 / 0)");
      expect(result).toBe(false);
    });

    it("short-circuits or", async () => {
      const result = await run("true or (1 / 0)");
      expect(result).toBe(true);
    });
  });

  describe("string operations", () => {
    it("concatenates strings", async () => {
      expect(await run('"hello" + " world"')).toBe("hello world");
    });

    it("concatenates string with number", async () => {
      expect(await run('"value: " + 42')).toBe("value: 42");
    });
  });

  describe("variables", () => {
    it("binds let variables", async () => {
      const result = await run(dedent(`
        let x = 10
        x + 5
      `));
      expect(result).toBe(15);
    });

    it("allows var reassignment", async () => {
      const result = await run(dedent(`
        var x = 10
        x = 20
        x
      `));
      expect(result).toBe(20);
    });

    it("throws on let reassignment", async () => {
      await expect(run(dedent(`
        let x = 10
        x = 20
      `))).rejects.toThrow();
    });
  });

  describe("functions", () => {
    it("defines and calls functions", async () => {
      const result = await run(dedent(`
        fn add a b:
            a + b
        add(3, 4)
      `));
      expect(result).toBe(7);
    });

    it("supports recursion", async () => {
      const result = await run(dedent(`
        fn factorial n:
            if n <= 1:
                1
            else:
                n * factorial(n - 1)
        factorial(5)
      `));
      expect(result).toBe(120);
    });

    it("creates closures", async () => {
      const result = await run(dedent(`
        fn makeCounter:
            var count = 0
            fn increment:
                count = count + 1
                count
            increment
        let counter = makeCounter()
        counter()
        counter()
        counter()
      `));
      expect(result).toBe(3);
    });
  });

  describe("if expressions", () => {
    it("evaluates if-then", async () => {
      const result = await run(dedent(`
        if true:
            "yes"
        else:
            "no"
      `));
      expect(result).toBe("yes");
    });

    it("evaluates if-else", async () => {
      const result = await run(dedent(`
        if false:
            "yes"
        else:
            "no"
      `));
      expect(result).toBe("no");
    });

    it("returns last expression from block", async () => {
      const result = await run(dedent(`
        if true:
            let x = 1
            let y = 2
            x + y
      `));
      expect(result).toBe(3);
    });
  });

  describe("member access", () => {
    it("accesses record fields", async () => {
      const result = await run(dedent(`
        let point = {x: 10, y: 20}
        point.x + point.y
      `));
      expect(result).toBe(30);
    });

    it("accesses nested fields", async () => {
      const result = await run(dedent(`
        let obj = {inner: {value: 42}}
        obj.inner.value
      `));
      expect(result).toBe(42);
    });
  });

  describe("index access", () => {
    it("accesses list elements", async () => {
      const result = await run(dedent(`
        let items = [10, 20, 30]
        items[1]
      `));
      expect(result).toBe(20);
    });
  });

  describe("stdlib", () => {
    it("has abs function", async () => {
      expect(await run("abs(-5)")).toBe(5);
    });

    it("has min/max functions", async () => {
      expect(await run("min(3, 7)")).toBe(3);
      expect(await run("max(3, 7)")).toBe(7);
    });

    it("has len function", async () => {
      expect(await run('len([1, 2, 3])')).toBe(3);
      expect(await run('len("hello")')).toBe(5);
    });
  });

  describe("complex programs", () => {
    it("runs the door puzzle example", async () => {
      const result = await run(dedent(`
        let player = {has_key: true}
        fn can_open_door:
            if player.has_key:
                true
            else:
                false
        can_open_door()
      `));
      expect(result).toBe(true);
    });

    it("runs fibonacci", async () => {
      const result = await run(dedent(`
        fn fib n:
            if n <= 1:
                n
            else:
                fib(n - 1) + fib(n - 2)
        fib(10)
      `));
      expect(result).toBe(55);
    });
  });
});
