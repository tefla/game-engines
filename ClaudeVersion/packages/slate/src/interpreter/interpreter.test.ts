import { describe, it, expect } from "bun:test";
import { runSlate, Num, Str, Bool, List, Record, stringify } from "../index";

// Helper to dedent template strings
function dedent(str: string): string {
  const lines = str.split("\n");
  // Remove first line if empty
  if (lines[0].trim() === "") lines.shift();
  // Remove last line if empty
  if (lines[lines.length - 1].trim() === "") lines.pop();

  // Find minimum indentation
  const minIndent = lines
    .filter((l) => l.trim().length > 0)
    .reduce((min, line) => {
      const indent = line.match(/^\s*/)?.[0].length ?? 0;
      return Math.min(min, indent);
    }, Infinity);

  // Remove that indentation from all lines
  return lines.map((l) => l.slice(minIndent)).join("\n");
}

describe("Interpreter", () => {
  describe("literals", () => {
    it("evaluates number literals", () => {
      const result = runSlate("42");
      expect(result).toEqual(Num(42));
    });

    it("evaluates string literals", () => {
      const result = runSlate('"hello"');
      expect(result).toEqual(Str("hello"));
    });

    it("evaluates boolean literals", () => {
      expect(runSlate("true")).toEqual(Bool(true));
      expect(runSlate("false")).toEqual(Bool(false));
    });

    it("evaluates list literals", () => {
      const result = runSlate("[1, 2, 3]");
      expect(result.type).toBe("list");
      expect((result as any).elements.length).toBe(3);
    });

    it("evaluates record literals", () => {
      const result = runSlate("{x: 1, y: 2}");
      expect(result.type).toBe("record");
      expect((result as any).fields.get("x")).toEqual(Num(1));
    });
  });

  describe("arithmetic", () => {
    it("evaluates addition", () => {
      expect(runSlate("1 + 2")).toEqual(Num(3));
    });

    it("evaluates subtraction", () => {
      expect(runSlate("10 - 4")).toEqual(Num(6));
    });

    it("evaluates multiplication", () => {
      expect(runSlate("3 * 4")).toEqual(Num(12));
    });

    it("evaluates division", () => {
      expect(runSlate("10 / 2")).toEqual(Num(5));
    });

    it("evaluates modulo", () => {
      expect(runSlate("10 % 3")).toEqual(Num(1));
    });

    it("respects operator precedence", () => {
      expect(runSlate("2 + 3 * 4")).toEqual(Num(14));
      expect(runSlate("(2 + 3) * 4")).toEqual(Num(20));
    });

    it("evaluates unary minus", () => {
      expect(runSlate("-5")).toEqual(Num(-5));
    });
  });

  describe("comparison", () => {
    it("evaluates less than", () => {
      expect(runSlate("1 < 2")).toEqual(Bool(true));
      expect(runSlate("2 < 1")).toEqual(Bool(false));
    });

    it("evaluates greater than", () => {
      expect(runSlate("2 > 1")).toEqual(Bool(true));
      expect(runSlate("1 > 2")).toEqual(Bool(false));
    });

    it("evaluates equality", () => {
      expect(runSlate("1 == 1")).toEqual(Bool(true));
      expect(runSlate("1 == 2")).toEqual(Bool(false));
      expect(runSlate('"a" == "a"')).toEqual(Bool(true));
    });

    it("evaluates inequality", () => {
      expect(runSlate("1 != 2")).toEqual(Bool(true));
      expect(runSlate("1 != 1")).toEqual(Bool(false));
    });
  });

  describe("logical operators", () => {
    it("evaluates and", () => {
      expect(runSlate("true and true")).toEqual(Bool(true));
      expect(runSlate("true and false")).toEqual(Bool(false));
    });

    it("evaluates or", () => {
      expect(runSlate("false or true")).toEqual(Bool(true));
      expect(runSlate("false or false")).toEqual(Bool(false));
    });

    it("evaluates not", () => {
      expect(runSlate("not true")).toEqual(Bool(false));
      expect(runSlate("not false")).toEqual(Bool(true));
    });

    it("short-circuits and", () => {
      // If and short-circuits, it won't try to divide by zero
      const result = runSlate("false and (1 / 0)");
      expect(result).toEqual(Bool(false));
    });

    it("short-circuits or", () => {
      const result = runSlate("true or (1 / 0)");
      expect(result).toEqual(Bool(true));
    });
  });

  describe("string operations", () => {
    it("concatenates strings", () => {
      expect(runSlate('"hello" + " world"')).toEqual(Str("hello world"));
    });

    it("concatenates string with number", () => {
      expect(runSlate('"value: " + 42')).toEqual(Str("value: 42"));
    });
  });

  describe("variables", () => {
    it("binds let variables", () => {
      const result = runSlate(dedent(`
        let x = 10
        x + 5
      `));
      expect(result).toEqual(Num(15));
    });

    it("allows var reassignment", () => {
      const result = runSlate(dedent(`
        var x = 10
        x = 20
        x
      `));
      expect(result).toEqual(Num(20));
    });

    it("throws on let reassignment", () => {
      expect(() =>
        runSlate(dedent(`
          let x = 10
          x = 20
        `))
      ).toThrow();
    });
  });

  describe("functions", () => {
    it("defines and calls functions", () => {
      const result = runSlate(dedent(`
        fn add a b:
            a + b
        add(3, 4)
      `));
      expect(result).toEqual(Num(7));
    });

    it("supports recursion", () => {
      const result = runSlate(dedent(`
        fn factorial n:
            if n <= 1:
                1
            else:
                n * factorial(n - 1)
        factorial(5)
      `));
      expect(result).toEqual(Num(120));
    });

    it("creates closures", () => {
      const result = runSlate(dedent(`
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
      expect(result).toEqual(Num(3));
    });
  });

  describe("if expressions", () => {
    it("evaluates if-then", () => {
      const result = runSlate(dedent(`
        if true:
            "yes"
        else:
            "no"
      `));
      expect(result).toEqual(Str("yes"));
    });

    it("evaluates if-else", () => {
      const result = runSlate(dedent(`
        if false:
            "yes"
        else:
            "no"
      `));
      expect(result).toEqual(Str("no"));
    });

    it("returns last expression from block", () => {
      const result = runSlate(dedent(`
        if true:
            let x = 1
            let y = 2
            x + y
      `));
      expect(result).toEqual(Num(3));
    });
  });

  describe("match expressions", () => {
    it("matches literal patterns", () => {
      const result = runSlate(dedent(`
        match 2:
            1 => "one"
            2 => "two"
            _ => "other"
      `));
      expect(result).toEqual(Str("two"));
    });

    it("matches wildcard pattern", () => {
      const result = runSlate(dedent(`
        match 99:
            1 => "one"
            _ => "other"
      `));
      expect(result).toEqual(Str("other"));
    });

    it("matches identifier patterns and binds", () => {
      const result = runSlate(dedent(`
        match 42:
            x => x + 1
      `));
      expect(result).toEqual(Num(43));
    });

    it("matches record patterns", () => {
      const result = runSlate(dedent(`
        let item = {type: "key", color: "gold"}
        match item:
            {type, color} => color
            _ => "unknown"
      `));
      expect(result).toEqual(Str("gold"));
    });

    it("matches list patterns", () => {
      const result = runSlate(dedent(`
        match [1, 2, 3]:
            [a, b, c] => a + b + c
            _ => 0
      `));
      expect(result).toEqual(Num(6));
    });
  });

  describe("member access", () => {
    it("accesses record fields", () => {
      const result = runSlate(dedent(`
        let point = {x: 10, y: 20}
        point.x + point.y
      `));
      expect(result).toEqual(Num(30));
    });

    it("accesses nested fields", () => {
      const result = runSlate(dedent(`
        let obj = {inner: {value: 42}}
        obj.inner.value
      `));
      expect(result).toEqual(Num(42));
    });
  });

  describe("index access", () => {
    it("accesses list elements", () => {
      const result = runSlate(dedent(`
        let items = [10, 20, 30]
        items[1]
      `));
      expect(result).toEqual(Num(20));
    });

    it("accesses record with string index", () => {
      const result = runSlate(dedent(`
        let obj = {name: "test"}
        obj["name"]
      `));
      expect(result).toEqual(Str("test"));
    });
  });

  describe("for loops", () => {
    it("iterates over lists", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for x in [1, 2, 3]:
            sum = sum + x
        sum
      `));
      expect(result).toEqual(Num(6));
    });

    it("iterates over range (number)", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for i in 5:
            sum = sum + i
        sum
      `));
      expect(result).toEqual(Num(10)); // 0 + 1 + 2 + 3 + 4
    });
  });

  describe("stdlib", () => {
    it("has abs function", () => {
      expect(runSlate("abs(-5)")).toEqual(Num(5));
    });

    it("has min/max functions", () => {
      expect(runSlate("min(3, 7)")).toEqual(Num(3));
      expect(runSlate("max(3, 7)")).toEqual(Num(7));
    });

    it("has clamp function", () => {
      expect(runSlate("clamp(5, 0, 10)")).toEqual(Num(5));
      expect(runSlate("clamp(-5, 0, 10)")).toEqual(Num(0));
      expect(runSlate("clamp(15, 0, 10)")).toEqual(Num(10));
    });

    it("has lerp function", () => {
      expect(runSlate("lerp(0, 100, 0.5)")).toEqual(Num(50));
    });

    it("has length function", () => {
      expect(runSlate('length([1, 2, 3])')).toEqual(Num(3));
      expect(runSlate('length("hello")')).toEqual(Num(5));
    });

    it("has push function", () => {
      const result = runSlate("push([1, 2], 3)");
      expect(result.type).toBe("list");
      expect((result as any).elements.length).toBe(3);
    });

    it("has first/last functions", () => {
      expect(runSlate("first([1, 2, 3])")).toEqual(Num(1));
      expect(runSlate("last([1, 2, 3])")).toEqual(Num(3));
    });

    it("has contains function", () => {
      expect(runSlate("contains([1, 2, 3], 2)")).toEqual(Bool(true));
      expect(runSlate("contains([1, 2, 3], 5)")).toEqual(Bool(false));
    });

    it("has string functions", () => {
      expect(runSlate('upper("hello")')).toEqual(Str("HELLO"));
      expect(runSlate('lower("HELLO")')).toEqual(Str("hello"));
      expect(runSlate('trim("  hi  ")')).toEqual(Str("hi"));
    });

    it("has vec3 function", () => {
      const result = runSlate("vec3(1, 2, 3)");
      expect(result.type).toBe("record");
      expect((result as any).fields.get("x")).toEqual(Num(1));
      expect((result as any).fields.get("y")).toEqual(Num(2));
      expect((result as any).fields.get("z")).toEqual(Num(3));
    });

    it("has typeof function", () => {
      expect(runSlate('typeof(42)')).toEqual(Str("number"));
      expect(runSlate('typeof("hi")')).toEqual(Str("string"));
      expect(runSlate('typeof([1,2])')).toEqual(Str("list"));
    });
  });

  describe("signals", () => {
    it("registers signal handlers", () => {
      // This tests that on statements don't throw
      const result = runSlate(dedent(`
        var received = false
        on @test.signal:
            received = true
        received
      `));
      expect(result).toEqual(Bool(false)); // Not emitted yet
    });
  });

  describe("complex programs", () => {
    it("runs the door puzzle example", () => {
      const result = runSlate(dedent(`
        let player = {has_key: true}
        fn can_open_door:
            if player.has_key:
                true
            else:
                false
        can_open_door()
      `));
      expect(result).toEqual(Bool(true));
    });

    it("runs fibonacci", () => {
      const result = runSlate(dedent(`
        fn fib n:
            if n <= 1:
                n
            else:
                fib(n - 1) + fib(n - 2)
        fib(10)
      `));
      expect(result).toEqual(Num(55));
    });
  });
});
