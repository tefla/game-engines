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

  describe("break and continue", () => {
    it("break exits for loop immediately", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for i in 10:
            if i == 5:
                break
            sum = sum + i
        sum
      `));
      expect(result).toEqual(Num(10)); // 0 + 1 + 2 + 3 + 4
    });

    it("break exits list iteration", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for x in [1, 2, 3, 4, 5]:
            if x == 3:
                break
            sum = sum + x
        sum
      `));
      expect(result).toEqual(Num(3)); // 1 + 2
    });

    it("continue skips to next iteration", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for i in 5:
            if i == 2:
                continue
            sum = sum + i
        sum
      `));
      expect(result).toEqual(Num(8)); // 0 + 1 + 3 + 4 (skips 2)
    });

    it("continue skips in list iteration", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for x in [1, 2, 3, 4, 5]:
            if x % 2 == 0:
                continue
            sum = sum + x
        sum
      `));
      expect(result).toEqual(Num(9)); // 1 + 3 + 5 (skips even numbers)
    });

    it("break only affects innermost loop", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for i in 3:
            for j in 3:
                if j == 1:
                    break
                sum = sum + 1
        sum
      `));
      expect(result).toEqual(Num(3)); // Each outer iteration adds 1 (j=0), then breaks
    });

    it("continue only affects innermost loop", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for i in 2:
            for j in 3:
                if j == 1:
                    continue
                sum = sum + 1
        sum
      `));
      expect(result).toEqual(Num(4)); // 2 outer * (j=0 and j=2) = 4
    });

    it("break exits loop statement", () => {
      const result = runSlate(dedent(`
        var count = 0
        loop:
            count = count + 1
            if count == 5:
                break
        count
      `));
      expect(result).toEqual(Num(5));
    });
  });

  describe("string interpolation", () => {
    it("interpolates simple variable", () => {
      const result = runSlate(dedent(`
        let name = "World"
        "Hello, {name}!"
      `));
      expect(result).toEqual(Str("Hello, World!"));
    });

    it("interpolates expression", () => {
      const result = runSlate('"Result: {2 + 2}"');
      expect(result).toEqual(Str("Result: 4"));
    });

    it("interpolates multiple expressions", () => {
      const result = runSlate(dedent(`
        let x = 10
        let y = 20
        "{x} + {y} = {x + y}"
      `));
      expect(result).toEqual(Str("10 + 20 = 30"));
    });

    it("interpolates member access", () => {
      const result = runSlate(dedent(`
        let obj = {name: "Alice", age: 30}
        "{obj.name} is {obj.age} years old"
      `));
      expect(result).toEqual(Str("Alice is 30 years old"));
    });

    it("handles empty interpolation expression", () => {
      const result = runSlate('"{42}"');
      expect(result).toEqual(Str("42"));
    });

    it("handles string at end only", () => {
      const result = runSlate('"{42} apples"');
      expect(result).toEqual(Str("42 apples"));
    });

    it("handles string at start only", () => {
      const result = runSlate('"Count: {42}"');
      expect(result).toEqual(Str("Count: 42"));
    });

    it("handles nested record inside interpolation", () => {
      const result = runSlate(dedent(`
        let data = {inner: {value: 99}}
        "Value: {data.inner.value}"
      `));
      expect(result).toEqual(Str("Value: 99"));
    });

    it("handles function call in interpolation", () => {
      const result = runSlate(dedent(`
        fn double x:
            x * 2
        "Double of 5 is {double(5)}"
      `));
      expect(result).toEqual(Str("Double of 5 is 10"));
    });

    it("handles complex expression with braces", () => {
      // This tests that record literals work inside interpolation
      const result = runSlate(dedent(`
        let r = {x: 1}
        "Value: {r.x + 10}"
      `));
      expect(result).toEqual(Str("Value: 11"));
    });
  });

  describe("range syntax", () => {
    it("creates exclusive range (..)", () => {
      const result = runSlate("0..5");
      expect(result.type).toBe("list");
      expect((result as any).elements.map((e: any) => e.value)).toEqual([
        0, 1, 2, 3, 4,
      ]);
    });

    it("creates inclusive range (..=)", () => {
      const result = runSlate("0..=5");
      expect(result.type).toBe("list");
      expect((result as any).elements.map((e: any) => e.value)).toEqual([
        0, 1, 2, 3, 4, 5,
      ]);
    });

    it("works with variables", () => {
      const result = runSlate(dedent(`
        let start = 1
        let end = 4
        start..end
      `));
      expect((result as any).elements.map((e: any) => e.value)).toEqual([
        1, 2, 3,
      ]);
    });

    it("works with expressions", () => {
      const result = runSlate("(1 + 1)..(3 + 2)");
      expect((result as any).elements.map((e: any) => e.value)).toEqual([
        2, 3, 4,
      ]);
    });

    it("works in for loop", () => {
      const result = runSlate(dedent(`
        var sum = 0
        for i in 1..=5:
            sum = sum + i
        sum
      `));
      expect(result).toEqual(Num(15)); // 1 + 2 + 3 + 4 + 5
    });

    it("creates empty range when start equals end (exclusive)", () => {
      const result = runSlate("5..5");
      expect(result.type).toBe("list");
      expect((result as any).elements.length).toBe(0);
    });

    it("creates single element range when start equals end (inclusive)", () => {
      const result = runSlate("5..=5");
      expect(result.type).toBe("list");
      expect((result as any).elements.map((e: any) => e.value)).toEqual([5]);
    });

    it("handles negative ranges", () => {
      const result = runSlate("-3..1");
      expect((result as any).elements.map((e: any) => e.value)).toEqual([
        -3, -2, -1, 0,
      ]);
    });

    it("handles descending range when start > end", () => {
      const result = runSlate("5..2");
      expect((result as any).elements.map((e: any) => e.value)).toEqual([
        5, 4, 3,
      ]);
    });
  });

  describe("null coalescing", () => {
    it("returns left value when not null", () => {
      const result = runSlate("42 ?? 0");
      expect(result).toEqual(Num(42));
    });

    it("returns right value when left is null", () => {
      const result = runSlate(dedent(`
        fn maybe_null:
            if false:
                42
        maybe_null() ?? 100
      `));
      expect(result).toEqual(Num(100));
    });

    it("returns left value for strings", () => {
      const result = runSlate('"hello" ?? "default"');
      expect(result).toEqual(Str("hello"));
    });

    it("chains correctly: null ?? null ?? value", () => {
      const result = runSlate(dedent(`
        fn null_fn:
            if false:
                1
        null_fn() ?? null_fn() ?? 42
      `));
      expect(result).toEqual(Num(42));
    });

    it("chains correctly: value ?? null ?? null", () => {
      const result = runSlate(dedent(`
        fn null_fn:
            if false:
                1
        10 ?? null_fn() ?? 42
      `));
      expect(result).toEqual(Num(10));
    });

    it("short-circuits evaluation", () => {
      // If short-circuiting works, dividing by zero won't happen
      const result = runSlate("42 ?? (1 / 0)");
      expect(result).toEqual(Num(42));
    });

    it("works with false boolean (not null)", () => {
      const result = runSlate("false ?? true");
      expect(result).toEqual(Bool(false));
    });

    it("works with zero (not null)", () => {
      const result = runSlate("0 ?? 100");
      expect(result).toEqual(Num(0));
    });

    it("works with empty string (not null)", () => {
      const result = runSlate('"" ?? "default"');
      expect(result).toEqual(Str(""));
    });

    it("works with empty list (not null)", () => {
      const result = runSlate("[] ?? [1, 2, 3]");
      expect(result.type).toBe("list");
      expect((result as any).elements.length).toBe(0);
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

  describe("optional chaining", () => {
    it("returns value for existing property with ?.", () => {
      const result = runSlate(dedent(`
        let obj = {x: 42}
        obj?.x
      `));
      expect(result).toEqual(Num(42));
    });

    it("returns null for missing property with ?.", () => {
      const result = runSlate(dedent(`
        let obj = {x: 42}
        obj?.y
      `));
      expect(result.type).toBe("null");
    });

    it("returns null when object is null with ?.", () => {
      const result = runSlate(dedent(`
        let obj = null
        obj?.x
      `));
      expect(result.type).toBe("null");
    });

    it("chains optional member access", () => {
      const result = runSlate(dedent(`
        let obj = {nested: {value: 123}}
        obj?.nested?.value
      `));
      expect(result).toEqual(Num(123));
    });

    it("short-circuits on null in chain", () => {
      const result = runSlate(dedent(`
        let obj = {nested: null}
        obj?.nested?.value
      `));
      expect(result.type).toBe("null");
    });

    it("returns value for existing index with ?[]", () => {
      const result = runSlate(dedent(`
        let arr = [1, 2, 3]
        arr?[1]
      `));
      expect(result).toEqual(Num(2));
    });

    it("returns null for out of bounds with ?[]", () => {
      const result = runSlate(dedent(`
        let arr = [1, 2, 3]
        arr?[10]
      `));
      expect(result.type).toBe("null");
    });

    it("returns null when list is null with ?[]", () => {
      const result = runSlate(dedent(`
        let arr = null
        arr?[0]
      `));
      expect(result.type).toBe("null");
    });

    it("calls function with ?()", () => {
      // Test via stdlib which has the map function
      const result = runSlate(dedent(`
        fn add_one x:
            x + 1
        add_one?(5)
      `));
      expect(result).toEqual(Num(6));
    });

    it("returns null when function is null with ?()", () => {
      const result = runSlate(dedent(`
        let f = null
        f?()
      `));
      expect(result.type).toBe("null");
    });

    it("combines optional chaining with null coalescing", () => {
      const result = runSlate(dedent(`
        let obj = {x: null}
        obj?.x ?? "default"
      `));
      expect(result).toEqual(Str("default"));
    });

    it("mixes optional and regular access", () => {
      const result = runSlate(dedent(`
        let obj = {a: {b: {c: 42}}}
        obj?.a.b.c
      `));
      expect(result).toEqual(Num(42));
    });
  });

  describe("lambda expressions", () => {
    it("evaluates simple lambda with one parameter", () => {
      const result = runSlate(dedent(`
        let double = |x| x * 2
        double(5)
      `));
      expect(result).toEqual(Num(10));
    });

    it("evaluates lambda with multiple parameters", () => {
      const result = runSlate(dedent(`
        let add = |a, b| a + b
        add(3, 4)
      `));
      expect(result).toEqual(Num(7));
    });

    it("evaluates lambda with no parameters", () => {
      const result = runSlate(dedent(`
        let greet = || "hello"
        greet()
      `));
      expect(result).toEqual(Str("hello"));
    });

    it("captures variables from enclosing scope", () => {
      const result = runSlate(dedent(`
        let x = 10
        let add_x = |y| x + y
        add_x(5)
      `));
      expect(result).toEqual(Num(15));
    });

    it("can be passed as argument to higher-order function", () => {
      const result = runSlate(dedent(`
        fn apply f x:
            f(x)
        apply(|n| n * 3, 7)
      `));
      expect(result).toEqual(Num(21));
    });

    it("can be immediately invoked", () => {
      const result = runSlate("(|x| x + 1)(10)");
      expect(result).toEqual(Num(11));
    });

    it("works with map-like function", () => {
      // Define map in Slate since stdlib native functions can't call lambdas
      const result = runSlate(dedent(`
        fn map_list lst f:
          var result = []
          for x in lst:
            result = push(result, f(x))
          result
        let nums = [1, 2, 3]
        map_list(nums, |x| x * 2)
      `));
      expect(result.type).toBe("list");
      const elements = (result as any).elements;
      expect(elements[0]).toEqual(Num(2));
      expect(elements[1]).toEqual(Num(4));
      expect(elements[2]).toEqual(Num(6));
    });

    it("works with filter-like function", () => {
      // Define filter in Slate since stdlib native functions can't call lambdas
      const result = runSlate(dedent(`
        fn filter_list lst pred:
          var result = []
          for x in lst:
            if pred(x):
              result = push(result, x)
          result
        let nums = [1, 2, 3, 4, 5]
        filter_list(nums, |x| x > 2)
      `));
      expect(result.type).toBe("list");
      const elements = (result as any).elements;
      expect(elements.length).toBe(3);
      expect(elements[0]).toEqual(Num(3));
    });

    it("can return another lambda", () => {
      const result = runSlate(dedent(`
        let make_adder = |n| |x| x + n
        let add5 = make_adder(5)
        add5(10)
      `));
      expect(result).toEqual(Num(15));
    });

    it("works with string expressions", () => {
      const result = runSlate(dedent(`
        let greet = |name| "Hello, " + name + "!"
        greet("World")
      `));
      expect(result).toEqual(Str("Hello, World!"));
    });

    it("supports complex expressions in body", () => {
      const result = runSlate(dedent(`
        let compute = |a, b, c| (a + b) * c
        compute(2, 3, 4)
      `));
      expect(result).toEqual(Num(20));
    });
  });
});
