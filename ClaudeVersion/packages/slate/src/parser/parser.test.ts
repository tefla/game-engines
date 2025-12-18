import { describe, it, expect } from "bun:test";
import { Lexer } from "../lexer";
import { Parser } from "./parser";

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

function parse(source: string) {
  const lexer = new Lexer(source);
  const tokens = lexer.tokenize();
  const parser = new Parser(tokens);
  return parser.parse();
}

describe("Parser", () => {
  describe("let statements", () => {
    it("parses let with number literal", () => {
      const ast = parse("let x = 42");

      expect(ast.statements.length).toBe(1);
      expect(ast.statements[0].type).toBe("Let");
      const letStmt = ast.statements[0] as any;
      expect(letStmt.name).toBe("x");
      expect(letStmt.value.type).toBe("Literal");
      expect(letStmt.value.value).toBe(42);
    });

    it("parses let with string literal", () => {
      const ast = parse('let name = "Alice"');

      const letStmt = ast.statements[0] as any;
      expect(letStmt.name).toBe("name");
      expect(letStmt.value.value).toBe("Alice");
    });

    it("parses let with type annotation", () => {
      const ast = parse("let x: Number = 42");

      const letStmt = ast.statements[0] as any;
      expect(letStmt.typeAnnotation).toBeDefined();
      expect(letStmt.typeAnnotation.name).toBe("Number");
    });
  });

  describe("var statements", () => {
    it("parses var statement", () => {
      const ast = parse("var count = 0");

      expect(ast.statements[0].type).toBe("Var");
      const varStmt = ast.statements[0] as any;
      expect(varStmt.name).toBe("count");
      expect(varStmt.value.value).toBe(0);
    });
  });

  describe("function statements", () => {
    it("parses function with no parameters", () => {
      const ast = parse(dedent(`
        fn greet:
            "hello"
      `));

      const fnStmt = ast.statements[0] as any;
      expect(fnStmt.type).toBe("Fn");
      expect(fnStmt.name).toBe("greet");
      expect(fnStmt.params.length).toBe(0);
    });

    it("parses function with parameters", () => {
      const ast = parse(dedent(`
        fn add a b:
            a + b
      `));

      const fnStmt = ast.statements[0] as any;
      expect(fnStmt.name).toBe("add");
      expect(fnStmt.params.length).toBe(2);
      expect(fnStmt.params[0].name).toBe("a");
      expect(fnStmt.params[1].name).toBe("b");
    });

    it("parses function with return type", () => {
      const ast = parse(dedent(`
        fn add a: Number, b: Number -> Number:
            a + b
      `));

      const fnStmt = ast.statements[0] as any;
      expect(fnStmt.returnType).toBeDefined();
      expect(fnStmt.returnType.name).toBe("Number");
    });
  });

  describe("expressions", () => {
    it("parses binary arithmetic", () => {
      const ast = parse("1 + 2 * 3");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Binary");
      // Should respect precedence: 1 + (2 * 3)
      expect(expr.left.value).toBe(1);
      expect(expr.right.type).toBe("Binary");
    });

    it("parses comparison operators", () => {
      const ast = parse("x > 10");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Binary");
      expect(expr.left.name).toBe("x");
      expect(expr.right.value).toBe(10);
    });

    it("parses logical operators", () => {
      const ast = parse("a and b or c");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Logical");
    });

    it("parses unary not", () => {
      const ast = parse("not true");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Unary");
      expect(expr.operand.value).toBe(true);
    });

    it("parses unary minus", () => {
      const ast = parse("-42");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Unary");
      expect(expr.operand.value).toBe(42);
    });
  });

  describe("records and lists", () => {
    it("parses record literal", () => {
      const ast = parse("{x: 1, y: 2}");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Record");
      expect(expr.fields.length).toBe(2);
      expect(expr.fields[0].key).toBe("x");
      expect(expr.fields[1].key).toBe("y");
    });

    it("parses list literal", () => {
      const ast = parse("[1, 2, 3]");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("List");
      expect(expr.elements.length).toBe(3);
    });

    it("parses nested structures", () => {
      const ast = parse("{pos: {x: 0, y: 0}, items: [1, 2]}");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Record");
      expect(expr.fields[0].value.type).toBe("Record");
      expect(expr.fields[1].value.type).toBe("List");
    });
  });

  describe("if expressions", () => {
    it("parses if expression", () => {
      const ast = parse(dedent(`
        if x > 0:
            "positive"
        else:
            "non-positive"
      `));

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("If");
      expect(expr.condition.type).toBe("Binary");
      expect(expr.thenBranch.type).toBe("Block");
      expect(expr.elseBranch.type).toBe("Block");
    });

    it("parses if without else", () => {
      const ast = parse(dedent(`
        if true:
            "yes"
      `));

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("If");
      expect(expr.elseBranch).toBeUndefined();
    });
  });

  describe("match expressions", () => {
    it("parses match with literal patterns", () => {
      const ast = parse(dedent(`
        match x:
            1 => "one"
            2 => "two"
            _ => "other"
      `));

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Match");
      expect(expr.arms.length).toBe(3);
      expect(expr.arms[0].pattern.type).toBe("LiteralPattern");
      expect(expr.arms[2].pattern.type).toBe("WildcardPattern");
    });

    it("parses match with record patterns", () => {
      const ast = parse(dedent(`
        match item:
            {type} => unlock()
      `));

      const expr = (ast.statements[0] as any).expression;
      expect(expr.arms[0].pattern.type).toBe("RecordPattern");
    });
  });

  describe("function calls", () => {
    it("parses function call with parentheses", () => {
      const ast = parse("foo(1, 2)");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Call");
      expect(expr.callee.name).toBe("foo");
      expect(expr.args.length).toBe(2);
    });

    it("parses juxtaposition call", () => {
      const ast = parse('say "hello"');

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Call");
      expect(expr.callee.name).toBe("say");
      expect(expr.args[0].value).toBe("hello");
    });

    it("parses member access", () => {
      const ast = parse("player.health");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Member");
      expect(expr.object.name).toBe("player");
      expect(expr.property).toBe("health");
    });

    it("parses index access", () => {
      const ast = parse("items[0]");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Index");
      expect(expr.object.name).toBe("items");
      expect(expr.index.value).toBe(0);
    });
  });

  describe("signal handling", () => {
    it("parses on statement", () => {
      const ast = parse(dedent(`
        on @player.moved:
            update()
      `));

      const onStmt = ast.statements[0] as any;
      expect(onStmt.type).toBe("On");
      expect(onStmt.signal.parts).toEqual(["player", "moved"]);
    });

    it("parses on statement with filter", () => {
      const ast = parse(dedent(`
        on @puzzle.solved "door":
            open_door()
      `));

      const onStmt = ast.statements[0] as any;
      expect(onStmt.type).toBe("On");
      expect(onStmt.filter.value).toBe("door");
    });

    it("parses emit statement", () => {
      const ast = parse("emit @player.died");

      const emitStmt = ast.statements[0] as any;
      expect(emitStmt.type).toBe("Emit");
      expect(emitStmt.signal.parts).toEqual(["player", "died"]);
    });

    it("parses emit with data", () => {
      const ast = parse("emit @player.moved {x: 10, y: 20}");

      const emitStmt = ast.statements[0] as any;
      expect(emitStmt.type).toBe("Emit");
      expect(emitStmt.data.type).toBe("Record");
    });
  });

  describe("assignment", () => {
    it("parses variable assignment", () => {
      const ast = parse("x = 42");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Assign");
      expect(expr.target.name).toBe("x");
      expect(expr.value.value).toBe(42);
    });

    it("parses member assignment", () => {
      const ast = parse("player.health = 100");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Assign");
      expect(expr.target.type).toBe("Member");
    });
  });

  describe("color literals", () => {
    it("parses hex color", () => {
      const ast = parse("#FF0000");

      const expr = (ast.statements[0] as any).expression;
      expect(expr.type).toBe("Color");
      expect(expr.hex).toBe("#FF0000");
    });
  });

  describe("loop statements", () => {
    it("parses for loop", () => {
      const ast = parse(dedent(`
        for i in items:
            print(i)
      `));

      const forStmt = ast.statements[0] as any;
      expect(forStmt.type).toBe("For");
      expect(forStmt.variable).toBe("i");
      expect(forStmt.iterable.name).toBe("items");
    });
  });
});
