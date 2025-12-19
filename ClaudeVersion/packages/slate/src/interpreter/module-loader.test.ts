import { describe, it, expect } from "bun:test";
import { Lexer } from "../lexer/lexer";
import { Parser } from "../parser/parser";
import { Interpreter } from "./interpreter";
import {
  ModuleLoader,
  InMemoryModuleResolver,
  type ModuleInterpreter,
} from "./module-loader";
import type { SlateRecord } from "@oort/core";
import { Num, Str, isRecord, isNumber } from "@oort/core";
import { stdlib } from "../stdlib/stdlib";

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

// Helper to create an interpreter factory for module loading
function createModuleInterpreterFactory(
  resolver: InMemoryModuleResolver
): (loader: ModuleLoader) => ModuleInterpreter {
  return (loader: ModuleLoader) => ({
    executeModule(source: string, _path: string): SlateRecord {
      const lexer = new Lexer(source);
      const tokens = lexer.tokenize();
      const parser = new Parser(tokens);
      const ast = parser.parse();

      // Include stdlib in module execution
      const interpreter = new Interpreter({
        globals: stdlib,
        moduleLoader: loader,
      });
      interpreter.run(ast);
      return interpreter.getExports();
    },
  });
}

// Helper to run Slate code with modules
function runWithModules(source: string, modules: Record<string, string>) {
  // Dedent both source and module sources
  const dedentedModules: Record<string, string> = {};
  for (const [path, src] of Object.entries(modules)) {
    dedentedModules[path] = dedent(src);
  }

  const resolver = new InMemoryModuleResolver(dedentedModules);
  const loader = new ModuleLoader(resolver, createModuleInterpreterFactory(resolver));

  const lexer = new Lexer(dedent(source));
  const tokens = lexer.tokenize();
  const parser = new Parser(tokens);
  const ast = parser.parse();

  const interpreter = new Interpreter({
    globals: stdlib,
    moduleLoader: loader,
  });

  return interpreter.run(ast);
}

describe("Module System", () => {
  describe("import parsing", () => {
    it("parses basic import statement", () => {
      const lexer = new Lexer("import math");
      const tokens = lexer.tokenize();
      const parser = new Parser(tokens);
      const ast = parser.parse();

      expect(ast.statements.length).toBe(1);
      expect(ast.statements[0].type).toBe("Import");
      expect((ast.statements[0] as any).path).toBe("math");
      expect((ast.statements[0] as any).alias).toBeUndefined();
      expect((ast.statements[0] as any).names).toBeUndefined();
    });

    it("parses import with alias", () => {
      const lexer = new Lexer("import math as m");
      const tokens = lexer.tokenize();
      const parser = new Parser(tokens);
      const ast = parser.parse();

      expect(ast.statements.length).toBe(1);
      expect((ast.statements[0] as any).path).toBe("math");
      expect((ast.statements[0] as any).alias).toBe("m");
    });

    it("parses from import", () => {
      const lexer = new Lexer("from math import sin, cos");
      const tokens = lexer.tokenize();
      const parser = new Parser(tokens);
      const ast = parser.parse();

      expect(ast.statements.length).toBe(1);
      expect((ast.statements[0] as any).path).toBe("math");
      expect((ast.statements[0] as any).names).toEqual(["sin", "cos"]);
    });
  });

  describe("module loading", () => {
    it("loads and uses a simple module with namespace import", () => {
      const result = runWithModules(
        `
        import utils
        utils.double(5)
        `,
        {
          utils: `
            fn double x:
              x * 2
          `,
        }
      );

      expect(result).toEqual(Num(10));
    });

    it("loads and uses a module with aliased import", () => {
      const result = runWithModules(
        `
        import utils as u
        u.triple(4)
        `,
        {
          utils: `
            fn triple x:
              x * 3
          `,
        }
      );

      expect(result).toEqual(Num(12));
    });

    it("loads and uses a module with selective import", () => {
      const result = runWithModules(
        `
        from math import add, multiply
        add(3, multiply(2, 4))
        `,
        {
          math: `
            fn add a b:
              a + b
            fn multiply a b:
              a * b
          `,
        }
      );

      expect(result).toEqual(Num(11));
    });

    it("supports multiple imports from different modules", () => {
      const result = runWithModules(
        `
        from math import add
        from strings import greet
        let msg = greet("World")
        add(10, 5)
        `,
        {
          math: `
            fn add a b:
              a + b
          `,
          strings: `
            fn greet name:
              "Hello, " + name
          `,
        }
      );

      expect(result).toEqual(Num(15));
    });

    it("module can use other modules", () => {
      const result = runWithModules(
        `
        import calculator
        calculator.compute(5)
        `,
        {
          calculator: `
            from helpers import double
            fn compute x:
              double(double(x))
          `,
          helpers: `
            fn double x:
              x * 2
          `,
        }
      );

      expect(result).toEqual(Num(20));
    });

    it("caches loaded modules", () => {
      // Test that the same module loaded twice returns cached result
      const resolver = new InMemoryModuleResolver({
        counter: `
          let count = 1
          fn get_count:
            count
        `,
      });

      const loader = new ModuleLoader(resolver, createModuleInterpreterFactory(resolver));

      // Load module twice
      const first = loader.load("counter");
      const second = loader.load("counter");

      // Should be the exact same object (cached)
      expect(first).toBe(second);
    });
  });

  describe("error handling", () => {
    it("throws error when module not found", () => {
      expect(() => {
        runWithModules(
          `
          import nonexistent
          `,
          {}
        );
      }).toThrow("Module not found: nonexistent");
    });

    it("throws error when importing non-existent name", () => {
      expect(() => {
        runWithModules(
          `
          from math import nonexistent
          `,
          {
            math: `
              fn add a b:
                a + b
            `,
          }
        );
      }).toThrow("does not export 'nonexistent'");
    });
  });

  describe("InMemoryModuleResolver", () => {
    it("can add modules dynamically", () => {
      const resolver = new InMemoryModuleResolver();
      resolver.addModule("test", "let x = 42");
      expect(resolver.readModule("test")).toBe("let x = 42");
    });

    it("can be initialized with modules", () => {
      const resolver = new InMemoryModuleResolver({
        a: "let a = 1",
        b: "let b = 2",
      });
      expect(resolver.readModule("a")).toBe("let a = 1");
      expect(resolver.readModule("b")).toBe("let b = 2");
    });
  });
});
