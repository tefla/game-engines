import { describe, it, expect } from "bun:test";
import { runSlate } from "@oort/slate";
import { Lexer } from "@oort/slate/src/lexer";
import { Parser } from "@oort/slate/src/parser";
import { diffPrograms, formatDiff, diffSource } from "./diff";
import type { Program } from "@oort/core";

// Helper to parse Slate source
function parse(source: string): Program {
  const lexer = new Lexer(source);
  const tokens = lexer.tokenize();
  const parser = new Parser(tokens);
  return parser.parse();
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

describe("AST Diff", () => {
  describe("diffPrograms", () => {
    it("detects no changes for identical programs", () => {
      const source = dedent(`
        let x = 42
      `);

      const before = parse(source);
      const after = parse(source);

      const result = diffPrograms(before, after);

      expect(result.entries.length).toBe(0);
      expect(result.summary.unchanged).toBeGreaterThan(0);
    });

    it("detects added function", () => {
      const before = parse("let x = 1");
      const after = parse(dedent(`
        let x = 1
        fn add a b:
            a + b
      `));

      const result = diffPrograms(before, after);

      const added = result.entries.filter((e) => e.type === "added");
      expect(added.length).toBe(1);
      expect(added[0].path).toContain("fn:add");
    });

    it("detects removed function", () => {
      const before = parse(dedent(`
        let x = 1
        fn add a b:
            a + b
      `));
      const after = parse("let x = 1");

      const result = diffPrograms(before, after);

      const removed = result.entries.filter((e) => e.type === "removed");
      expect(removed.length).toBe(1);
      expect(removed[0].path).toContain("fn:add");
    });

    it("detects modified variable value", () => {
      const before = parse("let x = 1");
      const after = parse("let x = 2");

      const result = diffPrograms(before, after);

      expect(result.entries.some((e) => e.type === "modified")).toBe(true);
    });

    it("detects modified function parameters", () => {
      const before = parse(dedent(`
        fn add a b:
            a + b
      `));
      const after = parse(dedent(`
        fn add a b c:
            a + b + c
      `));

      const result = diffPrograms(before, after);

      const modified = result.entries.filter((e) => e.type === "modified");
      expect(modified.length).toBeGreaterThan(0);

      const paramChange = modified.find((e) => e.path.includes("params"));
      expect(paramChange).toBeDefined();
    });

    it("detects modified function body", () => {
      const before = parse(dedent(`
        fn double x:
            x * 2
      `));
      const after = parse(dedent(`
        fn double x:
            x + x
      `));

      const result = diffPrograms(before, after);

      const bodyChange = result.entries.find((e) => e.path.includes("body"));
      expect(bodyChange).toBeDefined();
      expect(bodyChange?.type).toBe("modified");
    });

    it("detects changes to signal handlers", () => {
      const before = parse(dedent(`
        on @player.moved:
            update()
      `));
      const after = parse(dedent(`
        on @player.died:
            game_over()
      `));

      const result = diffPrograms(before, after);

      // Old handler removed, new handler added
      expect(result.entries.length).toBeGreaterThan(0);
    });
  });

  describe("formatDiff", () => {
    it("formats empty diff", () => {
      const result = {
        entries: [],
        summary: { added: 0, removed: 0, modified: 0, unchanged: 1 },
      };

      const formatted = formatDiff(result);
      expect(formatted).toContain("No changes detected");
    });

    it("formats added entry with + prefix", () => {
      const before = parse("let x = 1");
      const after = parse(dedent(`
        let x = 1
        fn add a b:
            a + b
      `));

      const result = diffPrograms(before, after);
      const formatted = formatDiff(result);

      expect(formatted).toContain("+");
      expect(formatted).toContain("added");
    });

    it("formats removed entry with - prefix", () => {
      const before = parse(dedent(`
        fn old:
            42
      `));
      const after = parse("");

      const result = diffPrograms(before, after);
      const formatted = formatDiff(result);

      expect(formatted).toContain("-");
      expect(formatted).toContain("removed");
    });
  });

  describe("diffSource", () => {
    it("diffs source code strings", () => {
      const before = "let x = 1";
      const after = "let x = 2";

      const result = diffSource(before, after, parse, "/test.sl");

      expect(result.entries.length).toBeGreaterThan(0);
      expect(result.entries[0].path).toContain("/test.sl");
    });
  });
});
