import { describe, it, expect } from "bun:test";
import {
  highlightSlate,
  highlightToHtml,
  type HighlightToken,
  SLATE_HIGHLIGHT_CSS,
} from "./syntax";

describe("Syntax Highlighting", () => {
  describe("highlightSlate", () => {
    it("highlights keywords", () => {
      const tokens = highlightSlate("let x = 1");
      const letToken = tokens.find((t) => t.text === "let");

      expect(letToken).toBeDefined();
      expect(letToken?.category).toBe("keyword");
    });

    it("highlights all keyword types", () => {
      const source = "let var fn if else match on emit for in";
      const tokens = highlightSlate(source);

      const keywords = tokens.filter((t) => t.category === "keyword");
      expect(keywords.length).toBeGreaterThanOrEqual(10);
    });

    it("highlights numbers", () => {
      const tokens = highlightSlate("let x = 42");
      const numToken = tokens.find((t) => t.text === "42");

      expect(numToken).toBeDefined();
      expect(numToken?.category).toBe("number");
    });

    it("highlights decimal numbers", () => {
      const tokens = highlightSlate("let pi = 3.14");
      const numToken = tokens.find((t) => t.text === "3.14");

      expect(numToken).toBeDefined();
      expect(numToken?.category).toBe("number");
    });

    it("highlights strings", () => {
      const tokens = highlightSlate('let name = "hello"');
      const strToken = tokens.find((t) => t.text === '"hello"');

      expect(strToken).toBeDefined();
      expect(strToken?.category).toBe("string");
    });

    it("highlights identifiers", () => {
      const tokens = highlightSlate("let myVariable = 1");
      const idToken = tokens.find((t) => t.text === "myVariable");

      expect(idToken).toBeDefined();
      expect(idToken?.category).toBe("identifier");
    });

    it("highlights type names (PascalCase)", () => {
      const tokens = highlightSlate("let x: Number = 1");
      const typeToken = tokens.find((t) => t.text === "Number");

      expect(typeToken).toBeDefined();
      expect(typeToken?.category).toBe("type");
    });

    it("highlights operators", () => {
      const tokens = highlightSlate("1 + 2 * 3 - 4 / 5");
      const plusToken = tokens.find((t) => t.text === "+");
      const starToken = tokens.find((t) => t.text === "*");

      expect(plusToken?.category).toBe("operator");
      expect(starToken?.category).toBe("operator");
    });

    it("highlights comparison operators", () => {
      const tokens = highlightSlate("x == y != z < a > b");
      const eqToken = tokens.find((t) => t.text === "==");
      const neqToken = tokens.find((t) => t.text === "!=");
      const ltToken = tokens.find((t) => t.text === "<");

      expect(eqToken?.category).toBe("operator");
      expect(neqToken?.category).toBe("operator");
      expect(ltToken?.category).toBe("operator");
    });

    it("highlights signal @ symbol", () => {
      const tokens = highlightSlate("on @player.moved:");
      const atToken = tokens.find((t) => t.text === "@");

      expect(atToken).toBeDefined();
      expect(atToken?.category).toBe("signal");
    });

    it("highlights hex colors", () => {
      const tokens = highlightSlate("let color = #FF0000");
      const colorToken = tokens.find((t) => t.text?.startsWith("#FF"));

      expect(colorToken).toBeDefined();
      expect(colorToken?.category).toBe("color");
    });

    it("highlights punctuation", () => {
      const tokens = highlightSlate("{x: 1, y: [2, 3]}");

      const braceToken = tokens.find((t) => t.text === "{");
      const colonToken = tokens.find((t) => t.text === ":");
      const commaToken = tokens.find((t) => t.text === ",");
      const bracketToken = tokens.find((t) => t.text === "[");

      expect(braceToken?.category).toBe("punctuation");
      expect(colonToken?.category).toBe("punctuation");
      expect(commaToken?.category).toBe("punctuation");
      expect(bracketToken?.category).toBe("punctuation");
    });

    it("includes comments", () => {
      const tokens = highlightSlate("let x = 1 # this is a comment");
      const commentToken = tokens.find((t) => t.category === "comment");

      expect(commentToken).toBeDefined();
      expect(commentToken?.text).toContain("# this is a comment");
    });

    it("does not treat hex colors as comments", () => {
      const tokens = highlightSlate("let color = #FF0000");
      const commentToken = tokens.find((t) => t.category === "comment");

      expect(commentToken).toBeUndefined();
    });

    it("provides position information", () => {
      const tokens = highlightSlate("let x = 1");
      const letToken = tokens.find((t) => t.text === "let");

      expect(letToken?.start).toBe(0);
      expect(letToken?.end).toBe(3);
      expect(letToken?.line).toBe(1);
      expect(letToken?.column).toBe(1);
    });

    it("handles multi-line source", () => {
      const source = `let x = 1
let y = 2`;
      const tokens = highlightSlate(source);

      const xToken = tokens.find((t) => t.text === "x");
      const yToken = tokens.find((t) => t.text === "y");

      expect(xToken?.line).toBe(1);
      expect(yToken?.line).toBe(2);
    });
  });

  describe("highlightToHtml", () => {
    it("wraps tokens in span elements", () => {
      const tokens = highlightSlate("let x = 1");
      const html = highlightToHtml("let x = 1", tokens);

      expect(html).toContain('<span class="slate-keyword">let</span>');
      expect(html).toContain('<span class="slate-number">1</span>');
    });

    it("preserves whitespace", () => {
      const source = "let   x = 1";
      const tokens = highlightSlate(source);
      const html = highlightToHtml(source, tokens);

      // Should maintain spacing
      expect(html).toContain("   ");
    });

    it("escapes HTML entities", () => {
      const source = 'let x = "<test>"';
      const tokens = highlightSlate(source);
      const html = highlightToHtml(source, tokens);

      expect(html).toContain("&lt;test&gt;");
    });
  });

  describe("CSS styles", () => {
    it("provides CSS for all token categories", () => {
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-keyword");
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-number");
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-string");
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-comment");
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-signal");
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-color");
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-type");
      expect(SLATE_HIGHLIGHT_CSS).toContain(".slate-error");
    });
  });
});
