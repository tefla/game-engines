/**
 * Tests for the minimal S-expression runtime
 */

import { describe, it, expect } from "vitest";
import { read, sexprToString } from "./reader";
import { createSexprRuntime, sexprEval } from "./index";
import { valueToString } from "./evaluator";

describe("S-Expression Reader", () => {
  it("reads numbers", () => {
    expect(read("42")).toEqual([{ type: "number", value: 42 }]);
    expect(read("-5")).toEqual([{ type: "number", value: -5 }]);
    expect(read("3.14")).toEqual([{ type: "number", value: 3.14 }]);
  });

  it("reads strings", () => {
    expect(read('"hello"')).toEqual([{ type: "string", value: "hello" }]);
    expect(read('"with\\"quote"')).toEqual([{ type: "string", value: 'with"quote' }]);
  });

  it("reads symbols", () => {
    expect(read("foo")).toEqual([{ type: "symbol", name: "foo" }]);
    expect(read("+")).toEqual([{ type: "symbol", name: "+" }]);
  });

  it("reads booleans", () => {
    expect(read("#t")).toEqual([{ type: "boolean", value: true }]);
    expect(read("#f")).toEqual([{ type: "boolean", value: false }]);
  });

  it("reads lists", () => {
    expect(read("(1 2 3)")).toEqual([{
      type: "list",
      elements: [
        { type: "number", value: 1 },
        { type: "number", value: 2 },
        { type: "number", value: 3 },
      ]
    }]);
  });

  it("reads nested lists", () => {
    const result = read("(+ 1 (* 2 3))");
    expect(result[0]).toEqual({
      type: "list",
      elements: [
        { type: "symbol", name: "+" },
        { type: "number", value: 1 },
        {
          type: "list",
          elements: [
            { type: "symbol", name: "*" },
            { type: "number", value: 2 },
            { type: "number", value: 3 },
          ]
        }
      ]
    });
  });

  it("reads quote shorthand", () => {
    expect(read("'foo")).toEqual([{
      type: "list",
      elements: [
        { type: "symbol", name: "quote" },
        { type: "symbol", name: "foo" }
      ]
    }]);
  });

  it("skips comments", () => {
    expect(read("; comment\n42")).toEqual([{ type: "number", value: 42 }]);
  });

  it("reads multiple expressions", () => {
    expect(read("1 2 3")).toEqual([
      { type: "number", value: 1 },
      { type: "number", value: 2 },
      { type: "number", value: 3 },
    ]);
  });
});

describe("S-Expression Evaluator", () => {
  describe("self-evaluating", () => {
    it("evaluates numbers", () => {
      expect(sexprEval("42")).toEqual({ type: "number", value: 42 });
    });

    it("evaluates strings", () => {
      expect(sexprEval('"hello"')).toEqual({ type: "string", value: "hello" });
    });

    it("evaluates booleans", () => {
      expect(sexprEval("#t")).toEqual({ type: "boolean", value: true });
    });
  });

  describe("arithmetic", () => {
    it("adds numbers", () => {
      expect(sexprEval("(+ 1 2 3)")).toEqual({ type: "number", value: 6 });
    });

    it("subtracts numbers", () => {
      expect(sexprEval("(- 10 3)")).toEqual({ type: "number", value: 7 });
    });

    it("negates numbers", () => {
      expect(sexprEval("(- 5)")).toEqual({ type: "number", value: -5 });
    });

    it("multiplies numbers", () => {
      expect(sexprEval("(* 2 3 4)")).toEqual({ type: "number", value: 24 });
    });

    it("divides numbers", () => {
      expect(sexprEval("(/ 10 2)")).toEqual({ type: "number", value: 5 });
    });

    it("computes nested arithmetic", () => {
      expect(sexprEval("(+ 1 (* 2 3))")).toEqual({ type: "number", value: 7 });
    });
  });

  describe("define", () => {
    it("defines and looks up variables", () => {
      expect(sexprEval("(define x 42) x")).toEqual({ type: "number", value: 42 });
    });

    it("defines functions with shorthand", () => {
      expect(sexprEval("(define (square x) (* x x)) (square 5)")).toEqual({ type: "number", value: 25 });
    });
  });

  describe("let", () => {
    it("binds local variables", () => {
      expect(sexprEval("(let ((x 2) (y 3)) (+ x y))")).toEqual({ type: "number", value: 5 });
    });

    it("shadows outer bindings", () => {
      expect(sexprEval("(define x 10) (let ((x 5)) x)")).toEqual({ type: "number", value: 5 });
    });
  });

  describe("let*", () => {
    it("binds sequentially", () => {
      expect(sexprEval("(let* ((x 2) (y (* x 3))) y)")).toEqual({ type: "number", value: 6 });
    });
  });

  describe("fn/lambda", () => {
    it("creates and calls functions", () => {
      expect(sexprEval("((fn (x) (* x 2)) 5)")).toEqual({ type: "number", value: 10 });
    });

    it("creates closures", () => {
      expect(sexprEval(`
        (define (make-adder n)
          (fn (x) (+ x n)))
        (define add5 (make-adder 5))
        (add5 10)
      `)).toEqual({ type: "number", value: 15 });
    });
  });

  describe("if", () => {
    it("evaluates then branch when true", () => {
      expect(sexprEval("(if #t 1 2)")).toEqual({ type: "number", value: 1 });
    });

    it("evaluates else branch when false", () => {
      expect(sexprEval("(if #f 1 2)")).toEqual({ type: "number", value: 2 });
    });

    it("returns nil when no else and false", () => {
      expect(sexprEval("(if #f 1)")).toEqual({ type: "nil" });
    });
  });

  describe("cond", () => {
    it("evaluates matching clause", () => {
      expect(sexprEval("(cond (#f 1) (#t 2) (else 3))")).toEqual({ type: "number", value: 2 });
    });

    it("evaluates else clause", () => {
      expect(sexprEval("(cond (#f 1) (else 2))")).toEqual({ type: "number", value: 2 });
    });
  });

  describe("quote", () => {
    it("quotes symbols", () => {
      const result = sexprEval("'foo");
      expect(result).toEqual({ type: "list", elements: [
        { type: "string", value: "symbol" },
        { type: "string", value: "foo" }
      ]});
    });

    it("quotes lists", () => {
      const result = sexprEval("'(1 2 3)");
      expect(result).toEqual({ type: "list", elements: [
        { type: "number", value: 1 },
        { type: "number", value: 2 },
        { type: "number", value: 3 },
      ]});
    });
  });

  describe("and/or", () => {
    it("short-circuits and", () => {
      expect(sexprEval("(and #f (/ 1 0))")).toEqual({ type: "boolean", value: false });
    });

    it("short-circuits or", () => {
      expect(sexprEval("(or #t (/ 1 0))")).toEqual({ type: "boolean", value: true });
    });
  });

  describe("set!", () => {
    it("mutates variables", () => {
      expect(sexprEval("(define x 1) (set! x 2) x")).toEqual({ type: "number", value: 2 });
    });
  });

  describe("begin", () => {
    it("sequences expressions", () => {
      expect(sexprEval("(begin 1 2 3)")).toEqual({ type: "number", value: 3 });
    });
  });
});

describe("Primitives", () => {
  describe("strings", () => {
    it("appends strings", () => {
      expect(sexprEval('(string-append "hello" " " "world")')).toEqual({ type: "string", value: "hello world" });
    });

    it("gets string length", () => {
      expect(sexprEval('(string-length "hello")')).toEqual({ type: "number", value: 5 });
    });

    it("gets substring", () => {
      expect(sexprEval('(substring "hello" 1 4)')).toEqual({ type: "string", value: "ell" });
    });

    it("splits strings", () => {
      const result = sexprEval('(string-split "a,b,c" ",")');
      expect(result).toEqual({
        type: "list",
        elements: [
          { type: "string", value: "a" },
          { type: "string", value: "b" },
          { type: "string", value: "c" },
        ]
      });
    });
  });

  describe("lists", () => {
    it("constructs lists", () => {
      expect(sexprEval("(list 1 2 3)")).toEqual({
        type: "list",
        elements: [
          { type: "number", value: 1 },
          { type: "number", value: 2 },
          { type: "number", value: 3 },
        ]
      });
    });

    it("cons prepends", () => {
      expect(sexprEval("(cons 1 (list 2 3))")).toEqual({
        type: "list",
        elements: [
          { type: "number", value: 1 },
          { type: "number", value: 2 },
          { type: "number", value: 3 },
        ]
      });
    });

    it("car gets first", () => {
      expect(sexprEval("(car (list 1 2 3))")).toEqual({ type: "number", value: 1 });
    });

    it("cdr gets rest", () => {
      expect(sexprEval("(cdr (list 1 2 3))")).toEqual({
        type: "list",
        elements: [
          { type: "number", value: 2 },
          { type: "number", value: 3 },
        ]
      });
    });

    it("gets length", () => {
      expect(sexprEval("(length (list 1 2 3))")).toEqual({ type: "number", value: 3 });
    });

    it("appends lists", () => {
      expect(sexprEval("(append (list 1 2) (list 3 4))")).toEqual({
        type: "list",
        elements: [
          { type: "number", value: 1 },
          { type: "number", value: 2 },
          { type: "number", value: 3 },
          { type: "number", value: 4 },
        ]
      });
    });

    it("reverses lists", () => {
      expect(sexprEval("(reverse (list 1 2 3))")).toEqual({
        type: "list",
        elements: [
          { type: "number", value: 3 },
          { type: "number", value: 2 },
          { type: "number", value: 1 },
        ]
      });
    });
  });

  describe("predicates", () => {
    it("checks null?", () => {
      expect(sexprEval("(null? (list))")).toEqual({ type: "boolean", value: true });
      expect(sexprEval("(null? (list 1))")).toEqual({ type: "boolean", value: false });
    });

    it("checks pair?", () => {
      expect(sexprEval("(pair? (list 1))")).toEqual({ type: "boolean", value: true });
      expect(sexprEval("(pair? (list))")).toEqual({ type: "boolean", value: false });
    });

    it("checks string?", () => {
      expect(sexprEval('(string? "hello")')).toEqual({ type: "boolean", value: true });
      expect(sexprEval("(string? 42)")).toEqual({ type: "boolean", value: false });
    });

    it("checks number?", () => {
      expect(sexprEval("(number? 42)")).toEqual({ type: "boolean", value: true });
      expect(sexprEval('(number? "42")')).toEqual({ type: "boolean", value: false });
    });
  });

  describe("regex", () => {
    it("tests regex", () => {
      expect(sexprEval('(regex-test "[0-9]+" "abc123def")')).toEqual({ type: "boolean", value: true });
      expect(sexprEval('(regex-test "[0-9]+" "abcdef")')).toEqual({ type: "boolean", value: false });
    });

    it("matches regex", () => {
      const result = sexprEval('(regex-match "[0-9]+" "abc123def")');
      expect(result).toEqual({
        type: "list",
        elements: [{ type: "string", value: "123" }]
      });
    });
  });

  describe("characters", () => {
    it("checks whitespace", () => {
      expect(sexprEval('(char-whitespace? " ")')).toEqual({ type: "boolean", value: true });
      expect(sexprEval('(char-whitespace? "a")')).toEqual({ type: "boolean", value: false });
    });

    it("checks alphabetic", () => {
      expect(sexprEval('(char-alphabetic? "a")')).toEqual({ type: "boolean", value: true });
      expect(sexprEval('(char-alphabetic? "1")')).toEqual({ type: "boolean", value: false });
    });

    it("checks numeric", () => {
      expect(sexprEval('(char-numeric? "5")')).toEqual({ type: "boolean", value: true });
      expect(sexprEval('(char-numeric? "a")')).toEqual({ type: "boolean", value: false });
    });
  });
});

describe("Complex programs", () => {
  it("computes factorial", () => {
    const result = sexprEval(`
      (define (factorial n)
        (if (<= n 1)
            1
            (* n (factorial (- n 1)))))
      (factorial 5)
    `);
    expect(result).toEqual({ type: "number", value: 120 });
  });

  it("computes fibonacci", () => {
    const result = sexprEval(`
      (define (fib n)
        (cond
          ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))))
      (fib 10)
    `);
    expect(result).toEqual({ type: "number", value: 55 });
  });

  it("maps over list", () => {
    const result = sexprEval(`
      (define (my-map f lst)
        (if (null? lst)
            (list)
            (cons (f (car lst))
                  (my-map f (cdr lst)))))
      (my-map (fn (x) (* x 2)) (list 1 2 3))
    `);
    expect(result).toEqual({
      type: "list",
      elements: [
        { type: "number", value: 2 },
        { type: "number", value: 4 },
        { type: "number", value: 6 },
      ]
    });
  });

  it("filters list", () => {
    const result = sexprEval(`
      (define (my-filter pred lst)
        (cond
          ((null? lst) (list))
          ((pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))))
          (else (my-filter pred (cdr lst)))))
      (my-filter (fn (x) (> x 2)) (list 1 2 3 4 5))
    `);
    expect(result).toEqual({
      type: "list",
      elements: [
        { type: "number", value: 3 },
        { type: "number", value: 4 },
        { type: "number", value: 5 },
      ]
    });
  });
});
