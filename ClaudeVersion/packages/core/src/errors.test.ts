import { describe, it, expect } from "bun:test";
import {
  SlateError,
  SlateParseError,
  SlateRuntimeError,
  SlateVfsError,
  ErrorCode,
  Errors,
  formatErrors,
  wrapError,
  Ok,
  Err,
} from "./errors";

describe("Error Handling", () => {
  describe("SlateError", () => {
    it("creates error with code and message", () => {
      const error = new SlateError("Test error", ErrorCode.INVALID_OPERATION);

      expect(error.message).toBe("Test error");
      expect(error.code).toBe(ErrorCode.INVALID_OPERATION);
      expect(error.name).toBe("SlateError");
    });

    it("includes location information", () => {
      const error = new SlateError("Test error", ErrorCode.TYPE_ERROR, {
        line: 5,
        column: 10,
        file: "test.sl",
      });

      expect(error.location?.line).toBe(5);
      expect(error.location?.column).toBe(10);
      expect(error.location?.file).toBe("test.sl");
    });

    it("provides hint from error code", () => {
      const error = new SlateError(
        "Cannot assign",
        ErrorCode.IMMUTABLE_ASSIGNMENT
      );

      expect(error.hint).toContain("var");
    });

    it("allows custom hint", () => {
      const error = new SlateError(
        "Custom error",
        ErrorCode.INVALID_OPERATION,
        undefined,
        "Custom hint"
      );

      expect(error.hint).toBe("Custom hint");
    });
  });

  describe("SlateError.format", () => {
    it("formats basic error", () => {
      const error = new SlateError("Something went wrong", ErrorCode.TYPE_ERROR);
      const formatted = error.format();

      expect(formatted).toContain("Something went wrong");
      expect(formatted).toContain("E202"); // ErrorCode.TYPE_ERROR
    });

    it("formats error with location", () => {
      const error = new SlateError("Error here", ErrorCode.TYPE_ERROR, {
        line: 3,
        column: 5,
        file: "test.sl",
      });
      const formatted = error.format();

      expect(formatted).toContain("test.sl:3:5");
    });

    it("includes source context when available", () => {
      const source = "let x = 1\nlet y = oops\nlet z = 3";
      const error = new SlateError("Undefined", ErrorCode.UNDEFINED_VARIABLE, {
        line: 2,
        column: 9,
        source,
      });
      const formatted = error.format();

      expect(formatted).toContain("let y = oops");
      expect(formatted).toContain("^");
    });

    it("hides code when requested", () => {
      const error = new SlateError("Error", ErrorCode.TYPE_ERROR);
      const formatted = error.format({ showCode: false });

      expect(formatted).not.toContain("E202");
    });

    it("hides hint when requested", () => {
      const error = new SlateError("Error", ErrorCode.TYPE_ERROR);
      const formatted = error.format({ showHint: false });

      expect(formatted).not.toContain("Hint:");
    });
  });

  describe("Specific error types", () => {
    it("creates ParseError", () => {
      const error = new SlateParseError(
        "Unexpected token",
        ErrorCode.UNEXPECTED_TOKEN
      );

      expect(error.name).toBe("ParseError");
      expect(error.code).toBe(ErrorCode.UNEXPECTED_TOKEN);
    });

    it("creates RuntimeError", () => {
      const error = new SlateRuntimeError(
        "Division by zero",
        ErrorCode.DIVISION_BY_ZERO
      );

      expect(error.name).toBe("RuntimeError");
      expect(error.code).toBe(ErrorCode.DIVISION_BY_ZERO);
    });

    it("creates VfsError with path", () => {
      const error = new SlateVfsError(
        "File not found",
        ErrorCode.FILE_NOT_FOUND,
        "/test/file.sl"
      );

      expect(error.name).toBe("VfsError");
      expect(error.path).toBe("/test/file.sl");
    });
  });

  describe("Error factory functions", () => {
    it("creates undefinedVariable error", () => {
      const error = Errors.undefinedVariable("foo");

      expect(error.message).toContain("foo");
      expect(error.code).toBe(ErrorCode.UNDEFINED_VARIABLE);
    });

    it("creates typeError", () => {
      const error = Errors.typeError("number", "string", "addition");

      expect(error.message).toContain("number");
      expect(error.message).toContain("string");
      expect(error.message).toContain("addition");
    });

    it("creates arityMismatch error", () => {
      const error = Errors.arityMismatch("test", 2, 3);

      expect(error.message).toContain("test");
      expect(error.message).toContain("2");
      expect(error.message).toContain("3");
    });

    it("creates indexOutOfBounds error", () => {
      const error = Errors.indexOutOfBounds(5, 3);

      expect(error.message).toContain("5");
      expect(error.message).toContain("3");
    });

    it("creates fileNotFound error", () => {
      const error = Errors.fileNotFound("/missing.sl");

      expect(error.message).toContain("/missing.sl");
      expect(error instanceof SlateVfsError).toBe(true);
      expect((error as SlateVfsError).path).toBe("/missing.sl");
    });

    it("creates permissionDenied error", () => {
      const error = Errors.permissionDenied("/secret.sl", "write");

      expect(error.message).toContain("/secret.sl");
      expect(error.message).toContain("write");
    });
  });

  describe("formatErrors", () => {
    it("formats empty error list", () => {
      const result = formatErrors([]);
      expect(result).toBe("No errors");
    });

    it("formats single error", () => {
      const errors = [
        new SlateError("Test error", ErrorCode.TYPE_ERROR),
      ];
      const result = formatErrors(errors);

      expect(result).toContain("1 error found");
      expect(result).toContain("Test error");
    });

    it("formats multiple errors", () => {
      const errors = [
        new SlateError("Error 1", ErrorCode.TYPE_ERROR),
        new SlateError("Error 2", ErrorCode.UNDEFINED_VARIABLE),
      ];
      const result = formatErrors(errors);

      expect(result).toContain("2 errors found");
      expect(result).toContain("Error 1");
      expect(result).toContain("Error 2");
    });
  });

  describe("wrapError", () => {
    it("returns SlateError unchanged", () => {
      const original = new SlateError("Original", ErrorCode.TYPE_ERROR);
      const wrapped = wrapError(original);

      expect(wrapped).toBe(original);
    });

    it("wraps undefined variable error", () => {
      const error = new Error("Undefined variable: foo");
      const wrapped = wrapError(error);

      expect(wrapped.code).toBe(ErrorCode.UNDEFINED_VARIABLE);
    });

    it("wraps permission error", () => {
      const error = new Error("Permission denied");
      const wrapped = wrapError(error);

      expect(wrapped.code).toBe(ErrorCode.PERMISSION_DENIED);
    });

    it("wraps not found error", () => {
      const error = new Error("File not found");
      const wrapped = wrapError(error);

      expect(wrapped.code).toBe(ErrorCode.FILE_NOT_FOUND);
    });

    it("defaults to INVALID_OPERATION for unknown errors", () => {
      const error = new Error("Something weird happened");
      const wrapped = wrapError(error);

      expect(wrapped.code).toBe(ErrorCode.INVALID_OPERATION);
    });
  });

  describe("Result type", () => {
    it("creates Ok result", () => {
      const result = Ok(42);

      expect(result.ok).toBe(true);
      expect((result as any).value).toBe(42);
    });

    it("creates Err result", () => {
      const error = new SlateError("Error", ErrorCode.TYPE_ERROR);
      const result = Err(error);

      expect(result.ok).toBe(false);
      expect((result as any).error).toBe(error);
    });
  });
});
