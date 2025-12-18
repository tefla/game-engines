// Comprehensive Error Handling for Slate/Oort

// Error codes for categorization
export enum ErrorCode {
  // Parse errors (1xx)
  UNEXPECTED_TOKEN = 100,
  UNEXPECTED_EOF = 101,
  INVALID_SYNTAX = 102,
  MISSING_TOKEN = 103,
  INVALID_INDENTATION = 104,
  UNTERMINATED_STRING = 105,
  INVALID_NUMBER = 106,

  // Runtime errors (2xx)
  UNDEFINED_VARIABLE = 200,
  IMMUTABLE_ASSIGNMENT = 201,
  TYPE_ERROR = 202,
  DIVISION_BY_ZERO = 203,
  INDEX_OUT_OF_BOUNDS = 204,
  PROPERTY_NOT_FOUND = 205,
  NOT_CALLABLE = 206,
  ARITY_MISMATCH = 207,
  NO_MATCHING_PATTERN = 208,
  INVALID_OPERATION = 209,
  MAX_ITERATIONS = 210,

  // VFS errors (3xx)
  FILE_NOT_FOUND = 300,
  DIRECTORY_NOT_FOUND = 301,
  PERMISSION_DENIED = 302,
  PATH_EXISTS = 303,
  NOT_A_FILE = 304,
  NOT_A_DIRECTORY = 305,
  DIRECTORY_NOT_EMPTY = 306,
  INVALID_PATH = 307,

  // VCS errors (4xx)
  SNAPSHOT_NOT_FOUND = 400,
  INVALID_HISTORY_INDEX = 401,

  // Signal errors (5xx)
  INVALID_SIGNAL_PATH = 500,
}

// Hints for common errors
const ERROR_HINTS: Record<ErrorCode, string> = {
  [ErrorCode.UNEXPECTED_TOKEN]: "Check for typos or missing operators",
  [ErrorCode.UNEXPECTED_EOF]: "Your code may be incomplete - check for missing closing brackets or blocks",
  [ErrorCode.INVALID_SYNTAX]: "Review the syntax for this construct in the documentation",
  [ErrorCode.MISSING_TOKEN]: "Add the required token at this position",
  [ErrorCode.INVALID_INDENTATION]: "Use consistent indentation (4 spaces or 1 tab per level)",
  [ErrorCode.UNTERMINATED_STRING]: "Add a closing quote to your string",
  [ErrorCode.INVALID_NUMBER]: "Check that the number is properly formatted",

  [ErrorCode.UNDEFINED_VARIABLE]: "Make sure the variable is defined with 'let' or 'var' before use",
  [ErrorCode.IMMUTABLE_ASSIGNMENT]: "Use 'var' instead of 'let' if you need to reassign this variable",
  [ErrorCode.TYPE_ERROR]: "Check that you're using the right type for this operation",
  [ErrorCode.DIVISION_BY_ZERO]: "Ensure the divisor is not zero before dividing",
  [ErrorCode.INDEX_OUT_OF_BOUNDS]: "Check that the index is within the valid range",
  [ErrorCode.PROPERTY_NOT_FOUND]: "Verify the property name or check if the object has this property",
  [ErrorCode.NOT_CALLABLE]: "You can only call functions - check that this value is a function",
  [ErrorCode.ARITY_MISMATCH]: "Check the function signature for the correct number of arguments",
  [ErrorCode.NO_MATCHING_PATTERN]: "Add a catch-all pattern using '_' to handle unmatched cases",
  [ErrorCode.INVALID_OPERATION]: "This operation is not supported for these types",
  [ErrorCode.MAX_ITERATIONS]: "Add a break condition or reduce iterations to prevent infinite loops",

  [ErrorCode.FILE_NOT_FOUND]: "Check the file path for typos",
  [ErrorCode.DIRECTORY_NOT_FOUND]: "Create the directory first with mkdir()",
  [ErrorCode.PERMISSION_DENIED]: "You don't have permission - solve a puzzle to unlock access",
  [ErrorCode.PATH_EXISTS]: "Choose a different name or remove the existing file/directory",
  [ErrorCode.NOT_A_FILE]: "This path points to a directory, not a file",
  [ErrorCode.NOT_A_DIRECTORY]: "This path points to a file, not a directory",
  [ErrorCode.DIRECTORY_NOT_EMPTY]: "Remove all files inside first, or use recursive delete",
  [ErrorCode.INVALID_PATH]: "Check that all parent directories exist",

  [ErrorCode.SNAPSHOT_NOT_FOUND]: "Use snapshots() to see available snapshots",
  [ErrorCode.INVALID_HISTORY_INDEX]: "Use history() to see valid history indices",

  [ErrorCode.INVALID_SIGNAL_PATH]: "Signal paths should be strings or lists like 'player.moved' or ['player', 'moved']",
};

// Source location information
export interface SourceLocation {
  line: number;
  column: number;
  file?: string;
  source?: string;
}

// Base error class with enhanced information
export class SlateError extends Error {
  readonly code: ErrorCode;
  readonly location?: SourceLocation;
  readonly hint?: string;

  constructor(
    message: string,
    code: ErrorCode,
    location?: SourceLocation,
    hint?: string
  ) {
    super(message);
    this.name = "SlateError";
    this.code = code;
    this.location = location;
    this.hint = hint ?? ERROR_HINTS[code];
  }

  // Format the error for display
  format(options: { showHint?: boolean; showCode?: boolean } = {}): string {
    const { showHint = true, showCode = true } = options;
    const parts: string[] = [];

    // Error type and message
    const codeStr = showCode ? ` [E${this.code}]` : "";
    parts.push(`${this.name}${codeStr}: ${this.message}`);

    // Location
    if (this.location) {
      const file = this.location.file ? `${this.location.file}:` : "";
      parts.push(`  at ${file}${this.location.line}:${this.location.column}`);

      // Source context if available
      if (this.location.source) {
        const lines = this.location.source.split("\n");
        const lineNum = this.location.line;
        const contextLines: string[] = [];

        // Show line before, current line, line after
        for (let i = Math.max(0, lineNum - 2); i < Math.min(lines.length, lineNum + 1); i++) {
          const prefix = i === lineNum - 1 ? ">" : " ";
          const lineNumStr = String(i + 1).padStart(4, " ");
          contextLines.push(`${prefix}${lineNumStr} | ${lines[i]}`);
        }

        if (contextLines.length > 0) {
          parts.push("");
          parts.push(...contextLines);

          // Point to the column
          if (this.location.column > 0) {
            const pointer = " ".repeat(7 + this.location.column - 1) + "^";
            parts.push(pointer);
          }
        }
      }
    }

    // Hint
    if (showHint && this.hint) {
      parts.push("");
      parts.push(`Hint: ${this.hint}`);
    }

    return parts.join("\n");
  }
}

// Specific error types for better categorization
export class SlateParseError extends SlateError {
  constructor(message: string, code: ErrorCode, location?: SourceLocation, hint?: string) {
    super(message, code, location, hint);
    this.name = "ParseError";
  }
}

export class SlateRuntimeError extends SlateError {
  constructor(message: string, code: ErrorCode, location?: SourceLocation, hint?: string) {
    super(message, code, location, hint);
    this.name = "RuntimeError";
  }
}

export class SlateVfsError extends SlateError {
  readonly path: string;

  constructor(message: string, code: ErrorCode, path: string, location?: SourceLocation) {
    super(message, code, location);
    this.name = "VfsError";
    this.path = path;
  }
}

// Error factory functions for common cases
export const Errors = {
  // Parse errors
  unexpectedToken: (token: string, expected?: string, loc?: SourceLocation) =>
    new SlateParseError(
      expected
        ? `Unexpected token '${token}', expected ${expected}`
        : `Unexpected token '${token}'`,
      ErrorCode.UNEXPECTED_TOKEN,
      loc
    ),

  unexpectedEof: (loc?: SourceLocation) =>
    new SlateParseError("Unexpected end of input", ErrorCode.UNEXPECTED_EOF, loc),

  missingToken: (token: string, loc?: SourceLocation) =>
    new SlateParseError(`Missing '${token}'`, ErrorCode.MISSING_TOKEN, loc),

  unterminatedString: (loc?: SourceLocation) =>
    new SlateParseError("Unterminated string literal", ErrorCode.UNTERMINATED_STRING, loc),

  // Runtime errors
  undefinedVariable: (name: string, loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Undefined variable '${name}'`,
      ErrorCode.UNDEFINED_VARIABLE,
      loc
    ),

  immutableAssignment: (name: string, loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Cannot assign to immutable variable '${name}'`,
      ErrorCode.IMMUTABLE_ASSIGNMENT,
      loc
    ),

  typeError: (expected: string, got: string, operation?: string, loc?: SourceLocation) =>
    new SlateRuntimeError(
      operation
        ? `${operation} expects ${expected}, got ${got}`
        : `Expected ${expected}, got ${got}`,
      ErrorCode.TYPE_ERROR,
      loc
    ),

  divisionByZero: (loc?: SourceLocation) =>
    new SlateRuntimeError("Division by zero", ErrorCode.DIVISION_BY_ZERO, loc),

  indexOutOfBounds: (index: number, length: number, loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Index ${index} out of bounds (length: ${length})`,
      ErrorCode.INDEX_OUT_OF_BOUNDS,
      loc
    ),

  propertyNotFound: (property: string, type: string, loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Property '${property}' does not exist on ${type}`,
      ErrorCode.PROPERTY_NOT_FOUND,
      loc
    ),

  notCallable: (type: string, loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Cannot call ${type} - it is not a function`,
      ErrorCode.NOT_CALLABLE,
      loc
    ),

  arityMismatch: (name: string, expected: number | string, got: number, loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Function '${name}' expects ${expected} argument(s), got ${got}`,
      ErrorCode.ARITY_MISMATCH,
      loc
    ),

  noMatchingPattern: (loc?: SourceLocation) =>
    new SlateRuntimeError(
      "No matching pattern found",
      ErrorCode.NO_MATCHING_PATTERN,
      loc,
      "Add a catch-all pattern '_' to handle all cases"
    ),

  invalidOperation: (operation: string, types: string[], loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Cannot ${operation} on ${types.join(" and ")}`,
      ErrorCode.INVALID_OPERATION,
      loc
    ),

  maxIterations: (loc?: SourceLocation) =>
    new SlateRuntimeError(
      "Loop exceeded maximum iterations (possible infinite loop)",
      ErrorCode.MAX_ITERATIONS,
      loc
    ),

  // VFS errors
  fileNotFound: (path: string, loc?: SourceLocation) =>
    new SlateVfsError(`File not found: ${path}`, ErrorCode.FILE_NOT_FOUND, path, loc),

  directoryNotFound: (path: string, loc?: SourceLocation) =>
    new SlateVfsError(`Directory not found: ${path}`, ErrorCode.DIRECTORY_NOT_FOUND, path, loc),

  permissionDenied: (path: string, operation: string, loc?: SourceLocation) =>
    new SlateVfsError(
      `Permission denied: cannot ${operation} '${path}'`,
      ErrorCode.PERMISSION_DENIED,
      path,
      loc
    ),

  pathExists: (path: string, loc?: SourceLocation) =>
    new SlateVfsError(`Path already exists: ${path}`, ErrorCode.PATH_EXISTS, path, loc),

  notAFile: (path: string, loc?: SourceLocation) =>
    new SlateVfsError(`Not a file: ${path}`, ErrorCode.NOT_A_FILE, path, loc),

  notADirectory: (path: string, loc?: SourceLocation) =>
    new SlateVfsError(`Not a directory: ${path}`, ErrorCode.NOT_A_DIRECTORY, path, loc),

  // VCS errors
  snapshotNotFound: (name: string, loc?: SourceLocation) =>
    new SlateRuntimeError(
      `Snapshot not found: ${name}`,
      ErrorCode.SNAPSHOT_NOT_FOUND,
      loc
    ),

  // Signal errors
  invalidSignalPath: (loc?: SourceLocation) =>
    new SlateRuntimeError(
      "Invalid signal path format",
      ErrorCode.INVALID_SIGNAL_PATH,
      loc
    ),
};

// Error result type for recoverable errors
export type Result<T, E = SlateError> =
  | { ok: true; value: T }
  | { ok: false; error: E };

export const Ok = <T>(value: T): Result<T, never> => ({ ok: true, value });
export const Err = <E>(error: E): Result<never, E> => ({ ok: false, error });

// Format multiple errors (for batch compilation)
export function formatErrors(errors: SlateError[]): string {
  if (errors.length === 0) return "No errors";

  const header = errors.length === 1
    ? "1 error found:"
    : `${errors.length} errors found:`;

  const formatted = errors.map((e, i) => {
    const prefix = errors.length > 1 ? `\n[${i + 1}] ` : "\n";
    return prefix + e.format({ showCode: true, showHint: true });
  });

  return header + formatted.join("\n");
}

// Helper to wrap legacy errors
export function wrapError(error: Error, loc?: SourceLocation): SlateError {
  if (error instanceof SlateError) {
    return error;
  }

  // Try to detect error type from message
  const message = error.message.toLowerCase();

  if (message.includes("undefined variable")) {
    const match = error.message.match(/variable[:\s]+['"]?(\w+)['"]?/i);
    return Errors.undefinedVariable(match?.[1] ?? "unknown", loc);
  }

  if (message.includes("permission")) {
    return new SlateVfsError(error.message, ErrorCode.PERMISSION_DENIED, "", loc);
  }

  if (message.includes("not found")) {
    return new SlateRuntimeError(error.message, ErrorCode.FILE_NOT_FOUND, loc);
  }

  // Default to generic runtime error
  return new SlateRuntimeError(error.message, ErrorCode.INVALID_OPERATION, loc);
}
