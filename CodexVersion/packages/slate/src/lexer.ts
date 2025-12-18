export type TokenType =
  | "LET"
  | "VAR"
  | "FN"
  | "IF"
  | "ELSE"
  | "MATCH"
  | "ON"
  | "EXTEND"
  | "SYNTAX"
  | "EMIT"
  | "IMPORT"
  | "TRUE"
  | "FALSE"
  | "NULL"
  | "NOT"
  | "AND"
  | "OR"
  | "IDENT"
  | "NUMBER"
  | "STRING"
  | "COLOR"
  | "NEWLINE"
  | "INDENT"
  | "DEDENT"
  | "DASH"
  | "LPAREN"
  | "RPAREN"
  | "LBRACE"
  | "RBRACE"
  | "LBRACKET"
  | "RBRACKET"
  | "COMMA"
  | "COLON"
  | "DOT"
  | "EQ"
  | "EQEQ"
  | "NEQ"
  | "GT"
  | "GTE"
  | "LT"
  | "LTE"
  | "PLUS"
  | "MINUS"
  | "STAR"
  | "SLASH"
  | "FATARROW"
  | "EOF";

export type Token = {
  readonly type: TokenType;
  readonly lexeme: string;
  readonly line: number;
  readonly col: number;
};

const KEYWORDS: Record<string, TokenType> = {
  let: "LET",
  var: "VAR",
  fn: "FN",
  if: "IF",
  else: "ELSE",
  match: "MATCH",
  on: "ON",
  extend: "EXTEND",
  syntax: "SYNTAX",
  emit: "EMIT",
  import: "IMPORT",
  true: "TRUE",
  false: "FALSE",
  null: "NULL",
  not: "NOT",
  and: "AND",
  or: "OR",
};

function isAlpha(ch: string): boolean {
  const code = ch.charCodeAt(0);
  return (code >= 65 && code <= 90) || (code >= 97 && code <= 122) || ch === "_";
}

function isDigit(ch: string): boolean {
  const code = ch.charCodeAt(0);
  return code >= 48 && code <= 57;
}

function isHex(ch: string): boolean {
  const code = ch.charCodeAt(0);
  return (
    (code >= 48 && code <= 57) ||
    (code >= 65 && code <= 70) ||
    (code >= 97 && code <= 102)
  );
}

export function lex(source: string): Token[] {
  const tokens: Token[] = [];
  const indentStack: number[] = [0];

  const lines = source.replace(/\r\n/g, "\n").replace(/\r/g, "\n").split("\n");

  for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
    const rawLine = lines[lineIndex] ?? "";
    const lineNumber = lineIndex + 1;

    if (rawLine.trim().length === 0) continue;
    if (/^\s*#(?![0-9a-fA-F])/.test(rawLine)) continue;

    if (rawLine.includes("\t")) {
      throw new Error(`Tabs are not supported (line ${lineNumber})`);
    }

    const indentMatch = rawLine.match(/^ */);
    const indent = indentMatch ? indentMatch[0].length : 0;
    const currentIndent = indentStack[indentStack.length - 1] ?? 0;

    if (indent > currentIndent) {
      indentStack.push(indent);
      tokens.push({ type: "INDENT", lexeme: "", line: lineNumber, col: 1 });
    } else if (indent < currentIndent) {
      while (indentStack.length > 1 && (indentStack[indentStack.length - 1] ?? 0) > indent) {
        indentStack.pop();
        tokens.push({ type: "DEDENT", lexeme: "", line: lineNumber, col: 1 });
      }
      if ((indentStack[indentStack.length - 1] ?? 0) !== indent) {
        throw new Error(`Invalid indentation (line ${lineNumber})`);
      }
    }

    let i = indent;
    const line = rawLine;

    if (line.slice(i).startsWith("- ")) {
      tokens.push({ type: "DASH", lexeme: "-", line: lineNumber, col: i + 1 });
      i += 2;
    }

    while (i < line.length) {
      const ch = line[i]!;

      if (ch === " " || ch === "\n") {
        i++;
        continue;
      }

      const col = i + 1;

      if (ch === "#") {
        if (isHex(line[i + 1] ?? "")) {
          let j = i + 1;
          while (j < line.length && isHex(line[j]!)) j++;
          const hex = line.slice(i, j);
          tokens.push({ type: "COLOR", lexeme: hex, line: lineNumber, col });
          i = j;
          continue;
        }
        break;
      }

      if (isAlpha(ch)) {
        let j = i + 1;
        while (j < line.length && (isAlpha(line[j]!) || isDigit(line[j]!))) j++;
        const text = line.slice(i, j);
        const type = KEYWORDS[text] ?? "IDENT";
        tokens.push({ type, lexeme: text, line: lineNumber, col });
        i = j;
        continue;
      }

      if (isDigit(ch)) {
        let j = i + 1;
        while (j < line.length && isDigit(line[j]!)) j++;
        if (line[j] === "." && isDigit(line[j + 1] ?? "")) {
          j++;
          while (j < line.length && isDigit(line[j]!)) j++;
        }
        const num = line.slice(i, j);
        tokens.push({ type: "NUMBER", lexeme: num, line: lineNumber, col });
        i = j;
        continue;
      }

      if (ch === '"') {
        let j = i + 1;
        let value = '"';
        while (j < line.length) {
          const c = line[j]!;
          if (c === '"') {
            value += '"';
            j++;
            break;
          }
          if (c === "\\") {
            const next = line[j + 1];
            if (next === undefined) throw new Error(`Unterminated string (line ${lineNumber})`);
            value += "\\" + next;
            j += 2;
            continue;
          }
          value += c;
          j++;
        }
        if (!value.endsWith('"')) throw new Error(`Unterminated string (line ${lineNumber})`);
        tokens.push({ type: "STRING", lexeme: value, line: lineNumber, col });
        i = j;
        continue;
      }

      const two = line.slice(i, i + 2);
      switch (two) {
        case "=>":
          tokens.push({ type: "FATARROW", lexeme: "=>", line: lineNumber, col });
          i += 2;
          continue;
        case "==":
          tokens.push({ type: "EQEQ", lexeme: "==", line: lineNumber, col });
          i += 2;
          continue;
        case "!=":
          tokens.push({ type: "NEQ", lexeme: "!=", line: lineNumber, col });
          i += 2;
          continue;
        case ">=":
          tokens.push({ type: "GTE", lexeme: ">=", line: lineNumber, col });
          i += 2;
          continue;
        case "<=":
          tokens.push({ type: "LTE", lexeme: "<=", line: lineNumber, col });
          i += 2;
          continue;
      }

      switch (ch) {
        case "(":
          tokens.push({ type: "LPAREN", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case ")":
          tokens.push({ type: "RPAREN", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "{":
          tokens.push({ type: "LBRACE", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "}":
          tokens.push({ type: "RBRACE", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "[":
          tokens.push({ type: "LBRACKET", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "]":
          tokens.push({ type: "RBRACKET", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case ",":
          tokens.push({ type: "COMMA", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case ":":
          tokens.push({ type: "COLON", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case ".":
          tokens.push({ type: "DOT", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "=":
          tokens.push({ type: "EQ", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case ">":
          tokens.push({ type: "GT", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "<":
          tokens.push({ type: "LT", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "+":
          tokens.push({ type: "PLUS", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "-":
          tokens.push({ type: "MINUS", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "*":
          tokens.push({ type: "STAR", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
        case "/":
          tokens.push({ type: "SLASH", lexeme: ch, line: lineNumber, col });
          i++;
          continue;
      }

      throw new Error(`Unexpected character '${ch}' (line ${lineNumber}, col ${col})`);
    }

    tokens.push({ type: "NEWLINE", lexeme: "", line: lineNumber, col: line.length + 1 });
  }

  while (indentStack.length > 1) {
    indentStack.pop();
    const lastLine = lines.length;
    tokens.push({ type: "DEDENT", lexeme: "", line: lastLine, col: 1 });
  }

  tokens.push({ type: "EOF", lexeme: "", line: lines.length + 1, col: 1 });
  return tokens;
}
