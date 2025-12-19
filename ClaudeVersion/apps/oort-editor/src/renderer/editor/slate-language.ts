// Slate Language Definition for Monaco Editor

import type * as Monaco from "monaco-editor";

// Register Slate language with Monaco
export function registerSlateLanguage(monaco: typeof Monaco): void {
  // Register the language
  monaco.languages.register({ id: "slate", extensions: [".sl"] });

  // Set language configuration (brackets, comments, etc.)
  monaco.languages.setLanguageConfiguration("slate", {
    comments: {
      lineComment: "#",
    },
    brackets: [
      ["{", "}"],
      ["[", "]"],
      ["(", ")"],
    ],
    autoClosingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: '"', close: '"' },
    ],
    surroundingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: '"', close: '"' },
    ],
    indentationRules: {
      increaseIndentPattern: /:\s*$/,
      decreaseIndentPattern: /^\s*(else|elif)\b/,
    },
    folding: {
      offSide: true,
    },
  });

  // Set tokenizer (syntax highlighting)
  monaco.languages.setMonarchTokensProvider("slate", {
    keywords: [
      "let",
      "var",
      "fn",
      "if",
      "else",
      "match",
      "on",
      "extend",
      "emit",
      "import",
      "loop",
      "for",
      "in",
      "by",
      "yield",
      "spawn",
      "wait",
      "type",
      "with",
    ],

    booleans: ["true", "false"],

    operators: [
      "+",
      "-",
      "*",
      "/",
      "%",
      "=",
      "==",
      "!=",
      "<",
      "<=",
      ">",
      ">=",
      "=>",
      "->",
      "..",
      "|",
      ".",
    ],

    symbols: /[=><!~?:&|+\-*\/\^%]+/,

    tokenizer: {
      root: [
        // Comments
        [/#(?![0-9a-fA-F]{3,6}\b).*$/, "comment"],

        // Color literals (#RRGGBB or #RGB)
        [/#[0-9a-fA-F]{6}\b/, "constant.color"],
        [/#[0-9a-fA-F]{3}\b/, "constant.color"],

        // Signal references (@signal)
        [/@[a-zA-Z_]\w*(\.[a-zA-Z_]\w*)*/, "variable.signal"],

        // Keywords and identifiers
        [
          /[a-zA-Z_]\w*/,
          {
            cases: {
              "@keywords": "keyword",
              "@booleans": "constant.language",
              "not|and|or": "keyword.operator",
              "@default": { token: "identifier" },
            },
          },
        ],

        // Type names (PascalCase)
        [/[A-Z][a-zA-Z0-9]*/, "type"],

        // Strings
        [/"([^"\\]|\\.)*$/, "string.invalid"], // non-terminated
        [/"/, { token: "string.quote", bracket: "@open", next: "@string" }],

        // Numbers
        [/\d+\.\d*/, "number.float"],
        [/\d+/, "number"],

        // Operators
        [/=>/, "keyword.operator"],
        [/->/, "keyword.operator"],
        [/\.\./, "keyword.operator"],
        [/[+\-*\/%]/, "operator.arithmetic"],
        [/[=!<>]=?/, "operator.comparison"],
        [/\|/, "operator"],

        // Delimiters
        [/[{}()\[\]]/, "@brackets"],
        [/[,:]/, "delimiter"],
        [/\./, "delimiter.property"],

        // Whitespace
        [/\s+/, "white"],
      ],

      string: [
        [/[^\\"]+/, "string"],
        [/\\./, "string.escape"],
        [/"/, { token: "string.quote", bracket: "@close", next: "@pop" }],
      ],
    },
  });

  // Register completion provider
  monaco.languages.registerCompletionItemProvider("slate", {
    provideCompletionItems: (model, position) => {
      const word = model.getWordUntilPosition(position);
      const range = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      };

      const suggestions: Monaco.languages.CompletionItem[] = [
        // Keywords
        ...["let", "var", "fn", "if", "else", "match", "on", "emit", "import", "for", "in", "loop", "type", "with"].map(
          (kw) => ({
            label: kw,
            kind: monaco.languages.CompletionItemKind.Keyword,
            insertText: kw,
            range,
          })
        ),

        // Snippets
        {
          label: "fn",
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: "fn ${1:name} ${2:params}:\n\t${0}",
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: "Define a function",
          range,
        },
        {
          label: "if",
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: "if ${1:condition}:\n\t${0}",
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: "If statement",
          range,
        },
        {
          label: "ifelse",
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: "if ${1:condition}:\n\t${2}\nelse:\n\t${0}",
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: "If-else statement",
          range,
        },
        {
          label: "match",
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: "match ${1:value}:\n\t${2:pattern} => ${0}",
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: "Pattern matching",
          range,
        },
        {
          label: "on",
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: "on ${1:signal}:\n\t${0}",
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: "Signal handler",
          range,
        },
        {
          label: "for",
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: "for ${1:item} in ${2:collection}:\n\t${0}",
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: "For loop",
          range,
        },

        // Standard library functions
        ...["abs", "min", "max", "clamp", "lerp", "print", "len", "push", "pop", "keys", "values"].map(
          (fn) => ({
            label: fn,
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: fn,
            range,
          })
        ),

        // Constants
        {
          label: "true",
          kind: monaco.languages.CompletionItemKind.Constant,
          insertText: "true",
          range,
        },
        {
          label: "false",
          kind: monaco.languages.CompletionItemKind.Constant,
          insertText: "false",
          range,
        },
      ];

      return { suggestions };
    },
  });
}

// Dark theme for Slate
export const slateThemeDark: Monaco.editor.IStandaloneThemeData = {
  base: "vs-dark",
  inherit: true,
  rules: [
    { token: "keyword", foreground: "c586c0", fontStyle: "bold" },
    { token: "keyword.operator", foreground: "c586c0" },
    { token: "constant.language", foreground: "569cd6" },
    { token: "constant.color", foreground: "ce9178" },
    { token: "variable.signal", foreground: "4ec9b0" },
    { token: "type", foreground: "4ec9b0" },
    { token: "identifier", foreground: "9cdcfe" },
    { token: "string", foreground: "ce9178" },
    { token: "string.escape", foreground: "d7ba7d" },
    { token: "number", foreground: "b5cea8" },
    { token: "number.float", foreground: "b5cea8" },
    { token: "comment", foreground: "6a9955", fontStyle: "italic" },
    { token: "operator", foreground: "d4d4d4" },
    { token: "operator.arithmetic", foreground: "d4d4d4" },
    { token: "operator.comparison", foreground: "d4d4d4" },
    { token: "delimiter", foreground: "d4d4d4" },
    { token: "delimiter.property", foreground: "d4d4d4" },
  ],
  colors: {
    "editor.background": "#1e1e1e",
    "editor.foreground": "#d4d4d4",
    "editorLineNumber.foreground": "#858585",
    "editorCursor.foreground": "#aeafad",
    "editor.selectionBackground": "#264f78",
    "editor.lineHighlightBackground": "#2a2d2e",
  },
};

// Light theme for Slate
export const slateThemeLight: Monaco.editor.IStandaloneThemeData = {
  base: "vs",
  inherit: true,
  rules: [
    { token: "keyword", foreground: "0000ff", fontStyle: "bold" },
    { token: "keyword.operator", foreground: "0000ff" },
    { token: "constant.language", foreground: "0000ff" },
    { token: "constant.color", foreground: "a31515" },
    { token: "variable.signal", foreground: "267f99" },
    { token: "type", foreground: "267f99" },
    { token: "identifier", foreground: "001080" },
    { token: "string", foreground: "a31515" },
    { token: "string.escape", foreground: "ee0000" },
    { token: "number", foreground: "098658" },
    { token: "number.float", foreground: "098658" },
    { token: "comment", foreground: "008000", fontStyle: "italic" },
    { token: "operator", foreground: "1e1e1e" },
    { token: "operator.arithmetic", foreground: "1e1e1e" },
    { token: "operator.comparison", foreground: "1e1e1e" },
    { token: "delimiter", foreground: "1e1e1e" },
  ],
  colors: {
    "editor.background": "#ffffff",
    "editor.foreground": "#1e1e1e",
  },
};

// Register themes
export function registerSlateThemes(monaco: typeof Monaco): void {
  monaco.editor.defineTheme("slate-dark", slateThemeDark);
  monaco.editor.defineTheme("slate-light", slateThemeLight);
}
