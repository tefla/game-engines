# Slate Macro System Implementation Plan

## Overview

Implement `extend syntax` macros for Slate with:
- **Parse-time expansion** - Macros transform AST during parsing
- **Full AST node parameters** - Parameters capture raw AST nodes
- **YAML-style data blocks** - Indentation-based record syntax

## Target Syntax

```slate
# Define macro
extend syntax entity name props:
    spawn3d({id: name, ...props})

# Use macro - 'entity' becomes a keyword
entity Player:
    position: vec3(0, 1, 0)
    health: 100
    mesh: "/models/player.glb"

# Expands to:
spawn3d({id: "Player", position: vec3(0,1,0), health: 100, mesh: "/models/player.glb"})
```

---

## Phase 1: YAML-Style Data Blocks

Add indentation-based record syntax to the parser.

**File:** `packages/slate/src/parser/parser.ts`

### Syntax
```slate
# Current (JSON-style)
let player = {name: "Hero", health: 100}

# New (YAML-style) - colon + newline + indent = record
let player =
    name: "Hero"
    health: 100
    position:
        x: 0
        y: 5
```

### Implementation
1. In `primary()`, detect assignment followed by newline + INDENT
2. Add `parseYamlRecord()` method:
   - Consume INDENT
   - Loop: parse `identifier COLON expression` pairs
   - Handle nested indentation for nested records
   - Return `RecordExpr` AST node

### Changes
```typescript
// In primary() or assignment context
if (this.check(TokenType.INDENT)) {
    return this.parseYamlRecord();
}

private parseYamlRecord(): RecordExpr {
    const indent = this.consume(TokenType.INDENT);
    const fields: Array<{key: string, value: Expr}> = [];

    while (!this.check(TokenType.DEDENT) && !this.isAtEnd()) {
        const key = this.consume(TokenType.IDENTIFIER);
        this.consume(TokenType.COLON);

        // Check for nested YAML record
        if (this.check(TokenType.INDENT)) {
            fields.push({ key: key.lexeme, value: this.parseYamlRecord() });
        } else {
            fields.push({ key: key.lexeme, value: this.expression() });
        }
    }

    this.consume(TokenType.DEDENT);
    return { type: "Record", fields, line: indent.line, column: indent.column };
}
```

---

## Phase 2: Add SYNTAX Token

**File:** `packages/core/src/tokens.ts`

```typescript
// Add to TokenType enum
SYNTAX = "SYNTAX",

// Add to KEYWORDS map
"syntax": TokenType.SYNTAX,
```

---

## Phase 3: Parse `extend syntax` Statements

**File:** `packages/slate/src/parser/parser.ts`

### Add macro storage to Parser
```typescript
class Parser {
    private macros: Map<string, ExtendStmt> = new Map();
    // ...
}
```

### Add to declaration()
```typescript
private declaration(): Stmt | null {
    if (this.check(TokenType.EXTEND)) return this.extendStatement();
    // ... existing cases
}
```

### Implement extendStatement()
```typescript
private extendStatement(): ExtendStmt {
    const keyword = this.advance(); // consume 'extend'
    this.consume(TokenType.SYNTAX, "Expected 'syntax' after 'extend'");

    const macroName = this.consume(TokenType.IDENTIFIER, "Expected macro name");

    // Parse parameter names
    const params: string[] = [];
    while (this.check(TokenType.IDENTIFIER)) {
        params.push(this.advance().lexeme);
    }

    this.consume(TokenType.COLON, "Expected ':' before macro body");
    const body = this.block();

    // Register macro for later expansion
    const stmt: ExtendStmt = {
        type: "Extend",
        keyword: macroName.lexeme,
        params,
        body,
        line: keyword.line,
        column: keyword.column,
    };

    this.macros.set(macroName.lexeme, stmt);
    return stmt;
}
```

---

## Phase 4: Macro Invocation Detection

**File:** `packages/slate/src/parser/parser.ts`

### Modify declaration() to check for macro keywords
```typescript
private declaration(): Stmt | null {
    // ... existing keyword checks ...

    // Check for macro invocation before falling through to expression
    if (this.check(TokenType.IDENTIFIER)) {
        const name = this.peek().lexeme;
        if (this.macros.has(name)) {
            return this.macroInvocation(name);
        }
    }

    return this.expressionStatement();
}
```

### Implement macroInvocation()
```typescript
private macroInvocation(macroName: string): Stmt {
    const macro = this.macros.get(macroName)!;
    const startToken = this.advance(); // consume macro keyword

    // Capture arguments based on parameter count
    const args: Map<string, Expr> = new Map();

    for (let i = 0; i < macro.params.length; i++) {
        const paramName = macro.params[i];

        // Last param with colon = capture block as YAML record
        if (i === macro.params.length - 1 && this.check(TokenType.COLON)) {
            this.advance(); // consume ':'
            if (this.check(TokenType.INDENT)) {
                args.set(paramName, this.parseYamlRecord());
            } else {
                args.set(paramName, this.expression());
            }
        } else {
            // Capture as expression (identifier becomes string literal)
            if (this.check(TokenType.IDENTIFIER)) {
                const id = this.advance();
                args.set(paramName, {
                    type: "Literal",
                    value: id.lexeme,
                    line: id.line,
                    column: id.column
                });
            } else {
                args.set(paramName, this.expression());
            }
        }
    }

    // Expand macro
    return this.expandMacro(macro, args, startToken);
}
```

---

## Phase 5: AST Substitution (Macro Expansion)

**File:** `packages/slate/src/parser/parser.ts`

### Implement expandMacro()
```typescript
private expandMacro(
    macro: ExtendStmt,
    args: Map<string, Expr>,
    callSite: Token
): Stmt {
    // Clone and substitute the macro body
    const expandedBody = this.substituteBlock(macro.body, args);

    // Return as expression statement wrapping the block
    // Or if single statement, unwrap it
    if (expandedBody.statements.length === 1) {
        return expandedBody.statements[0];
    }

    // Multiple statements - wrap in immediately-invoked block
    return {
        type: "ExprStmt",
        expression: {
            type: "Call",
            callee: {
                type: "Function",
                params: [],
                body: expandedBody,
                line: callSite.line,
                column: callSite.column
            },
            args: [],
            line: callSite.line,
            column: callSite.column
        },
        line: callSite.line,
        column: callSite.column
    };
}
```

### Implement substitution helpers
```typescript
private substituteBlock(block: Block, subs: Map<string, Expr>): Block {
    return {
        ...block,
        statements: block.statements.map(s => this.substituteStmt(s, subs))
    };
}

private substituteStmt(stmt: Stmt, subs: Map<string, Expr>): Stmt {
    switch (stmt.type) {
        case "ExprStmt":
            return { ...stmt, expression: this.substituteExpr(stmt.expression, subs) };
        case "Let":
        case "Var":
            return { ...stmt, init: this.substituteExpr(stmt.init, subs) };
        case "If":
            return {
                ...stmt,
                condition: this.substituteExpr(stmt.condition, subs),
                then: this.substituteBlock(stmt.then, subs),
                else: stmt.else ? this.substituteBlock(stmt.else, subs) : undefined
            };
        // ... handle other statement types
        default:
            return stmt;
    }
}

private substituteExpr(expr: Expr, subs: Map<string, Expr>): Expr {
    switch (expr.type) {
        case "Identifier":
            // Replace if it's a macro parameter
            return subs.get(expr.name) ?? expr;

        case "Call":
            return {
                ...expr,
                callee: this.substituteExpr(expr.callee, subs),
                args: expr.args.map(a => this.substituteExpr(a, subs))
            };

        case "Record":
            return {
                ...expr,
                fields: expr.fields.map(f => ({
                    key: f.key,
                    value: this.substituteExpr(f.value, subs)
                }))
            };

        case "Binary":
            return {
                ...expr,
                left: this.substituteExpr(expr.left, subs),
                right: this.substituteExpr(expr.right, subs)
            };

        // ... handle other expression types
        default:
            return expr;
    }
}
```

---

## Phase 6: Spread Operator

**Files:**
- `packages/core/src/tokens.ts`
- `packages/slate/src/lexer/lexer.ts`
- `packages/slate/src/parser/parser.ts`
- `packages/slate/src/interpreter/interpreter.ts`

### Lexer - Add DOT_DOT_DOT token
```typescript
// tokens.ts
DOT_DOT_DOT = "DOT_DOT_DOT",

// lexer.ts - in scanToken()
case '.':
    if (this.match('.') && this.match('.')) {
        this.addToken(TokenType.DOT_DOT_DOT);
    } else {
        this.addToken(TokenType.DOT);
    }
    break;
```

### Parser - Handle spread in records
```typescript
// In recordLiteral()
if (this.match(TokenType.DOT_DOT_DOT)) {
    const spread = this.expression();
    fields.push({ key: "__spread__", value: spread, isSpread: true });
}
```

### Interpreter - Merge spread fields
```typescript
// In evaluateRecord()
for (const field of expr.fields) {
    if (field.isSpread) {
        const spreadValue = this.evaluate(field.value);
        if (isRecord(spreadValue)) {
            for (const [k, v] of spreadValue.fields) {
                result.set(k, v);
            }
        }
    } else {
        result.set(field.key, this.evaluate(field.value));
    }
}
```

---

## Phase 7: Interpreter - Handle ExtendStmt

**File:** `packages/slate/src/interpreter/interpreter.ts`

```typescript
// In executeStmt()
case "Extend":
    // Macros are expanded at parse time, nothing to do at runtime
    return Null();
```

---

## Phase 8: Hot Reload Integration

**Challenge:** Parse-time macros mean macro definitions are baked into the AST at parse time. When a macro definition changes, files using that macro need re-parsing.

**Current Hot Reload Flow:**
1. File watcher detects change
2. `runtime-service.ts` syncs file to VFS
3. If `.sl` file, re-parse and re-run

**Solution: Macro Registry with Dependency Tracking**

**File:** `packages/slate/src/parser/macro-registry.ts` (new)

```typescript
export class MacroRegistry {
    // Global macros available across files
    private macros: Map<string, ExtendStmt> = new Map();

    // Track which files define which macros
    private macroSources: Map<string, string> = new Map(); // macroName -> filePath

    // Track which files USE which macros (for invalidation)
    private macroDependents: Map<string, Set<string>> = new Map(); // macroName -> Set<filePath>

    registerMacro(name: string, def: ExtendStmt, sourceFile: string): void {
        this.macros.set(name, def);
        this.macroSources.set(name, sourceFile);
    }

    getMacro(name: string): ExtendStmt | undefined {
        return this.macros.get(name);
    }

    hasMacro(name: string): boolean {
        return this.macros.has(name);
    }

    // Called when a file uses a macro
    trackUsage(macroName: string, usingFile: string): void {
        if (!this.macroDependents.has(macroName)) {
            this.macroDependents.set(macroName, new Set());
        }
        this.macroDependents.get(macroName)!.add(usingFile);
    }

    // Get files that need re-parsing when a macro definition changes
    getDependentFiles(macroName: string): string[] {
        return Array.from(this.macroDependents.get(macroName) ?? []);
    }

    // Called when a file is re-parsed - clear its macro definitions
    clearMacrosFromFile(filePath: string): string[] {
        const removedMacros: string[] = [];
        for (const [name, source] of this.macroSources) {
            if (source === filePath) {
                this.macros.delete(name);
                this.macroSources.delete(name);
                removedMacros.push(name);
            }
        }
        return removedMacros;
    }

    // Clear usage tracking for a file (before re-parse)
    clearUsagesFromFile(filePath: string): void {
        for (const deps of this.macroDependents.values()) {
            deps.delete(filePath);
        }
    }
}

// Singleton for the runtime
export const macroRegistry = new MacroRegistry();
```

**Parser Changes:**
```typescript
// Parser constructor now accepts optional registry
constructor(tokens: Token[], registry?: MacroRegistry, sourceFile?: string) {
    this.registry = registry ?? new MacroRegistry();
    this.sourceFile = sourceFile ?? "<anonymous>";
}

// When parsing extend syntax, register globally
private extendStatement(): ExtendStmt {
    // ... existing parsing ...
    this.registry.registerMacro(macroName.lexeme, stmt, this.sourceFile);
    return stmt;
}

// When invoking macro, track dependency
private macroInvocation(macroName: string): Stmt {
    this.registry.trackUsage(macroName, this.sourceFile);
    const macro = this.registry.getMacro(macroName)!;
    // ... existing expansion ...
}
```

**Runtime Service Changes:**
```typescript
// In runtime-service.ts
import { macroRegistry } from "@oort/slate";

async syncFile(realPath: string): Promise<void> {
    const vfsPath = this.realPathToVfsPath(realPath);
    if (!vfsPath) return;

    if (vfsPath.endsWith(".sl")) {
        // Clear old macro definitions from this file
        const removedMacros = macroRegistry.clearMacrosFromFile(vfsPath);
        macroRegistry.clearUsagesFromFile(vfsPath);

        // Collect files that need re-parsing (used removed macros)
        const dependentFiles = new Set<string>();
        for (const macroName of removedMacros) {
            for (const dep of macroRegistry.getDependentFiles(macroName)) {
                if (dep !== vfsPath) dependentFiles.add(dep);
            }
        }

        // Re-parse this file first (may define new macros)
        await this.reloadSlateFile(vfsPath);

        // Re-parse dependent files
        for (const depFile of dependentFiles) {
            await this.reloadSlateFile(depFile);
        }
    }
}

private async reloadSlateFile(vfsPath: string): Promise<void> {
    const source = this.runtime?.getVfs().read(vfsPath);
    if (source) {
        this.runtime?.runFile(vfsPath);
        messageBus.emit("slate:file-reloaded", { path: vfsPath });
    }
}
```

**Hot Reload Sequence:**
1. User edits `macros.sl` which defines `entity` macro
2. File watcher triggers `syncFile("macros.sl")`
3. Clear old `entity` macro definition
4. Find files using `entity` (e.g., `game.sl`, `player.sl`)
5. Re-parse `macros.sl` (registers updated macro)
6. Re-parse `game.sl` and `player.sl` (expand with new definition)
7. Emit reload events for UI updates

---

## Files Summary

| File | Changes |
|------|---------|
| `packages/core/src/tokens.ts` | Add `SYNTAX`, `DOT_DOT_DOT` |
| `packages/core/src/ast.ts` | Already has `ExtendStmt` |
| `packages/slate/src/lexer/lexer.ts` | Tokenize `...` |
| `packages/slate/src/parser/parser.ts` | Macro parsing, YAML records, expansion |
| `packages/slate/src/parser/macro-registry.ts` | **NEW** - Macro registry with dependency tracking |
| `packages/slate/src/interpreter/interpreter.ts` | Handle spread, ExtendStmt no-op |
| `apps/oort-editor/src/renderer/core/runtime-service.ts` | Hot reload cascade |
| `packages/editor/src/syntax.ts` | Highlight `syntax` keyword |

---

## Test Cases

### Test 1: YAML Records
```slate
let config =
    name: "Test"
    nested:
        x: 1
        y: 2

assert(config.name == "Test")
assert(config.nested.x == 1)
```

### Test 2: Simple Macro
```slate
extend syntax log msg:
    say("[LOG] " + msg)

log "Hello"
# Should print: [LOG] Hello
```

### Test 3: Entity Macro
```slate
extend syntax entity name props:
    spawn3d({id: name, ...props})

entity Player:
    position: vec3(0, 1, 0)
    health: 100

# Should spawn entity with id "Player"
```

### Test 4: Spread Operator
```slate
let base = {x: 1, y: 2}
let extended = {z: 3, ...base}

assert(extended.x == 1)
assert(extended.z == 3)
```

---

## Implementation Order

1. **YAML-style records** (Phase 1) - Foundation for declarative blocks
2. **SYNTAX token** (Phase 2) - Quick addition
3. **Parse extend syntax** (Phase 3) - Store macro definitions
4. **Macro invocation** (Phase 4) - Detect and capture args
5. **AST substitution** (Phase 5) - Core expansion logic
6. **Spread operator** (Phase 6) - Enable `...props` pattern
7. **Interpreter integration** (Phase 7) - Handle ExtendStmt
8. **Hot reload integration** (Phase 8) - Macro registry and cascading re-parse
9. **Tests** - Comprehensive coverage

---

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Recursive expansion | Track depth, limit to 100 |
| Macro shadows variable | Macros take precedence, warn |
| Error location confusion | Preserve source locations |
| Complex AST cloning | Deep clone helper function |
| Hot reload cascade | Track dependencies, batch updates |
| Circular macro deps | Detect cycles during expansion |
