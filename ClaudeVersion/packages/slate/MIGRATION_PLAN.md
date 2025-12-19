# Slate S-expr Migration Plan

## Goal
Complete migration from old lexer/parser/interpreter to new S-expr implementation.

---

## Phase 1: Full Stdlib Implementation

Add all stdlib functions to the S-expr runtime.

### Math Functions
- [ ] `abs(n)` - absolute value
- [ ] `min(a, b, ...)` - minimum value
- [ ] `max(a, b, ...)` - maximum value
- [ ] `clamp(value, min, max)` - clamp to range
- [ ] `lerp(a, b, t)` - linear interpolation
- [ ] `floor(n)` - floor
- [ ] `ceil(n)` - ceiling
- [ ] `round(n)` - round to nearest
- [ ] `sqrt(n)` - square root
- [ ] `pow(base, exp)` - power
- [ ] `sin(n)`, `cos(n)`, `tan(n)` - trig functions
- [ ] `atan2(y, x)` - arc tangent
- [ ] `random()` - random 0-1
- [ ] `randomInt(min, max)` - random integer in range

### List Functions
- [ ] `length(list)` - list length
- [ ] `push(list, item)` - append item (returns new list)
- [ ] `pop(list)` - remove last (returns new list)
- [ ] `shift(list)` - remove first (returns new list)
- [ ] `slice(list, start, end)` - sublist
- [ ] `concat(list1, list2)` - concatenate lists
- [ ] `map(list, fn)` - map function over list
- [ ] `filter(list, fn)` - filter by predicate
- [ ] `reduce(list, fn, initial)` - reduce to single value
- [ ] `find(list, fn)` - find first matching
- [ ] `findIndex(list, fn)` - find index of first matching
- [ ] `includes(list, item)` - check if contains
- [ ] `indexOf(list, item)` - find index of item
- [ ] `reverse(list)` - reverse list
- [ ] `sort(list, fn?)` - sort list
- [ ] `join(list, separator)` - join to string
- [ ] `first(list)` - first element
- [ ] `last(list)` - last element
- [ ] `rest(list)` - all but first
- [ ] `take(list, n)` - first n elements
- [ ] `drop(list, n)` - all but first n

### Record Functions
- [ ] `keys(record)` - list of keys
- [ ] `values(record)` - list of values
- [ ] `entries(record)` - list of [key, value] pairs
- [ ] `has(record, key)` - check if key exists
- [ ] `get(record, key, default?)` - get value with optional default
- [ ] `set(record, key, value)` - set value (returns new record)
- [ ] `merge(record1, record2)` - merge records
- [ ] `omit(record, keys)` - remove keys
- [ ] `pick(record, keys)` - keep only keys

### String Functions
- [ ] `split(str, separator)` - split to list
- [ ] `trim(str)` - trim whitespace
- [ ] `upper(str)` - uppercase
- [ ] `lower(str)` - lowercase
- [ ] `replace(str, find, replace)` - replace first occurrence
- [ ] `replaceAll(str, find, replace)` - replace all occurrences
- [ ] `startsWith(str, prefix)` - check prefix
- [ ] `endsWith(str, suffix)` - check suffix
- [ ] `contains(str, substr)` - check contains
- [ ] `charAt(str, index)` - character at index
- [ ] `substring(str, start, end)` - substring
- [ ] `padStart(str, length, char)` - pad start
- [ ] `padEnd(str, length, char)` - pad end

### I/O Functions
- [ ] `say(args...)` - print with newline
- [ ] `print(args...)` - print without newline

### Type Functions
- [ ] `typeof(value)` - get type as string
- [ ] `isNumber(value)`, `isString(value)`, `isBool(value)`, etc.
- [ ] `toNumber(value)` - convert to number
- [ ] `toString(value)` - convert to string

### Files
- `packages/slate/src/sexpr/stdlib.ts` - Native stdlib implementations
- Update `packages/slate/src/sexpr/index.ts` - Inject stdlib into runtime

---

## Phase 2: Syntax Highlighting from S-expr Tokens

Create new syntax highlighter using S-expr lexer output.

### Tasks
- [ ] Create `packages/slate/src/sexpr/highlight.ts`
- [ ] Map S-expr token types to highlight categories
- [ ] Export `highlightSlate(source)` function
- [ ] Update `packages/editor/src/syntax.ts` to use new highlighter
- [ ] Remove dependency on old Lexer

### Token Category Mapping
```
IDENTIFIER -> identifier (or type if PascalCase)
NUMBER -> number
STRING -> string
keywords (LET, FN, IF, etc.) -> keyword
operators (+, -, *, etc.) -> operator
punctuation ({, }, [, ], etc.) -> punctuation
AT (@) -> signal
HASH (#) -> color (if followed by hex)
comments (#) -> comment
```

---

## Phase 3: Engine Adapter (SlateEngine)

Create adapter that wraps S-expr runtime with engine-specific features.

### Interface
```typescript
interface SlateEngine {
  run(source: string): unknown;

  // Signal handling
  getHandlers(): SignalHandler[];
  emit(signal: string[], data: unknown): void;

  // Output capture
  getOutput(): string[];

  // For debugging
  getEnvironment(): Map<string, unknown>;
}

interface SignalHandler {
  signal: string[];
  filter?: unknown;
  handler: (data: unknown) => unknown;
}
```

### Tasks
- [ ] Create `packages/slate/src/sexpr/engine.ts` with `createSlateEngine()`
- [ ] Implement handler collection in evaluator
- [ ] Implement output capture (override `say`/`print`)
- [ ] Add signal emission support
- [ ] Update `packages/engine/src/runtime/runtime.ts` to use SlateEngine

### Handler Collection Strategy
1. evaluator.sl's `eval-on` creates handler records
2. After evaluation, runtime collects all `(handler ...)` values
3. Each handler is wrapped in a TypeScript function that calls back into evaluator
4. Engine registers wrapped handlers with SignalBus

---

## Phase 4: Migrate Engine Runtime

Update engine to use new SlateEngine.

### Current Engine Structure
```typescript
class RuntimeInterpreter extends Interpreter {
  // Extends old Interpreter
}

class Runtime {
  private interpreter: Interpreter;
  // Uses interpreter.run(), emit(), getSignalHandlers(), getOutput()
}
```

### New Engine Structure
```typescript
class Runtime {
  private engine: SlateEngine;
  // Uses engine.run(), emit(), getHandlers(), getOutput()
}
```

### Tasks
- [ ] Replace `RuntimeInterpreter` with `SlateEngine`
- [ ] Update signal registration to use `engine.getHandlers()`
- [ ] Update output capture to use `engine.getOutput()`
- [ ] Update tests to work with new implementation
- [ ] Remove `Interpreter` import from engine

---

## Phase 5: VCS Diffing Decision

The VCS diff uses the old Parser to create typed AST for structural comparison.

### Options
A. **Keep old Parser** - Mark as legacy, used only for diffing
B. **Create AST converter** - Convert S-expr AST to typed AST nodes

### Recommendation
Keep old Parser for now. It's standalone and works. Can migrate later if needed.

### Tasks
- [ ] Mark `packages/slate/src/parser/` as legacy in comments
- [ ] Document that it's used for VCS diffing only

---

## Phase 6: Cleanup

### Remove
- [ ] `packages/slate/src/interpreter/` - Replaced by S-expr evaluator
- [ ] `packages/slate/src/macro/` - Already removed

### Keep (Legacy)
- `packages/slate/src/lexer/` - Used by parser for VCS diffing
- `packages/slate/src/parser/` - Used for VCS diffing

### Update Exports
- [ ] Update `packages/slate/src/index.ts` to remove Interpreter exports
- [ ] Keep Lexer/Parser exports for backward compatibility
- [ ] Primary exports: `createSlateRuntime()`, `createSlateEngine()`

---

## Testing Strategy

### Phase 1 Validation
- All 55 `slate-runtime.test.ts` tests pass
- All stdlib functions have unit tests

### Phase 2 Validation
- Syntax highlighting produces same output as old highlighter
- All token types correctly classified

### Phase 3-4 Validation
- All engine tests pass
- Signal handlers work correctly
- Game loop functions properly

### Final Validation
- All 700+ tests across monorepo pass
- No regressions in editor or VCS packages

---

## Implementation Order

| Phase | Description | Estimated Effort |
|-------|-------------|------------------|
| 1 | Full stdlib | Medium (2-3 hours) |
| 2 | S-expr syntax highlighting | Low (1 hour) |
| 3 | SlateEngine adapter | Medium (2 hours) |
| 4 | Migrate engine runtime | Medium (2 hours) |
| 5 | VCS decision (keep parser) | Low (30 min) |
| 6 | Cleanup | Low (30 min) |

Total: ~8-9 hours of focused work

---

## Notes

- The S-expr implementation is self-hosted (lexer.sl, reader.sl, expander.sl, evaluator.sl)
- Primitives are in TypeScript (primitives.ts) - stdlib extends these
- Signal handlers return `(handler signal body env)` records
- Engine wraps these in TypeScript functions for SignalBus registration
