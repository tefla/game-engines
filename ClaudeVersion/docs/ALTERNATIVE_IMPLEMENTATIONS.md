# Alternative Implementations

This document captures interesting approaches and ideas from the CodexVersion and GeminiVersion implementations that were explored before consolidating on ClaudeVersion.

---

## Overview

| Version | AI | Language Name | Syntax Style | Notable Features |
|---------|-----|---------------|--------------|------------------|
| **ClaudeVersion** | Claude Opus 4.5 | Slate | Python-like (indentation) | Full editor, signals, VFS |
| **CodexVersion** | OpenAI Codex | Slate | Python-like (indentation) | Generic ECS World |
| **GeminiVersion** | Google Gemini | Flux | Ruby-like (`do...end`) | TypeChecker, Visitor AST |

---

## GeminiVersion: Interesting Ideas

### 1. Gradual Type System

GeminiVersion implemented a basic type checker with inference. This is something we could add to Slate.

**Type annotations on variables:**
```flux
var health: Int = 100
var name: String = "Player"
```

**Type annotations on functions:**
```flux
fun add(a: Int, b: Int) -> Int {
    a + b
}
```

**TypeChecker implementation** (`src/lang/typechecker.ts`):
```typescript
export class TypeChecker implements Stmt.Visitor<void>, Expr.Visitor<TypeName> {
    private scopes: Record<string, TypeName>[] = [{}];

    visitVarStmt(stmt: Var): void {
        let valueType = "Any";
        if (stmt.initializer) {
            valueType = stmt.initializer.accept(this);
        }

        if (stmt.typeName) {
            if (valueType !== "Any" && valueType !== stmt.typeName) {
                console.error(`Type Error: Expected '${stmt.typeName}' but got '${valueType}'`);
            }
            this.define(stmt.name.lexeme, stmt.typeName);
        } else {
            // Type inference
            this.define(stmt.name.lexeme, valueType);
        }
    }

    visitLiteralExpr(expr: Literal): TypeName {
        if (typeof expr.value === 'number')
            return Number.isInteger(expr.value) ? "Int" : "Float";
        if (typeof expr.value === 'string') return "String";
        if (typeof expr.value === 'boolean') return "Bool";
        return "Nil";
    }
}
```

**Takeaway:** Type inference with optional annotations. Could add this to Slate for better tooling and error messages.

---

### 2. Visitor Pattern AST

GeminiVersion uses class-based AST with visitor pattern (like Crafting Interpreters):

```typescript
export abstract class Expr {
    abstract accept<R>(visitor: Visitor<R>): R;
}

export class Binary extends Expr {
    constructor(
        public left: Expr,
        public operator: Token,
        public right: Expr
    ) { super(); }

    accept<R>(visitor: Visitor<R>): R {
        return visitor.visitBinaryExpr(this);
    }
}
```

**ClaudeVersion uses discriminated unions:**
```typescript
type Expr =
    | { type: "Binary"; left: Expr; op: Token; right: Expr }
    | { type: "Literal"; value: any }
    // ...
```

**Trade-offs:**
| Aspect | Visitor (Gemini) | Discriminated Unions (Claude) |
|--------|------------------|------------------------------|
| Adding new node types | Easy | Easy |
| Adding new operations | Harder (modify all visitors) | Easy (add switch case) |
| Type safety | Runtime dispatch | Compile-time exhaustive checks |
| Boilerplate | More | Less |

---

### 3. Flux Syntax: `do...end` Blocks

GeminiVersion used Ruby-style blocks instead of Python-style indentation:

```flux
if ballPos.x > 10 do
    p1Score = p1Score + 1
    state = "GAME_OVER"
end

UI "Welcome to Pong" do
    Text "Press 'Start' to Play"
    Text "High Score: 999"
end
```

**Pros:**
- Explicit block boundaries (no whitespace sensitivity)
- Works better with copy-paste

**Cons:**
- More verbose
- Less clean visually

**We chose indentation** for Slate because it's cleaner and familiar to Python users.

---

### 4. DSL for UI (Command Pattern)

GeminiVersion had an interesting UI DSL pattern:

```flux
fun UI(title, block) {
    print "=== " + title + " ==="
    block()
    print "==================="
}

fun Text(content) {
    print "  " + content
}

// Usage
UI "Game Over" do
    Text "Player 1 Wins!"
    Text "Score: 42"
end
```

**Takeaway:** This pattern of passing blocks to functions for declarative UI is elegant. Our macro system could enable similar patterns:

```slate
extend syntax ui title content:
    say("=== " + title + " ===")
    content
    say("===================")

ui "Game Over":
    say("Player 1 Wins!")
```

---

## CodexVersion: Interesting Ideas

### 1. Generic ECS World

CodexVersion had a type-parameterized World class:

```typescript
export class World<T> {
    private readonly entities = new Map<EntityId, T>();

    spawn(value: T): EntityId {
        const id = `e${this.nextId++}`;
        this.entities.set(id, value);
        return id;
    }

    find(predicate: (value: T) => boolean): T[] {
        const out: T[] = [];
        for (const value of this.entities.values()) {
            if (predicate(value)) out.push(value);
        }
        return out;
    }
}

// Usage
const world = new World<GameEntity>();
const playerId = world.spawn({ health: 100, position: vec3(0,0,0) });
const enemies = world.find(e => e.type === "enemy");
```

**Takeaway:** Generic type parameter allows different entity shapes. Our current implementation uses a fixed entity type.

---

### 2. Shared Specification

Both CodexVersion and GeminiVersion started from the same `SLATE_SPEC.md` document, showing the value of a clear language specification as a foundation.

---

## Ideas for Future Slate Development

Based on these alternative implementations:

1. **Gradual Typing** (from Gemini)
   - Optional type annotations: `let x: Number = 5`
   - Type inference for unannotated bindings
   - TypeChecker pass before interpretation

2. **Block-passing Functions** (from Gemini)
   - Functions that take blocks as parameters
   - Our macro system enables this pattern

3. **Generic Collections** (from Codex)
   - Type-parameterized stdlib collections
   - `List[Number]`, `Map[String, Entity]`

4. **Better Error Messages**
   - Type errors with expected vs actual
   - Source location in all errors

---

## Pong Game Example (from GeminiVersion)

A complete game demonstrating the language:

```flux
// State
var state = "START"
var p1Score = 0
var p2Score = 0
var ballPos = Vec3(0, 0, 0)
var ballVel = Vec3(1, 1, 0)

fun ResetBall() {
    ballPos = Vec3(0, 0, 0)
}

fun UpdatePhysics() {
    ballPos = ballPos + ballVel

    if ballPos.x > 10 do
        p1Score = p1Score + 1
        state = "GAME_OVER"
    end

    if ballPos.x < -10 do
        p2Score = p2Score + 1
        state = "GAME_OVER"
    end
}

fun Tick() {
    if state == "START" do
        UI "Welcome to Pong" do
            Text "Press Start to Play"
        end
        state = "PLAY"
    end

    if state == "PLAY" do
        UpdatePhysics()
        print "Ball: " + ballPos
        print "Score: " + p1Score + " - " + p2Score
    end

    if state == "GAME_OVER" do
        UI "GAME OVER" do
            Text "Restart to play again"
        end
        state = "START"
        ResetBall()
    end
}

Tick()
```

**Equivalent in Slate:**
```slate
# State
var state = "START"
var p1Score = 0
var p2Score = 0
var ballPos = vec3(0, 0, 0)
var ballVel = vec3(1, 1, 0)

fn resetBall:
    ballPos = vec3(0, 0, 0)

fn updatePhysics:
    ballPos = ballPos + ballVel

    if ballPos.x > 10:
        p1Score = p1Score + 1
        state = "GAME_OVER"

    if ballPos.x < -10:
        p2Score = p2Score + 1
        state = "GAME_OVER"

on game.tick:
    match state:
        "START" =>
            say("Welcome to Pong!")
            state = "PLAY"
        "PLAY" =>
            updatePhysics()
            say("Score: " + p1Score + " - " + p2Score)
        "GAME_OVER" =>
            say("GAME OVER")
            resetBall()
            state = "START"
```

---

## Conclusion

The alternative implementations provided valuable exploration of different approaches:

- **GeminiVersion** showed that gradual typing and visitor-pattern AST are viable
- **CodexVersion** demonstrated generic type parameters for collections
- **Both** validated that the core Slate spec is implementable

ClaudeVersion combines the cleanest syntax (Python-style) with the most complete implementation (editor, signals, VFS, VCS).
