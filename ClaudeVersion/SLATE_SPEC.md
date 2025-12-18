# Oort Engine + Slate Language: Unified Specification

> A minimal scripting language for player-editable puzzle games, with a virtual filesystem where permissions are puzzles.

---

## 1. Design Principles

1. **Minimal syntax** — 7 core keywords, indentation-based
2. **VFS as puzzle mechanic** — file permissions control what players can edit
3. **Tree-walking interpreter** — simple, hot-reload friendly
4. **Signals over callbacks** — declarative, replayable events
5. **AST-level VCS** — built-in undo/redo, snapshots

---

## 2. Language: Slate

### 2.1 Core Keywords (7)

| Keyword | Purpose |
|---------|---------|
| `let` | Immutable binding |
| `var` | Mutable binding |
| `fn` | Function definition |
| `if` | Conditional (expression) |
| `match` | Pattern matching |
| `on` | Signal handler |
| `extend` | Define new syntax (macros) |

### 2.2 Syntax

```slate
# Variables
let name = "Player"
var health = 100

# Functions (colon blocks, no parens)
fn greet name:
    "Hello, " + name

# Conditionals are expressions
let status = if health > 50:
    "healthy"
else:
    "wounded"

# Pattern matching
match item:
    {type: "key", color} => unlock(color)
    {type: "potion"} => heal(10)
    _ => "Unknown"

# Signals
on door.opened:
    play_sound "click"

emit player.moved, {x: 10, y: 5}

# Macros
extend syntax entity name body:
    create_entity(name, body)

entity Door:
    position: [1, 0, 3]
    locked: true
```

### 2.3 Data Literals

```slate
# Records
let player = {
    name: "Hero"
    health: 100
    pos: {x: 0, y: 0}
}

# Lists
let items = [1, 2, 3]

# YAML-style nested data
room Main:
    size: [10, 3, 10]
    floor: stone
    lights:
        - point: [2, 2, 2]
        - point: [8, 2, 8]
```

---

## 3. Virtual Filesystem (VFS)

### 3.1 Structure

```
/
├── engine/          # r-- (read-only, learn from it)
│   ├── physics.sl
│   └── render.sl
├── game/            # r-- (game logic, visible)
│   ├── entities/
│   └── systems/
├── puzzles/         # rw- (player can edit!)
│   ├── door.sl
│   └── lever.sl
├── secrets/         # --- (hidden until unlocked)
│   └── final_boss.sl
└── player/          # rwx (player's scratchpad)
    └── notes.sl
```

### 3.2 Permissions

```
r = read (view code)
w = write (edit code)
x = execute (run/import)
```

### 3.3 Puzzle Mechanics

```slate
# Permission as puzzle reward
on puzzle.solved "find_key":
    chmod "/puzzles/door.sl", "rw-"
    notify "You can now edit the door!"

# Hidden file reveal
on player.reaches "secret_room":
    chmod "/secrets/", "r--"

# Lock file after solving
on puzzle.solved "door":
    chmod "/puzzles/door.sl", "r--"
    notify "Door fixed! (locked to prevent regression)"
```

---

## 4. Engine Architecture

```
/engine
  /core
    vfs.ts            # Virtual filesystem + permissions
    signals.ts        # Event system
    hotReload.ts      # AST swap without restart
  /world
    entity.ts         # ECS entities
    scene.ts          # Spatial queries
  /render
    primitives.ts     # box, plane, line
    renderer.ts       # Three.js wrapper
  /script
    lexer.ts          # Tokenizer
    parser.ts         # AST builder
    interpreter.ts    # Tree-walking eval
    stdlib.ts         # Built-in functions
  /vcs
    snapshot.ts       # World + code state
    diff.ts           # AST-level diffing
    history.ts        # Undo/redo
```

### 4.1 Interpreter (Not VM)

Tree-walking for simplicity:

```typescript
function evaluate(node: ASTNode, env: Environment): Value {
    switch (node.type) {
        case "Literal": return node.value;
        case "Variable": return env.get(node.name);
        case "Binary": return applyOp(node.op, evaluate(node.left, env), evaluate(node.right, env));
        case "If": return evaluate(node.condition, env) ? evaluate(node.then, env) : evaluate(node.else, env);
        case "Call": return call(evaluate(node.callee, env), node.args.map(a => evaluate(a, env)));
        // ...
    }
}
```

### 4.2 Hot Reload

1. **File change detected**
2. **Re-parse to AST**
3. **Diff against previous AST**
4. **Swap only changed function definitions**
5. **Preserve entity state**

### 4.3 Rendering

Primitives only (agent-friendly):

| Primitive | Description |
|-----------|-------------|
| `box` | Axis-aligned cube |
| `plane` | Flat surface |
| `line` | Line segment |
| `point` | Single point |

```slate
entity Wall:
    visual: box [1, 3, 10]
    color: #808080
```

---

## 5. Version Control (VCS)

### 5.1 Automatic Commits

Every code edit creates an implicit checkpoint.

### 5.2 API

```slate
import vcs

vcs.snapshot "before hard puzzle"
vcs.restore "before hard puzzle"
vcs.undo
vcs.redo
```

### 5.3 AST Diffing

Diffs track semantic changes, not text:

```
- Changed: condition in /puzzles/door.sl:on_interact
+ Added: alternate trigger "override.signal"
```

---

## 6. Standard Library

```slate
# Math
abs(x), min(a, b), max(a, b), clamp(x, lo, hi), lerp(a, b, t)

# Vectors
vec3(x, y, z), dot(a, b), normalize(v), distance(a, b)

# Signals
emit signal, data
on signal: handler

# Entities
spawn entity
destroy entity
find {type: "Enemy"}

# VFS
read path
write path, content
chmod path, perms
ls path

# I/O
say message
play_sound name
```

---

## 7. Example: A Complete Puzzle

```slate
# /puzzles/broken_door.sl (rw-)

entity Door:
    position: [5, 1, 0]
    visual: box [2, 2.5, 0.1]
    color: #8B4513
    var locked: true

# BUG: Logic is inverted! Player must fix this.
on player.interact Door:
    if not player.has "gold_key":  # <-- wrong!
        unlock Door
    else:
        say "Door won't open"

fn unlock door:
    door.locked = false
    door.color = #00FF00
    emit door.opened
```

**Puzzle**: Player reads the code, spots `not`, removes it, door works.

---

## 8. Implementation Phases

### Phase 1: Core (Week 1-2)
- [ ] Lexer + Parser
- [ ] Tree-walking interpreter
- [ ] Basic stdlib

### Phase 2: VFS (Week 3)
- [ ] Virtual filesystem
- [ ] Permission system
- [ ] `chmod`, `ls`, `read`, `write`

### Phase 3: Engine (Week 4-5)
- [ ] Entity system
- [ ] Three.js renderer
- [ ] Hot reload

### Phase 4: VCS (Week 6)
- [ ] Snapshots
- [ ] AST diffing
- [ ] Undo/redo

### Phase 5: Polish (Week 7-8)
- [ ] In-game code editor
- [ ] Error messages
- [ ] Example puzzle game

---

## 9. Gradual Typing (Optional)

Types are optional but useful for tooling, documentation, and catching bugs early.

### 9.1 Basic Types

```slate
let name: String = "Player"
let health: Number = 100
let active: Bool = true

fn add a: Number, b: Number -> Number:
    a + b
```

### 9.2 Record Types

```slate
type Position = {x: Number, y: Number, z: Number}
type Entity = {id: String, pos: Position, health: Number}

let player: Entity = {
    id: "p1"
    pos: {x: 0, y: 0, z: 0}
    health: 100
}
```

### 9.3 Sum Types (Enums)

```slate
type GameState =
    | Menu
    | Playing {level: Number, score: Number}
    | Paused {previous: GameState}
    | GameOver {finalScore: Number}

fn handle state: GameState -> String:
    match state:
        Menu => show_menu()
        Playing {level, score} => run_game(level)
        GameOver {finalScore} => "Game Over: " + finalScore
```

### 9.4 Generics

```slate
type Container[T] = {items: List[T], capacity: Number}

fn push[T] container: Container[T], item: T -> Container[T]:
    container with {items: container.items + [item]}
```

---

## 10. Coroutines

For game logic that spans multiple frames:

### 10.1 Yield

```slate
fn enemy_patrol:
    loop:
        move_to patrol_point_a
        yield                    # Pause until next frame
        wait 2                   # Yield for 2 seconds
        move_to patrol_point_b
        yield
        wait 2

spawn enemy_patrol()
```

### 10.2 Channels

```slate
let messages = Channel()

# Producer
spawn:
    loop:
        emit_to messages, {type: "tick"}
        wait 1

# Consumer
spawn:
    loop:
        let msg = receive messages
        handle msg
```

### 10.3 Integration with Signals

```slate
# Coroutine that reacts to signals
fn door_animation:
    on door.opened:
        for t in 0..1 by 0.1:
            door.rotation.y = lerp(0, 90, t)
            yield
```

---

## 11. Project Structure (Monorepo)

```
oort-engine/
├── packages/
│   ├── core/                 # Shared types, utilities
│   │   └── src/
│   ├── slate/                # Language implementation
│   │   ├── lexer/
│   │   ├── parser/
│   │   ├── interpreter/
│   │   └── stdlib/
│   ├── engine/               # Game engine
│   │   ├── ecs/
│   │   ├── vfs/
│   │   ├── signals/
│   │   └── hot-reload/
│   ├── renderer/             # Three.js rendering
│   │   ├── primitives/
│   │   └── scene/
│   ├── vcs/                  # Version control
│   │   ├── snapshot/
│   │   └── diff/
│   └── editor/               # In-game code editor
│       └── codemirror/
├── games/
│   └── puzzle-demo/          # Example game
├── docs/
└── package.json              # Bun workspace
```

### 11.1 Package Dependencies

```
core ──────────────────────────────────┐
  │                                    │
  ▼                                    ▼
slate ─────► engine ─────► renderer    vcs
                │                       │
                └───────────────────────┘
                            │
                            ▼
                         editor
```

### 11.2 Bun Workspace Config

```json
{
  "name": "oort-engine",
  "workspaces": [
    "packages/*",
    "games/*"
  ]
}
```

---

## 12. Grammar (EBNF)

```ebnf
program     = { statement } ;
statement   = let_stmt | var_stmt | fn_stmt | on_stmt | extend_stmt | expr ;
let_stmt    = "let" IDENT "=" expr ;
var_stmt    = "var" IDENT "=" expr ;
fn_stmt     = "fn" IDENT { IDENT } ":" block ;
on_stmt     = "on" signal ":" block ;
extend_stmt = "extend" "syntax" IDENT { IDENT } ":" block ;
block       = INDENT { statement } DEDENT ;
expr        = if_expr | match_expr | binary | call | literal | IDENT ;
if_expr     = "if" expr ":" block [ "else" ":" block ] ;
match_expr  = "match" expr ":" { pattern "=>" expr } ;
signal      = IDENT { "." IDENT } ;
```

---

This spec is ready to implement. Start with Phase 1?
