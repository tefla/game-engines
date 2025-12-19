# Oort Engine + Slate Language

A game engine featuring the **Slate** scripting language - a Python-inspired DSL designed for game development with signals, reactive programming, and a virtual filesystem.

## Slate Language

Slate is a scripting language designed for game development:

```slate
# Define a player entity
let player = spawn3d({
    id: "player",
    position: vec3(0, 1, 0),
    mesh: "box"
})

# React to signals
on @player.damaged:
    player.health = player.health - data
    if player.health <= 0:
        emit @player.died

# Define behaviors
fn update dt:
    let input = getInput()
    player.position.x = player.position.x + input.x * dt
```

### Key Features

- **Python-like syntax** - Clean, indentation-based
- **Signal system** - `emit @signal` and `on @signal:` for reactive programming
- **Entity system** - Built-in 3D entity spawning and management
- **Vector math** - `vec2`, `vec3`, `vec4` primitives
- **Pattern matching** - `match` expressions
- **Hex colors** - `#FF0000` as first-class values
- **VFS** - Virtual filesystem with permissions (puzzle mechanic)
- **Hot reload** - Edit code while the game runs

## Architecture

```
ClaudeVersion/
├── packages/
│   ├── @oort/core      # AST, tokens, values, errors
│   ├── @oort/slate     # Lexer, parser, interpreter, stdlib
│   ├── @oort/engine    # VFS, signals, runtime, 3D scene
│   ├── @oort/vcs       # Snapshots, diff, undo/redo
│   └── @oort/editor    # Syntax highlighting
├── apps/
│   └── oort-editor     # Electron-based game editor
├── games/
│   └── sample-project  # Example game project
└── docs/               # Documentation
```

## Oort Editor

A modular, extensible game editor built on Electron:

- **Panel system** - Dockable, tabbed panels
- **Project explorer** - File tree with VFS integration
- **Code editor** - Monaco with Slate language support
- **3D Viewport** - Three.js scene with camera controls
- **Console** - REPL and output
- **Play/Pause/Stop** - Game loop controls
- **Dark/Light themes**

## Getting Started

```bash
# Clone the repo
git clone https://github.com/tefla/game-engines.git
cd game-engines/ClaudeVersion

# Install dependencies
bun install

# Run tests
bun test

# Start the editor
cd apps/oort-editor && bun run dev
```

## Documentation

- [Slate Language Spec](./ClaudeVersion/SLATE_SPEC.md) - Full language specification
- [Editor Plan](./ClaudeVersion/EDITOR_PLAN.md) - Oort Editor architecture
- [Macro System Plan](./ClaudeVersion/docs/MACRO_SYSTEM_PLAN.md) - `extend syntax` implementation
- [Alternative Implementations](./ClaudeVersion/docs/ALTERNATIVE_IMPLEMENTATIONS.md) - Ideas from other AI implementations

## License

MIT
