# Game Engines

A comparison of game engine implementations featuring the **Slate** scripting language - a Python-inspired DSL designed for game development with signals, reactive programming, and visual scripting support.

## Implementations

| Version | AI | Status | Tests | Description |
|---------|-----|--------|-------|-------------|
| [ClaudeVersion](./ClaudeVersion) | Claude Opus 4.5 | ✅ Active | 319 passing | Full implementation with Electron editor |
| [CodexVersion](./CodexVersion) | OpenAI Codex | 🔨 WIP | - | Alternative implementation |
| [GeminiVersion](./GeminiVersion) | Google Gemini | 🔨 WIP | - | Alternative implementation (Flux lang) |

## Slate Language

Slate is a scripting language designed for game development:

```slate
# Define a player entity
let player = spawn Entity {
    position: vec3(0, 1, 0),
    health: 100
}

# React to signals
on @player.damaged amount:
    player.health = player.health - amount
    if player.health <= 0:
        emit @player.died

# Define behaviors
fn update dt:
    let input = get_input()
    player.position.x = player.position.x + input.x * dt
```

### Key Features

- **Python-like syntax** - Clean, indentation-based
- **Signal system** - `emit @signal` and `on @signal:` for reactive programming
- **Entity system** - Built-in support for game objects
- **Vector math** - `vec2`, `vec3`, `vec4` primitives
- **Pattern matching** - `match` expressions
- **Hex colors** - `#FF0000` as first-class values

## ClaudeVersion Architecture

```
packages/
├── @oort/core      # AST, tokens, values, errors
├── @oort/slate     # Lexer, parser, interpreter, stdlib
├── @oort/engine    # VFS, signals, runtime
├── @oort/vcs       # Snapshots, diff, undo/redo
└── @oort/editor    # Syntax highlighting

apps/
└── oort-editor     # Electron-based game editor

games/
└── puzzle-demo     # Example puzzle game
```

### Oort Editor

A modular, extensible game editor built on Electron:

- **Panel system** - Dockable, tabbed panels
- **Project explorer** - File tree with VFS integration
- **Code editor** - Monaco with Slate language support
- **Console** - REPL and output
- **Inspector** - Property viewer
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

# Start the editor (coming soon)
cd apps/oort-editor && bun run dev
```

## Documentation

- [Slate Language Spec](./ClaudeVersion/SLATE_SPEC.md) - Full language specification
- [Editor Plan](./ClaudeVersion/EDITOR_PLAN.md) - Oort Editor architecture and roadmap

## License

MIT
