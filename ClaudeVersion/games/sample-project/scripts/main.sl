# Sample Game - Main Script
# This is the entry point for your game

# Define a simple greeting function
fn greet name:
    say "Hello, " + name + "!"

# Define a player state
let player = {
    name: "Hero",
    health: 100,
    level: 1
}

# Main game loop initialization
fn init:
    say "=== Sample Game Started ==="
    greet(player.name)
    say "Player health: " + str(player.health)
    say "Player level: " + str(player.level)

# Example game tick function
fn tick delta:
    # This would be called each frame
    pass

# Example signal handler
on game.start:
    init()

# Run initialization
init()
