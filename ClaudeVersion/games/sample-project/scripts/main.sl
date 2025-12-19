# Sample Game - Main Script
# Demonstrates game loop, input handling, and 3D entity control

# === Game State ===
let speed = 5.0
let rotationSpeed = 90.0
let player = null
let gameTime = 0

# === Initialization ===
fn init:
    say "=== Oort Sample Game ==="
    say "Use WASD to move, Q/E to rotate"
    say "Press Play to start the game loop!"

    # Spawn the player entity
    player = spawn3d({
        id: "player",
        name: "Player Cube",
        position: vec3(0, 0.5, 0)
    })

# === Game Loop Handlers ===

# Called when the game starts (Play button pressed)
on game.start:
    say "[Game Started]"
    gameTime = 0

# Called when the game stops
on game.stop:
    say "[Game Stopped] - Total time: " + str(round(gameTime, 1)) + "s"

# Called when the game is paused
on game.pause:
    say "[Game Paused]"

# Called when the game resumes
on game.resume:
    say "[Game Resumed]"

# Called every frame while the game is running
on game.tick:
    let delta = data.delta
    gameTime = data.time

# === Input Handlers ===

# Keyboard input for movement
on input.key.down:
    if player == null:
        return

    let key = data.key
    let delta = 0.016  # Approximate frame time

    # WASD movement
    if key == "w" or key == "W":
        translate(player, vec3(0, 0, -speed * delta))
    else if key == "s" or key == "S":
        translate(player, vec3(0, 0, speed * delta))
    else if key == "a" or key == "A":
        translate(player, vec3(-speed * delta, 0, 0))
    else if key == "d" or key == "D":
        translate(player, vec3(speed * delta, 0, 0))

    # Q/E rotation
    else if key == "q" or key == "Q":
        rotate(player, vec3(0, rotationSpeed * delta, 0))
    else if key == "e" or key == "E":
        rotate(player, vec3(0, -rotationSpeed * delta, 0))

    # Space to jump
    else if key == " ":
        let pos = getPosition(player)
        if pos.y < 1:
            setPosition(player, vec3(pos.x, 2, pos.z))

# === Run Initialization ===
init()
