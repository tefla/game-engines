# The Lever Sequence Puzzle
# Status: INCOMPLETE - Player needs to implement the logic
# Hint: Pull the levers in the right order: Red, Blue, Green

var lever_state = {
    red: false,
    blue: false,
    green: false
}

var sequence = []
let correct_sequence = ["red", "blue", "green"]

let red_lever = create_entity("red_lever", {
    position: vec3(0, 1, 0),
    color: "#FF0000"
})

let blue_lever = create_entity("blue_lever", {
    position: vec3(2, 1, 0),
    color: "#0000FF"
})

let green_lever = create_entity("green_lever", {
    position: vec3(4, 1, 0),
    color: "#00FF00"
})

fn pull_lever color:
    sequence = push(sequence, color)

    match color:
        "red" => lever_state.red = true
        "blue" => lever_state.blue = true
        "green" => lever_state.green = true
        _ => say "Unknown lever!"

    say "You pull the " + color + " lever."
    check_sequence()

fn check_sequence:
    # TODO: Implement sequence checking!
    #
    # The sequence should match correct_sequence exactly.
    # If it matches, emit @puzzle.solved "lever_sequence"
    # If it's wrong, reset the sequence and all levers.
    #
    # Your code here:
    if length(sequence) == 3:
        # Check if sequence matches...
        say "TODO: Check if sequence is correct!"

fn reset_levers:
    sequence = []
    lever_state.red = false
    lever_state.blue = false
    lever_state.green = false
    say "The levers reset to their original positions."

on @player.interact "red_lever":
    pull_lever("red")

on @player.interact "blue_lever":
    pull_lever("blue")

on @player.interact "green_lever":
    pull_lever("green")
