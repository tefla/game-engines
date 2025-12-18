# The Broken Door Puzzle
# Status: BROKEN - Player needs to fix this!
# Hint: The door won't open even when you have the key...

let door = create_entity("door", {
    position: vec3(5, 0, 0),
    locked: true,
    color: "#8B4513"
})

# BUG: The logic is inverted! The door should open when
# the player HAS the key, not when they DON'T have it.
#
# Can you spot the bug and fix it?

on @player.interact "door":
    if not player_has("gold_key"):  # <- Something is wrong here!
        door.props.locked = false
        door.props.color = "#00FF00"
        emit @door.opened
        say "The door swings open!"
    else:
        say "The door won't budge..."

# This handler is correct - don't change it
on @door.opened:
    notify "You unlocked the door! Proceed to the next room."
    emit @puzzle.solved "broken_door"
