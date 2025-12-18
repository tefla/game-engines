# Player's Notes
# This file is your scratchpad - write anything here!
# Use it to test code, take notes, or experiment.

# ===== MY NOTES =====

# Puzzle 1: The Broken Door
# - Bug: The condition is inverted
# - Fix: Remove "not" from the condition
# - Status: TODO

# Puzzle 2: The Lever Sequence
# - Correct order: Red, Blue, Green
# - Need to implement check_sequence function
# - Status: TODO

# Puzzle 3: The Math Lock
# - Answer is 7
# - Bug: Uses = instead of ==
# - Status: TODO

# ===== TEST CODE =====

# You can test your ideas here!

fn test_idea:
    say "Testing my idea..."
    let result = 3 + 4
    say "3 + 4 = " + result

# Uncomment to run:
# test_idea()

# ===== USEFUL FUNCTIONS =====

# Check what items you have
fn check_inventory:
    let player = get_player()
    say "Inventory: " + player.inventory

# Get your current position
fn where_am_i:
    let player = get_player()
    say "Position: " + player.pos
