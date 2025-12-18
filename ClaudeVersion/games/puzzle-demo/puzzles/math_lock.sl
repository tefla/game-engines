# The Math Lock Puzzle
# Status: WORKING (but has a bug!)
# Hint: The combination is 7, but the lock accepts wrong answers...

let lock = create_entity("math_lock", {
    position: vec3(10, 1, 0),
    locked: true,
    combination: 7
})

# This function checks if the answer is correct
# BUG: The comparison operator is wrong!
fn check_answer answer:
    # The correct answer should be 7 (3 + 4)
    # But this uses the wrong operator...
    if answer = 7:   # <- This is assignment, not comparison!
        say "Correct! The lock clicks open."
        lock.props.locked = false
        emit @puzzle.solved "math_lock"
    else:
        say "Wrong answer. Try again!"

# Riddle: "I am 3 plus 4. What number am I?"
on @player.interact "math_lock":
    say "A riddle is inscribed on the lock:"
    say "\"I am 3 plus 4. What number am I?\""
    say "Type your answer by calling: solve_math(number)"

fn solve_math answer:
    check_answer(answer)
