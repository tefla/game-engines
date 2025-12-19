# Utility functions for Sample Game

# Clamp a value between min and max
fn clamp value min max:
    if value < min:
        return min
    if value > max:
        return max
    return value

# Linear interpolation
fn lerp a b t:
    return a + (b - a) * t

# Random integer between min and max (inclusive)
fn randint min max:
    return floor(random() * (max - min + 1)) + min

# Format a number with leading zeros
fn pad num width:
    let s = str(num)
    while len(s) < width:
        s = "0" + s
    return s
