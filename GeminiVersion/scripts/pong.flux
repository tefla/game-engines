print "Loading 3D Pong..."

// 1. State
var state = "START" // START, PLAY, GAME_OVER
var p1Score = 0
var p2Score = 0
var ballPos = Vec3(0, 0, 0)
var ballVel = Vec3(1, 1, 0)

// 2. UI DSL (Command Syntax!)
fun UI(title, block) {
    print ""
    print "=== " + title + " ==="
    block()
    print "==================="
}

fun Text(content) {
    print "  " + content
}

// 3. Logic
fun ResetBall() {
    ballPos = Vec3(0, 0, 0)
    // Randomize?
}

fun UpdatePhysics() {
    ballPos = ballPos + ballVel
    
    // Bounds Check (Mock)
    if ballPos.x > 10 do
        p1Score = p1Score + 1
        state = "GAME_OVER"
    end
    
    if ballPos.x < -10 do
        p2Score = p2Score + 1
        state = "GAME_OVER"
    end
}

// 4. Main Loop (Simulated for 1 frame)
// using 'do ... end' syntax
fun Tick() {
    if state == "START" do
        UI "Welcome to Oort Pong" do
            Text "Press 'Start' to Play"
            Text "High Score: 999"
        end
        state = "PLAY"
        return nil
    end

    if state == "PLAY" do
        UpdatePhysics()
        print "Ball: " + ballPos
        print "Score: " + p1Score + " - " + p2Score
        return nil
    end

    if state == "GAME_OVER" do
        UI "GAME OVER" do
            if p1Score > p2Score do
                Text "Player 1 Wins!"
            end
            else do
                Text "Player 2 Wins!"
            end
            Text "Reloader to Reset..."
        end
        // Reset for next tick demo
        p1Score = 0
        p2Score = 0
        state = "START"
        ResetBall()
    end
}

// Run one Tick
Tick()
