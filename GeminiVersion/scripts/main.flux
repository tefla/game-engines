print "Loading 3D Pong..."

var state = "START"
var p1Score = 0
var p2Score = 0
var ballPos = Vec3(0, 0, 0)
var ballVel = Vec3(1, 1, 0)

fun UI(title, block)
    print "=== " + title + " ==="
    block()
    print "==================="

fun Text(content)
    print "  " + content

fun ResetBall()
    ballPos = Vec3(0, 0, 0)

fun UpdatePhysics()
    ballPos = ballPos + ballVel
    
    if ballPos.x > 10
        p1Score = p1Score + 1
        state = "GAME_OVER"
    
    if ballPos.x < -10
        p2Score = p2Score + 1
        state = "GAME_OVER"

fun Tick()
    if state == "START"
        UI "Welcome to Oort Pong"
            Text "Press 'Start' to Play"
            Text "High Score: 999"
        
        state = "PLAY"
        return nil

    if state == "PLAY"
        UpdatePhysics()
        print "Ball: " + ballPos
        print "Score: " + p1Score + " - " + p2Score
        return nil

    if state == "GAME_OVER"
        UI "GAME OVER"
            if p1Score > p2Score
                Text "Player 1 Wins!"
            else
                Text "Player 2 Wins!"
            Text "Reloader to Reset..."
        
        p1Score = 0
        p2Score = 0
        state = "START"
        ResetBall()

Tick()
