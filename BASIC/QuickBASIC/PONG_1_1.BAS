' ****************************************************************************
' PROJECT: Classic Pong Reimplementation (Atari, 1972)
' AUTHOR:  kimpro82 with Google Gemini 3 Flash
' VERSION: 1.1
' DATE:    2026.02.16
' SYSTEM:  QuickBASIC 4.5
' FEATURES: 1P/2P Modes, Difficulty Settings, Speed Acceleration,
'           Flicker-Free Rendering, Variable Angle Serve,
'           and Paddle Segment Physics.
' ****************************************************************************

DECLARE SUB OpeningScreen ()
DECLARE SUB DrawPaddle (x!, y!, c!)
DECLARE SUB UpdateBall ()
DECLARE SUB CheckCollisions ()
DECLARE SUB DrawCenterLine ()
DECLARE SUB DrawScore (scoreVal1 AS INTEGER, scoreVal2 AS INTEGER)

' --- UI Alignment Constants ---
CONST TITLE.X = 2
CONST INFO.X = 18
CONST TITLE.Y = 3

' --- Game Constants ---
CONST SCREEN.WIDTH = 640
CONST SCREEN.HEIGHT = 480
CONST PADDLE.WIDTH = 10
CONST PADDLE.HEIGHT = 60
CONST PADDLE.SPEED = 8
CONST ACCEL.RATE = 0.3
CONST WINNING.SCORE = 11
CONST ANGLE.SENSITIVITY = 0.25

' --- Global Variables ---
DIM SHARED ballX, ballY, ballDX, ballDY AS SINGLE
DIM SHARED oldBallX, oldBallY AS SINGLE
DIM SHARED p1Y, p2Y, oldP1Y, oldP2Y AS SINGLE
DIM SHARED score1 AS INTEGER, score2 AS INTEGER
DIM SHARED p2IsAI AS INTEGER 
DIM SHARED difficulty AS INTEGER 
DIM SHARED ballSize AS INTEGER: ballSize = 8

' --- Initialization ---
RANDOMIZE TIMER  
SCREEN 12: CLS
OpeningScreen 

' Set base speed components
DIM bDX, bDY AS SINGLE
SELECT CASE difficulty
    CASE 1: bDX = 5: bDY = 3
    CASE 2: bDX = 7: bDY = 5
    CASE 3: bDX = 9: bDY = 7
END SELECT

' Initial Game Start: Randomized Serve direction and angle
IF RND > .5 THEN ballDX = bDX ELSE ballDX = -bDX
ballDY = (bDY * .7) + (RND * (bDY * .6))
IF RND > .5 THEN ballDY = -ballDY

ballX = 320: ballY = 240
p1Y = 210: p2Y = 210: oldP1Y = 210: oldP2Y = 210
score1 = 0: score2 = 0

' --- Initial Startup Delay (Ready?) ---
CLS
DrawScore score1, score2
DrawCenterLine
DrawPaddle 20, p1Y, 15
DrawPaddle 610, p2Y, 15
LINE (ballX, ballY)-(ballX + ballSize, ballY + ballSize), 15, BF

startTime# = TIMER
DO
    sOldP1Y = p1Y: sOldP2Y = p2Y
    k$ = INKEY$
    IF k$ = "w" OR k$ = "W" THEN p1Y = p1Y - PADDLE.SPEED
    IF k$ = "s" OR k$ = "S" THEN p1Y = p1Y + PADDLE.SPEED
    IF p2IsAI = 0 THEN
        IF k$ = CHR$(0) + "H" THEN p2Y = p2Y - PADDLE.SPEED
        IF k$ = CHR$(0) + "P" THEN p2Y = p2Y + PADDLE.SPEED
    END IF

    ' Clamp positions
    IF p1Y < 0 THEN p1Y = 0
    IF p1Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p1Y = SCREEN.HEIGHT - PADDLE.HEIGHT
    IF p2Y < 0 THEN p2Y = 0
    IF p2Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p2Y = SCREEN.HEIGHT - PADDLE.HEIGHT

    ' Partial redraw to avoid flicker
    IF p1Y > sOldP1Y THEN LINE (20, sOldP1Y)-(20 + PADDLE.WIDTH, p1Y), 0, BF
    IF p1Y < sOldP1Y THEN LINE (20, p1Y + PADDLE.HEIGHT)-(20 + PADDLE.WIDTH, sOldP1Y + PADDLE.HEIGHT), 0, BF
    IF p2Y > sOldP2Y THEN LINE (610, sOldP2Y)-(610 + PADDLE.WIDTH, p2Y), 0, BF
    IF p2Y < sOldP2Y THEN LINE (610, p2Y + PADDLE.HEIGHT)-(610 + PADDLE.WIDTH, sOldP2Y + PADDLE.HEIGHT), 0, BF

    DrawPaddle 20, p1Y, 15
    DrawPaddle 610, p2Y, 15
    FOR d = 1 TO 500: NEXT d
LOOP UNTIL TIMER > startTime# + 1.5

oldP1Y = p1Y: oldP2Y = p2Y
oldBallX = ballX: oldBallY = ballY

' --- Main Game Loop ---
DO
    oldBallX = ballX: oldBallY = ballY
    oldP1Y = p1Y: oldP2Y = p2Y

    kbd$ = INKEY$
    ' Player 1 Input
    IF kbd$ = "w" OR kbd$ = "W" THEN p1Y = p1Y - PADDLE.SPEED
    IF kbd$ = "s" OR kbd$ = "S" THEN p1Y = p1Y + PADDLE.SPEED

    ' Player 2 / AI Input
    IF p2IsAI = 1 THEN
        aiSpeed = PADDLE.SPEED - (1.5 / difficulty) 
        IF ballY > p2Y + (PADDLE.HEIGHT / 2) THEN p2Y = p2Y + aiSpeed
        IF ballY < p2Y + (PADDLE.HEIGHT / 2) THEN p2Y = p2Y - aiSpeed
    ELSE
        IF kbd$ = CHR$(0) + "H" THEN p2Y = p2Y - PADDLE.SPEED
        IF kbd$ = CHR$(0) + "P" THEN p2Y = p2Y + PADDLE.SPEED
    END IF

    ' Screen Boundary Check for Paddles
    IF p1Y < 0 THEN p1Y = 0
    IF p1Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p1Y = SCREEN.HEIGHT - PADDLE.HEIGHT
    IF p2Y < 0 THEN p2Y = 0
    IF p2Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p2Y = SCREEN.HEIGHT - PADDLE.HEIGHT

    UpdateBall
    CheckCollisions

    ' --- Win Condition Check ---
    IF score1 >= WINNING.SCORE OR score2 >= WINNING.SCORE THEN
        COLOR 15: CLS
        LOCATE 13, 36: PRINT "GAME OVER"
        IF score1 >= WINNING.SCORE THEN
            LOCATE 15, 34: PRINT "PLAYER 1 WINS!"
        ELSE
            LOCATE 15, 34: PRINT "PLAYER 2 WINS!"
        END IF
        
        ' Victory Jingle
        SOUND 523, 1: SOUND 659, 1: SOUND 783, 1: SOUND 1046, 3
        
        WHILE INKEY$ <> "": WEND
        endTime# = TIMER + 1.5
        DO: LOOP UNTIL TIMER > endTime#
        
        LOCATE 18, 28: PRINT "Press Any Key to Exit Game.    "
        DO: LOOP UNTIL INKEY$ <> ""
        EXIT DO
    END IF

    ' --- Rendering ---
    LINE (oldBallX, oldBallY)-(oldBallX + ballSize, oldBallY + ballSize), 0, BF
    IF oldBallX > 310 AND oldBallX < 330 THEN DrawCenterLine
    
    ' Redraw score if ball enters score region to fix trails
    IF oldBallY < 120 THEN DrawScore score1, score2

    ' Partial Redraw Logic for Paddles
    IF p1Y > oldP1Y THEN ' DOWN
        LINE (20, oldP1Y)-(20 + PADDLE.WIDTH, p1Y), 0, BF
    END IF
    IF p1Y < oldP1Y THEN ' UP
        LINE (20, p1Y + PADDLE.HEIGHT)-(20 + PADDLE.WIDTH, oldP1Y + PADDLE.HEIGHT), 0, BF
    END IF

    IF p2Y > oldP2Y THEN ' DOWN
        LINE (610, oldP2Y)-(610 + PADDLE.WIDTH, p2Y), 0, BF
    END IF
    IF p2Y < oldP2Y THEN ' UP
        LINE (610, p2Y + PADDLE.HEIGHT)-(610 + PADDLE.WIDTH, oldP2Y + PADDLE.HEIGHT), 0, BF
    END IF

    LINE (ballX, ballY)-(ballX + ballSize, ballY + ballSize), 15, BF
    DrawPaddle 20, p1Y, 15
    DrawPaddle 610, p2Y, 15

    FOR delay = 1 TO 2000: NEXT delay 
LOOP UNTIL kbd$ = CHR$(27)
END

' --- Subroutines ---

SUB OpeningScreen ()
    COLOR 15: CLS
    LOCATE TITLE.Y + 0, TITLE.X: PRINT " ____   ___  _   _  ____   _____ _   _ _____    ___  ____ ___ ____ ___ _   _ "
    LOCATE TITLE.Y + 1, TITLE.X: PRINT "|  _ \ / _ \| \ | |/ ___| |_   _| | | | ____|  / _ \|  _ \_ _/ ___|_ _| \ | |"
    LOCATE TITLE.Y + 2, TITLE.X: PRINT "| |_) | | | |  \| | |  _    | | | |_| |  _|   | | | | |_) | | |  _ | ||  \| |"
    LOCATE TITLE.Y + 3, TITLE.X: PRINT "|  __/| |_| | |\  | |_| |   | | |  _  | |___  | |_| |  _ <| | |_| || || |\  |"
    LOCATE TITLE.Y + 4, TITLE.X: PRINT "|_|    \___/|_| \_|\____|   |_| |_| |_|_____|  \___/|_| \_\__\____|___|_| \_|"

    COLOR 7
    LOCATE TITLE.Y + 7, INFO.X: PRINT "Reimplementation of Atari's 1972 Pong"
    LOCATE TITLE.Y + 8, INFO.X: PRINT "Written in QuickBASIC with Google Gemini 3 Flash"
    LOCATE TITLE.Y + 10, INFO.X: PRINT "Developer : kimpro82"
    LOCATE TITLE.Y + 11, INFO.X: PRINT "Date      : 2026.02.16"

    COLOR 15: LOCATE TITLE.Y + 13, INFO.X: PRINT "FIRST TO 11 POINTS WINS THE MATCH!"

    COLOR 11
    LOCATE TITLE.Y + 15, INFO.X: PRINT "[ CONTROLS ]"
    LOCATE TITLE.Y + 16, INFO.X: PRINT "P1 (Left) : W / S Keys"
    LOCATE TITLE.Y + 17, INFO.X: PRINT "P2 (Right): Up / Down Arrow Keys"

    COLOR 14
    LOCATE TITLE.Y + 19, INFO.X: PRINT "STEP 1: SELECT MODE"
    LOCATE TITLE.Y + 20, INFO.X: PRINT "1. Single Player (vs AI)"
    LOCATE TITLE.Y + 21, INFO.X: PRINT "2. Two Players"
    DO
        sel$ = INKEY$
        IF sel$ = "1" THEN p2IsAI = 1: EXIT DO
        IF sel$ = "2" THEN p2IsAI = 0: EXIT DO
    LOOP

    COLOR 13
    LOCATE TITLE.Y + 23, INFO.X: PRINT "STEP 2: SELECT DIFFICULTY"
    LOCATE TITLE.Y + 24, INFO.X: PRINT "1. EASY  2. NORMAL  3. HARD"
    DO
        sel$ = INKEY$
        IF sel$ >= "1" AND sel$ <= "3" THEN
            difficulty = VAL(sel$)
            EXIT DO
        END IF
    LOOP
END SUB

SUB DrawPaddle (x AS SINGLE, y AS SINGLE, c AS SINGLE)
    LINE (x, y)-(x + PADDLE.WIDTH, y + PADDLE.HEIGHT), c, BF
END SUB

SUB DrawCenterLine ()
    FOR i = 0 TO SCREEN.HEIGHT STEP 15
        LINE (320, i)-(320, i + 7), 8
    NEXT i
END SUB

SUB DrawScore (scoreVal1 AS INTEGER, scoreVal2 AS INTEGER)
    ' Internal font data (Bitmap character representations)
    DIM f$(11, 4)
    f$(0, 0) = " ### ": f$(0, 1) = "#   #": f$(0, 2) = "#   #": f$(0, 3) = "#   #": f$(0, 4) = " ### "
    f$(1, 0) = "  #  ": f$(1, 1) = " ##  ": f$(1, 2) = "  #  ": f$(1, 3) = "  #  ": f$(1, 4) = " ### "
    f$(2, 0) = "#### ": f$(2, 1) = "    #": f$(2, 2) = " ### ": f$(2, 3) = "#    ": f$(2, 4) = " ####"
    f$(3, 0) = "#### ": f$(3, 1) = "    #": f$(3, 2) = " ### ": f$(3, 3) = "    #": f$(3, 4) = "#### "
    f$(4, 0) = "#   #": f$(4, 1) = "#   #": f$(4, 2) = " ####": f$(4, 3) = "    #": f$(4, 4) = "    #"
    f$(5, 0) = " ####": f$(5, 1) = "#    ": f$(5, 2) = " ### ": f$(5, 3) = "    #": f$(5, 4) = "#### "
    f$(6, 0) = " ####": f$(6, 1) = "#    ": f$(6, 2) = " ####": f$(6, 3) = "#   #": f$(6, 4) = " ### "
    f$(7, 0) = "#####": f$(7, 1) = "    #": f$(7, 2) = "   # ": f$(7, 3) = "  #  ": f$(7, 4) = " #   "
    f$(8, 0) = " ### ": f$(8, 1) = "#   #": f$(8, 2) = " ### ": f$(8, 3) = "#   #": f$(8, 4) = " ### "
    f$(9, 0) = " ### ": f$(9, 1) = "#   #": f$(9, 2) = " ####": f$(9, 3) = "    #": f$(9, 4) = " ### "
    f$(10, 0) = "#  ### ": f$(10, 1) = "# #   #": f$(10, 2) = "# #   #": f$(10, 3) = "# #   #": f$(10, 4) = "#  ### "
    f$(11, 0) = "#  # ": f$(11, 1) = "#  # ": f$(11, 2) = "#  # ": f$(11, 3) = "#  # ": f$(11, 4) = "#  # "

    COLOR 15
    ' Draw Scores with equidistant alignment from the center line (Col 40)
    FOR row = 0 TO 4
        ' P1 Score: Centered around Col 30
        LOCATE 2 + row, 30: PRINT f$(scoreVal1, row)
        ' P2 Score: Centered around Col 47
        LOCATE 2 + row, 47: PRINT f$(scoreVal2, row)
    NEXT row
END SUB

SUB UpdateBall ()
    ballX = ballX + ballDX: ballY = ballY + ballDY
    
    ' Top/Bottom Boundary Reflection with Anti-Sticking Logic
    IF ballY <= 0 OR ballY >= SCREEN.HEIGHT - ballSize THEN
        ballDY = -ballDY
        ' Prevent horizontal sliding
        IF ballDY > 0 THEN ballDY = ballDY + 0.1 ELSE ballDY = ballDY - 0.1
        ' Force back to bounds
        IF ballY <= 0 THEN ballY = 1 ELSE ballY = SCREEN.HEIGHT - ballSize - 1
        SOUND 800, 0.1
    END IF
    
    ' Goal Scoring Detection
    IF ballX < 0 OR ballX > SCREEN.WIDTH THEN
        SOUND 440, 0.5: SOUND 330, 0.8
        DIM scorer AS INTEGER
        IF ballX < 0 THEN scorer = 2 ELSE scorer = 1

        IF scorer = 2 THEN score2 = score2 + 1 ELSE score1 = score1 + 1
        
        IF score1 < WINNING.SCORE AND score2 < WINNING.SCORE THEN
            CLS
            DrawScore score1, score2
            DrawCenterLine
            
            ballX = 320: ballY = 240
            DIM bDX, bDY AS SINGLE
            SELECT CASE difficulty
                CASE 1: bDX = 5: bDY = 3
                CASE 2: bDX = 7: bDY = 5
                CASE 3: bDX = 9: bDY = 7
            END SELECT
            
            IF scorer = 1 THEN ballDX = bDX ELSE ballDX = -bDX
            ballDY = (bDY * 0.7) + (RND * (bDY * 0.6))
            IF RND > 0.5 THEN ballDY = -ballDY

            startTime# = TIMER
            DO
                sOldP1Y = p1Y: sOldP2Y = p2Y
                k$ = INKEY$
                IF k$ = "w" OR k$ = "W" THEN p1Y = p1Y - PADDLE.SPEED
                IF k$ = "s" OR k$ = "S" THEN p1Y = p1Y + PADDLE.SPEED
                IF p2IsAI = 1 THEN
                    IF ballY > p2Y + (PADDLE.HEIGHT / 2) THEN p2Y = p2Y + 1
                    IF ballY < p2Y + (PADDLE.HEIGHT / 2) THEN p2Y = p2Y - 1
                ELSE
                    IF k$ = CHR$(0) + "H" THEN p2Y = p2Y - PADDLE.SPEED
                    IF k$ = CHR$(0) + "P" THEN p2Y = p2Y + PADDLE.SPEED
                END IF
                IF p1Y < 0 THEN p1Y = 0
                IF p1Y > 420 THEN p1Y = 420
                IF p2Y < 0 THEN p2Y = 0
                IF p2Y > 420 THEN p2Y = 420

                IF p1Y > sOldP1Y THEN LINE (20, sOldP1Y)-(20 + PADDLE.WIDTH, p1Y), 0, BF
                IF p1Y < sOldP1Y THEN LINE (20, p1Y + PADDLE.HEIGHT)-(20 + PADDLE.WIDTH, sOldP1Y + PADDLE.HEIGHT), 0, BF
                IF p2Y > sOldP2Y THEN LINE (610, sOldP2Y)-(610 + PADDLE.WIDTH, p2Y), 0, BF
                IF p2Y < sOldP2Y THEN LINE (610, p2Y + PADDLE.HEIGHT)-(610 + PADDLE.WIDTH, sOldP2Y + PADDLE.HEIGHT), 0, BF

                DrawPaddle 20, p1Y, 15
                DrawPaddle 610, p2Y, 15
                LINE (ballX, ballY)-(ballX + ballSize, ballY + ballSize), 15, BF
                FOR d = 1 TO 500: NEXT d
            LOOP UNTIL TIMER > startTime# + 1.2
            
            oldP1Y = p1Y: oldP2Y = p2Y
            oldBallX = ballX: oldBallY = ballY
        END IF
    END IF
END SUB

SUB CheckCollisions ()
    ' P1 Collision
    IF ballX <= 30 AND ballX >= 20 THEN
        IF ballY + ballSize >= p1Y AND ballY <= p1Y + PADDLE.HEIGHT THEN
            ballDX = ABS(ballDX) + ACCEL.RATE
            hitPos1 = (ballY + (ballSize / 2)) - (p1Y + (PADDLE.HEIGHT / 2))
            ballDY = ballDY + (hitPos1 * ANGLE.SENSITIVITY)
            IF ballDY > 12 THEN ballDY = 12
            IF ballDY < -12 THEN ballDY = -12
            SOUND 1200, 0.1
        END IF
    END IF
    ' P2 Collision
    IF ballX + ballSize >= 610 AND ballX <= 620 THEN
        IF ballY + ballSize >= p2Y AND ballY <= p2Y + PADDLE.HEIGHT THEN
            ballDX = -(ABS(ballDX) + ACCEL.RATE)
            hitPos2 = (ballY + (ballSize / 2)) - (p2Y + (PADDLE.HEIGHT / 2))
            ballDY = ballDY + (hitPos2 * ANGLE.SENSITIVITY)
            IF ballDY > 12 THEN ballDY = 12
            IF ballDY < -12 THEN ballDY = -12
            SOUND 1200, 0.1
        END IF
    END IF
END SUB
