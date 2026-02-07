' ****************************************************************************
' PROJECT: Classic Pong Reimplementation (Atari, 1972)
' AUTHOR:  kimpro82 with Google Gemini 3 Flash
' DATE:    2026.02.07
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

' --- UI Alignment Constants ---
CONST TITLE.X = 28
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
DIM SHARED score1, score2 AS INTEGER
DIM SHARED p2IsAI AS INTEGER 
DIM SHARED difficulty AS INTEGER 
DIM SHARED ballSize AS INTEGER: ballSize = 8

' --- Initialization ---
RANDOMIZE TIMER  
SCREEN 12: CLS
OpeningScreen 

' Apply difficulty scaling for initial movement
SELECT CASE difficulty
    CASE 1: ballDX = 5: ballDY = 3
    CASE 2: ballDX = 7: ballDY = 5
    CASE 3: ballDX = 9: ballDY = 7
END SELECT

ballX = 320: ballY = 240
p1Y = 210: p2Y = 210: oldP1Y = 210: oldP2Y = 210
score1 = 0: score2 = 0

CLS
LOCATE 2, 37: PRINT score1; " : "; score2
DrawCenterLine

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
            LOCATE 15, 33: PRINT "PLAYER 1 WINS!"
        ELSE
            LOCATE 15, 33: PRINT "PLAYER 2 WINS!"
        END IF
        LOCATE 18, 30: PRINT "Press Any Key to Exit."
        DO: LOOP UNTIL INKEY$ <> ""
        EXIT DO
    END IF

    ' --- Rendering (Erase & Restore) ---
    LINE (oldBallX, oldBallY)-(oldBallX + ballSize, oldBallY + ballSize), 0, BF
    
    ' Restore persistent UI elements if erased by ball movement
    IF oldBallX > 310 AND oldBallX < 330 THEN DrawCenterLine
    IF oldBallY < 35 THEN LOCATE 2, 37: PRINT score1; " : "; score2

    IF p1Y <> oldP1Y THEN LINE (20, oldP1Y)-(20 + PADDLE.WIDTH, oldP1Y + PADDLE.HEIGHT), 0, BF
    IF p2Y <> oldP2Y THEN LINE (610, oldP2Y)-(610 + PADDLE.WIDTH, oldP2Y + PADDLE.HEIGHT), 0, BF

    ' Draw Current Frame
    LINE (ballX, ballY)-(ballX + ballSize, ballY + ballSize), 15, BF
    DrawPaddle 20, p1Y, 15
    DrawPaddle 610, p2Y, 15

    ' Frame rate control
    FOR delay = 1 TO 2000: NEXT delay 
LOOP UNTIL kbd$ = CHR$(27)
END

' --- Subroutines ---

SUB OpeningScreen ()
    COLOR 15: CLS
    LOCATE TITLE.Y + 0, TITLE.X: PRINT " ____   ___   _   _  ____ "
    LOCATE TITLE.Y + 1, TITLE.X: PRINT "|  _ \ / _ \ | \ | |/ ___|"
    LOCATE TITLE.Y + 2, TITLE.X: PRINT "| |_) | | | ||  \| | |  _ "
    LOCATE TITLE.Y + 3, TITLE.X: PRINT "|  __/| |_| || |\  | |_| |"
    LOCATE TITLE.Y + 4, TITLE.X: PRINT "|_|    \___/ |_| \_|\____|"
    
    COLOR 7
    LOCATE TITLE.Y + 7, INFO.X: PRINT "Reimplementation of Atari's 1972 Pong"
    LOCATE TITLE.Y + 8, INFO.X: PRINT "Written in QuickBASIC with Google Gemini 3 Flash"
    LOCATE TITLE.Y + 10, INFO.X: PRINT "Developer : kimpro82"
    LOCATE TITLE.Y + 11, INFO.X: PRINT "Date      : 2026.02.07"

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

SUB UpdateBall ()
    ballX = ballX + ballDX: ballY = ballY + ballDY
    
    IF ballY <= 0 OR ballY >= SCREEN.HEIGHT - 8 THEN
        ballDY = -ballDY: SOUND 800, 0.1
    END IF
    
    IF ballX < 0 OR ballX > SCREEN.WIDTH THEN
        DIM scorer AS INTEGER
        IF ballX < 0 THEN scorer = 2: score2 = score2 + 1 ELSE scorer = 1: score1 = score1 + 1
        
        LINE (ballX, ballY)-(ballX + 8, ballY + 8), 0, BF
        LOCATE 2, 37: PRINT score1; " : "; score2
        
        IF score1 < WINNING.SCORE AND score2 < WINNING.SCORE THEN
            ' Reset position
            ballX = 320: ballY = 240
            
            ' --- RESET SPEED TO INITIAL VALUES ---
            DIM bDX, bDY AS SINGLE
            SELECT CASE difficulty
                CASE 1: bDX = 5: bDY = 3
                CASE 2: bDX = 7: bDY = 5
                CASE 3: bDX = 9: bDY = 7
            END SELECT
            
            ' Apply reset speeds (acceleration removed)
            IF scorer = 2 THEN ballDX = -bDX ELSE ballDX = bDX
            ballDY = (bDY * 0.7) + (RND * (bDY * 0.6))
            IF RND > 0.5 THEN ballDY = -ballDY

            ' Serve Delay loop
            startTime# = TIMER
            DO
                k$ = INKEY$
                IF k$ = "w" OR k$ = "W" THEN p1Y = p1Y - PADDLE.SPEED
                IF k$ = "s" OR k$ = "S" THEN p1Y = p1Y + PADDLE.SPEED
                IF p2IsAI = 0 THEN
                    IF k$ = CHR$(0) + "H" THEN p2Y = p2Y - PADDLE.SPEED
                    IF k$ = CHR$(0) + "P" THEN p2Y = p2Y + PADDLE.SPEED
                END IF
                
                IF p1Y < 0 THEN p1Y = 0
                IF p1Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p1Y = SCREEN.HEIGHT - PADDLE.HEIGHT
                IF p2Y < 0 THEN p2Y = 0
                IF p2Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p2Y = SCREEN.HEIGHT - PADDLE.HEIGHT

                DrawPaddle 20, p1Y, 15: DrawPaddle 610, p2Y, 15
                LINE (320, 240)-(320 + 8, 240 + 8), 15, BF
                DrawCenterLine: LOCATE 2, 37: PRINT score1; " : "; score2
            LOOP UNTIL TIMER > startTime# + 1.0
            DrawCenterLine
        END IF
    END IF
END SUB

SUB CheckCollisions ()
    ' --- Left Paddle Collision (P1) ---
    IF ballX <= 30 AND ballX >= 20 THEN
        IF ballY + 8 >= p1Y AND ballY <= p1Y + 60 THEN
            ballDX = ABS(ballDX) + ACCEL.RATE
            DIM hitPos1 AS SINGLE
            hitPos1 = (ballY + 4) - (p1Y + 30)
            ballDY = ballDY + (hitPos1 * ANGLE.SENSITIVITY)
            IF ballDY > 12 THEN ballDY = 12
            IF ballDY < -12 THEN ballDY = -12
            SOUND 1200, 0.1
        END IF
    END IF

    ' --- Right Paddle Collision (P2) ---
    IF ballX + 8 >= 610 AND ballX <= 620 THEN
        IF ballY + 8 >= p2Y AND ballY <= p2Y + 60 THEN
            ballDX = -(ABS(ballDX) + ACCEL.RATE)
            DIM hitPos2 AS SINGLE
            hitPos2 = (ballY + 4) - (p2Y + 30)
            ballDY = ballDY + (hitPos2 * ANGLE.SENSITIVITY)
            IF ballDY > 12 THEN ballDY = 12
            IF ballDY < -12 THEN ballDY = -12
            SOUND 1200, 0.1
        END IF
    END IF
END SUB
