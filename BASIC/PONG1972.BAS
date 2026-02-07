' ****************************************************************************
' PROJECT: Classic Pong Reimplementation (Atari, 1972)
' AUTHOR:  kimpro82 with Google Gemini 3 Flash
' DATE:    2026.02.07
' SYSTEM:  QuickBASIC 4.5
' FEATURES: 1P/2P Modes, Difficulty Settings, Atari-style Acceleration,
'           Persistent UI (Line/Score), Randomized Serve, and 1-Sec Delay.
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

' --- Global Variables ---
DIM SHARED ballX, ballY, ballDX, ballDY AS SINGLE
DIM SHARED oldBallX, oldBallY AS SINGLE
DIM SHARED p1Y, p2Y, oldP1Y, oldP2Y AS SINGLE
DIM SHARED score1, score2 AS INTEGER
DIM SHARED p2IsAI AS INTEGER 
DIM SHARED difficulty AS INTEGER 
DIM SHARED ballSize AS INTEGER: ballSize = 8

' --- Initialization ---
RANDOMIZE TIMER   ' ★ 매번 다른 무작위 서브를 위해 추가
SCREEN 12: CLS
OpeningScreen 

' Apply difficulty scaling
SELECT CASE difficulty
    CASE 1: ballDX = 5: ballDY = 4
    CASE 2: ballDX = 8: ballDY = 6
    CASE 3: ballDX = 11: ballDY = 8
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
    IF kbd$ = "w" OR kbd$ = "W" THEN p1Y = p1Y - PADDLE.SPEED
    IF kbd$ = "s" OR kbd$ = "S" THEN p1Y = p1Y + PADDLE.SPEED

    IF p2IsAI = 1 THEN
        aiSpeed = PADDLE.SPEED - (1.5 / difficulty) 
        IF ballY > p2Y + (PADDLE.HEIGHT / 2) THEN p2Y = p2Y + aiSpeed
        IF ballY < p2Y + (PADDLE.HEIGHT / 2) THEN p2Y = p2Y - aiSpeed
    ELSE
        IF kbd$ = CHR$(0) + "H" THEN p2Y = p2Y - PADDLE.SPEED
        IF kbd$ = CHR$(0) + "P" THEN p2Y = p2Y + PADDLE.SPEED
    END IF

    IF p1Y < 0 THEN p1Y = 0
    IF p1Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p1Y = SCREEN.HEIGHT - PADDLE.HEIGHT
    IF p2Y < 0 THEN p2Y = 0
    IF p2Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p2Y = SCREEN.HEIGHT - PADDLE.HEIGHT

    UpdateBall
    CheckCollisions

    ' --- Rendering (Erase & Restore) ---
    LINE (oldBallX, oldBallY)-(oldBallX + ballSize, oldBallY + ballSize), 0, BF
    
    ' ★ 스코어 및 점선 보호 로직 ★
    IF oldBallX > 310 AND oldBallX < 330 THEN DrawCenterLine
    IF oldBallY < 35 THEN LOCATE 2, 37: PRINT score1; " : "; score2

    IF p1Y <> oldP1Y THEN LINE (20, oldP1Y)-(20 + PADDLE.WIDTH, oldP1Y + PADDLE.HEIGHT), 0, BF
    IF p2Y <> oldP2Y THEN LINE (610, oldP2Y)-(610 + PADDLE.WIDTH, oldP2Y + PADDLE.HEIGHT), 0, BF

    ' Draw new positions
    LINE (ballX, ballY)-(ballX + ballSize, ballY + ballSize), 15, BF
    DrawPaddle 20, p1Y, 15
    DrawPaddle 610, p2Y, 15

    FOR delay = 1 TO 1500: NEXT delay 
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

    COLOR 11
    LOCATE TITLE.Y + 14, INFO.X: PRINT "[ CONTROLS ]"
    LOCATE TITLE.Y + 15, INFO.X: PRINT "P1 (Left) : W / S Keys"
    LOCATE TITLE.Y + 16, INFO.X: PRINT "P2 (Right): Up / Down Arrow Keys"

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
    
    ' Boundary detection and scoring
    IF ballX < 0 OR ballX > SCREEN.WIDTH THEN
        ' ★ 득점자 판별 (ballX 초기화 전 수행) ★
        DIM scorer AS INTEGER
        IF ballX < 0 THEN 
            scorer = 2  ' Player 2 scored
            score2 = score2 + 1 
        ELSE 
            scorer = 1  ' Player 1 scored
            score1 = score1 + 1 
        END IF
        
        ' Final erase and UI update
        LINE (ballX, ballY)-(ballX + 8, ballY + 8), 0, BF
        LOCATE 2, 37: PRINT score1; " : "; score2
        
        ' 2. Reset ball to center
        ballX = 320: ballY = 240
        
        ' 3. Reset speed based on difficulty
        SELECT CASE difficulty
            CASE 1: ballDX = 5: ballDY = 4
            CASE 2: ballDX = 8: ballDY = 6
            CASE 3: ballDX = 11: ballDY = 8
        END SELECT
        
        ' ★ 발사 방향 결정 및 무작위성 부여 ★
        ' 득점자가 서브권을 가짐 (상대방 방향으로 발사)
        IF scorer = 2 THEN ballDX = -ABS(ballDX) ELSE ballDX = ABS(ballDX)
        
        ' 상하 각도 무작위 (위 또는 아래)
        IF RND > .5 THEN ballDY = -ballDY

        ' 5. Serve Delay Logic (1 second)
        startTime# = TIMER
        DO
            kbd$ = INKEY$
            IF kbd$ = "w" OR kbd$ = "W" THEN p1Y = p1Y - PADDLE.SPEED
            IF kbd$ = "s" OR kbd$ = "S" THEN p1Y = p1Y + PADDLE.SPEED
            IF p2IsAI = 0 THEN
                IF kbd$ = CHR$(0) + "H" THEN p2Y = p2Y - PADDLE.SPEED
                IF kbd$ = CHR$(0) + "P" THEN p2Y = p2Y + PADDLE.SPEED
            END IF
            
            IF p1Y < 0 THEN p1Y = 0
            IF p1Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p1Y = SCREEN.HEIGHT - PADDLE.HEIGHT
            IF p2Y < 0 THEN p2Y = 0
            IF p2Y > SCREEN.HEIGHT - PADDLE.HEIGHT THEN p2Y = SCREEN.HEIGHT - PADDLE.HEIGHT

            DrawPaddle 20, p1Y, 15
            DrawPaddle 610, p2Y, 15
            LINE (320, 240)-(320 + 8, 240 + 8), 15, BF
            DrawCenterLine
            LOCATE 2, 37: PRINT score1; " : "; score2
        LOOP UNTIL TIMER > startTime# + 1.0
        DrawCenterLine
    END IF
END SUB

SUB CheckCollisions ()
    IF ballX <= 30 AND ballX >= 20 THEN
        IF ballY + 8 >= p1Y AND ballY <= p1Y + 60 THEN
            ballDX = ABS(ballDX) + ACCEL.RATE: SOUND 1200, 0.1
        END IF
    END IF
    IF ballX + 8 >= 610 AND ballX <= 620 THEN
        IF ballY + 8 >= p2Y AND ballY <= p2Y + 60 THEN
            ballDX = -(ABS(ballDX) + ACCEL.RATE): SOUND 1200, 0.1
        END IF
    END IF
END SUB
