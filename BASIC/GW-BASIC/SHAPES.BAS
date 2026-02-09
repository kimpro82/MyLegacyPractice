100 REM ===================================================
110 REM Draw a Square, Equilateral Triangle, and Circle
120 REM 2025.06.30.

130 REM [Description]
140 REM - SHAPEHT: Common height for all shapes (in pixels)
150 REM - CX, CY: Screen center coordinates
160 REM - Triangle base length is calculated automatically
170 REM - Circle radius = SHAPEHT / 2
180 REM ===================================================

200 REM ---- Initialize Constants ----
210 ON ERROR GOTO 700
220 SCREEN 1                        ' 320x200 graphics mode
230 SHAPEHT = 80                    ' Common height of all shapes
240 CX = 160 : CY = 100             ' Screen center

300 REM ---- Draw Title ----
310 LOCATE 2, 17 : PRINT "My Shapes"
320 LOCATE 4, 16 : PRINT "2025.06.30."

400 REM ---- Draw Square ----
410 SQSIZE = SHAPEHT
420 SQX = CX - 100 - SQSIZE / 2     ' Square left-top X coordinate
430 LINE (SQX, CY - SQSIZE / 2)-(SQX + SQSIZE, CY + SQSIZE / 2), 1, B

500 REM ---- Draw Equilateral Triangle ----
510 TRIX = CX                       ' Triangle center X coordinate
520 TRIBASE = SHAPEHT * 2 / SQR(3)  ' Triangle base length

530 REM Calculate Triangle Vertices
540 X1 = TRIX - TRIBASE / 2 : Y1 = CY + SHAPEHT / 2
550 X2 = TRIX + TRIBASE / 2 : Y2 = Y1
560 X3 = TRIX : Y3 = CY - SHAPEHT / 2

570 LINE (X1, Y1)-(X2, Y2), 2       ' Draw triangle base
580 LINE (X2, Y2)-(X3, Y3), 2       ' Draw triangle right side
590 LINE (X3, Y3)-(X1, Y1), 2       ' Draw triangle left side

600 REM ---- Draw Circle ----
610 CIRX = CX + 100                             ' Circle center X coordinate
620 CIRCLE (CIRX, CY), SHAPEHT / 2 * 1.15, 3    ' 1.15 : Magic Number!

700 REM ---- Wait for Key Press ----
710 LOCATE 24, 1 : PRINT "Press any key...";
720 IF INKEY$ = "" THEN GOTO 720
730 SCREEN 0
740 WIDTH 80

800 END
