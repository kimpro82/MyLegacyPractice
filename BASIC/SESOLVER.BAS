10  ' Subject : Simultaneous Equations Solver
20  ' Author  : kimpro82
30  ' Date    : 2024.08.20
40  '
50  ' Program Description: This program solves a system of linear equations:
60  ' ax + by = c
70  ' dx + ey = f
80  ' The user inputs the coefficients a, b, c, d, e, f. If no input is provided, random values are generated.
90  ' The solution for x and y is calculated and displayed. Enter Q or q to quit.

100 ' Initialize and get input data
110 DIM COEF#(5) ' Array to store coefficients
120 PRINT "The system of equations is:"
130 PRINT "  ax + by = c"
140 PRINT "  dx + ey = f"

150 PRINT "Enter coefficients a, b, c, d, e, f (separated by spaces), or Q/q to quit:"
160 INPUT INPUTDATA$ ' Get user input

200 ' Check for quit command or process input
210 IF INPUTDATA$ = "Q" OR INPUTDATA$ = "q" THEN GOTO 900
220 ' GOSUB 800 ' Initialize variables; Not use
230 GOSUB 300 ' Process input or generate random values
240 GOSUB 500 ' Solve the equations
250 GOTO 150

300 ' Parse input or use default values
310 INPUTDATA$ = INPUTDATA$ + " 0 0 0 0 0 0"  ' Pad the input with zeros if fewer than 6 values are provided
320 INDEX = 1
330 FOR I = 0 TO 5
340   WORD$ = ""
350   CONDITION = 1
360   WHILE CONDITION
370     CHAR$ = MID$(INPUTDATA$, INDEX, 1)
380     IF CHAR$ <> " " THEN WORD$ = WORD$ + CHAR$ ELSE CONDITION = 0
390     ' PRINT "I:"; I; "/ INDEX:"; INDEX; "/ CHAR:"; CHAR$; " / WORD:"; WORD$; " / CONDITION:"; CONDITION ' Debug print
400     INDEX = INDEX + 1
410   WEND
420   COEF#(I) = VAL(WORD$)
430 NEXT I
450 RETURN

500 ' Solve the system of linear equations
510 A = COEF#(0): B = COEF#(1): C = COEF#(2)
520 D = COEF#(3): E = COEF#(4): F = COEF#(5)
530 PRINT "The equations you entered are:"
540 PRINT "  "; A; "x +"; B; "y ="; C
550 PRINT "  "; D; "x +"; E; "y ="; F
560 DETERMINANT = A * E - B * D

600 IF DETERMINANT = 0 THEN PRINT "The system has no unique solution."
610 IF DETERMINANT <> 0 THEN X = (C * E - B * F) / DETERMINANT
620 IF DETERMINANT <> 0 THEN Y = (A * F - C * D) / DETERMINANT
630 IF DETERMINANT <> 0 THEN PRINT "Solution: x = "; X; ", y = "; Y
640 ' It is crazy to handle IF conditions in GW-BASIC!
650 RETURN

800 ' Initialize variables; Not use
810 ' FOR I = 0 TO 5
820 '   COEF#(I) = 0 ' Set all coefficients to 0
830 ' NEXT I
840 ' RETURN

900 ' Quit the program
910 PRINT "Program terminated."
920 END
