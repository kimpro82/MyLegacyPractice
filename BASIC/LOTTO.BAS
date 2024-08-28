10  ' Subject : Lotto Number Generator
20  ' Author  : kimpro82
30  ' Date    : 2024.08.28
40  '
50  ' Features
60  ' - Generates n sets of 7 unique numbers between 1 and 45
70  ' - If user inputs numbers, those are fixed and others are generated
80  ' - Displays the numbers in the format: a b c d e f + lucky number
90  ' - Validates user input and handles errors

100 SETSNUM = 4 ' Constant for the number of sets to generate
110 RANDOMIZE TIMER ' Initialize random number generator

120 PRINT "Press ENTER or input your fixed numbers (space-separated):"
130 INPUT USERSTR$ ' Get user input

140 DIM NUMSET(7) ' Declare array to hold one set of numbers
150 ' Parse user input and validate
160 IF USERSTR$ = "" THEN GOTO 200 ' No input, generate all random
170 GOSUB 500 ' Validate and parse user input
180 ' If invalid input, re-prompt user
190 IF ERRORFLAG = 1 THEN PRINT "Invalid input. Please try again." : GOTO 110

200 ' Start generating sets of numbers
210 FOR I = 1 TO SETSNUM ' Generate sets based on the constant
220   GOSUB 900 ' Initialize array for each set
230   COUNT = 0 ' Reset count for each set
240   ' Fill array with user-provided numbers
250   FOR J = 1 TO USERINDEX
260     NUMSET(J) = USERNUMS(J)
270     COUNT = COUNT + 1
280   NEXT J
290   ' Generate remaining random numbers
300   WHILE COUNT < 7 ' Generate only 6 random numbers
310     NEWNUM = INT(RND * 45) + 1
320     GOSUB 700 ' Check if number is already in the set
330     IF INSET = -1 THEN GOTO 310 ' If number is already in the set, generate again
340     COUNT = COUNT + 1
350     NUMSET(COUNT) = NEWNUM
360   WEND
370   ' Sort only the numbers from position USERINDEX + 1 to 6, leaving the last number (NUMSET(7)) unsorted
380   GOSUB 800 ' Sort the numbers from position USERINDEX + 1 to 6
390   ' Display the result with numbers formatted to two characters each
400   PRINT USING "## ## ## ## ## ## + ##"; NUMSET(1); NUMSET(2); NUMSET(3); NUMSET(4); NUMSET(5); NUMSET(6); NUMSET(7)
410 NEXT I
420 END

500 ' Subroutine to validate and parse user input
510 DIM USERNUMS(6) ' Maximum 6 numbers can be fixed
520 USERINDEX = 0
530 ERRORFLAG = 0
540 ' Split user input by spaces
550 TMP$ = "" ' Initialize temporary string
560 FOR K = 1 TO LEN(USERSTR$)
570   CHAR$ = MID$(USERSTR$, K, 1)
580   IF CHAR$ >= "0" AND CHAR$ <= "9" THEN TMP$ = TMP$ + CHAR$
590   IF CHAR$ <> " " AND K < LEN(USERSTR$) THEN GOTO 680
600   IF LEN(TMP$) = 0 THEN GOTO 680
610   VALNUM = VAL(TMP$)
620   IF VALNUM < 1 OR VALNUM > 45 THEN ERRORFLAG = 1 : RETURN
630   GOSUB 700 ' Check if number is already in the user input set
640   IF INSET = -1 THEN ERRORFLAG = 1 : RETURN
650   USERINDEX = USERINDEX + 1
660   USERNUMS(USERINDEX) = VALNUM
670   TMP$ = ""
680 NEXT K
690 RETURN

700 ' Subroutine to check if a number is in the set
710 INSET = 0
720 FOR L = 1 TO COUNT
730   IF NUMSET(L) = NEWNUM THEN INSET = -1 : RETURN
740 NEXT L
750 RETURN

800 ' Subroutine to sort the numbers from position USERINDEX + 1 to 6
810 START = USERINDEX + 1
820 ENDIDX = 6 ' Sort up to NUMSET(6), excluding NUMSET(7)
830 FOR M = START TO ENDIDX - 1
840   FOR N = M + 1 TO ENDIDX
850     IF NUMSET(M) > NUMSET(N) THEN SWAP NUMSET(M), NUMSET(N)
860   NEXT N
870 NEXT M
880 RETURN

900 ' Subroutine to initialize the NUMSET array
910 FOR T = 1 TO 7
920   NUMSET(T) = 0
930 NEXT T
940 RETURN
