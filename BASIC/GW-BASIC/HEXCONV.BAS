0 CLS

10 PRINT "*************************************"
20 PRINT "*  <Decimal/Hexadecimal Convertor>  *"
30 PRINT "*  kimpro82 / 2023.12.10, not 1993  *"
40 PRINT "* * * * * * * * * * * * * * * * * * *"
50 PRINT "* 1. Convert Decimal to Hexadecimal *"
60 PRINT "* 2. Convert Hexadecimal to Decimal *"
70 PRINT "* 3. Exit                           *"
80 PRINT "*************************************"
90 INPUT " Select a menu (1, 2, 3): "; MENU

100 IF MENU = 1 THEN GOSUB 200
110 IF MENU = 2 THEN GOSUB 300
120 IF MENU = 3 THEN END
130 GOTO 90

200 ' Subroutine for Decimal to Hexadecimal Conversion
210 INPUT " Enter a decimal value: "; DEC
220 PRINT " Hexadecimal value    : "; HEX$(DEC)
230 RETURN

300 ' Subroutine for Hexadecimal to Decimal Conversion
310 INPUT " Enter a hexadecimal value: "; HEXA$
315     ' The keyword HEX$(X) is already in use
320 PRINT " Decimal value            : "; VAL("&H" + HEXA$)
330 RETURN
