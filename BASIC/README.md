# [My Basic Practice](../README.md#basic)

My Nostalgia; codes for **the old BASIC product family** (*GW-BASIC*, *QuickBASIC* and so on) before [*Visual Basic*](https://github.com/kimpro82/MyPractice/blob/master/VBA)


### \<List>

- [Decimal/Hexadecimal Convertor (2023.12.10)](#decimalhexadecimal-convertor-20231210)
- [Line Numbering 2 (2023.07.22)](#line-numbering-2-20230722)
- [Line Numbering (2023.07.19)](#line-numbering-20230719)
- [Draw A Car (2022.02.09)](#draw-a-car-20220209)
- [Play Music (2021.02.20)](#play-music-20210220)
- [Hello World (2020.02.27)](#hello-world-20200227)


## [Decimal/Hexadecimal Convertor (2023.12.10)](#list)

- To practice Subroutine with `GOSUB` ~ `RETURN` statements
- Be aware of *BOM*(Byte-Order Mark; `EF BB BF`) signature generation when creating a code file with an external editor

  ![Decimal/Hexadecimal Convertor](./Images/GW-BASIC_HEXCONV_RUN.PNG)

  <details>
    <summary>Codes : HEXCONV.BAS</summary>

    ```bas
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
    ```
  </details>


## [Line Numbering 2 (2023.07.22)](#list)

- Finally I've got [GW-BASIC 3.23](https://web.archive.org/web/20091027112638/http://geocities.com/KindlyRat/GWBASIC.html)!
- Reversed line numbers are automatically rearranged as increasing order
- Line numbers are allowed only when they are not exceeding `65529` and without decimal points.

  <details>
    <summary>Why `65529`, not `65535`?</summary>

  - [Variances in Basic highest line numbers](https://retrocomputing.stackexchange.com/questions/13347/variances-in-basic-highest-line-numbers)  [(retrocomputing.stackexchange.com)](https://retrocomputing.stackexchange.com/)

    > Line numbers are stored as a two byte word but the largest allowed by the input routines is 65529. Primarily because this is an easier limit to test rather than checking for overflow. The line number is converted from ASCII to binary a character at a time using a pretty standard algorithm. Start with a 16 bit value `line` = 0. For each digit multiply `line` by 10 and add the digit to `line`.  
    >  
    >  To check if the line number is acceptable, compare `line` against 6552 before multiplying it by 10. This will guarantee the value is <= 65529 because a digit can add only 9 at most.
  </details>
  <details>
    <summary>What does `!` mean?</summary>

  - [Microsoft > Learn > Documentation > .NET > Visual Basic](https://learn.microsoft.com/en-us/dotnet/visual-basic/) > [Single Data Type](https://learn.microsoft.com/en-us/dotnet/visual-basic/language-reference/data-types/single-data-type)

    This is not exactly GW-BASIC, but it is a descendant with some traces of its syntax.

    > Type Characters. Appending the literal type character `F` to a literal forces it to the `Single` data type. Appending the identifier type character `!` to any identifier forces it to `Single`.
  </details>

  <details open="">
    <summary>Codes : LINENUM2.BAS</summary>

  - Saved in the file
    ```bas
    10 PRINT 10
    20 PRINT 20
    30 GOTO 50
    40 PRINT 40         ' Pass
    50 PRINT 50
    60 PRINT 60
    55 PRINT 55         ' Rearranged as increasing order
    70.5 PRINT 70.5     ' Syntax rrror; 70 .5 PRINT 70.5
    65530 PRINT 65530   ' Syntax rrror; 6553 0 PRINT 65530!
    65529 PRINT 65529   ' 65529!
    65531 PRINT 65531   ' Syntax rrror
    ```
  - Loaded on the GW-BASIC console
    ```bas
    10 PRINT 10
    20 PRINT 20
    30 GOTO 50
    40 PRINT 40             ' Pass
    50 PRINT 50
    55 PRINT 55             ' Rearranged as increasing order
    60 PRINT 60
    70 .5 PRINT 70.5                ' Syntax rrror; 70 .5 PRINT 70.5
    6553 0 PRINT 65530!     ' Syntax rrror; 6553 0 PRINT 65530!
    65529 PRINT 65529!      ' 65529!
    Syntax error
    ```
  - Output
    ```txt
    RUN
    10
    20
    50
    55
    60
    Syntax error in 70
    Ok
    ```
  </details>

## [Line Numbering (2023.07.19)](#list)

- I intended to write *GW-BASIC* code, but I actually executed it in *QuickBASIC*.
- Unexpectedly, many things are allowed, including aspects that were not even considered.  
  Ex) Line numbers including decimal points, reversed, or exceeding 65535 ……

  <details open="">
    <summary>Codes : LineNum.bas</summary>

  ```bas
  '-10 print "-10"        ' A negative line number causes an error
  0 CLS
  10 PRINT "10"
  15 GOTO 30
  20 PRINT "20"           ' Pass
  30 PRINT "30"
  25 PRINT "25"           ' Decreasing numbering is OK
  30.5 PRINT "30.5"       ' Decimal point is allowed
  A: GOTO C
  B: PRINT "B"
  C: PRINT "C"            ' Alphanumeric line labels can be mixed
  PRINT "No label"        ' Lines without labeling is also available
  65535 PRINT "65535"
  65536 PRINT "65536"     ' The line number can be over 65536
  ```
  ```txt
  10
  30
  25
  30.5
  C
  No label
  65535
  65536
  ```
  </details>

- References
  - *The BASIC Program Line* from [QUICKBASIC 4.5 Help file](https://hwiegman.home.xs4all.nl/qb45-man/index.html)
  - [Q73084: Differences Between GW-BASIC and QBasic](https://jeffpar.github.io/kbarchive/kb/073/Q73084/)


## [Draw A Car (2022.02.09)](#list)

- Remember how I felt when I was a primary school student

  ![Draw A Car](Images/QB_DrawingCar.PNG)

  <details>
    <summary>Codes : DrawCar.bas</summary>

  ```bas
  CLS

  SCREEN 12       '640 x 480 / 16 colors
  wid% = 640      'Can I get these parameters automatically?
  hei% = 480

  'Border
  LINE (10, 10)-(wid% - 10, 10), 15, B
  LINE (10, hei% - 80)-(wid% - 10, hei% - 80), 15, B
  LINE (10, 10)-(10, hei% - 80), 15, B
  LINE (wid% - 10, 10)-(wid% - 10, hei% - 80), 15, B

  'Memo
  LOCATE 3, 5
  PRINT "QuickBasic : My Nostalgia"
  LOCATE 3, 67
  PRINT "2022.02.09"

  'Body
  LINE (wid% / 2 - 100, hei% / 2 - 100)-(wid% / 2 + 100, hei% / 2), 7, BF
  LINE (wid% / 2 - 200, hei% / 2)-(wid% / 2 + 200, hei% / 2 + 100), 7, BF

  'Windows
  LINE (wid% / 2 - 100, hei% / 2 - 80)-(wid% / 2 - 60, hei% / 2), 9, BF
  LINE (wid% / 2 - 50, hei% / 2 - 80)-(wid% / 2 - 5, hei% / 2), 9, BF
  LINE (wid% / 2 + 5, hei% / 2 - 80)-(wid% / 2 + 50, hei% / 2), 9, BF
  LINE (wid% / 2 + 60, hei% / 2 - 80)-(wid% / 2 + 100, hei% / 2), 9, BF

  'Wheels
  CIRCLE (wid% / 2 - 90, hei% / 2 + 100), 50, 8
  CIRCLE (wid% / 2 + 90, hei% / 2 + 100), 50, 8
  PAINT (wid% / 2 - 120, hei% / 2 + 100), 8, 8
  PAINT (wid% / 2 + 120, hei% / 2 + 100), 8, 8
  CIRCLE (wid% / 2 - 90, hei% / 2 + 100), 30, 7
  CIRCLE (wid% / 2 + 90, hei% / 2 + 100), 30, 7
  PAINT (wid% / 2 - 90, hei% / 2 + 100), 7, 7
  PAINT (wid% / 2 + 90, hei% / 2 + 100), 7, 7

  END
  ```
  </details>


## [Play Music (2021.02.20)](#list)

- Practice of functions : `BEEP` `SOUND` `PLAY`
- Run by *MS QuickBASIC 4.5*

  <details>
    <summary>Codes : Xerxes.bas</summary>

  - Using `SHELL` function to borrow the `CLS` command from DOS
  ```bas
  SHELL "CLS"
  PRINT "I am generous"
  ```
  > I am generous
  </details>
  <details>
    <summary>Codes : Sound.bas</summary>

  - Refer to ☞ https://en.wikibooks.org/wiki/QBasic/Sound
  ```bas
  SHELL "CLS"

  'BEEP
  PRINT "BEEP"
  BEEP
  PRINT CHR$(7)
  SLEEP

  'SOUND
  PRINT "SOUND" + CHR$(13) 'CHR$(13) : Line break
  FOR i% = 1 TO 30
          SOUND i% * 100, 1  'Frequency, Duration
  NEXT
  SLEEP

  'PLAY
  PRINT "PLAY" + CHR$(13)
  PLAY "L16 CDEFGAB>C" '> : Move up one octave
  SLEEP
  ```
  </details>

  - **Results** : [BEEP](./Sounds/QB_SOUND_BEEP.wav) [SOUND](./Sounds/QB_SOUND_SOUND.wav) [PLAY](./Sounds/QB_SOUND_PLAY.wav)  
  (* These can't be played directly, but played after downloading.)

  <details open="">
    <summary>Codes : SchoolBell.bas</summary>

  - Play the same song with the keys of both C major and C minor
  ```bas
  SHELL "CLS"

  PRINT "School Bell"

  PRINT "C major"
  PLAY "MS G8G8A8A8 G8G8E4 G8G8E8E8 D6 P8"
  PLAY "MS G8G8A8A8 G8G8E4 G8E8D8E8 C6 P8"

  PRINT "C minor"
  PLAY "MS G8G8A-8A-8 G8G8E-4 G8G8E-8E-8 D6 P8"
  PLAY "MS G8G8A-8A-8 G8G8E-4 G8E-8D8E-8 C6 P8"
  ```
  </details>

  - **Results** : [C major](./Sounds/QB_PLAY_C%20major.wav) [C minor](./Sounds/QB_PLAY_C%20minor.wav)  
  (* These can't be played directly, but played after downloading.)


## [Hello World (2020.02.27)](#list)

- `PRINT`, not `print`

  <details>
    <summary>Codes : HelloWorld.bas</summary>

  ```bas
  print "Hello World!"
  ```
  > Call to undefined sub 'print'

  ```bas
  print("Hello World!")
  ```
  > Call to undefined sub 'print'

  ```bas
  print 'Hello World!'
  ```
  > Call to undefined sub 'print'

  How can I make `print` work?

  ```bas
  PRINT "Hello World!"
  ```
  > Hello World!

  The secret was UPPER CASE!

  ```bas
  PRINT 'Hello World!'
  ```
  >
  `''` seems to be used for single-line comments.

  ```bas
  'You can't see what I'm saying.'
  ```
  ㅋ
  </details>