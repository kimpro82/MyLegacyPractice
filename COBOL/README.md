# [My COBOL Practice](../README.md#cobol)

COBOL never dies


### \<List>

- [Multiplication Table (2024.10.25)](#multiplication-table-20241025)
- [Hello World (2023.09.04)](#hello-world-20230904)


## [Multiplication Table (2024.10.25)](#list)

- Run with GNU COBOL 3.2.0 in [JDoodle](https://www.jdoodle.com/execute-cobol-online)
- Code and Output
  <details>
    <summary>Code : MultiplicationTable.cob</summary>

    ```cobol
    IDENTIFICATION DIVISION.
        PROGRAM-ID.     MULTIPLICATION-TABLE.
        AUTHOR.         kimpro82.
        DATE-WRITTEN.   2024-10-25.
        *> Compiled with GNU COBOL 3.2.0

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 I        PIC 99.    *> Using 99 instead of 9 prevents unexpected results
        01 J        PIC 99.    *> PIC 99 ensures correct handling in PERFORM statements
        01 K        PIC 99.
        01 I-Z9     PIC Z9.
        01 J-Z9     PIC Z9.
        01 RESULT   PIC Z9.

    PROCEDURE DIVISION.
    MAIN-PROCEDURE.
        PERFORM VARYING K FROM 1 BY 3 UNTIL K > 7
            PERFORM VARYING J FROM 1 BY 1 UNTIL J > 9
                MOVE J TO J-Z9
                PERFORM VARYING I FROM K BY 1 UNTIL I > K + 2
                    COMPUTE RESULT = I * J
                    MOVE I TO I-Z9
                    DISPLAY I-Z9 " *" J-Z9 " = " RESULT "    "
                        WITH NO ADVANCING
                END-PERFORM
                DISPLAY SPACE
            END-PERFORM
            DISPLAY SPACE
        END-PERFORM
        STOP RUN.
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```txt
     1 * 1 =  1     2 * 1 =  2     3 * 1 =  3     
     1 * 2 =  2     2 * 2 =  4     3 * 2 =  6     
     1 * 3 =  3     2 * 3 =  6     3 * 3 =  9     
     ……   
     1 * 9 =  9     2 * 9 = 18     3 * 9 = 27     
     
     4 * 1 =  4     5 * 1 =  5     6 * 1 =  6     
     4 * 2 =  8     5 * 2 = 10     6 * 2 = 12     
     4 * 3 = 12     5 * 3 = 15     6 * 3 = 18     
     ……
     4 * 9 = 36     5 * 9 = 45     6 * 9 = 54     
     
     7 * 1 =  7     8 * 1 =  8     9 * 1 =  9     
     7 * 2 = 14     8 * 2 = 16     9 * 2 = 18     
     7 * 3 = 21     8 * 3 = 24     9 * 3 = 27     
     ……
     7 * 9 = 63     8 * 9 = 72     9 * 9 = 81     
    ```
  </details>


## [Hello World (2023.09.04)](#list)

- COBOL requires crazy indentation rules  
  (Reference ☞ [[Wikipedia] COBOL > Features > Code format](https://en.wikipedia.org/wiki/COBOL#Code_format))
  - Comments must start with `*` in column 7
  - DIVISION, SECTION headers must start from column 8
  - Do not exceed column 80
- Compiled by *cobc (GnuCOBOL) 3.1.2.0* in [replit](https://replit.com/)
- Codes and Output
  <details open="">
    <summary>Codes : HelloWorld.cob</summary>

  ```cobol
       * Hello World in COBOL
       * 2023.09.04

        IDENTIFICATION DIVISION.
            PROGRAM-ID. HelloWorld.

        DATA DIVISION.
            WORKING-STORAGE SECTION.
                01 WS-HELLO-MESSAGE PIC X(13) VALUE 'Hello, World!'.

        PROCEDURE DIVISION.
            DISPLAY WS-HELLO-MESSAGE.
            STOP RUN.
  ```
  </details>
  <details open="">
    <summary>Output</summary>

  ```cobol
  Hello, World!
  ```
  </details>
