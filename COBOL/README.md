# [My COBOL Practice](../README.md#my-cobol-practice)

COBOL never dies


### \<List>

- [Hello World (2023.09.04)](#hello-world-20230904)


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
