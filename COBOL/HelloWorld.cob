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
