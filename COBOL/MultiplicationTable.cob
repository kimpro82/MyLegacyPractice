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
