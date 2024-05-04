! Read CSV File Practice
! Date: 2024.05.04

! This program reads data from a CSV file containing information about girl groups.
! It splits each line of the CSV file into two columns and stores the data into an array.
! The first row of the CSV file is assumed to contain column headers.
! The maximum number of rows and columns is specified by MaxRows and MaxCols parameters.
! MaxColumnLength parameter sets the maximum length of each column.


PROGRAM ReadCSVFile

    IMPLICIT NONE

    ! Constants
    INTEGER, PARAMETER         :: MaxRows = 1000, MaxCols = 10  ! Maximum number of rows and columns
    INTEGER, PARAMETER         :: MaxColumnLength = 16          ! Maximum length of each column
    INTEGER, PARAMETER         :: HeaderRow = 1                 ! Row number containing column headers

    ! Variables
    CHARACTER(MaxColumnLength) :: DataArray(MaxRows, 2)         ! Array to store data
    CHARACTER(80)              :: FileName                      ! File name
    INTEGER                    :: NumRows, i, FileUnit          ! Number of rows and file unit
    CHARACTER(80)              :: Line                          ! Line read from the file

    ! Get the file name
    FileName = 'GirlGroups.csv'

    ! Open the data file
    OPEN(UNIT=FileUnit, FILE=FileName, STATUS='OLD', ACTION='READ')

    ! Initialize the number of rows
    NumRows = 0

    ! Read the data from the file
    DO
        READ(FileUnit, '(A)', END=999) Line
        NumRows = NumRows + 1
        ! Split the line into columns
        CALL SplitLine(Line, DataArray(NumRows, 1), DataArray(NumRows, 2))
    END DO

999 CONTINUE

    ! Close the data file
    CLOSE(UNIT=FileUnit)

    ! Print the data array
    DO i = 1, NumRows
        WRITE(*, '(A, A)') DataArray(i, 1), DataArray(i, 2)

        ! Print the line separator
        IF (i == HeaderRow) THEN
            WRITE(*, '(A)') REPEAT('-', MaxColumnLength*2)
        END IF
    END DO

CONTAINS

    ! Subroutine to split a line into two columns
    SUBROUTINE SplitLine(Line, Column1, Column2)

        IMPLICIT NONE
        CHARACTER(80), INTENT(IN)  :: Line                  ! Input line
        CHARACTER(MaxColumnLength) :: Column1, Column2      ! Output columns
        CHARACTER(80)              :: TempLine              ! Temporary line storage
        INTEGER                    :: Start, End            ! Start and end positions

        TempLine = TRIM(Line)
        Start = 1

        ! Read the first column until a comma is encountered
        DO WHILE (Start <= LEN(TempLine) .AND. TempLine(Start:Start) /= ',')
            Start = Start + 1
        END DO
        End = MIN(Start+MaxColumnLength-1, LEN(TempLine))
        Column1 = TempLine(1:Start-1)

        ! Read the second column from the comma to the end of the line
        Start = Start + 1
        Column2 = TempLine(Start:LEN(TempLine))

    END SUBROUTINE SplitLine

END PROGRAM ReadCSVFile
