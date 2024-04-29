! Depreciation Calculator 2.1
!   - Date: 2024.04.29
!
! This program calculates depreciation using various methods including:
!   - Straight-Line Method
!   - Declining Balance Method
!   - Double Declining Balance Method
!   - Sum-of-the-Years' Digits Method
!
! <Updates>
! Ver.1     2024.04.20  Initialize with Straight-Line Method
! Ver.2     2024.04.22  Add other 3 depreciation methods
!                         - Declining Balance Method
!                         - Double Declining Balance Method
!                         - Sum-of-the-Years' Digits Method
! Ver.2.1   2044.04.29  Refactoring; Divide into
!                         - SetTableFormats()
!                         - PrintProgramInfo()
!                         - PrintDepreciationMethodDetails()
!                         - GetUserInput()
!                         - CalculateDefaultDepreciationRate()
!                         - PrintDepreciationTableHeader()
!                         - PrintDepreciationTableRow()

MODULE GlobalFormats

    ! Declare global variables for formatting
    IMPLICIT NONE
    CHARACTER(LEN=19) :: TableColumnNameFormat
    CHARACTER(LEN=25) :: TableRowContentFormat
    CHARACTER(LEN=22) :: TableTitleFormatWithRate
    CHARACTER(LEN=10) :: TableTitleFormatWithoutRate

END MODULE GlobalFormats

PROGRAM DepreciationCalculator2

    USE GlobalFormats

    IMPLICIT NONE
    DOUBLE PRECISION :: InitialCost, SalvageValue, UsefulLife, DepreciationRate
    INTEGER :: Year

    ! Set column and row formatting
    CALL SetTableFormats(TableColumnNameFormat, TableRowContentFormat, &
                         TableTitleFormatWithRate, TableTitleFormatWithoutRate)

    ! Display program information
    CALL PrintProgramInfo()

    ! Notification of specific details
    CALL PrintDepreciationMethodDetails()

    ! Get user input
    CALL GetUserInput(InitialCost, SalvageValue, UsefulLife, DepreciationRate)

    ! Set default value of the depreciation rate for DB method
    CALL CalculateDefaultDepreciationRate(DepreciationRate, UsefulLife)

    ! Call the custom functions to calculate depreciation
    CALL CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)
    CALL CalculateDecliningBalance(InitialCost, SalvageValue, UsefulLife, DepreciationRate)
    CALL CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)
    CALL CalculateSYD(InitialCost, SalvageValue, UsefulLife)

CONTAINS

    SUBROUTINE SetTableFormats(TableColumnNameFormat, TableRowContentFormat, &
                               TableTitleFormatWithRate, TableTitleFormatWithoutRate)

        IMPLICIT NONE
        CHARACTER(*), INTENT(OUT) :: TableColumnNameFormat, TableRowContentFormat, &
                                     TableTitleFormatWithRate, TableTitleFormatWithoutRate

        ! Set column and row formatting
        TableColumnNameFormat = "(A6, A16, A16, A16)"
        TableRowContentFormat = "(I6, F16.2, F16.2, F16.2)"
        TableTitleFormatWithRate = "(A, A, A, A, F5.2, A/)"
        TableTitleFormatWithoutRate = "(A, A, A/)"

    END SUBROUTINE SetTableFormats

    SUBROUTINE PrintProgramInfo()

        IMPLICIT NONE

        ! Display program information
        WRITE(*, '(A)', ADVANCE='NO')  "Depreciation Calculator Ver.2.1"
        WRITE(*, '(A/)') " (2024.04.29)"

    END SUBROUTINE PrintProgramInfo

    SUBROUTINE PrintDepreciationMethodDetails()

        ! Print the notification about declining balance method and salvage value
        WRITE(*, '(A)')  "※ In declining balance method and double declining balance depreciation, "
        WRITE(*, '(A/)') "  the salvage value is not recognized in the calculation of the depreciation base."

    END SUBROUTINE PrintDepreciationMethodDetails

    SUBROUTINE GetUserInput(InitialCost, SalvageValue, UsefulLife, DepreciationRate)

        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(OUT) :: InitialCost, SalvageValue, UsefulLife, DepreciationRate

        ! Get user input
        WRITE(*, '(A)', ADVANCE='NO') "  Enter the initial cost of the asset           : "
        READ *, InitialCost

        WRITE(*, '(A)', ADVANCE='NO') "  Enter the salvage value of the asset          : "
        READ *, SalvageValue

        WRITE(*, '(A)', ADVANCE='NO') "  Enter the useful life of the asset (in years) : "
        READ *, UsefulLife

        WRITE(*, '(A)')               "  Enter the depreciation rate(%)"
        WRITE(*, '(A)', ADVANCE='NO') "    (Default: 2 / useful life, ≒ DDB)           : "
        READ *, DepreciationRate

        WRITE(*, '(A)') ""

    END SUBROUTINE GetUserInput

    SUBROUTINE CalculateDefaultDepreciationRate(DepreciationRate, UsefulLife)

        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(INOUT) :: DepreciationRate
        DOUBLE PRECISION, INTENT(IN) :: UsefulLife
    
        IF (DepreciationRate == 0.0) THEN
            DepreciationRate = (2 / UsefulLife) * 100
        END IF

    END SUBROUTINE CalculateDefaultDepreciationRate

    SUBROUTINE PrintDepreciationTableHeader(MethodName, DepreciationRate)

        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: MethodName
        DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: DepreciationRate

        ! Print the header for the specified depreciation method
        IF (PRESENT(DepreciationRate)) THEN
            WRITE(*, TableTitleFormatWithRate) "<", MethodName, ">", &
                                               " (Rate: ", DepreciationRate, "%)"
        ELSE
            WRITE(*, TableTitleFormatWithoutRate) "<", MethodName, ">"
        END IF
        WRITE(*, TableColumnNameFormat) "Year", "Depreciation", "Accumulated", "Book"
        WRITE(*, TableColumnNameFormat) "", "Expense", "Depreciation", "Value"

    END SUBROUTINE PrintDepreciationTableHeader

    SUBROUTINE PrintDepreciationTableRow(Year, DepreciationExpense, &
                                         AccumulatedDepreciation, BookValue)

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: Year
        DOUBLE PRECISION, INTENT(IN) :: DepreciationExpense, AccumulatedDepreciation, BookValue

        ! Print the row content for each year
        WRITE(*, TableRowContentFormat) Year, DepreciationExpense, &
                                        AccumulatedDepreciation, BookValue

    END SUBROUTINE PrintDepreciationTableRow

    SUBROUTINE CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)

        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost

        ! Calculate and print depreciation for each year
        CALL PrintDepreciationTableHeader("Straight-Line Method")
        DO Year = 1, INT(UsefulLife)
            DepreciationExpense = (InitialCost - SalvageValue) / UsefulLife
            AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
            BookValue = BookValue - DepreciationExpense
            CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                           AccumulatedDepreciation, BookValue)
        END DO
        WRITE(*, '(A)') ""

    END SUBROUTINE CalculateStraightLine

    SUBROUTINE CalculateDecliningBalance(InitialCost, SalvageValue, &
                                         UsefulLife, DepreciationRate)

        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, &
                                        UsefulLife, DepreciationRate
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost

        ! Calculate and print depreciation for each year
        CALL PrintDepreciationTableHeader("Declining Balance Method", DepreciationRate)
        DO Year = 1, INT(UsefulLife)
            IF (BookValue * (1 - DepreciationRate / 100) >= SalvageValue) THEN
                DepreciationExpense = BookValue * DepreciationRate / 100
                AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
                BookValue = BookValue - DepreciationExpense
            ELSE
                BookValue = SalvageValue
                DepreciationExpense = (InitialCost - SalvageValue) - AccumulatedDepreciation
                AccumulatedDepreciation = InitialCost - SalvageValue
            END IF

            CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                           AccumulatedDepreciation, BookValue)

            IF (BookValue <= SalvageValue) THEN
                EXIT
            END IF
        END DO
        WRITE(*, '(A)') ""

    END SUBROUTINE CalculateDecliningBalance

    SUBROUTINE CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)

        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationRate, DepreciationExpense, &
                            AccumulatedDepreciation, BookValue

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost
        DepreciationRate = (2 / UsefulLife) * 100

        ! Calculate and print depreciation for each year
        CALL PrintDepreciationTableHeader("Double Declining Balance Method", DepreciationRate)
        DO Year = 1, INT(UsefulLife)
            IF (BookValue * (1 - DepreciationRate / 100) >= SalvageValue) THEN
                DepreciationExpense = BookValue * DepreciationRate / 100
                AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
                BookValue = BookValue - DepreciationExpense
            ELSE
                BookValue = SalvageValue
                DepreciationExpense = (InitialCost - SalvageValue) - AccumulatedDepreciation
                AccumulatedDepreciation = InitialCost - SalvageValue
            END IF

            CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                           AccumulatedDepreciation, BookValue)

            IF (BookValue <= SalvageValue) THEN
                EXIT
            END IF
        END DO
        WRITE(*, '(A)') ""

    END SUBROUTINE CalculateDoubleDecliningBalance

    SUBROUTINE CalculateSYD(InitialCost, SalvageValue, UsefulLife)

        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
        INTEGER :: SumOfYears

        ! Calculate the sum of the years' digits
        SumOfYears = UsefulLife * (UsefulLife + 1) / 2

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost

        ! Calculate and print depreciation for each year
        CALL PrintDepreciationTableHeader("Sum-of-the-Years' Digits Method")
        DO Year = 1, INT(UsefulLife)
            DepreciationExpense = (InitialCost - SalvageValue) * (UsefulLife - Year + 1) / SumOfYears
            AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
            BookValue = BookValue - DepreciationExpense
            CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                           AccumulatedDepreciation, BookValue)
        END DO
        WRITE(*, '(A)') ""

    END SUBROUTINE CalculateSYD

END PROGRAM DepreciationCalculator2
