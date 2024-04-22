! Depreciation Calculator 2
! Date: 2024.04.22
!
! This program calculates depreciation using various methods including:
!   - Straight-Line Method
!   - Declining Balance Method
!   - Double Declining Balance Method
!   - Sum-of-the-Years' Digits Method

MODULE GlobalFormats

    IMPLICIT NONE

    ! Declare global variables for formatting
    CHARACTER(LEN=19) :: ColumnNameFormat
    CHARACTER(LEN=25) :: RowContentFormat

END MODULE GlobalFormats

PROGRAM DepreciationCalculator2

    USE GlobalFormats

    IMPLICIT NONE

    DOUBLE PRECISION :: InitialCost, SalvageValue, UsefulLife
    DOUBLE PRECISION :: DepreciationRate
    DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
    INTEGER :: Year

    ! Set column and row formatting
    ColumnNameFormat = "(A6, A16, A16, A16)"
    RowContentFormat = "(I6, F16.2, F16.2, F16.2)"

    ! Get user input
    WRITE(*, '(A)', ADVANCE='NO') "Enter the initial cost of the asset           : "; READ *, InitialCost
    WRITE(*, '(A)', ADVANCE='NO') "Enter the salvage value of the asset          : "; READ *, SalvageValue
    WRITE(*, '(A)', ADVANCE='NO') "Enter the useful life of the asset (in years) : "; READ *, UsefulLife
    WRITE(*, '(A)') "Enter the depreciation rate (%)"
    WRITE(*, '(A)', ADVANCE='NO') "  (Default: 2 / useful life, ≒ DDB)           : "; READ *, DepreciationRate
    WRITE(*, '(/)')

    ! Call the custom functions to calculate depreciation
    CALL CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)
    CALL CalculateDecliningBalance(InitialCost, SalvageValue, UsefulLife, DepreciationRate)
    CALL CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)
    CALL CalculateSYD(InitialCost, SalvageValue, UsefulLife)

CONTAINS

    SUBROUTINE CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)

        IMPLICIT NONE

        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
        INTEGER :: Year

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost

        ! Print header
        WRITE(*, '(A, /)') "<Straight-Line Method>"
        WRITE(*, ColumnNameFormat) "Year", "Depreciation", "Accumulated", "Book"
        WRITE(*, ColumnNameFormat) "", "Expense", "Depreciation", "Value"

        ! Calculate and print depreciation for each year
        DO Year = 1, INT(UsefulLife)
            DepreciationExpense = (InitialCost - SalvageValue) / UsefulLife
            AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
            BookValue = BookValue - DepreciationExpense
            WRITE(*, RowContentFormat) Year, DepreciationExpense, AccumulatedDepreciation, BookValue
        END DO
        WRITE(*, '(/)')

    END SUBROUTINE CalculateStraightLine

    SUBROUTINE CalculateDecliningBalance(InitialCost, SalvageValue, UsefulLife, DepreciationRate)

        IMPLICIT NONE

        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife, DepreciationRate
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
        INTEGER :: Year

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost

        ! Print header
        WRITE(*, '(A, F5.2, A/)') "<Declining Balance Method> (Rate: ", DepreciationRate, "%)"
        WRITE(*, ColumnNameFormat) "Year", "Depreciation", "Accumulated", "Book"
        WRITE(*, ColumnNameFormat) "", "Expense", "Depreciation", "Value"

        ! Calculate and print depreciation for each year
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

            WRITE(*, RowContentFormat) Year, DepreciationExpense, AccumulatedDepreciation, BookValue

            IF (BookValue <= SalvageValue) THEN
                EXIT
            END IF
        END DO
        WRITE(*, '(/)')

    END SUBROUTINE CalculateDecliningBalance

    SUBROUTINE CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)

        IMPLICIT NONE

        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationRate
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
        INTEGER :: Year

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost
        DepreciationRate = (2 / UsefulLife) * 100

        ! Print header
        WRITE(*, '(A, F5.2, A/)') "<Double Declining Balance Method> (Rate: ", DepreciationRate, "%)"
        WRITE(*, ColumnNameFormat) "Year", "Depreciation", "Accumulated", "Book"
        WRITE(*, ColumnNameFormat) "", "Expense", "Depreciation", "Value"

        ! Calculate and print depreciation for each year
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

            WRITE(*, RowContentFormat) Year, DepreciationExpense, AccumulatedDepreciation, BookValue

            IF (BookValue <= SalvageValue) THEN
                EXIT
            END IF
        END DO
        WRITE(*, '(/)')

    END SUBROUTINE CalculateDoubleDecliningBalance

    SUBROUTINE CalculateSYD(InitialCost, SalvageValue, UsefulLife)

        IMPLICIT NONE

        DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
        INTEGER :: Year
        INTEGER :: SumOfYears

        ! Calculate the sum of the years' digits
        SumOfYears = UsefulLife * (UsefulLife + 1) / 2

        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost

        ! Print header
        WRITE(*, '(A, /)') "<Sum-of-the-Years' Digits Method>"
        WRITE(*, ColumnNameFormat) "Year", "Depreciation", "Accumulated", "Book"
        WRITE(*, ColumnNameFormat) "", "Expense", "Depreciation", "Value"

        ! Calculate and print depreciation for each year
        DO Year = 1, INT(UsefulLife)
            DepreciationExpense = (InitialCost - SalvageValue) * (UsefulLife - Year + 1) / SumOfYears
            AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
            BookValue = BookValue - DepreciationExpense
            WRITE(*, RowContentFormat) Year, DepreciationExpense, AccumulatedDepreciation, BookValue
        END DO
        WRITE(*, '(/)')

    END SUBROUTINE CalculateSYD

END PROGRAM DepreciationCalculator2
