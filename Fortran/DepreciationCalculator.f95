! Depreciation Calculator
! Date: 2024.04.20
!
! This program supports calculating depreciation only using the straight-line method.

PROGRAM DepreciationCalculator

    IMPLICIT NONE

    DOUBLE PRECISION :: InitialCost, SalvageValue, UsefulLife
    DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
    INTEGER :: Year

    ! Get user input
    WRITE(*, '(A)', ADVANCE='NO') "Enter the initial cost of the asset           : "; READ *, InitialCost
    WRITE(*, '(A)', ADVANCE='NO') "Enter the salvage value of the asset          : "; READ *, SalvageValue
    WRITE(*, '(A)', ADVANCE='NO') "Enter the useful life of the asset (in years) : "; READ *, UsefulLife
    WRITE(*, '(A)') ""

    ! Initialize accumulatedDepreciation and bookValue
    AccumulatedDepreciation = 0.0
    BookValue = InitialCost

    ! Print header
    WRITE(*, '(A6, A22, A26, A20)') "Year", "Depreciation Expense", "Accumulated Depreciation", "Book Value"

    ! Calculate and print depreciation for each year
    DO Year = 1, INT(UsefulLife)
        DepreciationExpense = (InitialCost - SalvageValue) / UsefulLife
        AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
        BookValue = BookValue - DepreciationExpense
        WRITE(*, '(I6, F22.2, F26.2, F20.2)') Year, DepreciationExpense, AccumulatedDepreciation, BookValue
    END DO

END PROGRAM DepreciationCalculator
