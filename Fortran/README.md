# [My Fortran Practice](/README.md#fortran)


### \<List>

- [Depreciation Calculator 2.1 (2024.04.29)](#depreciation-calculator-21-20240429)
- [Depreciation Calculator 2 (2024.04.22)](#depreciation-calculator-2-20240422)
- [Depreciation Calculator (2024.04.20)](#depreciation-calculator-20240420)
- [Compound Interest Calculator 2 (2023.12.16)](#compound-interest-calculator-2-20231216)
- [Compound Interest Calculator (2023.12.07)](#compound-interest-calculator-20231207)


## [Depreciation Calculator 2.1 (2024.04.29)](#list)

- Refactoring : Divide into 7 subroutines
  - `SetTableFormats()` `PrintProgramInfo()` `PrintDepreciationMethodDetails()` `GetUserInput()` `CalculateDefaultDepreciationRate()` `PrintDepreciationTableHeader()` `PrintDepreciationTableRow()`
- Code and Output
  <details>
    <summary>Code : DepreciationCalculator2.1.f95 (Only mainly updated parts)</summary>

  ```fortran
  MODULE GlobalFormats

      ! Declare global variables for formatting
      IMPLICIT NONE
      CHARACTER(LEN=19) :: TableColumnNameFormat
      CHARACTER(LEN=25) :: TableRowContentFormat
      CHARACTER(LEN=22) :: TableTitleFormatWithRate
      CHARACTER(LEN=10) :: TableTitleFormatWithoutRate

  END MODULE GlobalFormats
  ```
  ```fortran
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
      ……

  CONTAINS

      SUBROUTINE SetTableFormats(TableColumnNameFormat, TableRowContentFormat, &
                                TableTitleFormatWithRate, TableTitleFormatWithoutRate)
      ……

      SUBROUTINE PrintProgramInfo()
      ……

      SUBROUTINE PrintDepreciationMethodDetails()
      ……

      SUBROUTINE GetUserInput(InitialCost, SalvageValue, UsefulLife, DepreciationRate)
      ……

      SUBROUTINE CalculateDefaultDepreciationRate(DepreciationRate, UsefulLife)
      ……

      SUBROUTINE PrintDepreciationTableHeader(MethodName, DepreciationRate)
      ……

      SUBROUTINE PrintDepreciationTableRow(Year, DepreciationExpense, &
                                          AccumulatedDepreciation, BookValue)
      ……

      SUBROUTINE CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)
      ……

      SUBROUTINE CalculateDecliningBalance(InitialCost, SalvageValue, &
                                          UsefulLife, DepreciationRate)
      ……

      SUBROUTINE CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)
      ……

      SUBROUTINE CalculateSYD(InitialCost, SalvageValue, UsefulLife)
      ……

  END PROGRAM DepreciationCalculator2
  ```
  ```fortran
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
  ```
  ```fortran
      SUBROUTINE PrintProgramInfo()

          IMPLICIT NONE

          ! Display program information
          WRITE(*, '(A)', ADVANCE='NO')  "Depreciation Calculator Ver.2.1"
          WRITE(*, '(A/)') " (2024.04.29)"

      END SUBROUTINE PrintProgramInfo
  ```
  ```fortran
      SUBROUTINE PrintDepreciationMethodDetails()

          ! Print the notification about declining balance method and salvage value
          WRITE(*, '(A)')  "※ In declining balance method and double declining balance depreciation, "
          WRITE(*, '(A/)') "  the salvage value is not recognized in the calculation of the depreciation base."

      END SUBROUTINE PrintDepreciationMethodDetails
  ```
  ```fortran
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
  ```
  ```fortran
      SUBROUTINE CalculateDefaultDepreciationRate(DepreciationRate, UsefulLife)

          IMPLICIT NONE
          DOUBLE PRECISION, INTENT(INOUT) :: DepreciationRate
          DOUBLE PRECISION, INTENT(IN) :: UsefulLife
      
          IF (DepreciationRate == 0.0) THEN
              DepreciationRate = (2 / UsefulLife) * 100
          END IF

      END SUBROUTINE CalculateDefaultDepreciationRate
  ```
  ```fortran
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
  ```
  ```fortran
      SUBROUTINE PrintDepreciationTableRow(Year, DepreciationExpense, &
                                          AccumulatedDepreciation, BookValue)

          IMPLICIT NONE
          INTEGER, INTENT(IN) :: Year
          DOUBLE PRECISION, INTENT(IN) :: DepreciationExpense, AccumulatedDepreciation, BookValue

          ! Print the row content for each year
          WRITE(*, TableRowContentFormat) Year, DepreciationExpense, &
                                          AccumulatedDepreciation, BookValue

      END SUBROUTINE PrintDepreciationTableRow
  ```
  ```fortran
      SUBROUTINE CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)

          ……

          ! Calculate and print depreciation for each year
          CALL PrintDepreciationTableHeader("Straight-Line Method")
          DO Year = 1, INT(UsefulLife)
              ……
              CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                            AccumulatedDepreciation, BookValue)
          END DO
          ……

      END SUBROUTINE CalculateStraightLine
  ```
  ```fortran
      SUBROUTINE CalculateDecliningBalance(InitialCost, SalvageValue, &
                                          UsefulLife, DepreciationRate)

          ……

          ! Calculate and print depreciation for each year
          CALL PrintDepreciationTableHeader("Declining Balance Method", DepreciationRate)
          DO Year = 1, INT(UsefulLife)
              ……

              CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                            AccumulatedDepreciation, BookValue)

              ……
          END DO
          ……

      END SUBROUTINE CalculateDecliningBalance
  ```
  ```fortran
      SUBROUTINE CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)

          ……

          ! Calculate and print depreciation for each year
          CALL PrintDepreciationTableHeader("Double Declining Balance Method", DepreciationRate)
          DO Year = 1, INT(UsefulLife)
              ……

              CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                            AccumulatedDepreciation, BookValue)

              ……
          END DO
          ……

      END SUBROUTINE CalculateDoubleDecliningBalance
  ```
  ```fortran
      SUBROUTINE CalculateSYD(InitialCost, SalvageValue, UsefulLife)

          ……

          ! Calculate and print depreciation for each year
          CALL PrintDepreciationTableHeader("Sum-of-the-Years' Digits Method")
          DO Year = 1, INT(UsefulLife)
              ……
              CALL PrintDepreciationTableRow(Year, DepreciationExpense, &
                                            AccumulatedDepreciation, BookValue)
          END DO
          ……

      END SUBROUTINE CalculateSYD
  ```
  </details>
  <details open="">
    <summary>Output</summary>

  ```fortran
  Depreciation Calculator Ver.2.1 (2024.04.29)

  ※ In declining balance method and double declining balance depreciation, 
    the salvage value is not recognized in the calculation of the depreciation base.

    Enter the initial cost of the asset           : ……
  ```
  </details>


## [Depreciation Calculator 2 (2024.04.22)](#list)

- Add various methods of depreciation  
  - Declining Balance Method
  - Double Declining Balance Method
  - Sum-of-the-Years' Digits Method
- Future Improvements
  - ~~Calculation of depreciation adjustment related with tax accounting~~
  - ~~Consideration of memorandum value~~
- Code and Output
  <details>
    <summary>Code : DepreciationCalculator2.f95</summary>

    ```fortran
    MODULE GlobalFormats

        IMPLICIT NONE

        ! Declare global variables for formatting
        CHARACTER(LEN=19) :: ColumnNameFormat
        CHARACTER(LEN=25) :: RowContentFormat

    END MODULE GlobalFormats
    ```
    ```fortran
    PROGRAM DepreciationCalculator2

        USE GlobalFormats

        IMPLICIT NONE

        DOUBLE PRECISION :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationRate
        INTEGER :: Year

        ! Set column and row formatting
        ColumnNameFormat = "(A6, A16, A16, A16)"
        RowContentFormat = "(I6, F16.2, F16.2, F16.2)"

        ! Display program information
        WRITE(*, '(A)', ADVANCE='NO')  "Depreciation Calculator Ver.2"
        WRITE(*, '(A/)') " (2024.04.22)"

        ! Get user input
        WRITE(*, '(A)', ADVANCE='NO') "  Enter the initial cost of the asset           : "; READ *, InitialCost
        WRITE(*, '(A)', ADVANCE='NO') "  Enter the salvage value of the asset          : "; READ *, SalvageValue
        WRITE(*, '(A)', ADVANCE='NO') "  Enter the useful life of the asset (in years) : "; READ *, UsefulLife
        WRITE(*, '(A)') "  Enter the depreciation rate(%)"
        WRITE(*, '(A)', ADVANCE='NO') "    (Default: 2 / useful life, ≒ DDB)           : "; READ *, DepreciationRate

        ! Notification of specific details
        WRITE(*, '(/A)') "※ In declining balance method and double declining balance depreciation, "
        WRITE(*, '(A)')  "  the salvage value is not recognized in the calculation of the depreciation base."
        WRITE(*, '(/)')

        ! Set default value of the depreciation rate for DB method
        IF (DepreciationRate == 0.0) THEN
            DepreciationRate = (2 / UsefulLife) * 100
        END IF

        ! Call the custom functions to calculate depreciation
        CALL CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)
        CALL CalculateDecliningBalance(InitialCost, SalvageValue, UsefulLife, DepreciationRate)
        CALL CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)
        CALL CalculateSYD(InitialCost, SalvageValue, UsefulLife)

    CONTAINS

        SUBROUTINE CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)
        ……

        SUBROUTINE CalculateDecliningBalance(InitialCost, SalvageValue, UsefulLife, DepreciationRate)
        ……
        
        SUBROUTINE CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)
        ……
        
        SUBROUTINE CalculateSYD(InitialCost, SalvageValue, UsefulLife)
        ……
        
    END PROGRAM DepreciationCalculator2
    ```
    ```fortran
        SUBROUTINE CalculateStraightLine(InitialCost, SalvageValue, UsefulLife)

            IMPLICIT NONE

            DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
            DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue

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
            WRITE(*, '(A)') ""

        END SUBROUTINE CalculateStraightLine
    ```
    ```fortran
        SUBROUTINE CalculateDecliningBalance(InitialCost, SalvageValue, UsefulLife, DepreciationRate)

            IMPLICIT NONE

            DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife, DepreciationRate
            DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue

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
            WRITE(*, '(A)') ""

        END SUBROUTINE CalculateDecliningBalance
    ```
    ```fortran
        SUBROUTINE CalculateDoubleDecliningBalance(InitialCost, SalvageValue, UsefulLife)

            IMPLICIT NONE

            DOUBLE PRECISION, INTENT(IN) :: InitialCost, SalvageValue, UsefulLife
            DOUBLE PRECISION :: DepreciationRate, DepreciationExpense, AccumulatedDepreciation, BookValue

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
            WRITE(*, '(A)') ""

        END SUBROUTINE CalculateDoubleDecliningBalance
    ```
    ```fortran
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
            WRITE(*, '(A)') ""

        END SUBROUTINE CalculateSYD
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```fortran
    Depreciation Calculator Ver.2 (2024.04.22)

      Enter the initial cost of the asset           : 11000
      Enter the salvage value of the asset          : 1000
      Enter the useful life of the asset (in years) : 5
      Enter the depreciation rate(%)
        (Default: 2 / useful life, ≒ DDB)           : 50

    ※ In declining balance method and double declining balance depreciation, 
      the salvage value is not recognized in the calculation of the depreciation base.


    <Straight-Line Method>

     Year    Depreciation     Accumulated            Book
                  Expense    Depreciation           Value
        1         2000.00         2000.00         9000.00
        2         2000.00         4000.00         7000.00
        3         2000.00         6000.00         5000.00
        4         2000.00         8000.00         3000.00
        5         2000.00        10000.00         1000.00

    <Declining Balance Method> (Rate: 50.00%)

     Year    Depreciation     Accumulated            Book
                  Expense    Depreciation           Value
        1         5500.00         5500.00         5500.00
        2         2750.00         8250.00         2750.00
        3         1375.00         9625.00         1375.00
        4          375.00        10000.00         1000.00

    <Double Declining Balance Method> (Rate: 40.00%)

     Year    Depreciation     Accumulated            Book
                  Expense    Depreciation           Value
        1         4400.00         4400.00         6600.00
        2         2640.00         7040.00         3960.00
        3         1584.00         8624.00         2376.00
        4          950.40         9574.40         1425.60
        5          425.60        10000.00         1000.00

    <Sum-of-the-Years' Digits Method>

     Year    Depreciation     Accumulated            Book
                  Expense    Depreciation           Value
        1         3333.33         3333.33         7666.67
        2         2666.67         6000.00         5000.00
        3         2000.00         8000.00         3000.00
        4         1333.33         9333.33         1666.67
        5          666.67        10000.00         1000.00
    ```
  </details>


## [Depreciation Calculator (2024.04.20)](#list)

- Calculate depreciation only using the straight-line method
- Future Improvements
  - Add various methods of depreciation  
    : Declining Balance Method, Accelerated Depreciation Method, Sum-of-the-Years’ Digits Method, etc. → [Done](#depreciation-calculator-2-20240422)
- Code and Output
  <details>
    <summary>Code : DepreciationCalculator.f95</summary>

    ```fortran
    PROGRAM DepreciationCalculator

      (All of the below code is located here)

    END PROGRAM DepreciationCalculator
    ```
    ```fortran
        IMPLICIT NONE
    ```
    ```fortran
        DOUBLE PRECISION :: InitialCost, SalvageValue, UsefulLife
        DOUBLE PRECISION :: DepreciationExpense, AccumulatedDepreciation, BookValue
        INTEGER :: Year
    ```
    ```fortran
        ! Get user input
        WRITE(*, '(A)', ADVANCE='NO') "Enter the initial cost of the asset           : "; READ *, InitialCost
        WRITE(*, '(A)', ADVANCE='NO') "Enter the salvage value of the asset          : "; READ *, SalvageValue
        WRITE(*, '(A)', ADVANCE='NO') "Enter the useful life of the asset (in years) : "; READ *, UsefulLife
        WRITE(*, '(A)') ""
    ```
    ```fortran
        ! Initialize accumulatedDepreciation and bookValue
        AccumulatedDepreciation = 0.0
        BookValue = InitialCost
    ```
    ```fortran
        ! Print header
        WRITE(*, '(A6, A22, A26, A20)') "Year", "Depreciation Expense", "Accumulated Depreciation", "Book Value"

        ! Calculate and print depreciation for each year
        DO Year = 1, INT(UsefulLife)
            DepreciationExpense = (InitialCost - SalvageValue) / UsefulLife
            AccumulatedDepreciation = AccumulatedDepreciation + DepreciationExpense
            BookValue = BookValue - DepreciationExpense
            WRITE(*, '(I6, F22.2, F26.2, F20.2)') Year, DepreciationExpense, AccumulatedDepreciation, BookValue
        END DO
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```fortran
    Enter the initial cost of the asset           : 11000
    Enter the salvage value of the asset          : 1000
    Enter the useful life of the asset (in years) : 5

     Year  Depreciation Expense  Accumulated Depreciation          Book Value
        1               2000.00                   2000.00             9000.00
        2               2000.00                   4000.00             7000.00
        3               2000.00                   6000.00             5000.00
        4               2000.00                   8000.00             3000.00
        5               2000.00                  10000.00             1000.00
    ```
  </details>


## [Compound Interest Calculator 2 (2023.12.16)](#list)

- Improvements
  - Add 2 items : Total withdrawal amount, Maturity balance
  - Improve the calculation method for `Pmt < 0` by using a loop.
  - Add a test mode.
    - Failed to apply conditional compilation.
  - Adhere to the standard Fortran conventions (use uppercase keywords).
  - Separate frequently used formats into separate strings.
- Code and Output
  <details>
    <summary>Code : CompoundInterestCalc.f95</summary>

    ```fortran
    PROGRAM CompoundInterestCalculator_2

      (All of the below code is located here)

    END PROGRAM CompoundInterestCalculator_2
    ```
    ```fortran
      IMPLICIT NONE
    ```
    ```fortran
      LOGICAL :: IsTest
      REAL :: Principal, Rate, Pmt, &
              Deposit, Interest, CurrentValue, &
              TotalInterest, TotalWithdrawl, TotalAmount, MaturityBalance
      INTEGER :: Periods, i
      CHARACTER(LEN=30), PARAMETER :: Format1 = "(A,I6,A,F12.2,A,F12.2,A,F12.2)"
      CHARACTER(LEN= 9), PARAMETER :: Format2 = "(A,F12.2)"

      ! Set if test mode
      IsTest = .TRUE.
    ! #IF DEFINED(TEST)
    !   IsTest = .TRUE.
    ! #ELSE
    !   IsTest = .FALSE.
    ! #ENDIF
    ```
    ```fortran
      ! Get values from user
      WRITE(*, "(A)", ADVANCE='NO') 'Enter the principal amount                 : '; READ*, Principal
      WRITE(*, "(A)", ADVANCE='NO') 'Enter the interest rate(%)                 : '; READ*, Rate
      WRITE(*, "(A)", ADVANCE='NO') 'Enter the number of periods                : '; READ*, Periods
      WRITE(*, "(A)", ADVANCE='NO') 'Enter the Periodic Payment (Withdrawl < 0) : '; READ*, Pmt
    ```
    ```fortran
      ! Initialize variables
      Deposit = Principal
      CurrentValue = Principal
      Interest = 0.0
      TotalInterest = 0.0
      TotalWithdrawl = 0.0
    ```
    ```fortran
      ! Calculate
      ! Pmt : The initial deposit is assumed.
      IF ((Pmt >= 0) .AND. .NOT. (IsTest)) THEN
        Deposit = Principal + Pmt * Periods
        CurrentValue = Principal * (1 + Rate/100) ** Periods + &
                      Pmt * ( ( (1 + Rate/100) ** Periods - 1) / (Rate/100) )
        TotalInterest = CurrentValue - Deposit
      ELSE
        IF (IsTest) THEN
          WRITE(*, "(A)") ""
          WRITE(*, "(A)") "<Test>"
        END IF

        DO i = 1, Periods
          Interest = (CurrentValue + Pmt) * (Rate/100)
          CurrentValue = CurrentValue + Pmt + Interest
          TotalInterest = TotalInterest + Interest

          IF (Pmt >= 0) THEN
            Deposit = Deposit + Pmt
          ELSE
            TotalWithdrawl = TotalWithdrawl - Pmt
          END IF

          IF (IsTest) THEN
            ! WRITE(*, "(I6)") " i: ", i                            ! " i: " requires "(A)"!
            WRITE(*, Format1) &
              " i: ", i, ", Deposit: ", Deposit, ", Interest: ", Interest, ", CurrentValue : ", CurrentValue
          END IF
        END DO
      END IF

      TotalAmount = Deposit + TotalInterest
      MaturityBalance = TotalAmount - TotalWithdrawl
    ```
    ```fortran
      ! Display the calculated results
      WRITE(*, "(A)") ""
      WRITE(*, Format2) "The deposit amount          : ", Deposit
      WRITE(*, Format2) "The interest amount         : ", TotalInterest
      WRITE(*, Format2) "The total amount            : ", TotalAmount
      WRITE(*, Format2) "The early withdrawal amount : ", TotalWithdrawl
      WRITE(*, Format2) "The maturity balance        : ", MaturityBalance
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```fortran
    Enter the principal amount                 : 12000
    Enter the interest rate(%)                 : 6
    Enter the number of periods                : 12
    Enter the Periodic Payment (Withdrawl < 0) : 0

    The deposit amount          :     12000.00
    The interest amount         :     12146.36
    The total amount            :     24146.36
    The early withdrawal amount :         0.00
    The maturity balance        :     24146.36
    ```
    ```fortran
    Enter the principal amount                 : 0
    Enter the interest rate(%)                 : 6
    Enter the number of periods                : 12
    Enter the Periodic Payment (Withdrawl < 0) : 1000

    The deposit amount          :     12000.00
    The interest amount         :      5882.14
    The total amount            :     17882.14
    The early withdrawal amount :         0.00
    The maturity balance        :     17882.14
    ```
    ```fortran
    Enter the principal amount                 : 12720
    Enter the interest rate(%)                 : 6
    Enter the number of periods                : 12
    Enter the Periodic Payment (Withdrawl < 0) : -720

    <Test>
    i:      1, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      2, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      3, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      4, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      5, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      6, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      7, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      8, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:      9, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:     10, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:     11, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00
    i:     12, Deposit:     12720.00, Interest:       720.00, CurrentValue :     12720.00

    The deposit amount          :     12720.00
    The interest amount         :      8640.00
    The total amount            :     21360.00
    The early withdrawal amount :      8640.00
    The maturity balance        :     12720.00
    ```
  </details>


## [Compound Interest Calculator (2023.12.07)](#list)

- My initial *Fortran* code to overcome coding yips(?)
- Environment : *Fortran 95* supported on [Replit](https://replit.com/)
- Compilation and Run
  - Compilation
    ```batchfile
    gfortran -o main CompoundInterestCalc.f95
    ```
  - Run
    ```batchfile
    ./main
    ```
- Code and Output
  <details>
    <summary>Code : CompoundInterestCalc.f95</summary>

    ```fortran
    program compound_interest_calculator

      implicit none
      real :: principal, rate, time, pmt, deposit, future_value

      ! Get principal amount from user
      write(*, "(A)", advance='no') 'Enter the principal amount  : '
      read*, principal
      
      ! Get interest rate from user
      write(*, "(A)", advance='no') 'Enter the interest rate(%)  : '
      read*, rate
      
      ! Get number of periods from user
      write(*, "(A)", advance='no') 'Enter the number of periods : '
      read*, time
      
      ! Get periodic payment from user
      write(*, "(A)", advance='no') 'Enter the Periodic Payment  : '
      read*, pmt

      ! Calculate deposit and future value
      deposit = principal + pmt * time
      future_value = principal * (1 + rate/100) ** time + pmt * ( ( (1 + rate/100) ** time - 1) / (rate/100) )
      
      ! Display the calculated results
      write(*, "(A)") " "
      write(*, "(A,F12.2)") "The deposit amount  : ", deposit
      write(*, "(A,F12.2)") "The interest amount : ", future_value - deposit
      write(*, "(A,F12.2)") "The total amount    : ", future_value

    end program compound_interest_calculator
    ```
  </details>
  <details open="">
    <summary>Output</summary>

    ```fortran
    Enter the principal amount  : 10000
    Enter the interest rate(%)  : 6
    Enter the number of periods : 12
    Enter the Periodic Payment  : 0
    
    The deposit amount  :     10000.00
    The interest amount :     10121.95
    The total amount    :     20121.95
    ```
    ```fortran
    Enter the principal amount  : 0   
    Enter the interest rate(%)  : 5
    Enter the number of periods : 12
    Enter the Periodic Payment  : 1000
    
    The deposit amount  :     12000.00
    The interest amount :      3917.11
    The total amount    :     15917.11
    ```
    ```fortran
    Enter the principal amount  : 10000
    Enter the interest rate(%)  : 3
    Enter the number of periods : 12
    Enter the Periodic Payment  : -300
    
    The deposit amount  :      6400.00
    The interest amount :      3600.00
    The total amount    :     10000.00
    ```
  </details>
