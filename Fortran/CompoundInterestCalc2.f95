! Compound Interest Calculator
! 2023.12.16.

PROGRAM CompoundInterestCalculator_2

  IMPLICIT NONE

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

  ! Get values from user
  WRITE(*, "(A)", ADVANCE='NO') 'Enter the principal amount                 : '; READ*, Principal
  WRITE(*, "(A)", ADVANCE='NO') 'Enter the interest rate(%)                 : '; READ*, Rate
  WRITE(*, "(A)", ADVANCE='NO') 'Enter the number of periods                : '; READ*, Periods
  WRITE(*, "(A)", ADVANCE='NO') 'Enter the Periodic Payment (Withdrawl < 0) : '; READ*, Pmt

  ! Initialize variables
  Deposit = Principal
  CurrentValue = Principal
  Interest = 0.0
  TotalInterest = 0.0
  TotalWithdrawl = 0.0

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

  ! Display the calculated results
  WRITE(*, "(A)") ""
  WRITE(*, Format2) "The deposit amount          : ", Deposit
  WRITE(*, Format2) "The interest amount         : ", TotalInterest
  WRITE(*, Format2) "The total amount            : ", TotalAmount
  WRITE(*, Format2) "The early withdrawal amount : ", TotalWithdrawl
  WRITE(*, Format2) "The maturity balance        : ", MaturityBalance

END PROGRAM CompoundInterestCalculator_2
