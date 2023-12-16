! Compound Interest Calculator
! 2023.12.16.

PROGRAM CompoundInterestCalculator_2

  IMPLICIT NONE

  LOGICAL :: Test
  REAL :: Principal, Rate, Pmt
  REAL :: Deposit, CurrentValue, Interest, TotalInterest, TotalAmount, TotalWithdrawl, MaturityBalance
  INTEGER :: Periods, i

  ! Set Test Mode
  Test = .TRUE.

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
  MaturityBalance = 0.0

  IF (Test) THEN
    WRITE(*, "(A)") ""
    WRITE(*, "(A)") "<Test>"
  END IF

  ! Calculate
  ! Pmt : The initial deposit is assumed.
  DO i = 1, Periods
    Interest = (CurrentValue + Pmt) * (Rate/100)
    CurrentValue = CurrentValue + Pmt + Interest
    TotalInterest = TotalInterest + Interest

    IF (Pmt >= 0) THEN
      Deposit = Deposit + Pmt
    ELSE
      TotalWithdrawl = TotalWithdrawl - Pmt
    END IF

    IF (.NOT. Test) THEN
      CONTINUE
    END IF

    ! WRITE(*, "(I6)") " i: ", i                            ! " i: " requires "(A)"!
    WRITE(*, "(A,I6,A,F12.2,A,F12.2,A,F12.2)") &
      " i: ", i, ", Deposit: ", Deposit, ", Interest: ", Interest, ", CurrentValue : ", CurrentValue
  END DO

  TotalAmount = Deposit + TotalInterest
  MaturityBalance = TotalAmount - TotalWithdrawl

  ! Display the calculated results
  WRITE(*, "(A)") ""
  WRITE(*, "(A,F12.2)") "The deposit amount          : ", Deposit
  WRITE(*, "(A,F12.2)") "The interest amount         : ", TotalInterest
  WRITE(*, "(A,F12.2)") "The total amount            : ", TotalAmount
  WRITE(*, "(A,F12.2)") "The early withdrawal amount : ", TotalWithdrawl
  WRITE(*, "(A,F12.2)") "The maturity balance        : ", MaturityBalance

END PROGRAM CompoundInterestCalculator_2
