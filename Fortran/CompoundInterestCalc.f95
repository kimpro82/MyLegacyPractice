! Compound Interest Calculator
! 2023.12.07.

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
