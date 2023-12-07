## [Fortran > Compound Interest Calculator](/README.md#fortran)


# [Compound Interest Calculator](/README.md#fortran)


## \<List>

- [Compound Interest Calculator (2023.12.07)](#compound-interest-calculator-20231207)


## [Compound Interest Calculator (2023.12.07)](#list)

- My first *Fortran* code to overcome coding yips(?)
- Environment : *Fortran 95* supported from [Replit](https://replit.com/)
- Compilation and Run
  - Compilation
    ```
    gfortran -o main CompoundInterestCalc.f95
    ```
  - Run
    ```
    ./main
    ```
- Codes and Results
  <details>
    <summary>Codes : CompoundInterestCalc.f95</summary>

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
    <summary>Results</summary>

    ```yaml
    Enter the principal amount  : 10000
    Enter the interest rate(%)  : 6
    Enter the number of periods : 12
    Enter the Periodic Payment  : 0
    
    The deposit amount  :     10000.00
    The interest amount :     10121.95
    The total amount    :     20121.95
    ```
    ```yaml
    Enter the principal amount  : 0   
    Enter the interest rate(%)  : 5
    Enter the number of periods : 12
    Enter the Periodic Payment  : 1000
    
    The deposit amount  :     12000.00
    The interest amount :      3917.11
    The total amount    :     15917.11
    ```
    ```yaml
    Enter the principal amount  : 10000
    Enter the interest rate(%)  : 3
    Enter the number of periods : 12
    Enter the Periodic Payment  : -300
    
    The deposit amount  :      6400.00
    The interest amount :      3600.00
    The total amount    :     10000.00
    ```
  </details>