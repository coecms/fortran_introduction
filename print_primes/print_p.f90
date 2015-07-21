PROGRAM print_primes
  IMPLICIT NONE
  INTEGER :: counter, divisor, candidate

  ! First two have to be calculated differently, easier to 
  ! print them straight away.
  print *, 1, 2
  print *, 2, 3

  ! Initialise the outer loop
  candidate = 3
  counter = 2
  outer_loop : DO

    ! Get a new candidate
    candidate = candidate+2

    ! Initialise the inner loop
    divisor = 3
    inner_loop : DO 
      if (mod(candidate, divisor) == 0) CYCLE outer_loop
      divisor = divisor + 2
      if (divisor**2 > candidate) EXIT inner_loop
    END DO inner_loop

    ! Found one, increment the counter, and print it.
    counter = counter + 1
    print *, counter, candidate

    ! If we have found enough, exit.
    if (counter >= 50) EXIT outer_loop
  END DO outer_loop
END PROGRAM print_primes
