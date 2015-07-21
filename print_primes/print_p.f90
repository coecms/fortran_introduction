MODULE prime_mod
  IMPLICIT NONE

CONTAINS

  function is_prime(candidate)
    IMPLICIT NONE
    integer, intent(in) :: candidate
    logical :: is_prime
    integer :: divisor

    ! 2 or 3 won't be caught by the loop
    if ( candidate == 2 .or. candidate == 3 ) then
      is_prime = .TRUE.
      RETURN
    end if

    is_prime = .FALSE.

    ! 1 will result in a false positive, so catch it together
    ! with 0 and all negative numbers.
    if ( candidate < 2 ) RETURN

    divisor = 3
    DO 
      if (mod(candidate, divisor) == 0) RETURN
      divisor = divisor + 2
      if (divisor**2 > candidate) EXIT
    END DO
    is_prime = .TRUE.
    RETURN
  end function is_prime

END MODULE prime_mod

PROGRAM print_p
  USE prime_mod, only: is_prime
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

    if (is_prime(candidate)) then
      counter = counter + 1
      print *, counter, candidate

      ! If we have found enough, exit.
      if (counter >= 50) EXIT outer_loop
    end if
  END DO outer_loop
END PROGRAM print_p
