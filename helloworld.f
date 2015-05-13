      PROGRAM HELLO
          IMPLICIT NONE
          INTEGER i
C         This is a comment
          DO 100 i = 1, 10
            PRINT *, "I am in iteration ", i, 
     &          " of the loop"
100       CONTINUE
      END PROGRAM
