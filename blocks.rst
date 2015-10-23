IF Statement
============

.. code-block:: fortran

   IF (cond) statement

statement is executed if and only if cond is true, example:

.. code-block:: fortran

   IF (i < 5) PRINT * , "less than five"

Rarely used.


IF Block
========

.. code-block:: fortran

   IF (cond) THEN
     statement1
     statement2
   END IF

Lines between THEN and ENDIF are only executed if cond is .TRUE.


ELSE Block
==========

.. code-block:: fortran

   IF (cond) THEN
     statement1
   ELSE
     statement2
   END IF

if cond is .TRUE., then execute statement1, else statement2


ELSEIF Block
============

.. code-block:: fortran

   IF (cond1) THEN
     statement1
   ELSEIF (cond2) THEN
     statement2
   ELSE
     statement3
   END IF

Note that cond2 is not even checked if cond1 is .TRUE.


LOOPS
=====

.. code-block:: fortran

   ! Old style:
   DO 1000 i = 1, 10
     PRINT * , i
   1000 CONTINUE

   ! New style
   DO i = 1, 10
     PRINT * , i
   END DO


Named Loops
===========

.. code-block:: fortran

   my_loop : DO i = 1, 10
     PRINT * , i
   END DO my_loop

If you name a loop, you have to repeat the name for the END DO line.

In long and nested loops, this helps to keep track of which loop you end.

WHILE Loop:
===========

.. code-block:: fortran

   DO WHILE (i < 10)
     i = i + 1
     PRINT * , i
   END DO

Checks every iteration whether the condition is still valid.

WHERE Block
===========

.. code-block:: fortran

   WHERE (a < 10)    ! a can be a multi-dimensional array
     b = b + 1       ! b must have the same dimensions as a
   ELSEWHERE
     b = b - 1
   END WHERE

You can not do print statements inside a WHERE block.


EXIT Loop
=========

.. code-block:: fortran

   DO
     i = i + 1
     IF (i >= 10) EXIT
     PRINT * , i
   END DO

EXIT ends execution of the loop, jumps to first statement after END DO.

If you nest loops, naming them can clarify which loop to exit.

CYCLE Loop
==========

.. code-block:: fortran

   DO i = 1, 100
     IF (mod(i, 5) == 0) CYCLE
     PRINT * , i
   END DO

CYCLE stops the execution of *this* iteration and begins with the next.
