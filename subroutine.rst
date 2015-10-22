SUBROUTINE
==========

.. code-block:: fortran

   SUBROUTINE print_hello()
     IMPLICIT NONE
     PRINT *, "Hello"
   END SUBROUTINE print_hello

   PROGRAM my_program
     IMPLICIT NONE
     CALL print_hello()
   END PROGRAM my_program


Arguments
=========

.. code-block:: fortran

   SUBROUTINE greet(cname)
     IMPLICIT NONE
     CHARACTER(len=*) :: cname
     PRINT *, "Hello " // cname
   END SUBROUTINE greet

   CALL greet("Holger")
   CALL greet(cname="Holger")


INTENT
======

.. code-block:: fortran

   SUBROUTINE my_sub(a, b, c)
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: a
     INTEGER, INTENT(OUT) :: b
     INTEGER, INTENT(INOUT) :: c
     ...
   END SUBROUTINE


INTENT (cont'd)
===============

+--------+-----------------------------------+
| INTENT | MEANING                           |
+========+===================================+
| IN     | Subroutine won't change value.    |
+--------+-----------------------------------+
| OUT    | Subroutine will set this to a new |
|        | value.                            |
+--------+-----------------------------------+
| INOUT  | Subroutine takes this value and   |
|        | might change it.                  |
+--------+-----------------------------------+

Helps the compiler to spot bugs.


FUNCTION
========

.. code-block:: fortran

   FUNCTION my_func(a)
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: a
     INTEGER :: my_func
     my_func = ...
   END FUNCTION my_func

   b = my_func(c)

Inside the function body, the function name can be used like a normal variable.


FUNCTION (cont'd)
=================

.. code-block:: fortran

   FUNCTION my_func() result(r)
     IMPLICIT NONE
     INTEGER :: r
     ...
     r = ...
   END FUNCTION


RETURN
======

Both functions and subroutines can have one or more RETURN statements.
When a RETURN is encountered, the procedure stops executing and passes control
back to the calling code.

.. code-block:: fortran
   
   SUBROUTINE my_abs(a)
     IMPLICIT NONE
     INTEGER, INTENT(INOUT) :: a
     IF (a >= 0) RETURN
     a = -a
   END SUBROUTINE my_abs


Declaration of Procedures
=========================

Three possible ways to declare a procedure (function or subroutine):

1) Independent of the program
2) Inside the program's contains area
3) Inside a MODULE

Independent
===========

.. code-block:: fortran

   SUBROUTINE my_sub(a)
     ...
   END SUBROUTINE my_sub

   PROGRAM my_program
     CALL my_sub(a)
   END PROGRAM my_sub

Procedure knows nothing about program and vice versa.
Hope the arguments are correct, because the compiler won't know.

(Interfaces can help, but they are a lot of overhead.)

CONTAINS
========

.. code-block:: fortran

   PROGRAM my_program
     ...
   CONTAINS
     SUBROUTINE my_sub()
       ...
     END SUBROUTINE my_sub
   END PROGRAM my_program

* Compiler can check whether used correctly
* Subroutine has access to program's variables
