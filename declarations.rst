Variable Types
==============

Basic types:

+------------+------------------------------------+
| Type       | Example                            |
+============+====================================+
| LOGICAL    | True, False                        |
+------------+------------------------------------+
| INTEGER    | 0, 1, -1, 4711                     |
+------------+------------------------------------+
| REAL       | 0.0, 0.5, -2.1, 3.141529           |
+------------+------------------------------------+
| COMPLEX    | (1.0, 1.0), (0.0, 4.5)             |
+------------+------------------------------------+
| CHARACTER  | "A", "Hello"                       |
+------------+------------------------------------+


Arrays
======

.. code-block:: fortran

   INTEGER, DIMENSION(10) :: my_array
   INTEGER :: my_second_array(10)
   INTEGER, DIMENSION(1:10) :: my_third_array
   INTEGER :: 2D_Array(10, 20)
   INTEGER, DIMENSION(:), ALLOCATABLE :: &
                            alloc_array
 
   allocate(alloc_array(10))
   deallocate(alloc_array)


Arrays (cont'd)
===============

Indices usually start at 1.
In multi-dimensional arrays, the first index is the fastest changing:

(1, 1) (2, 1) (3, 1) (1, 2) (2, 2) (3, 2)

Parameters
==========

Also known as constants:

.. code-block:: fortran

   REAL, PARAMETER :: pi = 3.141592
   REAL :: e
   PARAMETER(e = 2.71)


Variable Kinds
==============

.. code-block:: fortran

   INTEGER, PARAMETER :: dp = &
            SELECTED_REAL_KIND(P = 15)
   REAL(KIND=dp) :: double_precision_var
   REAL*8 :: dp2_var

   double_precision_var = 1.0_dp

Usually, the KIND is the number of bytes required to represent the variable.


CHARACTER
=========

Fixed length strings.

.. code-block:: fortran

   CHARACTER*10 :: string1
   CHARACTER(LEN=20) :: string2

   ! String Concatenation:
   print * , string1 // string2


LOGICAL
=======

Boolean type, .TRUE. or .FALSE.

Note the points on either side.

+-------+-----------------------+
| .NOT. | negates next logical  |
+-------+-----------------------+
| .AND. | .TRUE. if both left   |
|       | and right are true    |
+-------+-----------------------+
| .OR.  | .TRUE. if either left |
|       | or right are true     |
+-------+-----------------------+


Conditionals
============

+------+-----+------------------+
| F77  | F90 | Meaning          |
+======+=====+==================+
| .LT. | \<  | less than        |
+------+-----+------------------+
| .GT. | \>  | greater than     |
+------+-----+------------------+
| .LE. | <=  | less or equal    |
+------+-----+------------------+
| .GE. | >=  | greater or equal |
+------+-----+------------------+
| .EQ. | ==  | equal            |
+------+-----+------------------+
| .NE. | /=  | not equal        |
+------+-----+------------------+


TYPE
====

.. code-block:: fortran

   TYPE :: my_type    ! Declare the type
     INTEGER :: my_int
     REAL :: my_real
   END TYPE my_array

   TYPE(my_type) :: t ! Declare a var of the type

   t % my_int = 1     ! Assign the int variable
   t % my_real = 0.5  ! Assign the real variable

Useful to bundle data together.
