Basic Assignment
================

.. code-block:: fortran

   a = 3 + 4 * 2
   b = (3 + 4) * 2
   c = sqrt(2.0)
   i = i + 1

A single equals sign is an assignment opeator. The right hand side is evaluated, and the result assigned to the variable on the left hand side.

While general arithmetic rules are observed, use of parenthesis is encouraged to remove all doubt.

Left hand side must be a single variable.


Assignment Gotcha
=================

.. code-block:: fortran

   real :: a
   a = 3 / 4
   print * , a   ! Will be 0.0

Why is **a** zero? Because both 3 and 4 are integers, so the division is an integer division, and remainders are dropped. 
The result of the evaluation is 0, and that is then typecast to a real 0.00000.

Solution:
=========

.. code-block:: fortran

   real :: a
   a = float(3) / 4
   print * , a    ! Will be 0.75

We need to make at least one of the values a float, either by explicit casting, or by multiplying with 1.0.

Note: This will not work:

.. code-block:: fortran

   a = float(3/4)    ! This doesn't work

because 3/4 is still evaluated to 0 before being cast to a floating point number.
