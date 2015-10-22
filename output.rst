Input/Output
============

How to process input and output?

* print
* write
* read

print
=====

.. code-block:: fortran

    print * , "Hello", var1, 'boop'

The simplest way to write values to the screen.
Fortran simply dumps everything onto the screen.

This is equivalent to write (\*, \*)

write
=====

.. code-block:: fortran

    write(*, *) "Hello", var1, 'boop'

is equivalent to **print \*,**. Note that there is no comma any more between the closing parenthesis and the first thing to write.

The two asterisks stand for the default unit (standard output) and default format.

format
======

The second argument of the write statement is the format. 
There are 3 ways to describe the format:

1. As a literal string
2. A string variable
3. A label pointing to a **FORMAT** statement


FORMAT format
=============

.. code-block:: fortran

    Example: (A2, I5, F6.2, A, I3.3)

+-------------+-------------------------------------------------------------+
| Identifier  | Description                                                 |
+=============+=============================================================+
| In.w        | Integer. n=total length, w=minimum length (prepend zeros)   |
+-------------+-------------------------------------------------------------+
| Fn.w        | Floating point number. n=total length, w=decimal digits     |
+-------------+-------------------------------------------------------------+
| En.w        | Floating poing number, scientific notation.                 |
|             | n=total length, w=decimal digits                            |
+-------------+-------------------------------------------------------------+
| An          | String, n=total length                                      |
+-------------+-------------------------------------------------------------+

**Warning:** if n is too small to hold the value, only asterisks are printed.

Examples
========

.. code-block:: fortran

    write(* , '(A5, I3.3, A1)') "Hello", 12, "!"
    ! Prints "Hello012!"

    b = 1.5
    fmt="(A, F6.3)"
    write(* , fmt) "Value of b is ", b
    ! Prints "Value of b is   1.500"

    c = 1200
    write(* , 101) "Value of c is ", c
    101  FORMAT(A, I3)
    ! Prints "Value of c is ***" because 
    ! 3 digits is not enough to display 1200


READ
====

Read is similar to write.
The syntax is the same, the format is the same, except that it reads data from the unit and writes it to the corresponding variables.

.. code-block:: fortran

    integer :: a, b
    read(* , '(2I4)') a, b


Writing to Variable
===================

Instead of standard output, you can write into a character variable:

.. code-block:: fortran

    character(len=20) :: text

    write(text, '(A, X, A)') "Hello", "Students"
    print * , text ! prints "Hello Students"

You can use this to create a format string to use in the real output.


File I/O
========

.. code-block:: fortran

    INTEGER, PARAMETER :: out_unit = 300 ! unique number, preferably >10

    open(unit = out_unit, file = "test.txt", action = "WRITE")
    write(out_unit, '(A)') "This is written to the file"
    close(out_unit)

OPEN has a lot of optional parameters, when you need to use them, google the usage.

Standard output is also a UNIT, often 1 or 6, depending on the operating system and compiler.
Because of this, I usually just declare some parameter with a random, large number, and that's my unit.
