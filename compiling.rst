Compiling Steps
===============

Compiling means the translation of human-readable source code into computer instructions.

Very simply put, this is done in three steps:

1) Parsing
2) Compiling
3) Linking


Parsing
=======

The Parser looks at the source code and evaluates exactly what is needed to be done.

It also checks the syntax for errors, and might produce some warnings depending on its settings.


Compiling
=========

The compiler parsed data and converts it into machine code.

Depending on the options, it might make certain changes to improve runtime performance.


Linking
=======

The linking step tries to piece the different compiled objects together.


Compiling a simple program
==========================

.. code-block:: fortran

   PROGRAM hello
     IMPLICIT NONE
     PRINT *, "Hello World"
   END PROGRAM hello

Such a simple program can be parsed, compiled, and linked in a single step:

.. code-block:: bash

   $ ifort -warn all -o hello hello.f90

It's good practice to always enable the compiler's warning messages by adding `-warn all`. This makes
sure your code follows the Fortran standards and checks for common programming errors.

Compiling a complex program
===========================

If you have a program that contains of several source files, you will have to first
compile each one separately, possibly in a certain order, and then link them together at the end:

.. code-block:: bash

   $ ifort -warn all -c -o mod_hello.o mod_hello.f90
   $ ifort -warn all -c -o hello.o hello.f90
   $ ifort -warn all -o hello hello.o mod_hello.o

The -c means that the compiler should stop after the compiler stage, and not try to link just yet.

