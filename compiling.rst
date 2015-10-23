Writing Code
============

Code is written using a Text Editor.

Not to be confused with Word Processor.
If you can easily change the font, it's a word processor, not a text editor.

Common ones are notebook++ (Windows), vim, emacs, nano.

There are also IDEs (Integrated Development Environments) like eclipse, but they are more involved.

Compiling Steps
===============

Compiling means the translation of human-readable source code into computer instructions.

Very simply put, this is done in three steps:

1) Parsing
2) Compiling
3) Linking


Parsing
=======

The Parser looks at the source code and evaluates exactly what needs to be done.

It also checks the syntax for errors, and might produce some warnings depending on its settings.


Compiling
=========

The parsed source code is converted into machine code.

Depending on the options, it might make certain changes to improve runtime performance.


Linking
=======

The linking step tries to piece the different compiled objects together to create an executable.


Compiling a simple program
==========================

.. code-block:: fortran

   PROGRAM hello
     IMPLICIT NONE
     PRINT *, "Hello World"
   END PROGRAM hello

Such a simple program can be parsed, compiled, and linked in a single step:

.. code-block:: bash

   $ ifort -o hello hello.f90

In this example the program which compiles the code is 'ifort', and the name of the executable is set to
'hello' with the option '-o'.

Compiling a complex program
===========================

If you have a program that contains several source files, you will have to first
compile each one separately, possibly in a certain order, and then link them together at the end:

.. code-block:: bash

   $ ifort -c -o mod_hello.o mod_hello.f90
   $ ifort -c -o hello.o hello.f90
   $ ifort -o hello hello.o mod_hello.o

The -c means that the compiler should stop after the compiler stage, and not try to link just yet.

Compiler Warnings
=================

.. code-block:: bash

   $ ifort -warn all -o hello hello.f90

Warnings mean that while the code is syntactically correct, there's something fishy about it.
Examples: Variables are declared but never used, blocks of code that are never reached.

Compiler Warnings (cont'd)
==========================

Warnings hint at the code not doing what you think it does, meaning bugs.

It's good practice to always enable the compiler's warning messages by adding ``-warn all`` (``ifort``) or ``-Wall`` (``gfortran``). 

You should strive to program in such a way as to not produce warnings.
