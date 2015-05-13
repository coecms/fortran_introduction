.. include:: <s5defs.txt>

==============
Fortran Basics
==============

:Author: Holger Wolff
:Date: 27/04/2015


.. |bullet| unicode:: U+02022
.. footer:: Monash |bullet| 27/04/2015


What is Fortran?
================

* FORmula TRANslation
* Around since 1952
* Procedural programming
* Versions usually denoted by year of declaration:
  eg. 66, 77, 90, 95, 03, 08
* Most important versions for us: 77 to 95.
  (You shouldn't write 77, but you will see its ugly heads)

Fortran 77
==========

* Fixed Form: 

Example F77
===========

.. code-block:: fortran

       PROGRAM HELLO
           IMPLICIT NONE
           INTEGER i
 C         This is a comment
           DO 100 i = 1, 10
             PRINT *, "I am in iteration ", i, 
      &          " of the loop"
  100      CONTINUE
       END PROGRAM
