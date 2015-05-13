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

Fixed Form: 

* First character: comment (Usually c, C, or * )
* 1st-5th character: label (if number)
* 6th character: continuation line
* 7th-72nd characters: Statements

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

Fortran 90
==========

* Free Form
* Comments start with !
* DO - END DO
* modules
* Still 100% backwards compatible with F77

Example F90
===========

.. code-block:: fortran

  program hello
    implicit none
    integer :: i    ! This is a comment
    do i = 1, 10
      print *, "This is iteration", i, &
          " of the loop."
    end do
  end program hello


Fortran 95
==========

* No more 

  * DO with non-INT vars
  * ASSIGNed GOTOs
  * Some more

* FORALL and nested WHERE
* PURE and ELEMENTAL functions

Newer Fortrans
==============

Fortran 03
----------

Object oriented, Input/Output streams, ...

Fortran 08
----------

submodules, DO CONCURRENT, ...

Fortran 15
----------

???
