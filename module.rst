MODULE
======

.. code-block:: fortran
   
   MODULE my_module
     IMPLICIT NONE
     INTEGER :: my_int
   CONTAINS
     SUBROUTINE my_sub(a)
       ...
     END SUBROUTINE my_sub
     FUNCTION my_func(b)
       ...
     END FUNCTION my_func
   END MODULE my_module


MODULE (cont'd)
===============

Every routine using a module as well as every procedure inside the module has access to all its variables and procedures.
This makes modules a good way to share data between different routines.

When calling procedures from a module, the compiler can check whether the call arguments are correct.

USE
===

Modules are used by the USE statement **before** the IMPLICIT NONE:

.. code-block:: fortran

   PROGRAM my_prog
     USE my_module
     IMPLICIT NONE
     INTEGER :: i
     ...
   END PROGRAM my_prog


ONLY
====

You can limit which items of the module you want to use with the ONLY keyword:

.. code-block:: fortran

   USE my_module, ONLY: my_sub, my_func




