DEPRECIATED
===========

There are some statements that once were very useful, and you might still come across them.
While you need to understand what they do, you should be very wary of using them yourself.

There are now better methods for achieving the same or better results.


CONTINUE
========

CONTINUE does nothing. 
Usually used in conjuction with a label, so that there is no ambiguity as to whether the statement on that line is executed or not if jumped there.


COMMON
======

Common blocks were used as global variables, that is variables that are available
to all procedures without having to pass them through the parameters.

Use MODULEs instead.


EQUIVALENCE
===========

Names variables that should share their memory locations.
Changing one of these variables changes the other one as well.

Was useful before computers had huge memories: You could reuse memory.

While some people still argue that there are reasonable uses for EQUIVALENCE, you shouldn't worry about that.


DATA
====

Used to initialise variables. 

Not sure why that would be better than just assigning initialisation values to the variables at the beginning of the execution instructions.


GOTO
====

GOTO interrupts the flow of the program and jumps to a different location in the code.

The destination is a label (read integer number) in the same procedure from where program execution picks up again.

This label can be both ahead or behind the GOTO statement.

GOTO usually interrupts the flow of the program, so many programmers despise it.
However, it is okay to use it in case of an error to jump to the end of a procedure in order to clean up (eg. close files).
