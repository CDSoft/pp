=================================
 PP test file (reStructuredText)
=================================

Introduction
============

This document is a test file for ``pp``.
It is preprocessed and compared with ``pp-test-rst.ref``.

The document also computes the test results.

\define(nbok)(0)    \define(ok)(OK\add(nbok))
\define(nberr)(0)   \define(err)(ERROR\add(nberr))
\define(eq)(\ifeq(\1)(\2)(\ok)(\err))
\define(ne)(\ifne(\1)(\2)(\ok)(\err))
\define(isdef)(\ifdef(\1)(\ok)(\err))
\define(isndef)(\ifndef(\1)(\ok)(\err))

Dialect, Output language and output format
==========================================

Dialect
-------

=================================== =================================================================
The current dialect is rst:         \eq(\dialect)(rst)
Section for a markdown document:    \eq(\md(Hello World!))()
Section for a rst document:         \eq(\rst(Hello World!))(Hello World!)
=================================== =================================================================

Language
--------

=================================== =================================================================
The current language is "en":       \eq(\lang)(en)
Section in english:                 \eq(\en(Hello World!))(Hello World!)
Section in french:                  \eq(\fr(Bonjour le monde !))()
=================================== =================================================================

Format
------

=================================== =================================================================
The current format is HTML:         \eq(\format)(html)
Section for an HTML document:       \eq(\html(Hello World!))(Hello World!)
Section for a PDF document:         \eq(\pdf(Hello World!))()
=================================== =================================================================

Diagrams
========

The diagram test does not check the generated images, just the links in the output document.

\dot        (dot-test)         (Test of dot)       ( digraph { dot -> { A B } -> C -> dot } )
\neato      (neato-test)       (Test of neato)     ( digraph { neato -> { A B } -> C -> neato } )
\twopi      (twopi-test)       (Test of twopi)     ( digraph { twopi -> { A B } -> C -> twopi } )
\circo      (circo-test)       (Test of circo)     ( digraph { circo -> { A B } -> C -> circo } )
\fdp        (fdp-test)         (Test of fdp)       ( digraph { fdp -> { A B } -> C -> fdp } )
\patchwork  (patchwork-test)   (Test of patchwork) ( digraph { patchwork -> { A B } -> C } )
\osage      (osage-test)       (Test of osage)     ( digraph { osage -> { A B } -> C -> osage } )

\uml        (uml-test)         (Test of uml)       ( sender -> receiver )

\ditaa      (ditaa-test)       (Test of ditaa)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    +--------+   +-------+    +-------+
    |        | --+ ditaa +--> |       |
    |  Text  |   +-------+    |diagram|
    |Document|   |!magic!|    |       |
    |     {d}|   |       |    |       |
    +---+----+   +-------+    +-------+
        :                         ^
        |       Lots of work      |
        +-------------------------+

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\dot    (size_small    {:width: 50})                        (digraph { small })

\dot    (size_smallt   {:width: 50})    (Small with title)  (digraph { small })

\dot    (size_normal   {:width: 100})                       (digraph { normal })

\dot    (size_normalt  {:width: 100})   (Normal with title) (digraph { normal })

\dot    (size_big      {:width: 150})                       (digraph { big })

\dot    (size_bigt     {:width: 150})   (Big with title)    (digraph { big })

Literate programming
====================

Lets write and test a useful library:

\lit{.stack-work/mylib.h}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This is a C library
@functionDeclarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\lit{.stack-work/mylib.c}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This is a C library
@functionImplementations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``fib`` function computes Fibonacci's numbers:

\lit{@functionDeclarations}{C}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int fib(int n);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\lit{@functionImplementations}{C}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int fib(int n)
{
    return (n < 2) ? 1 : fib(n-1) + fib(n-2);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``fact`` function computes factorial numbers:

\lit{@functionDeclarations}{int fact(int n);}
\lit{@functionImplementations}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int fact(int n)
{
    return (n <= 1) ? 1 : n * fact(n-1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some tests of ``mylib.c``:

\lit{.stack-work/mylibtest.c}{C}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include <stdio.h>
#include <stdlib.h>

#include "mylib.h"

int main(int argc, char * argv[])
{
    int i;
    for (i = 1; i < argc; i++)
    {
        int n = atoi(argv[i]);
        printf("fact(%d) = %3d; fib(%d) = %3d\n", n, fact(n), n, fib(n));
    }
    return 0;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\exec(gcc .stack-work/mylib.c .stack-work/mylibtest.c -o .stack-work/mylibtest)

\def(test)(mylibtest 0 1 2 3 4 5)
\def(result)(\exec(.stack-work/\test))

``\test`` outputs::

\indent(4)(\result)

\test:     \eq(\result)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fact(0) =   1; fib(0) =   1
fact(1) =   1; fib(1) =   1
fact(2) =   2; fib(2) =   2
fact(3) =   6; fib(3) =   3
fact(4) =  24; fib(4) =   5
fact(5) = 120; fib(5) =   8
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The complete source files are:

``mylib.h``:

\lit{.stack-work/mylib.h}

``mylib.c``:

\lit{.stack-work/mylib.c}

``mylibtest.c``:

\lit{.stack-work/mylibtest.c}

A source file, not necessarily generated with \raw(\lit) can be
formated with \raw(\source or \src):

``mylib.h``:

\src{.stack-work/mylib.h}

Test results
============

=============================== ========================
Number of successful tests:     \nbok
Number of failures:             \nberr
=============================== ========================

\ifeq(\nberr)(0)(All tests passed!)(\nberr tests failed!)
