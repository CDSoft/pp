=================================
 PP test file (reStructuredText)
=================================

Introduction
============

This document is a test file for ``pp``.
It is preprocessed and compared with ``pp-test-rst.ref``.

The document also computes the test results.

Dialect, Output language and output format
==========================================

Dialect
-------

=================================== =================================================================
The current dialect is rst:         !dialect
All dialects:                       !dialects
Section for a markdown document:    !md(Hello World!)
Section for a rst document:         !rst(Hello World!)
=================================== =================================================================

Language
--------

=================================== =================================================================
The current language is "en":       !lang
All languages:                      !langs
Section in english:                 !en(Hello World!)
Section in french:                  !fr(Bonjour le monde !)
=================================== =================================================================

Format
------

=================================== =================================================================
The current format is HTML:         !format
All formats:                        !formats
Section for an HTML document:       !html(Hello World!)
Section for a PDF document:         !pdf(Hello World!)
=================================== =================================================================

Diagrams
========

The diagram test does not check the generated images, just the links in the output document.

!dot        (dot-test)         (Test of dot)       ( digraph { dot -> { A B } -> C -> dot } )
!neato      (neato-test)       (Test of neato)     ( digraph { neato -> { A B } -> C -> neato } )
!twopi      (twopi-test)       (Test of twopi)     ( digraph { twopi -> { A B } -> C -> twopi } )
!circo      (circo-test)       (Test of circo)     ( digraph { circo -> { A B } -> C -> circo } )
!fdp        (fdp-test)         (Test of fdp)       ( digraph { fdp -> { A B } -> C -> fdp } )
!patchwork  (patchwork-test)   (Test of patchwork) ( digraph { patchwork -> { A B } -> C } )
!osage      (osage-test)       (Test of osage)     ( digraph { osage -> { A B } -> C -> osage } )

!uml        (uml-test)         (Test of uml)       ( sender -> receiver )

!ditaa      (ditaa-test)       (Test of ditaa)
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

!asy        (asy-test)         (Test of Asymptote)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import math;
size(7cm,0);
add(grid(7,3));
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!dot    (size_small    {:width: 50})                        (digraph { small })

!dot    (size_smallt   {:width: 50})    (Small with title)  (digraph { small })

!dot    (size_normal   {:width: 100})                       (digraph { normal })

!dot    (size_normalt  {:width: 100})   (Normal with title) (digraph { normal })

!dot    (size_big      {:width: 150})                       (digraph { big })

!dot    (size_bigt     {:width: 150})   (Big with title)    (digraph { big })

Literate programming
====================

Lets write and test a useful library:

!lit{.stack-work/mylib.h}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This is a C library
@functionDeclarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!lit{.stack-work/mylib.c}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This is a C library
@functionImplementations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``fib`` function computes Fibonacci's numbers:

!lit{@functionDeclarations}{C}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int fib(int n);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!lit{@functionImplementations}{C}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int fib(int n)
{
    return (n < 2) ? 1 : fib(n-1) + fib(n-2);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``fact`` function computes factorial numbers:

!lit{@functionDeclarations}{int fact(int n);}
!lit{@functionImplementations}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int fact(int n)
{
    return (n <= 1) ? 1 : n * fact(n-1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some tests of ``mylib.c``:

!lit{.stack-work/mylibtest.c}{C}
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

!exec(gcc .stack-work/mylib.c .stack-work/mylibtest.c -o .stack-work/mylibtest)

!def(test)(mylibtest 0 1 2 3 4 5)
!def(result)(!exec(.stack-work/!test))

``!test`` outputs::

!indent(4)(!result)

The complete source files are:

``mylib.h``:

!lit{.stack-work/mylib.h}

``mylib.c``:

!lit{.stack-work/mylib.c}

``mylibtest.c``:

!lit{.stack-work/mylibtest.c}

A source file, not necessarily generated with !raw(!lit) can be
formated with !raw(!source or !src):

``mylib.h``:

!src{.stack-work/mylib.h}

CSV tables
==========

This file with a header:

!lit(/tmp/table1.csv)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Year,Make,Model,Description,Price
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!flushlit

is rendered by `!raw(!csv(file.csv))` as:

!csv(/tmp/table1.csv)

This file without any header:

!lit(/tmp/table2.csv)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!flushlit

is rendered by `!raw(!csv(file.csv)(Year|Make|Model|Description|Price))` as:

!csv(/tmp/table2.csv)(Year|Make|Model|Description|Price)

