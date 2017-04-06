% PP test file (markdown)
% Christophe Delord

Introduction
============

This document is a test file for `pp`.
It is preprocessed and compared with `pp-test.ref`.

The document also computes the test results.

\define(nbok)(0)    \define(ok)(OK\add(nbok))
\define(nberr)(0)   \define(err)(ERROR\add(nberr))
\define(eq)(\ifeq(\1)(\2)(\ok)(\err))
\define(ne)(\ifne(\1)(\2)(\ok)(\err))
\define(isdef)(\ifdef(\1)(\ok)(\err))
\define(isndef)(\ifndef(\1)(\ok)(\err))

Macros definition
=================

## Definition

----------------------------------- -----------------------------------------------------------------
Definition:                         \def(mac1)(This is macro `mac1`)\isdef(mac1)
Its definition is correct:          \eq(\rawdef(mac1))(This is macro `mac1`)
Its evaluation is correct:          \eq(\mac1)(This is macro `mac1`)
----------------------------------- -----------------------------------------------------------------

## Undefinition

----------------------------------- -----------------------------------------------------------------
Test of an undefined macro:         \isndef(mac2)
Evaluation of an undefined macro:   \eq(\mac2)(\raw(\mac2))
----------------------------------- -----------------------------------------------------------------

## Definition with arguments

----------------------------------- -----------------------------------------------------------------
Definition with arguments:          \define(swap)(!2 !1)\isdef(swap)
Its definition is correct:          \eq(\rawdef(swap))(\raw(!2 !1))
Its evaluation is correct:          \eq(\swap(\mac1)(\mac2))(\mac2This is macro `mac1`)
----------------------------------- -----------------------------------------------------------------

----------------------------------- -----------------------------------------------------------------
Undefinition:                       \undefine(mac1)\isndef(mac1)
----------------------------------- -----------------------------------------------------------------

----------------------------------- -----------------------------------------------------------------
Definition test:                    \eq(\ifdef(mac1)(mac1 is defined)                       )()
Definition test:                    \eq(\ifdef(swap)(swap is defined)                       )(swap is defined)
Definition test:                    \eq(\ifdef(mac1)(mac1 is defined)(mac1 is not defined)  )(mac1 is not defined)
Definition test:                    \eq(\ifdef(swap)(swap is defined)(swap is not defined)  )(swap is defined)
Undefinition test:                  \eq(\ifndef(mac1)(mac1 is not defined)                  )(mac1 is not defined)
Undefinition test:                  \eq(\ifndef(swap)(swap is not defined)                  )()
Undefinition test:                  \eq(\ifndef(mac1)(mac1 is not defined)(mac1 is defined) )(mac1 is not defined)
Undefinition test:                  \eq(\ifndef(swap)(swap is not defined)(swap is defined) )(swap is defined)
----------------------------------- -----------------------------------------------------------------

## Equality / Inequality

\define(one)(1) \define(un)(1) \define(two)(2)

----------------------------------- -----------------------------------------------------------------
Equality test:                      \eq(\ifeq(\one)(\un)(one == un)                 )(one == un)
Equality test:                      \eq(\ifeq(\one)(\two)(one == two)               )()
Equality test:                      \eq(\ifeq(\one)(\un)(one == un)(one /= un)      )(one == un)
Equality test:                      \eq(\ifeq(\one)(\two)(one == two)(one /= two)   )(one /= two)
Inequality test:                    \eq(\ifne(\one)(\un)(one /= un)                 )()
Inequality test:                    \eq(\ifne(\one)(\two)(one /= two)               )(one /= two)
Inequality test:                    \eq(\ifne(\one)(\un)(one /= un)(one == un)      )(one == un)
Inequality test:                    \eq(\ifne(\one)(\two)(one /= two)(one == two)   )(one /= two)
----------------------------------- -----------------------------------------------------------------

----------------------------------- -----------------------------------------------------------------
Raw text:                           \ne(\raw(\swap(a)(b)))(ba) `\raw(\swap(a)(b))` is not evaluated
----------------------------------- -----------------------------------------------------------------

File inclusion
==============

## Before including `pp-test.i`

----------------------------------- -----------------------------------------------------------------
File name of the main file:         \eq(\main)(test/pp-test.md)
File name of the current file:      \eq(\file)(\main)
----------------------------------- -----------------------------------------------------------------

## Inclusion of `pp-test.i`

\include(pp-test.i)

## After including `pp-test.i`

----------------------------------- -----------------------------------------------------------------
Definitions:                        \eq(\answer)(42)
----------------------------------- -----------------------------------------------------------------

## Files included without preprocessing

----------------------------------- -----------------------------------------------------------------
Undefinition:                       \undef(answer)\isndef(answer)
----------------------------------- -----------------------------------------------------------------

\rawinclude(pp-test.i)

----------------------------------- -----------------------------------------------------------------
No definitions:                     \isndef(answer)
----------------------------------- -----------------------------------------------------------------

File modification date
======================

----------------------------------- -----------------------------------------------------------------
Current file date:                  \eq(\mdate)(\exec(LANG=en date +"%A %e %B %Y" -r \file))
Specific file date:                 \eq(\mdate(test/pp-test.md test/pp-test.i))(\exec(LANG=en date +"%A %e %B %Y" -r test/$(ls -t test | head -1)))
----------------------------------- -----------------------------------------------------------------

Environment variables
=====================

----------------------------------- -----------------------------------------------------------------
Environment undefined variable:     \eq(\env(EMPTYENVVAR))(\exec(echo $EMPTYENVVAR))
Environment variable:               \eq(\env(TESTENVVAR))(\exec(echo $TESTENVVAR))
----------------------------------- -----------------------------------------------------------------

Simple arithmetic
=================

----------------------------------- -----------------------------------------------------------------
undefined + 1 = 1:                  \add(x)\eq(\x)(1)
1 + 1 = 2:                          \add(x)\eq(\x)(2)
undefined + 3 = 3:                  \add(y)(3)\eq(\y)(3)
3 + 4 = 7:                          \add(y)(4)\eq(\y)(7)
----------------------------------- -----------------------------------------------------------------

Dialect, Output language and output format
==========================================

## Dialect

----------------------------------- -----------------------------------------------------------------
The current dialect is md:          \eq(\dialect)(md)
Section for a markdown document:    \eq(\md(Hello World!))(Hello World!)
Section for a rst document:         \eq(\rst(Hello World!))()
----------------------------------- -----------------------------------------------------------------

## Language

----------------------------------- -----------------------------------------------------------------
The current language is "en":       \eq(\lang)(en)
Section in english:                 \eq(\en(Hello World!))(Hello World!)
Section in french:                  \eq(\fr(Bonjour le monde !))()
----------------------------------- -----------------------------------------------------------------

## Format

----------------------------------- -----------------------------------------------------------------
The current format is HTML:         \eq(\format)(html)
Section for an HTML document:       \eq(\html(Hello World!))(Hello World!)
Section for a PDF document:         \eq(\pdf(Hello World!))()
----------------------------------- -----------------------------------------------------------------

External commands and scripts execution
=======================================

\def(mymacro)(mymacro(!1) has been evaluated!)

----------------------------------- ------------------------------------------------------------------------------
Command line (uses sh):             \eq(\exec(echo "hi é"))(hi é)
`sh` script:                        \eq(\sh(echo "hi é"))(hi é)
`bash` script:                      \eq(\bash(echo hi à))(hi à)
`bat` script:                       \eq(\bat(echo hi ç))(hi ç)
`cmd` script:                       \eq(\cmd(echo hi ç))(hi ç)
`python` script:                    \eq(\python(print("hi")))(hi)
`python2` script:                   \eq(\python2(print "hi" ))(hi)
`python3` script:                   \eq(\python3(print("hi")))(hi)
`haskell` script:                   \eq(\haskell(main = putStrLn "hi"))(hi)
`exec` script:                      \eq(\exec(\raw(echo "\\def(x)(42)\\x")))(42)
`rawexec` script:                   \eq(\rawexec(\raw(echo "\\def(x)(42)\\x")))(\raw(\def(x)(42)\x))
Raw script output:                  \eq(\sh(printf "\\%s(%d)" mymacro 42))(\raw(\mymacro(42)))
Preprocessed script output:         \eq(\pp[\sh(printf "\\%s(%d)" mymacro 42)])(mymacro(42) has been evaluated!)
----------------------------------- ------------------------------------------------------------------------------

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

\dot    (size_small    {.img width=50})                        (digraph { small })

\dot    (size_smallt   {.img width=50})    (Small with title)  (digraph { small })

\dot    (size_normal   {.img width=100})                       (digraph { normal })

\dot    (size_normalt  {.img width=100})   (Normal with title) (digraph { normal })

\dot    (size_big      {.img width=150})                       (digraph { big })

\dot    (size_bigt     {.img width=150})   (Big with title)    (digraph { big })

Literate programming
====================

Lets write and test a useful library:

\lit{.build/mylib.h}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This is a C library
@functionDeclarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\lit{.build/mylib.c}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This is a C library
@functionImplementations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `fib` function computes Fibonacci's numbers:

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

The `fact` function computes factorial numbers:

\lit{@functionDeclarations}{int fact(int n);}
\lit{@functionImplementations}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int fact(int n)
{
    return (n <= 1) ? 1 : n * fact(n-1);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some tests of `mylib.c`:

\lit{.build/mylibtest.c}{C}
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

\exec(gcc .build/mylib.c .build/mylibtest.c -o .build/mylibtest)

\def(test)(mylibtest 0 1 2 3 4 5)
\def(result)(\exec(.build/\test))

`\test` outputs:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\result
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

`mylib.h`:

\lit{.build/mylib.h}

`mylib.c`:

\lit{.build/mylib.c}

`mylibtest.c`:

\lit{.build/mylibtest.c}

A source file, not necessarily generated with \raw(\lit) can be
formated with \raw(\source or \src):

`mylib.h`:

\src{.build/mylib.h}

And with a different codeblock separator:

15 '~':

\codeblock(15)(~)\src{.build/mylib.h}

20 '`':

\codeblock(20)(`)\src{.build/mylib.h}

25 '~' (default character):

\codeblock(25)\src{.build/mylib.h}

Test results
============

------------------------------- ------------------------
Number of successful tests:     \nbok
Number of failures:             \nberr
------------------------------- ------------------------

\ifeq(\nberr)(0)(All tests passed!)(\nberr tests failed!)
