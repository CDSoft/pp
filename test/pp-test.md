% PP test file (markdown)
% Christophe Delord

Introduction
============

This document is a test file for `pp`.
It is preprocessed and compared with `pp-test.ref`.

The document also computes the test results.

Macros definition
=================

## Definition

----------------------------------- -----------------------------------------------------------------
Definition of `mac1`                \def(mac1)(This is macro `mac1`)
Show macro definition:              \rawdef(mac1)
Show macro value:                   \mac1
----------------------------------- -----------------------------------------------------------------

## Undefinition

----------------------------------- -----------------------------------------------------------------
Undefined macro `mac2`              \undef(mac2)
Show macro definition:              \rawdef(mac2)
Show macro value:                   \mac2
----------------------------------- -----------------------------------------------------------------

## Definition with arguments

----------------------------------- -----------------------------------------------------------------
Definition with arguments:          \define(swap)(!2 !1)
Show macro definition:              \rawdef(swap)
Show macro value:                   \swap(\mac1)(\mac2)
----------------------------------- -----------------------------------------------------------------

## Passing arguments between user macros (see issue #29)

----------------------------------- -----------------------------------------------------------------
A calls B calls C:                  \def(A)(\B(\1))\def(B)(\C(\1))\def(C)(C got \1)\A(42)
same with 3 args:                   \def(A)(\B(\2)(\3)(\1))\def(B)(\C(\2)(\3)(\1))\def(C)(C got \1 \2 \3)\A(42)(43)(44)
----------------------------------- -----------------------------------------------------------------

----------------------------------- -----------------------------------------------------------------
Undefinition:                       \undefine(mac1)\rawdef(mac1)
----------------------------------- -----------------------------------------------------------------

----------------------------------- -----------------------------------------------------------------
Definition test:                    \ifdef(mac1)(mac1 is defined)
Definition test:                    \ifdef(swap)(swap is defined)
Definition test:                    \ifdef(mac1)(mac1 is defined)(mac1 is not defined)
Definition test:                    \ifdef(swap)(swap is defined)(swap is not defined)
Undefinition test:                  \ifndef(mac1)(mac1 is not defined)
Undefinition test:                  \ifndef(swap)(swap is not defined)
Undefinition test:                  \ifndef(mac1)(mac1 is not defined)(mac1 is defined)
Undefinition test:                  \ifndef(swap)(swap is not defined)(swap is defined)
----------------------------------- -------------------------------------------------------------

## Equality / Inequality

\define(one)(1) \define(un)(1) \define(two)(2)

----------------------------------- -----------------------------------------------------------------
Equality test:                      \ifeq(\one)(\un)    (one == un)
Equality test:                      \ifeq(\one)(\two)   (one == two)
Equality test:                      \ifeq(\one)(\un)    (one == un) (one /= un)
Equality test:                      \ifeq(\one)(\two)   (one == two)(one /= two)
Inequality test:                    \ifne(\one)(\un)    (one /= un)
Inequality test:                    \ifne(\one)(\two)   (one /= two)
Inequality test:                    \ifne(\one)(\un)    (one /= un) (one == un)
Inequality test:                    \ifne(\one)(\two)   (one /= two)(one == two)
----------------------------------- -----------------------------------------------------------------

----------------------------------- -----------------------------------------------------------------
Raw text:                           \raw(\swap(a)(b))
Evaluated:                          \swap(a)(b)
----------------------------------- -----------------------------------------------------------------

File inclusion
==============

## Before including `pp-test.i`

----------------------------------- -----------------------------------------------------------------
File name of the main file:         \main
File name of the current file:      \file
----------------------------------- -----------------------------------------------------------------

## Inclusion of `pp-test.i`

\include(pp-test.i)

## After including `pp-test.i`

----------------------------------- -----------------------------------------------------------------
Definitions:                        \answer
----------------------------------- -----------------------------------------------------------------

## Files included without preprocessing

----------------------------------- -----------------------------------------------------------------
Undefinition:                       \undef(answer)
----------------------------------- -----------------------------------------------------------------

\rawinclude(pp-test.i)

----------------------------------- -----------------------------------------------------------------
No definitions:                     \answer
----------------------------------- -----------------------------------------------------------------

## Files imported

In the document:

\undef(answer)

Before importing: answer = \answer
\import(pp-test.i)
After import: answer = \answer

And from the command line:

imported_macro: \imported_macro

Comments and quiet definitions
==============================

## Comments

\comment
~~~~~~~~~~~~~~~~~~~~~~~~~
This is a pretty useless comment for fun.
~~~~~~~~~~~~~~~~~~~~~~~~~

\comment(a comment with a title)
~~~~~~~~~~~~~~~~~~~~~~~~~
This is a pretty useless comment for fun.
~~~~~~~~~~~~~~~~~~~~~~~~~

## Quiet definitions

\quiet(Here are some definitions)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pi is: \def(pi)(3.14)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

----------------------------------- -----------------------------------------------------------------
Definitions:                        \pi
----------------------------------- -----------------------------------------------------------------

File modification date
======================

\sh
```
touch -d "2010-06-22 12:00:00" /tmp/pp-test-f1
touch -d "2010-06-23 12:00:00" /tmp/pp-test-f2
touch -d "2010-06-21 12:00:00" /tmp/pp-test-f3
```

----------------------------------- -----------------------------------------------------------------
Current file date:                  \ifeq(\mdate)(\exec(LANG=en date +"%A %e %B %Y" -r \file))(mdate = date of \main)(mdate = \mdate /= date of \main)
Specific file date:                 \mdate(/tmp/pp-test-f1 /tmp/pp-test-f2)(/tmp/pp-test-f3)
----------------------------------- -----------------------------------------------------------------

Environment variables
=====================

----------------------------------- -----------------------------------------------------------------
Undefined environment variable:     \env(EMPTYENVVAR)
Environment variable:               \env(TESTENVVAR)
----------------------------------- -----------------------------------------------------------------

OS architecture
===============

----------------------------------- -----------------------------------------------------------------
OS:                                 \os
Architecture:                       \arch
----------------------------------- -----------------------------------------------------------------

Simple arithmetic
=================

----------------------------------- -----------------------------------------------------------------
undefined + 1 =                     \add(x)\x
1 + 1 =                             \add(x)\x
undefined + 3 =                     \add(y)(3)\y
3 + 4 =                             \add(y)(4)\y
----------------------------------- -----------------------------------------------------------------

Dialect, Output language and output format
==========================================

## Dialect

----------------------------------- -----------------------------------------------------------------
The current dialect is md:          \dialect
All dialects:                       \dialects
Section for a markdown document:    \md(Hello World!)
Section for a rst document:         \rst(Hello World!)
----------------------------------- -----------------------------------------------------------------

## Language

----------------------------------- -----------------------------------------------------------------
The current language is "en":       \lang
All languages:                      \langs
Section in english:                 \en(Hello World!)
Section in french:                  \fr(Bonjour le monde !)
----------------------------------- -----------------------------------------------------------------

## Format

----------------------------------- -----------------------------------------------------------------
The current format is HTML:         \format
All formats:                        \formats
Section for an HTML document:       \html(Hello World!)
Section for a PDF document:         \pdf(Hello World!)
----------------------------------- -----------------------------------------------------------------

External commands and scripts execution
=======================================

\def(mymacro)(mymacro(!1) has been evaluated!)

----------------------------------- ------------------------------------------------------------------------------
Command line (uses sh):             exec(echo "hi é")
`sh` script:                        \sh(echo "hi é")
`bash` script:                      \bash(echo hi à)
`bat` script:                       \bat(echo hi ç)
`cmd` script:                       \cmd(echo hi ç)
`python` script:                    \python(print("hi"))
`python2` script:                   \python2(print "hi")
`python3` script:                   \python3(print("hi"))
`haskell` script:                   \haskell(main = putStrLn "hi")
`exec` script:                      \exec(\raw(echo "\\def(x)(42)\\x"))
`rawexec` script:                   \rawexec(\raw(echo "\\def(x)(42)\\x"))
Raw script output:                  \sh(printf "\\%s(%d)" mymacro 42)
Preprocessed script output:         \pp[\sh(printf "\\%s(%d)" mymacro 42)]
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

`\test` outputs:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\result
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The complete source files are:

`mylib.h`:

\lit{.stack-work/mylib.h}

`mylib.c`:

\lit{.stack-work/mylib.c}

`mylibtest.c`:

\lit{.stack-work/mylibtest.c}

A source file, not necessarily generated with \raw(\lit) can be
formated with \raw(\source or \src):

`mylib.h`:

\src{.stack-work/mylib.h}

And with a different codeblock separator:

15 '~':

\codeblock(15)(~)\src{.stack-work/mylib.h}

20 '`':

\codeblock(20)(`)\src{.stack-work/mylib.h}

25 '~' (default character):

\codeblock(25)\src{.stack-work/mylib.h}

CSV tables
==========

This file with a header:

\lit(/tmp/table1.csv)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Year,Make,Model,Description,Price
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\flushlit

is rendered by `\raw(\csv(file.csv))` as:

\csv(/tmp/table1.csv)

This file without any header:

\lit(/tmp/table2.csv)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\flushlit

is rendered by `\raw(\csv(file.csv)(Year|Make|Model|Description|Price))` as:

\csv(/tmp/table2.csv)(Year|Make|Model|Description|Price)

