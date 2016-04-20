% PP test file
% Christophe Delord
% \mdate

[pp-ref.md]: pp-ref.md

Introduction
============

This document is a test file for `pp`.
It is preprocessed and compared with [pp-ref.md].

The document also computes the test results.

\define(nbok)(0)    \define(ok)(OK\add(nbok))
\define(nberr)(0)   \define(err)(ERROR\add(nberr))
\define(eq)(\ifeq(\1)(\2)(\ok)(\err))
\define(ne)(\ifne(\1)(\2)(\ok)(\err))
\define(isdef)(\ifdef(\1)(\ok)(\err))
\define(isndef)(\ifndef(\1)(\ok)(\err))

Macros definition
=================

Definition:                         \def(mac1)(This is macro `mac1`)\isdef(mac1)
Its definition is correct:          \eq(\rawdef(mac1))(This is macro `mac1`)
Its evaluation is correct:          \eq(\mac1)(This is macro `mac1`)

Test of an undefined macro:         \isndef(mac2)
Evaluation of an undefined macro:   \eq(\mac2)(\raw(\mac2))

Definition with arguments:          \define(swap)(!2 !1)\isdef(swap)
Its definition is correct:          \eq(\rawdef(swap))(\raw(!2 !1))
Its evaluation is correct:          \eq(\swap(\mac1)(\mac2))(\mac2This is macro `mac1`)

Undefinition:                       \undefine(mac1)\isndef(mac1)

Definition test:                    \eq(\ifdef(mac1)(mac1 is defined)                       )()
Definition test:                    \eq(\ifdef(swap)(swap is defined)                       )(swap is defined)
Definition test:                    \eq(\ifdef(mac1)(mac1 is defined)(mac1 is not defined)  )(mac1 is not defined)
Definition test:                    \eq(\ifdef(swap)(swap is defined)(swap is not defined)  )(swap is defined)
Undefinition test:                  \eq(\ifndef(mac1)(mac1 is not defined)                  )(mac1 is not defined)
Undefinition test:                  \eq(\ifndef(swap)(swap is not defined)                  )()
Undefinition test:                  \eq(\ifndef(mac1)(mac1 is not defined)(mac1 is defined) )(mac1 is not defined)
Undefinition test:                  \eq(\ifndef(swap)(swap is not defined)(swap is defined) )(swap is defined)

\define(one)(1) \define(un)(1) \define(two)(2)
Equality test:                      \eq(\ifeq(\one)(\un)(one == un)                 )(one == un)
Equality test:                      \eq(\ifeq(\one)(\two)(one == two)               )()
Equality test:                      \eq(\ifeq(\one)(\un)(one == un)(one /= un)      )(one == un)
Equality test:                      \eq(\ifeq(\one)(\two)(one == two)(one /= two)   )(one /= two)
Inequality test:                    \eq(\ifne(\one)(\un)(one /= un)                 )()
Inequality test:                    \eq(\ifne(\one)(\two)(one /= two)               )(one /= two)
Inequality test:                    \eq(\ifne(\one)(\un)(one /= un)(one == un)      )(one == un)
Inequality test:                    \eq(\ifne(\one)(\two)(one /= two)(one == two)   )(one /= two)

Raw text:                           \ne(\raw(\swap(a)(b)))(ba) (should not evaluated: \raw(\swap(a)(b)))

File inclusion
==============

Inclusion of `pp-test.i`:

File name of the main file:         \eq(\main)(test/pp-test.md)
File name of the current file:      \eq(\file)(\main)
\include(pp-test.i)
Definitions:                        \eq(\answer)(42)

Files can also be included without being preprocessed:
Undefinition:                       \undef(answer)\isndef(answer)
\rawinclude(pp-test.i)
No definitions:                     \isndef(answer)

File modification date
======================

Current file date:                  \eq(\mdate)(\exec(date +"%A %d %B %Y" -r \file))
Specific file date:                 \eq(\mdate(test/pp-test.md test/pp-test.i))(\exec(date +"%A %d %B %Y" -r test/$(ls -t test | head -1)))

Environment variables
=====================

Environment variable:               \eq(\env(HOME))(\exec(echo $HOME))

Simple arithmetic
=================

undefined + 1 = 1:                  \add(x)\eq(\x)(1)
1 + 1 = 2:                          \add(x)\eq(\x)(2)
undefined + 3 = 3:                  \add(y)(3)\eq(\y)(3)
3 + 4 = 7:                          \add(y)(4)\eq(\y)(7)

Output language and output format
=================================

The current language is "en":       \eq(\lang)(en)
Section in english:                 \eq(\en(Hello World!))(Hello World!)
Section in french:                  \eq(\fr(Bonjour le monde !))()

The current format is HTML:         \eq(\format)(html)
Section for an HTML document:       \eq(\html(Hello World!))(Hello World!)
Section for a PDF document:         \eq(\pdf(Hello World!))()

External commands and scripts execution
=======================================

Command line (current shell):       \eq(\exec(echo hi))(hi)
`sh` script:                        \eq(\sh(echo hi))(hi)
`bash` script:                      \eq(\bash(echo hi))(hi)
`bat` script:                       \eq(\bat(echo hi))(hi)
`python` script:                    \eq(\python(print "hi"))(hi)
`haskell` script:                   \eq(\haskell(main = putStrLn "hi"))(hi)

Diagrams
========

Diagrams test do not check the generated image, just the link in the output document.

\dot([.build/]img/dot-test){Test of dot}{ digraph { dot -> { A B } -> C -> dot } }
\neato([.build/]img/neato-test){Test of neato}{ digraph { neato -> { A B } -> C -> neato } }
\twopi([.build/]img/twopi-test){Test of twopi}{ digraph { twopi -> { A B } -> C -> twopi } }
\circo([.build/]img/circo-test){Test of circo}{ digraph { circo -> { A B } -> C -> circo } }
\fdp([.build/]img/fdp-test){Test of fdp}{ digraph { fdp -> { A B } -> C -> fdp } }
\patchwork([.build/]img/patchwork-test){Test of sfdp}{ digraph { patchwork -> { A B } -> C } }
\osage([.build/]img/osage-test){Test of osage}{ digraph { osage -> { A B } -> C -> osage } }

\uml([.build/]img/uml-test){Test of uml}{ sender -> receiver }

\ditaa([.build/]img/ditaa-test){Test of ditaa}{ sender -> receiver }

Test results
============

Number of successful tests:     \nbok
Number of failures:             \nberr

\ifeq(\nberr)(0)(All tests passed!)(\nberr tests failed!)
