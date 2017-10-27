% PP - Generic preprocessor (with pandoc in mind)
% Christophe Delord - <http://cdsoft.fr/pp>
% !mdate{app/pp.hs !sh(ls src/******.hs) doc/pp.md}

!quiet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pp documentation contains a lot of examples.
The default macro execution character is redefined to avoid lots of `raw` calls in examples.

!macrochars(§)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[PP]: http://cdsoft.fr/pp "PP - Generic Preprocessor (for Pandoc)"
[DPP]: http://cdsoft.fr/dpp "DPP - Diagram Preprocessor (for Pandoc)"
[pp.tgz]: http://cdsoft.fr/pp/pp.tgz
[GraphViz]: http://graphviz.org/
[PlantUML]: http://plantuml.sourceforge.net/
[ditaa]: http://ditaa.sourceforge.net/
[GPP]: http://en.nothingisreal.com/wiki/GPP
[Pandoc]: http://pandoc.org/
[Bash]: https://www.gnu.org/software/bash/
[Cmd]: https://en.wikipedia.org/wiki/Cmd.exe
[PowerShell]: https://en.wikipedia.org/wiki/PowerShell
[Python]: https://www.python.org/
[Haskell]: https://www.haskell.org/
[Stack]: https://docs.haskellstack.org/en/stable/README/
[GitHub]: https://github.com/CDSoft/pp

PP - Generic preprocessor (with pandoc in mind)
===============================================

[PP] is a text preprocessor designed for Pandoc (and more generally Markdown and reStructuredText).

The [PP] package used to contain three preprocessors for [Pandoc].

I started using Markdown and [Pandoc] with [GPP].
Then I wrote [DPP] to embed diagrams in Markdown documents.
And finally [PP] which merges the functionalities of [GPP] and [DPP].

[GPP] and [DPP] are no longer included in [PP] as `pp` can now be used standalone.
`dpp` and `gpp` can be found in the legacy [DPP] repository.

`pp` now implements:

- macros
- literate programming
- [GraphViz], [PlantUML] and [ditaa] diagrams
- [Bash], [Cmd], [PowerShell], [Python] and [Haskell] scripts

Open source
===========

[PP] is an Open source software.
Anybody can contribute on [GitHub] to:

- suggest or add new features
- report or fix bugs
- improve the documentation
- add some nicer examples
- find new usages
- ...

Installation
============

**Compilation**:

1. Download and extract [pp.tgz].
2. Run `make`.

[PP] is written in [Haskell] and is built with [Stack]. On MacOS, running `make` requires the GNU version of `tar` which can be installed with `brew install gnu-tar`.

**Installation**:

- Run `make install` to copy `pp` in `~/.local/bin`.
- or copy `pp` (`pp.exe` on Windows) wherever you want.

`pp` requires [Graphviz] and Java ([PlantUML] and [ditaa] are embedded in `pp`).

**Precompiled binaries**:

The recommended way to get PP binaries is to compile them from the sources.
Anyway if you have no Haskell compiler, you can try some precompiled binaries.

- Latest Linux and Windows binaries:

    - Fedora §sh[cat /etc/redhat-release | tr -d -c "[0-9]"] (64 bit): <http://cdsoft.fr/pp/pp-linux-x86_64.txz>
    - Windows (64 bit): <http://cdsoft.fr/pp/pp-win.7z>

- Older version archive:

    - Fedora & Windows: <http://cdsoft.fr/pp/download.html>

- User contributed Mac OS binaries (outdated):

    - Mac OS (64 bit binaries): <https://github.com/dlardi/pp/releases/download/v1.0/pp-darwin-x86_64.txz>

Usage
=====

`pp` is a simple preprocessor written in Haskell.
It's mainly designed for Pandoc but may be used as a generic preprocessor.
It is not intended to be as powerful as GPP, for instance, but is a simple
implementation for my own needs, as well as an opportunity to play with
Haskell.

`pp` takes strings as input and incrementally builds an environment which is
a lookup table containing variables and various other information.
Built-in macros are Haskell functions that takes arguments (strings) and the current
environment and build a new environment in the IO monad.
User defined macros are simple definitions, arguments are numbered 1 to N.

`pp` emits the preprocessed document on the standard output. Inputs are listed
on the command line and concatenated, the standard input is used when no
input is specified.

Command line
------------

`pp` executes arguments in the same order as the command line.
It starts with an initial environment containing:

- the environment variables of the current process
- a `lang` variable containing the current langage
  (currently only French (`fr`), Italian (`it`), Spanish (`es`) and English (`en`) are supported)
- a `format` variable containing the current output format
  (`html`, `pdf`, `odt`, `epub` or `mobi`)
- a `dialect` variable containing the current dialect (`md` or `rst`)

The _dialect_ is used to format links and images in the output documents.
Currently only Markdown and reStructuredText are supported.

If no input file is specified, `pp` preprocesses the standard input.

The command line arguments are intentionally very basic.
The user can define and undefine variables and list input files.

**`-h`**
:   displays some help and exits.

**`-v`**
:   displays the current version and exits.

**`-DSYMBOL[=VALUE]`** or **`-D SYMBOL[=VALUE]`**
:   adds the symbol `SYMBOL` to the current environment and associates it to
    the optional value `VALUE`. If no value is provided, the symbol is simply
    defined with an empty value.

**`-USYMBOL`** or **`-U SYMBOL`**
:   removes the symbol `SYMBOL` from the current environment.

**`-languages`**
:   lists the languages.

§sh[echo §langs | sed 's/\(\w\{1,\}\)/**`-§raw(\1)`**/g' | tr ' ' '|']
:   changes the current language.

**`-formats`**
:   lists the formats.

§sh[echo §formats | sed 's/\(\w\{1,\}\)/**`-§raw(\1)`**/g' | tr ' ' '|']
:   changes the current output file format.

**`-dialects`**
:   lists the dialects.

§sh[echo §dialects | sed 's/\(\w\{1,\}\)/**`-§raw(\1)`**/g' | tr ' ' '|']
:   changes the current dialect (`-md` is the default dialect).

**`-img=PREFIX`** or **`-img PREFIX`**
:   changes the prefix of the images output path.

**`-import=FILE`** or **`-import FILE`**
:   preprocessed `FILE` but discards its output.
    It only keeps macro definitions and other side effects.

**`-M TARGET`** or **`-M=TARGET`**
:   tracks dependencies and outputs a make rule listing the dependencies.
    The target name is necessary since it can not be infered by `pp`.
    This option only lists files that are imported, included and
    used with `mdate` and `csv`macros.

**`-macrochars "chars"`** or **`-macrochars="chars"`**
:   defines the possible characters used to call macros.

**`-literatemacrochars "chars"`** or **`-literatemacrochars="chars"`**
:   defines the possible characters used to identify literate programming macros.

**`-macroargs "chars"`** or **`-macroargs="chars"`**
:   defines the possible character pairs used to separate macro args.

Other arguments are filenames.

Files are read and preprocessed using the current state of the environment.
The special filename "`-`" can be used to preprocess the standard input.

Macros
------

Built-in macros are hard coded in `pp` and can not be redefined.
User defined macros are simple text substitutions
that may have any number of parameters (named `!1` to `!n`).
User macros can be (re)defined on the command line or in the documents.

Macro names are:

- case sensitive (i.e.: `!my_macro` and `!My_Macro` are different macros)
- made of letters, digits and underscores (`a-zA-Z0-9_`)

To get the value of a variable you just have to write its name after a '`!`'.
Macros can be given arguments.
Each argument is enclosed in parenthesis, curly braces or square brackets.
For instance, the macro `foo` with two arguments can be called as `!foo(x)(y)`,
`!foo{x}{y}` or even `!foo[x][y]`.
Mixing brackets, braces and parenthesis within a single macro is not allowed:
all parameters must be enclosed within the same type of delimiters.
This helps ending a list of arguments in some edge cases:

    !macro(x)(y)

    [link]: foo bar

    Here, [link] is not parsed as a third parameter of !macro

Arguments are stripped. Removing leading and trailing spaces helps preserving
line structure in the document.

The last argument can be enclosed between lines of tildas or backquotes
(of the same length) instead of parenthesis, brackets or braces and.
This is useful for literate programming, diagrams or scripts (see [examples](#examples)).
Code block arguments are not stripped: spaces and blank lines are preserved.

Arguments can be on separate lines but must not be separated by blank lines.

You can choose the syntax that works better with your favorite editor and
syntax colorization.

For most of the macros, arguments are preprocessed before executing the macro.
Macros results are not preprocessed (unless used as a parameter of
an outer macro).
The `include` macro is an exception: its output is also preprocessed.
The `rawinclude` macro can include a file without preprocessing it.

§help

Literate programming example
============================

The main program just prints some messages:

    !lit(main.c)(C)
    ~~~~~~~~~~~~~~~~~~~~
    @includes
    void main()
    {
    @messages
    }
    ~~~~~~~~~~~~~~~~~~~~

First we need to be able to print messages:

    !lit(@includes)(C)
    ~~~~~~~~~~~~~~~~~~~~
    #include <stdio.h>
    ~~~~~~~~~~~~~~~~~~~~

The program must first say "Hello" :

    !lit(@messages)(C)
    ~~~~~~~~~~~~~~~~~~~~
        puts("Hello...\n");
    ~~~~~~~~~~~~~~~~~~~~

And also finally "Goodbye":

    !lit(@messages)
    ~~~~~~~~~~~~~~~~~~~~
        puts("Goodbye.");
    ~~~~~~~~~~~~~~~~~~~~

Diagram and script examples
===========================

## Diagrams

Diagrams are written in code blocks as argument of a diagram macro.
The first line contains the macro:

- the diagram generator (the macro name)
- the image name without the extension (first argument)
- the legend (second optional argument)

Block delimiters are made of three or more tilda or back quotes,
at the beginning of the line (no space and no tab).
The end delimiter must at least as long as the beginning delimiter.

    !dot(path/imagename)(optional legend)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        graph {
            "source code of the diagram"
        }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This extremely meaningful diagram is rendered as `path/imagename.png`
and looks like:

§dot(pp-syntax)(optional legend)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graph {
    "source code of the diagram"
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The image link in the output markdown document may have to be different than the
actual path in the file system. This happens when then `.md` or `.html` files are not
generated in the same path than the source document. Brackets can be used to
specify the part of the path that belongs to the generated image but not to the
link in the output document. For instance a diagram declared as:

    !dot([mybuildpath/]img/diag42)...

will be actually generated in:

    mybuildpath/img/diag42.png

and the link in the output document will be:

    img/diag42.png

For instance, if you use Pandoc to generate HTML documents with diagrams in a
different directory, there are two possibilities:

1. the document is a self contained HTML file (option `--self-contained`), i.e.
   the CSS and images are stored inside the document:
    - the CSS path shall be the actual path where the CSS file is stored
    - the image path in diagrams shall be the actual path where the images are
      stored (otherwise Pandoc won't find them)
    - e.g.: `outputpath/img/diag42`

2. the document is not self contained, i.e. the CSS and images are stored apart
   from the document:
    - the CSS path shall be relative to the output document
    - the image path in diagrams shall be relative to output document in HTML
      links and shall also describe the actual path where the images are stored.
    - e.g.: `[outputpath/]img/diag42`

Pandoc also accepts additional attributes on images (`link_attributes` extension).
These attributes can be added between curly brackets to the first argument.
e.g.:

    !dot(image.png { width=50 % })(caption)(...)

will generate the following link in the markdown output:

    ![caption](image.png){ width=50 % }

The diagram generator can be:

- dot
- neato
- twopi
- circo
- fdp
- sfdp
- patchwork
- osage
- uml
- ditaa

`pp` will not create any directory,
the path where the image is written must already exist.

§dot(pp-generators)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
digraph {

    subgraph cluster_cmd {
        label = "diagram generators"
        dot neato twopi circo fdp sfdp patchwork osage uml ditaa
    }

    PP [label="pp" shape=diamond]
    dot neato twopi circo fdp sfdp patchwork osage uml ditaa
    GraphViz [shape=box]
    PlantUML [shape=box]
    DITAA [shape=box label=ditaa]

    PP -> {dot neato twopi circo fdp sfdp patchwork osage uml ditaa}
    dot -> GraphViz
    neato -> GraphViz
    twopi -> GraphViz
    circo -> GraphViz
    fdp -> GraphViz
    sfdp -> GraphViz
    patchwork -> GraphViz
    osage -> GraphViz
    uml -> PlantUML
    ditaa -> DITAA
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Scripts

Scripts are also written in code blocks as arguments of a macro.

    !bash
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    echo Hello World!
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With no surprise, this script generates:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
§bash
~~~~~
echo Hello World!
~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The script language macro can be:

- `bash` (or `sh`)
- `python`
- `haskell` (or `stack`)
- `cmd` (DOS/Windows batch language)
- `powershell` (Windows only)

`pp` will create a temporary script before calling the associated interpretor.

§dot(pp-scripts)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
digraph {

    subgraph cluster_cmd {
        label = "script languages"
        bash sh python haskell stack cmd powershell
    }

    PP [shape=diamond label="pp"]
    bash sh cmd python haskell stack
    Bash [shape=box label="bash\nor bash.exe"]
    Sh [shape=box label="sh\nor sh.exe"]
    Python [shape=box label="python\nor python.exe"]
    Haskell [shape=box label="runhaskell\nor runhaskell.exe"]
    Stack [shape=box label="stack\nor stack.exe"]
    Cmd [shape=box label="wine cmd /c\nor cmd /c"]
    PowerShell [shape=box label="(Windows only)\npowershell.exe"]

    PP -> {bash sh python haskell stack cmd powershell}
    bash -> Bash
    sh -> Sh
    python -> Python
    haskell -> Haskell
    stack -> Stack
    cmd -> Cmd
    powershell -> PowerShell
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Examples

The [source code](pp.md) of this document contains some diagrams.

Here are some simple examples.
For further details about diagrams' syntax, please read the documentation of
[GraphViz], [PlantUML] and [ditaa].

### Graphviz

[GraphViz] is executed when one of these keywords is used:
`dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, `patchwork`, `osage`

    !twopi(doc/img/pp-graphviz-example)(This is just a GraphViz diagram example)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    digraph {
        O -> A
        O -> B
        O -> C
        O -> D
        D -> O
        A -> B
        B -> C
        C -> A
    }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `twopi` is the kind of graph
  (possible graph types: `dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, `patchwork`).
- `doc/img/pp-graphviz-example` is the name of the image.
  `pp` will generate `doc/img/pp-graphviz-example.dot`
  and `doc/img/pp-graphviz-example.png`.
- the rest of the first line is the legend of the graph.
- other lines are written to `doc/img/pp-graphviz-example.dot` before running [Graphviz].
- if the command line argument `-img=prefix`, `prefix` is added at the beginning of the image path.

Once generated the graph looks like:

§twopi(pp-graphviz-example)(This is just a GraphViz diagram example)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
digraph {
    O -> A
    O -> B
    O -> C
    O -> D
    D -> O
    A -> B
    B -> C
    C -> A
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[GraphViz] must be installed.

### PlantUML

[PlantUML] is executed when the keyword `uml` is used.
The lines `@startuml` and `@enduml` required by [PlantUML] are added by `pp`.

    !uml(pp-plantuml-example)(This is just a PlantUML diagram example)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Alice -> Bob: Authentication Request
    Bob --> Alice: Authentication Response
    Alice -> Bob: Another authentication Request
    Alice <-- Bob: another authentication Response
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once generated the graph looks like:

§uml(pp-plantuml-example)(This is just a PlantUML diagram example)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response
Alice -> Bob: Another authentication Request
Alice <-- Bob: another authentication Response
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[PlantUML](http://plantuml.sourceforge.net) is written in Java and is embedded in `pp`.
Java must be installed.

### Ditaa

[ditaa] is executed when the keyword `ditaa` is used.

    !ditaa(pp-ditaa-example)(This is just a Ditaa diagram example)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        +--------+   +-------+    +-------+
        |        | --+ ditaa +--> |       |
        |  Text  |   +-------+    |diagram|
        |Document|   |!magic!|    |       |
        |     {d}|   |       |    |       |
        +---+----+   +-------+    +-------+
            :                         ^
            |       Lots of work      |
            +-------------------------+
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once generated the graph looks like:

§ditaa(pp-ditaa-example)(This is just a Ditaa diagram example)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    +--------+   +-------+    +-------+
    |        | --+ ditaa +--> |       |
    |  Text  |   +-------+    |diagram|
    |Document|   |!magic!|    |       |
    |     {d}|   |       |    |       |
    +---+----+   +-------+    +-------+
        :                         ^
        |       Lots of work      |
        +-------------------------+
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[ditaa](http://plantuml.sourceforge.net) is written in Java and is embedded in `pp`.
Java must be installed.

### Bash

[Bash] is executed when the keyword `bash` is used.

    !bash
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    echo "Hi, I'm $SHELL $BASH_VERSION"
    RANDOM=42 # seed
    echo "Here are a few random numbers: $RANDOM, $RANDOM, $RANDOM"
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This script outputs:

~~~~~~~~~~
§bash
~~~~~
echo "Hi, I'm $SHELL $BASH_VERSION"
RANDOM=42 # seed
echo "Here are a few random numbers: $RANDOM, $RANDOM, $RANDOM"
~~~~~
~~~~~~~~~~

**Note**: the keyword `sh` executes `sh` which is generally a link to `bash`.

### Cmd

Windows' [command-line interpreter][Cmd] is executed when the keyword `cmd` is used.

    !cmd
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    echo Hi, I'm %COMSPEC%
    ver
    if "%WINELOADER%%WINELOADERNOEXEC%%WINEDEBUG%" == "" (
        echo This script is run from wine under Linux
    ) else (
        echo This script is run from a real Windows
    )
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This script outputs:

~~~~~~~~~~
§cmd
~~~~~
echo Hi, I'm %COMSPEC%
ver
if "%WINELOADER%%WINELOADERNOEXEC%%WINEDEBUG%" == "" (
    echo This script is run from a real Windows
) else (
    echo This script is run from wine under Linux
)
~~~~~
~~~~~~~~~~

### Python

[Python] is executed when the keyword `python` is used.

    !python
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    import sys
    import random

    if __name__ == "__main__":
        print("Hi, I'm Python %s"%sys.version)
        random.seed(42)
        randoms = [random.randint(0, 1000) for i in range(3)]
        print("Here are a few random numbers: %s"%(", ".join(map(str, randoms))))
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This script outputs:

~~~~~~~~~~
§python
~~~~~
import sys
import random

if __name__ == "__main__":
    print("Hi, I'm Python %s"%sys.version)
    random.seed(42)
    randoms = [random.randint(0, 1000) for i in range(3)]
    print("Here are a few random numbers: %s"%(", ".join(map(str, randoms))))
~~~~~
~~~~~~~~~~

### Haskell

[Haskell] is executed when the keyword `haskell` is used.

    !haskell
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    import System.Info
    import Data.Version
    import Data.List

    primes = filterPrime [2..]
        where filterPrime (p:xs) =
                p : filterPrime [x | x <- xs, x `mod` p /= 0]

    version = showVersion compilerVersion

    main = do
        putStrLn $ "Hi, I'm Haskell " ++ version
        putStrLn $ "The first 10 prime numbers are: " ++
                    intercalate " " (map show (take 10 primes))
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This script outputs:

~~~~~~~~~~
§haskell
~~~~~
import System.Info
import Data.Version
import Data.List

primes = filterPrime [2..]
    where filterPrime (p:xs) =
            p : filterPrime [x | x <- xs, x `mod` p /= 0]

version = showVersion compilerVersion
main = do
    putStrLn $ "Hi, I'm Haskell " ++ version
    putStrLn $ "The first 10 prime numbers are: " ++
                intercalate " " (map show (take 10 primes))
~~~~~
~~~~~~~~~~

### Stack

[Haskell] is also executed when the keyword `stack` is used.
In this case stack meta data must be added at the beginning of the script.

    !stack
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    {- stack script --resolver lts-9.1 --package base -}

    import System.Info
    import Data.Version
    import Data.List

    primes = filterPrime [2..]
        where filterPrime (p:xs) =
                p : filterPrime [x | x <- xs, x `mod` p /= 0]

    version = showVersion compilerVersion

    main = do
        putStrLn $ "Hi, I'm Haskell " ++ version
        putStrLn $ "The first 10 prime numbers are: " ++
                    intercalate " " (map show (take 10 primes))
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This script outputs:

~~~~~~~~~~
§stack
~~~~~
{- stack script --resolver lts-9.1 --package base -}

import System.Info
import Data.Version
import Data.List

primes = filterPrime [2..]
    where filterPrime (p:xs) =
            p : filterPrime [x | x <- xs, x `mod` p /= 0]

version = showVersion compilerVersion
main = do
    putStrLn $ "Hi, I'm Haskell " ++ version
    putStrLn $ "The first 10 prime numbers are: " ++
                intercalate " " (map show (take 10 primes))
~~~~~
~~~~~~~~~~

CSV tables
==========

CSV files can be included in documents and rendered as Markdown or reStructuredText tables.
The field separator is inferred from the content of the file.
It can be a comma, a semicolon, tabulation or a pipe.

## Files with a header line

This file:

§lit(/tmp/table1.csv)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Year,Make,Model,Description,Price
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
§flushlit

is rendered by `!csv(file.csv)` as:

§csv(/tmp/table1.csv)

## Files without any header line

This file:

§lit(/tmp/table2.csv)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
§flushlit

is rendered by `!csv(file.csv)(Year|Make|Model|Description|Price)` as:

§csv(/tmp/table2.csv)(Year|Make|Model|Description|Price)

OS support
==========

PP is meant to be portable and multi platform.
To be OS agnostic, the use free script languages is strongly recommended.
For instance, bash scripts are preferred to proprietary closed languages
because they can run on any platform. It is standard on Linux and pretty
well supported on Windows (Cygwin, MSYS/Mingw, Git Bash, BusyBox, ...).
Python is also a good choice.

Anyway, if some documents require portability and specific tools, PP provides
some macros to detect the OS (`!os`, `!arch`). E.g.:

    !quiet
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !ifeq(!os)(linux)
    `````````````````````
    !def(linux)(!1)
    !def(win)()
    `````````````````````
    !ifeq(!os)(windows)
    `````````````````````
    !def(linux)()
    !def(win)(!1)
    `````````````````````
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    !win(Sorry, you're running Windows)
    !linux(Hello, happy GNU/Linux user)

The `!exec` macro is also OS aware. It runs the *default* shell according
to the OS (`sh` on Linux and MacOS, `cmd` on Windows).

Third-party documentations, tutorials and macros
================================================

- [PP tutorial](https://github.com/tajmone/markdown-guide/tree/master/pp)
  by [tajmone](https://github.com/tajmone):
  a good starting point for beginners.
- [Pandoc-Goodies PP-Macros Library](https://github.com/tajmone/pandoc-goodies/tree/master/pp)
  by [tajmone](https://github.com/tajmone):
  an ongoing collaborative effort to build a library of PP macros.

Licenses
========

PP
--

Copyright (C) 2015, 2016, 2017 Christophe Delord <br>
<http://www.cdsoft.fr/pp>

PP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

PP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with PP.  If not, see <http://www.gnu.org/licenses/>.

PlantUML
--------

PlantUML.jar is integrated in [PP].
[PlantUML] is distributed under the [GPL license](http://www.gnu.org/copyleft/gpl.html).
See <http://plantuml.sourceforge.net/faq.html>.

ditaa
-----

ditaa.jar is not integrated anymore in [PP].
The [ditaa] version used is the one already integrated in [PlantUML].
[ditaa] is distributed under the [GNU General Public License version 2.0 (GPLv2)](http://sourceforge.net/directory/license:gpl/).
See <http://sourceforge.net/projects/ditaa/>.

Feedback
========

Your feedback and contributions are welcome.
You can contact me at <http://cdsoft.fr>
