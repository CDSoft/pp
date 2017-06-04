% PP - Generic preprocessor (with pandoc in mind)
% Christophe Delord - <http://cdsoft.fr/pp>
% \mdate{app/pp.hs doc/pp.md}

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

[PP] is written in [Haskell] and is built with [Stack].

**Installation**:

- Run `make install` to copy `pp` in `~/.local/bin`.
- or copy `pp` (`pp.exe` on Windows) wherever you want.

`pp` requires [Graphviz] and Java ([PlantUML] and [ditaa] are embedded in `pp`).

**Precompiled binaries**:

The recommended way to get PP binaries is to compile them from the sources.
Anyway if you have no Haskell compiler, you can try some precompiled binaries.

- Latest Linux and Windows binaries:

    - Fedora \sh[cat /etc/redhat-release | tr -d -c "[0-9]"] (64 bit): <http://cdsoft.fr/pp/pp-linux-x86_64.txz>
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

\sh[echo \langs | sed 's/\(\w\{1,\}\)/**`-\1`**/g' | tr ' ' '|']
:   changes the current language.

**`-formats`**
:   lists the formats.

\sh[echo \formats | sed 's/\(\w\{1,\}\)/**`-\1`**/g' | tr ' ' '|']
:   changes the current output file format.

**`-dialects`**
:   lists the dialects.

\sh[echo \dialects | sed 's/\(\w\{1,\}\)/**`-\1`**/g' | tr ' ' '|']
:   changes the current dialect (`-md` is the default dialect).

**`-img=PREFIX`** or **`-img PREFIX`**
:   changes the prefix of the images output path.

**`-import=FILE`** or **`-import FILE`**
:   preprocessed `FILE` but discards its output.
    It only keeps macro definitions and other side effects.

Other arguments are filenames.

Files are read and preprocessed using the current state of the environment.
The special filename "`-`" can be used to preprocess the standard input.

Macros
------

\raw{

Built-in macros are hard coded in `pp` and can not be redefined.
User defined macros are simple text substitutions
that may have any number of parameters (named `!1` to `!n`).
User macros can be (re)defined on the command line or in the documents.

Macro names are:

- case sensitive (i.e.: `!my_macro` and `!My_Macro` are different macros)
- made of letters, digits and underscores (`a-zA-Z0-9_`)

To get the value of a variable you just have to write its name after a '`!`' or '`\`'.
Macros can be given arguments.
Each argument is enclosed in parenthesis, curly braces or square brackets.
For instance, the macro `foo` with two arguments can be called as `!foo(x)(y)`,
`\foo{x}{y}` or even `!foo[x][y]`.
Mixing brackets, braces and parenthesis within a single macro is not allowed:
all parameters must be enclosed within the same type of delimiters.
This helps ending a list of arguments in some edge cases:

    \macro(x)(y)

    [link]: foo bar

    Here, [link] is not parsed as a third parameter of \macro

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
}

\raw{**`!def[ine](SYMBOL)[(VALUE)]`**}
:   Add the symbol `SYMBOL` to the current environment
    and associate it with the optional value `VALUE`.
    Arguments are denoted by `!1` ... `!n` in `VALUE`.

\raw{**`!undef[ine](SYMBOL)`**}
:   Remove the symbol `SYMBOL` from the current environment.

\raw{**`!ifdef(SYMBOL)(TEXT_IF_DEFINED)[(TEXT_IF_NOT_DEFINED)]`**}
:   if `SYMBOL` is defined in the current environnement `pp` preprocesses
    `TEXT_IF_DEFINED`. Otherwise it preprocesses `TEXT_IF_NOT_DEFINED`.

\raw{**`!ifndef(SYMBOL)(TEXT_IF_NOT_DEFINED)[(TEXT_IF_DEFINED)]`**}
:   if `SYMBOL` is not defined in the current environnement `pp` preprocesses
    `TEXT_IF_NOT_DEFINED`. Otherwise it preprocesses `TEXT_IF_DEFINED`.

\raw{**`!ifeq(X)(Y)(TEXT_IF_EQUAL)[(TEXT_IF_DIFFERENT)]`**}
:   if `X` and `Y` are equal `pp` preprocesses `TEXT_IF_EQUAL`.
    Otherwise it preprocesses `TEXT_IF_DIFFERENT`.
    Two pieces of text are equal if all characters are the same,
    spaces are ignored.

\raw{**`!ifne(X)(Y)(TEXT_IF_DIFFERENT)[(TEXT_IF_EQUAL)]`**}
:   if `X` and `Y` are different `pp` preprocesses `TEXT_IF_DIFFERENT`.
    Otherwise it preprocesses `TEXT_IF_EQUAL`.

\raw{**`!rawdef(X)`**}
:   get the raw (unevaluated) definition of `X`

\raw{**`!inc[lude](FILENAME)`**}
:   `pp` preprocesses the content of the file named `FILENAME` and includes it
     in the current document, using the current environment.
     If the file path is relative it is searched
     first in the directory of the current file
     then in the directory of the main file.

\raw{**`!import(FILENAME)`**}
:   works as `\raw{!include(FILENAME)}` but no text is emitted.
    This is useful to import macro definitions.

\raw{**`!raw(TEXT)`**}
:   `pp` emits `TEXT` without any preprocessing.

\raw{**`!rawinc[lude](FILE)`**}
:   `pp` emits the content of `FILE` without any preprocessing.

\raw{**`!pp(TEXT)`**}
:   `pp` forces the evaluation of `TEXT`.
    This macro is useful to preprocess the output of script macros
    for instance (`sh`, `python`, ...).

\raw{**`!comment(TEXT)`** or **`!comment(TITLE)(TEXT)`**}
:   considers `TEXT` as comment. Nothing is preprocessed or emitted.
    `TITLE` is also ignored.

    Example:

        \raw{!comment(This is the title of the comment)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        And this is a useful description of some
        macro definitions.
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}

\raw{**`!quiet(TEXT)`**}
:   quietly preprocess `TEXT` and emits nothing.
    Only the side effects (e.g. macro definitions) are kept in the environment.

\raw{**`!exec(COMMAND)`**}
:   executes a shell command with the default shell (`sh` or `cmd` according to the OS).

\raw{**`!rawexec(COMMAND)`** (*deprecated*)}
:   as `\raw{!exec(COMMAND)}`.
    This macro is deprecated. Consider using `exec` instead.

\raw{**`!mdate(FILES)`**}
:   returns the modification date of the most recent file.

\raw{**`!env(VARNAME)`**}
:   `pp` preprocesses and emits the value of the process environment variable `VARNAME`.

\raw{**`!os`**}
:   returns the OS name (e.g. `linux` on Linux, `darwin` on MacOS, `windows` on Windows)

\raw{**`!arch`**}
:   returns the machine architecture (e.g. `x86_64`, `i386`, ...)

\raw{**`!add(VARNAME)[(INCREMENT)]`**}
:   computes `VARNAME+INCREMENT` and stores the result to `VARNAME`.
    The default value of the increment is 1.

\raw{**`!lang`**}
:   emits the current language (\sh[echo \langs | sed -e 's/\(\w\{1,\}\)/*\1*/g' -e 's/ /, /g'])

\raw{**`!format`**}
:   emits the current format (\sh[echo \formats | sed -e 's/\(\w\{1,\}\)/*\1*/g' -e 's/ /, /g'])

\raw{**`!dialect`**}
:   emits the current dialect (\sh[echo \dialects | sed -e 's/\(\w\{1,\}\)/*\1*/g' -e 's/ /, /g'])

\sh[echo \langs | sed -e 's/\(\w\{1,\}\)/**`!\1(...)`**/g' -e 's/ /, /g']
:   emits some text only if the current language is \sh[echo \langs | sed -e 's/\(\w\{1,\}\)/*\1*/g' -e 's/ /, /g']

\sh[echo \formats | sed -e 's/\(\w\{1,\}\)/**`!\1(...)`**/g' -e 's/ /, /g']
:   emits some text only if the current format is \sh[echo \formats | sed -e 's/\(\w\{1,\}\)/*\1*/g' -e 's/ /, /g']

\sh[echo \dialects | sed -e 's/\(\w\{1,\}\)/**`!\1(...)`**/g' -e 's/ /, /g']
:   emits some text only if the current dialect is \sh[echo \dialects | sed -e 's/\(\w\{1,\}\)/*\1*/g' -e 's/ /, /g']

\raw{**`!dot(IMAGE)(LEGEND)(GRAPH DESCRIPTION)`**}
:   renders a diagram with [GraphViz], [PlantUML] and [Ditaa].
    See examples later.
    The name of the macro is the kind of diagram.
    The possible diagrams are:
    `dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, `patchwork`, `osage`,
    `uml` and `ditaa`.

\raw{**`!sh(SCRIPT)`**, **`!bash(SCRIPT)`**, **`!python[2|3](SCRIPT)`**, **`!haskell(SCRIPT)`** **`!cmd(SCRIPT)`**, **`!powershell(SCRIPT)`**,}
:   executes a script and emits its output.
    The possible programming languages are `sh`, `bash`, `python`, `haskell`, `cmd` and `powershell`.
    Python can be executed with `python`, `python2` or `python3` to use the default interpretor, the version 2 or 3.

\raw{**`!bat(SCRIPT)`** (*deprecated*)}
:   same as `\raw{!cmd}`.

\raw{**`!lit[erate](FILENAME)(LANG)(CONTENT)`**}
:   appends `CONTENT` to the file `FILENAME`.
    If `FILENAME` starts with `@` it's a macro, not a file.
    The output is highlighted using the programming language `LANGUAGE`.
    The list of possible languages is given by `pandoc --list-highlight-languages`.
    Files are actually written when all the documents have been successfully preprocessed.
    Macros are expanded when the files are written.
    This macro provides basic literate programming features.

\raw{**`!lit[erate](FILENAME)(CONTENT)`**}
:   appends `CONTENT` to the file `FILENAME`.
    The output is highlighted using the previously given language for this file.

    Example:

        The main program just prints some messages:

        \raw{!lit(main.c)(C)
        ~~~~~~~~~~~~~~~~~~~~
        @includes
        void main()
        {
        @messages
        }
        ~~~~~~~~~~~~~~~~~~~~}

        First we need to be able to print messages:

        \raw{!lit(@includes)(C)
        ~~~~~~~~~~~~~~~~~~~~
        #include <stdio.h>
        ~~~~~~~~~~~~~~~~~~~~}

        The program must first say "Hello" :

        \raw{!lit(@messages)(C)
        ~~~~~~~~~~~~~~~~~~~~
            puts("Hello...\n");
        ~~~~~~~~~~~~~~~~~~~~}

        And also finally "Goodbye":

        \raw{!lit(@messages)
        ~~~~~~~~~~~~~~~~~~~~
            puts("Goodbye.");
        ~~~~~~~~~~~~~~~~~~~~}

\raw{**`!lit[erate]`**}
:   emits the current content of `FILENAME`.

\raw{**`!flushlit[erate]`**}
:   writes files built with `\raw{!lit}` before reaching the end of the document.
    This macro is automatically executed before any script execution
    or file inclusion with `\raw{!src}`.

\raw{**`!src(FILENAME)[(LANG)]`**, **`!source(FILENAME)[(LANG)]`**}
:   formats an existing source file in a colorized code block.

\raw{**`!codeblock(LENGTH)[(CHAR)]`**}
:   sets the default line separator for code blocks.
    The default value is a 70 tilda row (`\raw{!codeclock(70)(~)}`).

\raw{**`!indent[(N)](BLOCK)`**}
:   indents each line of a block with `n` spaces.
    The default value of `n` is 4 spaces.

\raw{**`!csv(FILENAME)[(HEADER)]`**}
:   converts a CSV file to a Markdown or reStructuredText table.
    `HEADER` defines the header of the table, fields are separated by pipes (`|`).
    If `HEADER` is not defined, the first line of the file is used as the header of the table.

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

    \raw{\dot(path/imagename)(optional legend)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        graph {
            "source code of the diagram"
        }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

This extremely meaningful diagram is rendered as `path/imagename.png`
and looks like:

\dot(pp-syntax)(optional legend)
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

    \raw(\dot([mybuildpath/]img/diag42)...)

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

    \raw(\dot(image.png { width=50 % })(caption)(...))

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

\dot(pp-generators)
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

    \raw{\bash
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    echo Hello World!
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

With no surprise, this script generates:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\bash
~~~~~
echo Hello World!
~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The script language macro can be:

- `bash` (or `sh`)
- `python`
- `haskell`
- `cmd` (DOS/Windows batch language)
- `powershell` (Windows only)

`pp` will create a temporary script before calling the associated interpretor.

\dot(pp-scripts)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
digraph {

    subgraph cluster_cmd {
        label = "script languages"
        bash sh python haskell cmd powershell
    }

    PP [shape=diamond label="pp"]
    bash sh cmd python haskell
    Bash [shape=box label="bash\nor bash.exe"]
    Sh [shape=box label="sh\nor sh.exe"]
    Python [shape=box label="python\nor python.exe"]
    Haskell [shape=box label="runhaskell\nor runhaskell.exe"]
    Cmd [shape=box label="wine cmd /c\nor cmd /c"]
    PowerShell [shape=box label="(Windows only)\npowershell.exe"]

    PP -> {bash sh python haskell cmd powershell}
    bash -> Bash
    sh -> Sh
    python -> Python
    haskell -> Haskell
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

    \raw{\twopi(doc/img/pp-graphviz-example)(This is just a GraphViz diagram example)
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
    }

- `twopi` is the kind of graph
  (possible graph types: `dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, `patchwork`).
- `doc/img/pp-graphviz-example` is the name of the image.
  `pp` will generate `doc/img/pp-graphviz-example.dot`
  and `doc/img/pp-graphviz-example.png`.
- the rest of the first line is the legend of the graph.
- other lines are written to `doc/img/pp-graphviz-example.dot` before running [Graphviz].
- if the command line argument `-img=prefix`, `prefix` is added at the beginning of the image path.

Once generated the graph looks like:

\twopi(pp-graphviz-example)(This is just a GraphViz diagram example)
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

    \raw{\uml(pp-plantuml-example)(This is just a PlantUML diagram example)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Alice -> Bob: Authentication Request
    Bob --> Alice: Authentication Response
    Alice -> Bob: Another authentication Request
    Alice <-- Bob: another authentication Response
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

Once generated the graph looks like:

\uml(pp-plantuml-example)(This is just a PlantUML diagram example)
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

    \raw{\ditaa(pp-ditaa-example)(This is just a Ditaa diagram example)
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
    }

Once generated the graph looks like:

\ditaa(pp-ditaa-example)(This is just a Ditaa diagram example)
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

    \raw{\bash
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    echo "Hi, I'm $SHELL $BASH_VERSION"
    RANDOM=42 # seed
    echo "Here are a few random numbers: $RANDOM, $RANDOM, $RANDOM"
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

This script outputs:

~~~~~~~~~~
\bash
~~~~~
echo "Hi, I'm $SHELL $BASH_VERSION"
RANDOM=42 # seed
echo "Here are a few random numbers: $RANDOM, $RANDOM, $RANDOM"
~~~~~
~~~~~~~~~~

**Note**: the keyword `sh` executes `sh` which is generally a link to `bash`.

### Cmd

Windows' [command-line interpreter][Cmd] is executed when the keyword `cmd` is used.

    \raw{\cmd
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    echo Hi, I'm %COMSPEC%
    ver
    if "%WINELOADER%%WINELOADERNOEXEC%%WINEDEBUG%" == "" (
        echo This script is run from wine under Linux
    ) else (
        echo This script is run from a real Windows
    )
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

This script outputs:

~~~~~~~~~~
\cmd
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

    \raw{\python
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    import sys
    import random

    if __name__ == "__main__":
        print("Hi, I'm Python %s"%sys.version)
        random.seed(42)
        randoms = [random.randint(0, 1000) for i in range(3)]
        print("Here are a few random numbers: %s"%(", ".join(map(str, randoms))))
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

This script outputs:

~~~~~~~~~~
\python
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

    \raw{\haskell
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
    }

This script outputs:

~~~~~~~~~~
\haskell
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

CSV tables
==========

CSV files can be included in documents and rendered as Markdown or reStructuredText tables.
The field separator is inferred from the content of the file.
It can be a comma, a semicolon, tabulation or a pipe.

## Files with a header line

This file:

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

## Files without any header line

This file:

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

OS support
==========

PP is meant to be portable and multi platform.
To be OS agnostic, the use free script languages is strongly recommended.
For instance, bash scripts are preferred to proprietary closed languages
because they can run on any platform. It is standard on Linux and pretty
well supported on Windows (Cygwin, MSYS/Mingw, Git Bash, BusyBox, ...).
Python is also a good choice.

Anyway, if some documents require portability and specific tools, PP provides
some macros to detect the OS (\raw(`\os`, `\arch`)). E.g.:

    \raw[\quiet
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    \ifeq(\os)(linux)
    `````````````````````
    \def(linux)(\1)
    \def(win)()
    `````````````````````
    \ifeq(\os)(windows)
    `````````````````````
    \def(linux)()
    \def(win)(\1)
    `````````````````````
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    \win(Sorry, you're running Windows)
    \linux(Hello, happy GNU/Linux user)
    ]

The \raw(`\exec`) macro is also OS aware. It runs the *default* shell according
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
