% PP - Generic preprocessors (with pandoc in mind)
% Christophe Delord - <http://cdsoft.fr/dpp>
% \mdate{src/pp.hs src/pp.md src/dpp.c}

[PP]: http://cdsoft.fr/pp "PP - Generic Preprocessor (for Pandoc)"
[pp.tgz]: http://cdsoft.fr/pp/pp.tgz
[GraphViz]: http://graphviz.org/
[PlantUML]: http://plantuml.sourceforge.net/
[ditaa]: http://ditaa.sourceforge.net/
[GPP]: http://en.nothingisreal.com/wiki/GPP
[Pandoc]: http://pandoc.org/
[Bash]: https://www.gnu.org/software/bash/
[Bat]: https://en.wikipedia.org/wiki/Cmd.exe
[Python]: https://www.python.org/
[Haskell]: https://www.haskell.org/
[GitHub]: https://github.com/CDSoft/pp

Introduction
============

[PP] contains three preprocessors for [Pandoc].

I started using Markdown and [Pandoc] with [GPP].
[GPP] is still included in PP but I now use two more preprocessors that I have
written for my own needs:

- `pp` is a generic text preprocessor inspired by [GPP] and written in Haskel
- `dpp` is a diagramm preprocessor

Both are intended to be used with Pandoc.

Open source
===========

[PP] is an Open source software.
Any body can contribute on [GitHub] to:

- suggest or add new functionalities
- report or fix bugs
- improve the documentation
- add some nicer examples
- find new usages
- ...

Installation
============

1. Download and extract [pp.tgz]
2. Run `make`
3. Copy `pp`, `dpp` and `gpp` (`.exe` files on Windows) where you want.

`dpp` requires [Graphviz] and Java ([PlantUML] and [ditaa] are embedded in `dpp`).

If your are on Windows but don't have a C and Haskell compiler,
you can get already compiled executables here: <http://cdsoft.fr/pp/pp-win.zip>.

You can also download 64 bit linux binaries (built on `\exec(uname -srvmo)`),
they may or may not work on your specific platform: <http://cdsoft.fr/pp/pp-linux-\exec(uname -m).tgz>.

PP
==

`pp` is a simple preprocessor written in Haskell.
It's mainly designed for Pandoc but may be used as a generic preprocessor.
It is not intended to be as powerful as GPP for instance but is a simple
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

`pp` executes arguments in the same order than the command line.
It starts with an initial environment containing:

- the environment variables of the current process
- a `lang` variable containing the current langage
  (currently only French (`fr`) and English (`en`) are supported)
- a `format` variable containing the current output format
  (`html` or `pdf`)

If no input file is specified, `pp` also preprocesses the standard input.

The command line arguments are intensionally very basic.
The user can define and undefine variables and list input files.

**-DSYMBOL[=VALUE]**
:   adds the symbol `SYMBOL` to the current environment and associates it to
    the optional value `VALUE`. If value is not given the symbol is simply
    defined with an empty value

**-USYMBOL**
:   removes the symbol `SYMBOL` from the current environment.

Other arguments are filenames.

File are read and preprocessed using the current state of the environment.
The special file name `"-"` can be used to preprocess the standard input.

Macros
------

\raw{

Built-in macros are hard coded in `pp`.
User defined macros are simple text substitutions
that may have any number of parameters (named `!1` to `!n`).
User macros can be redefined on the command line or in the documents.

To get the value of a variable you just have to write its name after a `'!'` or `'\'`.
Macros can be given arguments. Each argument is enclosed in parenthesis, curly or square brackets.
For instance, the macro `foo` with two arguments can be called as `!foo(x)(y)`,
`\foo{x}{y}` or even `!foo[x]{y}`.

You can choose the syntax that works better with your favorite editor and
syntax colorization.

**`!def[ine](SYMBOL)[(VALUE)]`**
:   Add the symbol `SYMBOL` to the current environment and associate it with the optional value `VALUE`.
    Arguments are denoted by `!1` ... `!n` in `VALUE`.

**`!undef[ine](SYMBOL)`**
:   Remove the symbol `SYMBOL` from the current environment.

**`!ifdef(SYMBOL)(TEXT_IF_DEFINED)[(TEXT_IF_NOT_DEFINED)]`**
:   if `SYMBOL` is defined in the current environnement `pp` preprocesses
    `TEXT_IF_DEFINED`. Otherwise it preprocesses `TEXT_IF_NOT_DEFINED`.

**`!ifndef(SYMBOL)(TEXT_IF_NOT_DEFINED)[(TEXT_IF_DEFINED)]`**
:   if `SYMBOL` is not defined in the current environnement `pp` preprocesses
    `TEXT_IF_NOT_DEFINED`. Otherwise it preprocesses `TEXT_IF_DEFINED`.

**`!ifeq(X)(Y)(TEXT_IF_EQUAL)[(TEXT_IF_DIFFERENT)]`**
:   if `X` and 'Y' are equal `pp` preprocesses `TEXT_IF_EQUAL`.
    Otherwise it preprocesses `TEXT_IF_DIFFERENT`.
    Two pieces of text are equal if all characters are the same,
    spaces are ignored.

**`!ifne(X)(Y)(TEXT_IF_DIFFERENT)[(TEXT_IF_EQUAL)]`**
:   if `X` and 'Y' are different `pp` preprocesses `TEXT_IF_DIFFERENT`.
    Otherwise it preprocesses `TEXT_IF_EQUAL`.

**`!rawdef(X)`**
:   get the raw (unevaluated) definition of `X`

**`!inc[lude](FILENAME)`**
:   `pp` preprocesses the content of the file named `FILENAME` an include it
     in the current document, using the current environment.
     If the file path is relative it is searched first in the directory of the current file
     then in the directory of the main file.

**`!raw(TEXT)`**
:   `pp` emits `TEXT` without any preprocessing.

**`!rawinc[lude](FILE)`**
:   `pp` emits the content of `FILE` without any preprocessing.

**`!mdate(FILES)`**
:   returns the modification date of the most recent file

**`!env(VARNAME)`**
:   `pp` preprocesses and emits the value of the process environment variable `VARNAME`.

**`!add(VARNAME)[(INCREMENT)]`**
:   computes `VARNAME+INCREMENT` and stores the result to `VARNAME`.

**`!fr(...)`** or **`!en(...)`**
:   emits some text only if the current language is *fr* or *en*

**`!html(...)`** or **`!pdf(...)`**
:   emits some text only if the current format is *html* or *pdf*

}

DPP
===

Usage
-----

`dpp` is a filter and has no options.
It takes some text with embedded diagrams on `stdin` and generates a text with image links on `stdout`.
Some error messages may be written to `stderr`.

~~~~~ dot doc/img/dpp-pipe1
digraph {
    rankdir = LR
    INPUT [label="input documents" shape=folder color=blue]
    DPP [label=dpp shape=diamond]
    OUTPUT [label="output document" shape=folder color=blue]
    IMG [label="images" shape=folder color=blue]
    ERROR [label="error messages" shape=folder color=red]

    {rank=same; IMG OUTPUT ERROR}

    INPUT -> DPP [label=stdin]
    DPP -> OUTPUT [label=stdout]
    DPP -> IMG [label="file system"]
    DPP -> ERROR [label=stderr]
    IMG -> OUTPUT [label="hyper links"]
}
~~~~~

Being a filter, `dpp` can be chained with other preprocessors.
Another good generic purpose preprocessor is `pp` or `gpp`.

A classical usage of `dpp` along with `pp` and [Pandoc] is:

~~~~~ dot doc/img/dpp-pipe2
digraph {
    rankdir = LR
    INPUT [label="input documents" shape=folder color=blue]
    PP [label=pp shape=diamond]
    DPP [label=dpp shape=diamond]
    PANDOC [label=pandoc shape=diamond]
    IMG [label="images" shape=folder color=blue]
    OUTPUT [label="output document" shape=folder color=blue]

    {rotate=90; rank=same; PP DPP PANDOC}
    {rank=same; IMG OUTPUT}

    INPUT -> PP [label=stdin]
    PP -> DPP [label=stdout]
    DPP -> IMG [label="file system"]
    DPP -> PANDOC [label=stdout]
    PANDOC -> OUTPUT [label=stdout]
    IMG -> OUTPUT [label="hyper links"]
}
~~~~~

For instance, on any good Unix like system, you can use this command:

~~~~~ {.bash}
$ pp documents... | dpp | pandoc -f markdown -t html5 -o document.html
~~~~~

Design
------

`dpp` was initially a preprocessor for [GraphViz] diagrams.
It now also comes with [PlantUML], [ditaa] and scripting capabilities.
`dpp` requires [GraphViz] and Java to be installed,
[PlantUML] and [ditaa] are embedded in `dpp`.

Optionally, `dpp` can call [Bash], [Bat], [Python] or [Haskell] to execute general scripts.

~~~~~ uml doc/img/dpp-design

package DPP {
    stdin -> [Main Processor]
    [Main Processor] -> stdout
    [PlantUML.jar]
    [ditaa.jar]
}

node "Operating System" {
    [GraphViz]
    [Java]
    [Bash]
    [Bat]
    [Python]
    [Haskell]
}

database "Filesystem" {
    folder "input" {
        [Markdown with diagrams]
    }
    folder "output" {
        [images]
        [Markdown with image links]
    }
}

[Markdown with diagrams] --> stdin
[Main Processor] --> [GraphViz]
[Main Processor] --> [Java]
[Main Processor] --> [Bash]
[Main Processor] --> [Bat]
[Main Processor] --> [Python]
[Main Processor] --> [Haskell]
[Java] --> [PlantUML.jar]
[Java] --> [ditaa.jar]
stdout --> [Markdown with image links]
[GraphViz] --> [images]
[PlantUML.jar] -> [images]
[ditaa.jar] -> [images]
[Bash] -> stdout
[Bat] -> stdout
[Python] -> stdout
[Haskell] -> stdout

~~~~~

~~~~~ uml doc/img/dpp-design2

box "input" #Green
    participant stdin
end box
box "DDP" #LightBlue
    participant DPP
    participant "PlantUML or ditaa"
end box
box "external dependencies" #Yellow
    participant Java
    participant GraphViz
    participant "script languages"
end box
box "outputs" #Green
    participant stdout
    participant images
end box

group Normal text line
    stdin -> DPP : non diagram text line
    activate DPP
    DPP -> stdout : unmodified line
    deactivate DPP
end
...
group GraphViz diagram
    stdin -> DPP : diagram
    activate DPP
    DPP -> GraphViz : call
    activate GraphViz
    GraphViz -> images : PNG image
    deactivate GraphViz
    DPP -> stdout : hyper link
    deactivate DPP
end
...
group "PlantUML or ditaa" diagram
    stdin -> DPP : diagram
    activate DPP
    DPP -> Java : call
    activate Java
    Java -> "PlantUML or ditaa" : call
    activate "PlantUML or ditaa"
    "PlantUML or ditaa" -> images : PNG image
    deactivate "PlantUML or ditaa"
    deactivate Java
    DPP -> stdout : hyper link
    deactivate DPP
end
...
group script languages (Bash, Bat, Python or Haskell)
    stdin -> DPP : script
    activate DPP
    DPP -> "script languages" : call
    activate "script languages"
    "script languages" -> stdout : script output
    deactivate "script languages"
    deactivate DPP
end

~~~~~

Syntax
------

### Diagrams

Diagrams are written in code blocks.
The first line contains:

- the diagram generator
- the image name (without the extension)
- the legend (optional)

Block delimiters are made of three or more tilda or back quotes, at the beginning of the line (no space and no tab).
Both lines must have the same number of tilda or back quotes.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.dot}
`````````` quote
~~~~~ dot path/imagename optional legend
graph {
    "source code of the diagram"
}
~~~~~
``````````
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This extremely meaningful diagram is rendered as `path/imagename.png`
and looks like:

~~~~~ dot doc/img/dpp-syntax optional legend
graph {
    "source code of the diagram"
}
~~~~~

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

`dpp` will not create any directory, the path where the image is written must already exist.

~~~~~ dot doc/img/dpp-generators
digraph {

    subgraph cluster_cmd {
        label = "diagram generators"
        dot neato twopi circo fdp sfdp patchwork osage uml ditaa
    }

    DPP [shape=diamond]
    dot neato twopi circo fdp sfdp patchwork osage uml ditaa
    GraphViz [shape=box]
    PlantUML [shape=box]
    DITAA [shape=box label=ditaa]

    DPP -> {dot neato twopi circo fdp sfdp patchwork osage uml ditaa}
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
~~~~~

### Scripts

Scripts are also written in code blocks.
The first line contains only the kind of script.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.bash}
`````````` quote
~~~~~ bash
echo Hello World!
~~~~~
``````````
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With no surprise, this script generates:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~ bash
echo Hello World!
~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The script language can be:

- bash (or sh)
- bat (DOS/Windows batch language)
- python
- haskell

`dpp` will create a temporary script before calling the associated interpretor.

~~~~~ dot doc/img/dpp-scripts
digraph {

    subgraph cluster_cmd {
        label = "script languages"
        bash sh bat python haskell
    }

    DPP [shape=diamond]
    bash sh bat python haskell
    Bash [shape=box label="bash\nor bash.exe"]
    Sh [shape=box label="sh\nor sh.exe"]
    Bat [shape=box label="wine cmd /c\nor cmd /c"]
    Python [shape=box label="python\nor python.exe"]
    Haskell [shape=box label="runhaskell\nor runhaskell.exe"]

    DPP -> {bash sh bat python haskell}
    bash -> Bash
    sh -> Sh
    bat -> Bat
    python -> Python
    haskell -> Haskell
}
~~~~~

### Verbatim copy

Blocks can also contain verbatim text that is preserved in the output.
This is useful to put some diagram example in the documentation of `dpp`!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~ quote
`````````` quote
~~~ bash
# this bash script example won't be executed!
~~~
``````````
~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

becomes

~~~~~ quote
`````````` quote
~~~ bash
# this bash script example won't be executed!
~~~
``````````
~~~~~

Examples
--------

The [source code](pp.md) of this document contains some diagrams.

Here are some simple examples.
For further details about diagrams' syntax, please read the documentation of
[GraphViz], [PlantUML] and [ditaa].

### Graphviz

[GraphViz] is executed when one of these keywords is used:
`dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, `patchwork`, `osage`

~~~~~~~~~~~~~~ {.dot}
`````````` quote
~~~~~ twopi doc/img/dpp-graphviz-example This is just a GraphViz diagram example
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
~~~~~
``````````
~~~~~~~~~~~~~~

- `twopi` is the kind of graph (possible graph types: `dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, `patchwork`).
- `doc/img/dpp-graphviz-example` is the name of the image. `dpp` will generate `doc/img/dpp-graphviz-example.dot` and `doc/img/dpp-graphviz-example.png`.
- the rest of the first line is the legend of the graph.
- other lines are written to `doc/img/dpp-graphviz-example.dot` before running [Graphviz].

You can use `dpp` in a pipe before [Pandoc][] (as well as `pp` or `gpp`) for instance):

~~~~~ {.bash}
pp file.md | dpp | pandoc -s -S --self-contained -f markdown -t html5 -o file.html

or

cat file.md | gpp -T -x | dpp | pandoc -s -S --self-contained -f markdown -t html5 -o file.html
~~~~~

Once generated the graph looks like:

~~~~~ twopi doc/img/dpp-graphviz-example This is just a GraphViz diagram example
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
~~~~~

[GraphViz] must be installed.

### PlantUML

[PlantUML] is executed when the keyword `uml` is used.
The lines `@@startuml` and `@@enduml` required by [PlantUML] are added by `dpp`.

~~~~~~~~~~~~~~
`````````` quote
~~~~~ uml doc/img/dpp-plantuml-example This is just a PlantUML diagram example
Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response
Alice -> Bob: Another authentication Request
Alice <-- Bob: another authentication Response
~~~~~
``````````
~~~~~~~~~~~~~~

Once generated the graph looks like:

~~~~~ uml doc/img/dpp-plantuml-example This is just a PlantUML diagram example
Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response
Alice -> Bob: Another authentication Request
Alice <-- Bob: another authentication Response
~~~~~

[PlantUML](http://plantuml.sourceforge.net) is written in Java and is embedded in `dpp`.
Java must be installed.

### Ditaa

[ditaa] is executed when the keyword `ditaa` is used.

~~~~~~~~~~~~~~~~~~~~~~
`````````` quote
~~~~~ ditaa doc/img/dpp-ditaa-example This is just a Ditaa diagram example
    +--------+   +-------+    +-------+
    |        | --+ ditaa +--> |       |
    |  Text  |   +-------+    |diagram|
    |Document|   |!magic!|    |       |
    |     {d}|   |       |    |       |
    +---+----+   +-------+    +-------+
        :                         ^
        |       Lots of work      |
        +-------------------------+
~~~~~
``````````
~~~~~~~~~~~~~~~~~~~~~~

Once generated the graph looks like:

~~~~~ ditaa doc/img/dpp-ditaa-example This is just a Ditaa diagram example
    +--------+   +-------+    +-------+
    |        | --+ ditaa +--> |       |
    |  Text  |   +-------+    |diagram|
    |Document|   |!magic!|    |       |
    |     {d}|   |       |    |       |
    +---+----+   +-------+    +-------+
        :                         ^
        |       Lots of work      |
        +-------------------------+
~~~~~

[ditaa](http://plantuml.sourceforge.net) is written in Java and is embedded in `dpp`.
Java must be installed.

### Bash

[Bash] is executed when the keyword `bash` is used.

~~~~~~~~~~ {.bash}
`````````` quote
~~~~~ bash
echo "Hi, I'm $SHELL $BASH_VERSION"
echo "Here are a few random numbers: $RANDOM, $RANDOM, $RANDOM"
~~~~~
``````````
~~~~~~~~~~

This script outputs:

~~~~~~~~~~ {.bat}
~~~~~ bash
echo "Hi, I'm $SHELL $BASH_VERSION"
echo "Here are a few random numbers: $RANDOM, $RANDOM, $RANDOM"
~~~~~
~~~~~~~~~~

**Note**: the keyword `sh` executes `sh` which is generally a link to `bash`.

### Bat

[Bat] is executed when the keyword `bat` is used.

~~~~~~~~~~
`````````` quote
~~~~~ bat
echo Hi, I'm %COMSPEC%
ver
if not "%WINELOADER%" == "" (
    echo This script is run from wine under Linux
) else (
    echo This script is run from a real Windows
)
~~~~~
``````````
~~~~~~~~~~

This script outputs:

~~~~~~~~~~
~~~~~ bat
echo Hi, I'm %COMSPEC%
ver
if "%WINELOADER%" == "" (
    echo This script is run from a real Windows
) else (
    echo This script is run from wine under Linux
)
~~~~~
~~~~~~~~~~

### Python

[Python] is executed when the keyword `python` is used.

~~~~~~~~~~ {.python}
`````````` quote
~~~~~ python
import sys
import random

if __name__ == "__main__":
    print("Hi, I'm Python %s"%sys.version)
    randoms = [random.randint(0, 1000) for i in range(3)]
    print("Here are a few random numbers: %s"%(", ".join(map(str, randoms))))
~~~~~
``````````
~~~~~~~~~~

This script outputs:

~~~~~~~~~~
~~~~~ python
import sys
import random

if __name__ == "__main__":
    print("Hi, I'm Python %s"%sys.version)
    randoms = [random.randint(0, 1000) for i in range(3)]
    print("Here are a few random numbers: %s"%(", ".join(map(str, randoms))))
~~~~~
~~~~~~~~~~

### Haskell

[Haskell] is executed when the keyword `haskell` is used.

~~~~~~~~~~ {.haskell}
`````````` quote
~~~~~ haskell
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
``````````
~~~~~~~~~~

This script outputs:

~~~~~~~~~~
~~~~~ haskell
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

GPP
===

[GPP] is included in [PP] because it's a must have generic text preprocessor
I was using before writing `pp`.

Its documentation is here: [gpp.html](gpp.html)


Licenses
========

PP/DPP
------

Copyright (C) 2015, 2016 Christophe Delord <br>
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

PlantUML.jar is integrated in [DPP].
[PlantUML] is distributed under the [GPL license](http://www.gnu.org/copyleft/gpl.html).
See <http://plantuml.sourceforge.net/faq.html>.

ditaa
-----

ditaa.jar is integrated in [DPP].
[ditaa] is distributed under the [GNU General Public License version 2.0 (GPLv2)](http://sourceforge.net/directory/license:gpl/).
See <http://sourceforge.net/projects/ditaa/>.

GPP
---

[GPP] is included in the binary distribution of PP.
I have just recompiled the original sources of [GPP].

GPP was written by Denis Auroux <auroux@math.mit.edu>.
Since version 2.12 it has been maintained by Tristan Miller <psychonaut@nothingisreal.com>.

Copyright (C) 1996-2001 Denis Auroux.<br>
Copyright (C) 2003, 2004 Tristan Miller.

Permission is granted to anyone to make or distribute verbatim copies
of this document as received, in any medium, provided that the
copyright notice and this permission notice are preserved, thus giving
the recipient permission to redistribute in turn.

Permission is granted to distribute modified versions of this
document, or of portions of it, under the above conditions, provided
also that they carry prominent notices stating who last changed them.

Feedback
========

Your feedback and contributions are welcome.
You can contact me at <http://cdsoft.fr>
