# PP
# Copyright (C) 2015, 2016 Christophe Delord
# http://www.cdsoft.fr/pp
#
# This file is part of PP.
#
# PP is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PP is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PP.  If not, see <http://www.gnu.org/licenses/>.

#####################################################################
# Platform detection
#####################################################################

OS = $(shell uname)

GHCOPT = -Wall -Werror -O2

# Linux
ifeq "$(OS)" "Linux"

PP 	= pp

all: pp
all: README.md doc/pp.html
all: pp.tgz
all: pp-linux-$(shell uname -m).txz

WINEGHC = $(shell wine ghc --version 2>/dev/null || false)
GCCVERSION = $(shell gcc --version | head -1 | sed 's/.* \([0-9][0-9]*\)\..*/\1/')

# On Ubuntu 16.10, ghc 8.0.1 fails compiling hCalc with gcc 6 without -no-pie
ifeq "$(GCCVERSION)" "6"
GHCOPT_LINUX = -optl-no-pie
endif

ifneq "$(WINEGHC)" ""
all: pp.exe pp-win.7z

WINE = wine
endif

# MacOS
else ifeq "$(OS)" "Darwin"

PP 	= pp

all: pp
all: README.md doc/pp.html
all: pp.tgz
all: pp-darwin-$(shell uname -m).txz

# Windows
else ifeq "$(shell echo $(OS) | grep 'MSYS\|MINGW\|CYGWIN' >/dev/null && echo Windows)" "Windows"

PP 	= pp.exe

all: pp.exe
all: doc/pp.html

# Unknown platform (please contribute ;-)
else
$(error "Unknown platform: $(OS)")
endif

BUILD = .build
CACHE = .cache

TAG = src/Tag.hs
$(shell ./tag.sh $(TAG))
SOURCES = $(wildcard src/*.hs)

install: $(PP)
	install -v -C $^ $(shell (ls -d /usr/local/bin || echo /usr/bin) 2>/dev/null)

clean:
	rm -rf $(BUILD) doc
	rm -f pp pp.exe
	rm -f pp*.tgz pp-win.7z pp-linux-*.txz pp-darwin-*.txz

distclean: clean
	rm -rf $(CACHE)

dep:
	cabal update
	cabal install strict temporary
ifneq "$(WINE)" ""
	$(WINE) cabal update
	$(WINE) cabal install strict temporary
endif

.DELETE_ON_ERROR:

#####################################################################
# README
#####################################################################

README.md: $(PP)
README.md: src/pp.md
	@mkdir -p doc/img
	./$(PP) -en -img=doc/img -DREADME $< | pandoc -f markdown -t markdown_github > $@

#####################################################################
# archives
#####################################################################

pp.tgz: Makefile $(wildcard src/*) $(wildcard test/*) README.md LICENSE .gitignore
	tar -czf $@ $^

pp-win.7z: pp.exe doc/pp.html
	7z -mx9 a $@ $^

pp-linux-%.txz: pp doc/pp.html
	tar cJf $@ $^

pp-darwin-%.txz: pp doc/pp.html
	tar cJf $@ $^

#####################################################################
# Dependencies
#####################################################################

PLANTUML = plantuml
PLANTUML_URL = http://heanet.dl.sourceforge.net/project/plantuml/$(PLANTUML).jar

DITAA_VERSION = 0.9
DITAA = ditaa0_9
DITAA_URL = http://freefr.dl.sourceforge.net/project/ditaa/ditaa/$(DITAA_VERSION)/$(DITAA).zip

$(BUILD)/%.o: $(BUILD)/%.c
	ghc $(GHCOPT) -c -o $@ $^

$(BUILD)/%-win.o: $(BUILD)/%.c
	$(WINE) ghc $(GHCOPT) -c -o $@ $^

$(BUILD)/%.c: $(CACHE)/%.jar
	@mkdir -p $(dir $@)
	xxd -i $< $@
	sed -i -e 's/_cache_//g' $@

$(CACHE)/$(PLANTUML).jar:
	@mkdir -p $(dir $@)
	wget $(PLANTUML_URL) -O $@

$(CACHE)/$(DITAA).zip:
	@mkdir -p $(dir $@)
	wget $(DITAA_URL) -O $@

$(CACHE)/$(DITAA).jar: $(CACHE)/$(DITAA).zip
	unzip $< $(notdir $@) -d $(dir $@)
	@touch $@

$(CACHE)/pp.css:
	@mkdir -p $(dir $@)
	wget http://cdsoft.fr/cdsoft.css -O $@

#####################################################################
# PP
#####################################################################

pp: BUILDPP=$(BUILD)/$@
pp: $(SOURCES) $(BUILD)/$(PLANTUML).o $(BUILD)/$(DITAA).o
	@mkdir -p $(BUILDPP)
	ghc $(GHCOPT) $(GHCOPT_LINUX) -odir $(BUILDPP) -hidir $(BUILDPP) -o $@ $^
	@strip $@

pp.exe: BUILDPP=$(BUILD)/$@
pp.exe: $(SOURCES) $(BUILD)/$(PLANTUML)-win.o $(BUILD)/$(DITAA)-win.o
	@mkdir -p $(BUILDPP)
	$(WINE) ghc $(GHCOPT) -odir $(BUILDPP) -hidir $(BUILDPP) -o $@ $^
	@strip $@

doc/pp.html: $(PP) doc/pp.css
doc/pp.html: src/pp.md
	@mkdir -p $(dir $@) doc/img
	./$(PP) -en -img=doc/img $< | pandoc -S --toc --self-contained -c doc/pp.css -f markdown -t html5 > $@

doc/pp.css: $(CACHE)/pp.css
	@mkdir -p $(dir $@)
	cp $< $@

#####################################################################
# tests
#####################################################################

.PHONY: test
test: $(BUILD)/pp-test.output test/pp-test.ref
	diff -b -B $^
	@echo "Test passed!"

$(BUILD)/pp-test.output: $(PP) doc/pp.css
$(BUILD)/pp-test.output: test/pp-test.md test/pp-test.i
	@mkdir -p $(BUILD)/img
	TESTENVVAR=42 ./$(PP) -img="[$(BUILD)/]img" -en -html $< > $@
	pandoc -S --toc -c doc/pp.css -f markdown -t html5 $@ -o $(@:.output=.html)

.PHONY: ref
ref: $(BUILD)/pp-test.output
	meld $< test/pp-test.ref 2>/dev/null
