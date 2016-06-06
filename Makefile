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

ifeq "$(OS)" "Linux"

PP 	= pp
DPP = dpp
GPP = gpp

all: gpp pp dpp README.md pp-linux-$(shell uname -m).txz
all: pp.tgz
all: doc/gpp.html doc/pp.html doc/dpp.html

ifneq "$(shell wine ghc --version 2>/dev/null || false)" ""
all: gpp.exe pp.exe dpp.exe pp-win.7z

CCWIN = i686-w64-mingw32-gcc
WINE = wine
endif

else
ifeq "$(OS)" "MINGW32_NT-6.1"

PP 	= pp.exe
DPP = dpp.exe
GPP = gpp.exe

all: gpp.exe pp.exe dpp.exe
all: doc/gpp.html doc/pp.html doc/dpp.html

CCWIN = gcc
WINE =

else
ifeq "$(OS)" "CYGWIN_NT-6.1-WOW"

PP 	= pp.exe
DPP = dpp.exe
GPP = gpp.exe

all: gpp.exe pp.exe dpp.exe
all: doc/gpp.html doc/pp.html doc/dpp.html

CCWIN = mingw32-gcc
WINE =

else
$(error "Unknown platform: $(OS)")
endif
endif
endif

BUILD = .build
CACHE = .cache

install: $(PP) $(DPP) $(GPP)
	install -v -C $^ $(shell (ls -d /usr/local/bin || echo /usr/bin) 2>/dev/null)

clean:
	rm -rf $(BUILD) doc
	rm -f gpp gpp.exe pp pp.exe dpp dpp.exe
	rm -f pp.tgz pp-win.7z pp-linux-*.txz

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
	./$(PP) -en -DREADME $< | pandoc -f markdown -t markdown_github > $@

#####################################################################
# archives
#####################################################################

pp.tgz: Makefile $(wildcard src/*) $(wildcard test/*) README.md LICENSE .gitignore
	tar -czf $@ $^

pp-win.7z: gpp.exe pp.exe dpp.exe doc/gpp.html doc/pp.html doc/dpp.html
	7z -mx9 a $@ $^

pp-linux-%.txz: gpp pp dpp doc/gpp.html doc/pp.html doc/dpp.html
	tar cJf $@ $^

#####################################################################
# GPP
#####################################################################

GPP_URL = http://files.nothingisreal.com/software/gpp/gpp.tar.bz2

gpp: BUILDGPP=$(BUILD)/$@
gpp: $(CACHE)/$(notdir $(GPP_URL))
	@mkdir -p $(BUILDGPP)
	tar xjf $< -C $(BUILDGPP)
	cd $(BUILDGPP)/gpp-* && ./configure && make
	cp $(BUILDGPP)/gpp-*/src/gpp $@
	@strip $@

gpp.exe: BUILDGPP=$(BUILD)/$@
gpp.exe: $(CACHE)/$(notdir $(GPP_URL))
	@mkdir -p $(BUILDGPP)
	tar xjf $< -C $(BUILDGPP)
	export CC=$(CCWIN); cd $(BUILDGPP)/gpp-* && ./configure --host $(shell uname) && make
	cp $(BUILDGPP)/gpp-*/src/gpp.exe $@
	@strip $@

$(CACHE)/$(notdir $(GPP_URL)):
	@mkdir -p $(dir $@)
	wget $(GPP_URL) -O $@

doc/gpp.html: $(GPP)
	@mkdir -p $(dir $@)
	cp $(BUILD)/$</gpp-*/doc/gpp.html $@

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
	sed -i 's/_cache_//g' $@

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
pp: src/pp.hs $(BUILD)/$(PLANTUML).o $(BUILD)/$(DITAA).o
	@mkdir -p $(BUILDPP)
	ghc $(GHCOPT) -odir $(BUILDPP) -hidir $(BUILDPP) -o $@ $^
	@strip $@

pp.exe: BUILDPP=$(BUILD)/$@
pp.exe: src/pp.hs $(BUILD)/$(PLANTUML)-win.o $(BUILD)/$(DITAA)-win.o
	@mkdir -p $(BUILDPP)
	$(WINE) ghc $(GHCOPT) -odir $(BUILDPP) -hidir $(BUILDPP) -o $@ $^
	@strip $@

doc/pp.html: $(PP) doc/pp.css
doc/pp.html: src/pp.md
	@mkdir -p $(dir $@) doc/img
	./$(PP) -en $< | pandoc -S --toc --self-contained -c doc/pp.css -f markdown -t html5 > $@

doc/pp.css: $(CACHE)/pp.css
	@mkdir -p $(dir $@)
	cp $< $@

#####################################################################
# DPP
#####################################################################

dpp: src/dpp.c $(BUILD)/$(PLANTUML).o $(BUILD)/$(DITAA).o
	gcc -Werror -Wall $^ -o $@
	@strip $@

dpp.exe: src/dpp.c $(BUILD)/$(PLANTUML)-win.o $(BUILD)/$(DITAA)-win.o
	$(CCWIN) -Werror -Wall $^ -o $@
	@strip $@

doc/dpp.html: $(PP) $(DPP) doc/pp.css
doc/dpp.html: src/dpp.md
	@mkdir -p $(dir $@) doc/img
	./$(PP) -en $< | ./$(DPP) | pandoc -S --toc --self-contained -c doc/pp.css -f markdown -t html5 > $@

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
	TESTENVVAR=42 ./$(PP) -en -html $< > $@
	pandoc -S --toc -c doc/pp.css -f markdown -t html5 $@ -o $(@:.output=.html)

.PHONY: ref
ref: $(BUILD)/pp-test.output
	meld $< test/pp-test.ref
