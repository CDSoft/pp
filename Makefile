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

ifeq "$(OS)" "Linux"

all: gpp pp dpp pp-linux-$(shell uname -m).tgz
all: gpp.exe pp.exe dpp.exe pp-win.zip
all: pp.tgz
all: doc/gpp.html doc/pp.html

CCWIN = i686-w64-mingw32-gcc
WINE = wine

else
ifeq "$(OS)" "MINGW32_NT-6.1"

# Target not tested, feedback welcome!

all: gpp.exe pp.exe dpp.exe

CCWIN = gcc
WINE =

else
$(error "Unknown platform: $(OS)")
endif
endif

BUILD = .build
CACHE = .cache

clean:
	rm -rf $(BUILD) doc
	rm -f gpp gpp.exe pp pp.exe dpp dpp.exe
	rm -f pp.tgz pp-win.zip pp-linux-*.tgz

#####################################################################
# archives
#####################################################################

pp.tgz: Makefile $(wildcard src/*) README.md LICENSE .gitignore
	tar -czf $@ $^

pp-win.zip: gpp.exe pp.exe dpp.exe doc/gpp.html doc/pp.html
	zip $@ $^

pp-linux-%.tgz: gpp pp dpp doc/gpp.html doc/pp.html
	tar czf $@ $^

#####################################################################
# GPP
#####################################################################

GPP_URL = http://files.nothingisreal.com/software/gpp/gpp.tar.bz2

gpp: BUILDGPP=$(BUILD)/$@
gpp: $(CACHE)/$(notdir $(GPP_URL))
	mkdir -p $(BUILDGPP)
	tar xjf $< -C $(BUILDGPP)
	cd $(BUILDGPP)/gpp-* && ./configure && make
	cp $(BUILDGPP)/gpp-*/src/gpp $@
	strip $@

gpp.exe: BUILDGPP=$(BUILD)/$@
gpp.exe: $(CACHE)/$(notdir $(GPP_URL))
	mkdir -p $(BUILDGPP)
	tar xjf $< -C $(BUILDGPP)
	export CC=$(CCWIN); cd $(BUILDGPP)/gpp-* && ./configure --host $(shell uname) && make
	cp $(BUILDGPP)/gpp-*/src/gpp.exe $@
	strip $@

$(CACHE)/$(notdir $(GPP_URL)):
	mkdir -p $(dir $@)
	wget $(GPP_URL) -O $@

doc/gpp.html: gpp
	mkdir -p $(dir $@)
	cp $(BUILD)/$</gpp-*/doc/gpp.html $@

#####################################################################
# PP
#####################################################################

pp: BUILDPP=$(BUILD)/$@
pp: src/pp.hs
	mkdir -p $(BUILDPP)
	ghc -Werror -Wall -O2 -odir $(BUILDPP) -hidir $(BUILDPP) -o $@ $<
	strip $@

pp.exe: BUILDPP=$(BUILD)/$@
pp.exe: src/pp.hs
	mkdir -p $(BUILDPP)
	$(WINE) ghc -Werror -Wall -O2 -odir $(BUILDPP) -hidir $(BUILDPP) -o $@ $<
	strip $@

doc/pp.html: pp dpp doc/pp.css
doc/pp.html: src/pp.md
	mkdir -p doc/img
	LANG=en pp $< | dpp | pandoc -S --toc --self-contained -c doc/pp.css -f markdown -t html5 > $@

doc/pp.css:
	wget http://cdsoft.fr/cdsoft.css -O $@

#####################################################################
# DPP
#####################################################################

PLANTUML = plantuml
PLANTUML_URL = http://heanet.dl.sourceforge.net/project/plantuml/$(PLANTUML).jar

DITAA_VERSION = 0.9
DITAA = ditaa0_9
DITAA_URL = http://freefr.dl.sourceforge.net/project/ditaa/ditaa/$(DITAA_VERSION)/$(DITAA).zip

DPP_EXTERNAL = -Dplantuml_jar=_cache_$(PLANTUML)_jar \
               -Dplantuml_jar_len=_cache_$(PLANTUML)_jar_len \
               -Dditaa_jar=_build_$(DITAA)_jar \
               -Dditaa_jar_len=_build_$(DITAA)_jar_len \

dpp: src/dpp.c $(BUILD)/$(PLANTUML).c $(BUILD)/$(DITAA).c
	gcc -Werror -Wall $(DPP_EXTERNAL) $^ -o $@
	strip $@

dpp.exe: src/dpp.c $(BUILD)/$(PLANTUML).c $(BUILD)/$(DITAA).c
	$(CCWIN) -Werror -Wall $(DPP_EXTERNAL) $^ -o $@
	strip $@

$(BUILD)/%.c: $(BUILD)/%.jar
	xxd -i $< $@

$(BUILD)/%.c: $(CACHE)/%.jar
	xxd -i $< $@

$(CACHE)/$(PLANTUML).jar:
	wget $(PLANTUML_URL) -O $@

$(CACHE)/$(DITAA).zip:
	wget $(DITAA_URL) -O $@

$(BUILD)/$(DITAA).jar: $(CACHE)/$(DITAA).zip
	unzip $< $(notdir $@) -d $(dir $@)
	touch $@
