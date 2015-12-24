# PP
# Copyright (C) 2015 Christophe Delord
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

all: gpp gpp.exe pp pp.exe dpp dpp.exe
all: pp.tgz pp-win.zip
all: doc/gpp.html doc/pp.html

CCWIN = i686-w64-mingw32-gcc
WINE = wine

else
$(error "Unknown platform: $(OS)")
endif

BUILD = .build
CACHE = .cache

clean:
	rm -rf $(BUILD) doc
	rm -f gpp gpp.exe pp pp.exe dpp dpp.exe
	rm pp.tgz pp-win.zip

#####################################################################
# archives
#####################################################################

pp.tgz: Makefile $(wildcard src/*)
	tar -czf $@ $^

pp-win.zip: gpp.exe pp.exe dpp.exe doc/gpp.html doc/pp.html
	zip $@ $^

#####################################################################
# GPP
#####################################################################

GPP_URL = http://files.nothingisreal.com/software/gpp/gpp.tar.bz2

gpp: $(CACHE)/$(notdir $(GPP_URL))
	mkdir -p $(BUILD)/gpp
	tar xjf $< -C $(BUILD)/gpp
	cd $(BUILD)/gpp/gpp-* && ./configure && make
	cp $(BUILD)/gpp/gpp-*/src/gpp $@

gpp.exe: $(CACHE)/$(notdir $(GPP_URL))
	mkdir -p $(BUILD)/gpp.exe
	tar xjf $< -C $(BUILD)/gpp.exe
	export CC=$(CCWIN); cd $(BUILD)/gpp.exe/gpp-* && ./configure --host $(shell uname) && make
	cp $(BUILD)/gpp.exe/gpp-*/src/gpp.exe $@

$(CACHE)/$(notdir $(GPP_URL)):
	mkdir -p $(dir $@)
	wget $(GPP_URL) -O $@

doc/gpp.html: gpp
	mkdir -p $(dir $@)
	cp $(BUILD)/gpp/gpp-*/doc/gpp.html $@

#####################################################################
# PP
#####################################################################

pp: src/pp.hs
	mkdir -p $(BUILD)/pp
	ghc -Werror -Wall -O2 -odir $(BUILD)/pp -hidir $(BUILD)/pp -o $@ $<

pp.exe: src/pp.hs
	mkdir -p $(BUILD)/pp.exe
	$(WINE) ghc -Werror -Wall -O2 -odir $(BUILD)/pp.exe -hidir $(BUILD)/pp.exe -o $@ $<

doc/pp.html: pp dpp
doc/pp.html: src/pp.md
	mkdir -p doc/img
	LANG=en pp $< | dpp | pandoc -S --toc --self-contained -c http://cdsoft.fr/cdsoft.css -f markdown -t html5 > $@

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
	gcc $(DPP_EXTERNAL) $^ -o $@

dpp.exe: src/dpp.c $(BUILD)/$(PLANTUML).c $(BUILD)/$(DITAA).c
	$(CCWIN) $(DPP_EXTERNAL) $^ -o $@

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
