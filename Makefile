# PP
# Copyright (C) 2015-2020 Christophe Delord
# http://cdelord.fr/pp
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

SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

title = /bin/echo -e "\x1b[1m\x1b[32m\#\#\#\# $1\x1b[0m"
ok = /bin/echo -e "\x1b[1m\x1b[32m[OK] $1\x1b[0m"

#####################################################################
# Platform detection
#####################################################################

OS := $(shell uname)

export STACK_WORK	:= .stack-work
BUILD 				:= $(STACK_WORK)
BIN_DIR 			:= $(shell stack path --local-install-root)/bin

PP 		= $(BIN_DIR)/pp
PP_WINE	= $(BUILD)/wine/pp.exe

compile: $(PP)
doc: README.md doc/pp.html
dist: pp.tgz

all: compile doc test dist

# Linux
ifeq "$(OS)" "Linux"

dist: pp-linux-$(shell uname -m).txz
.PHONY: wine
wine: pp-win.7z

# MacOS
else ifeq "$(OS)" "Darwin"

dist: pp-darwin-$(shell uname -m).txz

# Windows
else ifeq "$(shell echo $(OS) | grep 'MSYS\|MINGW\|CYGWIN' >/dev/null && echo Windows)" "Windows"

dist: pp-win.7z

# Unknown platform (please contribute ;-)
else
$(error "Unknown platform: $(OS)")
endif

RESOLVER := $(shell awk '$$1=="resolver:" { print $$2 }' stack.yaml)

TAG = src/Tag.hs
$(shell ( test -d .git || ! test -f $(TAG) ) && ./tag.sh $(TAG))

install: $(PP)
	@$(call title,"installing $<")
	stack install
	@$(call ok,"installation done")

clean:
	stack clean
	rm -rf $(BUILD)
	rm -f doc/pp.html
	rm -rf doc/img
	rm -f pp*.{tgz,7z,txz}

.DELETE_ON_ERROR:

#####################################################################
# README
#####################################################################

README.md: $(PP)
README.md: doc/pp.md
	@$(call title,"preprocessing $<")
	@mkdir -p doc/img
	stack exec -- pp -en -img=doc/img -DRESOLVER=$(RESOLVER) -DREADME $< | pandoc -f markdown -t gfm > $@
	@$(call ok,"$@")

#####################################################################
# archives
#####################################################################

pp.tgz: Makefile $(wildcard app/*) $(wildcard doc/pp.*) $(wildcard src/*) $(wildcard tools/*) $(wildcard test/*) $(wildcard test/subdir/*) tag.sh README.md LICENSE .gitignore Setup.hs pp.cabal stack.yaml package.yaml
	@$(call title,"source archive: $@")
	tar -czf $@ $^

ifeq "$(OS)" "Linux"
pp-win.7z: $(PP_WINE)
else
pp-win.7z: $(PP)
endif
pp-win.7z: doc/pp.html
	@$(call title,"Windows binary archive: $@")
	7z -mx9 a $@ $(shell realpath $^)

pp-linux-%.txz: $(PP) doc/pp.html
	@$(call title,"Linux binary archive: $@")
	tar cJf $@ --transform='s,.*/,,' $^

pp-darwin-%.txz: $(PP) doc/pp.html
	@$(call title,"MacOS binary archive: $@")
	gtar cJf $@ --transform='s,.*/,,' $^

#####################################################################
# Dependencies
#####################################################################

PLANTUML = Plantuml
PLANTUML_URL_1 = http://sourceforge.net/projects/plantuml/files/plantuml.jar
PLANTUML_URL_2 = http://cdelord.fr/pp/plantuml.jar

DITAA = Ditaa
DITAA_URL = https://github.com/stathissideris/ditaa/releases/download/v0.11.0/ditaa-0.11.0-standalone.jar

BLOB = tools/blob.hs

$(BUILD)/%Jar_c.c $(BUILD)/%Jar.hs: $(BUILD)/%.jar $(BLOB)
	@$(call title,"converting $< to C")
	@mkdir -p $(dir $@)
	stack --resolver $(RESOLVER) $(BLOB) $<
	@$(call ok,"$@")

$(BUILD)/$(PLANTUML).jar:
	@$(call title,"downloading $(notdir $@)")
	@mkdir -p $(dir $@)
	wget $(PLANTUML_URL_1) -O $@ || wget $(PLANTUML_URL_2) -O $@
	@$(call ok,"$@")

$(BUILD)/$(DITAA).jar:
	@$(call title,"downloading $(notdir $@)")
	@mkdir -p $(dir $@)
	wget $(DITAA_URL) -O $@
	@$(call ok,"$@")

#####################################################################
# PP
#####################################################################

LIB_SOURCES = $(wildcard src/*.hs)
LIB_SOURCES += $(BUILD)/$(PLANTUML)Jar_c.c $(BUILD)/$(PLANTUML)Jar.hs
LIB_SOURCES += $(BUILD)/$(DITAA)Jar_c.c $(BUILD)/$(DITAA)Jar.hs
PP_SOURCES = app/pp.hs

$(PP): package.yaml
$(PP): $(PP_SOURCES) $(LIB_SOURCES)
	@$(call title,"building $@")
	stack build
	@$(call ok,"$@")

$(PP_WINE): package.yaml
$(PP_WINE): $(PP_SOURCES) $(LIB_SOURCES)
	@$(call title,"building $@")
	@mkdir -p $(dir $@)
	wine ghc --make $(PP_SOURCES) $(LIB_SOURCES) -o $@ -dumpdir $(dir $@) -hidir $(dir $@) -odir $(dir $@) -outputdir $(dir $@) -stubdir $(dir $@) -tmpdir $(dir $@)
	@$(call ok,"$@")

doc/pp.html: $(PP) doc/pp.css
doc/pp.html: doc/pp.md
	@$(call title,"preprocessing $<")
	@mkdir -p $(dir $@) doc/img
	stack exec -- pp -en -img=doc/img -DRESOLVER=$(RESOLVER) $< | pandoc --toc --self-contained -c doc/pp.css -f markdown -t html5 > $@
	@$(call ok,"$@")

#####################################################################
# tests
#####################################################################

.PHONY: test hspec test-md test-rst test-formats

test: hspec test-md test-rst test-md-d test-formats
	@$(call ok,"all pp tests passed!")

# Hspec tests

hspec: test/Spec.hs $(LIB_SOURCES)
	stack test

# Markdown test

test-md: $(BUILD)/pp-test.output test/pp-test.ref
	@$(call title,"checking $<")
	diff -b -B $^
	@$(call ok,"Markdown test passed!")

$(BUILD)/pp-test.output: $(PP) doc/pp.css
$(BUILD)/pp-test.output: test/pp-test.md test/subdir/pp-test.i test/subdir/pp-test-lib.i test/subdir/mustache.yaml test/subdir/mustache.json
	@$(call title,"preprocessing $<")
	@mkdir -p $(BUILD)/img
	TESTENVVAR=42 stack exec -- pp -md -img="[$(BUILD)/]img" -DRESOLVER=$(RESOLVER) -en -html -import=test/subdir/pp-test-lib.i $< > $@
	pandoc --toc -c doc/pp.css -f markdown -t html5 $@ -o $(@:.output=.html)

.PHONY: ref
ref: $(BUILD)/pp-test.output
	meld $< test/pp-test.ref 2>/dev/null

# reStructuredText test

test-rst: $(BUILD)/pp-test-rst.output test/pp-test-rst.ref
	@$(call title,"checking $<")
	diff -b -B $^
	@$(call ok,"reStructuredText test passed!")

$(BUILD)/pp-test-rst.output: $(PP) doc/pp.css
$(BUILD)/pp-test-rst.output: test/pp-test.rst
	@$(call title,"preprocessing $<")
	@mkdir -p $(BUILD)/img
	TESTENVVAR=42 stack exec -- pp -rst -img="[$(BUILD)/]img" -en -html $< > $@
	pandoc --toc -c doc/pp.css -f rst -t html5 $@ -o $(@:.output=.html)

.PHONY: ref-rst
ref-rst: $(BUILD)/pp-test-rst.output
	meld $< test/pp-test-rst.ref 2>/dev/null

# Dependency list test

test-md-d: $(BUILD)/pp-test.d test/pp-test.d.ref
	@$(call title,"checking $<")
	diff -b -B $^
	@$(call ok,"Dependency test passed!")

$(BUILD)/pp-test.d: $(PP)
$(BUILD)/pp-test.d: test/pp-test.md test/subdir/pp-test.i test/subdir/pp-test-lib.i
	@$(call title,"tracking dependencies of $<")
	@mkdir -p $(BUILD)/img
	TESTENVVAR=42 stack exec -- pp -M outputfile -md -img="[$(BUILD)/]img" -DRESOLVER=$(RESOLVER) -en -html -import=test/subdir/pp-test-lib.i $< > $@

.PHONY: ref-d
ref-d: $(BUILD)/pp-test.d
	meld $< test/pp-test.d.ref 2>/dev/null

# Diagram formats tests

test-formats: $(BUILD)/test-format.html \
			  $(BUILD)/test-format.pdf \
			  $(BUILD)/test-format.odt \
			  $(BUILD)/test-format.epub

$(BUILD)/test-format.html: $(PP)
$(BUILD)/test-format.html: test/formats.md
	@$(call title,"checking default diagram formats for HTML documents")
	stack exec -- pp -img="[$(BUILD)/]img" -html $< | pandoc -t html -o $@
	@$(call ok,"default diagram formats for HTML documents works!")

$(BUILD)/test-format.pdf: $(PP)
$(BUILD)/test-format.pdf: test/formats.md
	@$(call title,"checking default diagram formats for PDF documents")
	stack exec -- pp -img="$(BUILD)/img" -pdf $< | pandoc -t latex -o $@
	@$(call ok,"default diagram formats for PDF documents works!")

$(BUILD)/test-format.odt: $(PP)
$(BUILD)/test-format.odt: test/formats.md
	@$(call title,"checking default diagram formats for ODT documents")
	stack exec -- pp -img="$(BUILD)/img" -odf $< | pandoc -t odt -o $@
	@$(call ok,"default diagram formats for ODT documents works!")

$(BUILD)/test-format.epub: $(PP)
$(BUILD)/test-format.epub: test/formats.md
	@$(call title,"checking default diagram formats for EPUB documents")
	stack exec -- pp -img="$(BUILD)/img" -epub $< | pandoc -t epub -o $@
	@$(call ok,"default diagram formats for EPUB documents works!")
