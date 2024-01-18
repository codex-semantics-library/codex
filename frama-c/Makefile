##########################################################################
#  This file is part of the Codex semantics library.                     #
#                                                                        #
#  Copyright (C) 2013-2024                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file LICENSE).                      #
#                                                                        #
##########################################################################

.PHONY: all build clean

FRAMAC_SHARE:=$(shell frama-c -print-share-path)

sinclude ${FRAMAC_SHARE}/Makefile.common

##########################################################################
# Build

all:: build frama-c-codex.opam

frama-c-codex.opam: frama-c-codex.opam.template dune-project
	rm -f $@
	dune build $@

build:: codex
	dune build @install  --display=short

clean:: purge-tests
	dune clean
	rm -rf _build .merlin

codex::
	cd .. && dune build codex.install --display=short

./_build/default/utils/diff_tool::
	cd ../utils/diff_tool && dune build

##########################################################################
# Tests

#include ${FRAMAC_SHARE}/Makefile.testing

test.opt: build
	dune exec frama-c -- -machdep x86_32  -codex-debug 2 -kernel-debug 2 $(msg-keys) -codex -codex-print  -codex-no-print-value ../test.c -ulevel -1 -codex-exp-dump test.dump -no-allow-duplication -codex-html-dump output.html -codex-type-file test.types -codex-use-type-domain && cat test.dump #  -absolute-valid-range 0x1-0x7FF

tests.opt: build ./_build/default/utils/diff_tool
	- cd tests && byte="" dune exec make -- -B -k -j8  clean all ; cd .. && dune exec diff_tool tests > results-junit.xml #; make update


purge-tests: 

##########################################################################
# Install

include ${FRAMAC_SHARE}/Makefile.installation

##########################################################################
# Headers

# HDRCK_EXTRA:=-headache-config-file ${FRAMAC_SHARE}/headache_config.txt
# include ${FRAMAC_SHARE}/Makefile.headers

# SHORT_NAME:=codex
# LONG_NAME:=Codex
# PLUGIN_NAME:=Codex
# FROM_YEAR:=2022

# include headers/Makefile.generate-headers

##########################################################################
# Linting

include ${FRAMAC_SHARE}/Makefile.linting

##########################################################################
creduce:
	dune exec creduce -- ./reduce.sh reduce.c
