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

# See "make help" for a list of targets and brief documentation

FRAMAC_SHARE:=$(shell frama-c -print-share-path)

sinclude ${FRAMAC_SHARE}/Makefile.common

# set to ON/OFF to toggle ANSI escape sequences
COLOR = ON

# padding for help on targets
# should be > than the longest target
HELP_PADDING = 15

ifeq ($(COLOR),ON)
	color_yellow = \033[93;1m
	color_orange = \033[33m
	color_red    = \033[31m
	color_green  = \033[32m
	color_blue   = \033[34;1m
	color_reset  = \033[0m
endif

##########################################################################
# Build

all: build
.PHONY: all

build: codex ## Build frama_c_codex
	cd .. && dune build frama_c_codex.install --display=short
.PHONY: build

clean: purge-tests ## Remove build files
	dune clean
	rm -rf _build .merlin
.PHONY: clean

codex: ## Build codex
	cd .. && dune build codex.install --display=short
.PHONY: codex

##########################################################################
# Tests

#include ${FRAMAC_SHARE}/Makefile.testing

FRAMA_C=frama-c
#FRAMA_C=frama_c_codex
TEST_FLAGS = -codex -machdep x86_32 -codex-debug 2 -kernel-debug 2 $(msg-keys) \
	-codex-print -codex-no-print-value -ulevel -1 \
	../test.c \
	-codex-exp-dump test.dump \
	-codex-html-dump output.html \
	-codex-type-file test.types
# -no-allow-duplication -codex-use-type-domain

test.opt: build  ## Build and run codex on root/test.c
	dune exec $(FRAMA_C) -- $(TEST_FLAGS) && cat test.dump
.PHONY: test.opt

test.diff: build  ## Same as test.opt, but shows a diff with installed version of codex
	$(FRAMA_C) $(TEST_FLAGS) > works.dump && cat test.dump >> works.dump #  -absolute-valid-range 0x1-0x7FF
	dune exec $(FRAMA_C) -- $(TEST_FLAGS) > cur.dump && cat test.dump >> cur.dump #  -absolute-valid-range 0x1-0x7FF
	diff --color=always --tabsize 2 -u ./works.dump ./cur.dump
.PHONY: test.diff

tests.opt: build  ## Run all tests
	- cd tests && byte="" dune exec make -- -B -k -j8  clean all ; cd .. && diff2junit tests > results-junit.xml #; make update
.PHONY: tests.opt

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

creduce: ## Run creduce using frama-c/reduce.sh script on frama-c/reduce.c
	creduce ./reduce.sh reduce.c
.PHONY: creduce

.PHONY: help
help:: ## Show this help
	@echo "$(color_yellow)make codex/frama-c:$(color_reset) list of useful targets :"
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_blue)%-$(HELP_PADDING)s$(color_reset) %s\n", $$1, $$2}'
