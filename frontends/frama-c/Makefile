##########################################################################
#  This file is part of the Codex semantics library.                     #
#                                                                        #
#  Copyright (C) 2013-2025                                               #
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


##########################################################################
# Help

include ../../Makefile.common

.PHONY: help
help:: ## Show this help
	$(call PRINT_HELP,-C frontends/frama-c)

##########################################################################
# Build

all: build
.PHONY: all

plugin:  ## Build the Frama-C/Codex plugin
# Dune to the plugin environment? dune does not automatically build these dependencies.
	cd ../../utils/patricia-tree && dune build @install
	cd ../../utils/cudd.ml && dune build @install
	cd ../.. && dune build frama_c_codex.install --display=short
.PHONY: plugin

build: codex plugin frama_c_codex.exe
.PHONY: build


frama_c_codex.exe:
	dune build frama_c_codex.exe

clean: purge-tests ## Remove build files
	dune clean
	rm -rf _build .merlin
.PHONY: clean

codex:
	cd ../.. && dune build codex.install --display=short
.PHONY: codex

##########################################################################
# Tests

FUNCTION_TO_ANALYZE = main

FRAMA_C=frama-c
#FRAMA_C=frama_c_codex
TEST_FLAGS = -codex -machdep x86_32 -codex-debug 2 -kernel-debug 2 $(msg-keys) \
	-ulevel -1 \
	test.c \
	-codex-exp-dump \
	-codex-html-dump \
	-codex-overflow-alarms \
	-codex-use-type-domain -codex-type-file test.types \
	-codex-analyze-functions $(FUNCTION_TO_ANALYZE)
# -no-allow-duplication -codex-use-type-domain

test.opt: build  ## Build and run Frama-C/Codex plugin on root/test.c
	dune exec $(FRAMA_C) -- $(TEST_FLAGS) && cat $(FUNCTION_TO_ANALYZE).cdump
.PHONY: test.opt

test.diff: build
	$(FRAMA_C) $(TEST_FLAGS) > works.dump; cat main.cdump >> works.dump #  -absolute-valid-range 0x1-0x7FF
	dune exec $(FRAMA_C) -- $(TEST_FLAGS) > cur.dump; cat main.cdump >> cur.dump #  -absolute-valid-range 0x1-0x7FF
	diff --color=always --tabsize 2 -u ./works.dump ./cur.dump
.PHONY: test.diff

tests/integration: build  ## Whole-program integration tests
	- cd tests/integration && make -B -k -j `nproc`  clean all ; cd .. && diff2junit tests > results-junit.xml
.PHONY: tests.opt

tests/types: build  ## Some tests on types.
	- make -C tests/types -B -k -j `nproc` clean all ; cd tests && diff2junit tests > results-junit.xml
.PHONY: tests.opt

##########################################################################

creduce: ## Run creduce using c/reduce.sh script on reduce.c
	creduce ./reduce.sh reduce.c
.PHONY: creduce

##########################################################################
# Install

install: ## Install the Codex plugin with Frama-C plugins
	make -f Makefile.frama-c install


uninstall: ## Uninstall the Codex plugin
	make -f Makefile.frama-c uninstall

