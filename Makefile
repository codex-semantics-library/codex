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

# set to ON/OFF to toggle ANSI escape sequences
COLOR = ON

# padding for help on targets
# should be > than the longest target
HELP_PADDING = 20

ifeq ($(COLOR),ON)
	color_yellow = \033[93;1m
	color_orange = \033[33m
	color_red    = \033[31m
	color_green  = \033[32m
	color_blue   = \033[34;1m
	color_reset  = \033[0m
endif

test.opt:  ## Build and run frama-c/codex on root/test.c
	cd frama-c && make test.opt
.PHONY: test.opt

tests.opt:  ## Build and run frama-c/codex on all tests
	cd frama-c && make tests.opt
.PHONY: tests.opt

test.diff:  ## Same as test.opt, but shows a diff with installed version of codex
	cd frama-c && make test.diff
.PHONY: test.diff

dune: ## Build the project with dune
	dune build
.PHONY: dune

binsec: ## Build binsec_codex.exe
	dune build binsec/binsec_codex.exe
.PHONY: binsec

frama_c_codex: ## Build frama_c_codex.exe
	dune build frama-c/frama_c_codex.exe
	dune build -p codex,frama_c_codex
.PHONY: frama_c_codex

frama_c_codex_plugin: ## Build the codex frama-c plugin
	dune build frama-c/frama_c_codex.exe
	dune build -p codex,frama_c_codex,frama_c_codex_plugin
.PHONY: frama_c_codex_plugin

clean: ## Remove build files and executable
	dune clean
.PHONY: clean

doc: ## Build the documentation (accessible at _build/default/_doc/_html)
	dune build --display short @doc-private
	@echo "$(color_yellow)Browse doc at _build/default/_doc/_html/index.html$(color_reset)"
.PHONY: doc

docker:
	docker build -t codex .

.PHONY: binsec_codex.exe frama_c_codex.exe
binsec_codex.exe frama_c_codex.exe: Dockerfile.alpine ## Build static version of BINSEC/Codex && FramaC/Codex.
	docker build -f Dockerfile.alpine -t codexstatic .
	docker run --name my_container --detach codexstatic && docker cp my_container:/home/ocaml/codex/_build/default/binsec/binsec_codex.exe . && docker rm  my_container
	docker run --name my_container --detach codexstatic && docker cp my_container:/home/ocaml/codex/_build/default/frama-c/frama_c_codex.exe . && docker rm  my_container

install-opam:
	opam init --compiler 4.14.1

# We need a patched version of Frama-C. If you have opam,
# just type make opam-install-frama-c-with-cudd.
opam-install-frama-c-with-cudd: ## Install patched version of frama-c for codex
	cd ext/frama_c_with_cudd && make
.PHONY: opam-install-frama-c-with-cudd

.PHONY: dependency_graph.png
dependency_graph.png: ## Generate the library dependency graph
	dune-deps -x ext -x utils --no-ext | tred | dot -Tpng > $@

################ Rules to update headers and check that we did not forgot any file

FIND_EXCLUDE = -not -path './_build/*'
FIND_EXCLUDE += -not -path './ext/frama_c_with_cudd/frama-c*'
FIND_EXCLUDE += -not -path './.git/*'
FIND_EXCLUDE += -not -path './tests/*'
FIND_EXCLUDE += -not -path './frama-c/tests/*'
FIND_EXCLUDE += -not -path './benchmarks/*'

UPDATED_EXTENSIONS = mli ml c h

.PHONY: update_headers
update_headers: ## Update file headers using headache
	echo '$(patsubst %, -o -name "%", $(UPDATED_EXTENSIONS))'
	find . $(FIND_EXCLUDE) -type f \
	\( -name "*.ml" -o -name "*.mli" -o -name "*.mll" -o -name "*.mly" \
	-o -name "*.c" -o -name "*.h" \
	-o -name "dune" -o -name "dune-project" -o -name "Makefile" \
	-o -name "Dockerfile.*" -o -name "*.ini" -o -name "*.el" \) \
	-exec headache -c headers/headache_config.txt -h headers/CEA_LGPL21 {} \;



##### Check if we forgot some file.
EXTRA_EXCLUDE = $(FIND_EXCLUDE)
EXTRA_EXCLUDE += -not -name '*.ml'
EXTRA_EXCLUDE += -not -name '*.mli'
EXTRA_EXCLUDE += -not -name '*.mll'
EXTRA_EXCLUDE += -not -name '*.mly'
EXTRA_EXCLUDE += -not -name '*.c'
EXTRA_EXCLUDE += -not -name '*.h'
EXTRA_EXCLUDE += -not -name 'dune'
EXTRA_EXCLUDE += -not -name 'dune-project'
EXTRA_EXCLUDE += -not -name 'Makefile'
EXTRA_EXCLUDE += -not -name 'Dockerfile.*'
EXTRA_EXCLUDE += -not -name '*.ini'
EXTRA_EXCLUDE += -not -name '*.el'
EXTRA_EXCLUDE += -not -path './*.exp_dump'
EXTRA_EXCLUDE += -not -path './*.dump'
EXTRA_EXCLUDE += -not -path './*.result'
EXTRA_EXCLUDE += -not -path './*.nix'
EXTRA_EXCLUDE += -not -path './*.exe'
EXTRA_EXCLUDE += -not -path './*.orig'
EXTRA_EXCLUDE += -not -path './*.patch'

.PHONY: check_missing_files
check_missing_files: ## Check for files not update by update_headers
	@echo "Generating the list of files whose headers we do not maintain"
	find . -type f $(EXTRA_EXCLUDE) -print

source-distrib:
	CUR=`git log -1 | head -n 1 | cut -d ' ' -f 2`; \
	cd `mktemp -d --suffix codex` && \
	git clone $(PWD) codex-$$CUR && \
	rm -Rf codex-$$CUR/.git && \
	rm -Rf codex-$$CUR/tests && \
	rm -Rf codex-$$CUR/frama-c/tests && \
	tar czf codex-$$CUR.tar.gz codex-$$CUR && \
	echo "Created file "`pwd`"/codex-"$$CUR".tar.gz"

.PHONY: help
help:: ## Show this help
	@echo "$(color_yellow)make codex:$(color_reset) list of useful targets :"
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_blue)%-$(HELP_PADDING)s$(color_reset) %s\n", $$1, $$2}'
