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

include Makefile.common

.PHONY: help
help:: ## Show this help
	$(call PRINT_HELP,)
	@make -C frontends/frama-c help
	@make -C frontends/binsec help


################ Stage 1: get dependencies. ################


.PHONY: deps
deps:	## Download and build all the non-ocaml dependencies
# including for the submodules if they are present.
	make -C utils/gui/deps
ifneq ($(wildcard utils/cudd.ml/.git),)
	make -C utils/cudd.ml deps
endif

################ Stage 2: build. ################

build: ## Build the codex library
	dune build
.PHONY: build

frontends: frontends/frama-c frontends/binsec ## Builds every frontend

frontends/frama-c: ## Builds frama_c_codex.exe and the Codex plugin for Frama-C.
	make -C frontends/frama-c build

frontends/binsec: ## Builds the Codex plugin for Binsec.
	make -C frontends/binsec build

.PHONY: frontends/frama-c frontends/binsec frontends

clean: ## Remove build files and executable
	dune clean
.PHONY: clean

doc: ## Build the documentation (accessible at _build/default/_doc/_html)
	dune build @doc
#	dune build --display short @doc-private
	@echo "$(color_yellow)Browse doc at _build/default/_doc/_html/index.html$(color_reset)"
.PHONY: doc

################ Stage 3: tests and benchmarks ################

unit_tests:  ## Unit tests (run with dune test)
	dune test



benchmarks/types-benchmarks:
	git submodule update --init -- benchmarks/types-benchmarks
.PHONY: benchmarks/types-benchmarks

c_types_Olden_%: benchmarks/types-benchmarks
	cd benchmarks/types-benchmarks && make c_Olden/$*
c_types_%: benchmarks/types-benchmarks
	cd benchmarks/types-benchmarks && make c_$*
c_types: $(addprefix c_types_, contiki_list linux_rbtree vmcai2022 \
	   $(addprefix Olden_, bh bisort em3d health mst perimeter \
			 	power treeadd tsp voronoi))
binsec_vmcai2022: benchmarks/types-benchmarks
	cd tests/vmcai2022/liSemanticDirected2017 && make regression

c_small_types:  ## CI target for 'tests:c:small-types'
	cd tests/types && make c_analysis


alltests: unit_tests c_types binsec_vmcai2022 c_small_types  ## Run most of the CI's jobs


simplebin:
	cd tests/vmcai2022/liSemanticDirected2017 && dune exec binsec_codex -- -codex -codex-debug-level 0 kennedy-O0.exe -entrypoint merge -codex-type-file kennedy_types.c -codex-output-html out.html


test.opt:  ## Build and run frama-c/codex on root/test.c
	cd frontends/frama-c && make test.opt
.PHONY: test.opt

frama_c_codex:
	dune build frama-c/frama_c_codex.exe
	dune build -p codex,frama_c_codex
.PHONY: frama_c_codex

frama_c_codex_plugin:
	dune build frama-c/frama_c_codex.exe
	dune build -p codex,frama_c_codex,frama_c_codex_plugin
.PHONY: frama_c_codex_plugin


docker:
	docker build -t codex .

.PHONY: binsec_codex.exe frama_c_codex.exe
binsec_codex.exe frama_c_codex.exe: Dockerfile.alpine ## Build static version of BINSEC/Codex && FramaC/Codex.
	docker build -f Dockerfile.alpine -t codexstatic .
	docker run --name my_container --detach codexstatic && docker cp my_container:/home/ocaml/codex/_build/default/binsec/binsec_codex.exe . && docker rm  my_container
	docker run --name my_container --detach codexstatic && docker cp my_container:/home/ocaml/codex/_build/default/frama-c/frama_c_codex.exe . && docker rm  my_container

install-opam:
	opam init --compiler 4.14.1

# Install patched version of frama-c for codex
install-frama-c:
	cd ext/frama_c_with_cudd && make
.PHONY: install-frama-c

.PHONY: dependency_graph.png
dependency_graph.png: ## Generate the library dependency graph
	dune-deps -x ext -x utils --no-ext | tred | dot -Tpng > $@

################ Rules to update headers and check that we did not forgot any file

FIND_EXCLUDE = -not -path './_build/*'
FIND_EXCLUDE += -not -path './ext/frama_c_with_cudd/frama-c*'
FIND_EXCLUDE += -not -path './.git/*'
FIND_EXCLUDE += -not -path './tests/*'
FIND_EXCLUDE += -not -path './frontends/frama-c/tests/*'
FIND_EXCLUDE += -not -path './benchmarks/*'
FIND_EXCLUDE += -not -path './doc/types-tutorial/*.c'
FIND_EXCLUDE += -not -path './doc/getting-started/test.c'

UPDATED_EXTENSIONS = *.mli *.ml *.mll *.mly *.c *.h dune dune-project Dockerfile* *.ini *.el

.PHONY: update_headers
update_headers: ## Update file headers using headache
	which headache && echo $(patsubst %, -o -name "%", $(UPDATED_EXTENSIONS))
	find . $(FIND_EXCLUDE) -type f \
	\( -name Makefile $(patsubst %, -o -name "%", $(UPDATED_EXTENSIONS)) \) \
	-exec headache -c devenv/headers/headache_config.txt -h devenv/headers/CEA_LGPL21 {} \;
	find utils/patricia-tree -type f \
	$(patsubst %, -not -path 'utils/patricia-tree/%', .git* *.md *.mld LICENSE *.opam) \
	-exec headache -c devenv/headers/headache_config.txt -h devenv/headers/CEA_LGPL21_patricia-tree {} \;





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
update_headers_missing_files: ## Check for files not update by update_headers
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
