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

test.opt:
	cd frama-c && make test.opt

dune:
	dune build

binsec:
	dune build binsec/binsec_codex.exe

.PHONY: binsec

docker:
	docker build -t codex .

# Static version of BINSEC/Codex && FramaC/Codex.
binsec_codex.exe frama_c_codex.exe: Dockerfile.alpine
	docker build -f Dockerfile.alpine -t codexstatic .
	docker run --name my_container --detach codexstatic && docker cp my_container:/home/ocaml/codex/_build/default/binsec/binsec_codex.exe . && docker rm  my_container
	docker run --name my_container --detach codexstatic && docker cp my_container:/home/ocaml/codex/_build/default/frama-c/frama_c_codex.exe . && docker rm  my_container


install-opam:
	opam init --compiler 4.14.1

# We need a patched version of Frama-C. If you have opam,
# just type make opam-install-frama-c-with-cudd.
opam-install-frama-c-with-cudd:
	cd ext/frama_c_with_cudd && make

dependency_graph.png:
	dune-deps -x ext -x utils --no-ext | tred | dot -Tpng > $@

################ Rules to update headers and check that we did not forgot any file

FIND_EXCLUDE = -not -path './_build/*'
FIND_EXCLUDE += -not -path './ext/frama_c_with_cudd/frama-c*'
FIND_EXCLUDE += -not -path './.git/*'
FIND_EXCLUDE += -not -path './tests/*'
FIND_EXCLUDE += -not -path './frama-c/tests/*'
FIND_EXCLUDE += -not -path './benchmarks/*'

UPDATED_EXTENSIONS = mli ml c h

update_headers:
	echo '$(patsubst %, -o -name "%", $(UPDATED_EXTENSIONS))'
	find . $(FIND_EXCLUDE) -type f \
	\( -name "*.ml" -o -name "*.mli" -o -name "*.mll" -o -name "*.mly" \
	-o -name "*.c" -o -name "*.h" \
	-o -name "dune" -o -name "dune-project" -o -name "Makefile" \
	-o -name "Dockerfile.*" -o -name "*.ini"  \) \
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
EXTRA_EXCLUDE += -not -path './*.exp_dump'
EXTRA_EXCLUDE += -not -path './*.dump'
EXTRA_EXCLUDE += -not -path './*.result'
EXTRA_EXCLUDE += -not -path './*.nix'
EXTRA_EXCLUDE += -not -path './*.exe'
EXTRA_EXCLUDE += -not -path './*.orig'
EXTRA_EXCLUDE += -not -path './*.patch'


check_missing_files:
	@echo "Generating the list of files whose headers we do not maintain"
	find . -type f $(EXTRA_EXCLUDE) -print 
