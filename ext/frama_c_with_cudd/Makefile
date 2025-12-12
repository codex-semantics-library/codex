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

# Those versions are known to work.
# FRAMA_C_VERSION = frama-c-27.1-Cobalt
FRAMA_C_VERSION = frama-c-28.1-Nickel

all: opam-install-frama-c-with-cudd

$(FRAMA_C_VERSION).tar.gz:
	wget https://www.frama-c.com/download/$(FRAMA_C_VERSION).tar.gz

unzip: $(FRAMA_C_VERSION).tar.gz
	rm -Rf $(FRAMA_C_VERSION)
	tar xzf $(FRAMA_C_VERSION).tar.gz

# Create an Ocamlfile with all the machdeps in a here document,
# and a function to return the correct one.
# This is necessary to have a standalone frama_c_codex.exe with no runtime dependency.
$(FRAMA_C_VERSION)/src/kernel_services/ast_queries/internalized_machdeps.ml: unzip
	rm -f $@
	@for yaml_file in $(wildcard $(FRAMA_C_VERSION)/share/machdeps/*.yaml); do \
	  var_name=$$(basename -s .yaml $$yaml_file | tr '-' '_'); \
	  echo "" >> $@; \
	  echo "let $${var_name} = {yaml|" >> $@; \
	  cat $$yaml_file >> $@; \
	  echo "|yaml}" >> $@; \
	done
	@echo "(* Function to retrieve string based on file name *)" >> $@
	@echo "let get_machdep_content file_name =" >> $@
	@echo "(* Kernel.feedback \"Using internalized version of %s\" file_name; *)" >> $@
	@echo "  match String.lowercase_ascii file_name with" >> $@
	@for yaml_file in $(wildcard $(FRAMA_C_VERSION)/share/machdeps/*.yaml); do \
	  file_name=$$(basename -s .yaml $$yaml_file); \
	  var_name=$$(echo $${file_name} | tr '-' '_'); \
	echo "  | \"$${file_name}.yaml\" -> $${var_name}" >> $@; \
	done
	@echo "  | x -> failwith (\"Machdep not internalized: \" ^ x)" >> $@
	@echo "(* List of machdeps. *)" >> $@;
	@echo "let default_machdeps = [" >> $@;
	@for yaml_file in $(wildcard $(FRAMA_C_VERSION)/share/machdeps/*.yaml); do \
	  file_name=$$(basename -s .yaml $$yaml_file); \
	echo "  \"$${file_name}.yaml\";" >> $@; \
	done
	@echo "];;" >> $@;
	@echo "(* End of generated OCaml file *)" >> $@
	@echo "Generation of $@: done"

internalize_machdeps: unzip $(FRAMA_C_VERSION)/src/kernel_services/ast_queries/internalized_machdeps.ml
#	sed -i 's/(Yaml_unix.of_file (Fpath.v (file:>string)))/(Yaml.of_string @@ Internalized_machdeps.get_machdep_content (Filename.basename (file:>string)))/' $(FRAMA_C_VERSION)/src/kernel_services/ast_queries/file.ml
#	sed -i "s#Kernel.Share.get_dir ~mode:\`Must_exist \"machdeps\"#\"machdeps\"#" $(FRAMA_C_VERSION)/src/kernel_services/ast_queries/file.ml

patch:  unzip internalize_machdeps
	cd $(FRAMA_C_VERSION) && patch -Np1 -i ../internalize_machdeps.patch
	cd $(FRAMA_C_VERSION) && patch -Np1 -i ../patch-frama-c.patch
	cd $(FRAMA_C_VERSION) && patch -Np1 -i ../opam-$(FRAMA_C_VERSION).patch
	cd $(FRAMA_C_VERSION) && rm -Rf man src/plugins ivette && mkdir ivette && touch ivette/Makefile.installation
#	cd $(FRAMA_C_VERSION) && rm -Rf bin dev doc headers lib licenses nix releases tests tools

#$(FRAMA_C_VERSION): $(FRAMA_C_VERSION).tar.gz patch
#.PHONY: $(FRAMA_C_VERSION) unzip

build: patch
	opam remove frama-c --skip-updates
#	opam pin remove -n frama-c
	cd $(FRAMA_C_VERSION) && opam exec -- opam pin -y --working-dir add frama-c .
#	cd $(FRAMA_C_VERSION) && opam exec -- opam install frama-c

# We need a patched version of Frama-C. If you have opam,
# just type make opam-install-frama-c-with-cudd.
# opam-install-frama-c-with-cudd: $(FRAMA_C_VERSION).tar.gz patch 
# 	rm -Rf $(FRAMA_C_VERSION)
# 	tar xzf $(FRAMA_C_VERSION).tar.gz
# 	cd $(FRAMA_C_VERSION) && patch -Np1 -i ../patch-frama-c.patch
# 	cd $(FRAMA_C_VERSION) && patch -Np1 -i ../opam.patch
# 	cd $(FRAMA_C_VERSION) && rm -Rf man src/plugins ivette && mkdir ivette && touch ivette/Makefile.installation
# 	cd $(FRAMA_C_VERSION) && opam pin add frama-c .

opam-install-frama-c-with-cudd: build
