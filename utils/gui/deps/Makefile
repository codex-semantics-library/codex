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


EXTRA_SOURCE_FILES = tailwind4.1.5.css graphviz.umd.js js/bundle-output.js

all: $(EXTRA_SOURCE_FILES) ../../../codex.opam.template

tailwind4.1.5.css:
	curl -sSLo $@ https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4.1.5 

graphviz.umd.js:
	curl -sSLo $@ https://unpkg.com/@hpcc-js/wasm@2.22.4/dist/graphviz.umd.js

js/bundle-output.js: js/node_modules js/rollup.config.js js/bundle-input.js js/node_modules
	cd js && npx rollup -c


#  ; update this by doing npm update -y in the current dir, then remove node_modules
js/node_modules:   js/package.json
	cd js && npm install

../../../codex.opam.template: $(EXTRA_SOURCE_FILES)
	@echo "" > $@
	@for file in $(EXTRA_SOURCE_FILES); do \
		SHA256=$$(sha256sum $$file | cut -d' ' -f1); \
		MD5=$$(md5sum $$file | cut -d' ' -f1); \
		echo "extra-source \"$$file\" {" >> $@; \
		echo "  src: \"https://github.com/codex-semantics-library/codex/releases/download/%{version}%/$$(basename $$file)\"" >> $@; \
		echo "  checksum: [" >> $@; \
		echo "    \"sha256=$$SHA256\"" >> $@; \
		echo "    \"md5=$$MD5\"" >> $@; \
		echo "  ]" >> $@; \
		echo "}" >> $@; \
		echo "" >> $@; \
	done
