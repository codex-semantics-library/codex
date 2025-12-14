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

all: tailwind4.1.5.css graphviz.umd.js js/bundle-output.js

tailwind4.1.5.css:
	curl -sSLo $@ https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4.1.5 

graphviz.umd.js:
	curl -sSLo $@ https://unpkg.com/@hpcc-js/wasm@2.22.4/dist/graphviz.umd.js

js/bundle-output.js: js/node_modules js/rollup.config.js js/bundle-input.js js/node_modules
	cd js && npx rollup -c


#  ; update this by doing npm update -y in the current dir, then remove node_modules
js/node_modules:   js/package.json
	cd js && npm install
