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

FRAMA_C_CODEX=frama_c_codex.exe
# FRAMA_C = dune exec frama-c -- -machdep x86_32 -codex
BINSEC_CODEX=binsec_codex.exe
# BINSEC_CODEX=dune exec binsec_codex -- -codex


abs.cdump abs.c.html:
	$(FRAMA_C_CODEX) abs.c -codex-exp-dump abs.cdump -codex-html-dump abs.c.html -main abs

abs.exe:
	clang -m32 -O1 abs.c -o abs.exe

abs.bdump abs.exe.html:
	$(BINSEC_CODEX) abs.exe -entrypoint abs -codex-type-file abs.types -codex-output-html abs.exe.html
