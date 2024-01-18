(lang dune 2.2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file is part of the Codex semantics library.                     ;;
;;                                                                        ;;
;;  Copyright (C) 2013-2024                                               ;;
;;    CEA (Commissariat à l'énergie atomique et aux énergies              ;;
;;         alternatives)                                                  ;;
;;                                                                        ;;
;;  you can redistribute it and/or modify it under the terms of the GNU   ;;
;;  Lesser General Public License as published by the Free Software       ;;
;;  Foundation, version 2.1.                                              ;;
;;                                                                        ;;
;;  It is distributed in the hope that it will be useful,                 ;;
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;;
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;;
;;  GNU Lesser General Public License for more details.                   ;;
;;                                                                        ;;
;;  See the GNU Lesser General Public License version 2.1                 ;;
;;  for more details (enclosed in the file LICENSE).                      ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(implicit_transitive_deps false)
(using menhir 2.1)

;; Common to all packages.
(maintainers "Matthieu Lemerre <matthieu.lemerre@cea.fr>")
(authors "Matthieu Lemerre" "Julien Simonnet" "Olivier Nicole"
	 "Dorian Lesbre" "Iker Canut" "Corentin Gendreau"
	 "Guillaume Girol")
(license "LGPL-2.1")
(bug_reports "https://github.com/codex-semantics-library/codex/issues")


(package
   (name codex)
   (synopsis "The Codex library for static analysis")
   ;; Note: older versions may probably be used.
   (depends
    (bheap (>= "2.0.0"))
    (camlp-streams (>= "5.0.1"))
    (mlcuddidl (>= "3.0.7"))
    (qcheck-core (>= "0.21.2"))
    (ppx_inline_test (>= "v0.16.0"))
    (zarith (>= "1.13"))    
))

(generate_opam_files)


;; Local Variables:
;; mode: lisp
;; End:
