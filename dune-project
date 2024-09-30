(lang dune 2.8)
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
(name codex)

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
   (version "1.0-rc1")
   ;; Note: older versions may probably be used.
   (depends
    (bheap (>= "2.0.0"))
    (camlp-streams (>= "5.0.1"))
    (mlcuddidl (>= "3.0.8"))
    (qcheck-core (>= "0.21.2"))
    (pacomb (>= "1.3"))
    (ppx_inline_test (>= "v0.16.0"))
    (zarith (>= "1.13"))
    (patricia-tree (>= "0.10.0"))
    ))

(using dune_site 0.1) ;; For Frama-C

(package
   (name frama_c_codex)
   (synopsis "The Frama-C/Codex plugin for modular static analysis of C.
The package contains both a standalone frama_c_codex executable, as well
as a plugin which works on patched versions of Frama_C.")
   ;; Note: older versions of frama-c may probably also be used.
   ;; Note: frama-c 28.0 has a bug that impacts production of cdump files,
   ;; so we want to skip it.
   (depends
    ("frama-c" (and (>= 26.0) (< 28.0)))
    (codex (>= "1.0-rc1"))
    ))

(package
   (name binsec_codex)
   (synopsis "A BINSEC version built with the BINSEC/Codex plugin.")
   (depends
    ("binsec" (>= 0.7))
    (codex (>= "1.0-rc1"))
    ))

(generate_opam_files)


;; Local Variables:
;; mode: lisp
;; End:
