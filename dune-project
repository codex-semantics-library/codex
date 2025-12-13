(lang dune 3.14)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file is part of the Codex semantics library.                     ;;
;;                                                                        ;;
;;  Copyright (C) 2013-2025                                               ;;
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
(using mdx 0.4)
(name codex)

;; Code formatting conveys meaning, and the default style takes a lot
;; of vertical space which makes large functions harder to read.
(formatting disabled)

;; Common to all packages.
(maintainers "Matthieu Lemerre <matthieu.lemerre@cea.fr>")
(authors "Matthieu Lemerre" "Julien Simonnet" "Olivier Nicole"
	 "Paul Robert" "Dorian Lesbre" "Iker Canut" "Corentin Gendreau"
	 "Guillaume Girol" "Charles Babu" "Jérôme Faucheux")
(license "LGPL-2.1-or-later")
(bug_reports "https://github.com/codex-semantics-library/codex/issues")
(source (github codex-semantics-library/codex))
(homepage "https://codex.top")

(package
   (name codex)
   (synopsis "The Codex library for building static analysers based on abstract interpretation.")
   (version "1.0-rc4")
   ;; Note: older versions may probably be used.
   (depends
    dune
    (fmt (>= 0.11.0))
    (bheap (>= 2.0.0))
    (camlp-streams (>= 5.0.1))
    (qcheck-core (>= 0.21.2))
    (pacomb (>= 1.3))
    (ppx_inline_test (>= v0.16.0))
    (ppxlib (>= 0.35.0))
    (zarith (>= 1.14))
    (patricia-tree (>= 0.11.0))
    (js_of_ocaml (>= 6.0.0))
    (ppx_deriving (>= 5.2.1))
    ;; base64 should not be a dependency i think
    (base64 (>= 3.5.0))
    cudd
    (odoc :with-doc)
    (sherlodoc :with-doc)
    (mdx :with-test)
    ))

(using dune_site 0.1) ;; For Frama-C

(package
   (name frama_c_codex)
   (synopsis "A modular static analyser of C programs, based on abstract interpretation. The package contains both a standalone frama_c_codex executable, as well as a Frama-C plugin.")
   ;; Note: older versions of frama-c may probably also be used.
   ;; Note: frama-c 28.0 has a bug that impacts production of cdump files,
   ;; so we want to skip it.
   (depends
    dune
    ("frama-c" (= 31.0))
    (codex (>= "1.0-rc4"))
    (fmt (>= 0.11.0))
    (patricia-tree (>= 0.11.0))
    (zarith (>= 1.14))))

(package
   (name binsec_codex)
   (synopsis "A modular static analyser for machine code program based on abstract interpretation.")
   (depends
    dune
    ("binsec" (>= 0.10))
    ("ocamlgraph" (>= 2.2.0)) 
    ("zarith" (>= 1.14)) 
    (codex (>= "1.0-rc4"))
    "ocaml-embed-file"
    "js_of_ocaml-ppx"
    "vdom"
    ))

(generate_opam_files)


;; Local Variables:
;; mode: lisp
;; End:
