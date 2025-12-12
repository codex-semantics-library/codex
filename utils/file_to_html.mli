(**************************************************************************)
(*  This file is part of the Codex semantics library.                     *)
(*                                                                        *)
(*  Copyright (C) 2013-2025                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

(** This file allows to put binary files in HTML documents (by base64
    encoding), so that the binary file is accessible by javascript as
    a Uint8Array. Can be used e.g. for Marshalling OCaml data to the
    browser, which simplifies UI development (compared to using a
    webserver, with asynchronous requests following some protocols
    etc.)

    Warning:
    - Chrome does not support files > 2GB (Firefox works fine)

    - Performance is OK, but loading a 2GB file requires ~30s on my
      machine, i.e. we can load 67MB per second. The benefit of binary
      files is that is is compact and make it easy to share data... *)


(** Generate HTML <script>...<script> node in [outc] such that the
    content of [filename] will be placed in the Javascript variable
    [varid], which is a Uint8Array.

    Note: this require [preamble] (which is also a <script>...<script>) to be placed before. *)
val file_to_html : out_channel -> filename:string -> varid:string -> unit


(** A preamble string to be placed in the HTML file before the
    sequence of [file_to_html] calls start. *)
val preamble : string
