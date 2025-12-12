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

(** This file handles translation and desugaring between the types
    concrete syntax (defined in {!Type_parser}) to the abstract syntax
    (defined in {!TypedC}). *)


(** Parse a file and fills the definitions using the TypedC imperative
    interface. *)
val parse_file : infer_spec:bool -> string -> unit


(** Parses a string as a Ctype.  *)
val type_of_string : string -> TypedC.typ

(** Initialize the library with base type, which require setting up a
    data model, i.e. the size of int, long, pointers etc in bytes.
    See
    https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models.

    Must be call before we can parse types containing sizes that
    depend on a data model, like short/int/long/long long and pointers. *)
val init:  data_model:[< `ILP32 | `LLP64 | `LP64 ] -> unit
