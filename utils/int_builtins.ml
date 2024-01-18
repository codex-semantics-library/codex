(**************************************************************************)
(*  This file is part of the Codex semantics library.                     *)
(*                                                                        *)
(*  Copyright (C) 2013-2024                                               *)
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

(* Use #use_output "dune ocaml top" to use these functions in a toplevel. *)


(* Note that we directly pass the tagged value here. *)
external log2: int -> (int[@untagged]) =
  "caml_int_builtin_log2_byte" "caml_int_builtin_log2" [@@noalloc]

external log2_untagged: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_log2_byte" "caml_int_builtin_log2_untagged" [@@noalloc]

external highest_bit: int -> (int[@untagged]) =
  "caml_int_builtin_highest_bit_byte" "caml_int_builtin_highest_bit" [@@noalloc]

external highest_bit_untagged: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_highest_bit_byte" "caml_int_builtin_highest_bit_untagged" [@@noalloc]

external ffs: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_ffs_byte" "caml_int_builtin_ffs_untagged" [@@noalloc]

external count_trailing_zeroes: (int [@untagged]) -> (int [@untagged]) =
    "caml_int_builtin_ctz_byte" "caml_int_builtin_ctz_untagged" [@@noalloc]

external popcount: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_popcount_byte" "caml_int_builtin_popcount_untagged" [@@noalloc]
