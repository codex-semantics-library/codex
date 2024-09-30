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



(** In the following, the _untagged variant takes an untagged (i.e.,
    C) int, whereas the normal variant takes an OCaml int. Which one
    to choose depends on the context: if you were previously doing
    integer operations, the compiler probably already compiled the
    untagged int, so using the _untagged version might be slightly
    faster (though probably barely noticeable). If you are reading
    from an argument or a field in a datastructure, your integer is
    tagged, so you should use the normal version.

    Warning: for some corner arguments, the normal and untagged
    version may have different behaviours. In that case, we suffix the
    unsafe variant with _unsafe.

    If in doubt, just use the normal version. *)

(** For non-zero numbers, [log2 x] is the position of the highest bit
    set, i.e. such that [x lsr (log x) = 1]. Numbers are interpreted
    as unsigned (e.g. [(log2 -1) = 62] on 64-bit plateforms, as ints
    are 63-bit in OCaml. *)

(** In this version, [log2 0 = -1].  *)
external log2: int -> (int[@untagged]) =
  "caml_int_builtin_log2_byte" "caml_int_builtin_log2" [@@noalloc]

(** Warning: [log2_untagged_unsafe 0 = 1], which is probably not what you want.
    Furthermore, [log2_untagged_unsafe x = 1 + log2 x] when x is negative.
    log2_untagged_unsafe is thus safe only on strictly positive integer values. *)
external log2_untagged_unsafe: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_log2_byte" "caml_int_builtin_log2_untagged_unsafe" [@@noalloc]


(** [highest_bit x] is the value [y] where the only bit set is the
    most significant bit of x, i.e.
    - [x land y = y]
    - [x land (lnot y) land (lnot y - 1) = 0].
    Furthermore, [highest_bit 0 = 0]. *)
external highest_bit: int -> (int[@untagged]) =
  "caml_int_builtin_highest_bit_byte" "caml_int_builtin_highest_bit" [@@noalloc]

(** Like [highest_bit], but [highest_bit_untagged_unsafe 0] and
    [highest_bit_untagged_unsafe x] when [x < 0] are undefined. *)
external highest_bit_untagged_unsafe: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_highest_bit_byte" "caml_int_builtin_highest_bit_untagged_unsafe" [@@noalloc]



(** [ffs x] returns the position of the first least significant bit
    set, where the least significant bit starts at 1.  [ffs 0] returns
    [0]. *)
external ffs: (int) -> (int[@untagged]) =
  "caml_int_builtin_ffs_byte" "caml_int_builtin_ffs" [@@noalloc]

(** See [ffs]. *)
external ffs_untaggued: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_ffs_byte" "caml_int_builtin_ffs_untagged" [@@noalloc]

(* There is no point in the taggued version, as the implementation
   must untag anyway. *)
let ffs = ffs_untaggued

(** [count_trailing_zeroes x] returns the number of least significant
    bits that are all set to 0. [count_trailing_zeroes 0] returns
    [Sys.word_size]. *)
external count_trailing_zeroes: (int) -> (int [@untagged]) =
    "caml_int_builtin_ctz_byte" "caml_int_builtin_ctz" [@@noalloc]

external count_trailing_zeroes_untagged: (int [@untagged]) -> (int [@untagged]) =
  "caml_int_builtin_ctz_byte" "caml_int_builtin_ctz_untagged" [@@noalloc]

(* There is no point in the taggued version, as the implementation
   must untag anyway. *)
let count_trailing_zeroes = count_trailing_zeroes_untagged

(** [popcount] returns the number of bit set in the machine
    representation of the integer. *)
external popcount: int -> (int[@untagged]) =
  "caml_int_builtin_popcount_byte" "caml_int_builtin_popcount" [@@noalloc]

(** See [popcount]. *)
external popcount_untaggued: (int [@untagged]) -> (int[@untagged]) =
  "caml_int_builtin_popcount_byte" "caml_int_builtin_popcount_untagged" [@@noalloc]


let is_zero_or_a_power_of_2 x = (x land (x - 1) = 0)
let is_a_power_of_2 x = (x != 0) && is_zero_or_a_power_of_2 x
