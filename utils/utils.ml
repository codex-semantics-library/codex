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

(* Source: bit twiddling hacks https://graphics.stanford.edu/~seander/bithacks.html *)
let log2_obvious v =
  let rec loop v r =
    if v == 0 then r
    else loop (v lsr 1) (r+1)
  in loop v 0
;;

let log2 v =
  assert (v land (lnot 0x0FFFFFFF) == 0); (* Rejects numbers too big *)
  assert (v != 0);
  let (v,r) = (v,0) in
  let (v,r) = if 0 != v land 0x0FFF0000 then (v lsr 16), r lor 16 else (v,r) in
  let (v,r) = if 0 != v land 0xFF00 then (v lsr 8), r lor 8 else (v,r) in
  let (v,r) = if 0 != v land 0xF0 then (v lsr 4), r lor 4 else (v,r) in
  let (v,r) = if 0 != v land 0xC then (v lsr 2), r lor 2 else (v,r) in
  let r = if 0 != v land 0x2 then r lor 1 else r in
  r
;;

(* Zarith probably makes use of special processor instrucions for this. *)
(* let log2 v = Z.log2 (Z.of_int v) *)

let log2_remainder v =
  let q = log2 v in
  (q, v - (1 lsl q))
;;


let is_power_of_2 v = ((v land (v-1)) == 0) && (v != 0);;

let round_up_next_power_of_2 v =
  (* Works for small integers.  *)
  assert(v < (1 lsl 30));
  let v = v - 1 in
  let v = v lor (v lsr 1) in
  let v = v lor (v lsr 2) in
  let v = v lor (v lsr 4) in
  let v = v lor (v lsr 8) in
  let v = v lor (v lsr 16) in
  v + 1
;;
