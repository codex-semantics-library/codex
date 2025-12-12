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

(** Common tools to write custom hash-functions for our various types:
    Use {!hash2}, {!hash3}, {!hash4} or {!hash5} to create the hash of a tuple,
    {!hash_list} to create the hash of a list and {!hash_sum} to create the
    hash of a sum type.

    For example, one might write a hash function for the given type as follows
    {@ocaml[
        type t =
            | A
            | B of int
            | C of char * int
            | D of int list
            | E of { x: int; mutable y : int }
                   (* mutable values should NOT be included in hash *)

        let hash x =
            let total = 5 in (* 5 total constructors *)
            match x with
            | A       -> hash_sum ~total ~nb:0 0 (* hash for constant is 0 *)
            | B i     -> hash_sum ~total ~nb:1 i
            | C(c,i)  -> hash_sum ~total ~nb:2 @@ hash2 (int_of_char c) i
            | D l     -> hash_sum ~total ~nb:3 @@ hash_list Fun.id l
            | E {x;_} -> hash_sum ~total ~nb:4 x
    ]}*)

val hash_fast: 'a -> int
(** Fast hash function (for recursive subcalls), lots of collisions,
    only examines constructor tag *)

val hash_sum: nb:int -> total:int -> int -> int
(** [hash_sum ~nb ~total value] is used to generate a unique hash for sum types.

    @param nb is the constructor number (starting at [0])
    @param total is the total number of constructors [nb < total]
    @param value is the hash value of the constructors arguments *)

val hash2: int -> int -> int
(** hash of a pair *)

val hash3: int -> int -> int -> int
(** hash of a triplet *)

val hash4: int -> int -> int -> int -> int
(** hash of a quadruplet *)

val hash5: int -> int -> int -> int -> int ->int
(** hash of a quintuplet *)

val hash_list: ('a -> int) -> 'a list -> int
(** hash a list, using the given function to hash its elements *)
