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

(* Most of this code is adapated from the OCaml Standard library's implementation
   of Hashtbl.

   Copyright 1996 Institut National de Recherche en Informatique et en Automatique.
   distributed under the terms of the GNU Lesser General Public License
   version 2.1, with the  special exception on linking described in the license *)

module type HETEROGENEOUS_HASHED_TYPE = sig
  type 'a t
  val polyeq: 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp
  val hash: 'a t -> int
end

module type HETEROGENEOUS_SEEDED_HASHED_TYPE = sig
  type 'a t
  val polyeq: 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp
  val hash: int -> 'a t -> int
end

val is_randomized: unit -> bool

module type S = sig
  type 'a t
  type 'key key
  type ('key, 'a) value

  type 'b key_value = KeyValue: 'a key * ('a, 'b) value -> 'b key_value

  val create: ?random:bool -> int -> 'a t
  val clear: 'a t -> unit
  val reset: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> 'key key -> ('key, 'a) value -> unit
  val remove: 'a t -> 'b key -> unit
  val find: 'a t -> 'key key -> ('key, 'a) value
  val find_opt: 'a t -> 'key key -> ('key, 'a) value option
  val find_all: 'a t -> 'key key -> ('key, 'a) value list
  val replace: 'a t -> 'key key -> ('key, 'a) value -> unit
  val mem: 'a t -> 'key key -> bool
  val add_seq: 'a t -> 'a key_value Seq.t -> unit
  val replace_seq: 'a t -> 'a key_value Seq.t -> unit
  val of_seq: 'a key_value Seq.t -> 'a t

  type 'a polyiter = { f: 'key. 'key key -> ('key, 'a) value -> unit; } [@@unboxed]
  val iter: 'a polyiter -> 'a t -> unit

  type ('a, 'b) polyfiltermap = { f: 'key. 'key key -> ('key, 'a) value -> ('key, 'b) value option; } [@@unboxed]
  val filter_map_inplace: ('a, 'a) polyfiltermap -> 'a t -> unit

  type ('a, 'acc) polyfold = { f: 'key. 'key key -> ('key, 'a) value -> 'acc -> 'acc; } [@@unboxed]
  val fold: ('a, 'acc) polyfold -> 'a t -> 'acc -> 'acc

  val length: 'a t -> int
  val stats: 'a t -> Hashtbl.statistics
  val to_seq: 'a t -> unit -> 'a key_value Seq.node
end

module MakeSeeded
    (Key: HETEROGENEOUS_SEEDED_HASHED_TYPE)
    (Value: PatriciaTree.HETEROGENEOUS_VALUE):
S with type 'key key = 'key Key.t and type ('key, 'a) value = ('key, 'a) Value.t

module Make
    (Key: HETEROGENEOUS_HASHED_TYPE)
    (Value: PatriciaTree.HETEROGENEOUS_VALUE):
S with type 'key key = 'key Key.t and type ('key, 'a) value = ('key, 'a) Value.t
