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

(** A kind of skiplist (average constant-time push), log(n) access to
   the ith element (and faster for the latest indices).

   Also containes operations when a tree is represented as a set of
   lists from leaves to root, for testing if one is a prefix, or
   finding the nearest common ancestor with fast complexity. *)
type 'a t

(** Length of a list. Constant-time. *)
val length: 'a t -> int
val nil : 'a t
val is_nil: 'a t -> bool
val cons : 'a -> 'a t -> 'a t
val head : 'a t -> 'a
val tail : 'a t -> 'a t
val nth : int -> 'a t -> 'a
val nearest_common_ancestor : 'a t -> 'a t -> 'a t
