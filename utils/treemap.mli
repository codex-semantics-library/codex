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


(* This implement a map from keys that are in a tree relationship
   (e.g. a sets of paths) to a lattice.

   It is so that after n calls to "refine ki li", then a map k is
   mapped to the intersection of all the li for which there was a
   refine ki li call, and for which ki is a parent of k.
*)
module type Key =
sig
  type t

  (* Nearest common ancestor between two elements in the tree *)
  val nearest_common_ancestor : t -> t -> t

  (* If a an ancestor of b  *)
  val is_prefix : t -> t -> bool
  val equal : t -> t -> bool
  val pretty : Format.formatter -> t -> unit
end


(* More efficient veresion, but without empty. *)
module Make_no_empty(Key : Key):
sig
  type 'a t
  val refine : Key.t -> inter:('a -> 'a -> 'a) -> join:('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
  val find : Key.t -> 'a t -> 'a
  val make_pretty: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end


module Make(Key : Key):
sig
  type 'a t
  val refine : Key.t -> inter:('a -> 'a -> 'a) -> join:('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
  val find : Key.t -> 'a t -> 'a
  val empty : 'a t
  val make_pretty: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
