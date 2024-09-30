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

type 'a parents

(* Functor to create an efficient data structure for online
   computation of the nearest common ancestor. *)
module Make(Elt : sig
    type t
    val depth : t -> int
    val parents : t -> t parents
  end):
sig
  val nil : int * 'a array      (* XXX: Why nil here? *)
  val cons : Elt.t -> int * Elt.t parents
  val nth : Elt.t -> int -> Elt.t
  val nearest_common_ancestor_same_depth : Elt.t -> Elt.t -> Elt.t
  val nearest_common_ancestor : Elt.t -> Elt.t -> Elt.t
end


