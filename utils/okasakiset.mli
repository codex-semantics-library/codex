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

(* A replacement for standard sets using Okasaki data structure, which
   should be faster when sets have a lot of subsets in common
   (Okasakimaps are more stable, and we use physical comparison when
   doing union).

   There are some issues though; notably we can't do map (without
   creating a new set) *)


module Make(Elt:sig
    type t

    (** Each element must be uniquely identified by an integer. *)      
    val unique_id : t -> int
  end):
sig
  type elt = Elt.t
  type t
  (* This should be a subset of Set.S. *)
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val disjoint: t -> t -> bool
  val cardinal: t -> int
  (* These iterate in ascending element order ONLY IF all ids are positive *)
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val min_elt: t -> elt
  val max_elt: t -> elt
  val to_list: t -> elt list

  (* Functions not in Set.S *)
  val is_singleton: t -> elt option
  (* pp_sep defaults to [Format.pp_print_cut] *)
  val pp: 
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> elt -> unit) ->
    Format.formatter -> t -> unit
end

