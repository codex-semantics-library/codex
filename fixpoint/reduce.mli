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

(** Interface for directed graphs with labeled vertices. The reduction
 * algorithm reduces graphs into a regex over the alphabet of edge values (be
 * it edge labels, or pairs of vertices if the edges are unlabeled).  The input
 * graph must be without self-loops, i.e. edges of the form (n,n). 
   All the nodes should also be reachable from the initial node. *)
module type GRAPHI = sig
  type t
    (** CFG type *)

  module V : sig
    type t
    (** Vertex labels *)    
      
    val equal : t -> t -> bool
    val hash : t -> int
    val pretty : Format.formatter -> t -> unit
  end

  module E : sig
    type t
    val src : t -> V.t
    val dst : t -> V.t
  end

  (** Fold over the incoming edges of a node *)
  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module Make (G : GRAPHI) (R : Regex.S with type letter = G.E.t) : sig
  type state = (G.V.t, R.t) Hashtbl.t

  val compute_exprs : G.t -> G.V.t Wto.partition -> state
end
