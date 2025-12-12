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

 (**************************************************************************)
(*                                                                        *)
(*  This file is part of Codex.                                           *)
(*                                                                        *)
(*  Copyright (C) 2013-2021                                               *)
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
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)
(* Signature for control flow graphs. *)

(* Each control location represents a program point that we want to be
   "separated".

   This can encompass traces, callstacks, etc.  *)
module type Control_location = sig
  type t
  val hash: t -> int
  val equal: t -> t -> bool
  val pretty: Format.formatter -> t -> unit
  val compare: t -> t -> int
end


(* A node of the control-flow graph. These are mapped 1:1 with control
   locations, but contain more operations (e.g. to iterate on known
   predecessors and successors). *)
module type NODE = sig
  module Control_location:Control_location;;
  type t
  val control_location: t -> Control_location.t
  
  (* Standard operations *)
  val hash: t -> int
  val equal: t -> t -> bool
  val pretty: Format.formatter -> t -> unit

  (* Important: if a visited before b, then a < b.  This is made
     automatically by the fact that CFG nodes are created in order of
     their traversal. *)
  val compare: t -> t -> int      

  (* The predecessors and successors known so far.  *)
  val preds: t -> t list
  val succs: t -> t list  

end

(* The control-flow graph. *)
module type Cfg = sig
  module Control_location:Control_location;;
  module Node:NODE with module Control_location = Control_location;;

  (** Register an edge from a node to a (possibly new) control
     location. Returns the (possibly newly created) node for the
     control location.

     Note: this interface for creating nodes ensures that nodes are
     created in a traversal order. *)
  val register_edge: Node.t -> Control_location.t -> Node.t;;

  (** Returns the node if it was already known, or None otherwise.
     Known is useful when trying to construct the CFG by exploration. 
  *)
  val known: Control_location.t -> Node.t option
                                                       

  
end
