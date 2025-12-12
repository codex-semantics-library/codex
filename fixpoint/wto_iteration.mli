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


module type ABSTRACTDOMAIN = sig

  (* An abstract domain is a lattice... *)
  type t
  val join: t -> t -> t
  val is_included: t -> t -> bool
  val bottom: t

  module ControlLocation:sig
    include Datatype_sig.S
    val to_int: t -> int                  (* Unique integer identifier. *)
    val preds: t -> t list                (* Predecessors to the block. *)
  end

  (* And a transfer function. Given a control location (cl) and an
     abstract state, returns the successor control locations and
     abstract states. *)
  val transfer: ControlLocation.t -> t -> (ControlLocation.t * t) list

  (* Pretty printer used for debugging. *)
  val pp: Format.formatter -> t -> unit

end

module WTOFixpoint(L : ABSTRACTDOMAIN):
sig
  module CLMap: PatriciaTree.MAP with type key = L.ControlLocation.t

  (* Given an initial map with the prestate for some locations
     (typically, the entry point), perform a fixpoint iteration and
     return  a new map. *)
  val fixpoint_partition : L.t CLMap.t -> L.ControlLocation.t Wto.partition -> L.t CLMap.t
end
