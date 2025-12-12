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

(* Lattice from a set. May lead to divergence if the set is infinite. *)
open Lattice_sig;;

module type S = sig
  include Set.S
  include JOIN_SEMI_LATTICE with type t := t
  include WITH_BOTTOM with type t := t
  val is_bottom: t -> bool
  include WITH_INTER with type t := t
  val eq: t -> t -> Quadrivalent_Lattice.t        
  val intersects: t -> t -> bool
end

module Make(E:Datatype_sig.S):S with type elt = E.t    
