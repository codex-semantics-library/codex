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

(* A fully expanded and finite region: each access perform a weak
   update or weak read on all the possible addresses. For this, the
   addresses must be enumerables, which mandates finiteness of
   region. *)

open Memory_sig;;

module type Enumerable_offset = sig
  include Offset

  (* Additional queries. *)
  val fold_crop:
    size:int -> Context.t ->
    offset -> inf:Z.t -> sup:Z.t -> (Z.t -> 'a -> 'a) -> 'a -> 'a
  val is_precise:
    size:int -> Context.t -> offset -> Z.t Memory_sig.precision
  val in_bounds:
    size:int -> Context.t -> offset -> inf:Z.t -> sup:Z.t -> bool
end

module Memory
    (Offset:Enumerable_offset)
    (Value:Value)
    (_:sig
       (** Value is higher than offset on the domain stack, so we
          decompose a Value stack in an Offset stack and a context:
          [Lift.ctx value_ctx] = (offset_ctx,f) means that f
          offset_ctx = value_ctx. *)
       val ctx: Value.Context.t -> Offset.Context.t * (Offset.Context.t -> Value.Context.t)
     end)
  :sig
  include Block
    with module Offset = Offset
     and module Value = Value
     and module Context = Value.Context
     and type boolean = Value.boolean
end
