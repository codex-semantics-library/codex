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

(* A fully expanded and finite region: each access perform a weak
   update or weak read on all the possible addresses. For this, the
   addresses must be enumerables, which mandates finiteness of
   region. *)

open Memory_sig
open Units

module type Enumerable_offset = sig
  include OFFSET
  (* Additional queries. *)
  val fold_crop:
    size:In_bits.t -> Scalar.Context.t ->
    offset -> inf:Z.t -> sup:Z.t -> (Z.t -> 'a -> 'a) -> 'a -> 'a
  val is_precise:
    size:In_bits.t -> Scalar.Context.t -> offset -> Z.t Memory_sig.precision
  val in_bounds:
    size:In_bits.t -> Scalar.Context.t -> offset -> inf:Z.t -> sup:Z.t -> bool
end

module Make_Block
    (Offset:Enumerable_offset)
    (Value:VALUE with module Scalar = Offset.Scalar)
  :sig
    include BLOCK
      with module Scalar = Offset.Scalar
       and  module Offset = Offset
       and module Value = Value
       and module Context = Value.Context
       and type boolean = Value.boolean
end
