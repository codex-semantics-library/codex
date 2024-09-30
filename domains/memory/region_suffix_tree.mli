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

open Memory_sig

(* Use Region_suffix_tree with Fully_expanded_finite_region. *)
module MakeFullyExpanded(Scalar:Domain_sig.Base):sig
  module Offset:Fully_expanded_finite_region.Enumerable_offset
    with module Context = Scalar.Context
     and module Scalar = Scalar
     and type boolean = Scalar.boolean
  include Offset_Memory_domain with module Offset := Offset
end

(* Use Region_suffix_tree with custom memory regions.
   TODO: Pass the array functor as an argument.  *)
module Make(Scalar:Domain_sig.Base):sig
  module Offset:Fully_expanded_finite_region.Enumerable_offset
    with module Context = Scalar.Context
     and module Scalar = Scalar
     and type boolean = Scalar.boolean

  module Memory
      (Value:Value)
      (_:Value_to_offset with module Value := Value and module Offset := Offset) :Block
    with module Value = Value
     and module Offset = Offset
     and module Context = Value.Context
     and type boolean = Value.boolean
  include Offset_Memory_domain with module Offset := Offset and module Memory := Memory
end
