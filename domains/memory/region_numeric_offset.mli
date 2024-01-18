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

(* Lifts a Scalar domain into a Memory_domain, which directly uses the
   Scalar as offsets in a finite, contiguous memory region
   (implemented using Fully_expanded_finite_region) *)
open Memory_sig


module Make(Scalar:Domain_sig.Base):sig

  module Offset: sig
    include Memory_sig.Fixed_size_value_domain
      with module Context = Scalar.Context
       and type binary = Scalar.binary
       and type boolean = Scalar.boolean
       and module Context = Scalar.Context
       and module Scalar = Scalar
    include Fully_expanded_finite_region.Enumerable_offset
      with module Context := Context
       and module Scalar := Scalar
       and type offset = Scalar.binary
       and type boolean := boolean
  end

  (* module Memory *)
  (*     (Value:Value) *)
  (*     (Lift:Value_to_offset_contexts with module Value := Value and module Offset := Offset) :Region *)
  (*   with module Value = Value *)
  (*    and module Offset = Offset *)
  (*    and module Context = Value.Context *)
  (*    and type boolean = Value.boolean *)

  (* Offset_Memory_domain with additional features. *)
  include Offset_Memory_domain with module Offset := Offset (* and module Memory:= Memory *)

end
