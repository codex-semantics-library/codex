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

(* Translate an address into something which is either an address or a
   numeric value. The numeric values can be used as an address in a
   region handled by Region_numeric_offset, which is separated from
   the domain given as an argument. *)
module Make
    (Sub:Memory_sig.Memory_domain)
    (Address2Scalar: Memory_sig.Address_to_Scalar with module Scalar := Sub.Address.Scalar
                                                   and module Address := Sub.Address):sig     (* Memory_sig.Whole_Memory_domain *)

  module Address:Memory_sig.Fixed_size_value_domain with module Context = Sub.Address.Context
  module Memory
      (Value:Memory_sig.Value with module Context = Address.Context)
      (Lift:Memory_sig.Value_to_address with module Address := Sub.Address and module Value := Value)
    :Memory_sig.Memory
    with module Value = Value
     and module Address = Address
     and type boolean = Sub.Memory(Value)(Lift).boolean
end



