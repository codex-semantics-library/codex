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
(*
module type Fixed_Value = sig
  module Sub : Memory_sig.Fixed_size_value_domain
  include Memory_sig.Fixed_size_value_domain with module Context = Sub.Context
  val force : size:int -> Context.t -> binary -> Sub.binary
  val make_value : Sub.binary -> binary
end
*)
module MakeAddressOnly(Sub:Memory_sig.Fixed_size_value_domain):
  Memory_sig.Fixed_size_value_domain with module Context = Sub.Context and module Scalar = Sub.Scalar
(* Fixed_Value with module Sub = Sub *)

(*
module MakeFromAddress
  (Sub:Memory_sig.Whole_Memory_domain)
  (Address : Fixed_Value with module Sub = Sub.Address)
: Memory_sig.Whole_Memory_domain with module Address = Address
*)
(* Translate an address into something which is either an address or a
   numeric value. The numeric values can be used as an address in a
   region handled by Region_numeric_offset, which is separated from
   the domain given as an argument. *)
module Make (Sub:Memory_sig.Whole_Memory_domain):sig     (* Memory_sig.Whole_Memory_domain *)
  include Memory_sig.Whole_Memory_domain with module Address.Context = Sub.Address.Context and module Address.Scalar = Sub.Address.Scalar
end
