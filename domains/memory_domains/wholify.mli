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

module MakeAddressOnly (SubAddress : Memory_sig.ADDRESS) :
  Memory_sig.FIXED_SIZE_VALUE_DOMAIN
  with module Scalar = SubAddress.Scalar

(* Translate an address into something which is either an address or a
   numeric value. The numeric values can be used as an address in a
   region handled by Region_numeric_offset, which is separated from
   the domain given as an argument. *)
module Make (Sub : Memory_sig.ADDRESS_AND_MAKE_MEMORY) : sig
  include
    Memory_sig.WHOLE_MEMORY_DOMAIN
    with module Scalar = Sub.Scalar
end
