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

module TypedC = Types.TypedC

module MakeAddressOnly
    (SubAddress:Memory_sig.ADDRESS)
    (_:Memory_sig.FIXED_SIZE_VALUE_DOMAIN with module Scalar = SubAddress.Scalar)
: Memory_sig.ADDRESS with module Scalar = SubAddress.Scalar

module Make
    (Sub:Memory_sig.ADDRESS_AND_MAKE_MEMORY)
    (_:Memory_sig.FIXED_SIZE_VALUE_DOMAIN with module Scalar = Sub.Address.Scalar)
  : Memory_sig.ADDRESS_AND_MAKE_MEMORY
    with module Scalar = Sub.Scalar and module Offset = Sub.Offset
