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

module Ctypes = Types.Ctypes

module MakeAddressOnly
    (SubAddress:Memory_sig.Address)
    (_:Memory_sig.Fixed_size_value_domain with module Context = SubAddress.Scalar.Context
                                              and module Scalar = SubAddress.Scalar)
: Memory_sig.Address with module Context = SubAddress.Context and module Scalar = SubAddress.Scalar


module Make
    (Sub:Memory_sig.Memory_domain)
    (_:Memory_sig.Fixed_size_value_domain with module Context = Sub.Address.Context
                                              and module Scalar = Sub.Address.Scalar)
: Memory_sig.Memory_domain with module Address.Context = Sub.Address.Context and module Address.Scalar = Sub.Address.Scalar
