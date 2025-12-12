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

(* Abstraction to deal with flexible array members as the concatenation of a fixed size value
 * and an other block
 *)

module Make 
    (Value : Memory_sig.FIXED_SIZE_VALUE_DOMAIN)
    (Sub : Memory_sig.BLOCK
      with module Scalar = Value.Scalar
       and module Value = Value)
  : Memory_sig.BLOCK
    with module Scalar = Value.Scalar
     and module Value = Value

module MakeComplete
    (Sub : Memory_sig.WHOLE_MEMORY_DOMAIN)
    (Block : Memory_sig.BLOCK
     with module Scalar = Sub.Scalar
      and  module Value = Sub.Address)
: Memory_sig.COMPLETE_DOMAIN
  with module Scalar = Sub.Scalar
   and module Value.Context = Sub.Address.Scalar.Context and module Value.Scalar = Sub.Address.Scalar
