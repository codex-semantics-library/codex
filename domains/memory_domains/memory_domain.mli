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

module Make
  (Value: Memory_sig.FIXED_SIZE_VALUE_DOMAIN)
  (Block: Memory_sig.BLOCK
   with module Scalar = Value.Scalar
    and module Value = Value
  )
  (Memory:Memory_sig.MEMORY
   with module Scalar = Value.Scalar
    and module Address := Value
    and module Block := Block
  )
  :sig
    include Memory_sig.Base
      with type binary = Value.binary
      and type boolean = Value.boolean
      and type enum = Value.enum
      and type block = Block.block
      and type offset = Block.offset
      and module Context = Memory.Scalar.Context
  end
