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


(* Note: we use the Common argument because we cannot just say
   Address.Context = Value.Context. *)
module Make
    (Value:Memory_sig.Fixed_size_value_domain)
    (Block:Memory_sig.Block with module Value = Value)
    (Memory:Memory_sig.Memory
     with module Address.Context = Value.Context
     and module Block.Value.Context = Value.Context
     and type Block.Value.binary = Value.binary
     and type Address.binary = Value.binary
     and type boolean = Value.boolean
     and type address = Value.binary
    )
  :sig
    include Memory_sig.Base
      with type binary = Value.binary
      and type boolean = Value.boolean
      and type block = Block.block
      and module Context = Memory.Context
end
