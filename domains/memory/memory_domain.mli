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
    (Common:Memory_sig.Fixed_size_value_domain)
    (Memory:Memory_sig.Memory
     with module Address.Context = Common.Context
      and module Value.Context = Common.Context
      and type Value.binary = Common.binary
      and type Address.binary = Common.binary
      and type boolean = Common.boolean)
  :sig
    include Domain_sig.Base
      with type binary = Common.binary
      and type boolean = Common.boolean
      and module Context = Memory.Context
end
