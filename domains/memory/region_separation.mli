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

open Memory_sig;;

(* Lifts a memory domain into a memory domain that separates each
   malloc call into a distinct memory region, separated by the others,
   where each memory region is handled by Sub.Memory (and pointers by
   Sub.Address) *)
module Make
    (Sub:Memory_sig.Offset_Memory_domain)
    (Offset2Scalar:Memory_sig.Offset_to_Scalar
     with module Offset := Sub.Offset and module Scalar := Sub.Offset.Scalar)    
  :Memory_sig.Whole_Memory_domain
    with module Address.Context = Sub.Offset.Context
     and module Address.Scalar = Sub.Offset.Scalar
