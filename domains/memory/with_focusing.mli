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

module type Base_with_types = sig
  include Memory_sig.Base

  val flush_cache : Context.t -> memory -> memory
end

module type S = sig
  include Memory_sig.Base
end

module type S_with_types = sig
  include Base_with_types
  
  val analyze_summary : Context.t -> Ctypes.typ -> (int * binary) list -> memory -> bool * (int * binary) option * memory
  val serialize_memory_and_cache : 'a. Context.t -> memory -> Context.t -> memory -> (binary * binary) list -> 'a Context.in_acc -> (memory, 'a) Context.result 
end

module Make (D : Memory_sig.Base)  : S
  with module Context = D.Context
  and type boolean = D.boolean

module Make_with_types (D : Base_with_types) : S_with_types
  with module Context = D.Context
  and type boolean = D.boolean
  (* and module Type = D.Type *)
