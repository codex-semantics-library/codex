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

module StrMap = Codex.Utils.Datatype_sig.StringMap

type cell_typ = Codex.Types.TypedC.typ * Units.In_bytes.t

exception Incomparable_types of string
exception Falsified_predicate of string

module type MEMORY = sig
  type t
  val read_u8 : t -> Virtual_address.t -> Loader_types.u8
  val read_u32 : t -> Virtual_address.t -> Loader_types.u32
  val read_u64 : t -> Virtual_address.t -> Loader_types.u64
end

module Make (Memory : MEMORY) : sig
  val type_heap : symbols:(Z.t StrMap.t) -> Memory.t
    -> Virtual_address.t -> Codex.Types.TypedC.typ -> cell_typ Virtual_address.Htbl.t
end
