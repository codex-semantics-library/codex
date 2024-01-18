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

module type Base_with_types = sig
  include Domain_sig.Base
  (* module Type : Type_domain.S
    with module BR.Scalar.Context = Context *)

  val global_symbol : Context.t -> string -> int * binary
  val add_global_symbol : size:int -> Context.t -> string -> binary -> unit
  val flush_cache : Context.t -> memory -> memory
  val has_type : size:int -> Context.t -> Types.Ctypes.typ -> binary -> bool
end

module type S = sig
  include Domain_sig.Base
end

module type S_with_types = sig
  include Base_with_types
end

module Make (D : Domain_sig.Base)  : S
  with module Context = D.Context
  and type boolean = D.boolean

module Make_with_types (D : Base_with_types) : S_with_types
  with module Context = D.Context
  and type boolean = D.boolean
  (* and module Type = D.Type *)
