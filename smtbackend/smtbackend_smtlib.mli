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

(* Direct implementation of the SMTLib interface, where communication
   with the solver is done by sending textual information to a
   sub-process. *)
include module type of Smtbackend_smtlib_sig

module Make_Typed(P:PARAM_S):TYPED_S
module Make_Untyped(P:PARAM_S):UNTYPED_S
module Make_Untyped_Muz(P:PARAM_S):UNTYPED_MUZ


val with_z3: ?executable:string -> ((module UNTYPED_MUZ) -> 'a) -> 'a
