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

(** Translation of the constraints to an SMT problem, and resolution. *)

module MakeFirstOrder
    (T: Sig.TERMS)
    (S: Smtbackend.Smtlib_sig.UNTYPED_S):sig
  val translate:
    Operator.Function_symbol.boolean T.t ->
    Smtbackend.Smtlib_sig.sat
end

module MakeHorn
    (T: Sig.TERMS)
    (S:Smtbackend.Smtlib_sig.UNTYPED_MUZ):sig
  val translate:
    Operator.Function_symbol.boolean T.t ->
    Smtbackend.Smtlib_sig.sat
end
