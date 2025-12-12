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

(** Evaluation of constant terms. *)

module Make(Terms: Sig.TERMS):sig

  exception Empty
  exception Not_a_constant

  (** Returns the value if the term is indeed a singleton value;
      @raises Empty if it is the empty value;
      @raises Not_a_constant if it is not constant. *)

  val integer: Operator.Function_symbol.integer Terms.t -> Z.t
  val boolean: Operator.Function_symbol.boolean Terms.t -> bool
  val binary: Operator.Function_symbol.binary Terms.t -> Z.t
  val bitvector: Operator.Function_symbol.bitvector Terms.t -> Z.t
  val enum: Operator.Function_symbol.enum Terms.t -> Z.t
end
