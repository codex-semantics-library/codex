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

type pretty_terms =
  | Value (** Only print numeric constraint value, default option *)
  | Symbolic (** Print associated symbolic term with each variable *)
  | Both (** Print both symbolic term and value *)
  | Relation (** Same as above, but also print labeled union-find relations *)

val set_pretty_terms: pretty_terms -> unit
(** Set display mode. Default is {!Value}. Should be called before {!Make}. *)

module Make
    (Terms: Terms.Sig.TERMS)
    (_: Term_based_sig.Domain_S with module Terms = Terms)
  : Sig.BASE_WITH_INTEGER
    with type binary = Operator.Function_symbol.binary Terms.t
     and type boolean = Operator.Function_symbol.boolean Terms.t
     and type enum = Operator.Function_symbol.enum Terms.t
