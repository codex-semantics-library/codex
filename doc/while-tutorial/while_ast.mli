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

(** Abstract syntax for the while language *)

(** For referring to named storage locations, we use the {!module-Var} module which
    ensures that each variable of type {{!Var.t}[t]} is a record with name and a
    unique internal identifier to prevent any clashes. *)
module Var : sig
    type t = {name : string; id : int}
    (** Here variables with equal names are differentiated using integers through [id] *)

    val of_string :  string -> t
    val to_int : t -> int
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
end

type aexp =
  | Int of int          (** integer constant *)
  | Var of Var.t        (** variable *)
  | Add of aexp * aexp  (** addition *)
  | Sub of aexp * aexp  (** subtraction *)
  | Mul of aexp * aexp  (** multiplication *)
(** The [aexp] type represents integer-valued expressions in the While language.
    It includes literal constants and has three basic binary operations: addition, subtraction, and multiplication.
    Each constructor takes one or two sub-expressions of type [aexp], allowing
    arbitrarily nested arithmetic trees (for example, [(x + 2) * (y - 1)]). *)

type bexp =
  | True
  | False
  | Eq of aexp * aexp   (** equality *)
  | Le of aexp * aexp   (** less than or equal *)
  | Gt of aexp * aexp   (** striclty greater than *)
  | Not of bexp         (** negation *)
  | And of bexp * bexp  (** conjunction *)
(** The [bexp] boolean expression captures logical predicates used in conditionals
    and loops. It provides the Boolean constants ([True], [False]),
    comparison operators over arithmetic expressions -- equality ([Eq]),
    less-than-or-equal ([Le]), and greater-than ([Ge]) -- as well as logical
    negation ([Not]) and conjunction ([And]).

    With nesting, one can form complex expressions such as [(v <= 5 && !(v == 0))]. *)



type stmt =
  | Skip                      (* do nothing *)
  | Assign of Var.t * aexp    (* assignment *)
  | Seq of stmt * stmt        (* sequence of statements *)
  | If of bexp * stmt * stmt  (* conditional *)
  | While of bexp * stmt      (* while loop *)
(** Finally, the [stmt] type defines the statements of the While language.
    [Skip] is a no operation; [Assign] updates a variable from an arithmetic expression;
    [Seq] composes two commands sequentially; [If] chooses one of two branches based on a boolean test;
    and [While] repeatedly executes its body while a condition holds. *)

val pp_aexp: Format.formatter -> aexp -> unit (* doesn't handle parenthese *)
val pp_bexp: Format.formatter -> bexp -> unit
val pp_stmt: Format.formatter -> stmt -> unit (* not in depth *)
