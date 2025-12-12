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

(** Given the [while] syntax presented in {!While_ast}, in this module [Cinterpreter]
    provides the concrete interpreter for the AST containing expressions and commands.
    Towards this goal, we utlize {!Var} module to refer to named storage locations
    and then define each concrete state using {!State} as a map from each {!Var.t}
    to an integer [Z.t].

    With concrete states defined, the concrete interpreter {!Interp} module provides
    functions to interpret arithmetic expressions, boolean expressions, and commands.

    This is detailed in the While tutorial in {!page-chapter1}. *)

open While_ast

module State : sig
  type t
  val empty : t
  val find : Var.t -> t -> Z.t
  val add : Var.t -> Z.t -> t -> t
  val iter : (Var.t -> Z.t -> unit) -> t -> unit
  val pp : Format.formatter -> t -> unit
  val of_list: (string*int) list -> t
end
(** The module [State] is a map that maps each variable to a concrete integer [Z.t] *)

val initial_state: unit -> State.t

val interp_aexp : aexp -> State.t -> Z.t
(** The arithmetic‐expression interpreter. It is a straightforward recursive
    traversal of the {{!While_ast.aexp}[aexp]} AST that computes an integer result
    in a given program state. *)

val interp_bexp : bexp ->  State.t -> bool
(** The boolean-expression interpreter is also recursive traversal of the {{!While_ast.bexp}[bexp]}
    boolean expression AST that outputs a boolean value in a given program state. *)

val interp_stmt : stmt -> State.t -> State.t
(** In the While language, each statement represents a state‐transforming operation
    on the program’s state. We capture these transformations in a single recursive
    function, which takes a {{!While_ast.stmt}[stmt]} and an initial state and
    returns the updated state after executing that statement *)
