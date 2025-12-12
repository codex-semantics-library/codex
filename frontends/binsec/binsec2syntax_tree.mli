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

(** This file provides the decomposition from specific Dba.Expr.t to
    the language-agnostic Syntax_tree.t, and checks that the index
    used to identify expressions are correct (internally, using a
    single function as the source of truth for this decomposition). *)


val expr_of_binsec_expr : Dba.Expr.t -> Syntax_tree.expr
val instr_of_binsec_instr : Dba.Instr.t -> Syntax_tree.expr

(** [check_instr_idx (instr,locid) ~idx e] checks that the index [idx]
    that we use to identify an expression is consistent, in that the
    named expression corresponds to the one obtained by pattern
    matching. *)
val check_instr_idx: Dba.Instr.t Syntax_tree.located -> idx:int -> Dba.Expr.t -> Dba.Expr.t Syntax_tree.located

(** [check_expr_idx]: see [check_instr_idx]. *)
val check_expr_idx: Dba.Expr.t Syntax_tree.located -> idx:int -> Dba.Expr.t -> Dba.Expr.t Syntax_tree.located
