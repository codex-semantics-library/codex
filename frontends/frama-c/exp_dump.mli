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

module Cil_types = Frama_c_kernel.Cil_types

(* Iterate on all expressions, printing their location and what the
   function f outc (indent,ki,exp) returns, where indent is the
   indendation and (ki,exp) identifies the expression in its
   statement. Only print the expressions for which should_print is true. *)
val exp_dump : should_print:(Cil_types.kinstr * Cil_types.exp -> bool) -> 
  (out_channel -> int * Cil_types.kinstr * Cil_types.exp -> unit) ->
  out_channel -> unit
