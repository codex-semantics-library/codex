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


open Codex.Fixpoint.Region_analysis
open Frama_c_kernel

(* Helper function to make region analysis on Frama-C stmts. Produces
   a Node suitable as an argument to the [Region_analysis.Make]
   functor.*)
module MakeNode(M:sig
  val kf: Kernel_function.t

  open Cil_types
  type abstract_value
  val compile_node: stmt -> abstract_value -> (stmt edge * abstract_value) list
  val mu: (abstract_value -> abstract_value) -> abstract_value -> abstract_value
  val join: abstract_value list -> abstract_value

end):Node with type abstract_value = M.abstract_value
          and type node = Cil_types.stmt
