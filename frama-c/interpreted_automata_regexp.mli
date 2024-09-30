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

open Codex;;
open Frama_c_kernel;;

(* A letter is labelled as (src,edge,dst) *)
module Letter: Fixpoint.Regex.Letter
  with type t = Interpreted_automata.Vertex.t 
                * Interpreted_automata.Edge.t
                * Interpreted_automata.Vertex.t
                    
module Regexp : module type of Fixpoint.Regex.Make(Letter)

(* Given a kernel function, return the regexp leading to its exit
   point (if any); and a map from every node in the automata to a
   regexp of the paths leading to it. *)
val regexp_of_kf : Kernel_function.t -> 
  Regexp.t * Regexp.t Interpreted_automata.Vertex.Map.t
