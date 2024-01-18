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

(** The CFG is created from the instruction graph like so: the vertices are the
 * basic blocks of the instruction graph, and every edge is labelled with the
 * DBA instructions contained in their destination basic block, plus an
 * expression assumed to be zero or non-zero if the source basic block ended
 * with a conditional jump. *)

module V_hashable = struct
  type call_stack = Virtual_address.t list

  (** CFG vertex type: a basic block, characterized by a leader address and a
   * call stack. *)
  type t = Virtual_address.t * call_stack

  let hash (addr,stack) =
    Hashtbl.hash (Virtual_address.to_int addr,
      List.map Virtual_address.to_int stack)

  let rec compare_list cmp l1 l2 = match l1,l2 with
  | [],[] -> 0
  | x :: xs, y :: ys ->
      let c = cmp x y in
      if c = 0 then compare_list cmp xs ys
      else c
  | [], _::_ -> -1
  | _::_, [] -> 1

  let compare (a1,s1) (a2,s2) =
    let c = Virtual_address.compare a1 a2 in
    if c = 0
    then compare_list Virtual_address.compare s1 s2
    else c

  let equal x y =
    compare x y = 0
end

module V = struct
  include Basic_types.Collection_make.Hashed(V_hashable)
  type call_stack = V_hashable.call_stack

  type label = t

  let label x = x
  let create x = x

  let hash = V_hashable.hash

  let equal = V_hashable.equal

  let pp_stack fmt stack =
    let open Format in
    if stack <> [] then fprintf fmt "_";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "_") Virtual_address.pp
      fmt stack

  let pretty fmt (leader,stack) =
    let open Format in
    fprintf fmt "%a%a" Virtual_address.pp leader pp_stack stack
end

module E = struct
  type vertex = V.t
  type t = vertex * vertex
  type label = unit

  let create x () y = (x,y)
  let label _ = ()

  let src (x,_) = x
  let dst (_,y) = y

  let compare (x1,x2) (y1,y2) =
    let c = V.compare x1 y1 in
    if c <> 0 then c
    else V.compare x2 y2

  let equal x y =
    compare x y = 0

  let hash (x,y) =
    Hashtbl.hash (V.hash x, V.hash y)

  let pretty fmt (v1,v2) =
    Format.fprintf fmt "(%a->%a)" V.pretty v1 V.pretty v2
end

module Cfg_0 = Graph.Imperative.Digraph.ConcreteBidirectional(V)

module Cfg = struct
  (* Include and constrain using destructive substitution in order to redefine
   * modules [V] and [E]. *)
  module V = V
  module E = E
  include (Cfg_0 : module type of Cfg_0
    with module V := V and module E := E)
end

module CfgRegex = Fixpoint.Regex.Make(Cfg.E)
module Reduce = Fixpoint.Reduce.Make(Cfg)(CfgRegex)
module Wto = Fixpoint.Wto.Make(Cfg.V)
