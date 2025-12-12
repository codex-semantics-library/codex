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

(** This module presents the implementation of a simple example analyzer for
    {{!While_ast}the while language}. It is detailed in
    the While tutorial's {!page-chapter3}. *)


module Var = While_ast.Var

module Log = Tracelog.Make(struct let category = "IntvAnalysis" end)

(* $MDX part-begin=sva *)
module SVA_Ival = Single_value_abstraction.Ival
module SVA_Bval = Single_value_abstraction.Quadrivalent
(* $MDX part-end *)

(* $MDX part-begin=sva_state *)
module State = PatriciaTree.MakeMap(Var)

type sva_state = SVA_Ival.integer State.t
(* $MDX part-end *)

let map_pp pp fmt store = Format.fprintf fmt "{@[<hov>%a@]}"
  (State.pretty
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
      (fun fmt key value -> Format.fprintf fmt "@[%a -> %a@]" Var.pp key pp value))
  store

let state_pp = map_pp Framac_ival.Ival.pretty

(* $MDX part-begin=initial_state *)
let initial_state() = State.empty
(* $MDX part-end *)

(* $MDX part-begin=expression_sva *)
let rec expression_sva : sva_state -> While_ast.aexp -> SVA_Ival.integer =
  fun state exp -> let open While_ast in
  match exp with
  | Var v -> State.find v state
  | Int c -> SVA_Ival.Integer_Forward.iconst (Z.of_int c)
  | Add(e1,e2) ->
      SVA_Ival.Integer_Forward.iadd (expression_sva state e1) (expression_sva state e2)
  | Sub(e1,e2) ->
      SVA_Ival.Integer_Forward.isub (expression_sva state e1) (expression_sva state e2)
  | Mul(e1,e2) ->
      SVA_Ival.Integer_Forward.imul (expression_sva state e1) (expression_sva state e2)
(* $MDX part-end *)

(* $MDX part-begin=bexpression_sva *)
let rec bexpression_sva : sva_state -> While_ast.bexp -> SVA_Bval.boolean =
  fun state exp -> let open While_ast in
  match exp with
  | True -> SVA_Bval.Boolean_Forward.true_
  | False -> SVA_Bval.Boolean_Forward.false_
  | Le (e1, e2) -> SVA_Ival.Integer_Forward.ile (expression_sva state e1) (expression_sva state e2)
  | Eq (e1, e2) -> SVA_Ival.Integer_Forward.ieq (expression_sva state e1) (expression_sva state e2)
  | Not e1 -> SVA_Bval.Boolean_Forward.not (bexpression_sva state e1)
  | And (e1, e2) -> SVA_Bval.Boolean_Forward.(&&) (bexpression_sva state e1) (bexpression_sva state e2)
  | Gt (e1, e2) -> SVA_Bval.Boolean_Forward.not @@ SVA_Ival.Integer_Forward.ile (expression_sva state e1) (expression_sva state e2)
(* $MDX part-end *)

(* $MDX part-begin=join *)
let join s1 s2 =
  State.idempotent_inter (fun _ v1 v2 -> SVA_Ival.Integer_Lattice.join v1 v2) s1 s2

let widen s1 s2 =
  State.idempotent_inter (fun _ v1 v2 -> SVA_Ival.Integer_Lattice.widen ~previous:v1 v2) s1 s2
(* $MDX part-end *)

(* $MDX part-begin=inter *)
let inter s1 s2 =
  State.idempotent_union (fun _ v1 v2 -> SVA_Ival.Integer_Lattice.inter v1 v2) s1 s2
(* $MDX part-end *)

(* $MDX part-begin=includes *)
let includes: sva_state -> sva_state -> bool = fun l r ->
  State.reflexive_subset_domain_for_all2 (fun _ a b -> SVA_Ival.Integer_Lattice.includes a b) l r
(* $MDX part-end *)


(* $MDX part-begin=command_sva *)
let rec analyze_stmt : sva_state -> While_ast.stmt -> sva_state =
  fun state stmt -> let open While_ast in
  Log.trace (fun p -> p "Statement: %a" While_ast.pp_stmt stmt) ~pp_ret:state_pp @@ fun () ->
  match stmt with
  | Skip -> state
  | Assign(var,exp) ->
      let v = expression_sva state exp in
      State.add var v state
  | Seq (c1,c2) ->
      let state' = analyze_stmt state c1 in
      analyze_stmt state' c2
  | If (cond, if_true, if_false) ->
      begin match bexpression_sva state cond with
      | True -> analyze_stmt state if_true
      | False -> analyze_stmt state if_false
      | Bottom -> state (* Should be unreachable *)
      | Top ->
          (* analyze both, then join the results *)
          let true_state = analyze_stmt state if_true in
          let false_state = analyze_stmt state if_false in
          join true_state false_state
      end
  | While (cond, body) ->
      begin match bexpression_sva state cond with
      | False -> state (* no need to execute the body *)
      | Bottom -> state (* Should be unreachable *)
      | Top | True ->
          let next = analyze_stmt state body in
          if includes state next
          then state (* fixpoint reached *)
          else analyze_stmt (widen state (join state next)) stmt
      end
(* $MDX part-end *)


(* $MDX part-begin=refine_aexp *)
let rec refine_aexp : sva_state -> While_ast.aexp -> SVA_Ival.integer -> sva_state =
  fun state exp result -> match exp with
  | Var v -> State.add v (SVA_Ival.Integer_Lattice.inter result (State.find v state)) state
  | Int c -> state
  | Add(e1,e2) ->
      let (v1, v2) = SVA_Ival.Integer_Backward.iadd (expression_sva state e1) (expression_sva state e2) result in
      let state = match v1 with Some v -> refine_aexp state e1 v | None -> state in
      let state = match v2 with Some v -> refine_aexp state e2 v | None -> state in
      state
  | Sub(e1,e2) ->
      let (v1, v2) = SVA_Ival.Integer_Backward.isub (expression_sva state e1) (expression_sva state e2) result in
      let state = match v1 with Some v -> refine_aexp state e1 v | None -> state in
      let state = match v2 with Some v -> refine_aexp state e2 v | None -> state in
      state
  | Mul(e1,e2) ->
      let (v1, v2) = SVA_Ival.Integer_Backward.imul (expression_sva state e1) (expression_sva state e2) result in
      let state = match v1 with Some v -> refine_aexp state e1 v | None -> state in
      let state = match v2 with Some v -> refine_aexp state e2 v | None -> state in
      state
(* $MDX part-end *)

(* $MDX part-begin=refine_bexp *)
let rec refine_bexp : sva_state -> While_ast.bexp -> SVA_Bval.boolean -> sva_state =
  fun state exp result -> match exp with
  | True | False -> state
  | Not e1 ->
      let v = SVA_Bval.Boolean_Backward.not (bexpression_sva state e1) result in
      begin match v with Some v -> refine_bexp state e1 v | None -> state end
  | Le (e1, e2) ->
      let (v1, v2) = SVA_Ival.Integer_Backward.ile (expression_sva state e1) (expression_sva state e2) result in
      let state = match v1 with Some v -> refine_aexp state e1 v | None -> state in
      let state = match v2 with Some v -> refine_aexp state e2 v | None -> state in
      state
  | Eq (e1, e2) ->
      let (v1, v2) = SVA_Ival.Integer_Backward.ieq (expression_sva state e1) (expression_sva state e2) result in
      let state = match v1 with Some v -> refine_aexp state e1 v | None -> state in
      let state = match v2 with Some v -> refine_aexp state e2 v | None -> state in
      state
  | And (e1, e2) ->
      let (v1, v2) =  SVA_Bval.Boolean_Backward.(&&) (bexpression_sva state e1) (bexpression_sva state e2) result in
      let state = match v1 with Some v -> refine_bexp state e1 v | None -> state in
      let state = match v2 with Some v -> refine_bexp state e2 v | None -> state in
      state
  | Gt (e1, e2) -> refine_bexp state (Not(Le(e1,e2))) result
(* $MDX part-end *)


let rec analyze_stmt_refine : sva_state -> While_ast.stmt -> sva_state =
  fun state stmt -> let open While_ast in
  Log.trace (fun p -> p "Statement: %a" While_ast.pp_stmt stmt) ~pp_ret:state_pp @@ fun () ->
  match stmt with
  | Skip -> state
  | Assign(var,exp) ->
      let v = expression_sva state exp in
      State.add var v state
  | Seq (c1,c2) ->
      let state' = analyze_stmt_refine state c1 in
      analyze_stmt_refine state' c2
  (* $MDX part-begin=command_sva_refine *)
  | If (cond, if_true, if_false) ->
      begin match bexpression_sva state cond with
      (* no refinement possible when the value is known *)
      | True -> analyze_stmt_refine state if_true
      | False -> analyze_stmt_refine state if_false
      | Bottom -> state (* Should be unreachable *)
      | Top ->
          (* analyze both, then join the results *)
          let true_state = analyze_stmt_refine (refine_bexp state cond True) if_true in
          let false_state = analyze_stmt_refine (refine_bexp state cond False) if_false in
          join true_state false_state
      end
  | While (cond, body) ->
      let cond_value = bexpression_sva state cond in
      begin match cond_value with
      | False -> state (* no need to execute the body *)
      | Bottom -> state (* Should be unreachable *)
      | Top | True ->
          let refined_state = if cond_value == Top then refine_bexp state cond True else state in
          let next = analyze_stmt_refine refined_state body in
          if includes state next
          then refine_bexp state cond False (* at the loop exit, the condition is false *)
          else analyze_stmt_refine (widen state (join state next)) stmt
      end
(* $MDX part-end *)
