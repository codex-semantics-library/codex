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
    {{!While_ast}the while language} using codex constructs. It is detailed in
    the While tutorial's {!page-chapter4}. *)

(* Tracelog output will be handly. *)

module Log = Tracelog.Make(struct let category = "While_analysis" end)

(* $MDX part-begin=make_terms *)
module Terms = Terms.Builder.Make
    (Terms.Condition.ConditionCudd)
    (Terms.Relations.Equality)
    ()
(* $MDX part-end *)


(* $MDX part-begin=make_sva *)
module SVA: Single_value_abstraction.Sig.NUMERIC_ENUM = struct
  include Single_value_abstraction.Ival
  include Single_value_abstraction.Bitfield
end
(* $MDX part-end *)

(* $MDX part-begin=make_nonrel *)
module NonRelationalDomain = Codex.Domains.Term_based.Nonrelational.Make(Terms)(SVA)
(* $MDX part-end *)

(* $MDX part-begin=make_domain *)
(* This is an instance of the numerical domain, of signature Domain_sig.Base. *)
module Domain = Domains.Term_domain.Make(Terms)(NonRelationalDomain)
(* $MDX part-end *)

module Var = While_ast.Var
module Store = Analysis_sva.State
let store_pp ctx = Analysis_sva.map_pp (Domain.integer_pretty ctx)

(* $MDX part-begin=state *)
type state = {
  ctx: Domain.Context.t;
  store: Domain.integer Store.t
}

(* initial value at analysis start *)
let initial_state() = { ctx = Domain.root_context (); store = Store.empty }
(* $MDX part-end *)

let pp fmt {ctx;store} =
  Format.fprintf fmt "{@[<v>ctx:@[%a@]@,store:%a}@]"
    Domain.context_pretty ctx (store_pp ctx) store

(* $MDX part-begin=serialize *)
let serialize ~widens state_a state_b =
  (* for all While variables that are not bound to the same Domain.integer *)
  Store.fold_on_nonequal_union (fun var i1 i2 (Domain.Context.Result(included, in_tuple, continue) as result) ->
    if i1 == i2 then result else (* check they are indeed bound to different integers *)
    (* If one of these variable is unbound in one branch, bind it to top *)
    let i1 = match i1 with Some i1 -> i1 | None -> Domain.integer_empty state_a.ctx in
    let i2 = match i2 with Some i2 -> i2 | None -> Domain.integer_empty state_b.ctx in
    (* Create a new join variable using Domain.serialize_integer *)
    let Domain.Context.Result(included, in_tuple, local_continue) =
      Domain.serialize_integer ~widens
        state_a.ctx i1
        state_b.ctx i2
        (included, in_tuple) in
    Domain.Context.Result(included, in_tuple, fun ctx out_tuple ->
      (* Call the local continuation which will give us the new integer value for our variable *)
      let integer, out_tuple = local_continue ctx out_tuple in
      (* Call the global continuation which will build the rest of the store *)
      let store, out_tuple = continue ctx out_tuple in
      (* Add the new value to the store *)
      (Store.add var integer store, out_tuple))
    )
  state_a.store state_b.store
  (* Initial result: returned when all variables are the same:
     - inclusion is true
     - the serialize tuple is empty
     - the store is initialized to the left branch's store *)
  (Domain.Context.Result (true, Domain.Context.empty_tuple (), fun _ctx out -> state_a.store, out))
(* $MDX part-end *)

(* $MDX part-begin=join *)
let join state_a state_b =
  let Domain.Context.Result(included, in_tuple, continue) = serialize ~widens:false state_a state_b in
  let ctx, out = Domain.typed_nondet2 state_a.ctx state_b.ctx in_tuple in
  let store, _ = continue ctx out in
  { ctx; store }
(* $MDX part-end *)

(* $MDX part-begin=join_opt *)
let join_opt a b = match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some (join a b)
(* $MDX part-end *)

(* $MDX part-begin=widen *)
let widen widening_id state_a state_b =
  let Domain.Context.Result(included, in_tuple, continue) = serialize ~widens:true state_a state_b in
  let ctx, included, out = Domain.widened_fixpoint_step
      ~widening_id ~previous:state_a.ctx ~next:state_b.ctx (included,in_tuple) in
  let store, _ = continue ctx out in
  { ctx; store }, included
(* $MDX part-end *)

(* $MDX part-begin=aexp *)
let rec aexp: state -> While_ast.aexp -> Domain.integer =
  fun state exp -> match exp with
  | Var v -> Store.find v state.store
  | Int c -> Domain.Integer_Forward.iconst (Z.of_int c) state.ctx
  | Add (e1, e2) ->
      Domain.Integer_Forward.iadd state.ctx (aexp state e1) (aexp state e2)
  | Sub (e1, e2) ->
      Domain.Integer_Forward.isub state.ctx (aexp state e1) (aexp state e2)
  | Mul (e1, e2) ->
      Domain.Integer_Forward.imul state.ctx (aexp state e1) (aexp state e2)
(* $MDX part-end *)


(* $MDX part-begin=bexp *)
let rec bexp: state -> While_ast.bexp -> Domain.boolean =
  fun state exp -> match exp with
  | True -> Domain.Boolean_Forward.true_ state.ctx
  | False -> Domain.Boolean_Forward.false_ state.ctx
  | Le (e1, e2) -> Domain.Integer_Forward.ile state.ctx (aexp state e1) (aexp state e2)
  | Eq (e1, e2) -> Domain.Integer_Forward.ieq state.ctx (aexp state e1) (aexp state e2)
  | And (e1, e2) -> Domain.Boolean_Forward.(&&) state.ctx (bexp state e1) (bexp state e2)
  | Not e1 -> Domain.Boolean_Forward.not state.ctx (bexp state e1)
  | Gt (e1, e2) -> Domain.Boolean_Forward.not state.ctx @@ Domain.Integer_Forward.ile state.ctx (aexp state e1) (aexp state e2)
(* $MDX part-end *)

let pp_ret fmt = function
  | None -> Format.fprintf fmt "<bottom>"
  | Some x -> pp fmt x

(* $MDX part-begin=stmt *)
let copy { store; ctx } = { store; ctx=Domain.Context.copy ctx }

let (let*) = Option.bind

let rec analyze_stmt: state option -> While_ast.stmt -> state option =
  fun state_opt stmt ->
  let* state = state_opt in
  Log.trace (fun p -> p "analyze_stmt %a" While_ast.pp_stmt stmt) ~pp_ret @@ fun () ->
  match stmt with
  | Skip -> state_opt
  | Assign(var,exp) ->
      let v = aexp state exp in
      Some {state with store = Store.add var v state.store}
  | Seq (c1,c2) ->
      let state_opt' = analyze_stmt state_opt c1 in
      analyze_stmt state_opt' c2
  | If (cond, if_true, if_false) -> (
      let bexp_cond = bexp state cond in
      let state' = copy state in
      match
          Domain.assume state.ctx bexp_cond,
          Domain.assume state'.ctx (Domain.Boolean_Forward.not state'.ctx bexp_cond)
      with
      | None, None (* bottom *) -> None
      | Some ctx, None (* true *)  -> analyze_stmt (Some {state with ctx = ctx}) if_true
      | None, Some ctx (* false *) -> analyze_stmt (Some {state' with ctx = ctx}) if_false
      | Some ctx_true, Some ctx_false (* top *) ->
          join_opt
            (analyze_stmt (Some {state with ctx = ctx_true}) if_true)
            (analyze_stmt (Some {state' with ctx = ctx_false}) if_false))
  | While (cond, body) ->
          let widening_id = Domains.Sig.Widening_Id.fresh () in
          let one_iteration state =
            (* update the state by one loop iteration: assume the condition and apply the body *)
            let* ctx = Domain.assume state.ctx (bexp state cond) in
            analyze_stmt (Some {state with ctx}) body
          in
          let initial_state = copy state in
          let rec loop head =
            let* next_head = one_iteration head in
            let next_head = join initial_state next_head in
            let widened, included = widen widening_id head next_head in
            if not included
            then loop widened
            else (* fixpoint reached: exit loop, assume condition is false *)
              let* ctx =
                bexp next_head cond |>
                Domain.Boolean_Forward.not next_head.ctx |>
                Domain.assume next_head.ctx in
              Some { next_head with ctx }
          in loop state
(* $MDX part-end *)
