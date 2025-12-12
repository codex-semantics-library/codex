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

module Var = While_ast.Var

module State = struct
  module M = Map.Make(Var)

  type t = Z.t M.t
  let empty = M.empty
  let find v s =
    try M.find v s
    with Not_found -> Z.zero

  let add v n s =
    M.add v n s

  let iter = M.iter

  let pp fmt s =
    Format.fprintf fmt "{@[<hov2>%a@]}"
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
        (fun fmt (k,v) ->Format.fprintf fmt "%a -> %s@," Var.pp k (Z.to_string v)))
      (M.bindings s)

  let of_list (l : (string * int) list) : t =
    List.fold_left (fun s (name,n) ->
      let v = Var.of_string name in
      let z = Z.of_int n in
      add v z s
    ) empty l
end

let initial_state () = State.empty

(* $MDX part-begin=tracelog *)
module Log = Tracelog.Make(struct let category = "CInterpreter" end)
(* $MDX part-end *)

open While_ast


(* $MDX part-begin=interp_aexp *)
let rec interp_aexp : aexp -> State.t -> Z.t
  = fun exp state -> Log.trace (fun p -> p "interp_aexp: %a" pp_aexp exp) ~pp_ret:Z.pp_print @@ fun () ->
  match exp with
  | Int i -> Z.of_int i
  | Var v -> State.find v state
  | Add (e1, e2) -> Z.add (interp_aexp e1 state) (interp_aexp e2 state)
  | Sub (e1, e2) -> Z.sub (interp_aexp e1 state) (interp_aexp e2 state)
  | Mul (e1, e2) -> Z.mul (interp_aexp e1 state) (interp_aexp e2 state)
(* $MDX part-end *)

(* $MDX part-begin=interp_bexp *)
let rec interp_bexp : bexp -> State.t -> bool
  = fun bexp state -> Log.trace (fun p -> p "interp_bexp: %a" pp_bexp bexp) ~pp_ret:Format.pp_print_bool @@ fun () ->
  match bexp with
  | True -> true
  | False -> false
  | Eq (e1, e2) -> (interp_aexp e1 state) = (interp_aexp e2 state)
  | Le (e1, e2) -> (interp_aexp e1 state) <= (interp_aexp e2 state)
  | Gt (e1, e2) -> (interp_aexp e1 state) > (interp_aexp e2 state)
  | Not e -> interp_bexp e state
  | And (e1, e2) -> (interp_bexp e1 state) && (interp_bexp e2 state)
(* $MDX part-end *)

(* $MDX part-begin=interp_while *)
let rec interp_stmt : stmt -> State.t -> State.t
  = fun stmt state -> Log.trace (fun p -> p "interp_stmt: %a" pp_stmt stmt) ~pp_ret:State.pp @@ fun () ->
  match stmt with
  | Skip -> state
  | Assign (v, e) -> State.add v (interp_aexp e state) state
  | Seq (e1, e2) -> state |> interp_stmt e1 |> interp_stmt e2
  | If (b, e1, e2) -> if interp_bexp b state then interp_stmt e1 state else interp_stmt e2 state
  | While (b, e) ->
      if interp_bexp b state then
        let state' = interp_stmt e state in
        interp_stmt stmt state'
      else state
(* $MDX part-end *)
