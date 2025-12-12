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

(* $MDX part-begin=variables *)
module Var = struct
  type t = { name:string; id:int }
  let uniq = Hashtbl.create 117;;
  let id_count = ref 0;;

  let of_string name =
    try Hashtbl.find uniq name
    with Not_found ->
      let id = !id_count in
      incr id_count;
      let var = {name; id } in
      Hashtbl.replace uniq name var;
      var
  let to_int x = x.id
  let compare x y = Int.compare x.id y.id

  let pp fmt x = Format.pp_print_string fmt x.name
end
(* $MDX part-end *)

(* Arithmetic expressions *)
(* $MDX part-begin=aexpressions *)
type aexp =
  | Int of int          (* integer constant *)
  | Var of Var.t        (* variable *)
  | Add of aexp * aexp  (* addition *)
  | Sub of aexp * aexp  (* subtraction *)
  | Mul of aexp * aexp  (* multiplication *)
(* $MDX part-end *)

(* This does not handle parentheses well *)
let rec pp_aexp fmt = function
  | Int i -> Format.pp_print_int fmt i
  | Var v -> Var.pp fmt v
  | Add(l,r) -> Format.fprintf fmt "%a + %a" pp_aexp l pp_aexp r
  | Sub(l,r) -> Format.fprintf fmt "%a - %a" pp_aexp l pp_aexp r
  | Mul(l,r) -> Format.fprintf fmt "%a * %a" pp_aexp l pp_aexp r

(* Boolean expressions *)
(* $MDX part-begin=bexpressions *)
type bexp =
  | True
  | False
  | Eq of aexp * aexp   (* equality *)
  | Le of aexp * aexp   (* less than or equal *)
  | Gt of aexp * aexp   (* strictly greater than *)
  | Not of bexp         (* negation *)
  | And of bexp * bexp  (* conjunction *)
(* $MDX part-end *)

let rec pp_bexp fmt = function
  | True -> Format.fprintf fmt "True"
  | False -> Format.fprintf fmt "False"
  | Eq(l,r) -> Format.fprintf fmt "%a == %a" pp_aexp l pp_aexp r
  | Le(l,r) -> Format.fprintf fmt "%a <= %a" pp_aexp l pp_aexp r
  | Gt(l,r) -> Format.fprintf fmt "%a > %a" pp_aexp l pp_aexp r
  | Not b -> Format.fprintf fmt "!(%a)" pp_bexp b
  | And(l,r) -> Format.fprintf fmt "%a && %a" pp_bexp l pp_bexp r

(* Statements *)
(* $MDX part-begin=statements *)
type stmt =
  | Skip                      (* do nothing *)
  | Assign of Var.t * aexp    (* assignment *)
  | Seq of stmt * stmt        (* sequence of statements *)
  | If of bexp * stmt * stmt  (* conditional *)
  | While of bexp * stmt      (* while loop *)
(* $MDX part-end *)

let pp_stmt fmt = function
  | Skip -> Format.fprintf fmt "Skip"
  | Assign(v,e) -> Format.fprintf fmt "%a := %a" Var.pp v pp_aexp e
  | Seq _ -> Format.fprintf fmt "Seq"
  | If(b,_,_) -> Format.fprintf fmt "If %a" pp_bexp b
  | While(b,_) -> Format.fprintf fmt "While %a" pp_bexp b
