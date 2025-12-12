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

(** PPX to easily write While program in OCaml syntax *)

open Ppxlib

(* Helper functions for integer literals and variables *)
let is_integer_literal expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (_, _)) -> true
  | _ -> false

let extract_integer expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (s, _)) -> int_of_string s
  | _ -> Location.raise_errorf ~loc:expr.pexp_loc "Expected an integer literal"

let is_variable expr =
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident _; _ } -> true
  | _ -> false

let extract_variable expr =
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident var_name; _ } -> var_name
  | _ -> Location.raise_errorf ~loc:expr.pexp_loc "Expected a variable"


(* ppx_while.ml *)

open Ppxlib
open Ast_builder.Default        (* eint,estring *)
open Ast

let int_expr ~loc n = [%expr Int [%e eint ~loc n]]
let var_expr ~loc v = [%expr Var (Var.of_string [%e estring ~loc v])]

let rec translate_aexp ~loc = function
  | [%expr [%e? left] + [%e? right]] ->
      [%expr Add ([%e translate_aexp ~loc left], [%e translate_aexp ~loc right])]
  | [%expr [%e? left] - [%e? right]] ->
      [%expr Sub ([%e translate_aexp ~loc left], [%e translate_aexp ~loc right])]
  | [%expr [%e? left] * [%e? right]] ->
      [%expr Mul ([%e translate_aexp ~loc left], [%e translate_aexp ~loc right])]
  | [%expr [%e? v]] when is_integer_literal v ->
      int_expr ~loc (extract_integer v)
  | [%expr [%e? v]] when is_variable v ->
      var_expr ~loc (extract_variable v)
  | e -> Location.raise_errorf ~loc "At %a: Unknown arithmetic expression %a" Location.print loc Pprintast.expression e

let rec translate_bexp ~loc = function
  | [%expr true] -> [%expr True]
  | [%expr false] -> [%expr False]
  | [%expr [%e? left] = [%e? right]] ->
      [%expr Eq ([%e translate_aexp ~loc left], [%e translate_aexp ~loc right])]
  | [%expr [%e? left] <= [%e? right]] ->
       [%expr Le ([%e translate_aexp ~loc left], [%e translate_aexp ~loc right])]
  | [%expr [%e? left] > [%e? right]] ->
      [%expr Gt ([%e translate_aexp ~loc left], [%e translate_aexp ~loc right])]
  | [%expr not [%e? b]] ->
      [%expr Not [%e translate_bexp ~loc b]]
  | [%expr [%e? left] && [%e? right]] ->
      [%expr And ([%e translate_bexp ~loc left], [%e translate_bexp ~loc right])]
  | e -> Location.raise_errorf ~loc "At %a: Unknown boolean expression %a" Location.print loc Pprintast.expression e

let rec translate_com ~loc = function
  | [%expr skip] -> [%expr Skip]
  | [%expr [%e? lhs] := [%e? rhs]] when is_variable lhs ->
      [%expr Assign (Var.of_string [%e estring ~loc (extract_variable lhs)], [%e translate_aexp ~loc rhs])]
  | [%expr if [%e? cond] then [%e? tbranch] else [%e? fbranch]] ->
      [%expr If ([%e translate_bexp ~loc cond], [%e translate_com ~loc tbranch], [%e translate_com ~loc fbranch])]
  | [%expr while [%e? cond] do [%e? body] done] ->
      [%expr While ([%e translate_bexp ~loc cond], [%e translate_com ~loc body])]
  | [%expr [%e? c1]; [%e? c2]] ->
      [%expr Seq ([%e translate_com ~loc c1], [%e translate_com ~loc c2])]
  | e -> Location.raise_errorf ~loc "At %a: Unknown command expression %a" Location.print loc Pprintast.expression e

(* Register the PPX transformation *)
let () =
  let expr_pattern = Ast_pattern.(single_expr_payload __) in
  let rule =
    Extension.V3.declare
      "while_lang"
      Extension.Context.expression
      expr_pattern
      (fun ~ctxt expr ->
         let loc = Expansion_context.Extension.extension_point_loc ctxt in
         translate_com ~loc expr)
    |> Context_free.Rule.extension
  in
  Driver.register_transformation "while_lang" ~rules:[rule]
