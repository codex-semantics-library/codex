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

type unop =
  | Not
  | UMinus

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | And
  | Or
  | Xor
  | LShift
  | RShift
  | Eq
  | Diff
  | Ge
  | Gt
  | Le
  | Lt

type expr =
  | Self
  | Cst    of Z.t
  | Var    of string
  | Unary  of unop * expr
  | Binary of binop * expr * expr

type ptr_annot =
  | Maybe_null
  | Non_null

type typ =
  | Alias      of string
  | Pointer    of typ * ptr_annot
  | Array      of typ * expr
  | Struct     of (string * typ) list
  | Union      of (string * typ) list
  | Constraint of typ * expr
  | Applied    of constr * (expr list)
  | Exists     of string * typ * typ
  | Function   of typ * typ list

and constr =
  | Lambda       of (string list) * typ
  | LambdaAlias  of string

type t =
  | Type      of typ
  | Constr    of constr
