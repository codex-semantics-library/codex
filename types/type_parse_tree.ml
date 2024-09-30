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
  | Plus                        (* + *)
  | Minus                       (* - *)
  | Mult                        (* * *)
  | Div                         (* / *)
  | Mod                         (* % *)
  | Bitwise_and                 (* & *)
  | Bitwise_or                  (* | *)    
  | Bitwise_xor                 (* ^ *)
  | LShift                      (* << *)
  | RShift                      (* >> *)
  | Eq                          (* == *)
  | Diff                        (* != *)
  | Ge                          (* >= *)
  | Gt                          (* > *)
  | Le                          (* <= *)
  | Lt                          (* < *)
  | Logical_and                 (* && *)
  | Logical_or                  (* || *)

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
  | Name      of string
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
  | FunDef    of {inline:bool; pure:bool; funtyp:typ} (* funtyp must be a function type. *)
  | Global    of typ


module Pretty = struct

  
  let pp_sep str = fun fmt () -> Format.fprintf fmt str;;
  let pp_list_with_sep str x =
    Format.pp_print_list ~pp_sep:(pp_sep str) x

  
  let rec typ fmt = function
    | Name s -> Format.fprintf fmt "%s" s
    | Pointer(t,a) -> Format.fprintf fmt "(%a)%a" typ t ptr_annot a
    | Array(t,e) -> Format.fprintf fmt "(%a)[%a]" typ t expr e
    | Struct l -> Format.fprintf fmt "struct { @[<hv>%a@] }" (pp_list_with_sep "@ " cfield) l
    | Union l -> Format.fprintf fmt "union { @[<hv>%a@] }" (pp_list_with_sep "@ " cfield) l
    | Constraint(t,p) -> Format.fprintf fmt "(%a) with %a" typ t expr p
    | Exists(v,t1,t2) -> Format.fprintf fmt "∃ %s:(%a). (%a)" v typ t1 typ t2
    | Applied((LambdaAlias x),args) ->
      Format.fprintf fmt "%s(%a)" x (pp_list_with_sep ", " expr) args
    | Applied _ -> assert false (* Probably unused. *)
    | Function(ret,args) ->
      Format.fprintf fmt "%a <- (%a)" typ ret (pp_list_with_sep ", " typ) args
                           


  and binop = function
    | Eq -> "=="
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Bitwise_or -> "|"
    | Bitwise_and -> "&"
    | Bitwise_xor -> "^"
    | LShift -> "<<"
    | RShift -> ">>"
    | Diff -> "!="
    | Ge -> ">="
    | Gt -> ">"
    | Le -> "<="
    | Lt -> "<"
    | Logical_and -> "&&"
    | Logical_or -> "||"
      

  and expr fmt = function
    | Binary(bop,a,b) -> Format.fprintf fmt "(%a) %s (%a)" expr a (binop bop) expr b
    | Cst a -> Format.fprintf fmt "%s" (Z.format "%d" a)
    | Self -> Format.fprintf fmt "self"
    | Var v -> Format.fprintf fmt "%s" v
    | Unary _ -> assert false
                
  
  and cfield fmt (name,t) = Format.fprintf fmt "%a %s;" typ t name
  
  and ptr_annot fmt = function
    | Maybe_null -> Format.fprintf fmt "?"
    | Non_null -> Format.fprintf fmt "+"

  and def fmt = function
    | (name,Type t) -> Format.fprintf fmt "type %s = %a" name typ t
    | (name, FunDef{inline;funtyp=Function(ret,args)}) ->
       Format.fprintf fmt "%s %a %s(%a);" (if inline then "inline " else "")
         typ ret name (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") typ) args
    | (name, FunDef{inline;pure;funtyp}) -> 
       Format.fprintf fmt "%s %s %a" (if inline then "inline " else "") (if pure then "pure " else "") typ funtyp
    | (name, Constr(Lambda(v,t))) ->
      Format.fprintf fmt "type %s(%a) = %a" name vars v typ t
    | (name, Constr(_)) -> assert false (* Unused. *)
    | (name, Global t) -> Format.fprintf fmt "%a %s;" typ t name  

  and vars fmt vars = (pp_list_with_sep ",@ " var) fmt vars

  and var = Format.pp_print_string
       

  and annotations fmt x =
    x |> List.iter (fun d -> Format.fprintf fmt "%a@." def d)
       
end
