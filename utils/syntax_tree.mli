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

(* Language-independent representation of syntax trees.  *)

type ar1_format = {prefix:string; suffix:string}
type ar2_format = {prefix:string; middle:string ; suffix:string}
type ar3_format = {prefix:string; middle1:string; middle2:string; suffix:string}

type expr =
  |Ar0 of {s:string}
  |Ar1 of {format:ar1_format; arg:expr}
  |Ar2 of {format:ar2_format; arg1:expr; arg2:expr}
  |Ar3 of {format:ar3_format; arg1:expr; arg2:expr; arg3:expr}          
  (* TODO: Generalize Ite to other ternary operators,
     but I have no use for thiw for now. *)
  |Ite of {cond:expr; then_expr:expr; else_expr: expr}

val arity: expr -> int

val equal_expr: expr -> expr -> bool

(* A path in the expression tree: 
   [] = root
   n::path = first take the nth child, then follow the rest of the path. *)
(* MAYBE: It could be a list of pair, for the nth child out of
   m. Would make it easier to find issues. *)
type expr_path = int list

val string_of_expr: expr -> string

(* Hashconsing expression reduces their size by deduplication, and is
   useful before dumping. *)
val hashcons: expr -> expr

module Location_identifier:sig
  
  (* A path in the abstract syntax tree. *)
  type path = 
    (* Absolute identifiers. *)
    | Name of string
    | Int64 of int64
        
    (* Relative identifier expressing position [idx] (starting from 0)
       inside [locid]. Useful e.g. for expressions that may not have
       identifiers, but can be identified relatively to an absolute
       element (e.g. the expression inside a named statement). *)
    | Inside: {locid:'b t;idx:int} -> path

  (** The syntactic element represented. *)
  and 'a kind =
    | Address
    | Expression
    | DbaInstruction            (* In general: micro-instructions. *)

  and 'a t = 'a kind * path

  
  val equal: 'a t -> 'b t -> bool

  val hash: 'a t -> int

  type any = Any: 'a t -> any [@@unboxed]
  
end

type 'a located = 'a * 'a Location_identifier.t
