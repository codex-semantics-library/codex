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

type ar1_format = {prefix:string;suffix:string}
type ar2_format = {prefix:string;middle:string;suffix:string}
type ar3_format = {prefix:string;middle1:string;middle2:string;suffix:string}

(* TODO: arn. *)
type expr =
  |Ar0 of {s:string}
  |Ar1 of {format:ar1_format; arg:expr}
  |Ar2 of {format:ar2_format; arg1:expr; arg2:expr}
  |Ar3 of {format:ar3_format; arg1:expr; arg2:expr; arg3:expr}
  |Ite of {cond:expr; then_expr:expr; else_expr: expr}

type expr_path = int list

let rec equal_expr e1 e2 =
  match e1, e2 with
  | _ when e1 == e2 -> true
  | Ar0 {s=s1}, Ar0 {s=s2} -> s1 = s2
  | Ar1 {format=format1; arg=arg1}, Ar1 {format=format2; arg=arg2} -> (format1 == format2 || format1 = format2) && equal_expr arg1 arg2
  | Ar2 {format=formata; arg1=arg1a;arg2=arg2a}, Ar2 {format=formatb; arg1=arg1b;arg2=arg2b} -> (formata == formatb || formata = formatb) && equal_expr arg1a arg1b && equal_expr arg2a arg2b
  | Ar3 {format=formata; arg1=arg1a;arg2=arg2a;arg3=arg3a}, Ar3 {format=formatb; arg1=arg1b;arg2=arg2b;arg3=arg3b} ->
    (formata == formatb || formata = formatb)
    && equal_expr arg1a arg1b
    && equal_expr arg2a arg2b
    && equal_expr arg3a arg3b
  | Ite {cond=c1;then_expr=t1;else_expr=e1}, Ite {cond=c2;then_expr=t2;else_expr=e2}
    -> equal_expr c1 c2 && equal_expr t1 t2 && equal_expr c1 c2
  | _ -> false

let arity = function
  | Ar0 _ -> 0 | Ar1 _ -> 1 | Ar2 _ -> 2 | Ar3 _ -> 3 | Ite _ -> 3


let isdeep expr = match expr with
  |Ar0 {s} -> String.contains s ' '
  |Ar1 {format; arg} -> true
  |Ar2 _ -> true
  |Ar3 _ -> true
  |Ite _ -> true

let rec string_of_expr_impl display_par expr  = (if display_par then "(" else "") ^  (match expr with
  |Ar0 {s} -> s
  |Ar1 {format={prefix;suffix}; arg} -> prefix ^ (string_of_expr_impl (isdeep arg) arg) ^ suffix
  |Ar2 {format={prefix;middle;suffix}; arg1;arg2} ->
    prefix ^ (string_of_expr_impl (isdeep arg1) arg1) ^
    middle ^ (string_of_expr_impl (isdeep arg2) arg2) ^
    suffix
  |Ar3 {format={prefix;middle1;middle2;suffix}; arg1;arg2;arg3} ->
    prefix ^ (string_of_expr_impl (isdeep arg1) arg1) ^
    middle1 ^ (string_of_expr_impl (isdeep arg2) arg2) ^
    middle2 ^ (string_of_expr_impl (isdeep arg2) arg3) ^
    suffix
  |Ite {cond:expr; then_expr:expr; else_expr: expr} ->
    string_of_expr_impl (isdeep cond) cond ^ " ? " ^ string_of_expr_impl (isdeep then_expr) then_expr ^ " : " ^ string_of_expr_impl (isdeep else_expr) else_expr
  )
  ^ (if display_par then ")" else "")

let string_of_expr expr = string_of_expr_impl false expr

module Hashcons = struct

  (* TODO: We could be hash-consing the format too. *)
  module ExprH = Weak.Make(struct
      type t = expr
      let equal = equal_expr
      let hash = Hashtbl.hash
    end)
  let expr_h = ExprH.create 17
  let rec hashcons x =
      try ExprH.find expr_h x
      with Not_found ->
        begin
          let x = match x with
            | Ar0{s} -> x 
            | Ar1{format;arg} ->
              Ar1{format;arg = hashcons arg}
            | Ar2{format;arg1;arg2} ->
              Ar2{format;arg1 = hashcons arg1; arg2 = hashcons arg2}
            | Ar3{format;arg1;arg2;arg3} ->
              Ar3{format;arg1 = hashcons arg1; arg2 = hashcons arg2; arg3 = hashcons arg3}
            | Ite{cond;then_expr;else_expr} ->
              Ite{cond = hashcons cond; then_expr = hashcons then_expr; else_expr = hashcons else_expr}
          in
          ExprH.add expr_h x;
          x
        end
end

let hashcons = Hashcons.hashcons


module Location_identifier = struct

  type path = 
    | Name of string
    | Int64 of int64
    | Inside: {locid:'b t;idx:int} -> path

  and 'a kind =
    | Address
    | Expression
    | DbaInstruction
    (* | Instruction *)
    (* | Symbol *)
    (* | Function *)
    (* | Type *)

  and 'a t = 'a kind * path

  type any = Any: 'a t -> any [@@unboxed]

  let equal_kind: type a b. a kind -> b kind -> bool = fun a b ->
    assert(Obj.is_int @@ Obj.repr a);
    assert(Obj.is_int @@ Obj.repr b);
    Int.equal (Obj.magic a) (Obj.magic b)

  let rec equal: type a b. a t -> b t -> bool =
    fun (ka,pa) (kb,pb) ->
    equal_kind ka kb && equal_path pa pb

  and equal_path a b = match a,b with
    | Name sa, Name sb -> String.equal sa sb
    | Int64 ia, Int64 ib -> Int64.equal ia ib
    | Inside{locid=locida;idx=idxa},Inside{locid=locidb;idx=idxb} ->
      idxa = idxb && equal locida locidb
    | Name _, (Int64 _ | Inside _)
    | (Int64 _ | Inside _), Name _ -> false
    | Int64 _, Inside _ | Inside _, Int64 _ -> false

  let hash = Hashtbl.hash
  
end

type 'a located = 'a * 'a Location_identifier.t

