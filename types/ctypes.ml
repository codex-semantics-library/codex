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

module Int_option = Datatype_sig.Option(Datatype_sig.Int)
module String_option = Datatype_sig.Option(Datatype_sig.String)

(** String to type map (parameter to expression) *)
module SymbolMap = Datatype_sig.StringMap

type value =
  | Const of Z.t
  | Sym of string

let pp_value fmt =
  let open Format in function
    | Const x -> Z.pp_print fmt x
    | Sym s -> fprintf fmt "\"%s\"" s

module Pred = struct
  type unop =
    | Extract of int * int (** start index, length *)

  let pp_unop op pp_expr fmt expr =
    match op with
    | Extract (idx,len) ->
      Format.fprintf fmt "%a{%d, %d}" pp_expr expr idx (idx+len-1)

  type binop =
    | Add | Sub | Mul | And | Or
    | Concat of int * int (** Size 1, size 2. First argument is the most significant *)

  let pp_binop fmt op =
    Format.pp_print_string fmt @@ match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | And -> "&"
    | Or -> "|"
    | Concat (size1, size2) -> Format.sprintf "::<%d,%d>" size1 size2

  type expr =
    | Val of value
    | Self
    | Unop of unop * expr
    | Binop of binop * expr * expr

  type cmpop = Equal | NotEqual | ULt | SLt | ULeq | SLeq | UGeq | SGeq | UGt | SGt

  let pp_cmpop fmt op =
    Format.pp_print_string fmt @@ match op with
    | Equal -> "="
    | NotEqual -> "!="
    | ULt -> "<u"
    | SLt -> "<s"
    | ULeq -> "<=u"
    | SLeq -> "<=s"
    | UGeq -> ">=u"
    | SGeq -> ">=s"
    | UGt -> ">u"
    | SGt -> ">s"

  (** Predicate on a structure field or array element, which is supposed to be
      true at all times. *)
  type t =
    | True
    | Cmp of cmpop * expr * expr
    | And of t * t

  let compare : t -> t -> int = Stdlib.compare

  let equal t u = compare t u = 0

  let rec pp_expr fmt =
    let open Format in function
      | Val value -> pp_value fmt value
      | Self -> pp_print_string fmt "self"
      | Unop (op, e) ->
        fprintf fmt "@[<hov 2>%a@]" (pp_unop op pp_expr) e
      | Binop (op, e1, e2) ->
        fprintf fmt "@[<hov 2>(%a@ %a@ %a)@]" pp_expr e1 pp_binop op pp_expr e2

  let rec pp fmt =
    let open Format in function
      | True -> pp_print_string fmt "true"
      | Cmp (cop, e1, e2) -> fprintf fmt "@[<hov 2>(%a@ %a@ %a)@]" pp_expr e1 pp_cmpop cop pp_expr e2
      | And (p1, p2) -> fprintf fmt "@[<hov 2>%a@ &&@ %a@]" pp p1 pp p2

  let eq v =
    Cmp (Equal, Self, Val v)

  let neq v =
    Cmp (NotEqual, Self, Val v)

  let slt v =
    Cmp (SLt, Self, Val v)

  let sleq v =
    Cmp (SLeq, Self, Val v)

  let sgeq v =
    Cmp (SGeq, Self, Val v)

  let sgt v =
    Cmp (SGt, Self, Val v)

  let ult v =
    Cmp (ULt, Self, Val v)

  let uleq v =
    Cmp (ULeq, Self, Val v)

  let ugeq v =
    Cmp (UGeq, Self, Val v)

  let ugt v =
    Cmp (UGt, Self, Val v)

  exception Undefined_symbol of string

  let lift_unop : unop -> Z.t -> Z.t = function
    | Extract (idx, len) -> fun x -> Z.extract x idx len

  let lift_binop : binop -> Z.t -> Z.t -> Z.t = function
    | Add -> Z.add
    | Sub -> Z.sub
    | Mul -> Z.mul
    | And -> Z.logand
    | Or -> Z.logor
    | Concat (_,size2) -> fun x y ->
      Z.logor (Z.shift_left x size2) y

  let rec eval_expr ~symbols ~self = function
    | Val (Const c) -> c
    | Val (Sym s) ->
      begin try Datatype_sig.StringMap.find s symbols
        with Not_found -> raise (Undefined_symbol s) end
    | Self -> self
    | Unop (op, e) ->
      lift_unop op (eval_expr ~symbols ~self e)
    | Binop (op, e1, e2) ->
      lift_binop op (eval_expr ~symbols ~self e1) (eval_expr ~symbols ~self e2)

  let lift_cmpop : cmpop -> Z.t -> Z.t -> bool = function
    | Equal -> Z.equal | NotEqual -> (fun a b -> not (Z.equal a b))
    | ULt -> Z.lt | ULeq -> Z.leq | UGeq -> Z.geq | UGt -> Z.gt
    | SLt -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) < 0
    | SLeq -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) <= 0
    | SGeq -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) >= 0
    | SGt -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) > 0

  let rec eval ~symbols ~self p =
    let module SM = Datatype_sig.StringMap in
    match p with
    | True -> true
    | And (p1,p2) -> eval ~symbols ~self p1 && eval ~symbols ~self p2
    | Cmp (cmpop, e1, e2) ->
      lift_cmpop cmpop (eval_expr ~symbols ~self e1) (eval_expr ~symbols ~self e2)


  let conjunction pred1 pred2 =
    match pred1, pred2 with
    | _ when pred1 = pred2 -> pred1
    | True, _ -> pred2
    | _, True -> pred1
    | _ -> And(pred1, pred2)

  let rec substitutes_expr expr env =
    match expr with
    | Val(Sym a) when SymbolMap.mem a env -> SymbolMap.find a env
    (* | Val _ | Self -> pe *)
    | Unop (op, expr) -> Unop (op, substitutes_expr expr env)
    | Binop (op, expr1, expr2) -> Binop (op, substitutes_expr expr1 env, substitutes_expr expr2 env)
    | _ -> expr
  
  let rec substitutes pred env =
    match pred with
    | True -> pred
    | Cmp (op, e1, e2) -> Cmp (op, substitutes_expr e1 env, substitutes_expr e2 env)
    | And (pred1, pred2) -> And (substitutes pred1 env, substitutes pred2 env)
end

type basic = int * string (** Byte size and name. *)

type enum =
  { en_name : string option;
    en_byte_size : int;
    en_values : (string * int) list; }

type index_value =
  { idx : string ;
    fe : string ;
    ofs : int
  }

type index =
  | ValidUnknown
  | PastEnd
  | Zero
  | Idx of index_value

let pp_index fmt =
  let open Format in function
    | ValidUnknown -> pp_print_string fmt "ValidUnknown"
    | Zero -> pp_print_string fmt "idx 0"
    | PastEnd -> pp_print_string fmt "PastEnd"
    | Idx {idx; fe=_; ofs} -> fprintf fmt "idx [%s].%d" idx ofs

let join_indices i1 i2 =
  match i1,i2 with
  | Zero,Zero -> Zero
  | _ -> ValidUnknown

type structure =
  { st_byte_size : int option;
    st_members : (int * string * typ) list;
    (** Offset, field name, type, and predicate on type. *)
  }

and union =
  { un_byte_size : int option;    (* Size of the union *)
    un_types : (string * typ) list; (* typename, and type of the field *)
  }

and pointer = { pointed : typ; index : index }

and descr =
  | Void
  | Base of basic
  | Structure of structure
  | Ptr of pointer
  (** Pointed type and index of pointed element in array. For non-array
      pointed types, the index should be [ConstIdx 0]. *)
  | Enum of enum
  | Array of typ * value option
  (** Element type and number of elements (if statically known). *)
  | Function of typ * typ list (** Return type and parameter types *)
  | Name of string
  | Application of constr * (Pred.expr list)
  | Existential of typ * string * typ
  | Union of union
  | Weak of typ

and constr =
  | ConstrName of string
  (** Type and its parameters *)
  | Constructor of typ * (string list)

and typ =
  { mutable descr : descr;
    pred : Pred.t;
  }


exception Undefined_type of string
exception Undefined_type_constructor of string
exception Application_error
module StringHash = Hashtbl.Make(struct include String let hash = Hashtbl.hash end)
let type_map:(string,typ) Hashtbl.t = Hashtbl.create 17;;
let add_type_name_definition name typ = if not @@ Hashtbl.mem type_map name then Hashtbl.add type_map name typ ;;
let type_of_name name =
  try Hashtbl.find type_map name
  with Not_found -> raise (Undefined_type name)

let rec pp_ctype fmt f =
  match f.descr with
  | Void -> Printf.fprintf fmt "void"
  | Base (_,s) -> Printf.fprintf fmt "%s" s
  | Structure s -> Printf.fprintf fmt "Struct"
  | Ptr p -> Printf.fprintf fmt "(%a)*" pp_ctype p.pointed
  | Enum e -> Printf.fprintf fmt "Enum"
  | Array (t, vo) -> Printf.fprintf fmt "Array"
  | Function (r, a) -> Printf.fprintf fmt "Function"
  | Name s -> Printf.fprintf fmt "%s" s 
  | Application (c, pl) -> Printf.fprintf fmt "Aplication"
  | Existential (t, string, t2) -> Printf.fprintf fmt "Existential"
  | Union u -> Printf.fprintf fmt "Union"
  | Weak _ -> Printf.fprintf fmt "Weak"
;;

let rec pp_ctype_list fmt l =
  match l with
  | [] -> ()
  | [x] -> pp_ctype fmt x
  | x::xs -> pp_ctype fmt x; Printf.fprintf fmt ", "; pp_ctype_list fmt xs
;;

let pp_function_type fmt f =
  try
    match (type_of_name f).descr with
    | Function (ret, args) -> Printf.fprintf fmt "[%a] -> %a" 
      pp_ctype_list args
      pp_ctype ret
    | _ -> ()
  with _ -> Printf.fprintf fmt "undefined ctype"
;;

let constr_map:(string,constr) Hashtbl.t = Hashtbl.create 17;;
let add_constr_name_definition name typ = Hashtbl.replace constr_map name typ;;

let constr_of_name name =
  try Hashtbl.find constr_map name
with Not_found -> raise (Undefined_type_constructor name)

let rec inlined_constr constr =
  match constr with
  | Constructor (typ, params) -> (typ, params)
  | ConstrName n -> inlined_constr (constr_of_name n)

let expr_to_symbol expr =
  let open Pred in
  match expr with
  | Val((Sym a) as symb)  -> symb
  | _ -> failwith("Expression is not a simply symbol")

let function_map:(string,typ) Hashtbl.t = Hashtbl.create 17;;
let add_function_name_definition name typ = Hashtbl.replace function_map name typ;;
let function_of_name name =
  try Hashtbl.find function_map name
  with Not_found -> raise (Undefined_type name)

let pp_string fmt s =
  Format.fprintf fmt "\"%s\"" s

(* Prints a type's description as an OCaml value, or simply its name if {reuse}
 * is true. (output in OCaml) *)
let rec pp_descr_name precedence reuse fmt descr =
  let open Format in
  match descr with
  | Void -> pp_print_string fmt "void"
  | Base (sz,name) -> fprintf fmt "@[<hov 2>%s(%d)@]" name sz
  | Ptr {pointed = typ; index = _} ->
  (* | Ptr {pointed = typ; index = Zero} -> *)
    fprintf fmt "@[<hov 2>%a*@]" (pp_type_name precedence true) typ
  (* | Ptr {pointed = typ; index} ->
    fprintf fmt "@[<hov 2>%a* at %a@]" (pp_type_name precedence true) typ pp_index index *)
  | Structure s ->
    fprintf fmt "@[<hov 2>struct {@,\
                 st_byte_size =@ %a;@ \
                 st_members =@ \
                 @[<v 2>[@ %a@ ]@];@ \
                 }@]"
      Int_option.pretty s.st_byte_size
      (pp_print_list
         (fun fmt (offset,name,typ) ->
            fprintf fmt "(@[<hov 2>%d,@ %a,@ %a@]);"
              offset pp_string name (pp_type_name 0 false) typ))
      s.st_members
  | Enum ({en_name = Some name;_}) when reuse ->
    (* Simply refer to a (normally) declared enum *)
    pp_print_string fmt name
  | Enum e ->
    fprintf fmt "@[<hov 2>enum {@ \
                 en_name =@ %a;@ \
                 en_byte_size =@ %d;@ \
                 en_values =@ \
                 @[<v 2>[ %a@ \
                 ]@];@ \
                 }@]"
      String_option.pretty e.en_name
      e.en_byte_size
      (pp_print_list
         (fun fmt (name,value) ->
            fprintf fmt "(%a, %d);" pp_string name value))
      e.en_values
  | Array (typ, Some size) ->
    (if precedence > 1 then
       fprintf fmt "@[<hov 2>(%a[%a])@]"
     else
       fprintf fmt "@[<hov 2>%a[%a]@]") (pp_type_name 1 true) typ pp_value size
  | Array (typ, None) ->
    (if precedence > 1 then
       fprintf fmt "@[<hov 2>(%a[])@]"
     else
       fprintf fmt "@[<hov 2>%a[]@]") (pp_type_name 1 true) typ
  | Function (ret_t, params) ->
    (if precedence > 0 then
       fprintf fmt "@[<hov 2>(Function (%a, [%a]))@]"
     else
       fprintf fmt "@[<hov 2>Function (%a, [%a])@]")
      (pp_type_name 0 reuse) ret_t
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_type_name 0 true)) params
  
  | Name n -> fprintf fmt "Name(@[<hov 2>%s@])" n
  | Application (const, args) ->
    fprintf fmt "@[<hov 2>%a(%a)@]"
      (pp_constr_name 0 reuse) const
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") Pred.pp_expr) args
  | Existential (typ, var, tvar) -> 
    fprintf fmt "@[<hov 2>∃%s : %a.%a@]" var (pp_type_name precedence true) tvar (pp_type_name precedence true) typ

  | Union u ->
    fprintf fmt "@[<hov 2>union {@,\
                  un_byte_size =@ %a;@ \
                  un_types =@ \
                  @[<v 2>[@ %a@ ]@];@ \
                  }@]"
      Int_option.pretty u.un_byte_size
      (pp_print_list
          (fun fmt (name,typ) ->
            fprintf fmt "(@[<hov 2>%a,@ %a@]);"
              pp_string name (pp_type_name 0 false) typ))
      u.un_types
  
  | Weak typ ->
    fprintf fmt "@[<hov 2>Weak %a@]" (pp_type_name precedence true) typ

and pp_constr_name precedence reuse fmt constr =
  let open Format in
  match constr with
  | ConstrName n -> fprintf fmt "@[<hov 2>%s@]" n
  | Constructor (t,params) ->
    fprintf fmt "@[<hov 2>(<%a> %a)@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_print_string) params
      (pp_type_name 0 reuse) t

and pp_type_name precedence reuse fmt typ =
  let open Format in
  (if precedence > 0 then
     fprintf fmt "@[<hov 2>(%a%a)@]" (pp_descr_name 0 reuse) typ.descr
   else
     fprintf fmt "@[<hov 2>%a%a@]" (pp_descr_name precedence reuse) typ.descr)
    (fun fmt p -> if p <> Pred.True then fprintf fmt "@ with %a" Pred.pp p else ()) typ.pred

(* Print the type's name. *)
let pp fmt typ =
  pp_type_name 0 false fmt typ

let pp_descr fmt descr =
  pp_descr_name 0 false fmt descr

let pp_constr fmt constr =
  pp_constr_name 0 false fmt constr


let rec subst_type ~in_ptr t env =
  let pred = Pred.substitutes t.pred env in
  match t.descr with
  | Void | Base _ | Enum _ | Name _ -> {t with pred = pred}
  | Structure s ->
      let members = List.map (fun (ofs,name,typ) -> (ofs,name,subst_type ~in_ptr typ env)) s.st_members in 
        {descr = Structure {s with st_members = members} ; pred}
  | Ptr {pointed; index } -> (* does not substitutes further than pointers *)
    (* let params, args = StringMap.fold (fun p a (params, args)  -> p::params, a::args) env ([],[]) in
      let app = {descr = Application (Constructor (pointed, params), args); pred = Pred.True} in
        {descr = Ptr {pointed = app; index}; pred} *)
        {descr = Ptr {pointed = subst_type ~in_ptr:true pointed env; index}; pred}
  | Array (t, Some (Sym s)) when SymbolMap.mem s env ->
      let sz_value = expr_to_symbol (SymbolMap.find s env) in
        {descr = Array (subst_type ~in_ptr t env, Some sz_value); pred}
  | Array (t, sz) -> {descr = Array ((subst_type ~in_ptr t env), sz); pred}
  | Function (t, param) -> {t with pred = pred}
  | Application (c, args) ->
    let args = List.map (fun e -> Pred.substitutes_expr e env) args in
    if not in_ptr then
      let res = apply c args in
      {res with pred = Pred.conjunction pred res.pred}
    else {descr = Application (c, args); pred = pred}

  | Existential (t, v, tv) -> {descr = Existential (subst_type ~in_ptr t env, v, subst_type ~in_ptr tv env); pred }

  | Union u ->
      let types = List.map (fun (name, typ) -> (name, subst_type ~in_ptr typ env)) u.un_types in
        {descr = Union {u with un_types = types}; pred }

  | Weak _ -> assert false

and apply constr args =
  let rec map_bindings params args =
    match params, args with
    | [], [] -> SymbolMap.empty
    | v::vs, e::es ->
      SymbolMap.add v e (map_bindings vs es)
    | _-> raise Application_error
  in
    let t, params = inlined_constr constr in
    subst_type ~in_ptr:false t (map_bindings params args)

let substitute_symbol typ prev symb =
  Codex_log.debug "Ctypes.substitute_symbol %s by %s" prev symb ; 
  let env = SymbolMap.singleton prev Pred.(Val (Sym symb)) in
  subst_type ~in_ptr:false typ env

let rec inlined typ =
  match typ.descr with
  | Name n ->
    let res = type_of_name n in
      inlined {res with pred = Pred.conjunction typ.pred res.pred}
  | Application (c, args) ->
    let res = apply c args in
      inlined {res with pred = Pred.conjunction typ.pred res.pred}
  | _ -> typ

let inlined_descr descr = 
  match descr with
  | Name n -> (inlined (type_of_name n)).descr
  | Application (c, args) -> (inlined (apply c args)).descr
  | _ -> descr


let compare_list cmp_elt l1 l2 =
  let rec cmp = function
    | [],[] -> 0
    | [],_ -> -1
    | _,[] -> 1
    | (h1 :: t1), (h2 :: t2) ->
      let c = cmp_elt h1 h2 in
      if c = 0 then cmp (t1, t2) else c
  in
  cmp (l1,l2)

let rec compare_struct ~only_descr s1 s2 =
  let c = Stdlib.compare s1.st_byte_size s2.st_byte_size in
    if c = 0 then
      compare_list (fun (i1,_,t1) (i2,_,t2) ->
            compare ~only_descr t1 t2) s1.st_members s2.st_members
    else c

and compare_union ~only_descr u1 u2 =
  let c = Stdlib.compare u1.un_byte_size u2.un_byte_size in
    if c = 0 then
      compare_list (fun (_,t1) (_,t2) ->
            compare ~only_descr t1 t2) u1.un_types u2.un_types
    else c

and compare_descr ~only_descr x y =
  (* let open Format in
    fprintf Format.std_formatter "comparing types %a to %a\n" pp_descr x pp_descr y ; *)
  match x, y with

  | Void,Void -> 0
  | Void, _ -> -1
  | _,Void -> 1
  | Base sz1, Base sz2 -> Stdlib.compare sz1 sz2
  | Base _, _ -> -1
  | _, Base _ -> 1
  
  | Structure s1, Structure s2 -> compare_struct ~only_descr s1 s2
  | Structure _, _ -> -1
  | _, Structure _ -> 1
  | Ptr {pointed=t;index=ti}, Ptr {pointed=u;index=ui} ->
    let c = compare ~only_descr t u in
    if only_descr || c <> 0 then c else (Stdlib.compare : index -> index -> int) ti ui
  | Ptr _, _ -> -1
  | _, Ptr _ -> 1
  | Enum { en_name = n1; _ }, Enum { en_name = n2; _ } -> Stdlib.compare n1 n2
  | Enum _, _ -> -1
  | _, Enum _ -> 1
  | Array (t,l), Array (t',l') ->
    let c = (Stdlib.compare : value option -> value option -> int) l l' in
    if c <> 0 then c else compare ~only_descr t t'
  | Array _, _ -> -1
  | _, Array _ -> 1
  | Function (t,ts), Function (u,us) ->
    compare_list (compare ~only_descr) (t :: ts) (u :: us)

  | Name n1, Name n2 -> Stdlib.compare n1 n2
  | Name n1, _ -> -1
  | _, Name n2 -> 1

  | Application (c1, a1), Application (c2, a2) ->
    let c = compare_constr ~only_descr c1 c2 in
      if c <> 0 then c
      else compare_list (Stdlib.compare : Pred.expr -> Pred.expr -> int) a1 a2
  | Application _, _ -> -1
  | _, Application _ -> 1
  
  | Existential (t1, v1, tv1), Existential (t2, v2, tv2) ->
    let c = Stdlib.compare v1 v2 in
      if c <> 0 then c
      else
        (* let c = compare ~only_descr tv1 tv2 in
        if c <> 0 then c 
        else *) compare ~only_descr t1 t2
  
  | Existential _, _ -> -1
  | _, Existential _ -> 1

  | Union u1, Union u2 -> compare_union ~only_descr u1 u2
  | Union _, _ -> -1
  | _, Union _ -> 1

  | Weak t1, Weak t2 -> compare ~only_descr t1 t2
  | Weak _, _ -> -1
  | _, Weak _ -> 1

and compare_constr ~only_descr a b =
  match a, b with
  | ConstrName n1, ConstrName n2 -> Stdlib.compare n1 n2
  | ConstrName _, _ -> -1
  | _, ConstrName _ -> 1

  | Constructor (t1, p1), Constructor (t2, p2) -> 
    let c = compare ~only_descr t1 t2 in
      if c <> 0 then c
      else compare_list Stdlib.compare p1 p2

and compare ~only_descr t u =
  if only_descr then compare_descr ~only_descr t.descr u.descr
  else
    let c1 = compare_descr ~only_descr t.descr u.descr in
    if c1 <> 0 then c1
    else (Stdlib.compare : Pred.t -> Pred.t -> int) t.pred u.pred

exception Unsizeable_type

(* Size in bytes *)
let rec sizeof_descr descr = (* function *)
  match (inlined_descr descr) with
    | Base (sz,_) | Structure {st_byte_size = Some sz;_}
    | Enum {en_byte_size = sz;_} -> sz
    | Ptr _ -> 4
    | Array (elem_t, Some (Const sz)) ->
      Z.to_int sz * (sizeof_descr elem_t.descr)
    | Void | Structure { st_byte_size = None;_ }
    | Array (_, _) | Function (_, _) ->
      (* Not handled. *)
      raise Unsizeable_type
      (* raise (Invalid_argument "type_size") *)
    | Name _ -> assert false 
    | Application _ -> assert false
    | Existential (t, _, _) -> sizeof_descr t.descr

    | Union {un_byte_size = Some sz; _} -> sz
    | Union {un_byte_size = None; _} ->
      raise Unsizeable_type
      (* raise (Invalid_argument "type_size") *)
    | Weak t -> sizeof_descr t.descr

let sizeof x = sizeof_descr x.descr


let check_pred px py = 
  let open Pred in
  match px, py with
  (* | True, _ | _, True -> true *)
  | _ -> ((=) : t -> t -> bool) px py
      
    
let rec equiv ~only_descr x y =
  match x.descr, y.descr with
  | Name n1, Name n2 when n1 = n2 -> (not only_descr) || check_pred x.pred y.pred 
  | Name n1, _ ->
      let t = type_of_name n1 in
      equiv ~only_descr {t with pred = Pred.conjunction t.pred x.pred} y
  | _, Name n2 ->
      let t = type_of_name n2 in
      equiv ~only_descr x {t with pred = Pred.conjunction t.pred y.pred}

  | Void, Void -> (not only_descr) || check_pred x.pred y.pred
  | Base sz1, Base sz2 -> sz1 = sz2 && ((not only_descr) || check_pred x.pred y.pred)
  
  | Structure s1, Structure s2 -> (compare_struct ~only_descr s1 s2) = 0

  | Ptr {pointed=t; index=ti}, Ptr {pointed=u; index=ui} ->
    equiv ~only_descr t u &&
    (not only_descr || ti = ui)
  
  | Enum {en_name = n1; _}, Enum {en_name = n2; _ } ->
      n1 = n2 && ((not only_descr) || check_pred x.pred y.pred)

  | Array (t1,l1), Array (t2, l2) ->
    (l1 = l2) && (equiv ~only_descr t1 t2)

  | Function (t, ts), Function (u,us) ->
    compare_list (fun x y -> if equiv ~only_descr x y then 0 else 1) (t :: ts) (u :: us) = 0

  | Application (ConstrName n1, args1), 
    Application (ConstrName n2, args2) when n1 = n2 -> 
     (compare_list (fun x y -> (Stdlib.compare : Pred.expr -> Pred.expr -> int) x y) args1 args2) = 0 

  | Application (c, args), _ ->
    let t = apply c args in
      equiv ~only_descr {descr = t.descr; pred = Pred.conjunction t.pred x.pred} y
  | _, Application (c, args) ->
    let t = apply c args in
      equiv ~only_descr x {descr = t.descr; pred = Pred.conjunction t.pred y.pred}

  (* | _ when sizeof x = sizeof y -> check_pred x.pred y.pred *)

  | _ -> false

(* checks that u is equivalent of t (if u is a subtype of t or vice versa) *)
let equiv_descr ~only_descr t u =
  equiv ~only_descr {descr = t; pred = Pred.True} {descr = u; pred = Pred.True}

let equal_descr ~only_descr t u =
  compare_descr ~only_descr t u = 0

let equal ~only_descr t u =
  equal_descr ~only_descr t.descr u.descr
  && ((=) : Pred.t -> Pred.t -> bool) t.pred u.pred

let rec is_pointer_type t =
  match t.descr with
  | Ptr _ -> true
  | Structure {st_members=[_,_,typ]} -> is_pointer_type typ
  | Name str -> is_pointer_type (type_of_name str)
  | _ -> false

let rec contains_descr t u =
  (* Codex_log.debug "Ctypes.contains %a %a" pp_descr t pp_descr u ;   *)
  (* To see : Subtype or Equal *)
  equiv_descr ~only_descr:true t u
  || match (inlined_descr t) with
  | Void | Base _ | Ptr _ | Enum _ | Function _ -> false
  | Structure {st_members;_} ->
      List.exists (fun (_,_,member) -> contains_descr member.descr u) st_members
  | Array (member,_) -> contains_descr member.descr u
  | Name _ | Application _ -> assert false 
  | Existential (t, _, _) -> contains_descr t.descr u
  | Union {un_types; _} ->
      List.exists (fun (_,typ) -> contains_descr typ.descr u) un_types
  | Weak _ -> assert false

let contains t u =
  contains_descr t.descr u.descr

let get_type_definitions () =
  List.of_seq @@ Hashtbl.to_seq_keys type_map

let print_type_map () = 
  Hashtbl.iter (fun name typ ->
    Format.printf "Result: %s -> %a@." name pp typ
  ) type_map
;;

let print_constr_map () =
  Hashtbl.iter (fun name constr ->
    Format.printf "Result: %s -> %a@." name pp_constr constr
  ) constr_map
;;

let print_function_map () =
  Hashtbl.iter (fun name funct ->
    Format.printf "Result: %s -> %a@." name pp funct
  ) function_map
;;


(* Some builtin types; this depend on the data model. *)
let base_type byte_size name = {descr = Base (byte_size,name); pred = Pred.True}

(* This is the common data model for 32-bit architectures: ints, long
   and pointers are 32bits *)
module ILP32 = struct
  let short = base_type 2 "short";;
  let int = base_type 4 "int";;
  let long = base_type 4 "long";;
  let long_long = base_type 4 "long_long";;
  (* Pointers are 32 bits. *)
end


(* This is the data model common on 64-bits Unix-like environments:
   ints are 32 bits, but longs and pointers are 64bits.
   See: https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models
 *)
module LP64 = struct
  let short = base_type 2 "short";;
  let int = base_type 4 "int";;
  let long = base_type 8 "long";;
  let long_long = base_type 8 "long_long";;
  (* Pointers are 64 bits. *)
end

(* This is the data model for windows and MinGW.
   Only long long and pointers are 64bits.
   See: https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models
 *)
module LLP64 = struct
  let short = base_type 2 "short";;
  let int = base_type 4 "int";;
  let long = base_type 4 "long";;
  let long_long = base_type 8 "long_long";;
  (* Pointers are 64 bits. *)
end

(* Default one; but this should be computed from the code being analyzed. *)
let char = base_type 1 "char";;
let word ~byte_size =
  assert(byte_size > 0);
  base_type byte_size @@ "word" ^ (string_of_int byte_size)
;;

let tuple types = 
  let total_size,members = List.fold_left (fun (offset,types) typ ->
      let size = offset + sizeof typ in
      (size,(offset,"anon",typ) :: types)) (0,[]) types
  in
  let descr = Structure
      { st_members = List.rev members;
        st_byte_size = Some total_size;
      }
  in
  { descr; pred = Pred.True }
;;

(** Additionnal functions used for type domains *)

let concat ~size1 t ~size2 u =
  Codex_log.debug "Ctypes.concat ~size1:%d ~size2:%d of %a and %a" size1 size2 pp t pp u ;
  let pair ~size1 t ~size2 u =
    {descr = Structure {st_byte_size = Some (size1 + size2); st_members = [(0, "anon", t);(size1, "anon", u)]}; pred = Pred.True}
  in
    match t, u with
    | {descr = Structure {st_members = ts_a; _}; pred = Pred.True},
      {descr = Structure {st_members = ts_b; _}; pred = Pred.True} -> 
        let ts = ts_a @ (List.map (fun (ofs, _, typ) -> ofs + size1, "anon", typ) ts_b) in
          {descr = Structure {st_byte_size = Some (size1 + size2); st_members = ts}; pred = Pred.True}
    | _, {descr = Structure {st_members = ts_b; _}; pred = Pred.True} -> 
        let ts = (0, "anon", t)::(List.map (fun (ofs, _, typ) -> ofs + size1, "anon", typ) ts_b) in
          {descr = Structure {st_byte_size = Some (size1 + size2); st_members = ts}; pred = Pred.True}
    | {descr = Structure {st_members = ts_a; _}; pred = Pred.True}, _ ->
        let ts = (List.map (fun (ofs, _, typ) -> ofs, "anon", typ) ts_a) @ [(size1, "anon", u)] in
          {descr = Structure {st_byte_size = Some (size1 + size2); st_members = ts}; pred = Pred.True}
    | _ -> pair ~size1 t ~size2 u

(* let rec extract_from_record ~size ~index ~oldsize {st_byte_size; st_members} =

  let rec filter_end lst = match lst with 
    | [] -> []
    | (i,_,_)::tl when index + size = i -> []
    | (i,n,t)::(j,_,_)::tl when index + size < j ->
      let typ = Option.get @@ extract ~oldsize:(j-i) ~size:(j - index - size) ~index:0 t in
      [(i, n, typ)]
    | x :: tl -> x :: (filter_end tl) in

  let rec filter_start lst = match lst with
    | [] -> []
    | [x] -> lst
    | (i,_,_)::tl when index = i -> filter_end lst
    | (i,n,t)::((j,_,_)::tl as l) when index < j ->
        let typ = Option.get @@ extract ~oldsize:(j-i) ~size:(j-index) ~index:(index-i) t in
        filter_end ((j - index, n, typ)::l)
    | x::tl -> filter_start tl in

    try
      begin
        match filter_start st_members with
        | [] -> None
        | [(ofs,_,typ)]-> Some typ
        | (ofs,name,typ)::tl ->
            let l = List.map (fun (i,f,t) -> (i - ofs, f, t)) tl in 
            Some {descr = Structure {st_byte_size = Some size ; st_members = (0,name,typ)::l}; pred = Pred.True}
      end

    with Invalid_argument _ -> None *)

let rec extract_from_record ~size ~index ~oldsize {st_byte_size; st_members} =

  let rec filter lst = match lst with
  | [] -> []
  | ((i,_,t) as a)::tl when index <= i && i < index + size -> a::(filter tl)
  | _::tl -> filter tl in

    match filter st_members with
    | [] -> None
    | [(ofs,_,typ)]-> Some typ
    | (ofs,name,typ)::tl ->
        let l = List.map (fun (i, f, t) -> (i - ofs, f, t)) tl in 
        Some {descr = Structure {st_byte_size = Some size ; st_members = (0,name,typ)::l}; pred = Pred.True}
  
        
and extract  ~size ~index ~oldsize t = 
  Codex_log.debug "Ctypes.extract ~size:%d ~index:%d ~oldsize:%d %a" size index oldsize pp t ;
  let t = inlined t in
  match t.descr with
  | _ when size = 0 -> None
  | _ when (sizeof t = size) && (index = 0) -> Some t
  | Structure ({st_byte_size = Some sz; _} as s) when (size < sz) && (index < sz) ->
      Option.map (fun typ -> {typ with pred = Pred.conjunction typ.pred t.pred}) (extract_from_record ~size ~index ~oldsize s)
  
  | Array (typ, Some (Const sz)) when (index mod (sizeof typ) = 0) && (size = sizeof typ) -> Some typ 
  
  | Array (typ, Some (Const sz)) when let sz = Z.to_int sz in (size < sz) && (index < sz) && (index mod (sizeof typ) = 0) ->
      Some {t with descr = Array (typ, Some (Const (Z.of_int size)))} 
  | Base (sz, _) when (size < sz) && (index < sz) -> Some (word ~byte_size:size)
  | _ -> None


include ILP32

module type Type_settings = sig
  val root : string * typ
  val special_data_addrs : (int * (typ * int)) list
  val unique_array_types : typ list
  val non_modifiable_types : typ list
  val global_symbols : (string * int * Pred.t) list
end
