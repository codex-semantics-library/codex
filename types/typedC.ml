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

module Log = Tracelog.Make (struct
  let category = "Types.TypedC"
end)

module Int_option = Datatype_sig.Option (Datatype_sig.Int)
module String_option = Datatype_sig.Option (Datatype_sig.String)

module In_bits = Units.In_bits
module In_bytes = Units.In_bytes

module SymbolMap = Datatype_sig.StringMap
(** String to type map (parameter to expression) *)

type value_symbol = string
type value = Sym of value_symbol

(* We treat arrays length specially, at least for now. *)
type array_length = Fixed_length of Z.t | Variable_length of value_symbol

let pp_value fmt =
  let open Format in
  function Sym s -> fprintf fmt "\"%s\"" s

let pp_array_length fmt =
  let open Format in
  function
  | Fixed_length x -> Z.pp_print fmt x
  | Variable_length s -> fprintf fmt "\"%s\"" s

module Pred = struct
  type unop = Extract of In_bits.t * In_bits.t  (** start index, length *)

  let pp_unop op pp_expr fmt expr =
    match op with
    | Extract (idx, len) ->
        Format.fprintf fmt "%a{%d, %d}" pp_expr expr (idx:>int) ((idx:>int) + (len:>int) - 1)

  type binop =
    | Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Mod
    | Concat of In_bits.t * In_bits.t
        (** Size 1, size 2. First argument is the most significant *)

  let pp_binop fmt op =
    Format.pp_print_string fmt
    @@
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | And -> "&"
    | Or -> "|"
    | Mod -> "%"
    | Concat (size1, size2) -> Format.sprintf "::<%d,%d>" (size1:>int) (size2:>int)

  type expr =
    | Const of Z.t
    | Val of value
    | Self
    | Unop of unop * expr
    | Binop of binop * expr * expr

  type cmpop =
    | Equal
    | NotEqual
    | ULt
    | SLt
    | ULeq
    | SLeq
    | UGeq
    | SGeq
    | UGt
    | SGt

  let pp_cmpop fmt op =
    Format.pp_print_string fmt
    @@
    match op with
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

  type mutval = { id : int; mutable value : bool }


  (** Predicate on a structure field or array element, which is supposed to be
      true at all times. *)
  type t =
    | True
    | Cmp of cmpop * expr * expr
    | And of t * t
    | Mutval of mutval * t

  let rec is_true = function
    | True | Mutval ({ value = false }, _) -> true
    | Mutval (_, p) -> is_true p
    | _ -> false

  let true_ = True

  let evaluate_mutval { value } p = match value with false -> True | true -> p

  (**initialize a mutval wich will make the associated value true*)
  let init_mutval t =
    let counter =
      let x = ref 0 in
      fun () ->
        incr x;
        !x
    in
    Mutval ({ id = counter (); value = true }, t)

  let equal t u = compare t u = 0

  let rec pp_expr fmt =
    let open Format in
    function
    | Const x -> Z.pp_print fmt x
    | Val value -> pp_value fmt value
    | Self -> pp_print_string fmt "self"
    | Unop (op, e) -> fprintf fmt "@[<hov 2>%a@]" (pp_unop op pp_expr) e
    | Binop (op, e1, e2) ->
        fprintf fmt "@[<hov 2>(%a@ %a@ %a)@]" pp_expr e1 pp_binop op pp_expr e2

  let rec pp fmt =
    let open Format in
    function
    | True -> pp_print_string fmt "true"
    | Cmp (cop, e1, e2) ->
        fprintf fmt "@[<hov 2>(%a@ %a@ %a)@]" pp_expr e1 pp_cmpop cop pp_expr e2
    | And (p1, p2) -> fprintf fmt "@[<hov 2>%a@ &&@ %a@]" pp p1 pp p2
    | Mutval (mutval, p) -> pp fmt (evaluate_mutval mutval p)

  let array_length_to_expr = function
    | Fixed_length z -> Const z
    | Variable_length s -> Val (Sym s)

  let _lift_unop : unop -> Z.t -> Z.t = function
    | Extract (idx, len) -> fun x -> Z.extract x (idx:>int) (len:>int)

  let _lift_binop : binop -> Z.t -> Z.t -> Z.t = function
    | Add -> Z.add
    | Sub -> Z.sub
    | Mul -> Z.mul
    | Div -> Z.div
    | And -> Z.logand
    | Or -> Z.logor
    | Concat (_, size2) -> fun x y -> Z.logor (Z.shift_left x (size2:>int)) y
    | Mod -> Z.rem

  let _lift_cmpop : cmpop -> Z.t -> Z.t -> bool = function
    | Equal -> Z.equal
    | NotEqual -> fun a b -> not (Z.equal a b)
    | ULt -> Z.lt
    | ULeq -> Z.leq
    | UGeq -> Z.geq
    | UGt -> Z.gt
    | SLt -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) < 0
    | SLeq -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) <= 0
    | SGeq -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) >= 0
    | SGt -> fun x y -> Int32.compare (Z.to_int32 x) (Z.to_int32 y) > 0

  let conjunction pred1 pred2 =
    match (pred1, pred2) with
    | _ when pred1 = pred2 -> pred1
    | _ when is_true pred1 -> pred2
    | _ when is_true pred2 -> pred1
    | _ -> And (pred1, pred2)

  let rec substitutes_expr expr env =
    match expr with
    | Val (Sym a) when SymbolMap.mem a env -> SymbolMap.find a env
    (* | Val _ | Self -> pe *)
    | Unop (op, expr) -> Unop (op, substitutes_expr expr env)
    | Binop (op, expr1, expr2) ->
        Binop (op, substitutes_expr expr1 env, substitutes_expr expr2 env)
    | _ -> expr

  let rec substitutes pred env =
    match pred with
    | True -> pred
    | Cmp (op, e1, e2) ->
        Cmp (op, substitutes_expr e1 env, substitutes_expr e2 env)
    | And (pred1, pred2) -> And (substitutes pred1 env, substitutes pred2 env)
    | Mutval (mut, p) -> Mutval (mut, substitutes p env)
  (*if mut.value then substitutes p env else pred*)
end

type basic = In_bytes.t * string
(** Byte size and name. *)

type structure = {
  st_byte_size : In_bytes.t;
  st_members : (In_bytes.t * string * typ) list;
      (** Offset, field name, type, and predicate on type. *)
}

and union = {
  un_byte_size : In_bytes.t option; (* Size of the union *)
  un_types : (string * typ) list; (* typename, and type of the field *)
}

and descr =
  | Void (* TODO: REmove, and have flexible array member instead. *)
  | Base of basic
  | Structure of structure
  | StructureFAM of { structure : typ; array : typ }
  | Ptr of pointer
      (** Pointed type and index of pointed element in array. For non-array
          pointed types, the index should be [ConstIdx 0]. *)
  | Array of typ * array_length
  (** Element type and number of elements (if statically known). *)
  | Function of funtyp (** Return type and parameter types *)
  | Application of name
  (*∃ bound_var : bound_typ, body*)
  | Existential of {bound_var:string;bound_typ:typ;body:typ}
  | Union of union
  | Weak of typ

and constr_name =
  | ConstrName of string
  | ConstrNameFunc of string
  | ConstrNameUnion of string
  | ConstrNameStruct of string
  | ConstrNameEnum of string
  | ConstrNameArray of constr_name (* Arity 1. Printed as name[a]* instead of name[](a)* *)
  (* Maybe:ConstrNameStar *)

(** TODO: Should be pointing to a name only.  *)
and pointer = { pointed : typ }

and name = { constr : constr; args : Pred.expr list }

and constr = { id : int; name : constr_name; arity : int }

and constr_def =
  typ * string list (* Function from a list of arguments to a type. *)

and funtyp = { ret : typ; args : typ list; pure : bool }
and typ = { mutable descr : descr; mutable pred : Pred.t }

type fundef = { funtyp : typ; inline : bool }

exception Undefined_type_constructor of string

module StringHash = Hashtbl.Make (struct
  include String

  let hash = Hashtbl.hash
end)

let rec pp_constr_name fmt = function
  | ConstrName str -> Format.fprintf fmt "%s" str
  | ConstrNameFunc str -> Format.fprintf fmt "function %s" str
  | ConstrNameUnion str -> Format.fprintf fmt "union %s" str
  | ConstrNameStruct str -> Format.fprintf fmt "struct %s" str
  | ConstrNameEnum str -> Format.fprintf fmt "enum %s" str
  | ConstrNameArray cn -> Format.fprintf fmt "%a[]" pp_constr_name cn

(***************************************************************************************)
(***************************************************************************************)
(*Below are utilies manipulating the ctypes storing, which is done in an imperative way*)
(***************************************************************************************)
(***************************************************************************************)

module Constr = struct
  type t = constr

  let count = ref 0
  let tbl = Hashtbl.create 17

  let make name arity =
    match Hashtbl.find tbl name with
    | exception Not_found ->
        incr count;
        let res = { id = !count; name; arity } in
        Hashtbl.replace tbl name res;
        res
    | x when x.arity = arity -> x
    | x ->
        Log.fatal (fun p ->
            p "Two uses of the same name %a with different arities (%d and %d)"
              pp_constr_name name x.arity arity)

  let hash x = x.id
  let equal = ( == )
  let compare a b = Int.compare a.id b.id
  let pp fmt x = pp_constr_name fmt x.name
  let to_string x = Format.asprintf "%a" pp x
end

module ConstrHash = Hashtbl.Make (Constr)

type ctype_spec = {
  constr_map : constr_def ConstrHash.t;
  function_map : (string, fundef) Hashtbl.t;
  global_map : (string, typ) Hashtbl.t;
}

let ctype_spec =
  {
    constr_map = ConstrHash.create 17;
    function_map = Hashtbl.create 17;
    global_map = Hashtbl.create 17;
  }

let add_constr_definition constr def =
  let constr_map = ctype_spec.constr_map in
  if ConstrHash.mem constr_map constr then
    let res = ConstrHash.find constr_map constr in
    if res = def then ()
    else
      Log.fatal (fun p ->
          p "There is already a definition for %a" Constr.pp constr)
  else ConstrHash.add constr_map constr def

let constr_of_name constr =
  ConstrHash.find_opt ctype_spec.constr_map constr

let add_function_name_definition name funtyp inline =
  let constr = Constr.make (ConstrNameFunc name) 0 in
  add_constr_definition constr (funtyp,[]);
  let funtyp = {descr=Application{constr;args = []};pred=Pred.true_ } in
  Hashtbl.replace ctype_spec.function_map name {funtyp; inline};;

let function_of_name name =
  match Hashtbl.find ctype_spec.function_map name with
  | {funtyp} -> Some funtyp
  | exception Not_found -> None


(* TODO: Should return a name. *)
let function_definition_of_name name =
  Hashtbl.find_opt ctype_spec.function_map name

let add_global_name_definition name typ =
  Hashtbl.replace ctype_spec.global_map name typ

let global_of_name name = Hashtbl.find_opt ctype_spec.global_map name

let get_type_definitions () =
  ConstrHash.to_seq ctype_spec.constr_map
  |> Seq.filter_map (function
       | name, (_, []) -> Some (Constr.to_string name)
       | _ -> None)
  |> List.of_seq

(*type storing is done*)

let expr_to_symbol expr =
  let open Pred in
  match expr with
  | Val (Sym a) -> Variable_length a
  | Const z -> Fixed_length z
  | _ -> failwith "Expression is not a simply symbol"

let pp_string fmt s = Format.fprintf fmt "\"%s\"" s

(* Prints a type's description as an OCaml value.  *)
let rec pp_descr_name precedence fmt descr =
  let open Format in
  match descr with
  | Void -> pp_print_string fmt "void"
  | Base (sz, name) -> fprintf fmt "@[<hov 2>%s(%d)@]" name (sz:>int)
  | Ptr { pointed = typ } ->
      (* | Ptr {pointed = typ; index = Zero} -> *)
      fprintf fmt "@[<hov 2>%a*@]" (pp_type_name precedence) typ
  (* | Ptr {pointed = typ; index} ->
    fprintf fmt "@[<hov 2>%a* at %a@]" (pp_type_name precedence) typ pp_index index *)
  | Structure s ->
      fprintf fmt
        "@[<hov 2>struct {@,\
         st_byte_size =@ %d;@ st_members =@ @[<v 2>[@ %a@ ]@];@ }@]"
        (s.st_byte_size:>int)
        (pp_print_list (fun fmt (offset, name, typ) ->
             fprintf fmt "(@[<hov 2>%d,@ %a,@ %a@]);" (In_bytes.to_int offset) pp_string name
               (pp_type_name 0) typ))
        s.st_members
  | StructureFAM { structure; array } ->
      fprintf fmt "@[<hov 2>structfam { prefix = %a; array = %a@]"
        (pp_type_name 1) structure (pp_type_name 1) array
  | Array (typ, size) ->
    (if precedence > 1 then
       fprintf fmt "@[<hov 2>(%a[%a])@]"
     else
       fprintf fmt "@[<hov 2>%a[%a]@]") (pp_type_name 1) typ pp_array_length size
  | Function {ret; args} ->
    (if precedence > 0 then
       fprintf fmt "@[<hov 2>(Function (%a, [%a]))@]"
     else
       fprintf fmt "@[<hov 2>Function (%a, [%a])@]")
      (pp_type_name 0) ret
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_type_name 0)) args

  | Application name-> pp_name fmt name
  | Existential {bound_typ;bound_var;body}->
    fprintf fmt "@[<hov 2>∃%s : %a.%a@]" bound_var (pp_type_name precedence) bound_typ (pp_type_name precedence) body

  | Union u ->
      fprintf fmt
        "@[<hov 2>union {@,\
         un_byte_size =@ %a;@ un_types =@ @[<v 2>[@ %a@ ]@];@ }@]"
        Int_option.pretty (u.un_byte_size |> Option.map In_bytes.to_int)
        (pp_print_list (fun fmt (name, typ) ->
             fprintf fmt "(@[<hov 2>%a,@ %a@]);" pp_string name (pp_type_name 0)
               typ))
        u.un_types
  | Weak typ -> fprintf fmt "@[<hov 2>Weak %a@]" (pp_type_name precedence) typ

and pp_constr_name precedence fmt constr =
  let open Format in
  match constr with n -> fprintf fmt "@[<hov 2>%s@]" (Constr.to_string n)

and pp_constr_def fmt (t, params) =
  let open Format in
  fprintf fmt "@[<hov 2>(<%a> %a)@]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_print_string)
    params (pp_type_name 0) t

and pp_type_name precedence fmt typ =
  let open Format in
  (if precedence > 0 then
     fprintf fmt "@[<hov 2>(%a%a)@]" (pp_descr_name 0) typ.descr
   else fprintf fmt "@[<hov 2>%a%a@]" (pp_descr_name precedence) typ.descr)
    (fun fmt p ->
      if not @@ Pred.is_true p then fprintf fmt "@ with %a" Pred.pp p else ())
    typ.pred


and pp_name fmt {constr;args} =
  let open Format in
  match args with
  |[] -> fprintf fmt "Name(@[<hov 2>%s@])" (Format.asprintf "%a" Constr.pp constr)
  | args ->
    fprintf fmt "@[<hov 2>%a(%a)@]"
      (pp_constr_name 0) constr
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") Pred.pp_expr) args


(* Print the type's name. *)
let pp fmt typ = pp_type_name 0 fmt typ
let pp_constr fmt constr = pp_constr_name 0 fmt constr

let print_constr_map fmt () =
  ConstrHash.iter
    (fun name constr ->
      Format.fprintf fmt "%s %a@." (Constr.to_string name) pp_constr_def constr)
    ctype_spec.constr_map

let print_function_map fmt () =
  Hashtbl.iter
    (fun name { funtyp; inline } -> Format.fprintf fmt "%s %a@." name pp funtyp)
    ctype_spec.function_map

let print_global_map fmt () =
  Hashtbl.iter
    (fun name global -> Format.printf "Result: %s -> %a@." name pp global)
    ctype_spec.global_map

let rec substitute_in_type typ env =
  Log.trace (fun p -> p "substitute_in_type %a" pp typ) ~pp_ret:pp @@ fun () ->
  let pred = Pred.substitutes typ.pred env in
  match typ.descr with
  | Void | Base _ | Application {args = []} -> {typ with pred = pred}
  | Structure s ->
      let members =
        List.map
          (fun (ofs, name, typ) -> (ofs, name, substitute_in_type typ env))
          s.st_members
      in
      { descr = Structure { s with st_members = members }; pred }
  | StructureFAM { structure; array } ->
      {descr =
          StructureFAM
            {
              structure = substitute_in_type structure env;
              array = substitute_in_type array env;
            };
        pred;
      }
  | Ptr { pointed } ->
      (* does not substitutes further than pointers *)
      { descr = Ptr { pointed = substitute_in_type pointed env }; pred }
  | Array (elem_typ, Variable_length s) when SymbolMap.mem s env ->
      let sz_value = expr_to_symbol (SymbolMap.find s env) in
      { descr = Array (substitute_in_type elem_typ env, sz_value); pred }
  | Array (elem_typ, sz) -> { descr = Array (substitute_in_type elem_typ env, sz); pred }
  | Function funtyp ->
      let args = List.map (fun t -> substitute_in_type t env) funtyp.args in
      let ret = substitute_in_type funtyp.ret env in
      {descr = Function {funtyp with ret; args}; pred = pred}
  | Application {constr; args} ->
    let args = List.map (fun t -> Pred.substitutes_expr t env) args in
    {descr = Application {constr; args}; pred = pred}

  | Existential {bound_typ;bound_var;body} ->
    {descr = Existential {bound_typ = substitute_in_type bound_typ env; bound_var;body =  substitute_in_type body env}; pred }

  | Union u ->
      let types =
        List.map
          (fun (name, typ) -> (name, substitute_in_type typ env))
          u.un_types
      in
      { descr = Union { u with un_types = types }; pred }
  | Weak _ -> assert false

let substitute_in_type typ bindings =
  let env = SymbolMap.of_seq @@ List.to_seq bindings in
  substitute_in_type typ env

(* Apply constructor to args, but also perform substitution. *)
let apply constr args =
  Log.trace
    (fun p ->
      p "apply %a %a" Constr.pp constr (Format.pp_print_list Pred.pp_expr) args)
    ~pp_ret:pp
  @@ fun () ->
  let typ, params = match constr_of_name constr
  with Some x -> x | None -> raise (Undefined_type_constructor (Constr.to_string constr)) in
  let bindings = List.combine params args in
  substitute_in_type typ bindings

let substitute_symbol typ prev symb =
  Log.trace (fun p -> p "substitute_symbol %s by %s" prev symb) ~pp_ret:pp
  @@ fun () ->
  substitute_in_type typ [(prev, Pred.(Val (Sym symb)))]

let rec inlined typ =
  Log.trace (fun p -> p "inlined %a" pp typ) ~pp_ret:pp @@ fun () ->
  match typ.descr with
  | Application {constr; args} ->
    let res = apply constr args in
      inlined {res with pred = Pred.conjunction typ.pred res.pred}
  | _ -> typ

let inlined_descr descr =
  match descr with
  (* unify cases*)
  | Application {constr; args = []} ->
    let t = match constr_of_name constr with
      | Some (t,_) -> t
      | None -> raise (Undefined_type_constructor (Constr.to_string constr)) in
    (inlined t).descr
  | Application {constr; args} -> (inlined (apply constr args)).descr
  | _ -> descr

let compare_list cmp_elt l1 l2 =
  let rec cmp = function
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: t1, h2 :: t2 ->
        let c = cmp_elt h1 h2 in
        if c = 0 then cmp (t1, t2) else c
  in
  cmp (l1, l2)

let rec compare_struct s1 s2 =
  let c = Stdlib.compare s1.st_byte_size s2.st_byte_size in
  if c = 0 then
    compare_list
      (fun (i1, _, t1) (i2, _, t2) -> compare t1 t2)
      s1.st_members s2.st_members
  else c

and compare_union u1 u2 =
  let c = Stdlib.compare u1.un_byte_size u2.un_byte_size in
  if c = 0 then
    compare_list (fun (_, t1) (_, t2) -> compare t1 t2) u1.un_types u2.un_types
  else c

and compare_descr x y =
  (* let open Format in
    fprintf Format.std_formatter "comparing types %a to %a\n" pp_descr x pp_descr y ; *)
  match (x, y) with
  | Void, Void -> 0
  | Void, _ -> -1
  | _, Void -> 1
  | Base sz1, Base sz2 -> Stdlib.compare sz1 sz2
  | Base _, _ -> -1
  | _, Base _ -> 1
  | Structure s1, Structure s2 -> compare_struct s1 s2
  | Structure _, _ -> -1
  | _, Structure _ -> 1
  | StructureFAM s1, StructureFAM s2 ->
      let c = compare s1.structure s2.structure in
      if c = 0 then compare s1.array s2.array else c
  | StructureFAM _, _ -> -1
  | _, StructureFAM _ -> 1
  | Ptr { pointed = t }, Ptr { pointed = u } ->
      let c = compare t u in
      if (*only_descr*) false || c <> 0 then c else 0
  | Ptr _, _ -> -1
  | _, Ptr _ -> 1
  | Array (t, l), Array (t', l') ->
      let c = (Stdlib.compare : array_length -> array_length -> int) l l' in
      if c <> 0 then c else compare t t'
  | Array _, _ -> -1
  | _, Array _ -> 1
  | Function {ret=t; args=ts}, Function {ret=u; args=us} ->
    compare_list compare (t :: ts) (u :: us)
  | Application {constr = c1; args = a1}, Application {constr = c2; args = a2} ->
    let c = compare_constr c1 c2 in
      if c <> 0 then c
      else compare_list (Stdlib.compare : Pred.expr -> Pred.expr -> int) a1 a2
  | Application _, _ -> -1
  | _, Application _ -> 1
  | Existential {bound_typ=e1;bound_var=v1;body=i1}, Existential {bound_typ=e2;bound_var=v2;body=i2} ->
      let c = Stdlib.compare v1 v2 in
      if c <> 0 then c
      else
        (* let c = compare tv1 tv2 in
        if c <> 0 then c
        else *)
        compare i1 i2
  | Existential _, _ -> -1
  | _, Existential _ -> 1
  | Union u1, Union u2 -> compare_union u1 u2
  | Union _, _ -> -1
  | _, Union _ -> 1
  | Weak t1, Weak t2 -> compare t1 t2
  | Weak _, _ -> -1
  | _, Weak _ -> 1

and compare_constr a b = match (a, b) with n1, n2 -> Constr.compare n1 n2

and compare t u =
  if (* only_descr *) false then compare_descr t.descr u.descr
  else
    let c1 = compare_descr t.descr u.descr in
    if c1 <> 0 then c1
    else (Stdlib.compare : Pred.t -> Pred.t -> int) t.pred u.pred

exception Unsizeable_type

(* Size in bytes *)
let rec sizeof_descr descr =
  (* function *)
  match inlined_descr descr with
  | Base (sz, _) | Structure { st_byte_size = sz; _ } -> sz
  | Ptr _ -> Units.In_bits.(Codex_config.ptr_size () |> in_bytes)
  | Array (elem_t, Fixed_length sz) ->
    In_bytes.times (Z.to_int sz) (sizeof_descr elem_t.descr)
      (* | Void -> 4 *)
      (* TODO : check if this is valid or if a new type should be used behind pointers *)
  | Void ->
      raise (Failure "The generic type \"void\" is currently not supported")
  | Array (_, _) | Function _ | StructureFAM _ ->
      (* Not handled. *)
      raise Unsizeable_type
      (* raise (Invalid_argument "type_size") *)
  | Application _ -> assert false
  | Existential {body} -> sizeof_descr body.descr
  | Union { un_byte_size = Some sz; _ } -> sz
  | Union { un_byte_size = None; _ } ->
      raise Unsizeable_type (* raise (Invalid_argument "type_size") *)
  | Weak t -> sizeof_descr t.descr

let sizeof x = sizeof_descr x.descr

let rec equiv x y =
  match (x.descr, y.descr) with
  | Void, Void -> true
  | Base sz1, Base sz2 -> sz1 = sz2

  | Structure s1, Structure s2 -> (compare_struct s1 s2) = 0

  | Ptr {pointed=t; }, Ptr {pointed=u} ->
    equiv t u

  | Array (t1,l1), Array (t2, l2) ->
    (l1 = l2) && (equiv t1 t2)

  | Function {ret=t; args=ts}, Function {ret=u; args=us} ->
    compare_list (fun x y -> if equiv x y then 0 else 1) (t :: ts) (u :: us) = 0

  | Application {constr = c1;args = args1},
    Application {constr = c2;args = args2} when Constr.equal c1 c2 ->
     (compare_list (fun x y -> (Stdlib.compare : Pred.expr -> Pred.expr -> int) x y) args1 args2) = 0

  | Application {constr;args}, _ ->
    let t = apply constr args in
      equiv {descr = t.descr; pred = Pred.conjunction t.pred x.pred} y
  | _, Application {constr;args} ->
    let t = apply constr args in
      equiv x {descr = t.descr; pred = Pred.conjunction t.pred y.pred}

  | Weak t1, Weak t2 -> equiv t1 t2
  | _ -> false

(* checks that u is equivalent of t (if u is a subtype of t or vice versa) *)
let equiv_descr t u =
  equiv { descr = t; pred = Pred.true_ } { descr = u; pred = Pred.true_ }

let equal_descr t u = compare_descr t u = 0

let equal t u =
  equal_descr t.descr u.descr
  && (( = ) : Pred.t -> Pred.t -> bool) t.pred u.pred

let rec is_flexible_array t =
  let t = inlined t in
  match t.descr with
  | StructureFAM _ -> true
  | Existential {body} -> is_flexible_array body
  | Weak { descr = Array (_, Variable_length _) } -> true
  | Weak _ -> false
  | _ -> false

let rec is_function_pure ft =
  match ft.descr with
  | Existential {body} -> is_function_pure body
  | Function { pure } -> pure
  | _ -> false

let rec contains_descr t u =
  (* Codex_log.debug "TypedC.contains %a %a" pp_descr t pp_descr u ;   *)
  (* To see : Subtype or Equal *)
  equiv_descr t u
  ||
  match inlined_descr t with
  | Void | Base _ | Ptr _ | Function _ -> false
  | Structure { st_members; _ } ->
      List.exists
        (fun (_, _, member) -> contains_descr member.descr u)
        st_members
  | Array (member, _) -> contains_descr member.descr u
  | StructureFAM { structure; array } ->
      contains_descr structure.descr u || contains_descr array.descr u
  | Application _ -> assert false
  | Existential {body} -> contains_descr body.descr u
  | Union { un_types; _ } ->
      List.exists (fun (_, typ) -> contains_descr typ.descr u) un_types
  | Weak _ -> assert false

let contains t u = contains_descr t.descr u.descr

let get_function_names () =
  List.of_seq @@ Hashtbl.to_seq_keys ctype_spec.function_map

(* Some builtin types; this depend on the data model. *)
let base_type byte_size name =
  { descr = Base (byte_size, name); pred = Pred.true_ }

let word ~(byte_size:In_bytes.t) =
  assert In_bytes.(zero < byte_size);
  base_type byte_size @@ "word" ^ string_of_int (byte_size:>int)

let tuple types =
  let total_size, members =
    List.fold_left
      (fun (offset, types) typ ->
        let size = In_bytes.(offset + sizeof typ) in
        (size, (offset, "anon", typ) :: types))
      (In_bytes.zero, []) types
  in
  let descr =
    Structure { st_members = List.rev members; st_byte_size = total_size }
  in
  { descr; pred = Pred.true_ }

(** Additionnal functions used for type domains *)

let _concat ~(size1:In_bytes.t) ~(size2:In_bytes.t) t u =
  Log.debug (fun p ->
      p "TypedC.concat ~size1:%d ~size2:%d of %a and %a" (size1:>int) (size2:>int) pp t pp u);
  let pair ~size1 t ~size2 u =
    {
      descr =
        Structure
          {
            st_byte_size = In_bytes.(size1 + size2);
            st_members = [ (In_bytes.zero, "anon", t); (size1, "anon", u) ];
          };
      pred = Pred.true_;
    }
  in
  match (t, u) with
  | ( { descr = (Structure { st_members = ts_a; _ }); pred = pred1 },
      { descr = Structure { st_members = ts_b; _ }; pred = pred2} )
      when Pred.is_true pred1 && Pred.is_true pred2 ->
      let ts =
        ts_a @ List.map (fun (ofs, _, typ) -> (In_bytes.(ofs + size1), "anon", typ)) ts_b
      in
      {
        descr = Structure { st_byte_size = In_bytes.(size1 + size2); st_members = ts };
        pred = Pred.true_;
      }
  | _, { descr = Structure { st_members = ts_b; _ }; pred } when Pred.is_true pred ->
      let ts =
        (In_bytes.zero, "anon", t)
        :: List.map (fun (ofs, _, typ) -> (In_bytes.(ofs + size1), "anon", typ)) ts_b
      in
      {
        descr = Structure { st_byte_size = In_bytes.(size1 + size2); st_members = ts };
        pred = Pred.true_;
      }
  | { descr = Structure { st_members = ts_a; _ }; pred }, _ when Pred.is_true pred ->
      let ts =
        List.map (fun (ofs, _, typ) -> (ofs, "anon", typ)) ts_a
        @ [ (size1, "anon", u) ]
      in
      {
        descr = Structure { st_byte_size = In_bytes.(size1 + size2); st_members = ts };
        pred = Pred.true_;
      }
  | _ -> pair ~size1 t ~size2 u

(* let rec extract_from_record ~size ~index ~oldsize { st_byte_size; st_members } = *)
(*   let rec filter lst = *)
(*     match lst with *)
(*     | [] -> [] *)
(*     | ((i, _, t) as a) :: tl when In_bytes.(index <= i && i < index + size) -> *)
(*         a :: filter tl *)
(*     | _ :: tl -> filter tl *)
(*   in *)

(*   match filter st_members with *)
(*   | [] -> None *)
(*   | [ (ofs, _, typ) ] -> Some typ *)
(*   | (ofs, name, typ) :: tl -> *)
(*       let l = List.map (fun (i, f, t) -> (In_bytes.(i - ofs), f, t)) tl in *)
(*       Some *)
(*         { *)
(*           descr = *)
(*             Structure { st_byte_size = size; st_members = (In_bytes.zero, name, typ) :: l }; *)
(*           pred = Pred.true_; *)
(*         } *)

(* and extract ~(size:In_bits.t) ~(index:In_bits.t) ~(oldsize:In_bits.t) t = *)
(*   Log.debug (fun p -> *)
(*       p "TypedC.extract ~size:%d ~index:%d ~oldsize:%d %a"  *)
(*         (size:>int) (index:>int) (oldsize:>int) pp *)
(*         t); *)
(*   let t = inlined t in *)
(*   match t.descr with *)
(*   | _ when size = In_bits.zero -> None *)
(*   | _ when sizeof t = size && index = In_bits.zero -> Some t *)
(*   | Structure ({ st_byte_size = sz; _ } as s) when size < sz && index < sz -> *)
(*       Option.map *)
(*         (fun typ -> { typ with pred = Pred.conjunction typ.pred t.pred }) *)
(*         (extract_from_record ~size ~index ~oldsize s) *)
(*   | Array (typ, Fixed_length sz) *)
(*     when index mod sizeof typ = 0 && size = sizeof typ -> *)
(*       Some typ *)
(*   | Array (typ, Fixed_length sz) *)
(*     when let sz = Z.to_int sz in *)
(*          size < sz && index < sz && index mod sizeof typ = 0 -> *)
(*       Some { t with descr = Array (typ, Fixed_length (Z.of_int size)) } *)
(*   | Base (sz, _) when size < sz && index < sz -> Some (word ~byte_size:size) *)
(*   | _ -> None *)

module Build = struct
  (* TODO: pred should always be != 0, as possibly-null pointers
     should be made by union with zero. *)
  (* TODO: Two builders: one for non-null, one for null pointers. *)
  (* TODO: Ensure that pointed is a name.  *)
  (* Next step: function pointers as heap pointers. *)
  let ptr pointed pred =
    (* We should be pointing to a name. *)
    (match pointed.pred with Pred.True -> ()|_ -> assert false);
    if not ((match pointed.descr with (Application _ | Weak _ ) -> true | _ -> false))
    then
      Log.fatal (fun p -> p "Creating a pointer to %a" pp pointed);
    {descr = Ptr {pointed}; pred}

  let ptr_to_name {constr;args} =
    let pointed = {descr = Application{constr;args}; pred = Pred.true_ } in
    {descr = Ptr {pointed}; pred = Pred.true_}

  let refine typ pred =
    { descr = typ.descr; pred = Pred.conjunction pred typ.pred }
end

module ParsablePrinter = struct
  (*dirty hack*)
  let get_string_of_pointer_pred pred =
    let rec aux =
      let open Pred in
      function
      | Cmp (NotEqual, Self, Const zero) -> Some "+"
      | Mutval ({ value = true }, _) -> Some "*"
      | And (t1, t2) ->
          Option.bind (aux t1) (fun default ->
              Some (Option.value (aux t2) ~default))
      | _ -> None
    in
    Option.value (aux pred) ~default:"?"

  let rec pp fmt { descr; pred } =
    let open Format in
    match descr with
    | Void -> fprintf fmt "void"
    | Structure { st_members } ->
          (pp_print_list
            ~pp_sep:(fun p () -> fprintf p "@;")
             (fun p (_, field, typ) -> fprintf p "%a %s;" pp typ field)) fmt
          st_members
    | Base (_,name) -> fprintf fmt "%s" name
    | StructureFAM _ -> assert false
    (* TODO: clarify this, we never print p*, which implies that we cant "save" a postfixpoint iteration*)
    | Ptr { pointed } ->
        fprintf fmt "%a%s" pp pointed (get_string_of_pointer_pred pred)
    (*| Ptr { pointed; maybe = false } -> fprintf fmt "%a%s" pp pointed (if is_not_null pred then "+" else "?")*)
    | Array (typ, length) -> fprintf fmt "%a[%a]" pp typ pp_array_length length
    | Application {constr} -> fprintf fmt "%s" (Constr.to_string constr)
    | Existential {bound_var;bound_typ;body} -> fprintf fmt "∃ %s : %a.%a"  bound_var pp bound_typ pp body
    | Union { un_byte_size; un_types } ->
      fprintf fmt "union @[<v>{@;%a@;}@]"
          (pp_print_list
             ~pp_sep:(fun p () -> fprintf p "@;")
             (fun p (n, t) -> fprintf p "%a %s;" pp t n))
          un_types
    | Weak _ -> assert false
    | Function {ret;args} -> fprintf fmt "[%a]" (pp_print_list pp) (ret::args)

  let pp_constr_name_def fmt (name, (typ, l)) =
    let open Format in
    match (name.name, typ) with
    | ConstrName name, _ -> fprintf fmt "type %s = %a;@." name pp typ
    | ConstrNameFunc f, { descr = Function { ret; args } } ->
        fprintf fmt "%a %s(%a);@." pp ret f
          (pp_print_list
             ~pp_sep:(fun p () -> fprintf p ",")
             (fun p (i, t) -> fprintf p "%a x_%i" pp t (i + 1)))
          (List.mapi (fun i t -> (i, t)) args)
    | ConstrNameUnion _, _ -> fprintf fmt "todo union@."
    | ConstrNameStruct s, typ -> fprintf fmt "@.@[<v 2>struct %s {@;%a@]@;};@." s pp typ
    | ConstrNameEnum _, _ -> fprintf fmt "todo enum@."
    | ConstrNameArray _, _ -> fprintf fmt "todo array@."
    | ConstrNameFunc _, t -> fprintf fmt "%a@." pp t


end

let base_types =
  [
    "char";
    "short";
    "int";
    "long";
    "long long";
    "intmax_t";
    "uintmax_t";
    "float";
    "double";
  ]

let print_spec fmt () =
  ConstrHash.iter
    (fun name constr_def ->
      if List.mem (Constr.to_string name) base_types then ()
      else
        Format.fprintf fmt "%a" ParsablePrinter.pp_constr_name_def
          (name, constr_def))
    ctype_spec.constr_map
