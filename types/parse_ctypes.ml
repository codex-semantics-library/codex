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

module Log = Tracelog.Make(struct let category = "Types.Parse_ctypes" end);;

module In_bytes = Units.In_bytes

(* Size that depend on a data model.
   See: https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models.

   Note that alignment constrainst correspond to the size. *)
module type DATA_MODEL = sig
  val short_size     : int
  val int_size       : int
  val long_size      : int
  val long_long_size : int
  val pointer_size   : int
end


(* This is the common data model for 32-bit architectures: everything
   is 32bits except long long. *)
module ILP32:DATA_MODEL = struct
  let short_size = 2
  let int_size = 4
  let long_size = 4
  let long_long_size = 8
  let pointer_size = 4
end

(* This is the data model common on 64-bits Unix-like environments:
   ints are 32 bits, but longs and pointers are 64bits. *)
module LP64:DATA_MODEL = struct
  let short_size = 2
  let int_size = 4
  let long_size = 8
  let long_long_size = 8
  let pointer_size = 8
end


(* This is the data model for windows and MinGW.
   Only long long and pointers are 64bits. *)
module LLP64:DATA_MODEL = struct
  let short_size = 2
  let int_size = 4
  let long_size = 4
  let long_long_size = 8
  let pointer_size = 8
end

let r_data_model : (module DATA_MODEL) option ref = ref None

let the_data_model() = match !r_data_model with
  | None -> Log.fatal (fun p -> p "This module needs to be configured with a data model")
  | Some x -> x              

let short_size()     = let module X = (val (the_data_model ())) in X.short_size
let int_size()       = let module X = (val (the_data_model ())) in X.int_size
let long_size()      = let module X = (val (the_data_model ())) in X.long_size
let long_long_size() = let module X = (val (the_data_model ())) in X.long_long_size
let _pointer_size()   = let module X = (val (the_data_model ())) in X.pointer_size

let char_size = 1
let float_size = 4
let double_size = 8


(* TODO: Add C++-style using a = b to the parser, and make a first pass to discover all the aliases
   (so that they can be expanded here). *)

(* These are GCC's aliases. *)
let basic_aliases_common = 
  [ "int_least8_t", "char"
  ; "int_least16_t", "short"
  ; "int_least32_t", "int"
  ; "int_least64_t", "long long"
  ; "int8_t", "char"
  ; "int16_t", "short"
  ; "int32_t", "int"
  ; "int64_t", "long long"
  ; "uint_least8_t", "char"
  ; "uint_least16_t", "short"
  ; "uint_least32_t", "int"
  ; "uint_least64_t", "long long"
  ; "uint8_t", "char"
  ; "uint16_t", "short"
  ; "uint32_t", "int"
  ; "uint64_t", "long long" ];;
  
let basic_aliases32 = basic_aliases_common @
  [ "int_fast8_t", "char"
  ; "int_fast16_t", "int"
  ; "int_fast32_t", "int"
  ; "int_fast64_t", "long long"
  ; "intptr_t", "int"
  ; "uint_fast8_t", "char"
  ; "uint_fast16_t", "int"
  ; "uint_fast32_t", "int"
  ; "uint_fast64_t", "long long"
  ; "uintptr_t", "int" ];;

let basic_aliases64 = basic_aliases_common @
  [ "int_fast8_t", "char"
  ; "int_fast16_t", "long"
  ; "int_fast32_t", "long"
  ; "int_fast64_t", "long"
  ; "intptr_t", "long"
  ; "uint_fast8_t", "char"
  ; "uint_fast16_t", "long"
  ; "uint_fast32_t", "long"
  ; "uint_fast64_t", "long"
  ; "uintptr_t", "long" ];;


module StringHash = Hashtbl.Make(struct include String;; let hash = Hashtbl.hash end)
let aliases_map = StringHash.create 127;;
let populate_basic_aliases aliases =
  aliases |> List.iter (fun (key,value) -> StringHash.replace aliases_map key value)

let set_data_model = function
  | `ILP32 ->
    r_data_model := Some (module ILP32);
    populate_basic_aliases basic_aliases32
  | `LP64 -> r_data_model := Some (module LP64);
    populate_basic_aliases basic_aliases64    
  | `LLP64 -> r_data_model := Some (module LLP64);
    populate_basic_aliases basic_aliases64        

(* TODO: A pass that warns in case there is some padding (padding should be explicit).
   Perhaps we should also check that unions have the same size? *)


module Conversion = struct

  let pred_nz = TypedC.Pred.(Cmp (NotEqual, Self, Const Z.zero) )
  let infer_spec = ref false
 (* TODO: We should convert each type to a pair:
     - Set of free symbols;
     - Mapping from environment (of these free symbols) to a variable, returning a type.

     (The goal is to reuse types whenever possible, i.e. when they do not contain free variables).

     Substitution is replacement of a free variable. *)


  (* TODO: A pass that checks that we cannot have "f with pred",
     where f is a named type, as the content of a region g. For instance:
     
     type g = struct { ... ; (f with ...) field; ... } is forbidden, but

     type g = \exists x:(f with ...). int is allowed.

     The reason is that we allow casts from g to f, but those are
     unsound if the contents of f is constrained in g. I.e., we would have
     two pointers f* and g* that make incompatible assumption on the contents of the f field.

     
     We could check that all updates to f are compatible with all the
     restrictions in all the regions that derive from f, but it is
     simpler to write the type definition in a way that all these
     restrictions already are in f.

     Note that this includes (int with ...) fields in structures,
     because int* is a type that already exists. Instead, one should
     use word4 with ... or integer with ... because pointers to
     integer and word4 should not exist (only pointer to the enclosing
     struct exists). *)

  (* TODO: A pass that check that "unnamed basic types", like bytes,
     words, and integers, cannot be pointed to. Or conversely, that
     pointers can only point to named types (and byte/word are not). *)
     
     
  (** Type definition map *)
  module ConstrHash = Hashtbl.Make (TypedC.Constr);;
  
  exception Undefined_type_constructor of string

  let type_map : (TypedC.typ * string list) ConstrHash.t = ConstrHash.create 17 ;;
  
  let add_type_definition constr def =
    let constr_map = type_map in
    if ConstrHash.mem constr_map constr then
      let res = ConstrHash.find constr_map constr in
      if res = def then ()
      else
        Log.fatal (fun p ->
            p "There is already a definition for %a" TypedC.Constr.pp constr)
    else ConstrHash.add constr_map constr def
    
  let is_type_definition constr = 
    ConstrHash.mem type_map constr

  let get_type_definition constr = 
    ConstrHash.find_opt type_map constr
    
  let apply_inlining constr args =
    let typ, params = match get_type_definition constr 
    with Some x -> x | None -> raise (Undefined_type_constructor (TypedC.Constr.to_string constr)) in
    let bindings = List.combine params args in 
    TypedC.substitute_in_type typ bindings 

  
  let base name size pred = TypedC.{descr = Base (size,name); pred = pred}
  let convert_unop op = assert false

  let convert_binop op =
    let open Type_parse_tree in
      let open TypedC.Pred in
      match op with
      | Plus -> Add
      | Minus -> Sub
      | Mult -> Mul
      | Bitwise_and -> And
      | Bitwise_or -> Or
      | Mod -> Mod
      | _ -> assert false

  let rec convert_expr expr =
    let open Type_parse_tree in
    match expr with
    | Self -> TypedC.Pred.Self
    | Cst(n) -> TypedC.Pred.(Const n)
    | Var v -> TypedC.Pred.(Val(Sym(v.name)))
    | Unary (op, e) -> TypedC.Pred.Unop(convert_unop op, convert_expr e)
    | Binary(op, e1, e2) ->
      TypedC.Pred.Binop(convert_binop op, convert_expr e1, convert_expr e2)

  let binop_to_cmpop op e1 e2 =
    let open Type_parse_tree in
    match op with
    | Eq -> TypedC.Pred.(Cmp(Equal, convert_expr e1, convert_expr e2))
    | Diff -> TypedC.Pred.(Cmp(NotEqual, convert_expr e1, convert_expr e2))
    | Ge -> TypedC.Pred.(Cmp(SGeq, convert_expr e1, convert_expr e2))
    | Gt -> TypedC.Pred.(Cmp(SGt, convert_expr e1, convert_expr e2))
    | Le -> TypedC.Pred.(Cmp(SLeq, convert_expr e1, convert_expr e2)) (* look here *)
    | Lt -> TypedC.Pred.(Cmp(SLt, convert_expr e1, convert_expr e2))
    | _ -> TypedC.Pred.true_


  let rec convert_predicate pred =
    let open Type_parse_tree in
    match pred with
    | Binary(Logical_and, p1, p2) -> TypedC.Pred.conjunction (convert_predicate p1) (convert_predicate p2)
    | Binary(op, e1, e2) -> binop_to_cmpop op e1 e2
    | _ -> TypedC.Pred.true_

  let convert_type_name = let open Type_parse_tree in function
    | TypeName n -> TypedC.ConstrName n
    | TypeNameUnion n -> TypedC.ConstrNameUnion n
    | TypeNameStruct n -> TypedC.ConstrNameStruct n
    | TypeNameEnum n -> assert false
    (* | TypeNameArray _ -> assert false *)


  (* Create names on demand to ensure that pointers point to names. *)
  let rec provide_name typ : TypedC.name =
    let open TypedC in
    let default() = Log.fatal (fun p -> p "Cannot provide a name for %a" pp typ) in
    match typ.descr with
    | Application {constr;args} ->
      assert(Pred.is_true typ.pred);
      (assert (not @@ is_type_definition constr)) ;
      {constr;args}
    | Array(t,len) -> begin
        let tname = provide_name t in
        let len = match len with
          | Fixed_length z -> Pred.Const z
          | Variable_length l -> Pred.(Val(Sym l))
        in
        match tname with
        | {constr;args = []} ->
          let constr = Constr.make (ConstrNameArray constr.name) 1 in
          let symbol = "array_length_param" in
          add_constr_definition constr (TypedC.({descr= Array(t,Variable_length symbol);
                                                      pred= Pred.true_}), [symbol]);
          {constr; args = [len]}
        | _ -> default()
      end
      
    | Ptr{pointed={descr=Application{constr;args = []}; pred}} when Pred.is_true pred -> begin
        match constr.name with
        (* Convert char** to charstar*.  *)
        | ConstrName ("char" | "double" as name) ->
          if name = "char" then Log.warning (fun p -> p "char* may be ambiguous"); 
          (* TODO: Warn that we probably want a string instead. *)
          let constr = Constr.make (ConstrName (name ^ "star")) 0 in
          add_constr_definition constr (typ,[]);
          {constr;args = []}
        | ConstrNameStruct name -> let constr = Constr.make (ConstrName (name ^ "_s") ) 0 in 
            add_constr_definition constr (typ,[]);
            {constr;args = []}
        | ConstrName _ 
        | ConstrNameFunc _ 
        | ConstrNameUnion _
        | ConstrNameEnum _
        | ConstrNameArray _ -> default ()
      end
    | Void -> begin
        let constr = Constr.make (ConstrName "void") 0 in
        Log.warning (fun p -> p "Should not use void* pointer in spec"); 
        add_constr_definition constr TypedC.({descr=Void;pred=Pred.true_},[]);
        {constr; args = []}
      end
    | Base _
    | Structure _ 
    | StructureFAM _ 
    | Ptr _ 
    | Function _ 
    | Existential _ 
    | Union _ 
    | Weak _ -> default ()


  let rec convert_type_pred typ pred =
    let open Type_parse_tree in
    let open TypedC in
    match typ with
    | Struct fields -> { descr = convert_struct fields; pred = pred; }
    | Name (TypeName n) when StringHash.mem aliases_map n ->
      let rename = StringHash.find aliases_map n in
      convert_type_pred (Name (TypeName rename)) pred
    | Name (TypeName "word8") -> base "word8" (In_bytes.of_int 8) pred
    (* An integer is like an int but it does not alias with int and we
       should never directly take its address, so there is no
       soundness issue when we refine it. *)
    | Name (TypeName "integer") -> base "integer" (In_bytes.of_int @@ int_size()) pred
    | Name (TypeName "word4") -> base "word4" (In_bytes.of_int 4) pred
    | Name (TypeName "word2") -> base "word2" (In_bytes.of_int 2) pred
    | Name (TypeName "word1") -> base "word1" (In_bytes.of_int 1) pred
    | Name (TypeName "void") -> {descr = Void; pred = Pred.true_}
    | Name n -> 
      let constr = Constr.make (convert_type_name n) 0 in 
      if is_type_definition constr then 
        let typ = apply_inlining constr [] in 
        {typ with pred = TypedC.Pred.conjunction typ.pred pred}
      else {descr = Application{constr = constr; args = []}; pred = pred}
    
    | Pointer(t,ptr_annot) ->
      let name = provide_name @@ convert_type t in
      let typ = Build.ptr_to_name name in
      let pred = match ptr_annot with
        | Maybe_null (* p* *) -> 
        (*in a simple codex run, we make no differences between p? and p*,
          if we try to infer spec, we lower to a mutable non null*)
            if !infer_spec then
            (Pred.conjunction (Pred.init_mutval pred_nz) pred) else pred 
        | Non_null (* p+ *)-> (Pred.conjunction pred_nz pred)
        | Null_or_non_null (* p? *) -> pred in
              Build.refine typ pred
    | Array(t,e) -> 
      let elem_typ = convert_type t in
      begin match e with
      | Cst len -> {descr = Array (elem_typ, Fixed_length len); pred = pred}
      | Var symb -> {descr = Array (elem_typ,Variable_length symb.name); pred = pred}
      | _ ->
          let expr = convert_predicate e in Log.debug (fun p -> p "invalid array size expression : %a" Pred.pp expr);
          assert false (* An array length should be given by a predicate expression *)
      end

    | Applied(LambdaAlias cons, exprs) -> 
      let args = List.map convert_expr exprs in
      let constr = Constr.make (convert_type_name cons) (List.length args) in 
      if is_type_definition constr then 
        let typ = apply_inlining constr args in 
        {typ with pred = TypedC.Pred.conjunction typ.pred pred}
      else {descr = Application{constr; args}; pred=pred }

    | Exists (v, tv, t) ->
        let bound_typ = convert_type tv in
      {descr = Existential {bound_typ;bound_var = v.name;body = convert_type t} ; pred = pred}

    | Union types ->
      let un_byte_size, un_types = convert_union_types types in
      { descr = TypedC.Union
            { un_byte_size;
              un_types
            };
        pred = pred;
      }
    
    | Function (ret,args) -> 
      let args = List.map convert_type args in
      let ret = convert_type ret in
      { descr = Function {ret; args; pure = false}; pred=pred }

    | Constraint (t,e) ->
        convert_type_pred t (Pred.conjunction (convert_predicate e) pred)

  and convert_type typ = convert_type_pred typ TypedC.Pred.true_

  (* and convert_fields fs =
    let total_size, l = List.fold_left (fun (offset,acc) (name,typ) ->
        let typ = convert_type typ in
        let byte_size = TypedC.sizeof typ in
        let field = (offset, name, typ) in
        (offset + byte_size, field::acc)
      ) (0,[]) fs
    in total_size, List.rev l
  *)

  and convert_struct fs =
    let exception Return of TypedC.descr in
    let make_struct st_byte_size fields =
      TypedC.Structure { st_byte_size; st_members = List.rev fields }
    in
    try 
      let total_size, l = List.fold_left (fun (ofs,acc) (name,typ) ->
          let typ = convert_type typ in
          try 
            let byte_size = TypedC.sizeof typ in
            let field = (ofs, name, typ) in
            In_bytes.(ofs + byte_size), field::acc
          with TypedC.Unsizeable_type ->
            let structure = TypedC.{descr=make_struct ofs acc;pred=Pred.true_} in
            raise (Return (TypedC.StructureFAM {structure;array=typ}))
        ) (In_bytes.zero, []) fs
      in make_struct total_size l
    with Return t -> t
      

  (* and convert_union_types ts =
    let name, t = List.hd ts in
    let typ = convert_type t in
    let size = TypedC.sizeof typ in
    let total_size, l = List.fold_left (fun (size,acc) (name,typ) ->
        let typ = convert_type typ in
        let byte_size = TypedC.sizeof typ in
        assert (byte_size = size) ;
        (byte_size, (name, typ)::acc)
      ) (size,[(name, typ)]) (List.tl ts)
    in total_size, List.rev l
  *)
  and convert_union_types ts : In_bytes.t option * (string * TypedC.typ) list =
    let name, t = List.hd ts in
    let typ = convert_type t in
    let sizeof_opt = (fun t -> try Some (TypedC.sizeof t) with TypedC.Unsizeable_type -> None) in
    let size = sizeof_opt typ in
    let total_size, l = List.fold_left (fun (size,acc) (name,typ) ->
        let typ = convert_type typ in
        let sz = sizeof_opt typ in

        let new_size = match size, sz with
          | Some s1, Some s2 when s1 = s2 -> Some s1
          | _ -> None
        in
        
        (new_size, (name, typ)::acc)
      ) (size,[(name, typ)]) (List.tl ts)
    in total_size, List.rev l

  and convert_constr_def =
    let open Type_parse_tree in
    function Lambda (vars, typ) -> (convert_type typ, vars)

  let convert_function_type typ pure =
    let open Type_parse_tree in
    let open TypedC in
    match typ with
    | Exists (v, tv, t) ->
      let bound_typ = convert_type tv in
      {descr = Existential {bound_typ;bound_var=v.name;body = convert_type t} ; pred = Pred.true_}
    | Function(ret,args) -> 
      let args = List.map convert_type args in
      let ret = convert_type ret in
      { descr = Function {ret; args; pure}; pred = Pred.true_ }

    | _ -> assert false

  (* We work around circularity, at least for pointers, by using names as types.
    However, if used as a value, a structure should be defined before it is
    referenced in the definition of another type
  *)
  let convert_definition =
    let open Type_parse_tree in
    function
    | RegionDefinition(name,type_def) ->
      let name = convert_type_name name in
      begin match type_def with
        | Type t ->
          let constr = (TypedC.Constr.make name 0) in
          TypedC.add_constr_definition constr (convert_type t,[])
        | Constr c ->
          let def = (convert_constr_def c) in
          let arity = List.length @@ snd def in
          let constr = TypedC.Constr.make name arity in
          TypedC.add_constr_definition constr def
      | _ -> assert false
      end
    | TypeDefinition(name,type_def) -> 
      let name = convert_type_name name in
      begin match type_def with
        | Type t ->
          let constr = (TypedC.Constr.make name 0) in
          add_type_definition constr (convert_type t,[])
        | Constr c ->
          let def = (convert_constr_def c) in
          let arity = List.length @@ snd def in
          let constr = TypedC.Constr.make name arity in
          TypedC.add_constr_definition constr def
      | _ -> assert false
      end
    | GlobalDefinition(name,globdef) -> begin match globdef with 
        | FunDef {inline; pure; funtyp=t} ->
          TypedC.add_function_name_definition name (convert_function_type t pure) inline      
        | Global t -> TypedC.add_global_name_definition name (convert_type t)
        | _ -> assert false
      end
  ;;

end


let parse_file ~infer_spec name =
  let parse_tree = Type_parser.(parse_file name annotations) in
  Conversion.infer_spec := infer_spec;
  (* We ensure type expression definitions which are meant to be inlined are 
    obtained first as they may be used during the region name definitions *)
  let type_definitions, region_definitions = 
    List.partition (fun def -> 
      match def with Type_parse_tree.TypeDefinition _ -> true | _ -> false) parse_tree in 
  List.iter Conversion.convert_definition type_definitions;
  List.iter Conversion.convert_definition region_definitions;
  (*TypedC.print_spec ()*)
  ()

(** Parses a string into [TypedC.typ]
    @raise [Failure] if the string is not a valid type *)
let type_of_string str =
  let parse_tree = Type_parser.(parse_string str typeexpr) in
  Conversion.convert_type parse_tree

open Conversion
let init ~data_model =
  assert(!r_data_model = None); (* Not already initialized. *)
  set_data_model data_model;
  


  (* Note that (C standard 3096, \S 6.7.8) "typedef declaration does not introduce a new type",
     so stdint pointers should only be seen as aliases.

     TODO: We should have alias declarations, maybe using the C++ syntax "using name = type".
     (And possibly also also some C typedefs). *)
  let base_types =
    [ "char", char_size      (* TODO: char* should be considered specially. *)
    ; "short", short_size()
    ; "int", int_size()
    ; "long",long_size()
    ; "long long", long_long_size()
    ; "intmax_t", long_long_size() (* C23 7.22.1.5: they are new types. *)
    ; "uintmax_t", long_long_size()        
    ; "float", float_size
    ; "double", double_size
    ] in
  base_types |> List.iter (fun (name,size) ->
      let size = In_bytes.of_int size in
      let constr = TypedC.Constr.make (TypedC.ConstrName name) 0 in
      TypedC.add_constr_definition constr ((base name size TypedC.Pred.true_),[]))
