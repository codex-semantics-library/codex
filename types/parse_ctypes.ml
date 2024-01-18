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

module Conversion = struct

  module Size = struct
    let pointer_size = 4;;
    let char_size = 1;;
    let short_size = 2;;
    let int_size = 4;;
    let long_size = 8;;

    let float_size = 4;;
    let double_size = 8;;

  end

  let pred_nz = Ctypes.Pred.(Cmp (NotEqual, Self, Val (Ctypes.Const Z.zero)));;

  let base name size pred = Ctypes.{descr = Base (size,name); pred = pred}
  let convert_unop op = assert false

  let convert_binop op =
    let open Type_parse_tree in
      let open Ctypes.Pred in
      match op with
      | Plus -> Add
      | Minus -> Sub
      | Mult -> Mul
      | And -> And
      | Or -> Or
      | _ -> assert false

  let rec convert_expr expr =
    let open Type_parse_tree in
    match expr with
    | Self -> Ctypes.Pred.Self
    | Cst(n) -> Ctypes.Pred.(Val(Const(n)))
    | Var v -> Ctypes.Pred.(Val(Sym(v)))
    | Unary (op, e) -> Ctypes.Pred.Unop(convert_unop op, convert_expr e)
    | Binary(op, e1, e2) ->
      Ctypes.Pred.Binop(convert_binop op, convert_expr e1, convert_expr e2)

  let binop_to_cmpop op e1 e2 =
    let open Type_parse_tree in
    match op with
    | Eq -> Ctypes.Pred.(Cmp(Equal, convert_expr e1, convert_expr e2))
    | Diff -> Ctypes.Pred.(Cmp(NotEqual, convert_expr e1, convert_expr e2))
    | Ge -> Ctypes.Pred.(Cmp(SGeq, convert_expr e1, convert_expr e2))
    | Gt -> Ctypes.Pred.(Cmp(SGt, convert_expr e1, convert_expr e2))
    | Le -> Ctypes.Pred.(Cmp(SLeq, convert_expr e1, convert_expr e2)) (* look here *)
    | Lt -> Ctypes.Pred.(Cmp(SLt, convert_expr e1, convert_expr e2))
    | _ -> True


  let rec convert_predicate pred =
    let open Type_parse_tree in
    match pred with
    | Binary(And, p1, p2) -> Ctypes.Pred.conjunction (convert_predicate p1) (convert_predicate p2)
    | Binary(op, e1, e2) -> binop_to_cmpop op e1 e2
    | _ -> Ctypes.Pred.True

  let rec convert_type_pred typ pred =
    let open Type_parse_tree in
    let open Ctypes in
    match typ with
    | Struct fields ->
      let st_byte_size, st_members = convert_fields fields in
      { descr = Structure
            { st_byte_size;
              st_members
            };
        pred = pred;
      }
    | Alias "word" -> base "short" Size.short_size pred
    | Alias "long" -> base "long" Size.long_size pred
    | Alias "int" -> base "int" Size.int_size pred
    | Alias "short" -> base "short" Size.short_size pred
    | Alias "char" -> base "char" Size.char_size pred
    | Alias "double" -> base "double" Size.double_size pred
    | Alias "float" -> base "float" Size.float_size pred
    | Alias "void" -> {descr = Void; pred = Pred.True}
    | Alias n -> {descr = Name(n); pred = pred}
    | Pointer(t,Maybe_null) ->
      let pointed = convert_type t in
      { descr = Ptr {pointed; index=Zero}; pred=pred}
    | Pointer(t,Non_null) ->
      let pointed = convert_type t in
      { descr = Ptr {pointed; index=Zero}; pred=(Pred.conjunction pred pred_nz)}
    | Array(t,e) -> 
      let elem_typ = convert_type t in
      begin match e with
      | Cst len -> {descr = Array (elem_typ, Some(Const len)); pred = pred}
      | Var symb -> {descr = Array (elem_typ, Some(Sym symb)); pred = pred}
      | _ ->
          let expr = convert_predicate e in Codex_log.debug "invalid array size expression : %a" Pred.pp expr ;
          assert false (* An array length should be given by a predicate expression *)
      end

    | Applied(cons, exprs) ->
      let args = List.map convert_expr exprs in
        {descr = Application(convert_constr cons, args); pred=pred }

    | Exists (v, tv, t) ->
        let te = convert_type tv in
        {descr = Existential (convert_type t, v, te); pred = pred}

    | Union types ->
      let un_byte_size, un_types = convert_union_types types in
      { descr = Ctypes.Union
            { un_byte_size;
              un_types
            };
        pred = pred;
      }
    
    | Function(ret,args) -> 
      let args = List.map convert_type args in
      let ret = convert_type ret in
      { descr = Function(ret, args); pred=pred }

    | Constraint (t,e) ->
        convert_type_pred t (Pred.conjunction pred (convert_predicate e))

  and convert_type typ = convert_type_pred typ Ctypes.Pred.True

  (* and convert_fields fs =
    let total_size, l = List.fold_left (fun (offset,acc) (name,typ) ->
        let typ = convert_type typ in
        let byte_size = Ctypes.sizeof typ in
        let field = (offset, name, typ) in
        (offset + byte_size, field::acc)
      ) (0,[]) fs
    in total_size, List.rev l
  *)

  and convert_fields fs =
    let total_size, l = List.fold_left (fun (offset,acc) (name,typ) ->
        let typ = convert_type typ in
        try 
          let byte_size = Ctypes.sizeof typ in
          match offset with
          | None -> 
            let field = (-1, name, typ) in
            (None, field::acc)
          | Some ofs ->
            let field = (ofs, name, typ) in
            (Some (ofs + byte_size), field::acc)
            
        with Ctypes.Unsizeable_type -> (
          match offset with
          | None -> 
            let field = (-1, name, typ) in
            (None, field::acc)
          | Some ofs ->
            let field = (ofs, name, typ) in
            (None, field::acc)
        )
      ) (Some 0, []) fs
    in total_size, List.rev l

  (* and convert_union_types ts =
    let name, t = List.hd ts in
    let typ = convert_type t in
    let size = Ctypes.sizeof typ in
    let total_size, l = List.fold_left (fun (size,acc) (name,typ) ->
        let typ = convert_type typ in
        let byte_size = Ctypes.sizeof typ in
        assert (byte_size = size) ;
        (byte_size, (name, typ)::acc)
      ) (size,[(name, typ)]) (List.tl ts)
    in total_size, List.rev l
  *)
  and convert_union_types ts : int option * (string * Ctypes.typ) list =
    let name, t = List.hd ts in
    let typ = convert_type t in
    let sizeof_opt = (fun t -> try Some (Ctypes.sizeof t) with Ctypes.Unsizeable_type -> None) in
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

  and convert_constr cons =
    let open Type_parse_tree in
    let open Ctypes in
    match cons with
    | Lambda (vars, typ) -> Constructor(convert_type typ, vars)
    | LambdaAlias n -> ConstrName n

  (* We work around circularity, at least for pointers, by using names as types.
    However, if used as a value, a structure should be defined before it is
    referenced in the definition of another type
  *)
  let convert_definition (name,type_def) =
    let open Type_parse_tree in
    match type_def with
    | Type (Function _ as f) -> Ctypes.add_function_name_definition name (convert_type f) 
    | Type t -> Ctypes.add_type_name_definition name (convert_type t)
    | Constr c -> Ctypes.add_constr_name_definition name (convert_constr c)
  ;;

end


let parse_file name =
  let file = Stdlib.open_in name in
  let lexbuf = Lexing.from_channel file in
  let parse_tree =
    try Type_parser.annotations_eof Type_lexer.token lexbuf
    with Type_parser.Error ->
      let start_p = Lexing.lexeme_start_p lexbuf in
      let end_p = Lexing.lexeme_end_p lexbuf in
      let err = Format.asprintf "File `%s', line %d, column %d-%d: parsing error" name start_p.pos_lnum (start_p.pos_cnum - start_p.pos_bol) (end_p.pos_cnum - end_p.pos_bol) in
      failwith err
  in
  List.iter Conversion.convert_definition parse_tree;
  Ctypes.print_type_map () ;
  Ctypes.print_constr_map () ;
  Ctypes.print_function_map () ;
  ()
;;

(** Parses a string into [Ctypes.typ]
    @raise [Failure] if the string is not a valid type *)
let type_of_string str =
  let lexbuf = Lexing.from_string str in
  let parse_tree =
    try Type_parser.core_eof Type_lexer.token lexbuf
    with Type_parser.Error ->
      raise (Failure (Format.sprintf "type parsing error: '%s' is not a valid type" str))
  in
  Conversion.convert_type parse_tree
;;
            
