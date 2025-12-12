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

(** A parser for Codex annotations. For documentation of the concrete syntax,
    see {!Type_parse_tree}.

    The grammar may be ambiguous;
    rather than arbitrarily choose a parse tree, we prefer to detect
    ambiguities and to ask the user to specify its intention. *)

(*
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   WHEN MODIFYING THIS FILE, PLEASE ALSO UPDATE THE RELEVANT REFERENCE COMMENT
   IN type_parse_tree.ml
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*)

module Log = Tracelog.Make(struct let category = "Type_parser" end);;


module Ast = Type_parse_tree

(** Comparing operators by precedence may be surprising, especially as
   in C the [&] or [|] operators bind less strongly than [==].

   To avoid any mistake, we allow only obvious precedences:

   - multiplicative operators ([*], [/] and [%]) bind more strongly than
     additive operators ([+] and [-]);
   - both bind less strongly than binary predicates ([==], [<], ...);
   - binary predicates bind less strongly than [&&] and [||];
   - multiplicative and additive operators are left-associative.
   - we report an error otherwise.

    We don't allow chaining binary predicates, e.g.
    [a < b == c], [a < b > c], or [a || b && c] is forbidden.

    This technique is inspired by Per Vognsen, who pointed it out in
    {: https://www.scattered-thoughts.net/writing/better-operator-precedence/}
*)
module Binop = struct

  type precedence =
    | LeftBindsTighter
    | RightBindsTighter
    | Ambiguous


  let is_additive = let open Ast in function
    | Plus | Minus -> true
    | _ -> false

  let is_multiplicative = let open Ast in function
    | Mult | Mod | Div -> true
    | _ -> false

  let is_predicate = let open Ast in function
      | Gt | Ge | Lt | Le | Eq | Diff -> true
      | _ -> false
  ;;

  let is_logical = let open Ast in function
      | Logical_and | Logical_or -> true
      | _ -> false
  ;;

  (* The bitwise priority is very unintuitive in C, so we leave it as
     ambiguous. *)
  (* let is_bitwise = let open Ast in function *)
  (*   | Bitwise_and | Bitwise_or -> true *)
  (*   | _ -> false *)


  (* Given (x a y b z), should the result be:
     - (x a y) b z (LeftBindsTighter)
     - x a (y b z) (RightBindsTighter)
     - Unclear and we report an error. *)
  let compare_precedence a b =
    if is_multiplicative a then begin
      if is_multiplicative b || is_additive b || is_predicate b || is_logical b then LeftBindsTighter
      else Ambiguous
    end
    else if is_additive a then begin
      if is_multiplicative b then RightBindsTighter
      else if is_additive b || is_predicate b || is_logical b then LeftBindsTighter
      else Ambiguous
    end
    else if is_predicate a then begin
      if is_logical b then LeftBindsTighter
      else if is_additive b || is_multiplicative b then RightBindsTighter
      else Ambiguous            (* We dont allow pairs of predicates either. *)
    end
    else if is_logical a then begin
      if a == b then LeftBindsTighter (* Allow a && b && c, a || b || c, but do not mix. *)
      else if is_additive b || is_multiplicative b || is_predicate b then RightBindsTighter
      else Ambiguous
    end
    else Ambiguous
  ;;

  (* An expression, or a list of expressions whose associativity is
     yet to be found. *)
  type expr_list =
    | SingleExp of Ast.expr
    | BinaryExp of Ast.expr * Ast.binop * expr_list


  (* Algorithm: if we have a op1 b op2 tl.
     - If op1 < op2: rewrite in (a op1 b) op2 tl
     - If op1 > op2:
        - If tl is a singleton c: rewrite in a op1 (b op2 c)
        - If tl is c op3 d: rewrite in a op1 (b op2 c) op3 d.  *)

  let disambiguate l =
    let rec loop a op1 = function
      | SingleExp b -> Ast.Binary(op1,a,b)
      | BinaryExp(b,op2,tl) -> begin
          match compare_precedence op1 op2 with
          | LeftBindsTighter -> loop (Ast.Binary(op1,a,b)) op2 tl
          | RightBindsTighter -> begin
              match tl with
              | SingleExp c -> Ast.Binary(op1,a,Ast.Binary(op2,b,c))
              | BinaryExp(c,op3,d) -> loop a op1 (BinaryExp(Ast.Binary(op2,b,c),op3,d))
            end
          | Ambiguous ->
            let error_msg =
              (Format.asprintf "Error: Ambiguous precedence between the %s and %s operators. \
 Use parentheses to disambiguate."
                 (Ast.Pretty.binop op1) (Ast.Pretty.binop op2))
            in raise (Pacomb.Lex.Give_up error_msg)
        end
    in
    match l with
    | SingleExp e -> e
    | BinaryExp(e1,op1,tl) -> loop e1 op1 tl

end

(* We use merge function to report parsing ambiguities sooner than
   when computing all the parse trees (which provides better error
   messages, and computing all the parse trees can be very slow if
   there are lots of them).  We still do computing all the parse
   trees, and then report error using [parse_all_buffer], which
   ensures that we will catch all errors. *)
let report_ambiguities name pretty e1 e2 =
  Log.error (fun p -> p "Found ambiguous %s that can be parsed either as:@ @[<hv>%a@ or@ %a@].
Please disambiguate using parentheses." name
                pretty e1 pretty e2);
  failwith ("Ambiguous " ^ name ^ " detected")
;;

let merge_typeexpr = report_ambiguities "type expression" Ast.Pretty.typ;;
let merge_expr = report_ambiguities "expression" Ast.Pretty.expr;;
let merge_definition = report_ambiguities "definition" Ast.Pretty.def;;
;;

let id_regexp = "[a-zA-Z_][a-zA-Z0-9_]*"
let fun_id_regexp = "[a-zA-Z_][a-zA-Z0-9_@]*"

(* PPX documentation: see https://www.raffalli.eu/pacomb/pacomb/index.html *)
let %parser rec
  annotations =
  (l::(~* def)) EOF => l
  (* (l::def) EOF => [l] *)

(* The file is a sequence of definitions. *)
and [@merge merge_definition] def =
  (x::type_definition) ~? ';' => Ast.TypeDefinition x
; (x::region_name_definition) ~? ';' => Ast.RegionDefinition x
; (x::c_function_declaration) ~? ';' => Ast.GlobalDefinition x
; (x::c_type_definition) ~? ';'  => Ast.RegionDefinition x
; (x::c_global_declaration) ~? ';' => Ast.GlobalDefinition x

(* Comma-separated list of varid. *)
and varids = (args::(~+ [","] varid)) => args

(*  Definition of a new region name. *)
and region_name_definition =
  ("region" => ()) (name::typeid) '=' (e::typeexpr) =>  (name,Ast.Type e)
; ("region" => ()) (name::typeid) '=' '<' (vars::varids) '>' (t::typeexpr) =>
  (Log.warning (fun p -> p "The syntax `type name = <args> type' is deprecated.
 Use `region name(args) = type' instead.");
                      (name, Ast.(Constr(Lambda(vars,t)))))
; ("region" => ()) (name::typeid) '(' (vars::varids) ')' '='  (t::typeexpr) =>
  (name, Ast.(Constr(Lambda(vars,t))))
and ptr_annot =
  '*' => Ast.Maybe_null;
  '+' => Ast.Non_null;
  '?' => Ast.Null_or_non_null

and type_definition =
  ("type" => ()) (name::typeid) '=' (e::typeexpr) =>  (name,Ast.Type e)
; ("type" => ()) (name::typeid) '=' '<' (vars::varids) '>' (t::typeexpr) =>
  (Log.warning (fun p -> p "The syntax `type name = <args> type' is deprecated.
 Use `type name(args) = type' instead.");
                      (name, Ast.(Constr(Lambda(vars,t)))))
; ("type" => ()) (name::typeid) '(' (vars::varids) ')' '='  (t::typeexpr) =>
  (name, Ast.(Constr(Lambda(vars,t))))

(* Type expressions, as can appear at the right of type ... = <type expression>. *)
and [@merge merge_typeexpr] typeexpr =
  (x::typeid) => Ast.Name x
; (x::typeid) '(' (args::exprs) ')' => Ast.(Applied(LambdaAlias x,args))
; '(' (e::typeexpr) ')' => e
; (e::typeexpr) (a::ptr_annot) => Ast.Pointer(e,a)
; (e::typeexpr) '[' (v::expr) ']' => Ast.Array(e,v)
; (e::struct_constructor) => e
; (e::union_constructor) => e
; (e::typeexpr) "with" (p::predicate) => Ast.Constraint(e,p)
; ("∃" => (); "\\exists" => ()) (v::varid) ':' (t::typeexpr) '.' (t2::typeexpr)   => Ast.(Exists(Symbol.intern v,t,t2))
(* Should this be the same as c_function_parameters? for which naming the arugmets could be optional? *)
; '[' (args::(~* [","] typeexpr)) ']' "->" (ret::typeexpr) => Ast.Function(ret,args)
(* Looks more like C. *)
; (ret::typeexpr) "<-" '(' (args::(~* [","] typeexpr)) ')'  => Ast.Function(ret,args)
(* Classical function-pointer syntax in C.  *)
; (ret::typeexpr) '(' (a::ptr_annot) ')' '(' (args::(~* [","] typeexpr)) ')'  => (Ast.Pointer(Ast.Function(ret,args),a))

(* Comma-separated list of type expressions. *)
and exprs = (es::(~+ [","] expr)) => es

and id = (x::RE id_regexp) => x

and varid_fun = (x::RE fun_id_regexp) => x

(* Name of C or symbolic variables *)
and [@warning "-39"] varid = id

(* Usable ids for types. "struct foo" and "union foo" are valid names, like in C.
   But as in C, struct and union cannot be used as type names, as this
   creates too many ambiguous parses. *)
and typeid =
  (x::id) =>
    if (x = "struct" || x = "union")
    then raise (Pacomb.Lex.Give_up "Cannot use struct or union as type identifiers.")
    else Ast.TypeName x
; "struct" (x::id) => Ast.TypeNameStruct x
; "union" (x::id) => Ast.TypeNameUnion x

(* Names of fields in structures and unions.  *)
and fieldname_id = id
and cfield = (e::typeexpr) (name::fieldname_id) ';' => (name,e)

(* Anonymous struct and union typexprs constructor. *)
and struct_constructor = "struct" '{' (l::(~* cfield)) '}' => Ast.Struct l
and union_constructor = "union" '{' (l::(~* cfield)) '}' => Ast.Union l


and inline_keyword =
  "inline" => true
; () => false

and pure_keyword =
  "pure" => true
; () => false

and c_function_expression =
  (ret_type::typeexpr) (funname::varid_fun) '(' (args::c_function_parameters) ')' => (funname, Ast.Function(ret_type,args))
; ("∃" => (); "\\exists" => ()) (v::varid) ':' (t::typeexpr) '.' ((funname,f)::c_function_expression) => (funname, Ast.(Exists(Symbol.intern v,t,f)))
; '(' (e::c_function_expression) ')' => e

and c_function_declaration =
  (inline::inline_keyword) (pure::pure_keyword) ((funname,f)::c_function_expression)  => (funname,Ast.FunDef({inline;pure;funtyp=f}))

and c_function_parameters =
  "void" => []                  (* (void) = no parameter. *)
; l::(~* [","] ( (e::typeexpr) (n::varid) => e)) => l

(* Shorthand for "type struct foo = struct { ... }". *)
and c_type_definition =
  "struct" (name::id) '{' (l::(~* cfield)) '}'  => (Ast.TypeNameStruct name,Ast.Type (Ast.Struct l))
; "union" (name::id) '{' (l::(~* cfield)) '}'  =>  (Ast.TypeNameUnion name,Ast.Type (Ast.Union l))

and c_global_declaration =
  (t::typeexpr) (name::varid) => (name,Ast.Global t);

(* TODO: Allow ULL etc. For hex, uses the number of digits to deduce the size? *)
and num_cst =
  (x::RE "[0-9]+") => (Z.of_string x)

and binop =
  '+' => Ast.Plus
; '-' => Ast.Minus
; '*' => Ast.Mult
; '/' => Ast.Div
; '%' => Ast.Mod
; '|' => Ast.Bitwise_or
; '&' => Ast.Bitwise_and
; '^' => Ast.Bitwise_xor
; "<<" => Ast.LShift
; ">>" => Ast.RShift
; ">=" => Ast.Ge
; '>' => Ast.Gt
; "<=" => Ast.Le
; '<' => Ast.Lt
; '=' => Ast.Eq
; "==" => Ast.Eq
; "!=" => Ast.Diff
; "&&" => Ast.Logical_and
; "||" => Ast.Logical_or

(* Expr priorities: atom < unary < binary. *)
and atom_expr =
  '(' (e::expr) ')' => e
; (x::varid) => if x = "self" then Ast.Self else Ast.(Var (Symbol.intern x))
; (x::num_cst) => Ast.Cst x

and unary_expr =
  (e::atom_expr) => e

(* We make sure that we have a single parse tree when using binary
   operators; disambiguate is done in the [disambiguate] semantic
   action. *)
and binary_expr =
  (e::unary_expr) => Binop.SingleExp e
; (e::unary_expr) (op::binop) (tl::binary_expr) => Binop.BinaryExp(e,op,tl)

and [@merge merge_expr] expr =
  (e::binary_expr) => (Binop.disambiguate e)

and predicate = (e::expr) => e

(* TODO: Allow #define u typeexpr and #define u expr. *)

;;
open Pacomb

(* Hard-coded state machine to lex C comments and other blanks out. *)
module MyBlank = struct

  let rec my_blank buf idx =
    let char,new_buf,new_idx = Input.read buf idx in
    match char with
    | ' ' | '\t' | '\n' | '\r' -> my_blank new_buf new_idx
    | '/' -> begin
        let char,new_buf,new_idx = Input.read new_buf new_idx in
        match char with
        | '*' -> comment new_buf new_idx
        | '/' -> kill_end_of_line new_buf new_idx
        | _ -> (buf,idx)          (* Not a comment. *)
      end
    | _ -> (buf,idx)

  and kill_end_of_line buf idx =
    let char,new_buf,new_idx = Input.read buf idx in
    match char with
    | '\n' -> my_blank new_buf new_idx
    | _ -> kill_end_of_line new_buf new_idx

  and comment buf idx =
    let char,new_buf,new_idx = Input.read buf idx in
    match char with
    | '\255' -> eof_in_comment()
    | '*' -> comment_after_star new_buf new_idx
    | _ -> comment new_buf new_idx

  and comment_after_star buf idx =
    let char,new_buf,new_idx = Input.read buf idx in
    match char with
    | '\255' -> eof_in_comment()
    | '*' -> comment_after_star new_buf new_idx
    | '/' -> my_blank new_buf new_idx
    | _ -> comment new_buf new_idx

  and eof_in_comment() = raise (Lex.Give_up "Reached EOF while parsing a comment starting with /*. ")
  ;;
end

(* A pair of a parser and printer. *)
type 'a grammar =
  'a Pacomb.Grammar.grammar
  * (Format.formatter -> 'a -> unit);;

let annotations = (annotations,Ast.Pretty.annotations);;
let typeexpr = (typeexpr,Ast.Pretty.typ);;

let parse input (grammar,printer) =
  let f input grammar =
    let res = Pacomb.Grammar.parse_all_buffer grammar MyBlank.my_blank input in
    match res with
    | [] -> assert false
    | [res] ->
      Log.info (fun p -> p "Parsed types file as:@.%a@." printer res);
      res
    | _ ->
      let dump_parse_trees fmt =
        res |> List.iter (fun parse_tree ->
            Format.printf "One parse tree is:@.%a@." printer parse_tree)
      in
      Log.error (fun p -> p "Ambiguous parsing: please resolve.@.%t@." dump_parse_trees);
      failwith "Ambiguous parse"
  in
  (* [Pos] module provides a function to handle exception with
     an optional argument to call for error (default is to exit with
     code 1 *)
  Pos.handle_exception ~error:(fun _ -> exit 1) (f input) grammar
;;


let parse_file filename grammar =
  try
    let input = Input.from_file ~utf8:Utf8.ASCII filename in
    parse input grammar
  with
  | Unix.Unix_error(Unix.ENOENT, "open", _) ->
    Log.fatal (fun p -> p "Cannot open `%s': no such file or directory" filename)
  | e -> raise e
;;

let parse_string string grammar =
  let input = Input.from_string ~utf8:Utf8.ASCII string in
  parse input grammar
