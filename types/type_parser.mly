/**************************************************************************/
/*  This file is part of the Codex semantics library.                     */
/*                                                                        */
/*  Copyright (C) 2013-2024                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file LICENSE).                      */
/*                                                                        */
/**************************************************************************/

%{
    open Type_parse_tree
%}

%token EOF

%token PLUS MINUS DIV MOD
%token EQUAL DIFF LE LT GE GT
%token NOT AND OR XOR SL SR
%token LAND LOR

%token STAR SEMICOLON QUESTION_MARK LPAR RPAR LBRACKET RBRACKET LBRACE RBRACE
%token COMMA DOT EXISTS COLON QUOTE

%token TYPE STRUCT UNION WITH SELF DEF

%token <string> ID
%token <Z.t> INT
%token ARROW

%left LOR
%left LAND
%nonassoc EQUAL DIFF LE LT GE GT
%left SL SR
%left PLUS MINUS
%left STAR DIV MOD AND OR XOR
%nonassoc NOT

%start <(string * t) list> annotations_eof
%start <typ> core_eof

%%

let annotations_eof := ~=annotations; EOF; <>

let core_eof := ~=core; EOF; <>

let annotations := ~=list(def); <>

let def :=
  | ~=c_function_declaration; <>
  | def_or_type; n=ID; EQUAL; cons=constr; { (n, Constr cons) }
  | ~=type_definition; <>
  | ~=cDef; {cDef}

%inline c_function_declaration :
   | ret=core; n=c_function_id; LPAR; args=separated_list(COMMA, fun_field); RPAR; SEMICOLON; { (n, Type (Function (ret, args))) }

let c_function_id := ~=ID; <>

%inline def_or_type:
  | DEF   {}
  | TYPE  {}

%inline type_definition :
  | def_or_type; tn=type_name; EQUAL; typ=core; { (tn, Type typ) }
      
%inline type_name :
  | n=ID; {n} 
  | STRUCT; n=ID; { "struct " ^ n }
  | UNION; n=ID; { "union " ^ n }

(* C-style definitions of structure and unions. *)
let cDef :=
  | STRUCT; n=ID; LBRACE; fields=nonempty_list(c_field); RBRACE; SEMICOLON; { ("struct " ^ n, Type (Struct fields)) }
  | UNION; n=ID; LBRACE; fields=nonempty_list(c_field); RBRACE; SEMICOLON; { ("union " ^ n, Type (Union fields)) }

let ocaml_field := name=ID; COLON; ~=core; { name, core} 
let c_field := ~=core; name=ID; SEMICOLON; { name, core }
let fun_field := ~=core; ID; { core }

(* Pointer and array types. *)
let postfix_typ :=
  | core=core; QUESTION_MARK; { Pointer (core, Maybe_null) }
  | core=core; STAR; { Pointer (core, Maybe_null) }
  | core=core; PLUS; { Pointer (core, Non_null) }
  | core=core; LBRACKET; ~=expr; RBRACKET; { Array (core, expr) }
      

let core :=
  | ~=type_name; <Alias>
  | ~=postfix_typ; <>
  | ~=core; WITH; ~=predicate; <Constraint>
  | ~=constr; LPAR; ~=nonempty_list(expr); RPAR; <Applied>
  | n=ID; LPAR; args=nonempty_list(expr); RPAR; { Applied (LambdaAlias n, args) }
  | EXISTS; v=ID; COLON; tv=core; DOT; t=core; { Exists (v, tv, t) }
  | LBRACKET; args=separated_list(COMMA, core); RBRACKET; ARROW; ret=core; { Function (ret, args)}
  | LPAR; ~= core; RPAR; <>
  | STRUCT; LBRACE; ~=nonempty_list(c_field); RBRACE; <Struct>
  | UNION; LBRACE; ~=nonempty_list(c_field); RBRACE; <Union>                           


let constr :=
  | LT; ~=nonempty_list(ID); GT; ~=core; <Lambda>
  | LPAR; ~=constr; RPAR; <>

let predicate :=
  | x=expr; ~=cmpop; y=expr; { Binary (cmpop, x, y) }
  | x=predicate; LAND; y=predicate; { Binary (And, x, y) }
  | LPAR; ~=predicate; RPAR; <>

let expr :=
  | SELF; { Self }
  | ~=INT; <Cst>
  | ~=ID; <Var>
  | ~=unop; ~=expr; <Unary>
  | x=expr; ~=binop; y=expr; { Binary (binop, x, y) }
  | ~=predicate; <>
  | LPAR; ~=expr; RPAR; <>

%inline unop :
  | NOT;    { Not }
  | MINUS;  { UMinus }

%inline binop :
  | PLUS;   { Plus }
  | MINUS;  { Minus }
  | STAR;   { Mult }
  | DIV;    { Div }
  | MOD;    { Mod }
  | AND;    { And }
  | OR;     { Or }
  | XOR;    { Xor }
  | SL;     { LShift }
  | SR;     { RShift }
 // | LAND;   { LAnd }
 // | LOR;    { Or }

%inline cmpop :
  | EQUAL;  { Eq }
  | DIFF;   { Diff }
  | GE;     { Ge }
  | GT;     { Gt }
  | LE;     { Le }
  | LT;     { Lt }
