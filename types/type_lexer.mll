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

{
  open Type_parser
}

let space = ' ' | '\t' | '\r'
let newline =  '\n'
let digit = ['0'-'9']
let hex = '0' ['x']['0'-'9''A'-'F''a'-'f']+
let bin = '0' ['b']['0''1']+
let alpha = ['a'-'z''A'-'Z']
let alpha_num = (alpha | digit)
let ident = '_'* alpha (alpha_num | '_')*

rule token = parse
  | eof                       { EOF }
  | "/*"                      { comment lexbuf }      
  | '+'                       { PLUS }
  | '-'                       { MINUS }
  | '/'                       { DIV }
  | '%'                       { MOD }
  | '='                       { EQUAL }
  | "!="                      { DIFF }
  | "<="                      { LE }
  | '<'                       { LT }
  | ">="                      { GE }
  | '>'                       { GT }
  | '!'                       { NOT }
  | '~'                       { NOT }
  | '&'                       { AND }
  | '|'                       { OR }
  | '^'                       { XOR }
  | "<<"                      { SL }
  | ">>"                      { SR }
  | "&&"                      { LAND }
  | "||"                      { LOR }
  | '*'                       { STAR }
  | ';'                       { SEMICOLON }
  | '?'                       { QUESTION_MARK }  
  | '('                       { LPAR }
  | ')'                       { RPAR }
  | '['                       { LBRACKET }
  | ']'                       { RBRACKET }
  | '{'                       { LBRACE }
  | '}'                       { RBRACE }
  | ','                       { COMMA }
  | "∃"                       { EXISTS }
  (* | "∀"                       { FORALL } *)
  | ":"                       { COLON }
  | "."                       { DOT }
  | "type"                    { TYPE }
  | "struct"                  { STRUCT }
  | "union"                   { UNION }
  | "with"                    { WITH }
  | "self"                    { SELF }
  | "->"                      { ARROW }
  | "\""                      { QUOTE }
  | "def"                     { DEF }
  | ident as id               { ID id }
  | bin | hex | digit+ as x   { INT (Z.of_string x) }
  | space+                    { token lexbuf }
  | newline                   { let pos = lexbuf.Lexing.lex_curr_p in
                                lexbuf.Lexing.lex_curr_p <- { pos with
                                   Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                                   Lexing.pos_bol = pos.Lexing.pos_cnum;
                                   };
                                token lexbuf }
  | _
      {
        let open Lexing in
        let line = (lexeme_start_p lexbuf).pos_lnum in
        let msg =
          Format.asprintf "Unkown lexeme %s at line %d" (lexeme lexbuf) line in
        failwith msg }

and comment = parse
  | "*/" { token lexbuf }
  | _ { comment lexbuf }
