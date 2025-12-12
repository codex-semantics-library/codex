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

(* The interface between the analyze and its UI. *)

(* Will consist in OCaml objects (that gets marshalled to a file),
   but also in other binary files.

   Note that some work is required from the server to produce this
   output (e.g. to deduplicate strings, etc.) We could use
   table-pin-rows for this.
*)
open Syntax_tree
module Int64Map = Map.Make (Int64)
type disassembly_line = {
  binary_code : string;
  mnemonic : string;
  symbols_names: string list;
  instructions: expr list;
  (** Ideally, would include this: *)
  (* bdump: (Syntax_tree.expr * string Syntax_tree.Tree.t) array; (\* Dba instructions, and its evaluation.  *\) *)
  (* the strings represents results from the analysis such as "eax -> {0,1}" *)
  results: string list
}


(* Informations about the disassembly of a file. *)
type disassembly = {
  decompiled : disassembly_line Int64Map.t;
  symbol_table: (string * int64) array;
}



module Graph = struct

  module Wto = struct
    type component = Node of int | SCC of int * component list
  end

  type node_label = {
    head: int64;
    tail: int64;
    call_stack: int64 list
  }
  type edge_info = {
    back: bool
  }

  (** Each cfg node is a basic block. *)
  type light_cfg_node = {
    id: int;
    label: node_label;
    (* This field is mutable because the structure is recursive.
       You should not mutate it in the webapp. *)
    mutable succs: (edge_info * light_cfg_node) list;
  }
end

type cfg = {
  cfg_entry_node: Graph.light_cfg_node; (** Entry basic block to the CFG. *)
  wto: Graph.Wto.component list;
}


(* To put information in the code, we use the null character to insert
   markup.

   The format is:
   - null followed by a character whose last two bits is 01 -> beginning of source_color_marker.
     Follow by a symbol, and end by null null to close the span.
   - null followed by a character whose last two bits is 10 -> definition marker
     (we should mark the name, the type of definition, and probably the body of the definition,
     so that we can show this body on demand, and fold/unfold it.
   - null followed by a character whose last two bits is 11 -> use marker (allows jumping to definitions).
   - we should have a way to mark expressions, etc. to make them hoverable. *)
type color_marker =
  | Keyword_color
  | Type_color
  | Var_color
  | Fun_color



type marker =
  | Color_marker of color_marker (*Marker for syntax highlighting*)
  | Function_Definition of string (*Marker to indicate the beginning of a function definition. The string corresponds to the function name*)
  | Instruction of int (*Marker to indicate the beginning of a C instruction within the source code with the integer referring to [Cil_types.stmt.sid]*)
  | End_marker (*A closing marker/tag to indicate the end of a marked region*)

  
type token = 
  | Marker of marker (*A parsed marker tag*)
  | String of string * int * int (** A token for contiguous segment of raw text until a marker or EOF.*)
  | EOF (*End of text indicator*)

let source_color_marker_code = function
  | Keyword_color -> (0 lsl 2) lor 1
  | Type_color    -> (1 lsl 2) lor 1
  | Var_color     -> (2 lsl 2) lor 1
  | Fun_color     -> (3 lsl 2) lor 1
  
let marker_to_string = function
  | End_marker -> "\x00\x00"
  | Color_marker sc ->
      let code = source_color_marker_code sc in
      "\x00" ^ String.make 1 (Char.chr code)
  | Function_Definition name-> 
      let code = 0b10 in 
      let len = String.length name in
      let hi = Char.chr ((len lsr 8) land 0xFF) in
      let lo = Char.chr (len land 0xFF) in
      "\x00" ^ String.make 1 (Char.chr code) ^ String.make 1 hi ^ String.make 1 lo ^ name
  | Instruction id -> 
      let code = 0b11 in
      let b1 = Char.chr ((id lsr 24) land 0xFF) in
      let b2 = Char.chr ((id lsr 16) land 0xFF) in
      let b3 = Char.chr ((id lsr 8) land 0xFF) in
      let b4 = Char.chr (id land 0xFF) in
      "\x00" ^ String.make 1 (Char.chr code) ^ String.init 4 (fun j ->
        match j with
        | 0 -> b1 | 1 -> b2 | 2 -> b3 | _ -> b4)
      
let string_to_marker s i =
  (* Precondition: only call when s.[i] = '\x00' *)
  assert (s.[i] = '\x00');
  let len = String.length s in
  if i + 1 >= len then
    failwith "Malformed marker: truncated input";

  let kind = Char.code s.[i + 1] land 0b11 in
  match kind with
  | 0 -> (End_marker, i, i + 2)
  | 1 ->
      let full_code = Char.code s.[i + 1] in
      let idx = full_code lsr 2 in
      let sm =
        match idx with
        | 0 -> Keyword_color
        | 1 -> Type_color
        | 2 -> Var_color
        | 3 -> Fun_color
        | _ -> failwith ("Unknown color marker index: " ^ string_of_int idx)
      in
      (Color_marker sm, i, i + 2)

  | 2 ->
      if i + 4 >= len then failwith "Malformed Function_Definition marker";
      let hi = Char.code s.[i + 2] in
      let lo = Char.code s.[i + 3] in
      let name_len = (hi lsl 8) lor lo in
      if i + 4 + name_len > len then
        failwith "Function_Definition name truncated";
      let name = String.sub s (i + 4) name_len in
      (Function_Definition name, i, i + 4 + name_len)

  | 3 ->
      if i + 5 >= len then failwith "Malformed Instruction marker";
      let b1 = Char.code s.[i + 2]
      and b2 = Char.code s.[i + 3]
      and b3 = Char.code s.[i + 4]
      and b4 = Char.code s.[i + 5] in
      let id = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
      (Instruction id, i, i + 6)

  | _ ->
      failwith ("Unknown marker kind: " ^ string_of_int kind)


let get_token (s : string) (i : int) : int * token =
  let len = String.length s in
  if i >= len then (i, EOF)
  else if s.[i] = '\x00' then
    let m, _, j = string_to_marker s i in
    (j, Marker m)
  else
    let rec find_next_marker j =
      if j >= len || s.[j] = '\x00' then j else find_next_marker (j + 1)
    in
    let j = find_next_marker (i + 1) in
    (j, String (s, i, j))


type source = {
  text: string;                 (* A special string, using the null character to insert markup. *)
}

type marshalled = {
  disassembly: disassembly option;
  cfg: cfg option;
  tracelog: Binarytrace.trace_line list;
  source:source option;
}