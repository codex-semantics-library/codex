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

let outc_trace = ref None
let marshal_flags = []
let counter = ref (-1)
let rejected = ref 0

type trace_line = 
  | Node: {loc: 'a Syntax_tree.Location_identifier.t option; category: string; content:string} -> trace_line
  | Single: {severity: int; category:string; content:string} -> trace_line
  | Result of string option



module type S = sig
  val init: unit -> unit
  val close: unit -> unit
  val add_trace_to_file: string -> trace_line -> unit
  val read_all_items: ?file:string -> unit -> trace_line list
end

module To_file:S = struct
  let filename = "bin_trace.codex"

  let init () = 
    outc_trace := Some (open_out_bin filename)

  let close () =
    print_endline ( "rejected " ^ string_of_int !rejected ^ "/" ^ string_of_int !counter);
    match !outc_trace with
    | None -> assert false
    | Some outc -> close_out outc

  let write_trace_elt (elt: trace_line) =
    match !outc_trace with
    | None -> () (* using frama_c_codex doesn't init the out_channel *)
    | Some outc -> Marshal.to_channel outc elt marshal_flags; flush outc

  let read_item ic =
    (Marshal.from_channel ic : trace_line)

  let add_trace_to_file category trace_line  =
    incr counter;
    if (category <> "Terms.Builder") then begin
      write_trace_elt trace_line
    end
    else incr rejected


  let read_all_items ?(file=filename) () =
    let ic = open_in_bin file in
    let items = ref [] in
    try
      while true do
        items := read_item ic :: !items
      done;
      [] (* never reached *)
    with
    | End_of_file ->
      close_in ic;
      List.rev !items
    | e ->
      close_in ic;
      raise e

end

(* Test: in-memory logging is already much faster than marshalling. *)
module In_Memory:S = struct

  let init () = ()
  let close () = ()
                 

  let in_memory:trace_line list ref = ref []

  let add_trace_to_file category (trace_line:trace_line) = 
    in_memory := trace_line :: !in_memory

  let read_all_items ?file () = List.rev !in_memory
end

include In_Memory
