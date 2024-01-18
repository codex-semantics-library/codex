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

let run_command command =
  (* First we create the in_channel *)
  let ic = Unix.open_process_in command in
  (* We store the results in a buffer *)
  let buffer = Buffer.create 200 in 
  try
    while true do
      let line = input_line ic in
      Buffer.add_string buffer (line ^ "\n")
    done;
    assert false
  with End_of_file ->
    (* When the command is over, we print the results *)
    ignore (Unix.close_process_in ic);
    Buffer.contents buffer


let print_correct item =
  (* This is printed when there are no differences with the commited file *)
  print_endline ("<testcase name=\"" ^ item ^ "\" classname=\"Tests\" />");;


let print_incorrect item =
  (* This is printed when there are differences with the commited file *)
  print_endline ("<testcase name=\"" ^ item ^ "\" classname=\"Tests\" />");
  print_endline "<failure message=\"Differences found\" type=\"Diff\">";
  print_string (run_command ("git diff " ^ item ^ " | tail -n +4" ));
  print_endline "</failure>\n</testcase>";;


let print_xml hashTableDiff listAll =
  (* This prints the header of the XML file *)
  print_endline "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  print_endline "<testsuites><testsuite>";
  (* Then it iterates over the files and decides whether they are correct or no *)
  List.iter (fun item -> if Hashtbl.mem hashTableDiff item
    then if (String.length item > 0) then (print_incorrect item) else ()
    else (print_correct item)) listAll;
  print_endline "</testsuite></testsuites>";;


let calculate_differences folder =
  (* First we execute the two commands *)
  let stringAll  = run_command ("find " ^ folder ^ " -type f | sort") in
  let stringDiff = run_command ("git diff --name-only --relative " ^ folder) in
  (* Then we store the results in a list *)
  let listAll  = String.split_on_char '\n' stringAll in
  let listDiff = String.split_on_char '\n' stringDiff in
  (* And we store the files with differences in a hash table *)
  let hashTableDiff = Hashtbl.create 50 in
  List.iter (fun item -> Hashtbl.add hashTableDiff item ()) listDiff;
  (* Finally we print the differences in an XML format *)
  print_xml hashTableDiff listAll;;


let () =
  match Array.to_list Sys.argv with
  | [_; folder] -> calculate_differences folder
  | _           -> print_endline "ERROR: Usage: dune exec diff_tool <folder>";;
