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

module Test_Prefix_Tree = struct

  module StringSet = Set.Make(String);;

  (* This checks that the trie can remember the set of strings in order. *)
  (* We test with a small number of different characters to test more situations. *)
  QCheck.Test.check_exn @@
  QCheck.Test.make ~count:1000 ~name:"compare with stringset"
    QCheck.(small_list (small_list (int_bound 1))) (fun str_list ->
        let str_list =
          str_list |> List.map (fun char_list ->
              char_list |> List.map (fun x -> Char.chr (Char.code 'a' + x))
              |> List.to_seq |> String.of_seq) in
        let trie = List.fold_left Prefix_tree.add Prefix_tree.empty_set str_list in
        let set = List.fold_left (fun a b -> StringSet.add b a) StringSet.empty str_list in
        (* Prefix_tree.dump trie; Stdlib.flush_all(); *)
        (Prefix_tree.to_list trie = StringSet.elements set))
  ;;



  open Prefix_tree

  let trie_of_list l = 
    List.fold_left Prefix_tree.add Prefix_tree.empty_set l
  
  (* let _ = *)
  (*   Prefix_tree.dump (trie_of_list ["\019"; "\019a"]) *)

  let _ =
    Prefix_tree.dump (trie_of_list ["u"; "ua"]);;

  Printf.printf "--------------\n";;
  
  let _ = 
    let trie = (singleton "u") in
    let trie = add trie "ua" in
    dump trie;;

  Printf.printf "--------------\n";;
  
  let _ = 
    let trie = empty_set in 
    let trie = add trie "u" in
    (* let trie = add trie "ua" in *)
    dump trie;;

  Printf.printf "================--------------\n";;  
  
  let _ = 
    let trie = (singleton "mardi") in
    let trie = add trie "matin" in
    let trie = add trie "michel" in
    dump trie;;

(* let trie = *)
(*   let trie = (singleton "mardi") in *)
(*   let trie = add trie "casimir" in *)
(*   trie;; *)
(* iter print_endline trie;; *)

(* (\* TODO *\) *)
(* let trie = *)
(*   let trie = (singleton "mardi") in *)
(*   let trie = add trie "" in *)
(*   trie;; *)
(* iter print_endline trie;; *)


(* (\* TODO *\) *)
(* let trie = *)
(*   let trie = (singleton "mardi") in *)
(*   let trie = add trie "mar" in *)
(*   trie;; *)
(* iter print_endline trie;; *)

(* let trie = *)
(*   let trie = (singleton "mar") in *)
(*   let trie = add trie "mardi" in *)
(*   trie;; *)
(* iter print_endline trie;; *)
(* (\* Format.printf "%a" pp trie;; *\) *)

end
