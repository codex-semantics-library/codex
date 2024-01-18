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

module Make(Key:sig include Hashtbl.HashedType
    val pretty: Format.formatter -> t -> unit
  end) = struct

  module H = Hashtbl.Make(Key);;

  type substring = Key.t array
  
  type 'a node = {
    mutable self: 'a option;    (* When this node is looked up. *)
    mutable hash: 'a edge H.t option;   (* When the looked-up node is in a suffix.  *)
  }
  and 'a edge = (substring * 'a node) ref (* An edge is the substring without fork, 
                                             with the nodes where it leads to. *)
  
  let create () = { self = None; hash = None };;

  let rec find curnode key =
    match key with
    | [] -> (match curnode.self with None -> raise Not_found | Some v -> v)
    | a::b ->
      (match curnode.hash with
       | None -> raise Not_found
       | Some h ->
         let edge = 
           try !(H.find h a)
           with Not_found -> raise Not_found
         in find_edge edge b)
  and find_edge (substring,node) key =
    let rec loop key i = match key with
      | _ when i == Array.length substring -> find node key
      | [] -> raise Not_found   (* No node here. *)
      | a::b when Key.equal substring.(i) a -> loop b (i + 1)
      | a::b -> raise Not_found
    in loop key 0
  ;;

  let rec replace curnode key value =
    match key with
    | [] -> curnode.self <- Some value
    | a::b ->
      let over h =
        let substring = Array.of_list b in
        let leaf_node = { self = Some value; hash = None } in
        H.replace h a @@ ref (substring,leaf_node)
      in
      (match curnode.hash with
          | None ->
            (* We know that the hash does not exist. *)
            let h = H.create 17 in
            curnode.hash <- Some h;
            over h
          | Some h ->
            try let edge = H.find h a in replace_edge edge b value
            with Not_found -> over h)
    and replace_edge edge key value =
      (* Find the first i where key and edge do not match; *)
      let substring,existing_node = !edge in
      let rec loop key i = match key with
        | _ when i = Array.length substring -> replace existing_node key value
        | a::b when Key.equal substring.(i) a -> loop b (i + 1)
        | _ ->
          (* Creates a branch with the existing node and the new
             one. Note the special cases where the new is a prefix of
             the existing one. *)
          let matching_prefix = Array.sub substring 0 i in
          let h = H.create 17 in
          let interm_node = { self = None; hash = Some h } in
          edge := (matching_prefix,interm_node);

          (* Handle the current node *)
          (match key with
          | [] -> interm_node.self <- Some value
          | a::b ->
            let leaf_node = { self = Some value; hash = None } in
            let edge = ref (Array.of_list b,leaf_node) in
            H.replace h a edge);

          (* Handle the existing node *)
          let not_matching_prefix =
            Array.sub substring (i + 1) @@ (Array.length substring) - (i + 1) in
          let edge = ref (not_matching_prefix, existing_node) in
          H.replace h (substring.(i)) edge
      in
      loop key 0

    let rec pretty_node pretty_value fmt node =
      let pretty_hash fmt h =
        h |> H.iter (fun v edge -> 
            Format.fprintf fmt "+ %a%a@\n" Key.pretty v (pretty_edge pretty_value) edge)
      in
      (match node.self,node.hash with
       | None, None -> assert false
       | Some value, None -> pretty_value fmt value
       | None, Some h -> Format.fprintf fmt "@[<v>%a@]" pretty_hash h
       | Some value, Some h -> Format.fprintf fmt "@[<v>%a@\n%a@]"
                                 pretty_value value pretty_hash h)
    and pretty_edge pretty_value fmt {contents=(string,node)} =
      string |> Array.iter (Key.pretty fmt);
      Format.fprintf fmt " -> ";
      pretty_node pretty_value fmt node
    ;;

    type 'a t = 'a node
    let pp_trie = pretty_node

    let rec fold node f init =
      let res = (match node.self with
        | None -> init
        | Some value -> f value init) in
      match node.hash with
      | None -> res
      | Some h -> H.fold (fun _ {contents=(_,node)} acc -> fold node f acc) h res 
    ;;

    let rec map node f =
      let self = match node.self with
        | None -> None
        | Some value -> Some (f value) in
      let hash = match node.hash with
        | None -> None
        | Some hash ->
          let newhash = H.create 17 in
          hash |> H.iter (fun key edge ->
              H.replace newhash key (map_edge f edge));
          Some newhash in
      { self; hash}

    and map_edge f {contents=(string,node)} = ref (string,map node f)
    
    
end

(* Test code. *)

(* module IntTrie = Make(struct *)
(*     type t = int *)
(*     let equal = (==) *)
(*     let hash x = x *)
(*     let pretty fmt i = Format.fprintf fmt "%d" i *)
(*   end) *)

(* let pretty = IntTrie.pretty_node Format.pp_print_string;; *)

(* let trie1 = IntTrie.create();; *)
(* IntTrie.replace trie1 [1;2;3;4] "toto";; *)
(* Format.printf "Trie1: %a@." pretty trie1;; *)
(* IntTrie.replace trie1 [1;2;5;7] "tutu";; *)
(* Format.printf "Trie1: %a@." pretty trie1;; *)
(* IntTrie.replace trie1 [1;2;5;8] "tata";; *)
(* Format.printf "Trie1: %a@." pretty trie1;; *)
(* IntTrie.replace trie1 [1;2;5;8] "titi";; *)
(* Format.printf "Trie1: %a@." pretty trie1;; *)
(* IntTrie.replace trie1 [1;2;5;7;9] "tete";; *)
(* Format.printf "Trie1: %a@." pretty trie1;; *)
(* IntTrie.replace trie1 [1;2] "tonton";; *)
(* Format.printf "Trie1: %a@." pretty trie1;; *)


