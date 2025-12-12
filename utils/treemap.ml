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


module type Key = sig
  type t
  val nearest_common_ancestor: t -> t -> t
  val is_prefix: t -> t -> bool
  val equal: t -> t -> bool
  val pretty: Format.formatter -> t -> unit
end

(* let res = Key.nearest_common_ancestor 0b10111 0b10101;; *)

module Make_no_empty(Key:Key) = struct

  type 'a t =
    | Node of {key:Key.t; value:'a; children:'a t list}
  ;;

  let leaf key value = Node{key;value;children=[]};;

  let get_key = function
    | Node {key} -> key
  ;;


  let find key_to_find map =
    (* Invariant of this loop: key_to_find is a suffix of the key of map. *)
      let rec find key_to_find = function
        | Node{key;value} when Key.equal key key_to_find -> value (* avoids iteration on the children. *)
        | Node{key;value;children} ->
          assert (Key.is_prefix key key_to_find);
          (* Try to find a more precise children. *)
          let rec loop = function
            (* No children correspond: returns value. *)        
            | [] -> value
            | (Node{key=child_key} as hd)::tl ->
              if Key.is_prefix child_key key_to_find
              then find key_to_find hd
              else loop tl
          in loop children
      in
      let Node{key} = map in
      if Key.is_prefix key key_to_find
      then  find key_to_find map
      else assert false         (* Trying to find a key that was never mapped. *)
          (* Codex_log.fatal *)(* Codex_log.feedback "Trying to find for a key outside of this map: %a %a"
           * Key.pretty key Key.pretty key_to_find; *)
;;

  (* The general idea: select the branch with the longest prefix match, and insert an interior node where they diverge. To find which son is the good one, we use nearest_common_ancestor between the son and  the key to insert; if it goes further than the parent, then the son goes in the right direction.
*)

  (* We take a key, a group, and we maintain a structure such that for
     all elements in the tree, we have the product of all the values
     of the group with operation inter. *)
  let update_all inter value_to_insert trie = 
      let rec loop = function
        (* | Leaf{key;value} -> Leaf{key;value=inter value value_to_insert} *)
        | Node{key;value;children} ->
          let children = List.map loop children in
          let value = match value with
            (* | None -> Some value_to_insert *)
            | x -> inter x value_to_insert
          in Node{key;value;children}
      in loop trie
  ;;
  
  let rec refine key_to_insert ~inter ~join value_to_insert trie =
    match trie with
    | Node{key;value;children} when Key.equal key key_to_insert ->
      update_all inter value_to_insert trie
    | Node{key;value;children} ->
      let nca = Key.nearest_common_ancestor key key_to_insert in
      if Key.equal nca key_to_insert
      then
        (* Add a new interior node, and change everythng beneath *)        
        Node{key=key_to_insert;value=inter value value_to_insert;
                 children=[update_all inter value_to_insert trie]}
      else if Key.equal nca key
      then
        (* Insert somewhere in the tree, either further down, or as a
           new child. *)
        let rec loop = function
          | [] ->
            (* No children correspond: add a new. *)
            [leaf key_to_insert (inter value value_to_insert)]
          | hd::tl ->
            let child_key = get_key hd in
            (* If it progresses in the right direction, go see this children, else try the next. *)
            if (Key.equal (Key.nearest_common_ancestor child_key key_to_insert) key)
            then hd::(loop tl)
            else (refine key_to_insert ~inter ~join value_to_insert hd)::tl
        in Node{key;value;children = loop children}
      else
        let joined_value = join value value_to_insert in
        (* The two keys are on different branches. *)
        Node{key=nca;value=joined_value;children=[trie;leaf key_to_insert value_to_insert]}
  ;;

  let singleton key value = leaf key value

  let rec make_pretty fvalue fmt = function
    | Node{key;value;children;} ->
      Format.fprintf fmt "%a -> %a@\n" Key.pretty key fvalue value;
      if children != [] then begin
        Format.fprintf fmt "[  @[<v>";
        children |> List.iter (fun x -> Format.fprintf fmt "%a" (make_pretty fvalue) x);
        Format.fprintf fmt "@]]"
      end
  ;;

end

module Make(Key:Key) = struct

  module M = Make_no_empty(Key)
  
  type 'a t = 'a M.t option

  let refine key_to_insert ~inter ~join value_to_insert trie = match trie with
    | None -> Some(M.singleton key_to_insert value_to_insert)
    | Some x -> Some(M.refine key_to_insert ~inter ~join value_to_insert x)

  let find key = function
    | None ->
      (* Codex_log.feedback "Find with key %a" Key.pretty key; *)
      raise Not_found
    | Some x -> M.find key x

  let empty = None

  let refine key_to_insert ~inter ~join value_to_insert trie =
    (* Codex_log.feedback "Treemap.refine key %a" Key.pretty key_to_insert; *)
    refine key_to_insert ~inter ~join value_to_insert trie
  ;;

  let make_pretty fvalue fmt = function
    | None -> Format.fprintf fmt "<empty>"
    | Some x -> M.make_pretty fvalue fmt x
  ;;
    
  
  
end


(**************** Testing code. ****************)
(* ocamlc treemap.ml && ./a.out  *)
(* A map from a set having tree elements. *)

(* module Key:Key with type t = int = struct
 * 
 *   type t = int
 * 
 *   let parent x = x lsr 1
 * 
 *   let equal a b = a = b
 * 
 *   (\* Leq means is prefix in the tree order, or is on the path to the root *\)
 *   let rec is_prefix a b =
 *     if b < a then false         (\* Speedup *\)
 *     else if a = b then true
 *     else is_prefix a (parent b)
 * 
 *   let rec nearest_common_ancestor a b =
 *     if a = b
 *     then a
 *     else if a < b
 *     then  nearest_common_ancestor a (parent b)
 *     else nearest_common_ancestor (parent a) b
 *   ;;
 * 
 *   let pretty fmt x = Format.fprintf fmt "%d" x
 *   
 * end
 * 
 * 
 * module M = Make_no_empty(Key)
 * let refine key value trie = M.refine key (^) (^) value trie;;
 * 
 * let treei = M.singleton 0b1 "i";;
 * let tree0 = refine 0b101 "a" treei;;
 * let tree1 = refine 0b01010 "b" tree0;;
 * let tree2 = refine 0b101 "c" tree1;;
 * let tree3 = refine 0b1111 "d" tree2;;
 * let tree4 = refine 0b1100 "e" tree3;;
 * 
 * let find = M.find;;
 * find 0b1111 tree4;;
 * find 0b1100 tree4;;
 * find 0b1 tree4;;
 * find 0b01010 tree4;;
 * find 0b101 tree4;;
 * find 0b11 tree4;;
 * find 0b1011 tree4;;
 * 
 * let tree2_i = M.singleton 0b1110 "a";;
 * let tree2_1 = refine 0b1111 "b" tree2_i;; *)
