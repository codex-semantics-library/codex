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

(** A prefix trie (or radix tree) for storing a finite set of strings.

   The trie is organised so that each node represents:
     - A common prefix shared by all strings in the subtree
     - An optional flag indicating that the prefix itself is a stored string
     - A mapping from next characters to child tries for the remaining suffixes

   This is a *compressed* trie: rather than storing one character per node,
   each node stores an entire substring `prefix` that is common to all
   descendants. This saves memory and speeds up traversal. *)

module CharMap = Map.Make(Char);;


type _ internal =
  | Empty : [`empty] internal
  | NonEmpty : {
      prefix : string;                  (** Common prefix for all children; may be "" *)
      contains_prefix : bool;           (** True if prefix itself is in the set *)
      children : [`nonempty] internal CharMap.t  (** Maps next char → subtree; empty only if single element *)
    } -> [`nonempty] internal

type t = Any: 'a internal -> t [@@unboxed]

let iter_for_dump f t =
  let rec loop prefix f (NonEmpty t) =
    let prefix = prefix ^ t.prefix in
    if t.contains_prefix then f prefix;
    t.children |> CharMap.iter (fun c rest ->
        let prefix = prefix ^ "[" ^ (String.make 1 c)  ^ "]" in
        loop prefix f rest)
  in
  loop ">" f t

let dump t = iter_for_dump print_endline t


let iter f t =
  let rec loop prefix f (NonEmpty t) =
    let prefix = prefix ^ t.prefix in
    if t.contains_prefix then f prefix;
    t.children |> CharMap.iter (fun c rest ->
        let prefix = prefix ^ (String.make 1 c) in
        loop prefix f rest)
  in
  loop "" f t

let to_list t =
  let r = ref [] in
  iter (fun x -> r := x::!r) t;
  List.rev !r

let singleton string =
  NonEmpty {
    prefix = string;
    contains_prefix = true;
    children = CharMap.empty
  }

let empty_set = Empty



(* Returns (prefix,char,suffix); e.g. split_at "matin" 3 = ("mat", 'i', "n") *)
let split_at string i =
  String.sub string 0 i,
  String.get string i,
  String.sub string (i + 1) ((String.length string) -i - 1)
;;



(* Returns prefix, (char * suffix) option, (char * suffix) option,
   depending of who is the suffix. *)
let decomp_strings str1 str2 =
  let l1 = String.length str1 in
  let l2 = String.length str2 in
  let l = min l1 l2 in
  let exception Found_at of int in
  try
    for i = 0 to l - 1 do
      if not @@ Char.equal (String.get str1 i) (String.get str2 i)
      then raise @@ Found_at i
    done;
    if l1 = l2 then
      str1, None, None          (* Both are equal, no suffix. *)
    else if l1 < l2 then
      let (pre2,char2,suf2) = split_at str2 l in
      assert(str1 = pre2);
      str1, None, Some(char2,suf2)
    else
      let (pre1,char1,suf1) = split_at str1 l in
      assert(str2 = pre1);
      str2, Some(char1,suf1), None
  with Found_at i ->
    let (pre1,char1,suf1) = split_at str1 i in
    let (pre2,char2,suf2) = split_at str2 i in
    assert(pre1 = pre2);
    pre1, Some(char1, suf1), Some(char2,suf2)
;;

(* decomp_strings "marin" "maton";; *)
(* decomp_strings "mar" "marin";; *)


let rec add (NonEmpty trie) string =
  (* assert(string <> ""); *)
  match decomp_strings trie.prefix string with
  | prefix, Some(c1,suf1), Some(c2,suf2) -> begin
      (* The strings differ, we need to branch. *)
      let children =
        let map = CharMap.singleton c1 (NonEmpty {trie with prefix = suf1 }) in
        let map = CharMap.add c2 (singleton suf2) map in
        map
      in
      NonEmpty { prefix; contains_prefix = false; children }
    end
  | prefix, Some(c1,suf1), None -> begin
      (* The string is a prefix of the current prefix: we need to split. *)
      let children = CharMap.singleton c1 (NonEmpty {trie with prefix = suf1 }) in
      NonEmpty {prefix; contains_prefix = true; children }
    end
  | prefix, None, None -> begin
      (* The string matches the prefix. *)
      NonEmpty {trie with contains_prefix = true }
    end
  | prefix, None, Some(c2,suf2) -> begin
      (* The string is a suffix of the prefix. *)
      match CharMap.find c2 trie.children with
      | exception Not_found ->
        (* Add a new branch to the tree *)
        NonEmpty {trie with children = CharMap.add c2 (singleton suf2) trie.children }
      | sub ->
        (* Recursively add to the correct location. *)
        let sub = add sub suf2 in
        let children = CharMap.add c2 sub trie.children in
        NonEmpty {trie with children}
    end

let rec suggestions (NonEmpty trie) string ~max =
  let empty = "", [] in
  match decomp_strings trie.prefix string with
  | _, Some _, Some _ -> empty    (* The string is not in the trie. *)
  | _, suf, None ->   (* The string match or is a prefix: iterate on all the strings. *)
    let r = ref [] in
    let count = ref 0 in
    let exception Exit in
    (try
      iter (fun x ->
          incr count;
          if !count > max then raise Exit;
          r:=x::!r) (NonEmpty trie)
     with Exit -> ());
    let completion = match suf with
      | Some(c,suf) -> (String.make 1 c) ^ suf
      | None -> ""
    in
    completion, List.rev !r

  | _, None, Some (c,suffix) -> (* The string match this prefix and continue. Lookup a subtree. *)
    begin
      match CharMap.find c trie.children with
      | exception Not_found -> empty (* The string is not in the tree. *)
      | sub ->
        let toconcat x = (trie.prefix ^ String.make 1 c) ^ x in
        let r, l = suggestions sub suffix ~max in
        r, l |> List.map toconcat
    end

(* Final interface: handle also the empty cases. *)

let add (Any trie) string  = match trie with
  | Empty -> (Any (singleton string))
  | NonEmpty _ -> Any (add trie string)


let empty_set = Any empty_set
let singleton x = Any (singleton x)
let suggestions (Any t) str ~max = match t with
  | Empty -> "", []
  | NonEmpty _ -> suggestions t str ~max


let dump (Any t) = match t with
  | Empty -> ()
  | NonEmpty _ -> dump t

let to_list (Any t) = match t with
  | Empty -> []
  | NonEmpty _ -> to_list t
