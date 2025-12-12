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

(* Okasakis big endiant fast integer maps, with the change that when a key is not present, we find 
   the one which is just below. *)
module Okasaki = struct

  (* A mask is an integer with a single bit set (i.e. a power of 2). *)
  type mask = int

  let lowest_bit x =
    x land (-x)

  let rec highest_bit x =
    let m = lowest_bit x in
    if x = m then
      m
    else
      highest_bit (x - m)
  ;;

  let branching_bit a b = highest_bit (a lxor b);;

  let mask i m =
    i land (lnot (2*m-1));;

  type key = int


  type 'a t =
    | Empty
    | Leaf of {key:key;value: 'a}            (* The entire key. *)
    | Branch of {prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}

  let _leaf key value  = Leaf {key;value}
  let branch ~prefix ~branching_bit ~tree0 ~tree1 = Branch{prefix;branching_bit;tree0;tree1}

  let match_prefix k p m =
    mask k m = p
  ;;


  (* Merge trees whose prefix disagree. *)
  let join pa treea pb treeb =
    (* Printf.printf "join %d %d\n" pa pb; *)
    let m = branching_bit pa pb in
    let p = mask pa (* for instance *) m in
    if (pa land m) = 0 then
      branch ~prefix:p ~branching_bit:m ~tree0:treea ~tree1:treeb
    else
      branch ~prefix:p ~branching_bit:m ~tree0:treeb ~tree1:treea
  ;;

  (* Standard insertion in a Patricia tree. *)
  let insert ~merge key value node =
    let rec ins node = 
      match node with
      | Empty -> Leaf{key;value}
      | Leaf{key=j;value=old} ->
        if key = j then Leaf{key;value=(merge ~old value)}
        else join key (Leaf{key;value}) j node
      | Branch{prefix;branching_bit;tree0;tree1} ->
        if match_prefix key prefix branching_bit
        then if (key land branching_bit) == 0
          then branch ~prefix ~branching_bit ~tree0:(ins tree0) ~tree1
          else branch ~prefix ~branching_bit ~tree0 ~tree1:(ins tree1)
        else join key (Leaf{key;value}) prefix node
    in ins node
  ;;

  (* Find the value associated to the highest key which is <= [key]. *)
  let find key node =
    let exception Too_much_on_the_right in
let rec loop node =
  match node with
  | Empty -> raise Not_found
  | Leaf{key=key';_} when key' > key -> raise Too_much_on_the_right
  | Leaf{value=data;_} -> data
  | Branch{prefix;tree0;tree1;_} ->
    (* Printf.printf "prefix %d branch %d key %d\n" prefix branching_bit key; *)
    if prefix > key (* The minimum key. *)
    then raise Too_much_on_the_right
    else
      try loop tree1
      with Too_much_on_the_right -> loop tree0
in loop node
(* let (node,zipper) as res = loop node End in *)
(* assert (match node with Leaf{key=key'} when key' > key -> true | _ -> false); *)
(* res *)
;;

end


type 'a t = 'a Okasaki.t
let insert = Okasaki.insert
let find = Okasaki.find
let empty = Okasaki.Empty
