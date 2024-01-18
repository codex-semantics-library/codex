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

(* To test: ocamlfind ocamlopt -package zarith int_builtins.ml int_builtins_c.c  skiplist.mli skiplist.ml -linkpkg && ./a.out *)

(* A kind of skiplist (average constant-time push), log(n) access to
   the ith element (and faster for the latest indices).

   Also containes operations when a tree is represented as a set of
   lists from leaves to root, for testing if one is a prefix, or
   finding the nearest common ancestor with fast complexity. *)

let log2_elements_per_node = 2;;
let elements_per_node = 1 lsl log2_elements_per_node;;


(* Each node contains four values, to remove some space overhead.  *)
(* MAYBE: Could possibly mutable, so that we can share the storage
   between several elements. *)
type 'a full_node = {
  (* Each pointer at index i points to 2^i nodes before. Preds is
     always at least 1, except for the first node. 

     Moving into the list requires logarthmitc time, while the memory consumption for the links is:
     (e.g. only n/4 elements in a list have an extra pointer to 4 elements before, etc )
     n + n/2 + n/4 + .... = n * sum 1/(2^k) = 2 n

     So we consume only two times the amount of memory.

*)
  preds: 'a full_node array;  
  value0: 'a;
  value1: 'a;
  value2: 'a;
  value3: 'a;
}
;;

(* No need to get the length for every node. *)
type 'a t =
  | Nil
  (* TODO: Save space by not using a full node for the last node, when
     it is not fully filled. *)
  | Non_nil of { skip_list: 'a full_node; length: int; }
;;

let nil = Nil
let is_nil x = (x = Nil)
let length = function
  | Nil -> 0
  | Non_nil {length} -> length
;;

(* unsigned int v;  // find the number of trailing zeros in v
 * int r;           // put the result in r
 * static const int Mod37BitPosition[] = // map a bit value mod 37 to its position
 * {
 *   32, 0, 1, 26, 2, 23, 27, 0, 3, 16, 24, 30, 28, 11, 0, 13, 4,
 *   7, 17, 0, 25, 22, 31, 15, 29, 10, 12, 6, 0, 21, 14, 9, 5,
 *   20, 8, 19, 18
 * };
 * r = Mod37BitPosition[(-v & v) % 37];
 * 
 * The code above finds the number of zeros that are trailing on the right, so binary 0100 would produce 2. It makes use of the fact that the first 32 bit position values are relatively prime with 37, so performing a modulus division with 37 gives a unique number from 0 to 36 for each. These numbers may then be mapped to the number of zeros using a small lookup table. It uses only 4 operations, however indexing into a table and performing modulus division may make it unsuitable for some situations. I came up with this independently and then searched for a subsequence of the table values, and found it was invented earlier by Reiser, according to Hacker's Delight. *)

(* We do not expect lists that would be too large. *)
let find_last_set v =
  let idx = ((-v) land v) mod 37 in
  [|  32; 0; 1; 26; 2; 23; 27; 0; 3; 16; 24; 30; 28; 11; 0; 13; 4;
      7; 17; 0; 25; 22; 31; 15; 29; 10; 12; 6; 0; 21; 14; 9; 5;
      20; 8; 19; 18   |].(idx) + 1
;;

let find_last_set = Int_builtins.ffs;;

(* TODO: use my int_builtins instead. *)
let log2 x = Z.log2 @@ Z.of_int x

let log2 = Int_builtins.log2;;

  (* XXX: A bien tester, mais on devrait etre pas si mal. *)

(* Invariant: Un noeud multiple de 2*n a n pointeurs en arrière *)
let cons hd tl =
  match tl with
  | Nil ->
    Non_nil{skip_list = {value0=hd;value1=hd;value2=hd;value3=hd;preds=[||]};
             length=1}
  | Non_nil tl ->
    let res = match tl.length land (elements_per_node - 1) with
      | 0 ->
        let num_nodes = tl.length lsr 2 in
        (* Every node for which we are a power of two. *)
        let preds_length = find_last_set num_nodes (* - 1 *) (* Somethink like this *) in
        let first_pred = tl.skip_list in
        let preds = Array.make preds_length first_pred in
        let rec loop i pred =
          let newi = i + 1 in
          if newi = preds_length then ()
          else begin
            let newpred = pred.preds.(i) in
            preds.(newi) <- newpred;
            loop (i + 1) newpred
          end
        in loop 0 first_pred;
        let new_node = (* Node *) { value0 = hd; value1 = hd; value2 = hd; value3= hd;
                              preds = preds }
in new_node
     (* Just add one element to the node. *)
      | 1 -> let (* Node *) x = tl.skip_list in (* Node *) {x with value1 = hd }
      | 2 -> let (* Node *) x = tl.skip_list in (* Node *) {x with value2 = hd }
      | 3 -> let (* Node *) x = tl.skip_list in (* Node *) {x with value3 = hd }
      | _ -> assert false
    in Non_nil{ skip_list = res; length = tl.length + 1}
;;

let head = function
  | Nil -> failwith "Invalid argument for Skiplist.head"
  | Non_nil _ -> assert false   (* TODO *)

let tail = function
  | Nil -> failwith "Invalid argument for Skiplist.tail"
  | Non_nil _ -> assert false   (* TODO *)

let nth n = function
  | Nil -> failwith "Invalid argument for Skiplist.nth"
  | Non_nil {skip_list;length} ->
    (* Printf.printf "nth %d %d\n" n length; *)
    assert(n < length);
    (* let pos_needed = n lsr 2 in *)
    (* Invariant: skip_list is the ith node. *)
    let rec find_node skip_list todo =
      let preds_size = Array.length skip_list.preds in
      (* Printf.printf "find_node todo %d  preds_size %d\n" todo preds_size; *)
      (* How far do we need to go. *)
      (* XXX: I could do it in reverse, count how much I need to travel from the start,
         and then decrease that. 

 *)
      (* let needed = (i - pos_needed) in *)
      if todo = 0
      then skip_list
      (* maybe: a fast case with no log2. *)
      else
        let chosen = min (preds_size - 1) (log2 todo) in
      (* Printf.printf "find_node chosen %d log2 todo = %d\n" chosen (log2 todo); *)
        find_node skip_list.preds.(chosen) (todo - (1 lsl chosen))
    in
    let todo = ((length - 1) lsr 2) - (n lsr 2) in
    let node = find_node skip_list todo in
    match n land 3 with
    | 0 -> node.value0
    | 1 -> node.value1
    | 2 -> node.value2
    | 3 -> node.value3
    | _ -> assert false
;;


let a1 = cons 0 nil;;
let a2 = cons 1 a1;;
let a3 = cons 2 a2;;
let a4 = cons 3 a3;;
let a5 = cons 4 a4;;
let a6 = cons 5 a5;;
let a7 = cons 6 a6;;
let a8 = cons 7 a7;;
let a9 = cons 8 a8;;
let a10 = cons 9 a9;;
let a11 = cons 10 a10;;
let a12 = cons 11 a11;;
let a13 = cons 12 a12;;
let a14 = cons 13 a13;;
let a15 = cons 14 a14;;
let a16 = cons 15 a15;;
let a17 = cons 16 a16;;

nth 1 a2;;
nth 3 a5;;
nth 4 a9;;

(* A systematic test. *)
for i = 0 to 3000 do
  let ai =
    let rec loop acc j =
      if j = i then acc
      else loop (cons j acc) (j + 1)
    in loop nil 0
  in
  for j = 0 to i - 1 do
    assert((nth j ai) == j)
  done;
  ()
done;;


let nearest_common_ancestor a b =
  (* Using the lengths, first get them at the same skiplist.  After
     that, on each predecessor node, we can use a dichotomic search
     (starting from the furthest node) to find the furthest common
     ancestor. This will allow finding the nearest_common_ancestor in 
     logarithmic time.

     actually: with linear search, it would be log(n) * log(n) (we make a log(n) operation on log(n) nodes). With dichotomic, it would be log(n) * log(log(n)).


 *)
  assert false
;;
