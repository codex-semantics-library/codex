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

(* This is an algorithm for computing level ancestor and
   lowest/nearest/least common ancestor between trees represented as
   lists from leaf to root, that change a lot.

   The main data structure is a kind of "deterministic skiplist" where
   each node at depth d of the list has a link to its ancestor at
   depth (d - 2^k) if d is a multiple of 2^k. For instance, a node at
   depth 5 links only to its parent at depth 4, a node at depth 6
   links to its ancestors at depth 5 and 4, and a node at depth 8
   links to its ancestors at depth 7,6,4,and 0, where the node at
   depth 0 is the root. The root has no ancestor.

   The maximum number of links created for a node at depth d is
   log2(d), which is the maximum complexity of node creation. However,
   creating a list of $n$ elements requires the creation of
   sum_k=0=0^n n/(2^k) ~= 2 n links, so the average complexity of node
   creation is constant.

   Solving the level ancestor problem, i.e. finding the ancestor at
   depth i for a node at depth d is solved in log2(n-d) time.

   Finding the nearest common ancestor between two nodes / lists is
   solved in log2(h)*log2(log2(d)) time, where d is the maximum depth
   of both lists, and h is the difference between the maximum depth
   and the depth of the common ancestor. log2(log2(d)) grows very
   slowly and can be considered constant for practical purposes.

   A nice property of this algorithm is that it reuses memory as much
   as possible (no new lists are created), which makes it practical
   for on-disk storage or when many lists are used simultanously. *)
let log2 = Int_builtins.log2_untagged_unsafe;;
let count_trailing_zeroes = Int_builtins.count_trailing_zeroes;;


(* For testing in the toplevel.

let count_trailing_zeroes v =
  let idx = ((-v) land v) mod 37 in
  [|  32; 0; 1; 26; 2; 23; 27; 0; 3; 16; 24; 30; 28; 11; 0; 13; 4;
      7; 17; 0; 25; 22; 31; 15; 29; 10; 12; 6; 0; 21; 14; 9; 5;
      20; 8; 19; 18   |].(idx);;

let log2 v =
  assert (v land (lnot 0x0FFFFFFF) == 0); (* Rejects numbers too big *)
  assert (v != 0);
  let (v,r) = (v,0) in
  let (v,r) = if 0 != v land 0x0FFF0000 then (v lsr 16), r lor 16 else (v,r) in
  let (v,r) = if 0 != v land 0xFF00 then (v lsr 8), r lor 8 else (v,r) in
  let (v,r) = if 0 != v land 0xF0 then (v lsr 4), r lor 4 else (v,r) in
  let (v,r) = if 0 != v land 0xC then (v lsr 2), r lor 2 else (v,r) in
  let r = if 0 != v land 0x2 then r lor 1 else r in
  r
;;
*)

type 'a parents = 'a array;;


(* The functor is such that the helper "parents" structure can be easily embedded in another structure. *)
module Make(Elt:sig
    type t
    (* Distance from the root, where the root is 0. *)      
    val depth: t -> int

    (* If the depth d is a multiple of 2^k, then the length of this
       array is at least k+1. parents.(i) points to the parent whose
       depth is d - 2^i. *)    
    val parents: t -> t parents

  end) = struct

  (* TODO: only do all of this above some multiple, like 4. *)

  let nil = 0,[||];;

  (* Cons worst complexity is O(log n). It is proportional to the size of the parents array.
     But, if we create a list of size n:
     n elements have a pointer to their predecessor;
     n/2 elements have a pointer to 2 elements before;
     n/4 elements have a pointer to 4 elements before;
     etc.
     Thus, the total number of cells in the array is \sum_{k=0}^{n} n/(2^k) which is asympotically equivalent to 2 * n. 
     In other words, the mean number of array cells in a list node is 2, and the mean complexity of cons is also 2.  *)
  let cons x =
    (* Printf.printf "in cons\n"; *)
    let depth = Elt.depth x in
    let depth' = depth + 1 in
    let trailing_zeroes = (count_trailing_zeroes depth') in
    let parents_length = trailing_zeroes + 1 in

    (* Printf.printf "deph' %d parent lengh %d\n" depth' parents_length; *)
    (* MAYBE: The initialization could be lazy, to avoid the lookups.  
       Then, find could perform path compression, as in union-find structures. *)
    let parents : Elt.t parents = Array.make parents_length x in
    let rec loop i (pred: Elt.t) =
      parents.(i) <- pred;
      let i' = i + 1 in
      if i' == parents_length then ()
      else
        let pred = (Elt.parents pred).(i) in
        loop i' pred
    in loop 0 x;
    (* Printf.printf "out of cons\n"; *)

    (* Invariant on parents. *)
    assert(for i = 0 to parents_length - 1 do
             assert (Elt.depth (parents.(i)) == depth' - (1 lsl i))
           done; true);
    depth',parents
  ;;

  (* The best-case complexity of this operation is O(1).
     The worst case is log2(d - n), where d is the depth of the list.

     Indeed, moving from d to to a number multiple of log2(d - n) in the interval [d,n] takes log2(d - n) steps,
     and similarly when moving to that multiple of log2(d-n) to n; 
     so we need at most 2*log2(d-n) calls to nth, each of constant time, to succeed.

     For instance, the worst case when moving from a list of size 15 is going to 1:
     nth [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] 1 =
     nth [1;2;3;4;5;6;7;8;9;10;11;12;13;14] 1 =
     nth [1;2;3;4;5;6;7;8;9;10;11;12] 1 =
     nth [1;2;3;4;5;6;7;8] 1 =
     nth [1;2;3;4] 1 =
     nth [1;2] 1 =
     nth [1] 1 =
     [1]
  *)
  let rec nth x v =
    let todo = Elt.depth x - v in
    (* Printf.printf "nth: todo %d\n" todo; *)
    assert(todo >= 0);
    if todo = 0 then x
    else
      let parents = Elt.parents x in
      let parents_length = Array.length parents in
      let idx = min (parents_length - 1) (log2 todo) in
      nth parents.(idx) v
  ;;

  

  (* The binary search is log2(log2(depth)). The number of steps is log(depth-target). So we have log2(depth-target)*log2(log2(depth)) complexity. *)
  let rec nearest_common_ancestor_same_depth a b =
    (* Printf.printf "Same depth %d %d\n" (Elt.depth a) (Elt.depth b); *)
    assert(a != b);
    assert(Elt.depth a == Elt.depth b);
    let aparents = Elt.parents a and bparents = Elt.parents b in
    let length = Array.length aparents in
    assert(length == Array.length bparents);
    (* Printf.printf "here\n"; *)

    (* Returns the highest index i in the table such that a.(i) != b.(i), using binary search.

       (This use the fact that if a.(i) == b(i), then a.(i+1) == b.(i+1) when it exists).
       left is largest known index such that a.(left) != b.(left).
       right is smallest known index such that a.(right) != b.(right).

       Note that it is possible that all are equal, then we have found the nearest_common_ancestor. *)
    (* XXX: This algorithm seems weird. We should try first with the highest parent: if not an ancestor,
       continue climbing rightaway. Binary search is only if this does not work. *)
    let rec binary_search left right =
      assert(left != right);
      if right == left + 1
      then left
      else
        let mid = (left + right) / 2 in
        if aparents.(mid) == bparents.(mid)
        then binary_search left mid
        else binary_search mid right
    in
    let res = binary_search (-1) (length) in
    if res == -1
    (* All the elements in the array are the same: we found our ancestor. *)       
    then begin
      assert(aparents.(0) == bparents.(0));
      aparents.(0)
    end
    else begin
      assert(aparents.(res) != bparents.(res));
      nearest_common_ancestor_same_depth aparents.(res) bparents.(res)
    end
  ;;



  let nearest_common_ancestor a b =
    let adepth = Elt.depth a and bdepth = Elt.depth b in
    if adepth > bdepth
    then
      let a' = nth a bdepth in
      if a' == b then a'
      else nearest_common_ancestor_same_depth a' b
    else if adepth < bdepth then
      let b'= nth b adepth in
      if b' == a then b'
      else nearest_common_ancestor_same_depth a b'
    else 
    if a == b then a
    else nearest_common_ancestor_same_depth a b
  ;;

end


(**************** Test. ****************)


module Test() = struct
  type node = {
    parents: node parents;
    value:int;
    depth:int;
  }

  module T = struct
    type t = node
    let depth x = x.depth
    let parents x = x.parents
  end

  module MakeElt = Make(T);;

  let root = 
    let depth,parents = MakeElt.nil in
    { parents; depth; value = 0 }

  let cons value x =
    let depth,parents = MakeElt.cons x in
    { parents; value; depth = x.depth + 1 }
  ;;

  assert(MakeElt.nth root 0 == root);;

  (* Systematic test of nth upto max. *)
  let test_nth max =
    let array = Array.make max root in
    let rec loop x i =
      Printf.printf "creating %d\n" i; 
      array.(i) <- x;
      let i = i + 1 in
      if i == max then ()
      else loop (cons i x) i
    in loop root 0;

    for j = 0 to max - 1 do
      for i = 0 to j do 
        Printf.printf "nth %d %d\n" j i;
        assert(MakeElt.nth array.(j) i == array.(i))
      done;
    done;
    array
  ;;

  test_nth 1000

  let rec cons_x_times x acc =
    if x = 0
    then acc
    else cons_x_times (x - 1) @@ cons x acc
  ;;

  (* Test nearest_common ancestor with a common of depth common, and branches at depths common + a and common + b  *)
  let test_nca common a b =
    Printf.printf "test_nca %d %d %d\n" common a b;
    let c = cons_x_times common root in
    let a = cons_x_times a c in
    let b = cons_x_times b c in
    assert(MakeElt.nearest_common_ancestor a b == c)
  ;;

  test_nca 10 2 2;;

  let systematic_test_nca max =
    for c = 0 to max - 1 do
      for a = 0 to max - 1 do
        for b = 0 to max - 1 do
          test_nca c a b
        done
      done
    done
  ;;

  systematic_test_nca 129;;
end
(* Cons is O(log depth).  *)
