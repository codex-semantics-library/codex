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

(* This is the OCaml translation of the online least common ancestor algorithm given in:

- https://www.slideshare.net/ekmett/skewbinary-online-lowest-common-ancestor-search
- https://www.schoolofhaskell.com/user/edwardk/online-lca.Arith_status

It has some drawbacks compared to the one I wrote, but we keep it for reference.

*)

(* OK, id is the type of the value that we are checking. *)
type id = int
(* A complete binary tree of ids. *)
(* MAYBE: Tip = 0, Bin = 1 in binary. Two can appear only at the beginning, and is seend as two ones. *)
(* Non, en fait on a une liste chainee avec que les 1, mais les 0 sont
   absents, et a la place on utilise la taille des arbres pour savoir
   à quel chiffre ca correspond. Et un invariant: path_size = sum des tree_size dans l'arbre. 
   Le 2 correspond a avoir deux 1 a la suite, et ca ne peut arriver qu'en tête de liste.

   We could create a unique id on cons to avoid that.
*)
type tree = Leaf of id | Node of id * tree * tree

let equal_id = (==)

(* We identify the the tree (logically representing the tip of a path in the tree) 
   using the head element. *)
let equal_tree a b = match a,b with
  | Leaf(a), Leaf(b) -> equal_id a b
  | Node(a,_,_), Node(b,_,_) -> equal_id a b
  | _ -> false
;;



(* Note: we avoid an unboxing by redefining the list here. *)
type path =
  | Nil
  | Cons of { path_size:int;       (** Number of entries in the entire path or depth of the branch. *)
              tree_size:int;       (** Number of elements in the tree.  *)
              tree:tree;
              tail:path}


let equal_path a b = match a,b with
  | Nil, Nil -> true
  | Cons{tree=treea},Cons{tree=treeb} -> equal_tree treea treeb
  | Nil, Cons _ | Cons _, Nil -> false
;;



let path_size = function
  | Nil -> 0
  | Cons {path_size} -> path_size
;;

(* consT :: Int -> Tree -> Path -> Path *)
(* consT w t ts = Cons (w + size ts) w t ts *)

(* cons :: Id -> Path -> Path *)
(* cons k (Cons n w t (Cons _ w' t2 ts))  *)
(*   | w == w' = Cons (n + 1) (2 * w + 1) (Bin k t t2) ts *)
(* cons k ts = Cons (size ts + 1) 1 (Tip k) ts *)

(* Note: most of the size I already know the path_size, so this is not needed. *)
let const tree_size tree tail = Cons {path_size = tree_size + path_size tail;
                                      tree_size; tree; tail }

let cons id path = match path with
  (* Two consecutive ones correspond to a two in the skew binary sytem. *)
  | Cons{path_size;tree_size=ts1;tree=t1;tail=Cons{path_size=_;tree_size=ts2;tree=t2;tail}}
    when ts1 == ts2 ->
    let tree = Node(id,t1,t2) and tree_size = 2 * ts1 + 1 in
    Cons{ path_size=path_size + 1; tree_size; tree; tail}
  | tail -> let tree_size = 1 and tree = Leaf id in
    Cons{path_size = (path_size tail) + 1; tree_size; tree; tail}

let uncons = function
  | Nil -> None
  | Cons{tree=Leaf x;tail} -> Some(x,tail)
  | Cons{tree_size;tree=Node(elt,left,right);tail} ->
    let tree_size = tree_size / 2 in
    Some(elt,const tree_size left @@ const tree_size right tail)


(* Note: Compared to what I had, this lca is more expensive because we
   need to rebuild the path. It means that we cannot just compare
   paths using ==.

   So, cons is more expensive, but nth is more lightweight, more suitable for a disk database
   for instance.

   Also, we might not use physiqual equality for lca, we need a unique identifier. 
   Which works here if what we cons uniquely identifies an element in the tree.

   And finally, because we consume memory when we reconstruct lcas, we consume more space 
   (logically identical lcas may require to be stored twice in memory). So, for long-lived objects this is not ideal.

   The log2(log2(n)) factor is probably not very problematic: log2(log2(65536)) = 4, log2(log2(2^32))=5, log2(log2(2^64))=6. So in practice, it is almost equivalent as being constant.
 *)

(* I need to reconstruct the tail from the tree, using the index. *)
let rec keept idx tree_size tree tail =
  let half_tree_size = tree_size / 2 in
  match tree with
  | Node(_,left,right) ->               (* What should I had to tail? *)
    if idx < tree_size then keept idx half_tree_size right tail  (* A part of right. *)
    else
      let tail' = const half_tree_size right tail in
      if idx == tree_size then tail' (* Exactly right. *)
      else
    if idx == tree_size - 1 then const half_tree_size left tail' (* Both left and right. *)
    else keept (idx - half_tree_size) half_tree_size left tail' (* A part of left and right. *)
  | Leaf x -> assert(idx == 0); tail

let rec keep i path = match path with
  | Nil -> assert (i == 0); Nil
  | Cons{path_size} when path_size == i -> path
  | Cons{path_size;tree_size;tree;tail} ->
    let tail_size = tree_size - path_size in
    (* Is the index in the tail, or in the tree? *)
    if i < tail_size then keep i tail (* In the tail. *)
    else if i > tail_size then keept (i - tail_size) tree_size tree tail (* In the tree. *)
    else tail (* i == tail_size *)
;;


let rec lcat tree_size ta tb tail =
  let half_tree_size = tree_size/2 in
  match ta, tb with
  | Node(_,lefta,righta),Node(_,leftb,rightb) ->
    if equal_tree lefta leftb then const half_tree_size lefta (const half_tree_size righta tail)
    else if equal_tree righta rightb then lcat half_tree_size lefta leftb (const half_tree_size righta tail)
    else lcat half_tree_size righta rightb tail
  | _ -> tail

(* Same length. *)
let rec lca' a b = match a,b with
  | Nil, Nil -> Nil
  | Nil, _ | _, Nil -> assert false
  | Cons{tree_size=tree_sizea;tree=treea;tail=taila},
    Cons{tree_size=tree_sizeb;tree=treeb;tail=tailb} ->
    assert(tree_sizea == tree_sizeb);
    if equal_tree treea treeb then a
    else if equal_path taila tailb then lcat tree_sizea treea treeb taila
    else lca' taila tailb
;;

let lca a b =
  let sizea = path_size a and sizeb = path_size b in
  if sizea < sizeb then lca' a (keep sizea b)
  else if sizeb < sizea then lca' (keep sizeb a) b
  else lca' a b
        

(* lca' :: Path -> Path -> Path *)
(* lca' h@(Cons _ w x xs) (Cons _ _ y ys) *)
(*   | x == y = h *)
(*   | xs == ys = lcaT w x y ys *)
(*   | otherwise = lca' xs ys *)
(* lca' _ _ = Nil *)

(* lcaT :: Int -> Tree -> Tree -> Path -> Path *)
(* lcaT w (Bin _ la ra) (Bin _ lb rb) ts *)
(*   | la == lb = consT w2 la (consT w2 ra ts) *)
(*   | ra == rb = lcaT w2 la lb (consT w ra ts) *)
(*   | otherwise = lcaT w2 ra rb ts *)
(*   where w2 = div w 2 *)
(* lcaT _ _ _ ts = ts *)

(* lca :: Path -> Path -> Path *)
(* lca xs ys = case compare nxs nys of *)
(*   LT -> lca' xs (keep nxs ys) *)
(*   EQ -> lca' xs ys *)
(*   GT -> lca' (keep nys xs) ys *)
(*  where *)
(*   nxs = size xs *)
(*   nys = size ys *)
  
(* fromList :: [Int] -> Path *)
(* fromList = foldr cons Nil *)

(* toList :: Path -> [Int] *)
(* toList = unfoldr uncons *)

(* xs = fromList [6,4,3,2,1] *)
(* ys = fromList [5,3,2,1] *)

(* main = print $ toList (lca xs ys) *)
