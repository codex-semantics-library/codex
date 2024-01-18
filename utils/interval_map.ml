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


(* A mask is an integer with a single bit set (i.e. a power of 2). *)
type mask = int


let lowest_bit x =
  x land (-x)

(* This simple implementation is actually the fastest on most benchmarks. *)
let rec highest_bit x =
  let m = lowest_bit x in
  if x = m then
    m
  else
    highest_bit (x - m)
;;

(* Note: using a hardware count leading zero instruction often not
   faster on the benchmarks (maybe because of the cost of calling an
   external C function), so disable it for now. *)
(* let highest_bit = Int_builtins.highest_bit_untagged;; *)

(* This computation does not work on values too high, depends on the
   int size, and is actually not the fastest on the benchmarks.. *)
let _highest_bit v =
  (* compute highest bit. 
     First, set all bits with weight less than
     the highest set bit *)
  let v1 = v lsr 1 in
  let v2 = v lsr 2 in
  let v = v lor v1 in
  let v = v lor v2 in
  let v1 = v lsr 3 in
  let v2 = v lsr 6 in
  let v = v lor v1 in
  let v = v lor v2 in
  let v1 = v lsr 9 in
  let v2 = v lsr 18 in
  let v = v lor v1 in
  let v = v lor v2 in
  (* then get highest bit *)
  (succ v) lsr 1
;;


(* TODO: Okasaki also pass the masks as arguments, to optimize the
   computation of highest_bit, but I am not sure we can pass the masks
   in the interval map algorithms. *)
let branching_bit a b = highest_bit (a lxor b);;

(* Given a mask with a single bit set, select all the bits below this
   bit. *)
let mask i m =
  i land (lnot (2*m-1));;

type key = int


(* We represent interval maps as a (big-endian) patricia trie, where
   the value for an interval is the value of attached to the index
   corresponding to the beginning of the next interval, in the
   patricia trie. Thus, the boundaries to the interval correspond to
   the indices in the map, plus the value 0. This representations
   avoids the need to store the size of the map.

   For instance, the map [8 -> 11; 32 -> 22] corresponds to an
   interval map where the interval (0-7) is mapped to 11, and (8-31)
   is mapped to 22.

   One of the reasons for doing this is that this allows the interval
   map to store the size of the interval at no cost; and the key for
   zero does not need to be stored. Thus, the representation is very
   space-efficient, in particular for interval maps with a single
   interval. *)


(* We have implemented two versions of the code, one with hash-consing
   and one without. The version with hash-consing has slower node
   construction, but the hash-consing allows faster comparison with
   fold_on_diff. Benchmarks indicates that hash-consing slows down the
   execution. *)
module TNoHashCons:sig 
  type 'a t = private
    | Leaf of {key:key; value:'a}            (* The entire key. *)
    | Branch of {prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}
    (* Prefixes are normalized: the bits below the branching bit are
       set to zero. *)
  val leaf: key -> 'a -> 'a t
  val branch: prefix:key -> branching_bit:mask -> tree0:'a t -> tree1:'a t -> 'a t

end = struct

  type 'a t =
    | Leaf of {key:key;value: 'a}            (* The entire key. *)
    | Branch of {prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}

  let leaf key value  = Leaf {key;value}
  let branch ~prefix ~branching_bit ~tree0 ~tree1 = Branch{prefix;branching_bit;tree0;tree1}
end

module THashCons:sig
  type 'a t = private
    | Leaf of {id:int;key:key; value:'a }            (* The entire key. *)
    | Branch of {id:int;prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}
      (* Prefixes are normalized: the bits below the branching bit are
         set to zero. *)
  val leaf: key -> 'a -> 'a t
  val branch: prefix:key -> branching_bit:mask -> tree0:'a t -> tree1:'a t -> 'a t
  
end = struct
  let id_count = ref 0;;
  type 'a t =
    | Leaf of {id:int;key:key;value: 'a}            (* The entire key. *)
    | Branch of {id:int;prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}
    (* Prefixes are normalized: the bits below the branching bit are
       set to zero. *)

  module WeakH = Weak.Make(struct
      type tmp = int list t         (* We cheat a bit: a polymorphic argument could be used here.. *)
      type t = tmp
      let equal a b = match a,b with
        | Branch{prefix=prefixa;branching_bit=branching_bita;tree0=tree0a;tree1=tree1a},
          Branch{prefix=prefixb;branching_bit=branching_bitb;tree0=tree0b;tree1=tree1b} ->
          prefixa == prefixb && branching_bita == branching_bitb && tree0a == tree0b && tree1a == tree1b
        | _ -> assert false

      let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;

      let get_id = function
        | Leaf{id} -> id
        | Branch{id} -> id
      
      let [@warning "-8"] hash (Branch{prefix;branching_bit;tree0;tree1}) =
        sdbm (prefix lor branching_bit) @@ sdbm (get_id tree0) (get_id tree1)
      ;;
      
    end
    );;


  let weakh = WeakH.create 120;;

  let leaf key value  =
    let id = !id_count in
    incr id_count;
    Leaf {id;key;value};;
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    let id = !id_count in
    let tentative = Branch{id;prefix;branching_bit;tree0;tree1} in
    let v = WeakH.merge weakh (Obj.magic tentative) in
    let v = Obj.magic v in
    if v == tentative then (Codex_log.debug "HASHCONS WORKS"; incr id_count);
    v
  ;;
    
end

include TNoHashCons;;
(* include THashCons;;   *)


(* For debugging. *)
let rec dump_tree fmt = function
  | Leaf{key} -> Format.fprintf fmt "(leaf %x)" key 
  | Branch{prefix;branching_bit;tree0;tree1} ->
    Format.fprintf fmt "@[<hov>(branch p:%x@ bb:%x@ %a@ %a)@]"
      prefix branching_bit dump_tree tree0 dump_tree tree1
;;
let _dump_tree = dump_tree

(* Merge trees whose prefix disagree. *)
let join pa treea pb treeb =
  let m = branching_bit pa pb in
  let p = mask pa (* for instance *) m in
  if (pa land m) = 0 then
    branch ~prefix:p ~branching_bit:m ~tree0:treea ~tree1:treeb
  else
    branch ~prefix:p ~branching_bit:m ~tree0:treeb ~tree1:treea
;;

let create ~size value = leaf size value;;

let rec rightmost_boundary = function
  | Branch{tree1} -> rightmost_boundary tree1
  | Leaf{key} -> key
;;

(* Total size of the interval map is the index of the last stored
   element. *)
let get_size = rightmost_boundary;;

(* [match_prefix k p m] returns [true] if and only if the key [k] has prefix [p] up to bit [m]. *)
let match_prefix k p m =
  mask k m = p
;;

(**************** Iterators ****************)


(* Fold on all boundaries in [node]. This is the fold function if we
   do not consider the tree as an interval map, but just as a map from
   boundaries to values. *)
let fold_on_boundaries acc node f =
  let rec loop acc node stack f =
    match node with
    | Leaf{key;value} ->
      (* Codex_log.feedback "folding: %d-%d" prev key; *)
      let acc = f key value acc in
      begin match stack with
        | [] -> acc
        | hd::tl -> loop acc hd tl f
      end
    | Branch{tree0;tree1} ->
      loop acc tree0 (tree1::stack) f
  in
  loop acc node [] f
;;


(* Small step iterators; hold the state necessary to iterate on the
   structure. It is lighter than a full zipper. *)
module Iterator = struct

  (* An iterator is a triple (prev,node,stack) where
     - prev is the index corresponding to the beginning of the first interval of node.
     - node is the node to process
     - stack is the list of nodes yet to process. *)
   type nonrec 'a t = int * 'a t * 'a t list
  
   (* Return the iterator for the position starting at start.

      The algorithm is like a linear search, except that we can skip
      subtrees where all values are smaller than the key. Still, we
      sometimes have to backtrack. *)
   let start key node =
     let exception Not_in_this_tree in
     let rec loop node stack=  
       match node with
       | Leaf{key=key'} when key' <= key -> raise Not_in_this_tree
       | Leaf _ -> (key,node,stack)
       | Branch{branching_bit;prefix;tree0;tree1} ->
         if (prefix + (branching_bit lsl 1)) <= key
         (* All the boundaries in this subtree are smaller than key; skip it. *)
         then raise Not_in_this_tree
         else
           try
             (* Note: we could also skip tree0 if prefix +
                branching_bit <= key, but this is already caught by the
                main cases, so we don't need it. *)
             loop tree0 (tree1::stack)
           with Not_in_this_tree -> loop tree1 stack
     in loop node []
   ;;

   (* Fold on all intervals starting on iter, and stop when we reach
      key (not included). *)
   let fold_upto upto acc iter f =
     let rec loop acc (prev,node,stack) =
       match node with
       | Leaf{key;value} ->
         (* Codex_log.feedback "calling f key %d upto %d %d %d" key upto prev ((min key upto)-prev); *)
         let acc = f ~size:((min key upto) - prev) prev value acc in
         begin match stack with
           | _ when key >= upto -> acc
           | hd::tl -> loop acc (key,hd,tl)
           | [] -> assert false     (* should not happen if upto is <= the size of the map. *)
         end
       | Branch{tree0;tree1} -> loop acc (prev,tree0,tree1::stack)
     in loop acc iter
   ;;

end

(* Big-step iterators based on small-step ones. *)

(* Fold on all intervals between from and ~from+size. *)
let fold_between ~size from node acc f =
    let iter = Iterator.start from node in
    let acc = Iterator.fold_upto (from + size) acc iter f in
    acc
;;

(* Iter on all intervals between from and ~from+size. *)
let iter_between ~size from node f =
  let f ~size key a () = f ~size key a in
  let () = fold_between ~size from node () f in
  ()
;;

let fold_on_diff (type a) (a:a t) b acc f =
  (* The optimizations below catch the cases where the nodes are the
     same; here we also catch the case where the value would be the
     same. *)
  let f ~size key va vb acc =
    if va == vb then acc
    else f ~size key va vb acc
  in

  let module Backlog = struct
    (* The f function requires that both values for va and vb are
       passed and known. Because values are mapped to the end of the
       interval and we process boundaries in order, we can be
       temporarily in a situation where we know the value for a and
       but not those for b, or vice-versa.

       To handle this, we create "backlogs" containing these
       intervals; if we have a backlog for a, then once we reach the
       end of the interval of b, we can process all the intervals
       for a that were in the backlog.

       When the boundaries for a and b is the same, we can
       completely empty the backlog.

       Note that either the backlog for a is empty, or the backlog
       for b is; we cannot have simultaneously two backlogs. *)

    (* Note: we could reuse actual leaves if we used a GADT to
       distinguish the leaf and branch cases. *)
    type leaf = { ival_end: int; value: a; }
  end in
  let open Backlog in

  (* We only have a boundary for a. Handle b backlog, and add to a backlog. *)
  let fa va enda (lastdone,backloga,backlogb,acc) =
    (* Codex_log.feedback "fa %d %d" starta enda; *)
    assert(backloga == [] || backlogb == []);
    let lastdone = Lazy.force_val lastdone in
    assert(lastdone < enda);            
    let lastdone,acc = List.fold_right (fun {ival_end;value} (lastdone,acc) ->
        ival_end,f ~size:(ival_end - lastdone) lastdone va value acc) backlogb (lastdone,acc) in
    (Lazy.from_val lastdone,{ival_end=enda;value=va}::backloga,[],acc)
  in
  (* We only have a boundary for b. Handle a backlog, and add to b backlog. *)
  let fb vb endb (lastdone,backloga,backlogb,acc) =
    assert(backloga == [] || backlogb == []);
    let lastdone = Lazy.force_val lastdone in      
    assert(lastdone < endb);      
    (* Codex_log.feedback "fb %d %d" startb endb;       *)
    let lastdone,acc = List.fold_right (fun {ival_end;value} (lastdone,acc) ->
        ival_end,f ~size:(ival_end - lastdone) lastdone value vb acc) backloga (lastdone,acc) in
    (Lazy.from_val lastdone,[],{ival_end=endb;value=vb}::backlogb,acc)
  in
  (* We have a boundary for both. Handle both backlogs, and contintue with empty backlogs. *)
  let fab va vb endab (lastdone,backloga,backlogb,acc) =
    let lastdone = Lazy.force_val lastdone in      
    assert(lastdone < endab);
    (* Codex_log.feedback "fab %d %d %d" starta startb endab;             *)
    assert(backloga == [] || backlogb == []);
    let lastdone, acc =
      if(backloga == []) then
        List.fold_right (fun {ival_end;value} (lastdone,acc) ->
            ival_end,f ~size:(ival_end - lastdone) lastdone va value acc) backlogb (lastdone,acc)
      else
        List.fold_right (fun {ival_end;value} (lastdone,acc) ->
            ival_end,f ~size:(ival_end - lastdone) lastdone value vb acc) backloga (lastdone,acc)
    in
    let acc = f ~size:(endab-lastdone) lastdone va vb acc in
    (Lazy.from_val endab,[],[],acc)
  in

  (* Unwind the stack. *)
  let go_up stack =
    match stack with
    | a::b -> (a,b)
    | [] -> assert false
  in

  (* Principle of the algorithm: we simultaneously iterate on each
     pair of intervals, in ascending order. The intervals are
     represented by patricia trees, mapping the end of the interval
     to their value.

     The functions fa,fb,fab must be called on each interval
     boundary in order, thus we always update the leftmost position.

     The current position is represented by a pair (nodex,stackx), where:

     - nodex is the current node 
     - stackx is the stack of nodes that remain to be traversed.

     In addition, lastdone represents the last boundary where
     the f function was called.
  *)
  let rec loop acc (nodea,stacka) (nodeb,stackb) =
    match nodea,nodeb with

    (* Optimisation: skip subtrees where both values are the same. *)
    | _ when nodea == nodeb ->
      let (lastdone,backloga,backlogb,acc) = acc in

      (* Handle the backlog, if there is one. *)
      let acc =
        assert(backloga == [] || backlogb == []);
        if backloga == [] && backlogb == []
        then acc    (* Fast case: no backlog *)
        else begin
          (* We have to find the value to handle the backlog. *)
          let vab = 
            let rec leftmost = function
              | Branch{tree0} -> leftmost tree0
              | Leaf{value} -> value
            in leftmost nodea
          in
          let lastdone = Lazy.force_val lastdone in
          (* Note: this is a part of the fab function. *)
          let lastdone, acc =
            if(backloga == []) then
              List.fold_right (fun {ival_end;value} (lastdone,acc) ->
                  ival_end,f ~size:(ival_end - lastdone) lastdone vab value acc) backlogb (lastdone,acc)
            else
              List.fold_right (fun {ival_end;value} (lastdone,acc) ->
                  ival_end,f ~size:(ival_end - lastdone) lastdone value vab acc) backloga (lastdone,acc)
          in
          acc
        end
      in

      (* The last boundary done is the rightmost in this interval,
         but we may not need it (if the next processed node is also
         the same), so we compute it lazily. *)
      let rightmost_boundary = lazy (rightmost_boundary nodea) in
      let acc = (rightmost_boundary,[],[],acc) in

      (* We may have reached the end of the two interval maps. *)
      if stacka == [] then begin
        assert(stackb == []);
        acc
      end
      else loop acc (go_up stacka) (go_up stackb)
    | Leaf{key=keya;value=va},Leaf{key=keyb;value=vb} ->
      if(keya < keyb) then begin
        let acc = fa va keya acc in
        loop acc (go_up stacka) (nodeb,stackb)
      end
      else if(keyb < keya) then begin
        let acc = fb vb keyb acc in
        loop acc (nodea,stacka) (go_up stackb)
      end
      else begin (* keya == keyb *)
        let acc = fab va vb keya acc in
        (* As the memories have the same size, this is the only case
           (with the optimized same-tree case) where the algorithm
           can terminate.  *)
        if stacka == [] then begin
          assert(stackb == []);
          acc
        end
        else loop acc (go_up stacka) (go_up stackb)
      end
    | Leaf{key=keya;value=va}, Branch{prefix=prefixb;branching_bit=bbb;tree0=t0b;tree1=t1b} ->
      (* This code may seem complex, as we could just do
         loop acc (nodea,stacka) (t0b,t1b::stackb).

         But it is optimised in two cases: 
         - If we don't need to descend inside nodeb, then we gain some time 
           (e.g. if we later find that nodeb and nodea are the same)
         - If the intervals do not overlap, we can just fold on the boundaries of b,
           and skip all the checks of this function.

         The difference is noticeable in the benchmarks. *)
      if true then
        (* Check if the intervals do not overlap. *)
        if keya < prefixb then begin
          (* The next common boundary is keya. *)
          let acc = fa va keya acc in
          loop acc (go_up stacka) (nodeb,stackb)
        end
        else
          (* We will handle all the boundaries in nodeb before handling keya. *)
          let end_of_interval = prefixb + (bbb lsl 1) in
          if keya >= end_of_interval then begin
            let acc = fold_on_boundaries acc nodeb (fun boundary value acc ->
                fb value boundary acc
              ) in
            loop acc (nodea,stacka) (go_up stackb)
          end
          else begin
            (* General case: the intervals overlap. *)
            loop acc (nodea,stacka) (t0b,t1b::stackb)
          end
      else   loop acc (nodea,stacka) (t0b,t1b::stackb)
    | Branch{prefix=prefixa;branching_bit=bba;tree0=t0a;tree1=t1a},Leaf{key=keyb;value=vb} ->
      (* This is the opposite version of the code above. *)
      if true then
        if keyb < prefixa then begin
          let acc = fb vb keyb acc in
          loop acc (nodea,stacka) (go_up stackb)
        end
        else
          let end_of_interval = prefixa + (bba lsl 1) in
          if keyb >= end_of_interval then begin
            let acc = fold_on_boundaries acc nodea (fun boundary value acc ->
                fa value boundary acc
              ) in
            loop acc  (go_up stacka) (nodeb,stackb)
          end
          else begin
            loop acc  (t0a,t1a::stacka) (nodeb,stackb)
          end
      else loop acc  (t0a,t1a::stacka) (nodeb,stackb)
    | Branch {tree0=t0a;tree1=t1a}, Branch {tree0=t0b;tree1=t1b} ->
      loop acc (t0a,t1a::stacka) (t0b,t1b::stackb)
  in
  let (lastdone,backloga,backlogb,acc) = loop (Lazy.from_val 0,[],[],acc) (a,[]) (b,[]) in
  assert(backloga == []);
  assert(backlogb == []);
  acc
;;  

(**************** Zippers ****************)

(* Algorithms based on zippers. These are useful when modifications
   are necessary. *)

(* This is a "special" zipper, because We try not to
   destruct/reconstruct values when feasible. Also, you cannot change
   the keys, only the value. *)
type 'a zipper =
  | End
  | Branch0 of 'a t * 'a zipper
  | Branch1 of 'a t * 'a zipper
;;

  let rec unzip node zipper =
    match zipper with
    | End -> node
    | Branch0(Branch{prefix;branching_bit;tree1},zipper) ->
      unzip (branch ~prefix ~branching_bit ~tree0:node ~tree1) zipper
    | Branch1(Branch{prefix;branching_bit;tree0},zipper) ->
      unzip (branch ~prefix ~branching_bit ~tree1:node ~tree0) zipper
    | Branch0(Leaf _,_) -> assert false
    | Branch1(Leaf _,_) -> assert false
  ;;


  let rec leftmost_child node zipper =
    match node with
    | Leaf _ -> node,zipper
    | Branch{tree0} -> leftmost_child tree0 (Branch0(node,zipper))
  ;;

  exception Rightmost

  (* Move from leaves to leaves; no need to update the nodes as they
     are unmodified. *)
  let rec move_right_unmodified zipper =
    match zipper with
    | Branch0(Branch{tree1} as node,zipper) -> leftmost_child tree1 (Branch1(node,zipper))
    | Branch1(Branch _,zipper) -> move_right_unmodified zipper
    | End -> raise Rightmost       (* Cannot go past the last boundary. *)
    | Branch1(Leaf _,_) -> assert false
    | Branch0(Leaf _,_) -> assert false      
  ;;

  let rec rightmost_child node zipper =
    match node with
    | Leaf _ -> node,zipper
    | Branch{tree1} -> rightmost_child tree1 (Branch1(node,zipper))
  ;;

  exception Leftmost

  (* Move from leaves to leaves; no need to update the nodes as they
     are unmodified. *)
  let rec move_left_unmodified zipper =
    match zipper with
    | Branch1(Branch{tree0} as node,zipper) -> rightmost_child tree0 (Branch0(node,zipper))
    | Branch0(Branch _,zipper) -> move_left_unmodified zipper
    | End -> raise Leftmost       (* Cannot go past the last boundary. *)
    | Branch0(Leaf _,_) -> assert false
    | Branch1(Leaf _,_) -> assert false      
  ;;

  (* Maybe: stitch intervals which contain the same value. *)
  (* Try to keep the nodes that were not changed. *)
let rec move_right node zipper =
    match zipper with
    | Branch1(Branch{prefix;branching_bit;tree0;tree1} as n,zipper) ->
      if node == tree1
      then move_right n zipper
      else move_right (branch ~prefix ~branching_bit ~tree0 ~tree1:node) zipper
    | Branch0(Branch{prefix;branching_bit;tree1;tree0} as n,zipper) ->
      let node =
        if tree0 == node then n
        else branch ~prefix ~branching_bit ~tree0:node ~tree1 in
      let zipper = Branch1(node,zipper) in
      leftmost_child tree1 zipper
    | End -> assert false       (* Cannot go past the last boundary. *)
    | Branch1(Leaf _,_) -> assert false
    | Branch0(Leaf _,_) -> assert false
  ;;
  
  let delete_and_move_right zipper =
    match zipper with
    | Branch1(Branch{tree0},zipper) -> move_right tree0 zipper
    | Branch0(Branch{tree1},zipper) -> leftmost_child tree1 zipper
    | End -> assert false
    | Branch1(Leaf _,_) -> assert false
    | Branch0(Leaf _,_) -> assert false
  ;;

  (* Split and choose left. *)
  (* Unzip, then regular add. *)
  (* let rec split key node zipper = *)
  

  (* Create a new interface boundary at key, and go to the left of this interval. *)
   let rec split_and_choose_left key node zipper =
     let get_value zipper = 
        match fst @@ move_right_unmodified zipper with
        | Branch _ -> assert false
        | Leaf{value} -> value
     in
     match node with
     | Leaf {key=key';value=data} when key' == key ->
       (* No need to split: already exists. *) node,zipper
     | Leaf {key=key';value=data} when key' < key ->
       let value = get_value zipper in
       let n  = join key (leaf key value) key' node in
       (match n with
        | Leaf _ -> assert false
        | Branch{branching_bit;prefix;tree0;tree1} ->
          (* Printf.printf "key %d key' %d branching_bit %d" key key' branching_bit; *)
          assert ((key land branching_bit) != 0);
          tree1,Branch1(n,zipper))
     (* If possible: move right untouched to get the data, then split. *)
     | Leaf {key=key';value=data} (* when key' > key *) ->
       let n = join key' node  key (leaf key data) in
       (match n with
        | Leaf _ -> assert false
        | Branch{branching_bit;prefix;tree0;tree1} ->
          assert ((key land branching_bit) == 0);
          tree0,Branch0(n,zipper))
     | Branch{prefix;branching_bit;tree0;tree1} ->
      if match_prefix key prefix branching_bit
      then
        if (key land branching_bit) == 0
        then split_and_choose_left key tree0 (Branch0(node,zipper))
        else split_and_choose_left key tree1 (Branch1(node,zipper))
      else
        (* Get the value from the next interval; which depends on
           where we are going to be put. *)
        let value = 
          if prefix + (branching_bit lsl 1) <= key
          then get_value zipper
          else
            let rec leftmost = function
              | Leaf{value} -> value
              | Branch{tree0} -> leftmost tree0
            in leftmost tree0 
        in
        let n = join key (leaf key value) prefix node in
        (match n with
         | Leaf _ -> assert false
         | Branch{branching_bit;tree0;tree1} ->
           if (key land branching_bit == 0)
           then tree0, (Branch0(n,zipper))
           else tree1, (Branch1(n,zipper)))
  ;;

  (* We do not need to unzip completely, but it is simpler to
     implement this way. *)
  let split_and_choose_left key node zipper =
    let node = unzip node zipper in
    split_and_choose_left key node End
  ;;


  (* This does not start from an initial zipper. This is how we begin a
    write. *)
 let split_and_choose_right key node =
  if key == 0 then leftmost_child node End
  else
    let n,z = split_and_choose_left key node End in
    move_right n z
 ;;

  let go_to_right_of key node =
    let exception Too_much_on_the_left in
    let rec loop node zipper =
      match node with
      | Leaf{key=key';value=data} when key' <= key -> raise Too_much_on_the_left
      | Leaf{key=key';value=data} -> node,zipper
      | Branch{branching_bit;prefix;tree0;tree1} ->
        (* Printf.printf "prefix %d branch %d key %d\n" prefix branching_bit key; *)
        if prefix + (branching_bit lsl 1) <= key (* The maximum possible key. *)
        then raise Too_much_on_the_left
        else
          try loop tree0 (Branch0(node,zipper))
          with Too_much_on_the_left -> loop tree1 (Branch1(node,zipper))
   in 
   let (node,zipper) as res = loop node End in
   assert (match node with Leaf{key=key'} when key' > key -> true | _ -> false);
   res
;;

  let store ~size first_key map newv =
  (* We inspect the interval starting at first_key, maybe splitting to
     get it. *)
  let node,zipper = split_and_choose_right first_key map in
  let end_ = first_key + size in
  let rec loop node zipper =
    match node with
    | Branch _ -> assert false
    | Leaf{key=key';value=v} when key' == end_ -> (leaf end_ newv), zipper
    | Leaf{key=key';value=v} when key' < end_ ->
      (* Codex_log.debug "key' %d end_ %d" key' end_; *)
      let node,zipper = delete_and_move_right zipper in
      loop node zipper
    | Leaf{key=key';value=v} when key' > end_ ->
      let node,zipper = split_and_choose_left end_ node zipper in
      (match node with
       | Branch _ -> assert false
       | Leaf{key=k;value=v'} -> assert (v' == v); assert (k == end_); (leaf k newv),zipper
      )
    | _ -> assert false       (* Delete and move right *)
  in
  let node,zipper = loop node zipper in
  unzip node zipper
;;


(* let region1 = create ~size:8 11;; *)
(* let region2 = store ~size:4 4 region1 22;; *)
(* let region3 = store ~size:4 4 region2 33;; *)
(* let region4a = store ~size:4 0 region1 44;; *)
(* let region4b = store ~size:4 2 region2 44;; *)

let _old_iter_between ~size first_key map f =
  let node,zipper = go_to_right_of first_key map in
  let last_key = first_key + size in
  let rec loop start node zipper =
    let key',v = match node with
      | Branch _ -> assert false
      | Leaf{key=key';value=v} ->
        assert (key' > first_key);
        key',v
    in
    let end_ = min key' last_key in
    f ~size:(end_ - start) start v;
    if key' < last_key
    then
      let node,zipper = move_right_unmodified zipper in
      loop key' node zipper
    else ()
  in loop first_key node zipper
;;


let _old_fold_between ~size first_key map acc f =
  let node,zipper = go_to_right_of first_key map in
  let last_key = first_key + size in
  let rec loop acc start node zipper =
    let key',v = match node with
      | Branch _ -> assert false
      | Leaf{key=key';value=v} ->
        assert (key' > first_key);
        key',v
    in
    let end_ = min key' last_key in
    assert(end_ > start);
    let acc = f ~size:(end_ - start) start v acc in
    if key' < last_key
    then
      let node,zipper = move_right_unmodified zipper in
      loop acc key' node zipper
    else acc
  in loop acc first_key node zipper
;;



(* let f ~size idx value = Printf.printf "idx %d size %d value %d\n" idx size value;; *)

(* let reg1 = create ~size:16 11;; *)
(* let reg2 = store ~size:4 8 reg1 22;; *)
(* let node,zipper = go_to_right_of 4 reg2;; *)
(* iter ~size:4 4 reg2 f;; *)

(* let reg1 = create ~size:16 11;; *)
(* let reg2 = store ~size:4 12 reg1 22;; *)
(* let reg3 = store ~size:4 8 reg2 33;; *)
(* let reg4 = store ~size:4 4 reg3 44;; *)
(* let node,zipper = split_and_choose_right 4 reg3;; *)
(* (\* let node,zipper = go_to_right_of 4 reg4;; *\) *)
(* iter ~size:4 4 reg2 f;; *)



(* Printf.printf "\n----\n";; *)
(* iter ~size:8 0 region4b f;; Printf.printf "----\n";; *)
(* iter ~size:7 1 region4b f;; Printf.printf "----\n";; *)
(* iter ~size:6 2 region4b f;; Printf.printf "----\n";; *)
(* iter ~size:5 3 region4b f;; Printf.printf "----\n";; *)
(* iter ~size:4 4 region4b f;; Printf.printf "----\n";; *)
(* iter ~size:3 5 region4b f;; Printf.printf "----\n";; *)
(* iter ~size:2 6 region4b f;; Printf.printf "----\n";; *)
(* iter ~size:1 7 region4b f;; Printf.printf "----\n";; *)

(* This version does not skip subtrees, but is still quite efficient. *)
let _old_fold_on_diff a b acc f =
  let nodea,zippera = go_to_right_of 0 a in
  let nodeb,zipperb = go_to_right_of 0 b in
  let rec loop prev (nodea, zippera) (nodeb, zipperb) acc =
    (* Advance the smallest boundary, or advance both. *)
    match nodea,nodeb with
    | Leaf{key=idxa;value=va},Leaf{key=idxb;value=vb} ->
      (* Printf.printf "folddiff: prev %d idxa %d idxb %d\n" prev idxa idxb; *)
      if idxa == idxb then
        let acc =
          if va == vb
          then (Codex_log.performance_warning "fold_on_diff is inefficient: should skip entire trees"; acc)
          else f ~size:(idxb - prev) prev va vb acc
        in
        let res = 
          try 
            let nexta = move_right_unmodified zippera in
            let nextb = move_right_unmodified zipperb in
            Some(nexta,nextb)
          with Rightmost -> None
        in (match res with
        | Some(nexta,nextb) -> loop idxa nexta nextb acc
        | None -> acc)
      else if idxa < idxb then
        let acc = f ~size:(idxa - prev) prev va vb acc in
        loop idxa (move_right_unmodified zippera) (nodeb,zipperb) acc
      else (* idxa > idxb *)
        let acc = f ~size:(idxb - prev) prev va vb acc in
        loop idxb (nodea,zippera) (move_right_unmodified zipperb) acc
    | _,_ -> assert false

    
  in
  loop 0 (nodea, zippera) (nodeb, zipperb) acc
;;

let fold_on_diff3 a b c acc f =
  let nodea,zippera = go_to_right_of 0 a in
  let nodeb,zipperb = go_to_right_of 0 b in
  let nodec,zipperc = go_to_right_of 0 c in  
  let rec loop prev (nodea, zippera) (nodeb, zipperb) (nodec, zipperc) acc =
    (* Advance the smallest boundary, or advance both. *)
    match nodea,nodeb,nodec with
    | Leaf{key=idxa;value=va},Leaf{key=idxb;value=vb},Leaf{key=idxc;value=vc} ->
      let all_equal () =
        (* Printf.printf "folddiff: prev %d idxa %d idxb %d\n" prev idxa idxb; *)
        (* if idxa == idxb && idxa == idxc then *)
        let acc =
          if va == vb && va == vc
          then (Codex_log.performance_warning "fold_on_diff3 is inefficient: should skip entire trees"; acc)
          else f ~size:(idxb - prev) prev va vb vc acc
        in
        let res = 
          try 
            let nexta = move_right_unmodified zippera in
            let nextb = move_right_unmodified zipperb in
            let nextc = move_right_unmodified zipperc in            
            Some(nexta,nextb,nextc)
          with Rightmost -> None
        in (match res with
        | Some(nexta,nextb,nextc) -> loop idxa nexta nextb nextc acc
        | None -> acc)
      and a_smallest () =
        let acc = f ~size:(idxa - prev) prev va vb vc acc in
        loop idxa (move_right_unmodified zippera) (nodeb,zipperb) (nodec,zipperc) acc
      and b_smallest () =
        let acc = f ~size:(idxb - prev) prev va vb vc acc in
        loop idxb (nodea,zippera) (move_right_unmodified zipperb) (nodec,zipperc) acc
      and c_smallest () =
        let acc = f ~size:(idxc - prev) prev va vb vc acc in
        loop idxc (nodea,zippera) (nodeb,zipperb) (move_right_unmodified zipperc) acc
      and ab_smallest () =
        let acc = f ~size:(idxa - prev) prev va vb vc acc in
        loop idxa (move_right_unmodified zippera) (move_right_unmodified zipperb) (nodec,zipperc) acc
      and bc_smallest () =
        let acc = f ~size:(idxb - prev) prev va vb vc acc in
        loop idxb (nodea,zippera) (move_right_unmodified zipperb) (move_right_unmodified zipperc) acc
      and ac_smallest () =
        let acc = f ~size:(idxa - prev) prev va vb vc acc in
        loop idxa (move_right_unmodified zippera)  (nodeb,zipperb) (move_right_unmodified zipperc) acc
      in
      if idxa == idxb then
        if idxa == idxc then all_equal ()
        else if idxc < idxa then c_smallest ()
        else ab_smallest ()
      else if idxa < idxb then
        if idxc == idxa then ac_smallest ()        
        else if idxc < idxa then c_smallest ()
        else a_smallest ()      (* idxa < idxc *)
      else                      (* idxb < idxa *)
      if idxc == idxb then bc_smallest ()
      else if idxc < idxb then c_smallest ()
      else b_smallest ()
    | _ -> assert false

    
  in
  loop 0 (nodea, zippera) (nodeb, zipperb) (nodec, zipperc) acc
;;

let subst_between key ~size map f =
  fold_between ~size key map map (fun ~size offset value acc ->
      let new_ = f ~size offset value in
      store ~size offset acc new_
    )
;;


module With_Extract(Value:sig
  type t
  end)= struct

  (* We store the key and size used when the value was stored. *)
  (* Note: this structure with three fields take four words, so is a small size for the GC?
  *)
  type wrapper = {
    key: int;                   
    size: int;
    value: Value.t;
  }

  type 'a map = 'a t
  type t = wrapper map

  let rec dump_tree fmt = function
    | Leaf{key;value={key=key';size=size'}} -> Format.fprintf fmt "(leaf %x orig: %x-%x)" key key' (key'+size')
    | Branch{prefix;branching_bit;tree0;tree1} ->
      Format.fprintf fmt "@[<hov>(branch p:%x@ bb:%x@ %a@ %a)@]"
        prefix branching_bit dump_tree tree0 dump_tree tree1
  ;;

  
  let create ~size value = create ~size {size;key=0;value};;
  let get_size = get_size 

  (* The store is unchanged. *)
  let store  ~size key map value = store ~size key map {size;key;value};;

  let do_extract ~extract ~size key {size=stored_size;key=stored_key;value} =
     let idx = key - stored_key in
     (* Codex_log.feedback "idx %d size %d stored_size %d key %d stored_key %d" idx size stored_size key stored_key; *)
     assert(idx >= 0);
     assert(idx + size <= stored_size);
     extract value ~idx ~size ~oldsize:stored_size
  ;;

  (* We do the eventual (rare) extractions upon iteration.  Note that
     several iter means several calls to extract, which is OK if lower
     level perform common subexpression elimination/hashconsing. *)
  let iter_between ~size start map ~extract f =
    let f' ~size key value =
      f ~size key (do_extract ~extract ~size key value )
    in iter_between ~size start map f'
  ;;

  let fold_between ~size start map ~extract acc f =
    assert(size != 0);
    let f' ~size key value acc =
      f ~size key (do_extract ~extract ~size key value) acc
    in fold_between ~size start map acc f'
  ;;
  
  let fold_on_diff a b acc ~extracta ~extractb (* ~equal *) f =
    (* Codex_log.feedback "with_extract.fold on diff %a %a" dump_tree a dump_tree b;     *)
    let f' ~size key a b acc =
      if a == b || (a.value == b.value && a.key == b.key && a.size == b.size) then acc
      (* Printf.printf "extract fold diff: key %d size %d\n" key size; *)
      else f ~size key (do_extract ~extract:extracta ~size key a) (do_extract ~extract:extractb ~size key b) acc
    in
    fold_on_diff a b acc f'
  ;;

  let fold_on_diff3 a b c acc ~extract f =
    let f' ~size key a b c acc =
      (* Printf.printf "extract fold diff: key %d size %d\n" key size; *)
      let a' = do_extract ~extract ~size key a in
      let b' = do_extract ~extract ~size key b in
      let c' = do_extract ~extract ~size key c in
      if a' == b' && b' == c' then acc
      else
      f ~size key a' b' c' acc
    in
    fold_on_diff3 a b c acc f'
  ;;
  

  let subst_between key ~size map ~extract f =
    let f' ~size key (v:wrapper) =
      let init = (do_extract ~extract ~size key v) in
      let res = f ~size key init in
      if res == init
      then v
      else {key = key; size = size; value = res}
    in
    subst_between key ~size map f'
  ;;
  
end
