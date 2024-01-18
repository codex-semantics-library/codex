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


module type S = sig
  type 'a t
  type key
  val empty: 'a t
  val singleton: key -> 'a -> 'a t
  val find: key -> 'a t -> 'a
  val find_opt: key -> 'a t -> 'a option
  val remove: key -> 'a t -> 'a t
  val insert : (old:'a -> value:'a -> 'a) -> key -> 'a -> 'a t -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val cardinal: 'a t -> int
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (int -> 'a -> 'b) -> 'a t -> 'b t
  val min_elt: 'a t -> (int * 'a)
  val max_elt: 'a t -> (int * 'a)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val unioni : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val interi : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val interi_filter : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t              

  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val iter : (int -> 'a -> unit) -> 'a t -> unit
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_list : 'a t -> (int * 'a) list
  (* val union_inter:
   *   funion:('a -> 'a -> 'a) ->
   *   finter:('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t -> 'a t *)

  val is_singleton : 'a t -> 'a option
end

(* A mask is an integer with a single bit set (i.e. a power of 2). *)
type mask = int

(* Optimized computation, but does not work for values too high. *)
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

let lowest_bit x =
  x land (-x)

let rec highest_bit x =
  let m = lowest_bit x in
  if x = m then
    m
  else
    highest_bit (x - m)
;;

let highest_bit x =
  1 lsl (Z.log2 @@ Z.of_int x)

(* Note: in the original version, okasaki give the masks as arguments
   to optimize the computation of highest_bit. *)
let branching_bit a b = highest_bit (a lxor b);;

let mask i m =
  i land (lnot (2*m-1));;

type key = int

(* We provide two versions: with or without hash-consing. Hash-consing
   allows faster implementations for the fold_on_diff* operations.
   Benchmarks seems to indicate that hashconsing and the more complex
   fold_on_diff are not very useful in practice (perhaps they would on
   huge structures?) *)

module TNoHashCons:sig 
  type 'a t = private
    | Empty                      (* Does not appear below interior nodes. *)
    | Leaf of {key:key; value:'a}            (* The entire key. *)
    (* Branching bit contains only one bit set; the corresponding mask is
       (branching_bit - 1).  The prefixes are normalized: the bits
       below the branching bit are set to zero (i.e. prefix &
       (branching_bit - 1) = 0). *)
    | Branch of {prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}
  val empty: 'a t
  val leaf: key -> 'a -> 'a t
  val branch: prefix:key -> branching_bit:mask -> tree0:'a t -> tree1:'a t -> 'a t
  
end = struct

  type 'a t =
    | Empty
    | Leaf of {key:key;value: 'a}            (* The entire key. *)
    | Branch of {prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}

  let empty = Empty
  let leaf key value  = Leaf {key;value}
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    match tree0,tree1 with
    | Empty, x -> x
    | x, Empty -> x
    | _ -> Branch{prefix;branching_bit;tree0;tree1}

end

(* With hash-consing of interior nodes: slower node construction, but
   faster comparison with fold_on_diff. *)
module THashCons:sig

  type 'a t = private
    | Empty
    | Leaf of {id:int;key:key; value:'a }            (* The entire key. *)
    (* Branching bit contains only one bit set; the corresponding mask is
       (branching_bit - 1).  The prefixes are normalized: the bits
       below the branching bit are set to zero (i.e. prefix &
       (branching_bit - 1) = 0). *)
    | Branch of {id:int;prefix:key;branching_bit:mask;tree0:'a t;tree1:'a t}

  val empty: 'a t
  val leaf: key -> 'a -> 'a t
  val branch: prefix:key -> branching_bit:mask -> tree0:'a t -> tree1:'a t -> 'a t
  
end = struct

  let id_count = ref 1;;
  
  type 'a t =
    | Empty
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
        | Empty -> 0
        | Leaf{id} -> id
        | Branch{id} -> id
      
      let [@warning "-8"] hash (Branch{prefix;branching_bit;tree0;tree1}) =
        sdbm (prefix lor branching_bit) @@ sdbm (get_id tree0) (get_id tree1)
      ;;
      
    end
    );;


  let weakh = WeakH.create 120;;

  let empty = Empty
  
  let leaf key value  =
    let id = !id_count in
    incr id_count;
    Leaf {id;key;value};;
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    match tree0,tree1 with
    | Empty, x | x, Empty -> x
    | _ ->
      let id = !id_count in
      let tentative = Branch{id;prefix;branching_bit;tree0;tree1} in
      let v = WeakH.merge weakh (Obj.magic tentative) in
      let v = Obj.magic v in
      if v == tentative then ((* Codex_log.debug "HASHCONS WORKS"; *) incr id_count);
      v
  ;;
    
end

include TNoHashCons;;
(* include THashCons;; *)

let rec min_elt = function
  | Empty -> raise Not_found
  | Leaf{key;value} -> (key,value)
  | Branch{tree0;_} -> min_elt tree0
let rec max_elt = function
  | Empty -> raise Not_found
  | Leaf{key;value} -> (key,value)
  | Branch{tree1;_} -> max_elt tree1

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Branch{tree0; tree1; _} -> cardinal tree0 + cardinal tree1

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


(* [match_prefix k p m] returns [true] if and only if the key [k] has prefix [p] up to bit [m]. *)
let match_prefix k p m =
  mask k m = p
;;

let singleton = leaf 

let rec find searched = function
  | Leaf{key;value} when key == searched -> value
  | Empty | Leaf _ -> raise Not_found
  | Branch{prefix;branching_bit;tree0;tree1} ->
    (* Optional if not (match_prefix searched prefix branching_bit) then raise Not_found
    else *) if (branching_bit land searched == 0)
    then find searched tree0
    else find searched tree1

let rec find_opt searched = function
  | Leaf{key;value} when key == searched -> Some value
  | Empty | Leaf _ -> None
  | Branch{prefix;branching_bit;tree0;tree1} ->
    (* Optional if not (match_prefix searched prefix branching_bit) then raise Not_found
    else *) if (branching_bit land searched == 0)
    then find_opt searched tree0
    else find_opt searched tree1

let rec map f = function
  | Empty -> empty
  | Leaf{key;value} -> leaf key (f value)
  | Branch{prefix;branching_bit;tree0;tree1} ->
    branch ~prefix ~branching_bit ~tree0:(map f tree0) ~tree1:(map f tree1)

let rec mapi f = function
  | Empty -> empty
  | Leaf{key;value} -> leaf key (f key value)
  | Branch{prefix;branching_bit;tree0;tree1} ->
    branch ~prefix ~branching_bit ~tree0:(mapi f tree0) ~tree1:(mapi f tree1)


let rec remove to_remove = function
  | Leaf{key;value} when key == to_remove -> empty
  | (Empty | Leaf _) as m -> m
  | Branch{prefix;branching_bit;tree0;tree1} as m ->
    if (branching_bit land to_remove) == 0
    then begin
      let tree0' = remove to_remove tree0 in
      if tree0' == empty then tree1
      else if tree0' == tree0 then m 
      else branch ~prefix ~branching_bit ~tree0:tree0' ~tree1
    end
    else begin
      let tree1' = remove to_remove tree1 in
      if tree1' == empty then tree0 
      else if tree1' == tree1 then m 
      else branch ~prefix ~branching_bit ~tree0 ~tree1:tree1'
    end
;;


let insert f thekey value t =
  (* Preserve physical equality whenever possible. *)
  let exception Unmodified in
  try 
    let rec loop = function
      | Empty -> leaf thekey value
      | Leaf{key;value=old} as t ->
        if thekey == key
        then
          let newv = f ~old ~value in
          if newv == old then raise Unmodified
          else leaf key newv
        else join thekey (leaf thekey value) key t
      | Branch{prefix;branching_bit;tree0;tree1} as t ->
        if match_prefix thekey prefix branching_bit then
          if (thekey land branching_bit) == 0
          then branch ~prefix ~branching_bit ~tree0:(loop tree0) ~tree1
          else branch ~prefix ~branching_bit ~tree0 ~tree1:(loop tree1)
        else join thekey (leaf thekey value) prefix t
    in loop t
  with Unmodified -> t
;;

let add key value t = insert (fun ~old ~value -> value) key value t;;

(* Fast equality test between two maps. *)
let rec equal f ta tb = match ta,tb with 
  | _ when ta == tb -> true
  | Empty, _ | _, Empty -> false
  | Leaf _, Branch _ | Branch _, Leaf _ -> false
  | Leaf{key=keya;value=valuea}, Leaf{key=keyb;value=valueb} -> keya == keyb && f valuea valueb
  | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
    Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
    pa == pb && ma == mb && equal f ta0 tb0 && equal f ta1 tb1

let rec compare f ta tb = match ta,tb with 
  | _ when ta == tb -> 0
  | Empty, _
  | Leaf _, Branch _ -> -1
  | _, Empty 
  | Branch _, Leaf _-> 1
  | Leaf{key=keya;value=valuea}, Leaf{key=keyb;value=valueb} -> 
      let cmp = Int.compare keya keyb in
      if cmp <> 0 then cmp else f valuea valueb
  | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
    Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      let cmp = Int.compare pa pb in
      if cmp <> 0 then cmp else
      let cmp = Int.compare ma mb in
      if cmp <> 0 then cmp else
      let cmp = compare f ta0 tb0 in
      if cmp <> 0 then cmp else 
      compare f ta1 tb1

let rec unioni f ta tb =
  match ta,tb with
  | _ when ta == tb -> ta
  | Empty, x | x, Empty -> x
  (* Bugfix: I want to call the argument from ta as the first argument. *)
  | Leaf{key;value},t -> insert (fun ~old ~value -> f key value old) key value t 
  | t,Leaf{key;value} -> insert (fun ~old ~value -> f key old value) key value t
  | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
    Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
    if ma == mb && pa == pb
    (* Same prefix: merge the subtrees *)
    then
      (* MAYBE: if ta0 == tb0 and ta1 == tb1, we can return ta (or
         tb). Probably not useful. *)
      branch ~prefix:pa ~branching_bit:ma ~tree0:(unioni f ta0 tb0) ~tree1:(unioni f ta1 tb1)
    else if ma > mb && match_prefix pb pa ma
    then if ma land pb == 0
      then branch ~prefix:pa ~branching_bit:ma ~tree0:(unioni f ta0 tb) ~tree1:ta1
      else branch ~prefix:pa ~branching_bit:ma ~tree0:ta0 ~tree1:(unioni f ta1 tb)
    else if ma < mb && match_prefix pa pb mb
    then if mb land pa == 0
      then branch ~prefix:pb ~branching_bit:mb ~tree0:(unioni f ta tb0) ~tree1:tb1
      else branch ~prefix:pb ~branching_bit:mb ~tree0:tb0 ~tree1:(unioni f ta tb1)
    else join pa ta pb tb
;;

let rec interi f ta tb =
  match ta,tb with
  | _ when ta == tb -> ta
  | Empty, _ | _, Empty -> empty
  | Leaf{key;value},t ->
    (try let res = find key t in
      leaf key (f key value res)
     with Not_found -> empty)
  | t,Leaf{key;value} ->
    (try let res = find key t in
      leaf key (f key res value)
    with Not_found -> empty)
  | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
    Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
    if ma == mb && pa == pb
    (* Same prefix: merge the subtrees *)
    then branch ~prefix:pa ~branching_bit:ma ~tree0:(interi f ta0 tb0) ~tree1:(interi f ta1 tb1)
    else if ma > mb && match_prefix pb pa ma
    then if ma land pb == 0
      then interi f ta0 tb
      else interi f ta1 tb
    else if ma < mb && match_prefix pa pb mb
    then if mb land pa == 0
      then interi f ta tb0
      else interi f ta tb1
    else empty
;;

let rec interi_filter f ta tb =
  match ta,tb with
  | _ when ta == tb -> ta
  | Empty, _ | _, Empty -> empty
  | Leaf{key;value},t ->
    (try let res = find key t in
       match (f key value res) with
       | Some v -> leaf key v
       | None -> empty
     with Not_found -> empty)
  | t,Leaf{key;value} ->
    (try let res = find key t in
       match f key res value with
       | Some v -> leaf key v
       | None -> empty
    with Not_found -> empty)
  | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
    Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
    if ma == mb && pa == pb
    (* Same prefix: merge the subtrees *)
    then branch ~prefix:pa ~branching_bit:ma ~tree0:(interi_filter f ta0 tb0) ~tree1:(interi_filter f ta1 tb1)
    else if ma > mb && match_prefix pb pa ma
    then if ma land pb == 0
      then interi_filter f ta0 tb
      else interi_filter f ta1 tb
    else if ma < mb && match_prefix pa pb mb
    then if mb land pa == 0
      then interi_filter f ta tb0
      else interi_filter f ta tb1
    else empty
;;

(* let rec mergei f ta tb = *)
(*   match ta,tb with *)
(*   | _ when ta == tb -> ta *)
(*   | Empty, tb -> map (fun key vb -> f key None (Some vb)) tb *)
(*   | ta, Empty -> map (fun key va -> f key (Some va) None) tb *)
(*   | Leaf{key;value},t -> insert (fun ~old ~value -> f key old value) key value t  *)
(*   | t,Leaf{key;value} -> insert (fun ~old ~value -> f key value old) key value t *)
(*   | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1}, *)
(*     Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} -> *)
(*     if ma == mb && pa == pb *)
(*     (\* Same prefix: merge the subtrees *\) *)
(*     then *)
(*       (\* MAYBE: if ta0 == tb0 and ta1 == tb1, we can return ta (or *)
(*          tb). Probably not useful. *\) *)
(*       branch ~prefix:pa ~branching_bit:ma ~tree0:(unioni f ta0 tb0) ~tree1:(unioni f ta1 tb1) *)
(*     else if ma > mb && match_prefix pb pa ma *)
(*     then if ma land pb == 0 *)
(*       then branch ~prefix:pa ~branching_bit:ma ~tree0:(unioni f ta0 tb) ~tree1:ta1 *)
(*       else branch ~prefix:pa ~branching_bit:ma ~tree0:ta0 ~tree1:(unioni f ta1 tb) *)
(*     else if ma < mb && match_prefix pa pb mb *)
(*     then if mb land pa == 0 *)
(*       then branch ~prefix:pb ~branching_bit:mb ~tree0:(unioni f ta tb0) ~tree1:tb1 *)
(*       else branch ~prefix:pb ~branching_bit:mb ~tree0:tb0 ~tree1:(unioni f ta tb1) *)
(*     else join pa ta pb tb *)
(* ;; *)






let union f = unioni (fun _ a b -> f a b)
let inter f = interi (fun _ a b -> f a b)    

let rec iter f x = match x with
  | Empty -> ()
  | Leaf{key;value} -> f key value
  | Branch{tree0;tree1} -> iter f tree0; iter f tree1
let rec fold f x acc = match x with
  | Empty -> acc
  | Leaf{key;value} -> f key value acc
  | Branch{tree0;tree1} -> fold f tree1 (fold f tree0 acc)
let to_list m = fold (fun k v a -> (k,v)::a) m []

let is_singleton = function
  | Leaf{value; _} -> Some value
  | _ -> None

(**************** Tests ****************)
let _m1 = singleton 7 1;;
let _m1 = add 3 2 _m1;;
let _m1 = remove 7 _m1;;


let _m2 = singleton 4 3;;
let _m2 = add 10 4 _m2;;

let _m3 = union (Obj.magic 0) _m1 _m2;;

(* let m8 = m1;; *)
let _m4 = inter (fun a b -> a) _m1 _m3;;
let _m5 = inter (fun a b -> a) _m2 _m3;;

let _m6 = inter (fun a b -> a) _m1 _m2;;

module Make(Key:sig
    type t
    val to_int: t -> int
  end) = struct
  type key = Key.t
  type 'a map = 'a t
  type 'a t = 'a map
  let empty = empty
  let singleton x v = singleton (Key.to_int x) v
  let find x m = find (Key.to_int x) m
  let find_opt x m = find_opt (Key.to_int x) m
  let remove x m = remove (Key.to_int x) m      
  let insert f k v m = insert f (Key.to_int k) v m
  let add k v m = add (Key.to_int k) v m
  let cardinal = cardinal
  let map = map
  let mapi = mapi
  let min_elt = min_elt
  let max_elt = max_elt
  let equal = equal
  let compare = compare
  let union = union
  let inter = inter
  let unioni = unioni
  let interi = interi
  let interi_filter = interi_filter    
  let iter = iter
  let fold = fold
  let to_list = to_list
  (* let union_inter = union_inter *)
  let is_singleton = is_singleton
end

(* This version also stores the key just so that we can pretty-print the map. *)
module MakePretty(Key:sig
    type t
    val to_int: t -> int
    val pp: Format.formatter -> t -> unit
  end) = struct
  type key = Key.t
  type 'a map = (Key.t * 'a) t
  type 'a t = 'a map
  let empty = empty
  let singleton x v = singleton (Key.to_int x) (x,v)
  let find x m = snd @@ find (Key.to_int x) m
  let find_opt x m = Option.map snd @@ find_opt (Key.to_int x) m
  let remove x m = remove (Key.to_int x) m      
  let insert f k v m =
    let x = Key.to_int k in
    insert (fun ~old:(_,old) ~value:(_,value) -> (k,f ~old ~value)) x (k,v) m
  let add k v m = add (Key.to_int k) (k,v) m
  let cardinal = cardinal
  let map f m = map (fun (k,v) -> (k, f v)) m
  let mapi f m = mapi (fun x (k,v) -> (k,f x v)) m
  let min_elt m = let (a,(_,b)) = min_elt m in (a,b)
  let max_elt m = let (a,(_,b)) = max_elt m in (a,b)
  let equal f = equal (fun (_,va) (_,vb) -> f va vb)
  let compare f = compare (fun (_,va) (_,vb) -> f va vb)
  let unioni f a b = unioni (fun x (keya,va) (keyb,vb) -> keya, f x va vb) a b
  let interi f a b = interi (fun x (keya,va) (keyb,vb) -> keya, f x va vb) a b
  let interi_filter f a b = interi_filter (fun x (keya,va) (keyb,vb) ->
      match f x va vb with
      | None -> None
      | Some res -> Some(keya,res)) a b      
  let union f = unioni (fun _ a b -> f a b)
  let inter f = interi (fun _ a b -> f a b)          
  let pp print_value fmt m = 
    Format.fprintf fmt "{@[<hv>";
    iter (fun x (k,v) -> Format.fprintf fmt "%a -> %a@ " Key.pp k print_value v) m;
    Format.fprintf fmt "}@]"
  ;;
  let olditer = iter
  let oldfold = fold
  let old2list = to_list
  let iter f m = iter (fun x (k,v) -> f x v) m
  let fold f m acc = fold (fun x (k,v) a -> f x v a) m acc
  let to_list m = List.map (fun (x,(_,v)) -> x,v) (to_list m) 
  let is_singleton m = Option.map snd (is_singleton m)
end


module MakeWithKey(Key:sig
    type t
    val to_int: t -> int
    val pp: Format.formatter -> t -> unit
  end) = struct
  include MakePretty(Key)

  let iter f m = olditer (fun x (k,v) -> f k v) m
  let fold f m acc = oldfold (fun _ (k,v) acc -> f k v acc) m acc
  let to_list m = List.map (fun (_,(k,v)) -> k,v) (old2list m) 
end



(* (\* Union of ta with the intersection of tb and tc. Avoids double iteration on the tree
 *    and creating an intermediate tree. *\)
 * (\* XXX: Finir ca, en esperant que ca resolve mon probleme. Ca pourrait. *\)
 * let rec union_inter ~funion ~finter ta tb tc =
 *   match ta,tb,tc with
 *   | _ when tb == tc -> union funion ta tb
 *   (\* XXX: Do something if ta == tb or ta == tc? *\)
 *   (\* ta union (tb inter tc) = (ta inter tb) join (ta inter tc) *\)
 *   | Empty, _, _ -> inter finter tb tc
 *   | x, _, Empty -> x
 *   | x, Empty, _ -> x
 *   | Leaf{key;value},tb,tc -> insert (fun ~old ~value -> funion old value) key value (inter finter tb tc)
 *   | Branch _, Leaf{key=keyb;value=valueb}, Leaf{key=keyc;value=valuec} ->
 *     if keyb != keyc then ta
 *     else insert (fun ~old ~value -> funion value old) keyb (finter valueb valuec) ta
 *   | Branch _, Leaf{key;value},t | Branch _, t, Leaf{key;value} ->
 *     (try let res = find key t in
 *        let value = finter res value in
 *        insert (fun ~old ~value -> funion old value) key value ta
 *      with Not_found -> ta)
 *   | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
 *     Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1},
 *     Branch{prefix=pc;branching_bit=mc;tree0=tc0;tree1=tc1} ->
 *     (\* Same prefix: merge the subtrees. *\)
 *     (\* if ma == mb && mb == mc && pa == pb && pb == pc
 *      * then branch ~prefix:pa ~branching_bit:ma ~tree0:(union_inter ~funion ~finter ta0 tb0 tc0) ~tree1:(union_inter ~funion ~finter ta1 tb1 tc1)
 *      * else
 *      *   union funion ta (inter finter tb tc) *\)
 *     if mb == mc then begin
 *       if pb != pc then ta
 *       else
 *       if ma == mb && pa == pb then (\* All equal prefixes. *\)
 *         branch ~prefix:pa ~branching_bit:ma
 *           ~tree0:(union_inter ~funion ~finter ta0 tb0 tc0)
 *           ~tree1:(union_inter ~funion ~finter ta1 tb1 tc1)
 *       else if ma > mb && match_prefix pb pa ma then (\* ma higher *\)
 *         if pb land ma == 0 then
 *           branch ~prefix:pa ~branching_bit:ma 
 *             ~tree0:(union_inter ~funion ~finter ta0 tb tc)
 *             ~tree1:ta1
 *         else
 *           branch ~prefix:pa ~branching_bit:ma
 *             ~tree0:ta0            
 *             ~tree1:(union_inter ~funion ~finter ta1 tb tc)
 *       else if match_prefix pa pb mb then (\* (mb == mc) > ma  *\)
 *         if pa land mb == 0
 *         then  branch ~prefix:pb ~branching_bit:mb ~tree0:(union_inter ~funion ~finter ta tb0 tc0) ~tree1:(inter finter tb1 tc1)
 *         else branch ~prefix:pb ~branching_bit:mb ~tree0:(inter finter tb0 tc0) ~tree1:(union_inter ~funion ~finter ta tb1 tc1)
 *       else (\* prefixes do not match. *\)            
 *         join pa ta pb (inter finter tb tc)
 *     end
 *     else union funion ta @@ inter finter tb tc
 *     (\* else if mb > mc then
 *      *   if ma == mb then
 *      *     assert false            (\\* ma and mb higher *\\)
 *      *   else if ma > mb then
 *      *     assert false            (\\* ma higher *\\)
 *      *   else
 *      *     assert false            (\\* mb higher. *\\)
 *      * else (\\* mb < mc *\\)
 *      *   if ma == mc then
 *      *     assert false            (\\* ma and mc higher *\\)
 *      *   else if ma > mc then
 *      *     assert false            (\\* ma higher *\\)
 *      *   else
 *      *     assert false            (\\* mc higher *\\) *\)
 *         
 *     (\* if ma > mb then
 *      *   if mb == mc && pb == pc then
 *      *   if mb > mc then assert false
 *      *     
 *      * 
 *      *   assert false *\)
 * ;;
 * 
*)

