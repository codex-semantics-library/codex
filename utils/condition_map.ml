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

exception Never_refined

module type L = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
end  

module type Condition = sig
  type t
  val pretty: Format.formatter -> t -> unit
  val all: t
  val equal: t -> t -> bool
  val empty: t
  val is_empty: t -> bool
  val inter: t -> t -> t
  val (&&~): t -> t -> t
  val union: t -> t -> t
  val (||~): t -> t -> t    
  val disjoint: t -> t -> bool
  val is_included: t -> t -> bool
  val complement: t -> t
  val var: unit -> t
  val hash: t -> int
end

module type LConditionMapCommon = sig
  module L:L
  module Cond:Condition
  type t
  val pretty: (Format.formatter -> L.t -> unit) -> Format.formatter -> t -> unit
  val find: join:(L.t -> L.t -> L.t) -> bottom:L.t -> t -> Cond.t -> L.t
  val refine: inter:(L.t -> L.t -> L.t) -> t -> cond:Cond.t -> ?notcond:Cond.t -> L.t -> t
end


module type LConditionMapNoPartial = sig
  include LConditionMapCommon
  val create: L.t -> t
end

module type LConditionMap = sig
  include LConditionMapCommon
  val create_partial: t
end

module type LConditionMapFold = sig
  include LConditionMap
  val create_partial: t
  val fold_with_cond: t -> Cond.t -> 'a -> (L.t -> Cond.t -> 'a -> 'a) -> 'a
end


module FakeTop(L:L)(* :L with type t = L.t option *) = struct
  type t = L.t option
  let join ~join a b = match (a,b) with
    | None, x | x, None -> raise Never_refined
    | Some a, Some b -> Some(join a b)

  let inter ~inter a b = match (a,b) with
    | None, x | x, None -> x
    | Some a, Some b -> Some(inter a b)

  let bottom ~bottom = Some bottom

  let pretty pp fmt = function
    | None -> Format.fprintf fmt "<none>"
    | Some x -> pp fmt x

  let equal a b = match a,b with
    | None,None -> true
    | Some a, Some b -> L.equal a b
    | _ ,_-> false

  let compare a b = match a,b with
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some a, Some b -> L.compare a b

  let hash = function
    | None -> 0
    | Some x -> 1 + L.hash x
  
end

module type TransferFunctions = sig
  module Cond:Condition

  type 'a t

  val ar0: (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> interres:('a -> 'a -> 'a) ->
    Cond.t -> 'a -> 'a t -> 'a t

  val ar1:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a ->
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) -> interres:('res -> 'res -> 'res) ->   
    Cond.t -> ('a -> 'res) -> 'a t -> 'res t -> 'res t

  val ar2:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a ->
    (module LConditionMap with type t = 'b t and type L.t = 'b and type Cond.t = Cond.t) -> joinb:('b -> 'b -> 'b) -> bottomb:'b ->  
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->  interres:('res -> 'res -> 'res) ->   
    Cond.t -> ('a -> 'b -> 'res) -> 'a t -> 'b t -> 'res t -> 'res t

  val ar1_bwd:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a -> intera:('a -> 'a -> 'a) -> 
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) -> joinres:('res -> 'res -> 'res) ->  bottomres:'res -> 
    Cond.t -> ('a -> 'res -> 'a option) ->
    'a t -> 'res t -> (Cond.t * 'a t)

  val ar2_bwd:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a -> intera:('a -> 'a -> 'a) -> 
    (module LConditionMap with type t = 'b t and type L.t = 'b and type Cond.t = Cond.t) -> joinb:('b -> 'b -> 'b) -> bottomb:'b -> interb:('b -> 'b -> 'b) ->   
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) -> joinres:('res -> 'res -> 'res) -> bottomres:'res ->  
    Cond.t -> ('a -> 'b -> 'res -> 'a option * 'b option) ->
    'a t -> 'b t -> 'res t -> (Cond.t * 'a t) * (Cond.t * 'b t)

  val nondet_disjoint:
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
    conda:Cond.t -> notconda:Cond.t -> cma:'res t ->
    condb:Cond.t -> notcondb:Cond.t -> cmb:'res t ->
    join:('res -> 'res -> 'res) -> bottom:'res -> inter:('res -> 'res -> 'res) -> 
    old:'res t  -> 'res t

  val nondet_non_disjoint:
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
    conda:Cond.t -> cma:'res t ->
    condb:Cond.t -> cmb:'res t ->
    condaorb:Cond.t -> notcondaorb:Cond.t ->
    join:('res -> 'res -> 'res) -> bottom:'res -> inter:('res -> 'res -> 'res) ->
    old:'res t  -> 'res t

end

module ConditionMapTree(BDD:Condition) = struct

  type 'a node_ =
    | Interior of 'a i * 'a i
    | Leaf of 'a

  and 'a i = { key: BDD.t; node: 'a node_}

  type 'a t = 'a option i;;

  module Make(L:L)
  = struct

    (* To handle partial information, we add a top on the lattice.  *)
    module FL = FakeTop(L)
    type t = L.t option i

    module Cond = BDD
    module L = L

    
    module Orig_(L:L) = struct
      let rec pretty pp fmt {key;node} =
        Format.fprintf fmt "@[<hv>%a -> %a@]" BDD.pretty key (pretty_node pp) node

      and pretty_node pp  fmt = function
        | Leaf a -> pp fmt a
        | Interior(n1,n2) -> Format.fprintf fmt "@[<v>%a@\n%a@]" (pretty pp) n1 (pretty pp) n2
      ;;

      let create top = {key=BDD.all; node=Leaf top}

      let rec find ~join ~bottom = fun {key;node} cond ->
        if BDD.disjoint key cond then bottom
        else
          match node with
          | Leaf(v) -> v
          | Interior(c1,c2) -> join (find ~join ~bottom c1 cond) (find ~join ~bottom c2 cond)
      ;;

      let refine ~inter =
        let rec refine_whole {key;node} value =
          let newnode = 
            match node with
            | Leaf(v) -> Leaf(inter v value)
            | Interior(n1,n2) -> Interior(refine_whole n1 value,refine_whole n2 value)
          in {key;node=newnode}
        in

        let rec f ({key;node} as obj) ~cond ?(notcond=(BDD.complement cond)) value = 
          let binter = BDD.inter cond key in
          if BDD.is_empty binter then obj
          else if BDD.equal binter key
          then refine_whole obj value
          else
            let newnode = match node with
              | Leaf v ->
                let n1 = {key = binter; node = Leaf(inter v value)} in
                let n2 = {key = BDD.inter key notcond; node = Leaf(v)} in
                Interior(n1,n2)
              | Interior(n1,n2) -> Interior(f n1 ~cond ~notcond value, f n2 ~cond ~notcond value)
            in {key;node=newnode}
        in f
      ;;
    end

    (**************** Handle partial. ****************)

    module Orig = Orig_(FL);;
    
    let pretty pp fmt x = Orig.pretty (FL.pretty pp) fmt x;;
    let create _ = assert false
    
    let create_partial = Orig.create None
    
    let find ~join ~bottom n c =
      match Orig.find ~join:(FL.join ~join) ~bottom:(FL.bottom ~bottom) n c with
      | None -> assert false
      | Some x -> x
    ;;
    
    let refine ~inter  n ~cond ?notcond v =
      Orig.refine ~inter:(FL.inter ~inter) n ~cond ?notcond (Some v)
    ;;

  end

end

(* Similar to ConditionMap below, but do not regroup partitions based
   on their values. In some case this leads to a huge number of
   cases. Merging partitions using lists is also not very
   efficient. So this structure should be used only for testing
   purpose, or expereminting changes in the interface. *)
module ConditionMapPartitionList(BDD:Condition) = struct

  type 'a i = ('a * BDD.t) list
  (* type 'a i = Cons of ('a * BDD.t * 'a i option) *)

  type 'a t = 'a option i

  module Make(L:L)
  = struct

    (* To handle partial information, we add a top on the lattice.  *)
    module FL = FakeTop(L)
    type t = L.t option i

    module Cond = BDD
    module L = L

    
    module Orig_(L:L) = struct
      let pretty fmt = assert false

      let create top = [(top,BDD.all)]

      let find ~join ~bottom n cond =
        List.fold_left (fun acc (v,c) ->
            if BDD.disjoint c cond then acc
            else FL.join ~join acc v) (FL.bottom ~bottom) n
      ;;

      (* One advantage of the alist is that we do not need to have
         that two different partitions have different values. This
         could allow to avoid computing a or if the BDD operation is
         too costly (e.g. CUDD allows to limit the time spent doing
         some operations). *)
      let rec remove_assoc x (accbdd,acclist) = function
        | [] -> (accbdd,acclist)
        | (v,c)::rest when L.equal x v -> remove_assoc x (BDD.union accbdd c,acclist) rest
        | a::rest -> remove_assoc x (accbdd,a::acclist) rest
      ;;
      
      let rec merge_partitions = function
        | [] -> []
        | (v1,c1)::rest ->
          let rest = merge_partitions rest in
          let (newc,rest) = remove_assoc v1 (c1,[]) rest in
          (v1,newc)::rest
      ;;

      (* We could join partitions with same BDD together. But it is also OK not to do it. *)
      let refine ~inter  n ~cond ?(notcond = BDD.complement cond) newv =
        let rec loop = function
          | [] -> []
          | (v,c)::rest ->
            let c1 = BDD.inter cond c in
            let res = loop rest in
            if BDD.is_empty c1 then (v,c)::res
            else if BDD.equal c1 c then (inter newv v,c)::res
            else
              let c2 = BDD.inter notcond c in
              (v,c2)::(inter newv v,c1)::res
        in
        let res = loop n in
        let length = List.length res in
        (* This is optional. *)
        let newres = merge_partitions res in
        let newlength = List.length newres in
        if length > 10 then
          Codex_log.warning "I have a list of length %d newlength %d" length newlength;
        newres
      ;;

    end

    (**************** Handle partial. ****************)

    (* Same as above *)
    (* module Orig = Orig_(FL);;
     * 
     * let pretty fmt x = Orig.pretty fmt x;;
     * let create _ = assert false
     * 
     * let create_partial = Orig.create None
     * 
     * let find  n c =
     *   match Orig.find n c with
     *   | None -> assert false
     *   | Some x -> x
     * ;;
     * 
     * let refine  n ~cond ?notcond v =
     *   Orig.refine n ~cond ?notcond (Some v)
     * ;; *)
  end
end

(* Old version of ConditionMap where we use a fake top on the lattice. *)
module ConditionMapPartitionOld(BDD:Condition) = struct

  module MakeReal(L:L):LConditionMapFold
    with module L = L
     and module Cond = BDD
  = struct

    (* To handle partial information, we add a top on the lattice.  *)
    module FL = FakeTop(L)
    (* type 'a tmp = 'a t *)

    module Cond = BDD
    module L = L

    module Map = Map.Make(FL)
    type t = BDD.t Map.t

    module Orig = struct
      let pretty pp fmt x =
        let l = Map.bindings x in
        let print_node fmt (v,c) = Format.fprintf fmt "%a -> %a" (FL.pretty pp) v BDD.pretty c in
        (Format.pp_print_list print_node) fmt l


      (* let create top = [(top,BDD.all)] *)

      let find ~join ~bottom n cond =
        (* Codex_log.feedback "find: %a %a" (pretty pp) n BDD.pretty cond; *)
        Map.fold (fun v c acc ->
            if BDD.disjoint c cond then acc
            else FL.join ~join acc v) n (FL.bottom ~bottom)
      ;;

      let refine ~inter n ~cond ?(notcond = BDD.complement cond) newv =
        let add v c map =
          match Map.find v map with
          | exception Not_found -> Map.add v c map
          | c2 -> Map.add v (BDD.union c c2) map
        in

        let f v c acc = 
          let c1 = BDD.inter cond c in
          if BDD.is_empty c1 then (add v c acc) 
          else if BDD.equal c1 c then
            (add (FL.inter ~inter newv v) c acc)
          else
            let c2 = BDD.inter notcond c in
            let acc = add (FL.inter ~inter newv v) c1 acc in
            let acc = add v c2 acc in
            acc
        in
        Map.fold f n Map.empty
      ;;

    end
    
    (* Same as above *)

    let pretty pp fmt x = Orig.pretty pp fmt x;;
    let create _ = assert false

    let create_partial = Map.singleton None BDD.all

    let find ~join ~bottom  n c =
      match Orig.find ~join ~bottom n c with
      | None -> assert false
      | Some x -> x
    ;;
    
    let refine ~inter  n ~cond ?notcond v =
      (* Kernel.feedback "refine %a %a with %a" L.pretty v pretty n BDD.pretty cond; *)
      let res = Orig.refine ~inter n ~cond ?notcond (Some v) in
      (* Kernel.feedback "refine %a %a with %a res %a" L.pretty v
         pretty n BDD.pretty cond pretty res; *)
      res
    ;;

    let fold_with_cond x cond acc g  =
      let f v c acc = match v with
        | None -> if not (BDD.disjoint cond c) then raise Never_refined else acc
        | Some v ->
          let inter = BDD.inter cond c in
          if BDD.is_empty inter then acc
          else g v inter(* inter *) acc
      in
      Map.fold f x acc
    ;;
    
  end

  (* I think that this is still safe to use, but the implementation is
     using Obj. *)
  module Unsafe = struct

    type 'a t = Obj.t

    module Make(L:L) = struct

      module CM = MakeReal(L)
      module Cond = BDD
      module L = L

      type t = Obj.t

      let pretty pp fmt x = CM.pretty pp fmt (Obj.obj x)
      let refine ~inter x ~cond ?notcond v =
        Obj.repr @@
        CM.refine ~inter (Obj.obj x) ~cond ?notcond v
      let find ~join ~bottom x cond = CM.find ~join ~bottom (Obj.obj x) cond
      let create_partial = Obj.repr @@ CM.create_partial
      let fold_with_cond = Obj.magic CM.fold_with_cond
      
    end

  end

  (* This is less performant, because we allocate an extra record, and
     we need to retrieve the function pointer from an element in the
     record (virtual call). *)
  module Safe = struct

    module type T = sig
      type key
      module CM:LConditionMapFold with type L.t = key and module Cond = BDD
      val value: CM.t
    end

    type 'a t = (module T with type key = 'a)

    module Make(L:L) = struct

      module CM = MakeReal(L)
      module Cond = BDD
      module L = L

      type 'a tmp = 'a t
      type t = L.t tmp

      let pretty pp fmt (module T:T with type key = L.t) = T.CM.pretty pp fmt T.value
      let refine ~inter (module T:T with type key = L.t) ~cond ?notcond v =
        let v = T.CM.refine ~inter T.value ~cond ?notcond v in
        let module Res = struct
          type key = T.key
          module CM = T.CM
          let value = v
        end in
        (module Res:T with type key = L.t)
      ;;

      let find ~join ~bottom (module T:T with type key = L.t) cond  =
        T.CM.find ~join ~bottom T.value cond
      ;;

      let fold_with_cond (module T:T with type key = L.t) cond acc f  =
        T.CM.fold_with_cond T.value cond acc f
      ;;

      
      let create_partial =
        let module Res = struct
          type key = L.t
          module CM = CM
          let value = CM.create_partial
        end in
        (module Res:T with type key = L.t)

    end
  end

  (* include Safe *)
  include Unsafe

end

module ConditionMapPartition(BDD:Condition) = struct

  module MakeReal(L:L):LConditionMapFold
    with module L = L
     and module Cond = BDD
  = struct

    module Cond = BDD
    module L = L

    module Orig = struct

      (* module Map = Map.Make(L) *)                 
      module Map = Smallmap.Make(L)
      type t = BDD.t Map.t

      (* Can be used only for explicit. *)
      let map_fold_explicit f map init = Map.fold f map init, BDD.empty

      (* For implicit: also return the union of the keys, which is the
         conditions for which the map is defined. *)
      let map_fold_implicit f map init =
        let f' key value (acc,bdds) =
          let acc = f key value acc in
          let bdds = BDD.union bdds value in
          acc,bdds
        in Map.fold f' map (init,BDD.empty)
      ;;

      let map_fold = map_fold_explicit
      
             
      let pretty pp fmt x =
        let l = Map.bindings x in
        let print_node fmt (v,c) = Format.fprintf fmt "%a -> %a" pp v BDD.pretty c in
        (Format.pp_print_list print_node) fmt l


      (* let create top = [(top,BDD.all)] *)

      let find ~join ~bottom n cond =
        (* Codex_log.feedback "find: %a" (\* (pretty pp) n *\)  BDD.pretty cond; *)
        map_fold (fun v c acc ->
            if BDD.disjoint c cond then acc
            else join acc v) n bottom
      ;;

      let add v c map =
        match Map.find v map with
        | exception Not_found -> Map.add v c map
        | c2 -> Map.add v (BDD.union c c2) map
      ;;

      let refine ~inter n ~cond ?(notcond = BDD.complement cond) newv =
        let f v c acc = 
          let c1 = BDD.inter cond c in
          if BDD.is_empty c1 then (add v c acc) 
          else if BDD.equal c1 c then
            (add (inter newv v) c acc)
          else
            let c2 = BDD.inter notcond c in
            let acc = add (inter newv v) c1 acc in
            let acc = add v c2 acc in
            acc
        in
        map_fold f n Map.empty
      ;;

    end


    (* Add the special case where the BDD is undefined on top. *)
    module Explicit(Arg:sig end) = struct

      assert(Orig.map_fold == Orig.map_fold_explicit);
      
      (* Explicit the condition for which the BDD is not defined;
         remove the need for the fake top. *)
      type t =
        { orig:Orig.t;
          undefined: BDD.t}

      let pretty pp fmt x = Orig.pretty pp fmt x.orig;;
      let create _ = assert false

      let create_partial =
        { orig = Orig.Map.empty; undefined = BDD.all }

      let find ~join ~bottom n c =
        if(not @@ BDD.disjoint n.undefined c)
        then raise Never_refined;
        fst @@ Orig.find ~join ~bottom n.orig c 


      let refine ~inter n ~cond ?(notcond = BDD.complement cond) v =
        (* Codex_log.feedback "refining condition %a oldundef %a" BDD.pretty cond BDD.pretty n.undefined; *)
        let newundefined = BDD.inter n.undefined notcond in
        if BDD.equal newundefined n.undefined
        then
          (* Refine already existing values. *)
          { orig = fst @@ Orig.refine ~inter n.orig ~cond ~notcond v;
            undefined = newundefined;
          }
        else
          (* We define new values whose values we previously not knew; 
             we make as if these old values were "top". *)
          let newlydefined  = BDD.inter cond n.undefined in
          assert(not @@ BDD.is_empty newlydefined);
          let orig = n.orig in
          let orig = fst @@ Orig.refine ~inter orig ~cond ~notcond v in          
          let orig = Orig.add v newlydefined orig in
          { orig; undefined = newundefined }

      let fold_with_cond x cond acc g = assert false
    (* let fold_with_cond x cond acc g  =
     *   let f v c acc = match v with
     *     | None -> if not (BDD.disjoint cond c) then raise Never_refined else acc
     *     | Some v ->
     *       let inter = BDD.inter cond c in
     *       if BDD.is_empty inter then acc
     *       else g v inter(\* inter *\) acc
     *   in
     *   Map.fold f x acc
     * ;; *)
      
    end


    module Implicit(Arg:sig end) = struct

      assert(Orig.map_fold == Orig.map_fold_implicit);
      
      type t = Orig.t
      let pretty pp fmt x = Orig.pretty pp fmt x;;

      let create_partial = Orig.Map.empty;;

      let find ~join ~bottom orig c =
        let res,defined = Orig.find ~join ~bottom orig c in
        if(not @@ BDD.is_included c defined)
        then raise Never_refined
        else res

      let refine ~inter n ~cond ?(notcond = BDD.complement cond) v =
        let orig,defined = Orig.refine ~inter n ~cond ~notcond v in
        if BDD.is_included cond defined
        (* Refine already existing values only. *)         
        then orig
        else
          (* We define new values whose values we previously not knew; 
             we make as if these old values were "top". *)
          let newlydefined  = BDD.inter cond @@ BDD.complement defined in
          assert(not @@ BDD.is_empty newlydefined);
          let orig = Orig.add v newlydefined orig in
          orig
      let fold_with_cond x cond acc g = assert false
                                      
    end

    (* Explicit requires more storage, but implicit requires
       additional BDD computation; so which is faster or more
       efficient depends on the benchmark. Maybe a good solution
       would be to store the value in the smallmap. *)
    include Explicit(struct end)
    (* include Implicit(struct end) *)          

          
  end

  (* I think that this is still safe to use, but the implementation is
     using Obj. *)
  module Unsafe = struct

    type 'a t = Obj.t

    module Make(L:L) = struct

      module CM = MakeReal(L)
      module Cond = BDD
      module L = L

      type t = Obj.t

      let pretty pp fmt x = CM.pretty pp fmt (Obj.obj x)
      let refine ~inter x ~cond ?notcond v =
        Obj.repr @@
        CM.refine ~inter (Obj.obj x) ~cond ?notcond v
      let find ~join ~bottom x cond = CM.find ~join ~bottom (Obj.obj x) cond
      let create_partial = Obj.repr @@ CM.create_partial
      let fold_with_cond = Obj.magic CM.fold_with_cond
      
    end

  end

  (* This is less performant, because we allocate an extra record, and
     we need to retrieve the function pointer from an element in the
     record (virtual call). *)
  module Safe = struct

    module type T = sig
      type key
      module CM:LConditionMapFold with type L.t = key and module Cond = BDD
      val value: CM.t
    end

    type 'a t = (module T with type key = 'a)

    module Make(L:L) = struct

      module CM = MakeReal(L)
      module Cond = BDD
      module L = L

      type 'a tmp = 'a t
      type t = L.t tmp

      let pretty pp fmt (module T:T with type key = L.t) = T.CM.pretty pp fmt T.value
      let refine ~inter (module T:T with type key = L.t) ~cond ?notcond v =
        let v = T.CM.refine ~inter T.value ~cond ?notcond v in
        let module Res = struct
          type key = T.key
          module CM = T.CM
          let value = v
        end in
        (module Res:T with type key = L.t)
      ;;

      let find ~join ~bottom (module T:T with type key = L.t) cond  =
        T.CM.find ~join ~bottom T.value cond
      ;;

      let fold_with_cond (module T:T with type key = L.t) cond acc f  =
        T.CM.fold_with_cond T.value cond acc f
      ;;

      
      let create_partial =
        let module Res = struct
          type key = L.t
          module CM = CM
          let value = CM.create_partial
        end in
        (module Res:T with type key = L.t)

    end
  end

  (* include Safe *)
  include Unsafe

end

(* Generic implementation. *)
module MakePathInsensitive
    (BDD:Condition)
    (M:sig type 'a t end)
  :TransferFunctions with module Cond = BDD and type 'a t = 'a M.t                           
= struct
  module Cond =  BDD

  type 'a t = 'a M.t

  let ar0 (type res) (module L:LConditionMap with type t = res M.t and type L.t = res and type Cond.t = Cond.t) ~interres
      cond value old =
    let notcond = BDD.complement cond in
    L.refine ~inter:interres old ~cond ~notcond value

  let ar1
      (type a) (type ma) (module La:LConditionMap with type t = ma and type L.t = a and type Cond.t = Cond.t) ~joina ~bottoma
      (type res) (type mres) (module Lres:LConditionMap with type t = mres and type L.t = res and type Cond.t = Cond.t) ~interres
      cond f a old =
    let av = La.find ~join:joina ~bottom:bottoma a cond in
    let notcond = BDD.complement cond in
    Lres.refine ~inter:interres old ~cond ~notcond (f av)
  ;;

  let ar2
      (type a) (type ma) (module La:LConditionMap with type t = ma and type L.t = a and type Cond.t = Cond.t) ~joina ~bottoma
      (type b) (type mb) (module Lb:LConditionMap with type t = mb and type L.t = b and type Cond.t = Cond.t) ~joinb ~bottomb        
      (type res) (type mres) (module Lres:LConditionMap with type t = mres and type L.t = res and type Cond.t = Cond.t) ~interres
      cond f a b old =
    let av = La.find ~join:joina ~bottom:bottoma a cond in
    let bv = Lb.find ~join:joinb ~bottom:bottomb b cond in      
    let notcond = BDD.complement cond in
    Lres.refine ~inter:interres old ~cond ~notcond (f av bv)
  ;;

  let changed (refine:'a -> cond:BDD.t -> ?notcond:BDD.t -> 'b -> 'a) cond r = function
    | None -> BDD.empty, r
    | Some x -> cond, refine r ~cond x
  ;;

  let ar1_bwd
      (type a) (type ma) (module La:LConditionMap with type t = ma and type L.t = a and type Cond.t = Cond.t) ~joina ~bottoma ~intera
      (type res) (type mres) (module Lres:LConditionMap with type t = mres and type L.t = res and type Cond.t = Cond.t) ~joinres ~bottomres
      cond f a res =
    let va = La.find ~join:joina ~bottom:bottoma a cond in
    let v = Lres.find ~join:joinres ~bottom:bottomres res cond in
    let newva = f va v in
    changed (La.refine ~inter:intera) cond a newva
  ;;    


  let ar2_bwd
      (type a) (type ma) (module La:LConditionMap with type t = ma and type L.t = a and type Cond.t = Cond.t) ~joina ~bottoma ~intera
      (type b) (type mb) (module Lb:LConditionMap with type t = mb and type L.t = b and type Cond.t = Cond.t) ~joinb ~bottomb ~interb      
      (type res) (type mres) (module Lres:LConditionMap with type t = mres and type L.t = res and type Cond.t = Cond.t) ~joinres ~bottomres
      cond f a b res =
    let va = La.find ~join:joina ~bottom:bottoma a cond in
    let vb = Lb.find ~join:joinb ~bottom:bottomb b cond in
    let v = Lres.find ~join:joinres ~bottom:bottomres res cond in
    let newva, newvb = f va vb v in
    (changed (La.refine ~inter:intera) cond a newva,
     changed (Lb.refine ~inter:interb) cond b newvb)
  ;;    

  (* Note: a simple implementation of path-sensitive analysis would
     get a list of (condition,value) leaves; and refine with the
     cartesian product. This would work, but there would be a
     quadratic, instead of linear behaviour when the elements have the
     same partitionning. *)

  let nondet_disjoint
      (type res) (type mres) (module L:LConditionMap with type t = mres and type L.t = res and type Cond.t = Cond.t)
      ~conda ~notconda ~cma ~condb ~notcondb ~cmb ~join ~bottom ~inter ~old =
    let av = L.find ~join ~bottom cma conda in
    let bv = L.find ~join ~bottom cmb condb in
    let res = L.refine ~inter old av ~cond:conda ~notcond:notconda in
    let res = L.refine ~inter res bv ~cond:condb ~notcond:notcondb in
    res
  ;;

  let nondet_non_disjoint
      (type res) (type mres) (module L:LConditionMap with type t = mres and type L.t = res and type Cond.t = Cond.t)
      ~conda ~cma ~condb ~cmb ~condaorb ~notcondaorb ~join ~bottom ~inter ~old =
    let av = L.find ~join ~bottom cma conda in
    let bv = L.find ~join ~bottom cmb condb in
    let abv = join av bv in
    let res = L.refine ~inter old abv ~cond:condaorb ~notcond:notcondaorb in
    res
  ;;
  
end


module MakePathSensitive
    (BDD:Condition)
    (M:sig type 'a t end)
  (* :TransferFunctions with module Cond = BDD and type 'a t = 'a M.t                            *)
= struct
  module Cond =  BDD

  type 'a t = 'a M.t
  
  let ar0
      (type res) (type mres) (module Lres:LConditionMap with type t = mres and type L.t = res and type Cond.t = BDD.t)
      cond value old =
    Lres.refine old ~cond value
  ;;

  
  let ar1
      (type a) (type ma) (module La:LConditionMapFold with type t = ma and type L.t = a and type Cond.t = BDD.t)
      (type res) (type mres) (module Lres:LConditionMap with type t = mres and type L.t = res and type Cond.t = BDD.t) ~inter
      cond f a old =
    La.fold_with_cond a cond old (fun v c acc ->
        let v = f v in
        Lres.refine ~inter acc ~cond:c v)
  ;;

  let ar2
      (type a) (type ma) (module La:LConditionMapFold with type t = ma and type L.t = a and type Cond.t = BDD.t)
      (type b) (type mb) (module Lb:LConditionMapFold with type t = mb and type L.t = b and type Cond.t = BDD.t)
      (type res) (type mres) (module Lres:LConditionMap with type t = mres and type L.t = res and type Cond.t = BDD.t) ~inter
      cond f a b old =
    La.fold_with_cond a cond old (fun va ca acc ->
        Lb.fold_with_cond b ca acc (fun vb cab acc ->
            Lres.refine ~inter acc ~cond:cab (f va vb)))
  ;;

  let ar1_bwd
      (type a) (type ma) (module La:LConditionMapFold with type t = ma and type L.t = a and type Cond.t = BDD.t) ~intera
      (type res) (type mres) (module Lres:LConditionMapFold with type t = mres and type L.t = res and type Cond.t = BDD.t)
      cond f a res =
    Lres.fold_with_cond res cond (BDD.empty,a) (fun vres cres ((acccond,acca) as acc) ->
        La.fold_with_cond a cres acc (fun va cares (acccond,acca) ->
            match f va vres with
              | None -> (acccond, acca)
              | Some v -> (BDD.union cares acccond, La.refine ~inter:intera acca ~cond:cares v)))
  ;;

  let ar2_bwd 
      (type a) (type ma) (module La:LConditionMapFold with type t = ma and type L.t = a and type Cond.t = BDD.t) ~intera
      (type b) (type mb) (module Lb:LConditionMapFold with type t = mb and type L.t = b and type Cond.t = BDD.t) ~interb
      (type res) (type mres) (module Lres:LConditionMapFold with type t = mres and type L.t = res and type Cond.t = BDD.t)
      cond f a b res =
    Lres.fold_with_cond res cond ((BDD.empty,a),(BDD.empty,b)) (fun vres cres acc ->
        La.fold_with_cond a cres acc (fun va cares acc ->
            Lb.fold_with_cond b cares acc (fun vb cabres ((accca,acca),(acccb,accb)) ->
                let newa,newb = f va vb vres in
                let (accca,acca) = match newa with
                |  None -> accca, acca
                |  Some v -> (BDD.union cabres accca, La.refine ~inter:intera acca ~cond:cabres v)
                in
                let (acccb,accb) = match newb with
                |  None -> acccb, accb
                |  Some v -> (BDD.union cabres acccb, Lb.refine ~inter:interb accb ~cond:cabres v)
                in
                ((accca,acca),(acccb,accb)))))
  
  ;;

  let nondet_disjoint
      (type res) (type mres) (module L:LConditionMapFold with type t = mres and type L.t = res and type Cond.t = Cond.t)
      ~conda ~notconda ~cma ~condb ~notcondb ~cmb ~inter ~old =
    let res = old in
    let res = L.fold_with_cond cma conda res (fun v c acc ->
        L.refine ~inter acc ~cond:c(* (BDD.inter c conda) *) v) in
    let res = L.fold_with_cond cmb condb res (fun v c acc ->
        L.refine ~inter acc ~cond:c(* (BDD.inter c condb) *) v) in
    res
  ;;

  let nondet_non_disjoint
      (type res) (type mres) (module L:LConditionMapFold with type t = mres and type L.t = res and type Cond.t = Cond.t)
      ~conda ~cma ~condb ~cmb ~condaorb ~notcondaorb ~inter ~old =
    (* Special case. *)
    if BDD.equal conda condb
    then ar2 (module L) (module L) (module L) conda (assert false) ~inter cma cmb old
    else
      (* MAYBE: pass these as an argument. *)
      let a_and_b = BDD.inter conda condb in
      let a_minus_b = BDD.inter conda @@ BDD.complement condb in
      let b_minus_a = BDD.inter condb @@ BDD.complement conda in
      (* Add the common values. *)
      let acc =
        ar2 (module L) (module L) (module L) a_and_b (* L.L.join *)(assert false) ~inter cma cmb old in
      (* Add the values in a only *)
      let acc =
        L.fold_with_cond cma a_minus_b acc (fun v c acc ->
            L.refine ~inter acc ~cond:c v) in
      (* Add the values in b only. *)
      let acc =
        L.fold_with_cond cmb b_minus_a acc (fun v c acc ->
            L.refine ~inter acc ~cond:c v) in
      acc

end
