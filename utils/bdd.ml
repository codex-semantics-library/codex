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

(* BDDs and MTBDDs. *)

module HashtblPair(T:Hashtbl.HashedType) = struct
  module Pair = struct
    type t = T.t * T.t
    let equal (a1,a2) (b1,b2) = T.equal a1 b1 && T.equal a2 b2
    let hash (a1,a2) = Hashing.hash2 (T.hash a1) (T.hash a2)
  end

  include Hashtbl.Make(Pair)
end


(* let cache_default_size = (\* 7001 *\) 251 *)
let weakhash_default_size = (* 7001 *) 251

module Make(Var:sig
  type t
  val equal: t -> t -> bool
  (* Variable order in the bdd: smallest variables are closer to the
     root. *)
  val compare: t -> t -> int
  val hash: t -> int
  val pretty: Format.formatter -> t -> unit
end) =
struct

  (* Hash-consing tag. *)
  type tag = int

  module BDD = struct

    (* This module sealing ensures that bdds are always hash-consed in
       the rest of the code.  *)
    module Type:sig

      type bdd = private Zero | One | If of tag * Var.t * bdd * bdd
      type t = bdd
      val equal: bdd -> bdd -> bool
      val hash: bdd -> int
      val compare: bdd -> bdd -> int

      val zero: bdd
      val one: bdd
      val mk: Var.t -> bdd -> bdd -> bdd
    end =
    struct

      (* Phantom type to track hash-consing, but this is cumbersome to
         use; especially this leaks to the interface of the
         module. The private type + encapsulation suffices for outside
         of [Type]; uncomment "Interned of" just to check conformity
         inside [Type]. *)
      [@@@ocaml.warning "-34"]
      type 'a interned = (* Interned of *) 'a
      [@@@ocaml.warning "+34"]      
      type bdd(* _uninterned *) = Zero | One | If of tag * Var.t * bdd * bdd
      (* and bdd = bdd_uninterned interned *)

      type t = bdd

      (* Equality and hashing when the bdd is hash-consed. *)
      (* let equal = fun (Interned a) (Interned b) -> a (==) b;; *)
      let equal = (==);;
      let hash ((* Interned *) x) = match x with
        | Zero -> 0
        | One -> 1
        | If(tag,_,_,_) -> tag
      ;;
      let compare a b = Stdlib.compare (hash a) (hash b)

      module WeakHash = Weak.Make(struct
      (* Equality and hashing when the bdd is not yet hash-consed (but
         sub-objects are). *)
        type t = bdd(* _uninterned *)

        let equal a b = match (a,b) with
          | _, _ when a == b -> true
          | If(_,vara,bdda1,bdda2), If(_,varb,bddb1,bddb2) ->
             bdda1 == bddb1 && bdda2 == bddb2 && Var.equal vara varb
          | _,_ -> false

        let hash = function
          | Zero -> 0
          | One -> 1
          (* Note: uses [hash] for the hash-consed bdds; there is no recursive call to [hash]!. *)
          | If(_,var,left,right) -> Hashing.hash3 (Var.hash var) (hash left) (hash right)
      end);;

      (* Tag of Zero and One are 0 and 1. *)
      let tag_ref = ref 2 ;;
      let table = WeakHash.create weakhash_default_size;;

      let zero = (* Interned *) Zero
      let one = (* Interned *) One
      let mk var then_ else_ =
        if then_ == else_ then then_
        else
          let tentative = If(!tag_ref, var, then_,else_) in
          let ret = WeakHash.merge table tentative in
          if ret == tentative then incr tag_ref;
          (* Interned *) ret
      ;;

    end

    include Type;;

    (* Naive version; more efficient one should use lets to avoid
       combinatory explosion. Note that pretty-printing bdds is
       reserved to debugging anyway. *)
    let rec pretty fmt bdd =
      match bdd with
      | Zero -> Format.fprintf fmt "0"
      | One -> Format.fprintf fmt "1"
      | If(_,var,bdd1,bdd2) ->
         Format.fprintf fmt "@[<v>@[<hv 2> %a ->@ %a@]@\n@[<hv 2>!%a ->@ %a@]@]"
           Var.pretty var pretty bdd1 Var.pretty var pretty bdd2
    ;;

    let var v = mk v one zero;;

    module Hash1 = Hashtbl.Make(Type);;
    module Hash2 = HashtblPair(Type);;
    module Var_Hash = Hashtbl.Make(Var)

    (* Computes the size of the BDD (number of nodes in the dag, and
       number of boolean variables). Useful for debugging. *)
    let _size_of bdd =
      let seen_dags = Hash1.create 97 in
      let seen_vars = Var_Hash.create 97 in
      let count_vars = ref 0 in
      let rec size bdd =
        if Hash1.mem seen_dags bdd then 0
        else match bdd with
        | Zero | One -> 1
        | If(_,v,then_,else_) ->
           if not (Var_Hash.mem seen_vars v)
           then (Var_Hash.replace seen_vars v ();
                 incr count_vars );
          size then_ + size else_
      in
      let res = size bdd in
      (res,!count_vars)
    ;;


    module WithCache(Param:sig val cache_default_size:int end) = struct

      (* MAYBE: we could create the cache to handle commutativity;
         i.e. a && b == b && a. This translates to equality of the
         pairs (a,b) and (b,a) in the Hash2 table. I am not sure yet,
         but probably, this would provide a performance gain (fewer
         hashtbl entries, more hits). To do this, we would need a
         apply_commutative function, that would call
         Hash2_commutative. It suffice to sort the arguments by their tag. *)
      (* MAYBE: Use weak hash instead of these hashes. *)
      let not_cache = Hash1.create Param.cache_default_size;;
      let and_cache = Hash2.create Param.cache_default_size;;
      let or_cache = Hash2.create Param.cache_default_size;;
      let imp_cache = Hash2.create Param.cache_default_size;;

      let rec (!~) x =
        try
          Hash1.find not_cache x
        with Not_found ->
          let res = match x with
            | Zero -> one
            | One -> zero
            | If (_, v, then_, else_) -> Type.mk v (!~ then_) (!~ else_)
          in
          Hash1.add not_cache x res;
          res
      ;;

      (* The different arguments are the different partial applications of
         the binary operation [opxy] when one of the two arguments x or y is
         known to be 0 or 1. *)
      let apply cache op0y op1y opx0 opx1 x y =
        (* TODO: Benchmark to test if the cache should be shared betwween invocations. *)
        (* let cache = Hash2.create Param.cache_default_size in *)
        (* let (sizex,varsx) = size_of x and (sizey,varsy) = size_of y in *)
        (* Kernel.feedback "sizex %d varsx %d sizey %d varsy %d" sizex varsx sizey varsy; *)
        let rec apply u1 u2 =
          let u12 = (u1,u2) in
          try Hash2.find cache u12
          with Not_found ->
            let res = match u12 with
              | Zero, _ -> op0y u2
              | One, _ -> op1y u2
              | _, Zero -> opx0 u1
              | _, One -> opx1 u1
              | If(_,v1,then1,else1),If(_,v2,then2,else2) ->
                 (match Var.compare v1 v2 with
                 | 0 -> Type.mk v1 (apply then1 then2) (apply else1 else2)
                 | x when x < 0 -> Type.mk v1 (apply then1 u2) (apply else1 u2)
                 | _ -> Type.mk v2 (apply u1 then2) (apply u1 else2))
            in Hash2.replace cache u12 res; res
        in apply x y
      ;;

      (* There are only 4 boolean functions of one argument. *)
      let f_one _ = one;;
      let f_zero _ = zero;;
      let f_same x = x
      let f_inv = (!~)

      (* Hopefully everything is inlined and this is very efficient... *)
      let (&&~) = apply and_cache f_zero f_same f_zero f_same;;
      let (||~) = apply or_cache f_same f_one f_same f_one;;
      let (==>~) = apply imp_cache f_one f_same f_inv f_one

    end

    include WithCache(struct let cache_default_size = 17 end);;
  end

  (****************************************************************)
  (* MTBDDs: functions from boolean variables to arbitrary
     terminals. *)

  (* Note: it is possible to have an implementation not to hash-cons
     Terminal of ...; but in this case one cannot use (==) for
     mtbdds. *)

  module type Terminal = sig
      type t
      val equal: t -> t -> bool
      val hash: t -> int
      val pretty: Format.formatter -> t -> unit
    end

  type 'a mtbdd = Terminal of tag * 'a | If of tag * Var.t * 'a mtbdd * 'a mtbdd

  module type MTBDD = sig
    module Terminal:Terminal
    type t = Terminal.t mtbdd
    val equal: t -> t -> bool
    val hash: t -> int
    val pretty: Format.formatter -> t -> unit

    val terminal: Terminal.t -> t
    val mk: Var.t -> t -> t -> t
  end

  let map1
      (type a) (module Ma:MTBDD with type Terminal.t = a)
      (type res) (module Mres:MTBDD with type Terminal.t = res)
      f =
    let module Hash1 = Hashtbl.Make(Ma) in
    let map1_cache = Hash1.create 251 in
    let rec map1 mtbdd =
      try Hash1.find map1_cache mtbdd
      with Not_found ->
        let res = match mtbdd with
          | Terminal (_, t) -> Mres.terminal (f t)
          | If(_,v,then_,else_) -> Mres.mk v (map1 then_) (map1 else_)
        in Hash1.replace map1_cache mtbdd res; res
    in map1
  ;;

  module Pair(M1:Hashtbl.HashedType)(M2:Hashtbl.HashedType) = struct
    type t = M1.t * M2.t
    let equal (a1,a2) (b1,b2) = M1.equal a1 b1 && M2.equal a2 b2;;
    let hash (a1,a2) = 65533 * (M1.hash a1) + M2.hash a2
  end

  let map2
      (type a) (module Ma:MTBDD with type Terminal.t = a)
      (type b) (module Mb:MTBDD with type Terminal.t = b)
      (type res) (module Mres:MTBDD with type Terminal.t = res)
      f =
    let module Hash2 = Hashtbl.Make(Pair(Ma)(Mb)) in
    let map2_cache = Hash2.create 251 in
    (* Share cache between applications of map1 etc. *)
    let map1a = map1 (module Ma) (module Mres) in
    let map1b = map1 (module Mb) (module Mres) in
    let rec map2 ma mb =
      let mab = (ma,mb) in
      try Hash2.find map2_cache mab
      with Not_found ->
        let res = match mab with
          | Terminal (_,t), _ -> map1b (f t) mb
          | _, Terminal (_,t) -> map1a (fun x -> f x t) ma
          | If(_,va,thena,elsea), If(_,vb,thenb,elseb) ->
            (match Var.compare va vb with
             | 0 -> Mres.mk va (map2 thena thenb) (map2 elsea elseb)
             | x when x < 0 -> Mres.mk va (map2 thena mb) (map2 elsea mb)
             | _ -> Mres.mk vb (map2 ma thenb) (map2 ma elseb))
        in Hash2.replace map2_cache mab res; res
    in map2
  ;;

  module Triple(M1:Hashtbl.HashedType)(M2:Hashtbl.HashedType)(M3:Hashtbl.HashedType) = struct
    type t = M1.t * M2.t * M3.t
    let equal (a1,a2,a3) (b1,b2,b3) = M1.equal a1 b1 && M2.equal a2 b2 && M3.equal a3 b3;;
    let hash (a1,a2,a3) = 65533 * (M1.hash a1) + 257 * M2.hash a2 + M3.hash a3
  end


  let map3
      (type a) (module Ma:MTBDD with type Terminal.t = a)
      (type b) (module Mb:MTBDD with type Terminal.t = b)
      (type c) (module Mc:MTBDD with type Terminal.t = c)
      (type res) (module Mres:MTBDD with type Terminal.t = res)
      f =
    let module Hash3 = Hashtbl.Make(Triple(Ma)(Mb)(Mc)) in
    let map3_cache = Hash3.create 251 in
    let map2ab = map2 (module Ma) (module Mb) (module Mres)  in
    let map2ac = map2 (module Ma) (module Mc) (module Mres)  in
    let map2bc = map2 (module Mb) (module Mc) (module Mres)  in
    let rec map3 ma mb mc =
      let mabc = (ma,mb,mc) in
      try Hash3.find map3_cache mabc
      with Not_found ->
        let res = match mabc with
          | Terminal (_,t), _, _ -> map2bc (f t) mb mc
          | _, Terminal (_,t), _ -> map2ac (fun a c -> f a t c) ma mc
          | _, _, Terminal (_,t) -> map2ab (fun a b -> f a b t) ma mb
          | If(_,va,thena,elsea), If(_,vb,thenb,elseb), If(_,vc,thenc,elsec) ->
            (match Var.compare va vb with
             | 0 -> begin
                 match Var.compare vb vc with
                 | 0 -> Mres.mk va (map3 thena thenb thenc) (map3 elsea elseb elsec)
                 | x when x < 0 -> Mres.mk va (map3 thena thenb mc) (map3 elsea elseb mc)
                 | _ -> Mres.mk vc (map3 ma mb thenc) (map3 ma mb elsec)
               end
             | x when x < 0 -> begin
                 match Var.compare va vc with
                 | 0 -> Mres.mk va (map3 thena mb thenc) (map3 elsea mb elsec)
                 | x when x < 0 -> Mres.mk va (map3 thena mb mc) (map3 elsea mb mc)
                 | _ -> Mres.mk vc (map3 ma mb thenc) (map3 ma mb elsec)
               end
             | _ -> begin
                 match Var.compare vb vc with
                 | 0 -> Mres.mk vb (map3 ma thenb thenc) (map3 ma thenb elsec)
                 | x when x < 0 -> Mres.mk vb (map3 ma thenb mc) (map3 ma elseb mc)
                 | _ -> Mres.mk vc (map3 ma mb thenc) (map3 ma mb elsec)
               end)
        in
        (* Kernel.feedback "map3 ma %a@\n mb %a@\n mc %a@\n mres %a" *)
        (*   Ma.pretty ma Mb.pretty mb Mc.pretty mc Mres.pretty res; *)
        Hash3.replace map3_cache mabc res; res
    in map3
  ;;

  module MTBDD_Make(Terminal:Terminal) =
  struct

    module Type:sig
      type t = Terminal.t mtbdd
      val equal: t -> t -> bool
      val hash: t -> int
      val compare: t -> t -> int

      val terminal: Terminal.t -> t
      val mk: Var.t -> t -> t -> t
    end =
    struct
      type t = Terminal.t mtbdd

      let equal = (==)
      (* To avoid collision between terminals and non-terminals, I use
         impair values for non-terminals (stored in the tag) and pair
         ones for terminals (stored, or re-computed in the
         Terminal). *)
      (* Note: we could avoid storing the tag in the terminal by just returning
         [2 * Terminal.hash t] here; however this is faster. *)
      let hash = function
        | Terminal (tag,t) -> tag
        | If(tag,_,_,_) -> tag
      ;;

      let compare a b = Stdlib.compare (hash a) (hash b);;

      [@@@ warning "-8"]
      (* We use 2 different hashes for terminal and non-terminal
         terms; this avoids spurious collisions in the table. MAYBE:
         Use GADT to avoid testing the cases of the Sum. *)
      module TerminalHash = Weak.Make(struct
        type t = Terminal.t mtbdd

        let equal (Terminal (_,a)) (Terminal (_,b)) = Terminal.equal a b;;
        let hash (Terminal (_,a)) = Terminal.hash a
      end)

      module IfHash = Weak.Make(struct
        type t = Terminal.t mtbdd
        let equal (If(_,var1,then1,else1)) (If(_,var2,then2,else2)) =
          Var.equal var1 var2 && then1 == then2 && else1 == else2

        let hash (If(_,var,then_,else_)) = Hashing.hash3 (Var.hash var) (hash then_) (hash else_);;
      end)
      [@@@ warning "+8"]

      let tag_ref = ref 1 ;;
      let terminal_table = TerminalHash.create weakhash_default_size;;
      let if_table = IfHash.create weakhash_default_size;;
      let terminal_tag_ref = ref 2;;

      let terminal x =
        let tentative = Terminal (!terminal_tag_ref, x) in
        let ret = TerminalHash.merge terminal_table tentative in
        (if ret == tentative then (terminal_tag_ref := !terminal_tag_ref + 2));
        ret
      ;;

      let mk var then_ else_ =
        if equal then_ else_ then then_
        else
          let tentative = If(!tag_ref, var, then_,else_) in
          let ret = IfHash.merge if_table tentative in
          (if ret == tentative then (tag_ref := !tag_ref + 2));
          ret
      ;;

    (* Note: a drawback of this implementation is that if Terminal is
       already hash-consed, we perform a double hash-consing. We could
       avoid hashconsing Terminal(t) terms; but in this case mtbdds
       would provide a equal function that is not physical
       equality. An advantage of this would be the possibility to
       provide a functional, and not functorial, interface to MTBDDs,
       that would only require that == works on terminals. *)
    end

    include Type

    (* MAYBE: Use a weak key hash, and keep the hash while the bdds
       exist. *)

    module Hash1 = Hashtbl.Make(Type);;
    module Hash2 = HashtblPair(Type);;


    (* Note: the caches here are per-application. They could be shared
       accross different applications, but there should be a different
       cache for each f function. *)
    let map1 f =
      let map1_cache = Hash1.create 251 in
      let rec map1 mtbdd =
        try Hash1.find map1_cache mtbdd
        with Not_found ->
          let res = match mtbdd with
            | Terminal (_, t) -> terminal (f t)
            | If(_,v,then_,else_) -> mk v (map1 then_) (map1 else_)
          in Hash1.replace map1_cache mtbdd res; res
      in map1
    ;;

    let map2 f =
      let map2_cache = Hash2.create 251 in
      let rec map2  bdd1 bdd2 =
        let bdd12 = (bdd1,bdd2) in
        try Hash2.find map2_cache bdd12
        with Not_found ->
          let res = match bdd12 with
            | Terminal (_,t), _ -> map1 (f t) bdd2
            | _, Terminal (_,t) -> map1 (fun x -> f x t) bdd1
            | If(_,v1,then1,else1), If(_,v2,then2,else2) ->
               (match Var.compare v1 v2 with
               | 0 -> mk v1 (map2 then1 then2) (map2 else1 else2)
               | x when x > 0 -> mk v2 (map2 bdd1 then2) (map2 bdd2 else2)
               | _ -> mk v1 (map2 then1 bdd2) (map2 else1 bdd2))
          in Hash2.replace map2_cache bdd12 res; res
      in map2
    ;;


    let all pred =
      let cache = Hash1.create 251 in
      let rec all x =
        try Hash1.find cache x
        with Not_found ->
          let res = match x with
            | Terminal (_,t) when pred t -> BDD.one
            | Terminal (_,t) -> BDD.zero
            | If(_,v,then_,else_) -> BDD.mk v (all then_) (all else_)
          in Hash1.replace cache x res; res
      in all
    ;;


    (* Naive version; more efficient one should use lets to avoid
       combinatory explosion. Note that pretty-printing bdds is
       reserved to debugging anyway. *)
    let rec pretty fmt bdd =
      match bdd with
      | Terminal (_,t) -> Terminal.pretty fmt t
      | If(_,var,bdd1,bdd2) ->
         Format.fprintf fmt "@[<v>@[<hv 2> %a ->@ %a@]@\n@[<hv 2>!%a ->@ %a@]@]"
           Var.pretty var pretty bdd1 Var.pretty var pretty bdd2

    module With_Set(TerminalSet:sig
      type t
      val empty: t
      val singleton: Terminal.t -> t
      val union: t -> t -> t
      (* val add: Terminal.t -> t -> t
       * val pretty: Format.formatter -> t -> unit *)
    end) = struct

      module Hash_MTBDD_BDD = Hashtbl.Make(struct
        type t = Terminal.t mtbdd * BDD.t
        let equal (a1,a2) (b1,b2) = Type.equal a1 b1 && BDD.equal a2 b2
        let hash (a1,a2) = Hashing.hash2 (Type.hash a1) (BDD.hash a2)
      end)

      (* Like in other places, this should maybe be a weak key hash. *)
      let find_all_cache = Hash1.create 251;;
      let find_cache = Hash_MTBDD_BDD.create 251;;

      (* Note: these are all specific version of apply/map2, with
         heterogeneous mtbdds; a bdd being a boolean mtbdd. *)

      (* Faster version: no need to test the BDD. *)
      let rec find_all x =
        try Hash1.find find_all_cache x
        with Not_found ->
          let res = match x with
            | Terminal (_,t) -> TerminalSet.singleton t
            | If(_,_,then_,else_) -> TerminalSet.union (find_all then_) (find_all else_)
          in Hash1.replace find_all_cache x res; res
      ;;

      (* Note: in the following, Zero means "no valuation" and One
         "all valuations".  *)
      let rec find mtbdd bdd =
        (* Kernel.feedback "find mtbdd %a bdd %a" pretty mtbdd BDD.pretty bdd; *)
        let pair = (mtbdd,bdd) in
        try Hash_MTBDD_BDD.find find_cache pair
        with Not_found ->
          let res = match bdd with
            | BDD.Zero -> TerminalSet.empty
            | BDD.One -> find_all mtbdd
            | BDD.If(_,var2,then2,else2) -> (match mtbdd with
              | Terminal (_,t) -> TerminalSet.singleton t
              | If(_,var1,then1,else1) -> (match Var.compare var1 var2 with
                | 0 -> TerminalSet.union (find then1 then2) (find else1 else2)
                | x when x < 0 -> TerminalSet.union (find then1 bdd) (find else1 bdd)
                | _ -> TerminalSet.union (find mtbdd then2) (find mtbdd else2)))
          in Hash_MTBDD_BDD.replace find_cache pair res;
          (* Kernel.feedback "find res: %a" TerminalSet.pretty res; *)
          res
      ;;

      (* As it is, the cache is a memory leak; this version clears the
         cache between invocations. TODO: Benchmarks should tell what
         is the best version. *)
      (* let find mtbdd bdd = *)
      (*   Hash1.clear find_all_cache; *)
      (*   Hash_MTBDD_BDD.clear find_cache; *)
      (*   find mtbdd bdd *)
      (* ;; *)

      (* Note: it may not be necessary to share add_cache between
         invocations of add.  The cache has three entry parameters,
         but the terminal to add need to be looked up only once. *)
      (* let add_cache: mtbdd Hash_MTBDD_BDD.t Hash_Terminal.t = Hash_Terminal.create 251;; *)

      (* For valuations of bdd, set to t2; else set to t1. Also fill
         the cache when the bdd changes. *)
      let rec add_single add_cache t1 bdd t2 singleton =
        try Hash_MTBDD_BDD.find add_cache (t1,bdd)
        with Not_found ->
          let res = match bdd with
          | BDD.Zero -> t1
          | BDD.One ->
             Hash_MTBDD_BDD.replace find_cache (t2,bdd) singleton;
            t2
          | BDD.If(_,var,then_,else_) ->
             let res =
               mk var (add_single add_cache t1 then_ t2 singleton) (add_single add_cache t1 else_ t2 singleton) in
             Hash_MTBDD_BDD.replace find_cache (res,bdd) singleton;
             res
          in Hash_MTBDD_BDD.replace add_cache (t1,bdd) res;
          res
      ;;


      let add mtbdd bdd t =
        let terminal = terminal t in
        let singleton = TerminalSet.singleton t in
        let add_cache =
            (* TODO: Should we share add_cache across invocations of
               add? Probably not for my current use cases.  *)
          (* try Hash_Terminal.find add_cache t *)
          (* with Not_found -> *)
            let new_cache = Hash_MTBDD_BDD.create 17 in
            (* Hash_Terminal.replace add_cache t new_cache; *)
            new_cache
        in
        let rec add mtbdd bdd =
          try Hash_MTBDD_BDD.find add_cache (mtbdd,bdd)
          with Not_found ->
            let res = match bdd with
            | BDD.Zero -> mtbdd
            | BDD.One ->
               (Hash_MTBDD_BDD.replace find_cache (terminal,bdd) singleton;
                terminal)
            | BDD.If(_,var2,then2,else2) ->
               let res = (match mtbdd with
                 | Terminal (_,t2) -> add_single add_cache mtbdd bdd terminal singleton
                 | If(_,var1,then1,else1) -> (match Var.compare var1 var2 with
                   | 0 -> mk var1 (add then1 then2) (add else1 else2)
                   | x when x < 0 -> mk var1 (add then1 bdd) (add else1 bdd)
                   | _ -> mk var2 (add mtbdd then2) (add mtbdd else2))) in
               Hash_MTBDD_BDD.replace find_cache (res,bdd) singleton;
               res
            in Hash_MTBDD_BDD.replace add_cache (mtbdd,bdd) res; res
        (* Pre-fill the cache in the likely case of a further call to find.  *)
        (* Note: it is important not to fill the cache for the
           BDD.Zero case; as find with a BDD of Zero should bottom,
           and not the singleton. *)
        in add mtbdd bdd
      ;;


      (* Simplified version: we know that BDD is one, and do not need to test it. *)
      let rec update_all add_cache mtbdd f =
        try Hash_MTBDD_BDD.find add_cache (mtbdd,BDD.one)
        with Not_found ->
          let res = match mtbdd with
            | Terminal (_,t2) ->
              let term = f t2 in
              let res = terminal term in
              (* We can prefill find_cache here, even if it won't save much work. *)
              Hash_MTBDD_BDD.replace find_cache (res,BDD.one) @@ TerminalSet.singleton term;
              res
            | If(_,var,then_,else_) ->
              mk var (update_all add_cache then_ f) (update_all add_cache else_ f)
          in Hash_MTBDD_BDD.replace add_cache (mtbdd,BDD.one) res; res
      ;;

      (* Note: update could be a base function, but it is not because
         of the need to update the cache. XXX: Which we do not do for now.*)
      (* Note: add is just a special version of update. *)
      let update mtbdd bdd f =
        let add_cache =
          (* TODO: Should we share add_cache across invocations of
             add? Probably not for my current use cases.  *)
          (* try Hash_Terminal.find add_cache t *)
          (* with Not_found -> *)
          let new_cache = Hash_MTBDD_BDD.create 17 in
          (* Hash_Terminal.replace add_cache t new_cache; *)
          new_cache
        in
        let rec update mtbdd bdd =
          try Hash_MTBDD_BDD.find add_cache (mtbdd,bdd)
          with Not_found ->
            let res = match bdd with
              | BDD.Zero -> mtbdd
              | BDD.One -> update_all add_cache mtbdd f
              | BDD.If(_,var2,then2,else2) ->
                let res = (match mtbdd with
                    | Terminal (_,t2) ->
                      let term = f t2 in
                      (* let terminal = f t2 in *)
                      let terminal = terminal term in
                      let singleton = TerminalSet.singleton term in
                      add_single add_cache mtbdd bdd terminal singleton
                    | If(_,var1,then1,else1) -> (match Var.compare var1 var2 with
                        | 0 -> mk var1 (update then1 then2) (update else1 else2)
                        | x when x < 0 -> mk var1 (update then1 bdd) (update else1 bdd)
                        | _ -> mk var2 (update mtbdd then2) (update mtbdd else2))) in
                (* Hash_MTBDD_BDD.replace find_cache (res,bdd) singleton; *)
                res
            in Hash_MTBDD_BDD.replace add_cache (mtbdd,bdd) res; res
            (* Pre-fill the cache in the likely case of a further call to find.  *)
            (* Note: it is important not to fill the cache for the
               BDD.Zero case; as find with a BDD of Zero should bottom,
               and not the singleton. *)
        in update mtbdd bdd
      ;;



    end
  end
end
(* Test. *)
module Var = struct
  type t = int
  let equal = (==)
  let compare = Stdlib.compare
  let hash x = x
  let pretty fmt x = Format.fprintf fmt "%d" x
end

include Make(Var);;

(* Tests BDDs. *)
open BDD
(* include WithCache(struct let cache_default_size = 251 end);; *)

let bdd1 = var 1;;
let bdd2 = var 2;;
let bdd3 = bdd1 &&~ bdd2;;
let bdd4 = bdd2 &&~ bdd1;;
assert( bdd3 == bdd4);;
let bdd5 = bdd1 ==>~ bdd2;;
let bdd6 = (!~ bdd1) ||~ bdd2;;
assert(bdd5 == bdd6);;
!~ bdd6;;

let bdd7 = !~ bdd1;;
let bdd8 = bdd1 ||~ bdd7;;
assert (bdd8 == BDD.one);;

(* Tests MTBDDs. *)
module MTBDD = MTBDD_Make(struct
  type t = int
  let hash = Hashtbl.hash
  let equal = (==)
  let pretty fmt x = Format.fprintf fmt "%d" x
end);;
open MTBDD


let bdd1 = terminal 3;;
let bdd2 = terminal 4;;
let bdd3 = mk 1 bdd1 bdd2;;
let bdd4 = terminal 5;;
let bdd5 = mk 2 bdd2 bdd4;;
map1 (fun x -> x+1) bdd3;;
map1 (fun x -> 8) bdd3;;
map2 (+) bdd3 bdd5;;

let bdd6 = mk 3 bdd1 bdd3;;
let bdd7 = mk 4 bdd3 bdd4;;
map2 (+) bdd6 bdd7;;

let bdd8 = mk 5 (terminal 8) (terminal 8);;
assert (bdd8 == (terminal 8));;

module IntSet = Set.Make(struct
  let compare = Stdlib.compare
  type t = int
end)

module WS = With_Set(IntSet);;

assert ((WS.add (mk 1 (terminal 3) (terminal 4)) (BDD.(!~) (BDD.var 1)) 3) == (terminal 3));;
assert ((WS.add (mk 1 (terminal 3) (terminal 4)) (BDD.var 1) 4) == (terminal 4));;
