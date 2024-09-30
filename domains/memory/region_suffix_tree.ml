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

(* XXX: Mon nouveau plan c'est de faire quelque chose de simple:

   - Si singleton: on applatit dans la structure parente;
   - Seulement si il y a plusieurs indices: on cree un tableau.

 *)

module Log = Tracelog.Make(struct let category = "Domains.Region_suffix_tree" end);;
open Memory_sig;;

module Ival = Framac_ival.Ival

module IntMap = struct
  include Extstdlib.Map.Make(struct
      type t = int
      let compare (a:int) (b:int) = Stdlib.compare a b
    end)

  let update key f map =
    let old = match find key map with
      | exception Not_found -> None
      | v -> Some v
    in
    add key (f old) map
end


module MakePrev(Scalar:Domain_sig.Base) = struct

  let ptrsize = Codex_config.ptr_size();;

  (* The abstract domain can be seen as follow.  Offsets in a region
     are given using paths of successive .field or [expr] suffixes
     Because of join, we may group several paths together using a tree
     (actually, a trie). For instance, .f.a[3].b[4] and .f.a[8].g is
     joined as .f.a[nondet(3,8)].{.g,.b[4]}. The tree allows to identify
     the paths that have the same prefix.

     Because C allows casts, the offsets and the indices are stored
     numerically and not symbolically, but the principle remains. In
     particular, an important point of this domain is that even if
     semantically correct and sound, the way that we partition the
     possible values into a tree commes from the syntax (e.g. .x when
     x is the first field is different than [a] when a is zero). *)
  module Operable_Value(* :Region_numeric_offset.Operable_Value_Enumerable
                        * with module Scalar = Scalar *)
      (* : Fully_expanded_finite_region.Enumerable_Offset *)
  =
  struct

    module Scalar = Scalar
    include Scalar
    module Context = Scalar.Context
    let boolean2scalar_bool _ctx x = x
    let scalar_bool2boolean _ctx x = x
    module Binary_Forward = struct
      include Assert_False_Transfer_Functions.Binary.Binary_Forward
      let bchoose ~size choice _ = assert false
      (* let bcopy = Scalar.Binary_Forward.bcopy *)
    end

    module Bound = struct
      type t =
        | UBound                (* Unknown bound *)
        | CBound of int         (* Constant bound (most common case) *)
      (* | VBound of Scalar.binary Useful with dynamic memory allocation. *)


      let _equal a b = match a,b with
        | UBound, UBound -> true
        | CBound a, CBound b -> a == b
        | UBound, CBound _ | CBound _, UBound -> false

      (* Manipulate bound using the notion of over-approximation of
         intersection and union of bound intervals. *)
      let inter (mina,maxa) (minb,maxb) =
        let min = match mina,minb with
          | CBound a, CBound b -> CBound (max a b)
          | UBound, x | x, UBound -> x
        and max = match maxa,maxb with
          | CBound a, CBound b -> CBound (min a b)
          | UBound, x | x, UBound -> x
        in (min,max)
      ;;

      let join (mina,maxa) (minb,maxb) =
        let min = match mina,minb with
          | CBound a, CBound b -> CBound (min a b)
          | UBound, x | x, UBound -> UBound
        and max = match maxa,maxb with
          | CBound a, CBound b -> CBound (max a b)
          | UBound, x | x, UBound -> UBound
        in (min,max)
      ;;

      let _pretty fmt = function
        | UBound -> Format.fprintf fmt "UBound"
        | CBound a -> Format.fprintf fmt "CBound(%d)" a
      ;;


      let bchoose _choice x = x

    end
    open Bound;;


    (* A node in the tree of possible paths in a memory region. The
       path from the root of the tree to a node of the tree represent
       a set of possible offsets. Every index in this node is
       considered relatively (i.e. as an increment) to these offsets. *)
    type t = {
      (* If true: the pointer may land here. Can be considered as
         0-offset, or an index of value 0. *)
      leaf: bool;
      (* Mapping from k -> suffixes when some field k is taken. *)
      offsets: t IntMap.t;
      (* Mapping k -> expression * suffixes, when index e is taken
         (for arrays with elements of size k).
         MAYBE: use integer intead of binary for indices. *)
      indices: (Scalar.binary * t) IntMap.t;

      (* The interest of putting min and max in each node is to have
         more precise information when joining pointers to different
         locations with different mins and maxes.

         For computing pointers, the offsets corresponding to min and
         max are both included.  For access, only min is included, max
         is excluded. *)
      min:Bound.t; max:Bound.t;
    }
    ;;

    let empty_suffix =
      { leaf = false;
        offsets = IntMap.empty;
        indices = IntMap.empty;
        min = CBound 0; max = CBound (-13);
      };;

    let offset_empty = fun ~size ctx -> empty_suffix
    ;;

    (* No Top representable in this domain. *)
    let binary_unknown ~size:_ _ctx = assert false
    let binary_unknown_typed ~size:_ _ctx = assert false
    let has_type ~size:_ = assert false

    let binary2scalar_binary ~size _ctx value = assert false
    let assume_type ~size _ctx value typ = assert false


    (* Maybe we should use hashconsing. *)
    (* let rec equal a b =
     *   a == b ||
     *   (a.leaf == b.leaf
     *    && Bound.equal a.min b.min
     *    && Bound.equal a.max b.max
     *    && (a.offsets == b.offsets || (IntMap.equal equal a.offsets b.offsets))
     *    && (a.indices == b.indices || (
     *        let equal (ab,at) (bb,bt) =
     *          Scalar.Binary.equal ab bb && equal at bt
     *        in
     *        IntMap.equal equal a.indices b.indices)))
     *    ;; *)

    let pp subpp fmt x =
      let rec pp fmt {leaf;offsets;indices;min;max} =
        Format.fprintf fmt "@[<v>";
        (* Format.fprintf fmt "(%a-%a)@[<v>" Bound.pretty min Bound.pretty max; *)
        let count = ref 0 in
        if IntMap.is_empty offsets && IntMap.is_empty indices then
          if leaf then ()
          else Format.fprintf fmt "+ <bottom>"
        else if leaf then Format.fprintf fmt " + 0@,"
                                         (* Only display + 0 when in an interior node. *)
        else ();
        offsets |> IntMap.iter (fun i x ->
                       (if !count > 0 then Format.pp_print_cut fmt ()); incr count;
                       Format.fprintf fmt " + %d%a" i pp x);
        indices |> IntMap.iter (fun i (e,x) ->
                       (if !count > 0 then Format.pp_print_cut fmt ()); incr count;
                       Format.fprintf fmt " + %d * %a%a" i subpp
                         e pp x);
        Format.fprintf fmt "@]";
      in pp fmt x
    ;;


    (* Note: if line wrapping occurs, the value can be misread. *)
    let offset_pretty ~size ctx fmt x = pp (Scalar.binary_pretty ~size ctx) fmt x

    type binary = t
    type offset = binary
    module Binary = struct
      type t = binary
      let equal a b = (a == b)  (* Fast equality check. *)
      let compare _ = assert false
      let hash _ = assert false
      let pretty = pp Scalar.Binary.pretty
    end
    module Offset = Binary


    module Query = struct
      include Scalar.Query

      module ZSet = Extstdlib.Set.Make(Z)

      module Binary_Lattice = struct
        include ZSet
        let top ~size = assert false
        let is_bottom ~size = is_empty
        let bottom ~size = empty
        let includes ~size _ = assert false
        let join ~size = union
        let inter ~size = inter
        let hash _ = assert false
        let widen ~size ~previous x = union previous x
        let includes_or_widen ~size ~previous _ = assert false
        let pretty ~size _ = assert false
        let singleton ~size k = ZSet.singleton k
      end

      (* MAYBE: Remove this this function. At least I don't enumerate
         using it anymore. *)
      let binary_to_ival ~signed ~size x =
        assert signed; (* unsigned unimplemented *)
        ZSet.fold (fun x acc -> Ival.join (Ival.inject_singleton x) acc) x Ival.bottom
      ;;

      let binary_to_known_bits ~size x = assert false

      let binary_fold_crop ~size bin ~inf ~sup acc f =
        ZSet.fold (fun x acc ->
            if Z.leq inf x && Z.leq x sup then f x acc else acc
          ) bin acc
      ;;

      let binary_is_empty ~size x = ZSet.is_empty x ;;

      let binary_is_singleton ~size x =
        if ZSet.is_singleton x then
          let res = ZSet.choose x in
          assert(Z.leq Z.zero res);
          Some(res) else None
      ;;

      (* Note: should not be used: if an ival returns top, we enumerate everything.
         Probably we should use a multi-interval instead. *)
      let binary ~size ctx x =
        let _ = assert false in
        (* Codex_log.feedback "binary %a" (binary_pretty ~size ctx) x; *)
        let rec loop { leaf; offsets; indices} =
          let acc = if leaf then ZSet.singleton Z.zero else ZSet.empty in
          let acc = IntMap.fold (fun i x acc ->
              ZSet.union acc @@ ZSet.map (Z.add (Z.of_int i)) (loop x)
            ) offsets acc in
          let acc = IntMap.fold (fun k (i,t) acc ->
              let ival = assert false in
              (* let ival = Scalar.Query.binary ~size ctx i |> Scalar.Query.binary_to_ival ~size in *)
              let lt = loop t in
              let res = Ival.fold_int (fun i acc ->
                  ZSet.union acc @@ ZSet.map (Z.add (Z.mul (Z.of_int k) i)) lt
                ) ival ZSet.empty in
              ZSet.union acc res)
              indices acc
          in
          acc
        in loop x
      ;;

    end

    (**************** Join and widening ****************)

    let serialize_offset ~size ctxa a ctxb b acc =
      (* Codex_log.feedback "suffixtree.serialize2 %a %a "
       *   (binary_pretty ~size ctx) a  (binary_pretty ~size ctx) b; *)
      (* We need a polymorphic recursion on the type of the input and output tuples. *)
      let rec suffix:type a. _ -> _ -> a Context.in_acc -> (_,a) Context.result =
        fun a b in_acc ->
          let Context.Result(inc_ofs,acc,d_offsets) = offset a.offsets b.offsets in_acc in
          let Context.Result(inc_ind,acc,d_indices) =
            indices a.indices b.indices (inc_ofs,acc) in
          Context.Result(inc_ofs && inc_ind, acc, fun ctx tup ->
              let indices,tup = d_indices ctx tup in
              let offsets,tup = d_offsets ctx tup in
              let (min,max) = Bound.join (a.min,a.max) (b.min,b.max) in
              let res = {leaf=a.leaf||b.leaf;offsets;indices; min; max } in
              (* Codex_log.feedback "serialize2 a %a b %a res %a"
               *   (binary_pretty ~size ctx) a  (binary_pretty ~size ctx) b
               *   (binary_pretty ~size ctx) res; *)
              res,tup
            )

      and offset: type a. _ -> _ -> a Context.in_acc -> (_,a) Context.result =
        fun a b (included, acc) ->
        IntMap.fold_on_diff2 a b (Context.Result(included, acc, fun _ctx tup -> a,tup))
          (fun i sa sb (Context.Result(inc,acc,d_map) as res) ->
             match (sa,sb) with
             | Some a, Some b when a == b -> res
             | None, None -> res
             | _,_ ->
               let empty_if_none = function
                 | Some x -> x
                 | None -> empty_suffix
               in
               let Context.Result(inc,acc,d_suffix) =
                 suffix (empty_if_none sa) (empty_if_none sb) (inc,acc)
               in
               Context.Result(inc,acc,fun ctx tup ->
                   let suffix,tup = d_suffix ctx tup in
                   let map,tup = d_map ctx tup in
                   IntMap.add i suffix map,tup)
          )
      and indices: type a. _ -> _ -> a Context.in_acc -> (_,a) Context.result =
        fun a b (included, acc) ->
        IntMap.fold_on_diff2 a b (Context.Result(included, acc, fun _ctx tup -> a,tup))
          (fun i sa sb (Context.Result(inc,acc,d_map) as res) ->
             match (sa,sb) with
             | Some a, Some b when a == b -> res
             | None, None -> res
             | _ ->
               let empty_if_none ctx = function
                 | Some x -> x
                 | None -> (Scalar.binary_empty ~size ctx,empty_suffix)
               in
               let (ea,sa) = empty_if_none ctxa sa
               and (eb,sb) = empty_if_none ctxb sb in
               let Context.Result(inc,acc,d_suffix) = suffix sa sb (inc,acc) in
               let equal = ea == eb || Scalar.Binary.equal ea eb in
               let Context.Result(inc,acc,d_e) =
                 if equal then Context.Result(inc,acc,fun _ctx tup -> ea,tup)
                 else Scalar.serialize_binary ~size ctxa ea ctxb eb (inc,acc)
               in
               Context.Result(inc, acc, fun ctx tup ->
                   let e,tup = d_e ctx tup in
                   let suffix,tup = d_suffix ctx tup in
                   let map,tup = d_map ctx tup in
                   IntMap.add i (e,suffix) map,tup)
          )
      in
      suffix a b acc
    ;;

    (* Used in some transfer functions. *)
    let nondet_binary ~size ctx v1 v2 =
      let Context.Result(_,tup,deserialize) =
        Scalar.serialize_binary ~size ctx v1 ctx v2 (true,Context.empty_tuple ())
      in
      let res_tup = Scalar.nondet_same_context ctx tup in
      let res,(_: Context.empty_tuple Context.out_tuple) = deserialize ctx res_tup in
      res
    ;;

    (**************** Transfer functions. ****************)

    (* Note: In GCC, the max object size is PTRDIFF_MAX, which is
       INT_MAX. This limits the length of arrays to
       PTRDIFF_MAX/sizeof(element). *)

    (* Translate a suffix tree into linear expressions +
       nondet. This is an extension of the translation of a suffix
       path into a linear expression. *)
    let to_linear_exp ~size ctx x =
      let rec loop {leaf;offsets; indices} =
        let acc = if leaf then [Scalar.Binary_Forward.biconst ~size Z.zero ctx] else [] in
        let acc = IntMap.fold (fun k (e,x) acc ->
            let k = Scalar.Binary_Forward.biconst ~size (Z.of_int k) ctx in
            let ke = Scalar.Binary_Forward.bimul ~size ~nsw:true ~nuw:false ctx k e in
            (Scalar.Binary_Forward.biadd ~size ~nsw:false ~nuw:false ~nusw:true ctx ke @@ loop x)::acc
          ) indices acc in
        let acc = IntMap.fold (fun k x acc ->
            let k = Scalar.Binary_Forward.biconst ~size (Z.of_int k) ctx in
            (Scalar.Binary_Forward.biadd ~size ~nsw:false ~nuw:false ~nusw:true ctx k @@ loop x)::acc
          ) offsets acc in
        match acc with
        | [a] -> a
        | [] -> Scalar.binary_empty ctx ~size (* MAYBE: assume false on this? *)
        | a::b -> List.fold_left (fun acc x ->
            nondet_binary ~size ctx acc x) a b
      in loop x
    ;;


    (* Comparison just translate terms to linear expressions, and
       then compare. It could probably be made more precise, but as
       most trees contain a single path, this is sufficient in practice. *)
    let ble ~size ctx a b =
      let la = to_linear_exp ~size ctx a and lb = to_linear_exp ~size ctx b in
      let res = Scalar.Binary_Forward.biule ~size ctx la lb in
      (* Codex_log.feedback "la %a lb %a res %a" (Scalar.binary_pretty ~size ctx) la
         (Scalar.binary_pretty ~size ctx) lb (Scalar.boolean_pretty ctx) res; *)
      res
    ;;

    let beq ~size ctx a b =
      let la = to_linear_exp ~size ctx a and lb = to_linear_exp ~size ctx b in
      let res = Scalar.Binary_Forward.beq ~size ctx la lb in
      (* Codex_log.feedback "la %a lb %a res %a" (Scalar.binary_pretty ~size ctx) la
         (Scalar.binary_pretty ~size ctx) lb (Scalar.boolean_pretty ctx) res; *)
      res
    ;;

    let bisub ~size ctx a b =
      let ba = to_linear_exp ~size ctx a in
      let bb = to_linear_exp ~size ctx b in
      Scalar.Binary_Forward.bisub ~size ~nsw:true ~nuw:false ~nusw:false ctx ba bb
    ;;

    let rec bchoose ~size choice ctx x = {
      leaf=x.leaf;
      offsets = IntMap.map (bchoose ~size choice ctx) x.offsets;
      indices = IntMap.map (fun (bin,t) -> Scalar.Binary_Forward.bchoose ~size choice ctx bin,
                                           bchoose ~size choice ctx t) x.indices;
      min=Bound.bchoose choice x.min;
      max=Bound.bchoose choice x.max
    }
    ;;

    (* No other offset than leaf. *)
    let only_leaf ~max =
      {leaf = true; offsets = IntMap.empty; indices = IntMap.empty; max; min = CBound 0};;

    let only_leaf_no_bound =
      {leaf = true; offsets = IntMap.empty; indices = IntMap.empty; max = UBound; min = UBound};;

    let zero_offset ~size ~max ctx =
      match max with
      | None -> only_leaf ~max:UBound
      | Some m -> only_leaf ~max:(CBound m)

    (* A version without offset compression (except for 0 offsets). *)
    (* let rec _bshift ~size ~offset ctx ~max:reqmax {leaf; offsets; indices; min; max } =
     *   if offset = 0 then {leaf; offsets; indices; min; max }
     *   else
     *     let indices = indices |> IntMap.map (fun (e1,s1) -> (e1,_bshift ~size ~offset ctx s1 ~max:reqmax)) in
     *     let offsets = IntMap.map (fun s -> _bshift ~size ctx ~offset s ~max:reqmax) offsets in
     *     (\* Add the new offset to the result. The offset k may already be there. *\)
     *     let offsets =
     *       if leaf then
     *         IntMap.update offset (function
     *             | None -> only_leaf ~max:reqmax
     *             | Some v -> {v with leaf = true}
     *           ) offsets
     *       else offsets
     *     in
     *     {leaf=false;offsets;indices;min;max}
     * ;; *)


    (* This version does "offset compression": successive offsets are
       added together. This reduces space usage. But as "+ 6 + 2" and
       "+ 8" are no longer separate, in some (rare) cases (join with
       unions) it could lead to precision losses. *)
    let rec bshift ~size ~offset ~max:reqmax ctx x =
      let {leaf; offsets; indices; min=oldmin; max=oldmax } = x in
      (* Improve the given bound with the already known bounds. *)
      let (newmin,newmax) = Bound.inter (oldmin,oldmax) (CBound 0, reqmax) in
      (* Note: Offset compression is incorrect when we have with min
         and max, in the case of array indexing followed by a
         struct. Do not use it. *)
      (* if offset = 0 then { x with min=newmin; max=newmax }
       * else *)
        let indices = indices |> IntMap.mapi (fun k (e1,s1) ->
          (* When "entering" array elements, we cannot go beyond their size.
             Actually sometimes we can; e.g. * (struct toto * ) &char_buffer[i];
             So we remove this limitation. *)
          (* let (_,max) = Bound.inter (UBound,CBound k) (UBound,newmax) in *)
            let max = newmax in
            (e1,bshift ~size ~offset ~max ctx s1)) in
        let offsets =
          let acc = if leaf then IntMap.singleton offset (only_leaf ~max:newmax) else IntMap.empty in
          IntMap.fold (fun key s acc ->

            (* If the offset point to a leaf: perform offset compression. *)
            let acc =
              if s.leaf then
                IntMap.update (key + offset) (function
                    | None -> (only_leaf ~max:newmax)
                    | Some olds ->
                      (* If there is already something (should be very
                         rare), it means that we were in case (HHH)
                         for another index. In this case, leaf should
                         be false.  *)
                      assert (olds.leaf = false);
                      let (min,max) = Bound.inter (olds.min,olds.max) (newmin,newmax) in
                      {olds with leaf = true; min; max }
                  ) acc
              else acc
            in

            (* If the offset contains other things: shift them too, keep the old offset. *)
            let acc =
              if IntMap.is_empty s.offsets && IntMap.is_empty s.indices
              then acc
              (* This is the (HHH) case *)
              else IntMap.add key (bshift ~size ~offset ~max:reqmax ctx {s with leaf = false}) acc
            in
            acc
          ) offsets acc in
        {leaf=false;offsets;indices;min=oldmin;max=oldmax}
    ;;

    let bshift ~size ~offset ~max ctx x =
      let max = match max with
        | None -> UBound
        | Some k -> CBound k
      in
      let res = bshift ~size ~offset ~max ctx x in
      (* Codex_log.feedback "after bshift: min %a max %a" Bound.pretty res.min Bound.pretty res.max; *)
      res
    ;;



    (* Go to the end of every path, and append an index. *)
    let rec bindex ~size k ctx {leaf;offsets;indices;min=oldmin;max=oldmax} e =
      assert(k != 0);
      (* Doing this allows to reduce the possible values for indices
         when used in indices. The C standard mandates that array
         accesses are such that you can only go one past the last
         element of an array.

         Unfortunately, for now we cannot generate C alarms in Frama-C
         on such operations, so doing this assume could make further
         invalid memory accesses become unnoticeable. So we do not
         activate it. Event then, it would be better to perform the
         assume using this alarm, and not during a regular
         operation. So this code is here for "historical" reasons. *)
      let _assume_bound min max k e =
        let acc = match min with
          | UBound -> Scalar.Boolean_Forward.true_ ctx
          | CBound x ->
            Scalar.Binary_Forward.bisle ~size ctx
              (Scalar.Binary_Forward.biconst ~size (Z.of_int (x/k)) ctx)
              e
        in
        let _acc = match max with
          | UBound -> acc
          | CBound x ->
            Scalar.Boolean_Forward.(&&) ctx acc @@
            Scalar.Binary_Forward.bisle ~size ctx e
              (Scalar.Binary_Forward.biconst ~size (Z.of_int (x/k)) ctx)
        in
        (* Codex_log.feedback "bound: %a %a %a %a" Bound.pretty min Bound.pretty max (Scalar.boolean_pretty ctx) acc (Scalar.binary_pretty ~size ctx) e ; *)
        (* Scalar.assume_binary ~size ctx acc e *)
        assert false
      in

      let assume_bound min max k e = e in


      (* Codex_log.feedback "Bindex %a %a" (binary_pretty ~size ctx) x
         (Scalar.binary_pretty ~size ctx) e; *)
      let new_offsets = offsets |> IntMap.map (fun x -> bindex ~size k ctx x e) in
      let new_indices:(Scalar.binary * t) IntMap.t =
        (IntMap.remove k indices) |> IntMap.mapi (fun k1 (e1,s) ->
            let r = bindex ~size k ctx s e in
            (* A posteriori limiting the accessible indices is a bad
               idea, for instance in this program:
               int * p = &x[0]; short *q = p; q[3].
               We do not want that q is such that &x[0] <= q < &x[1].  *)
            (* MAYBE: if size is different from 1 (i.e. is not a char) we can. *)
            (* let min = Bound.max (CBound 0) r.min in
             * let max = Bound.min (CBound k1) r.max in *)
            (* (e1,{r with min; max})) in *)
            (e1,r)) in
        let new_indices =
          match IntMap.find k indices with
          | exception Not_found ->
            if leaf
            then begin
              (* The index goes at most at the end of the array. *)
              let e = assume_bound oldmin oldmax k e in
              IntMap.add k (e,only_leaf_no_bound) new_indices
            end
            else new_indices
          | (e1, s1) ->
            if leaf then begin
              (* Add the zero of leaf to both the index and s1. *)
              let with_zero = nondet_binary ~size ctx e1 (Scalar.Binary_Forward.biconst ~size  Z.zero ctx) in
              let add = Scalar.Binary_Forward.biadd ~size ~nsw:true ~nuw:false ~nusw:false ctx with_zero e in
              let add = assume_bound oldmin oldmax k add in
              IntMap.add k (add,{s1 with leaf}) new_indices
            end
            else begin
              if s1 == only_leaf_no_bound then
                (* The only non-corner case. The use of this fast path
                   has a small incidence (a few percents) on the
                   benchmarks. *)
                (* Offset compression: + k * x + k * y -> k * (x + y)  *)
                let add = Scalar.Binary_Forward.biadd ~size ~nsw:true ~nuw:false ~nusw:false ctx e1 e in
                IntMap.add k (add,only_leaf_no_bound) new_indices
              else if s1.leaf = true then begin
                if ( IntMap.is_empty s1.offsets && IntMap.is_empty s1.indices) then begin
                  let add = Scalar.Binary_Forward.biadd ~size ~nsw:true ~nuw:false ~nusw:false ctx e1 e in
                  IntMap.add k (add,only_leaf_no_bound) new_indices
                end
                else begin
                  (* Probably vastly imprecise, and never
                     tested. Remove this assert false when (if) we
                     have a test (should be very rare). *)
                  let _ = assert false in
                  let _news1 = bindex ~size k ctx {s1 with leaf = false} e in
                  let _add = Scalar.Binary_Forward.biadd ~size ~nsw:true ~nuw:false ~nusw:false ctx e1 e in
                  let maybe_add = nondet_binary ~size ctx e1 e in
                  IntMap.add k (maybe_add,{s1 with leaf = true}) new_indices
                end
              end
              else begin
                (* Weird case, but should work in theory. Remove the
                   assert false when we have a test. *)
                let _  = assert false in
                IntMap.add k (e1,bindex ~size k ctx s1 e) new_indices
              end
            end
      in
      (* Codex_log.feedback "after bindex: min %a max %a" Bound.pretty oldmin Bound.pretty oldmax; *)
      {leaf=false;offsets=new_offsets;indices=new_indices;min=oldmin;max=oldmax}
    ;;

    (* Folding, taking care of not enumerating indefinitevely. For
       this reason, we build an offset "to_add" that we use to crop
       the folding on possible array indices.

       We maintain inf and sup as bounds that cannot be traversed. *)
    let fold_crop ~size ctx x ~inf ~sup f acc =
      (* Codex_log.feedback "fold_crop inf %s sup %s" (Z.to_string inf) (Z.to_string sup); *)
      (* in C, it is allowed to go beyond the max value by one, but
         one cannot use this value for access.  *)
      let for_access = true in
      let module ZSet = Query.ZSet in

      (* We build this temporary set to make sure that each indice
         appears only once. Generally the memory domain should work
         even if indices apperead twice, which is generally rare. But
         building this temporary set has almost no impact on the
         benchmarks. *)
      let rec loop ~inf ~sup to_add { leaf; offsets; indices; min; max} acc =
        (* Codex_log.feedback "loop inf %s sup %s x %a " (Z.to_string inf) (Z.to_string sup) (binary_pretty ~size ctx) { leaf; offsets; indices; min; max}; *)
        let inf = match min with
          | CBound min -> Z.max inf (Z.add to_add (Z.of_int min))
          | UBound -> inf
        in
        let sup = match max with
          | CBound max ->
            Z.min sup (Z.add to_add (Z.of_int @@ if for_access then max - 1 else max))
          | UBound -> sup
        in
        let acc = IntMap.fold (fun i x acc ->
            loop ~inf ~sup (Z.add to_add @@ Z.of_int i) x acc
          ) offsets acc in
        let acc = IntMap.fold (fun k (i,t) acc ->
            let bin = Scalar.Query.binary ~size ctx i in
            let ival_min = Z.div (Z.sub inf to_add) (Z.of_int k) in
            let ival_max = Z.div (Z.sub sup to_add) (Z.of_int k) in
            let res = Scalar.Query.binary_fold_crop ~size bin ~inf:ival_min ~sup:ival_max acc (fun i acc ->
                loop ~inf ~sup (Z.add to_add (Z.mul (Z.of_int k) i)) t acc
              ) in
            res)
            indices acc in
        if leaf && Z.leq inf to_add && Z.leq to_add sup
        then ZSet.add to_add acc
        else acc
      in
      (* We use this set, instead of folding directly, to avoid the
         cases where an index could appear more than once. *)
      let set = loop ~inf ~sup Z.zero x ZSet.empty in
      let card = ZSet.cardinal set in
      (* Codex_log.feedback "ZSet: %a" (ZSet.mk_pretty (fun fmt x -> Format.fprintf fmt "%s" @@ Z.to_string x)) set; *)
      (if card > 255 then Codex_log.warning "folding on a large number (%d) of locations" card);
      ZSet.fold f set acc
    ;;


    (* Like fold_crop, but with an intermediary set limited to a
       single element. MAYBE: we could share the folding code
       (especially because of the bound refinement.) *)
    let is_precise ~size ctx x =
      let module ZSet = Query.ZSet in
      let exception Exit in
      let add x = function
        | None -> Some x
        | Some _ -> raise Exit
      in

      let rec loop acc to_add { leaf; offsets; indices} =
        let acc = if leaf then add to_add acc  else acc in
        let acc = IntMap.fold (fun i x acc ->
            loop acc (Z.add to_add @@ Z.of_int i) x
          ) offsets acc in
        let acc = IntMap.fold (fun k (i,t) acc ->
            let i = Scalar.Query.binary ~size ctx i
                    |> Scalar.Query.binary_is_singleton ~size
                    |> (function Some x -> x | None -> raise Exit) in
            loop acc (Z.add to_add (Z.mul (Z.of_int k) i)) t
          ) indices acc
        in acc
      in
      match loop None Z.zero x with
      | None -> Memory_sig.Empty
      | Some res -> Memory_sig.Singleton res
      | exception Ival.Not_Singleton_Int -> Memory_sig.Imprecise
      | exception Exit -> Memory_sig.Imprecise
    ;;

    let in_bounds ~size ctx x ~inf ~sup = assert false ;;

    (* Check that each array index is within its bounds. Note that
       this is complementary to checking that a pointer is within a
       block, in that it also checks the bounds of arrays within a
       block.

       Note that when the suffix tree has several branches, we must use
       "nondet" to build the correct boolean expression (using (&&)
       would be incorrect!) *)
    let within_bounds ~size ctx off =

      (* Codex_log.feedback "within_bounds for %a" (binary_pretty ~size ctx) off; *)
      let _for_access = true in

      let module Bool = struct

        (* True is the most common value: we optimize to avoid useless
           conjunctions and nondet with true. *)
        type t = { true_: bool;
                   value: Scalar.boolean option }

        let nondet2_bool v1 v2 =
          let Context.Result(_,tup,deserialize) =
            Scalar.serialize_boolean ctx v1 ctx v2 (true,Context.empty_tuple ())
          in
          let res_tup = Scalar.nondet_same_context ctx tup in
          let res,_ = deserialize ctx res_tup in
          res
        ;;

        let nondet a b = {
          true_= a.true_ || b.true_;
          value= match a.value,b.value with
            | None, x | x, None -> x
            | Some a, Some b -> Some (nondet2_bool a b)
        }

        let true_ = { true_ = true; value = None }
        (* let value x = { true_ = false; value = Some x} *)
        let empty = { true_ = false; value = None}

        let _pretty fmt x = match x.value with
          | None -> Format.fprintf fmt "{true=%b;value=None}" x.true_;
          | Some y -> Format.fprintf fmt "{true=%b;value=Some(%a)}"
                        x.true_ (Scalar.boolean_pretty ctx) y
        ;;

        let (&&) x y = match x.true_, x.value with
          | true, Some x ->
            (* Flatten before doing the conjunction. *)
            let x = nondet2_bool x (Scalar.Boolean_Forward.true_ ctx) in
            let value = Some(Scalar.Boolean_Forward.(&&) ctx x y) in
            {true_ = false; value}
          | false, None -> empty
          | true, None -> {true_ = false; value = Some y }
          | false, Some x -> {true_ = false;
                              value = Some (Scalar.Boolean_Forward.(&&) ctx x y)}
        ;;

        let flatten x = match x.true_, x.value with
          | true, None -> Scalar.Boolean_Forward.true_ ctx
          | false, Some x -> x
          | true, Some x -> nondet2_bool (Scalar.Boolean_Forward.true_ ctx) x
          | false, None -> assert false  (* TODO: empty boolean.  *)


      end in

      let rec loop ~inf ~sup {leaf;offsets;indices;min;max}  =

        (* Codex_log.feedback "loop inf %a sup %a min %a max %a lead %b %a"
           Bound.pretty inf Bound.pretty sup Bound.pretty min Bound.pretty max
           leaf (binary_pretty ~size ctx) {leaf;offsets;indices;min;max}; *)
        let (inf,sup) = Bound.inter (inf,sup) (min,max) in
        let acc = if leaf then Bool.true_ else Bool.empty in
        let acc = IntMap.fold (fun _k x acc ->
            Bool.nondet acc @@ loop ~inf ~sup x
          ) offsets acc in
        let acc = IntMap.fold (fun k (e,x) acc ->
            let v = loop ~inf ~sup x in
            let v = match inf with
              | UBound -> v
              | CBound m ->
                (* Codex_log.feedback "min: m %d k %d" m k; *)
                Bool.(&&) v @@
                Scalar.Binary_Forward.bisle ~size:ptrsize ctx
                  (Scalar.Binary_Forward.biconst ~size:ptrsize (Z.of_int @@ m/k) ctx) e
            in
            let v = match sup with
              | UBound -> v
              | CBound m ->
                (* let to_sub = if for_access then 1 else 0 in *)
                (* Codex_log.feedback "max: m %d k %d" m k; *)
                Bool.(&&) v @@
                Scalar.Binary_Forward.bisle ~size:ptrsize ctx e
                  (* XXX: -1 ou utiliser la taille ici plutot? *)
                  (* (Scalar.Binary_Forward.biconst ~size:ptrsize (Z.of_int @@ m/k - to_sub) ctx) *)
                  (Scalar.Binary_Forward.biconst ~size:ptrsize (Z.of_int @@ (m - (size/8))/k) ctx)
            in
            Bool.nondet acc v
          ) indices acc in
        (* Codex_log.feedback "loop result: %a" Bool.pretty acc; *)
        acc
      in
      let res = loop ~inf:UBound ~sup:UBound off in
      let res = Bool.flatten res in
      (* Codex_log.feedback "within_bounds res: %a" (Scalar.boolean_pretty ctx) res; *)
      res
    ;;

    let _within_bounds ~size ctx _ = Scalar.Boolean_Forward.true_ ctx;;
    module Offset_transfer_functions = struct
      let offset_eq = beq
      let offset_le = ble
      let offset_zero = zero_offset
      let offset_sub = bisub
      let offset_index = bindex
      let offset_shift = bshift
      let offset_within_bounds = within_bounds
      let offset_choose = bchoose
    end
    include Offset_transfer_functions

    let query_boolean = Scalar.query_boolean
  end
  module Address = Operable_Value


  (* TODO: This should be placed in another module. *)

  (* The argument for an array should be a cell, but let's name it like this for now. *)
  module type Array_Arg = sig

    type t

    module Value:Value
      (* (\* An initial memory region, that contains the empty value. *\)
       * val initial: memsize:int -> Scalar.Context.t -> t
       *
       * (\* Reading and writing values out of a cell. *\)
       * val load: memsize:int -> offset:int -> size:int -> Scalar.Context.t -> t -> Value.binary
       * (\* val store: memsize:int -> offset:int -> size:int -> Scalar.Context.t -> t -> Value.binary -> t *\)
       * val store_cell: memsize:int -> offset:int -> size:int -> Scalar.Context.t -> t -> Cell.t -> t         *)


    (* Reading and writing values out of a cell. Probably temporary, i.e. can be replaced by extract? *)
    val load: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Value.binary (* Value.binary *)

    (* Copying part of a cell. *)
    val extract: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> t

    val serialize:memsize:int -> Value.Context.t -> t -> Value.Context.t ->  t -> 'a Value.Context.in_acc -> (t,'a) Value.Context.result

    val pretty:memsize:int -> Value.Context.t -> Format.formatter -> t -> unit

    val union: memsize:int -> Value.Context.t -> t -> t -> t

    val nconcat: Value.Context.t -> (int * t) list -> t

    val shared_addresses: memsize:int -> Value.Context.t -> t -> Value.Context.t -> t -> (Value.binary * Value.binary) list

  end

  module type Array_result = sig
    type t
    type cell
    module Value:Value

    val pretty:memsize:int -> Value.Context.t -> Format.formatter -> t -> unit

    (* Converts a cell into an array with a single cell whose
       contents is cell. *)
    val singleton: memsize:int -> Value.Context.t -> cell -> t

    (* Calls f on each cell matching e in array (whose elements have size k), returning a new array. *)
    (* Two functions are provided, for strong and weak update. We
       could do with only the strong one, but it is too
       inefficient. *)
    (* TODO: provide weak with the condition in which the value is written,
       so that it can "assume" on it. *)
    val subst: memsize:int -> Scalar.Context.t -> Scalar.binary -> int -> t ->
      strong:(memsize:int -> offset:int ->
              min:Operable_Value.Bound.t -> max:Operable_Value.Bound.t ->
              cell -> cell) ->
      weak:(memsize:int -> offset:int ->
            min:Operable_Value.Bound.t -> max:Operable_Value.Bound.t ->
            cell -> cell) ->
      t

    (* Loading a single element in an array. Useful at least for now. *)
    (* Probably temporary. *)
    val load: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Value.binary

    (* Extract a cell from an array. *)
    val extract: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> cell

    val serialize:memsize:int ->
      Value.Context.t -> t -> Value.Context.t -> t ->
      'a Value.Context.in_acc ->
      (t,'a) Value.Context.result

    val shared_addresses: memsize:int -> Value.Context.t -> t -> Value.Context.t -> t -> (Value.binary * Value.binary) list

    end


  (* In this implementation, the array is just a single cell. *)
  module Make_Array1(Cell:Array_Arg):Array_result
    with type cell = Cell.t
     and module Value = Cell.Value
  = struct

    module Value = Cell.Value
    type cell = Cell.t

      (* A simple implementation where Arrays are complete cells. *)
      type t = Cell.t

      let pretty = Cell.pretty;;

      let extract = Cell.extract;;

      let load = Cell.load;;

      let singleton ~memsize ctx cell = cell;;

      let serialize = Cell.serialize;;

      let shared_addresses = Cell.shared_addresses

      let sizeof _ = assert false
      let concat ~size1:_ ~size2:_ _ = assert false
      let binary_to_block ~size:_ _ = assert false

      module Bound = Operable_Value.Bound
      (* Different when we will do array squashing. *)
      let subst ~memsize ctx e k (* ~min ~max *) cell ~strong ~weak =
        (* let ctx = to_sub ctx in *)
        (* Codex_log.feedback "Array.subst memsize %d cell %a"
         *   memsize (Cell.pretty ~memsize ctx) cell; *)
        let ptr_size = (Codex_config.ptr_size()) in
        let bin = Scalar.Query.binary ~size:ptr_size ctx e in
        match Scalar.Query.binary_is_singleton ~size:ptr_size bin with
        | Some i -> begin (* Strong update. *)
          let i = Z.to_int i in
          let ki = k * i in
          (* assert(0 <= i);
           * assert(ki + size <= memsize); *)
          let open Bound in
          let min = (* match min with UBound -> UBound | CBound m -> *) CBound (0 - ki) in
          let max = (* match max with UBound -> UBound | CBound m -> *) CBound (memsize - ki) in
          let cell = strong ~memsize ~offset:ki ~min ~max cell in
          cell
          (* let open Bound in
           * let min = match min with UBound -> UBound | CBound m -> CBound (m - ki) in
           * let max = match max with UBound -> UBound | CBound m -> CBound (m - ki) in
           * PtrInteraction.store ~min ~max ~memsize ~offset:(offset + ki) ~size ctx mem suffix value *)
        end
        | None when Scalar.Query.binary_is_empty ~size:ptr_size bin -> raise Memory_Empty
        | None -> begin
          let inf = Z.of_int 0 and sup = (Z.of_int @@ (memsize - 1)/k) in
          let res = Scalar.Query.binary_fold_crop ~size:ptr_size bin ~inf ~sup cell (fun i acc ->
              (* Codex_log.feedback "fold i %s %a" (Z.to_string i) (Cell.pretty ~memsize ctx) acc; *)
              let i = Z.to_int i in
              let open Bound in
              let ki = k * i in
              let min = (* match min with UBound -> UBound | CBound m -> *) CBound (0 - ki) in
              let max = (* match max with UBound -> UBound | CBound m -> *) CBound (memsize - ki) in
              let acc = weak ~memsize ~offset:ki ~min ~max acc in
              acc) in
          res
        end
      ;;

    end

  (* Fully squashing implementation. *)
  module Make_Array2(Value:Value with module Context=Scalar.Context)(Cell:Array_Arg with module Value = Value):Array_result
    with type cell = Cell.t
     and module Value = Value
  = struct

    module Context = Scalar.Context

    (* An array with an unknown (non-null) number of cells with a fixed size. *)
    type t = {
      cell_size: int;
      cell_contents: Cell.t;
    }

    module Value = Value
    type cell = Cell.t


    let pretty ~memsize ctx fmt cell = Cell.pretty ~memsize ctx fmt cell.cell_contents
    let singleton ~memsize ctx cell = {cell_size=memsize;cell_contents=cell};;

    (* Grow the array n times.  *)
    let grow ctx array n =
      let rec loop i acc = match i with
        | 0 -> acc
        | _ -> loop (i-1) ((array.cell_size,array.cell_contents)::acc)
      in { cell_size=n*array.cell_size;
           cell_contents=Cell.nconcat ctx @@ loop n []}
    ;;

    (* Shrink the array n times; the original size must be a multiple of n. *)
    let shrink ctx array n =
      let oldsize = array.cell_size in
      let mem = array.cell_contents in
      assert (oldsize mod n == 0);
      let size = oldsize / n in
      let rec loop i acc =
        if i == oldsize then acc
        else
          let acc = Cell.union ~memsize:size ctx acc @@
            Cell.extract ~memsize:oldsize ~offset:i ~size ctx mem in
          loop (i+size) acc
      in
      let new_content = loop size (Cell.extract ~memsize:oldsize ~offset:0 ~size ctx mem) in
      { cell_contents = new_content;
        cell_size = size }
    ;;

    let rec gcd a b =
      assert(a > 0);
      assert(b > 0);
      match (a mod b) with
      | 0 -> b
      | r -> gcd b r
    ;;

    let lcm m n =
      assert(m > 0);
      assert(n > 0);
      (m * n) / (gcd m n)
    ;;

    (* Resize the array so that it has size k. *)
    let resize ctx array k =
      let oldsize = array.cell_size in
      let big_size = lcm oldsize k in
      let big = grow ctx array @@ big_size / oldsize in
      shrink ctx big @@ big_size / k
    ;;

    (* Resize the array to k. *)
    (* let resize ctx array k =  *)

    let _resize ctx array k =
      let oldsize = array.cell_size in
      (* Codex_log.feedback "Resize old %d k %d@." oldsize k; *)
      let mem = array.cell_contents in
      if oldsize == k
      then array                (* Quick case. *)
      else begin
        (* For now. *)
        assert(oldsize mod k == 0 || k mod oldsize == 0);
        (* Real algorithm: first concatenate cell so that the new size
           is a multiplier/gcd of k, and then extract everything and
           join. *)
        if oldsize mod k == 0 then begin
          let rec loop i acc =
            if i == oldsize then acc
            else
              let acc = Cell.union ~memsize:k ctx acc @@
                Cell.extract ~memsize:oldsize ~offset:i ~size:k ctx mem in
              loop (i+k) acc
          in
          let new_content = loop k (Cell.extract ~memsize:oldsize ~offset:0 ~size:k ctx mem) in
          { cell_contents = new_content;
            cell_size = k }
          (* XXX: Extract all, union between them, and use this. *)
        end
        else assert false       (* TODO *)
      end
    ;;

    let subst ~memsize ctx e k array ~strong ~weak =
      let resized = resize ctx array k in
      (* Completely ignore e *)
      let mem = resized.cell_contents in
      (* let mem = weak ~memsize:k ~offset:0 ~min:(Operable_Value.Bound.CBound 0) ~max:(Operable_Value.Bound.CBound 1) mem in *)
      let mem = weak ~memsize:k ~offset:0 ~min:(Operable_Value.Bound.UBound) ~max:(Operable_Value.Bound.UBound) mem in
      (* Codex_log.feedback "Result: resized %d memsize %d" resized.cell_size memsize; *)
      {resized with cell_contents = mem }
    ;;

    (* XXX: load enumerates the indices, but there should be something
       like subst for the load.  , *)
    let load ~memsize ~offset ~size ctx array =
      let k = array.cell_size in
      let offset = offset mod k in
      let array =
        if offset + size <= k then array
        (* Grow the cell to the smallest multiple needed so that the
           load can succeed. *)
        else grow ctx array ((offset + size + (k - 1))/k)
      in
      let v = Cell.load ~memsize:array.cell_size ~offset ~size ctx array.cell_contents in
      let level = Cell.Value.Context.level ctx in
      Value.Binary_Forward.bchoose ~size:(8 * size) (Transfer_functions.Choice.fresh ~level) ctx v
    ;;

    let extract ~memsize ~offset ~size ctx array =
      (* assert false; *)
      let k = array.cell_size in
      let offset = offset mod k in
      let array =
        if offset + size <= k then array
        else grow ctx array ((offset + size + (k - 1))/k)
      (* Should this call Cell.choose? *)
      in Cell.extract ~memsize:array.cell_size ~offset ~size ctx array.cell_contents
    ;;

    (* If size is not equal, take the gcm. *)
    let serialize ~memsize ctxa a ctxb b acc =
      (* We serialize using the largest length, as shrinking brings imprecisions. *)
      let newsize = lcm a.cell_size b.cell_size in
      let a = grow ctxa a (newsize / a.cell_size) in
      let b = grow ctxb b (newsize / b.cell_size) in
      assert(a.cell_size == newsize);
      assert(b.cell_size == newsize);
      (* ICI: les tests plantent a cause de ca. Il faut resizer en plus grand *)
      (* assert(a.cell_size == b.cell_size); *)
      let Context.Result(inc,acc,d) = Cell.serialize ~memsize:newsize ctxa a.cell_contents ctxb b.cell_contents acc
      in Context.Result(inc, acc, (fun ctx tup -> let x,tup = d ctx tup in {cell_size=newsize;cell_contents=x},tup))
    ;;

    let shared_addresses ~memsize ctxa a ctxb b =
      let newsize = lcm a.cell_size b.cell_size in
      let a = grow ctxa a (newsize / a.cell_size) in
      let b = grow ctxb b (newsize / b.cell_size) in
      assert(a.cell_size == newsize);
      assert(b.cell_size == newsize);
      Cell.shared_addresses ~memsize:newsize ctxa a.cell_contents ctxb b.cell_contents
    ;;

    let sizeof _ = assert false
    let concat ~size1:_ ~size2:_ _ = assert false
    let binary_to_block ~size:_ _ = assert false



    (* subst Operation: based on the written size and original size, we may
       have to resize or shrink the array by using extract,concat, and
       "merges/folds". *)

      (* Ici. Deplacer le merge sur les cells dans cell, rajouter un concat sur les cells,
       * et c'est parti!
       *
       * NB: Les cells ont une taille fixe, mais les arrays ne sont pas (toujours) des cells  *)
  end




  module Memory
      (Value:Value)
      (Lift:sig val ctx: Value.Context.t -> Address.Context.t * (Address.Context.t -> Value.Context.t) end)
  = struct
    (* module Scalar = Scalar *)
    module Domain:Domain_sig.Minimal
      with module Context = Value.Context
       and type boolean = Value.boolean
      = Value
    include Domain

    let to_sub x = fst @@ Lift.ctx x

    module Value = Value
    module Offset = Operable_Value
    type offset = Operable_Value.binary

    (* Join values with nondet. Works only when the array is fully-expanded.  *)
    let join_values_nondet ~size ctx v1 v2 =
      let Context.Result(_,tup,deserialize) =
        Value.serialize ~size ctx v1 ctx v2 (true, Context.empty_tuple ())
      in
      let res_tup = Value.nondet_same_context ctx tup in
      let res,_ = deserialize ctx res_tup in
      res
    ;;


    (* Join values with union. *)
    let join_values_union ~size ctx v1 v2 =
      let Context.Result(_,tup,deserialize) =
        Value.serialize ~size ctx v1 ctx v2 (true, Context.empty_tuple ())
      in
      let level = Value.Context.level ctx in
      let condition = Transfer_functions.Condition.fresh ~level in
      let res_tup = Value.union condition ctx tup in
      let res,_ = deserialize ctx res_tup in
      res
    ;;

    let join_values = match Codex_config.array_expansion() with
      | `full_expansion -> join_values_nondet
      | `full_squashing -> join_values_union
    ;;

    (* Choose between array implementations. *)
    module Make_Array(Cell:Array_Arg with module Value = Value):
    Array_result with type cell = Cell.t
                  and module Value = Value = struct
      module M1 = Make_Array1(Cell)
      (* module M2 = Make_Array2(Value)(Cell) *)
      (* module M = (val (if true then (module M1) else (module M2)): *)
      (*               Array_result with type cell = Cell.t *)
      (*                             and type value = Cell.value) *)
      include M1

    end



    (* First part: the memory structures. Sizes are all in bytes. *)
    (* MAYBE: store the size inside the objects, as is already the
       case when using interval maps. *)
    module rec Cell:sig

      (* A cell is a contiguous memory region. *)
      type t = Array of Array.t | Value of Value.binary | Struct of Struct.t
      (* | Concat of a,b, for flexible array members? *)


      (* An initial memory region, that contains the empty value. *)
      (* MAYBE: pass it the initial (char) value for the region. *)
      val initial: memsize:int -> Value.Context.t -> t

      (* Reading and writing values out of a cell. *)
      val load: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Value.binary
      (* val store: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Value.binary -> t *)
      val store_cell: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Cell.t -> t

      (* Copying part of a cell. *)
      val extract: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> t

      (* Maybe: Concat: easy with structs, just put 2 cells one after the other.
         Useful for array manipulation.  *)

      val serialize:memsize:int -> Value.Context.t -> t -> Value.Context.t -> t ->
        'a Value.Context.in_acc -> (t,'a) Value.Context.result

      val pretty:memsize:int -> Value.Context.t -> Format.formatter -> t -> unit

      (* "Fold" two (non-empty) cells together; useful e.g. for squashing array elements.  *)
      val union: memsize:int -> Value.Context.t -> t -> t -> t

      (* Concatenates two cells to make a larger cell (low addresses is the first argument) *)
      (* val concat: Value.Context.t ->
       *   memsize_low:int -> Cell.t ->
       *   memsize_high:int -> Cell.t ->
       *   t *)

      (* Concatenantes n cells in a larger cell. List elements are (size,cell); the head of the list
         represents the low addresses. *)
      val nconcat: Value.Context.t -> (int * t) list -> t

      val shared_addresses: memsize:int -> Value.Context.t -> t -> Value.Context.t -> t -> (Value.binary * Value.binary) list

    end = struct

      (* We choose to have Value < Array < Struct. In part because it
         is easy to convert a Value to a singleton array, and to
         convert any array to a struct. This means that joining a
         Value and a Array yields an Array, for intance. *)
      (* XXX: On doit pouvoir avoir une structure plus simple, ou les struct contiennent
         soit des valeurs soit des tableaux, et les tableaux contiennent
         toujours des structures. Ca demande seulement une structure intermediaire (d'un seul element)
         quand on a des tableaux de tableaux.

         => Migrer vers ce modele petit a petit?. *)
      type t =
        | Array of Array.t
        | Value of Value.binary
        | Struct of Struct.t
      ;;

      let initial ~memsize ctx =
        let zero = Value.binary_empty ~size:memsize ctx in
        Value zero
      ;;

      let pretty ~memsize ctx fmt = function
        | Value v -> Format.fprintf fmt "Value(%a)" (Value.binary_pretty ~size:(memsize * 8) ctx) v
        | Array a -> Format.fprintf fmt "Array(%a)" (Array.pretty ~memsize ctx) a
        | Struct s -> Format.fprintf fmt "Struct(%a)" (Struct.pretty ~memsize ctx) s;;
                        (* Struct.pretty ~memsize ctx fmt s *)

      let serialize ~memsize ctxa a ctxb b acc =
        (* Codex_log.feedback "Cell.serialize2 %a %a" (pretty ~memsize ctx) a (pretty ~memsize ctx) b; *)
        match a,b with
        | Value a, Value b ->
          let Context.Result(inc,acc,d) = Value.serialize ~size:(memsize * 8) ctxa a ctxb b acc in
          Context.Result(inc, acc, (fun ctx tup -> let x,tup = d ctx tup in Value x,tup))
        | Struct a, Struct b ->
          let Context.Result(inc,acc, d) = Struct.serialize ~memsize ctxa a ctxb b acc in
          Context.Result(inc, acc, (fun ctx tup -> let x,tup = d ctx tup in Struct x,tup))
        | Array a, Array b ->
          let Context.Result(inc,acc, d) = Array.serialize ~memsize ctxa a ctxb b acc in
          Context.Result(inc, acc, (fun ctx tup -> let x,tup = d ctx tup in Array(x),tup))
        (* Decay into an array. *)
        | Value a, Array b ->
          let Context.Result(inc, acc, d) =
            Array.serialize ~memsize ctxa (Array.singleton ~memsize ctxa @@ Value a) ctxb b acc in
          Context.Result(inc, acc, (fun ctx tup -> let res,tup = d ctx tup in Array res,tup))
        | Array a, Value b ->
          let Context.Result(inc, acc, d) =
            Array.serialize ~memsize ctxa a ctxb (Array.singleton ~memsize ctxb @@ Value b) acc in
          Context.Result(inc, acc, (fun ctx tup -> let res,tup = d ctx tup in Array res,tup))
        (* Decay into a struct. *)
        | (Value _ | Array _) as a, Struct b ->
          let Context.Result(inc, acc, d) =
            Struct.serialize ~memsize ctxa (Struct.singleton ~memsize a) ctxb b acc in
          Context.Result(inc, acc, (fun ctx tup -> let res,tup = d ctx tup in Struct res,tup))
        | Struct a, ((Value _ | Array _) as b) ->
          let Context.Result(inc, acc, d) =
            Struct.serialize ~memsize ctxa a ctxb (Struct.singleton ~memsize b) acc in
          Context.Result(inc, acc, (fun ctx tup -> let res,tup = d ctx tup in Struct res,tup))
        ;;


      let store_cell  ~memsize ~offset ~size ctx memory cell =
        (* Codex_log.feedback "store_here offset %d memsize %d size %d" offset memsize size; *)
        match memory with
        | _ when memsize == size -> assert(offset == 0); cell
        | Struct s -> Struct(Struct.store ~memsize ~offset ~size ctx s cell)
        | Value _ | Array _ ->
          (* Convert into a structure, then store. Storing might split
             the memory into contiguous intervals, which is handled
             there. *)
          let s = Struct.singleton ~memsize memory in
          Struct(Struct.store ~memsize ~offset ~size ctx s cell)
      ;;

      let _store_cell  ~memsize ~offset ~size ctx memory value =
        let res =  store_cell  ~memsize ~offset ~size ctx memory value in
        (* Codex_log.feedback "store_cell @\nold:%a@\nnew:%a" (pretty ~memsize ctx) old (pretty ~memsize ctx) res; *)
        res
      ;;


      let _store  ~memsize ~offset ~size ctx memory value =
        store_cell ~memsize ~offset ~size ctx memory @@ Value value
      ;;




      let load ~memsize ~offset ~size ctx memory =
        match memory with
        | Value v when memsize == size -> assert(offset == 0); v
        | Value v ->
          assert(offset >= 0);
          Value.Binary_Forward.bextract ctx v
            ~index:(offset * 8) ~size:(size * 8) ~oldsize:(memsize * 8)
        | Struct s -> Struct.load ~memsize ~offset ~size ctx s
        | Array a -> Array.load ~memsize ~offset ~size ctx a

      let typed_load ~size ctx mem x typ = assert false ;;
      let typed_store ~size ctx mem at typ value = assert false ;;

      (* Note: extract et concat peuvent s'implementer a partir de
         memcopy. Et memcopy il suffit de faire un fold sur les
         valeurs (meme pas besoin d'extraire, le faire au niveau plus
         haut dans intervalmap pour laisser l'extraction lazy).  *)
      let extract ~memsize ~offset ~size ctx cell =
        if offset == 0 && size == memsize
        then cell
        else match cell with
          | Value v ->     (* Note: depends on endianness. *)
            assert(offset >= 0);
            let v = Value.Binary_Forward.bextract ctx v
                ~index:(offset * 8) ~size:(size * 8) ~oldsize:(memsize * 8) in
            Value v
          | Struct s -> Struct.extract ~memsize ~offset ~size ctx s
          | Array a -> Array.extract ~memsize ~offset ~size ctx a

      ;;

      let _extract  ~memsize ~offset ~size ctx memory =
        let res =  extract  ~memsize ~offset ~size ctx memory in
        (* Codex_log.feedback "Cell.extract @\nold:%a@\nnew:%a" (pretty ~memsize ctx) old (pretty ~memsize:size ctx) res; *)
        res
      ;;

      (* MAYBE: drop this memsize argument. But we could have to store
         the size with Value, which could be costly.*)
      let union ~memsize ctx m1 m2 =
        let Context.Result(_,tup,deserialize2) = serialize ~memsize ctx m1 ctx m2 (true, Context.empty_tuple ()) in
        let level = Value.Context.level ctx in
        let res_tup = Value.union (Transfer_functions.Condition.fresh ~level) ctx tup in
        let res,_ = deserialize2 ctx res_tup in res
      ;;

      (* XXX: blit is just extract + store_cell. Implement this and use it.  *)
      let blit ~memsize ctx from_mem from_offset to_mem to_offset len =
      (* ArrayLabels.blit *)
      (* let concat  *)
        assert false

      (* Concatenation. mem_low will have the lower addresses, and mem_high the highest. *)
      let concat ctx ~memsize_low mem_low ~memsize_high mem_high =
        let memsize = memsize_low + memsize_high in
        let mem = Struct.singleton ~memsize (initial ~memsize ctx) in
        let mem = Struct.store ~memsize ~offset:0 ~size:memsize_low ctx mem mem_low in
        let mem = Struct.store ~memsize ~offset:memsize_low ~size:memsize_high ctx mem mem_high in
        Struct mem
      ;;

      let nconcat ctx l =
        let memsize = List.fold_left (fun acc (size,_) -> acc+size) 0 l in
        let mem = Struct.singleton ~memsize (initial ~memsize ctx) in
        let (_,res) = List.fold_left (fun (offset,acc) (size,cell) ->
            (offset + size, Struct.store ~memsize ~offset ~size ctx acc cell)
          ) (0,mem) l in
        Struct res
      ;;

      let rec combine_sorted_list ls1 ls2 =
        match ls1,ls2 with
        | [], _ | _, [] -> []
        | (i1,v1) :: tl1, (i2, v2) :: tl2 ->
          if i1 = i2 then (v1,v2) :: combine_sorted_list tl1 tl2
          else if i1 < i2 then combine_sorted_list tl1 ls2
          else combine_sorted_list ls1 tl2

      let shared_addresses ~memsize ctxa a ctxb b =
        Log.debug (fun p -> p "Region_suffix_tree.Cell.shared_addresses ~memsize:%d" memsize);
        match a,b with
        | Value a, Value b ->
          let lsa = Value.contained_addresses ~size:(8 * memsize) ctxa a in
          let lsb = Value.contained_addresses ~size:(8 * memsize) ctxb b in
          combine_sorted_list lsa lsb

        | Struct a, Struct b ->
          Struct.shared_addresses ~memsize ctxa a ctxb b
        | Array a, Array b ->
          Array.shared_addresses ~memsize ctxa a ctxb b
        (* Decay into an array. *)
        | Value a, Array b ->
          Array.shared_addresses ~memsize ctxa (Array.singleton ~memsize ctxa @@ Value a) ctxb b
        | Array a, Value b ->
          Array.shared_addresses ~memsize ctxa a ctxb (Array.singleton ~memsize ctxb @@ Value b)
        (* Decay into a struct. *)
        | (Value _ | Array _) as a, Struct b ->
          Struct.shared_addresses ~memsize ctxa (Struct.singleton ~memsize a) ctxb b
        | Struct a, ((Value _ | Array _) as b) ->
          Struct.shared_addresses ~memsize ctxa a ctxb (Struct.singleton ~memsize b)
        ;;

      let sizeof _ = assert false
      let concat ~size1:_ ~size2:_ _ = assert false
      let binary_to_block ~size:_ _ = assert false


      (* TODO: concat, or blit. This consists in creating a new
         structure, and writing the two parts in it. Maybe blit would
         generalize both extract and concat. *)
    end
    and Struct:sig
      type t

      (* Create a new structure with a single interval corresponding to cell. *)
      val singleton: memsize:int -> Cell.t -> t

      val store: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Cell.t -> t
      val load: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Value.binary

      val pretty:memsize:int -> Value.Context.t -> Format.formatter -> t -> unit

      (* Copying part of a struct. *)
      val extract: memsize:int -> offset:int -> size:int -> Value.Context.t -> t -> Cell.t

      val serialize:memsize:int -> Value.Context.t -> t -> Value.Context.t -> t ->
        'a Value.Context.in_acc -> (t,'a) Value.Context.result

      val shared_addresses:memsize:int -> Value.Context.t -> t -> Value.Context.t -> t -> (Value.binary * Value.binary) list

    end = struct

      module IntervalMap = Interval_map.With_Extract(Cell)
      type t = IntervalMap.t
      let singleton ~memsize c = IntervalMap.create ~size:memsize c

      (* Note that when we store, we may overwrite a part of an older
         value. We do this lazily (only if the split value is later
         accessed): we do not compute immediately what the old value
         is. Indeed it can be overwritten in the future, so that
         computation is maybe not necessary. *)
      let store ~memsize ~offset ~size ctx memory value =
        IntervalMap.store ~size offset memory value


      let pretty ~memsize ctx fmt map =
        let extract ctx v ~idx ~size ~oldsize =
          (v,idx,size,oldsize)
          (* Do not perform evaluations with pretty. *)
          (* Cell.extract ~memsize:oldsize ~offset:idx ~size ctx v *)
        in
        IntervalMap.iter_between ~size:memsize 0 map ~extract:(extract ctx) (fun ~size offset v ->
            Format.fprintf fmt " (%d-%d)" offset (offset+size-1);
            let (v,idx,size,oldsize) = v in
            if size == oldsize && idx == 0
            then Cell.pretty ctx ~memsize:size fmt v
            else Format.fprintf fmt "extract(%d,%d,%a)" idx size (Cell.pretty ctx ~memsize:oldsize) v
          )
      ;;



      let load ~memsize ~offset ~size (ctx:Value.Context.t) memory =
        let extract ctx v ~idx ~size ~oldsize =
          Cell.load ~memsize:oldsize ~offset:idx ~size ctx v
        in
        let key = offset in
        let map = memory in
        assert(size > 0);
        assert(key >= 0);
        let region_size = IntervalMap.get_size memory in
        (* Codex_log.feedback "load: memsize %d region_size %d" memsize region_size; *)
        assert(memsize == region_size);
        (* Codex_log.feedback "loading offset %d size %d region_size %d %a"
           key size region_size (pretty ctx) {map}; *)
        assert(key + size <= region_size);
        (* Codex_log.feedback "load idx %d size %d" key size; *)
        let l = IntervalMap.fold_between ~size key map []
            ~extract:(extract ctx)
            (fun ~size key value acc ->
               (size,value)::acc)
        in match l with
        | [] -> assert false    (* The address is correct, we must load something. *)
        | [_,v] -> v
        | (sizea,a)::rest ->
          (* TODO: this depends on endiannees *)
          let f (size,acc) (newsize,newv) =
            let newsize = newsize * 8 in
            let acc = Value.Binary_Forward.bconcat ctx ~size1:size acc ~size2:newsize newv in
            (size+newsize,acc)
          in
          let fsize,v = List.fold_left f (sizea*8,a) rest in
          assert (fsize == size * 8);
          v
      ;;

      let typed_load ~size ctx mem x typ = assert false ;;
      let typed_store ~size ctx mem at typ value = assert false ;;

      (* TODO. Should be similar to load, but the way the cells are
         merged is different: we build a new structure and writes all
         the values in it. Maybe Intervalmap could provide functions
         to do that more efficiently, but this will do for now. *)
      let extract ~memsize ~offset ~size ctx memory =
        let extract ctx v ~idx ~size ~oldsize =
          Cell.extract ~memsize:oldsize ~offset:idx ~size ctx v
        in
        let key = offset in
        let map = memory in
        assert(size > 0);
        assert(key >= 0);
        let region_size = IntervalMap.get_size memory in
        assert(memsize == region_size);
        (* Codex_log.feedback "loading offset %d size %d region_size %d %a"
           key size region_size (pretty ctx) {map}; *)
        assert(key + size <= region_size);
        (* Kernel.feedback "load idx %d size %d" key size; *)
        let l = IntervalMap.fold_between ~size key map []
            ~extract:(extract ctx)
            (fun ~size key value acc ->
               (size,value)::acc)
        in match l with
        | [] -> assert false    (* The address is correct, we must load something. *)
        | [s,v] -> assert(s == size); v
        | _ ->
          (* Note that fold build the list with the right part first. *)
          let extracted_size = size in
          let acc = IntervalMap.create ~size:extracted_size (Obj.magic 0) in
          let (offset,acc) = List.fold_left (fun (offset,acc) (size,cell) ->
              let offset = offset - size in
              let acc = store ~memsize:extracted_size ~offset ~size ctx acc cell in
              (offset,acc)
            ) (extracted_size,acc) l
          in
          assert(offset == 0);
          let acc:t = acc in
          (* Fixes the "Double vision" problem of the recursive modules. *)
          let acc:Struct.t = Obj.magic acc in
          Cell.Struct(acc)
      ;;

      (* This wrapper could be used to implement cell extraction
         lazily. This could be interesting here because this function
         eventually just folds on values, so we should not have to
         build intermediary extracted structs. But it is probably not
         worth the burden to implement it. *)
      module ExtractedCell:sig

        type t
        val extract :
          memsize:int ->
          offset:int -> size:int -> Value.Context.t -> Cell.t -> t

        val serialize:memsize:int -> Value.Context.t -> t -> Value.Context.t -> t ->
          'a Value.Context.in_acc -> (Cell.t,'a) Value.Context.result

      end = struct
        type t = Cell.t
        let extract = Cell.extract
        let serialize = Cell.serialize
      end

      let mk_extract ctx =
        (fun cell ~idx ~size ~oldsize ->
           ExtractedCell.extract ctx ~memsize:oldsize ~size ~offset:idx cell)
      ;;

      let serialize ~memsize ctxa a ctxb b (inc, acc) =
        (* Codex_log.feedback "serialize [%a] [%a]" (pretty ~memsize ctx) a (pretty ~memsize ctx) b; *)
        (* Note that we may end up serializing equal cells with
           different representation (i.e. they are not ==), but we will
           realize that when serializing sub-cells (and eventually, values). *)
        IntervalMap.fold_on_diff a b (Context.Result(inc, acc,fun _ctx tup -> a,tup))
          ~extracta:(mk_extract ctxa) ~extractb:(mk_extract ctxb)
          (fun ~size offset a b (Context.Result(inc,acc,d_map)) ->
             let Context.Result(inc,acc,d_cell) = ExtractedCell.serialize ~memsize:size
              ctxa a ctxb b (inc,acc) in
             Context.Result(inc,acc,fun ctx tup ->
                 let cell,tup = d_cell ctx tup in
                 let map,tup = d_map ctx tup in
                 IntervalMap.store offset map ~size cell,tup
               ))

      ;;

      let shared_addresses ~memsize ctxa a ctxb b = [] (* TODO : improve this *)

      let sizeof _ = assert false
      let concat ~size1:_ ~size2:_ _ = assert false
      let binary_to_block ~size:_ _ = assert false

    end


    (* TODO: 3 implementations:
       - Fully unsquashed
       - Fully squashed: squash the whole array even if we write at a single location.
       - Interval squashed: reuing structures, we decompose into contiguous intervals that
         are squashed. Useful in combination with loop unrolling. *)
    and Array:sig
      include Array_result with type cell := Cell.t and module Value := Value
    end =
      Make_Array(struct include Cell module Value = Value end)
      (* Make_Array1(struct include Cell type value = Value.binary end) *)
      (* Make_Array2(Value)(struct include Cell type value = Value.binary end) *)




    (* Note: if we used sets of cells (or arrays of cells) here, we
       would require operations so that loading from a set of cells
       returns a set of values. *)
    type block =
      {size:int;
       contents:Cell.t }


    let get_size {size} = size
    let initial ctx size =
      let contents = Cell.initial ~memsize:size ctx in
      {size;contents}

    let may_alias ~ptr_size:_ _ = assert false
    let should_focus ~size:_ _ = assert false
    let block_empty _ = assert false
    let free _ = assert false
    let malloc ~id:_ ~malloc_size:_ _ = assert false
    let unknown ~level:_ ctx = assert false;;


    let serialize ctxa a ctxb b acc =
      assert(a.size == b.size);
      let Context.Result(inc,acc,deserialize) =
        Cell.serialize ~memsize:a.size ctxa a.contents ctxb b.contents acc
      in Context.Result(inc, acc, (fun ctx tup -> let contents,tup = deserialize ctx tup in
                              {size=a.size;contents},tup))


    let shared_addresses ctxa a ctxb b =
      assert (a.size == b.size);
      Cell.shared_addresses ~memsize:a.size ctxa a.contents ctxb b.contents

    let sizeof _ = assert false
    let concat ~size1:_ ~size2:_ _ = assert false
    let binary_to_block ~size:_ _ = assert false

    (**************** Using the pointers to access the memory. ****************)

    module Bound = Operable_Value.Bound
    (* PtrInteraction and Indices both follow the pointer structure
       (defined in Operable_Value) to perform the individual load
       operations. PtrInteraction is generic and handles shifts, while
       Indices is specific to the index operation (and is linked on
       the way array are implemented. *)

    module type PtrInteraction = sig
      (* Load and store with [size] at [offset], in a cell [t],
         if within bounds [min] and [max] (which are relative to
         offset). *)
      val load: min:Bound.t -> max:Bound.t ->
        memsize:int -> offset:int -> size:int -> Value.Context.t ->
        Cell.t -> offset -> Value.binary

      val store: memsize: int -> offset:int -> size:int ->
        min:Bound.t -> max:Bound.t ->
        Value.Context.t ->
        Cell.t -> offset -> Value.binary -> Cell.t
    end

    module type Indices = sig
      (* Load and store with [size] at [offset + k * e], in a cell
         [t]. *)
      val load: min:Bound.t -> max:Bound.t ->
        memsize:int -> offset:int -> size:int -> Value.Context.t ->
        Scalar.binary -> int -> offset -> Cell.t -> Value.binary

      val store: min:Bound.t -> max:Bound.t ->
        memsize:int -> offset:int -> size:int -> Value.Context.t ->
        Scalar.binary -> int -> offset -> Cell.t -> Value.binary -> Cell.t
    end


    module MakePtrInteraction(Indices:Indices):PtrInteraction = struct


      (* Note: load and store have the same structure, but slight
         differences (the Memory_Empty exception, the fact that there
         is an extra "value" argument) make it cumbersome to factorize. *)
      let load ~min ~max ~memsize ~offset ~size ctx memory suffix =
        let open Operable_Value in
        let rec loop ~min ~max ~memsize ~offset ~size ctx memory suffix acc =
          (* Codex_log.feedback "PtrInteraction.load[loop] min %a max %a
           *    offset %d size %d memsize %d suffix %a" Bound.pretty min
           *    Bound.pretty max offset size memsize
           *    (Operable_Value.binary_pretty ~size:32 ctx) suffix; *)
          let (min,max) = Bound.inter (min,max) (suffix.min, suffix.max) in
          (* Optimization: When the bound is impossible, no need to fold in the rest of the tree. *)
          match min,max with
          | Bound.CBound min, Bound.CBound max when min + size > max -> acc
          | _ ->
            let open Bound in
            let acc =
              if suffix.leaf &&
                 (match min with UBound -> true | CBound m -> m <= 0) &&
                 (match max with UBound -> true | CBound m -> size <= m)
              then (Cell.load ~memsize ~offset ~size ctx memory)::acc
              else acc
            in
            let acc = IntMap.fold (fun off suffix acc ->
                let min = match min with UBound -> UBound | CBound m -> CBound (m - off) in
                let max = match max with UBound -> UBound | CBound m -> CBound (m - off) in
                loop ~min ~max ~memsize ~offset:(offset+off) ~size ctx memory suffix acc
              ) suffix.offsets acc
            in
            let acc = IntMap.fold (fun k (e,suffix) acc ->
                (Indices.load ~min ~max ~memsize ~offset ~size ctx e k suffix memory)::acc
              ) suffix.indices acc
            in
            acc
        in
        let res = loop ~min ~max ~memsize ~offset ~size ctx memory suffix [] in
        match res with
        | [] -> (Value.binary_empty ~size:(size * 8) ctx)
        | [x] -> x
        | a::b -> List.fold_left (join_values ~size:(size * 8) ctx) a b
      ;;

      let store ~memsize ~offset ~size ~min ~max ctx memory suffix value =
        let open Operable_Value in
        let rec loop ~min ~max ~memsize ~offset ~size ctx memory suffix value acc =
          (* Codex_log.feedback "PtrInteraction.load[loop] min %a max %a offset %d
           *    size %d memsize %d suffix %a" Bound.pretty min  Bound.pretty max offset
           *    size memsize (Operable_Value.binary_pretty ~size:32 ctx) suffix; *)
          let (min,max) = Bound.inter (min,max) (suffix.min,suffix.max) in
          (* Optimization: When the bound is impossible, no need to fold in the rest of the tree. *)
          match min,max with
          | Bound.CBound min, Bound.CBound max when min + size > max -> acc
          | _ ->
            let open Bound in
            let acc =
              if suffix.leaf &&
                 (match min with UBound -> true | CBound m -> m <= 0) &&
                 (match max with UBound -> true | CBound m -> size <= m)
              then
                try (Cell.store_cell ~memsize ~offset ~size ctx memory @@ Cell.Value value)::acc
                with Memory_Empty -> acc
              else acc
            in
            let acc = IntMap.fold (fun off suffix acc ->
                let min = match min with UBound -> UBound | CBound m -> CBound (m - off) in
                let max = match max with UBound -> UBound | CBound m -> CBound (m - off) in
                (loop ~min ~max ~memsize ~offset:(offset+off) ~size ctx memory suffix value acc)
              ) suffix.offsets acc
            in
            let acc = IntMap.fold (fun k (e,suffix) acc ->
                (* Detect double array indexing. *)
                (* TODO: also detect triple and more indexing. *)
                match IntMap.is_empty suffix.indices with
                | false -> begin
                    (* Handle the most common case for now; to handle
                       the other case we just have to join. *)
                    assert (suffix.leaf == false);
                    assert (IntMap.is_empty suffix.offsets);
                    let ksub,(esub,suffixsub) = IntMap.choose suffix.indices in
                    assert (IntMap.is_empty @@ IntMap.remove ksub suffix.indices);
                    assert (IntMap.is_empty @@ suffixsub.indices);  (* No triple indexing. *)
                    (* Cannot do it: suffix.min is relative to the offset. We forget it for now.
                       let (min,max)= Bound.inter (min,max) (suffix.min,suffix.max) in *)
                    let esub =
                      let subctx = to_sub ctx in
                      Scalar.Binary_Forward.biadd ~size:ptrsize ~nsw:true ~nuw:false ~nusw:false subctx esub @@
                      Scalar.Binary_Forward.bimul ~size:ptrsize ~nsw:true ~nuw:false subctx e @@
                      Scalar.Binary_Forward.biconst ~size:ptrsize (Z.of_int k) subctx
                    in
                    try
                      (Indices.store ~min ~max ~memsize ~offset ~size ctx esub ksub suffixsub memory value)::acc
                    with Memory_Empty -> acc
                  end
                | true ->
                try
                  (Indices.store ~min ~max ~memsize ~offset ~size ctx e k suffix memory value)::acc
                with Memory_Empty -> acc
              ) suffix.indices acc
            in
            acc
        in
        let res = loop ~min ~max ~memsize ~offset ~size ctx memory suffix value [] in
        match res with
        | [] -> raise Memory_Empty
        | [x] -> x
        | a::b -> List.fold_left (Cell.union ~memsize ctx) a b
      ;;

    end

    module MakeIndicesInPlaceEnumeration(PtrInteraction:PtrInteraction):Indices = struct
      (* Complete in-place expansion (on the original struct). *)


      (* We have the offset corresponding to [e * k + suffix] which
         must be in the interval [min,max-size], and we wish to
         compute appropriate bounds for e.

         Actually, we do not want "e * k" to go outside of the
         interval [min,max-size] (e.g. if suffix is always negative,
         we could do an offset computation that would go beyond the
         limit, before substracting suffix; but we do not want to
         allow that).

         A sufficient condition for min <= e * k is q <= e, with (q,u)
         the result of the euclidian division of (min + k - 1) by k
         (i.e. min + (k - 1) = q * k + u, with 0 <= u <= k -1). Indeed
         q <= e implies q * k + u <= e * k + u
                implies min + (k - 1) <= e * k + u
                implies min + (k - 1) -u <= e * k
                implies min <= e * k.

         A sufficient condition for e * k <= max - size is e <= q,
         with (q,r) the result of the euclidian division of (max -
         size) by k (i.e. max - size = q * k + r, with 0 <= r <= k -1). Indeed
         e <= q implies e * k <= q * k
                implies e * k <= q * k + r
                implies e * k <= (max - size). *)
      let get_indices_bound ~min ~max ~size ~k =
        assert(k > 0);
        let ediv x k =
          let q = x / k in
          let r = x mod k in
          if(r < 0) then q - 1 else q
        in
        ediv (min + k - 1) k, ediv (max - size) k
      ;;


      (* MAYBE: when double indexing: I should merge the indices together. *)

      let load ~min ~max ~memsize ~offset ~size (ctx:Value.Context.t) e k suffix mem =
        (* Codex_log.feedback "Indices.load offset %d suffix %a min %a max %a" offset
           (Operable_Value.binary_pretty ~size:32 ctx) suffix Bound.pretty min Bound.pretty max; *)
        let ptr_size = (Codex_config.ptr_size()) in
        let bin = Scalar.Query.binary ~size:ptr_size (to_sub ctx) e in
        (* Codex_log.feedback "load on ival %a" Ival.pretty ival; *)
        if Scalar.Query.binary_is_empty ~size:ptr_size bin
        then Value.binary_empty ~size:(size*8) ctx
        else
          (* Note: only works on finite regions. *)
          let inf,sup = match min,max with
            | Bound.CBound min, Bound.CBound max ->
              get_indices_bound ~min ~max ~size ~k
            | _ -> assert false
          in
          let res = Scalar.Query.binary_fold_crop ~size:ptr_size bin
              ~inf:(Z.of_int inf) ~sup:(Z.of_int sup) [] (fun i acc ->
              let i = Z.to_int i in
              let ki = k * i in
              let open Bound in
              let min = match min with UBound -> UBound | CBound m -> CBound (m - ki) in
              let max = match max with UBound -> UBound | CBound m -> CBound (m - ki) in
              let offset = offset + ki in
              let value = PtrInteraction.load ~min ~max ~memsize ~offset ~size ctx mem suffix in
              value::acc
            ) in
          match res with
          | [] ->  Value.binary_empty ~size:(size*8) ctx
          | [x] -> x
          | a::b -> List.fold_left (join_values ~size:(size * 8) ctx) a b
      ;;

      let store ~min ~max ~memsize ~offset ~size (ctx:Value.Context.t) e k suffix mem value =
        (* Codex_log.feedback "Indices.store min %a max %a" Bound.pretty min Bound.pretty max; *)
        let ptr_size = (Codex_config.ptr_size()) in
        let bin = Scalar.Query.binary ~size:ptr_size (to_sub ctx) e in
        match Scalar.Query.binary_is_singleton ~size:ptr_size bin with
        | Some i -> begin
          (* Strong update. *)
          let i = Z.to_int i in
          let ki = k * i in
          assert(0 <= i);
          assert(offset + ki + size <= memsize);
          let open Bound in
          let min = match min with UBound -> UBound | CBound m -> CBound (m - ki) in
          let max = match max with UBound -> UBound | CBound m -> CBound (m - ki) in
          PtrInteraction.store ~min ~max ~memsize ~offset:(offset + ki) ~size ctx mem suffix value
        end
        | None when Scalar.Query.binary_is_empty ~size:ptr_size bin -> raise Memory_Empty
        | None -> begin
          (* Note: only works on finite regions. *)
          let inf,sup = match min,max with
            | Bound.CBound min, Bound.CBound max ->
              get_indices_bound ~min ~max ~size ~k
            | _ -> assert false
          in
          let res = Scalar.Query.binary_fold_crop ~size:ptr_size bin
              ~inf:(Z.of_int inf) ~sup:(Z.of_int sup) mem (fun i acc ->
              let i = Z.to_int i in
              let open Bound in
              let ki = k * i in
              let min = match min with UBound -> UBound | CBound m -> CBound (m - ki) in
              let max = match max with UBound -> UBound | CBound m -> CBound (m - ki) in
              let old = PtrInteraction.load ~min ~max ~memsize ~offset:(offset + ki) ~size ctx acc suffix in
              let value = join_values ~size:(size * 8) ctx value old in
              try PtrInteraction.store ~min ~max ~memsize ~offset:(offset + ki) ~size ctx acc suffix value
              with Memory_Empty ->
                Codex_log.warning "Bounds are not tight enough: found a Memory_Empty";
                acc
                ) in
          res
        end
      ;;

    end


    module MakeIndicesSeparateEnumeration(PtrInteraction:PtrInteraction):Indices = struct

      module Y = MakeIndicesInPlaceEnumeration(PtrInteraction)

      (* Check that the indices in suffix fit within 0..k *)

      (* Note that in some cases, it will not fit. For instance, if
         you access a struct value in a buffer at an unknown
         position. *)
      let fits_within ctx suffix inf sup =
        let subctx = to_sub ctx in
        let exception False in
        (* Will we be writing outside of [inf,sup[? *)
        try
          let rec loop ~inf ~sup ~min ~max suffix =
            let open Operable_Value in
            let (min,max) = Bound.inter (min,max) (suffix.min,suffix.max) in
            match min,max with
            (* Shortcut: cannot be writing out of this.  *)
            | Bound.CBound min, Bound.CBound max when min >= inf && max < sup -> ()
            | _ -> begin
              if suffix.leaf then
                if (inf > 0) || (sup <= 0)
                then raise False
                else ()
              else ();
              suffix.offsets |> IntMap.iter (fun off suffix ->
                  do_with_offset off ~inf ~sup ~min ~max suffix);
              suffix.indices |> IntMap.iter (fun k (e,suffix) ->
                  let ptr_size = (Codex_config.ptr_size()) in
                  let bin = Scalar.Query.binary ~size:ptr_size subctx e in
                  (* todo: use signed or unsigned ? *)
                  let ival = Scalar.Query.binary_to_ival ~signed:true ~size:ptr_size bin in
                  if Ival.is_bottom ival then ()
                  else
                    let (emin,emax) = Ival.min_and_max ival in
                    let emin,emax = match emin,emax with
                      | None, _ | _, None -> raise False
                      | Some min, Some max -> (min,max) in
                    let emin = Z.to_int @@ Z.mul emin (Z.of_int k) in
                    let emax = Z.to_int @@ Z.mul emax (Z.of_int k) in
                    do_with_offset emin ~inf ~sup ~min ~max suffix;
                    do_with_offset emax ~inf ~sup ~min ~max suffix;
                );
            end
          and do_with_offset off ~inf ~sup ~min ~max suffix =
            let open Bound in
            let inf = inf - off in
            let sup = sup - off in
            let min = match min with UBound -> UBound | CBound m -> CBound (m - off) in
            let max = match max with UBound -> UBound | CBound m -> CBound (m - off) in
            (loop ~inf ~sup ~min ~max suffix)
          in
          loop ~inf ~sup ~min:suffix.Operable_Value.min ~max:suffix.Operable_Value.max suffix;
          true
        with False -> false
      ;;

      let _store ~min ~max ~memsize ~offset ~size ctx e k suffix mem value =
        (* Compute the slice size. Note that we do not use the ival to
           convert into an array; so as to have the largest arrays as
           possible. The array module can itself choose to split cells
           into partitions. *)
        (* XXX: Je pense qu'il suffit de changer ici pour avoir du dynamic squashing;
           i.e. on cree un array dont la taille correspond aux indices dans e.
        *)
        let inf,sup = match min,max with
          | Bound.CBound min, Bound.CBound max -> offset+min,offset+max
          | _ -> assert false in
        (* Hopefully the array was already extracted, and extract is a
           trivial operation. *)
        let subsize = sup - inf in
        let prev = Cell.extract ~memsize ~offset:inf ~size:subsize ctx mem in
        let arr = match prev with
          | Cell.Array arr -> arr (* Already an array. *)
          | _ -> Array.singleton ~memsize:subsize ctx prev
        in
        (* Codex_log.feedback "in store_ old %a extracted %a inf%d"
           (Cell.pretty ~memsize ctx) mem (Cell.pretty ~memsize:subsize ctx) prev inf; *)
        (* Fold on each value of e, performing the change on the cell. *)
        let strong ~memsize ~offset ~min ~max cell =
          assert(0 <= offset);
          assert(offset + size <= memsize);
          PtrInteraction.store ~memsize ~offset ~size ~min ~max ctx cell suffix value
        in
        let weak ~memsize ~offset ~min ~max cell =
          (* Codex_log.feedback "weak memsize %d offset %d cell %a"
           *    memsize offset (Cell.pretty ~memsize ctx) cell; *)
          assert(0 <= offset);
          if offset + size > memsize
          then (Codex_log.warning "out of bound in weak"; cell)
          else
          (* TODO: Put assume on old and join, that e is at the
             correct indice. *)
          let old = PtrInteraction.load ~memsize ~offset ~size ~min ~max ctx cell suffix in
          (* Codex_log.feedback "weak old %a value %a equal"
           *   (Value.binary_pretty ~size:(size * 8) ctx) old
           *   (Value.binary_pretty ~size:(size * 8) ctx) value; *)
            (* Value.Binary.equal old value *)
          let join = join_values ~size:(size * 8) ctx value old in
          PtrInteraction.store ~memsize ~offset ~size ~min ~max ctx cell suffix join
        in
        let arr = Array.subst ~memsize:subsize (to_sub ctx) e k arr ~strong ~weak in
        Cell.store_cell ~memsize ~offset ~size:subsize ctx mem (Cell.Array arr)
      ;;


      (* This will enumerate, but it will still be correct. *)
      let load ~min ~max ~memsize ~offset ~size ctx e k suffix mem =
        (* assert(fits_within ctx suffix 0 k); *)
        Y.load ~min ~max ~memsize ~offset ~size ctx e k suffix mem
      ;;
      let store ~min ~max ~memsize ~offset ~size (ctx:Value.Context.t) e k suffix mem value =
        (* assert(fits_within ctx suffix 0 k); *)
        if fits_within ctx suffix 0 k
        then
          _store ~min ~max ~memsize ~offset ~size ctx e k suffix mem value
        else
          (Codex_log.warning "Storing outside 0..k";
           Y.store ~min ~max ~memsize ~offset ~size ctx e k suffix mem value)
      ;;


    end

    module rec PtrInteraction:PtrInteraction = MakePtrInteraction(Indices)
    and Indices:Indices = MakeIndicesSeparateEnumeration(PtrInteraction)
    (* and Indices:Indices = MakeIndicesInPlaceEnumeration(PtrInteraction) *)

    let store ~size ctx {size=memsize;contents} offset value =
      let contents =
        PtrInteraction.store ~memsize ~offset:0 ~size:(size / 8)
          ~min:(Bound.CBound 0)
          ~max:(Bound.CBound memsize)
          ctx contents offset value
      in
      {size=memsize;contents}

    let _store ~size ctx {size=memsize;contents} offset value =
      let res = store ~size ctx {size=memsize;contents} offset value in
      (* Codex_log.feedback "main.store @\nold:%a@\nnew:%a@\nvalue:%a" (Cell.pretty ~memsize ctx) contents (Cell.pretty ~memsize ctx) res.contents (Value.binary_pretty ~size ctx) value; *)
      res



    let load ~size ctx {size=memsize;contents} offset =
      PtrInteraction.load
        ~min:(Bound.CBound 0)
        ~max:(Bound.CBound memsize)
        ~memsize ~offset:0 ~size:(size / 8) ctx contents offset

    let typed_load ~size ctx mem x typ = assert false ;;
    let typed_store ~size ctx mem at typ value = assert false ;;

    let pretty ctx fmt {size=memsize;contents} =
      Cell.pretty ~memsize ctx fmt contents

  end

end


module MakeFullyExpanded(Scalar:Domain_sig.Base) = struct
  module Scalar = Scalar
  module M = MakePrev(Scalar)
  module Offset = M.Operable_Value
  module Lift = (struct let ctx x = x,fun x -> x end)
  module Memory(Value:Memory_sig.Value) =
    Fully_expanded_finite_region.Memory(Offset)(Value)
end

module Make(Scalar:Domain_sig.Base) = struct
  module Scalar = Scalar
  module M = MakePrev(Scalar)
  module Offset = M.Operable_Value
  module Memory = M.Memory

end







(* De ce que je comprends du code:

   - Cell: top-level
   - Struct
   - Array

   Interagit plustot sur le code, tandis que les

   PtrInteraction et Indices

   sont plutot sur la decomposition du pointeur.

 *)

(* TODO:

   - Replace load by extract? Semble impossible: load "va au bout pour
   recuperer une valeur", et pas extract.

     On pourrait remplacer possiblement les deux par un "fold on the
   values it contains", sauf que dans le cas d'extract on essaie de
   pas rentrer dans les valeurs, alors qu'on rentre pour load. Donc,
   besoin des deux pour le moment au moins.

   - Rename join into union everywhere

   - Implement a fully-squashing array solution.  - Faire en sorte de
   coller avec les "array theory" des SMT, si possible.

   - On a deja store-cell; je crois que j'avais deja essaye
   d'optimiser des memcpy avec; on devrait essayer

   - On pourrait mettre l'operavable value et la memoire dans des
   fichies separes, c'est quand meme bien different.

 *)



(* Quelques idees:


   - Pour la serialisation, je ferai l'union des ensembles.

   - Je pourrais avoir un constructeur pour faire ca. Par facilité il
   peut avoir le même type que binary

   - Ou alors, mes cells pourraient toujours contenir un type
   ensemble. De toute façon, je ne fais rien des cell à part les
   loader (qui ferait un choose) ou les storer (qui peut convertir en
   un ensemble singleton).

   - Est-ce qu'on peut généraliser? Plutôt qu'un ensemble, on peut
   utiliser des tableaux, indexés par exemple par un entier. On a 1 si
   c'est la première valeur de l'ensemble, 2 si c'est la deuxième,
   etc.


 *)


(* Summary for the notion of set/decision tree.


   - To implement:
      - Having sets of binary will be boring, especially i would have to change every Operable_Value
        to include set operations?

      - I could just add "union" and "choose" operations for binary and reuse the standard binary
      - Although, binary would no longer be just a binary, but a function from a set of boolean variables        to binary. binary would be a special case when the set is empty.
        But I don't want operations such as "1+if(b1) A else B", because we don't know what choose
        could mean then. I can check that, but as a "singleton" and a binary would be the same,
        I cannot check that choose is always applied on a set. Thus, it would be quite OK.

      - Par contre, la traduction vers SMT sera peut-etre pas si facile?
        AU besoin, je peux rajouter un flag
        "singleton", qui dit si on est un singleton ou non. Mais je peux aussi calculer ca
        pendant la conversion vers le SMT.


   - In different mu iterations, we may make different choices. Thus, the choice_id depends
     on the mu level (it it equivalent to a call to unknown())

 *)
