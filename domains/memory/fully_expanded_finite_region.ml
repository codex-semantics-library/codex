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

open Memory_sig;;

(* Depracted name. *)
module type Enumerable_offset = sig
  include Offset
  val fold_crop:
    size:int -> Context.t ->
    offset -> inf:Z.t -> sup:Z.t -> (Z.t -> 'a -> 'a) -> 'a -> 'a
  (* MAYBE: could be subsumed by fold_crop. *)
  val is_precise:
    size:int -> Context.t -> offset -> Z.t Memory_sig.precision             
  val in_bounds:
    size:int -> Context.t -> offset -> inf:Z.t -> sup:Z.t -> bool
end

module Memory
    (Offset:Enumerable_offset)
    (Value:Value (* with module Context = Offset.Context *))
    (Lift:sig
       val ctx: Value.Context.t -> Offset.Context.t * (Offset.Context.t -> Value.Context.t)
     end)
= struct
  module Operable_Value = Offset
  module Offset = Offset

  module Domain:Domain_sig.Minimal
    with module Context = Value.Context
     and type boolean = Value.boolean
    = Value
  include Domain      

  
  module Value = Value
  type offset = Operable_Value.offset
  type address = offset

  (* Note: inside this module, the sizes are in bytes. *)
  module IntervalMap = struct

    module I = Interval_map.With_Extract(struct
        type t = Value.binary
      end)

    type t = I.t
    let create = I.create
    let store = I.store

    let extract ctx v ~idx ~size ~oldsize =
      if idx == 0 && size == oldsize (* Fast path *)
      then v
      else Value.Binary_Forward.bextract ctx v ~index:(idx * 8) ~size:(size * 8) ~oldsize:(oldsize * 8)
    ;;


    let fold_on_diff ctxa a ctxb b acc f =
      I.fold_on_diff a b acc ~extracta:(extract ctxa) ~extractb:(extract ctxb) f

    let pretty ~size ctx fmt map =
      I.iter_between ~size 0 map ~extract:(extract ctx) (fun ~size offset v ->
          Format.fprintf fmt " (%d-%d)" offset (offset+size-1);
          Value.binary_pretty ctx ~size fmt v) ;;

    let get_size = I.get_size

  end

  (* The arguments ~size passed to this module are also in bytes. *)
  module Memory = struct
    type t = {map : IntervalMap.t; } [@@unboxed];;

    let pretty ctx fmt {map} =
      IntervalMap.pretty ctx fmt ~size:(IntervalMap.get_size map) map

    let initial ctx size =
      assert(size >= 0);
      {map = IntervalMap.create ~size @@ (Value.binary_empty ctx ~size)}
    ;;

    let get_size {map} = IntervalMap.get_size map

    let load ~size ctx {map} key  =
      let key = Z.to_int key in
      assert(size > 0);
      assert(key >= 0);
      let region_size = IntervalMap.get_size map in
      (* Codex_log.feedback "loading offset %d size %d region_size %d %a" key size region_size (pretty ctx) {map}; *)                
      assert(key + size <= region_size);
      (* Kernel.feedback "load idx %d size %d" key size; *)
      let l = IntervalMap.I.fold_between ~size key map []
          ~extract:(IntervalMap.extract ctx)
          (fun ~size key value acc -> (size,value)::acc)
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

    let store ~size ctx region offset value =
      (* Codex_log.feedback "Store offset %d size %d" (Z.to_int offset) size; *)
      {map=IntervalMap.store ~size  (Z.to_int offset) region.map value}
    ;;

    let serialize ctxa a ctxb b (included, acc) =
      let Context.Result(included,acc,d) =
        IntervalMap.fold_on_diff ctxa a.map ctxb b.map (Context.Result(included,acc,fun ctx tup -> a.map,tup))
          (fun ~size offset a b  (Context.Result(included,acc,d_map)) ->
             (* Codex_log.feedback "### serialize at offset@ %x:@ a =@ %a b =@ %a" offset (Value.binary_pretty ~size ctx) a (Value.binary_pretty ~size ctx) b; *)
             let Context.Result(included,acc,d_value) = Value.serialize ~size:(size * 8)
                 ctxa a ctxb b (included,acc) in
             Context.Result(included,acc, fun ctx tup ->
                 let value,tup = d_value ctx tup in                  
                 let map,tup = d_map ctx tup in
                 IntervalMap.store offset map ~size value,tup)
          ) in
      Context.Result(included,acc,fun ctx tup -> let map,tup = d ctx tup in {map},tup)
    ;;

  end

  (* Here, the ~size arguments are in bits. *)

  let is_precise = Operable_Value.is_precise;;   
  
  let in_bounds ~size ctx offset ~inf ~sup =
    Operable_Value.in_bounds ~size ctx offset ~inf:(Z.of_int inf) ~sup:(Z.of_int sup)
  ;;   

  let fold_on_values ~size ctx (offset:Operable_Value.offset) ~sup acc f =
    Operable_Value.fold_crop ~size ctx offset ~inf:Z.zero ~sup:(Z.of_int sup) f acc
  ;;

  let join_values ~size ctx v1 v2 =
    let Context.Result(_,tup,deserialize) = Value.serialize ~size ctx v1 ctx v2
        (true, Context.empty_tuple ()) in
    let res_tup = Value.nondet_same_context ctx tup in
    let res,(_: Context.empty_tuple Context.out_tuple) = deserialize ctx res_tup in
    res
  ;;


  (* As we have an imperative signature, we generally don't need to recompose contexts. *)
  let to_sub:Value.Context.t -> Offset.Context.t = fun ctx -> fst @@ Lift.ctx ctx
  
  let load ~size ctx region offset =
    let sub_ctx = to_sub ctx in
    assert(size land 7 == 0);
    let region_size = Memory.get_size region in
    let c = not @@ in_bounds ~size sub_ctx offset ~inf:0 ~sup:region_size in
    if c then ( 
      (* Codex_log.alarm "region_offset_access" ; *)
      Codex_log.alarm "load_param_nonptr" ;
      Codex_log.error "@[<hov 2>Out-of-bounds access at offset %a in region of size %d.@]"
          (Offset.offset_pretty ~size:(8 * size) sub_ctx) offset
          region_size ;
    ) ;
    let loaded = fold_on_values ~size sub_ctx offset ~sup:(region_size - size/8) [] (fun offset acc ->
        let v = Memory.load ~size:(size/8) ctx region offset in
        v::acc
      ) in
    match loaded with
    | [] -> Value.binary_empty ~size ctx
    | [x] -> x                (* Most common case *)
    | head::rest -> List.fold_left (join_values ~size ctx) head rest
  ;;

  let get_size = Memory.get_size

  (* We have two version of stores. This one keeps a single memory,
     detects whether we make a weak or strong update, and join
     values. The other join memories, and is thus much slower. *)
  let store ~size (ctx:Value.Context.t) region offset (value:Value.binary) =
    let sub_ctx = to_sub ctx in
    assert(size land 7 == 0);
    match is_precise ~size sub_ctx offset with
    | Empty -> raise Memory_Empty
    | Singleton(offset)
      when Z.lt offset Z.zero
        || Z.gt offset (Z.of_int (Memory.get_size region - size/8))               
      -> raise Memory_Empty                   
    | Singleton(offset) -> Memory.store ~size:(size/8) ctx region offset value
    | Imprecise ->
      let region_size = Memory.get_size region in
      (* As we fold on the same memory, this works even when the
         potential writes overlap on one another. *)
      let region = fold_on_values ~size sub_ctx offset
          ~sup:(region_size - size/8) region (fun offset region ->
              let v = Memory.load ~size:(size/8) ctx region offset in
              let v' = join_values ~size ctx v value in
              let region = Memory.store ~size:(size/8) ctx region offset v' in
              region
            ) in
      region
  ;;

  let join_memories ctx v1 v2 =
    let Context.Result(included,tup,deserialize) =
      Memory.serialize ctx v1 ctx v2 (true, Context.empty_tuple ()) in
    let res_tup = Value.nondet_same_context ctx tup in
    let res,(_:Context.empty_tuple Context.out_tuple) = deserialize ctx res_tup in
    res
  ;;

  let _store ~size ctx region offset (value:Value.binary) =
    let sub_ctx = to_sub ctx in
    assert(size land 7 == 0);
    let region_size = Memory.get_size region in
    (* As we fold on the same memory, this works even when the
         potential writes overlap on one another. *)
    let regions = fold_on_values ~size sub_ctx offset
        ~sup:(region_size - size/8) [] (fun offset acc ->
            (* let v = Memory.load ~size:(size/8) ctx region offset in
             * let v' = join_values ~size ctx v value in *)
            let region' = Memory.store ~size:(size/8) ctx region offset value in
            region'::acc
          ) in
    match regions with
    | [] -> raise Memory_Empty
    | [x] -> x
    | a::b -> List.fold_left (join_memories ctx) a b
  ;;

  let serialize = Memory.serialize
  let initial = Memory.initial
  let pretty = Memory.pretty
  type block = Memory.t


  let may_alias ~ptr_size:_ _ = assert false
  let should_focus ~size:_ _ = assert false
  let is_weak ~size _ctx value = assert false

  let block_empty ctx = Memory.{map = IntervalMap.create ~size:0 @@ (Value.binary_empty ctx ~size:0)}

  let free _ = assert false

  let malloc ~id:_ ~malloc_size:_ _ = assert false

  (* Temporary *)
  let unknown ~level:_ ctx =
    match Codex_config.valid_absolute_addresses() with
    | None -> assert false
    | Some (_min,max) -> initial ctx @@ Z.to_int max
  ;;

  let typed_load ~size ctx mem x typ = assert false ;;
  let typed_store ~size ctx mem at typ value = assert false ;;

  let sizeof _ = assert false
  let concat ~size1 ~size2 _ = assert false
  let binary_to_block ~size _ = assert false

  let shared_addresses ctxa a ctxb a = [] (* assert false *)

end
