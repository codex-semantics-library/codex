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

module In_bits = Units.In_bits
module In_bytes = Units.In_bytes

(* Depracted name. *)
module type Enumerable_offset = sig
  include Memory_sig.OFFSET
  val fold_crop:
    size:In_bits.t -> Scalar.Context.t ->
    offset -> inf:Z.t -> sup:Z.t -> (Z.t -> 'a -> 'a) -> 'a -> 'a
  (* MAYBE: could be subsumed by fold_crop. *)
  val is_precise:
    size:In_bits.t -> Scalar.Context.t -> offset -> Z.t Memory_sig.precision
  val in_bounds:
    size:In_bits.t -> Scalar.Context.t -> offset -> inf:Z.t -> sup:Z.t -> bool
end

module Make_Block
    (Offset:Enumerable_offset)
    (Value: Memory_sig.VALUE with module Scalar = Offset.Scalar)
= struct
  module Scalar = Offset.Scalar
  module Operable_Value = Offset
  module Offset = Offset

  module M:Memory_sig.WITH_BOOLEAN_REDEFINITION
    with module Context = Scalar.Context
     and type boolean = Value.boolean =
    Value
  include M

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
      let idx = In_bytes.of_int idx and size = In_bytes.of_int size and oldsize = In_bytes.of_int oldsize in
      if idx == In_bytes.zero  && size == oldsize (* Fast path *)
      then v
      else Value.Binary_Forward.bextract ctx v
          ~index:(In_bytes.in_bits idx) ~size:(In_bytes.in_bits size) ~oldsize:(In_bytes.in_bits oldsize)

    let fold_on_diff ctxa a ctxb b acc f =
      I.fold_on_diff a b acc ~extracta:(extract ctxa) ~extractb:(extract ctxb) f

    let pretty ~size ctx fmt map =
      I.iter_between ~size 0 map ~extract:(extract ctx) (fun ~size offset v ->
          Format.fprintf fmt " (%d-%d)" offset (offset+size-1);
          let size = In_bytes.(of_int size |> in_bits) in
          Value.binary_pretty ctx ~size fmt v) ;;

    let get_size = I.get_size

  end

  (* The arguments ~size passed to this module are also in bytes. *)
  module Memory = struct
    type t = {map : IntervalMap.t; } [@@unboxed];;

    let pretty ctx fmt {map} =
      IntervalMap.pretty ctx fmt ~size:(IntervalMap.get_size map) map

    let initial ctx (size:In_bytes.t) =
      assert In_bytes.(size >= zero);
      let bitsize = In_bytes.(size |> in_bits) in
      {map = IntervalMap.create ~size:(size:>int) @@ (Value.binary_empty ctx ~size:bitsize)}
    ;;

    let get_size {map} = IntervalMap.get_size map |> In_bytes.of_int

    let load ~size ctx {map} key  =
      let key = Z.to_int key in
      assert In_bytes.(zero < size);
      assert(key >= 0);
      let region_size = IntervalMap.get_size map in
      (* Codex_log.feedback "loading offset %d size %d region_size %d %a" key size region_size (pretty ctx) {map}; *)
      assert(key + (size:>int) <= region_size);
      (* Kernel.feedback "load idx %d size %d" key size; *)
      let l = IntervalMap.I.fold_between ~size:(size:>int) key map []
          ~extract:(IntervalMap.extract ctx)
          (fun ~size key value acc -> (size,value)::acc)
      in match l with
      | [] -> assert false    (* The address is correct, we must load something. *)
      | [_,v] -> v
      | (sizea,a)::rest ->
        (* TODO: this depends on endiannees *)
        let f (size,acc) (newsize,newv) =
          let newsize = In_bytes.(of_int newsize |> in_bits) in
          let acc = Value.Binary_Forward.bconcat ctx ~size1:size acc ~size2:newsize newv in
          (In_bits.(size+newsize),acc)
        in
        let fsize,v = List.fold_left f (In_bytes.(of_int sizea |> in_bits),a) rest in
        assert (fsize == In_bytes.in_bits size);
        v
    ;;

    let store ~(size:In_bytes.t) ctx region offset value =
      (* Codex_log.feedback "Store offset %d size %d" (Z.to_int offset) size; *)
      {map=IntervalMap.store ~size:(size:>int)  (Z.to_int offset) region.map value}

    let serialize ~widens ctxa a ctxb b (included, acc) =
      let open Scalar in
      let Context.Result(included,acc,d) =
        IntervalMap.fold_on_diff ctxa a.map ctxb b.map (Scalar.Context.Result(included,acc,fun ctx tup -> a.map,tup))
          (fun ~size offset a b  (Context.Result(included,acc,d_map)) ->
             (* Codex_log.feedback "### serialize at offset@ %x:@ a =@ %a b =@ %a" offset (Value.binary_pretty ~size ctx) a (Value.binary_pretty ~size ctx) b; *)
             let Context.Result(included,acc,d_value) = Value.serialize ~widens ~size:(In_bytes.(of_int size |> in_bits))
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
    Operable_Value.in_bounds ~size ctx offset
      ~inf:(Z.of_int @@ In_bytes.to_int inf)
      ~sup:(Z.of_int @@ In_bytes.to_int sup)
  ;;

  let fold_on_values ~size ctx (offset:Operable_Value.offset) ~sup acc f =
    Operable_Value.fold_crop ~size ctx offset ~inf:Z.zero ~sup:(Z.of_int @@ In_bytes.to_int sup) f acc
  ;;

  let join_values ~size ctx v1 v2 =
    let open Scalar in
    let Context.Result(_,tup,deserialize) = Value.serialize ~widens:false ~size ctx v1 ctx v2
        (true, Context.empty_tuple ()) in
    let res_tup = nondet_same_context ctx tup in
    let res,(_: Context.empty_tuple Context.out_tuple) = deserialize ctx res_tup in
    res
  ;;



  let load ~(size:In_bits.t) ctx region offset =
    assert((size:>int) land 7 == 0);
    let region_size = Memory.get_size region in
    let c = not @@ in_bounds ~size ctx offset ~inf:In_bytes.zero ~sup:region_size in
    if c then (
      (* Codex_log.alarm "region_offset_access" ; *)
      Emit_alarm.emit_alarm Operator.Alarm.Load_param_nonptr ;
      Codex_log.error "@[<hov 2>Out-of-bounds access at offset %a in region of size %d.@]"
          (Offset.offset_pretty ctx) offset
          (region_size:>int);
    );
    let loaded = fold_on_values ~size ctx offset
        ~sup:In_bytes.(region_size - (In_bits.in_bytes size)) []
        (fun offset acc ->
           let v = Memory.load ~size:In_bits.(in_bytes size) ctx region offset in
           v::acc)
    in
    match loaded with
    | [] -> Value.binary_empty ~size ctx
    | [x] -> x                (* Most common case *)
    | head::rest -> List.fold_left (join_values ~size ctx) head rest
  ;;

  let get_size = Memory.get_size

  (* We have two version of stores. This one keeps a single memory,
     detects whether we make a weak or strong update, and join
     values. The other join memories, and is thus much slower. *)
  let store ~(size:In_bits.t) (ctx:Value.Context.t) region offset (value:Value.binary) =
    assert((size:>int) land 7 == 0);
    let region_size = Memory.get_size region in
    let size_in_bytes = In_bits.in_bytes size in
    let sup = In_bytes.(region_size - size_in_bytes) in
    match is_precise ~size ctx offset with
    | Empty -> raise Sig.Memory_Empty
    | Singleton(offset)
      when Z.lt offset Z.zero
        || Z.gt offset (Z.of_int @@ In_bytes.to_int sup)
      -> raise Sig.Memory_Empty
    | Singleton(offset) -> Memory.store ~size:size_in_bytes ctx region offset value
    | Imprecise ->
      (* As we fold on the same memory, this works even when the
         potential writes overlap on one another. *)
      let region = fold_on_values ~size ctx offset
          ~sup region (fun offset region ->
              let v = Memory.load ~size:size_in_bytes ctx region offset in
              let v' = join_values ~size ctx v value in
              let region = Memory.store ~size:size_in_bytes ctx region offset v' in
              region
            ) in
      region
  ;;

  let join_memories ctx v1 v2 =
    let open Scalar in    
    let Context.Result(included,tup,deserialize) =
      Memory.serialize ~widens:false ctx v1 ctx v2 (true, Context.empty_tuple ()) in
    let res_tup = nondet_same_context ctx tup in
    let res,(_:Context.empty_tuple Context.out_tuple) = deserialize ctx res_tup in
    res
  ;;

  let _store ~size ctx region offset (value:Value.binary) =
    let size_in_bytes = In_bits.in_bytes size in
    assert((size:>int) land 7 == 0);
    let region_size = Memory.get_size region in
    let sup = In_bytes.(region_size - size_in_bytes) in
    (* As we fold on the same memory, this works even when the
         potential writes overlap on one another. *)
    let regions = fold_on_values ~size ctx offset
        ~sup [] (fun offset acc ->
            (* let v = Memory.load ~size:(size/8) ctx region offset in
             * let v' = join_values ~size ctx v value in *)
            let region' = Memory.store ~size:size_in_bytes ctx region offset value in
            region'::acc
          ) in
    match regions with
    | [] -> raise Sig.Memory_Empty
    | [x] -> x
    | a::b -> List.fold_left (join_memories ctx) a b

  let serialize = Memory.serialize
  let initial = Memory.initial
  let pretty = Memory.pretty
  type block = Memory.t


  let may_alias ~ptr_size:_ _ = assert false
  let should_focus ~size:_ _ = assert false
  let is_weak ~size _ctx value = assert false

  let block_empty ctx = Memory.{map = IntervalMap.create ~size:0 @@ (Value.binary_empty ctx ~size:In_bits.zero)}

  let free _ = assert false

  let malloc ~id:_ ~malloc_size:_ _ = assert false

  (* Temporary *)
  let unknown ~level:_ ctx =
    match Codex_config.valid_absolute_addresses() with
    | None -> assert false
    | Some (_min,max) -> initial ctx @@ In_bytes.of_int @@ Z.to_int max

  let typed_load ~size ctx mem x typ = assert false ;;
  let typed_store ~size ctx mem at typ value = assert false ;;

  let sizeof _ = assert false
  let concat _ = assert false
  let binary_to_block ~size _ = assert false

  let block_unknown_typed _ = assert false

  let check_type _ = assert false

  let addresses_in_block ctxa a ctxb a = [] (* assert false *)

end
