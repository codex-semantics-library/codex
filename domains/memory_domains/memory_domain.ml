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

(* A memory domain using the modules corresponding to Memory_sig. The
   main requirement for this functor is that the values in memory
   correspond to the addresses, as it is applied when all the
   addresses domains and memory functors have been constructed. *)


(* MAYBE: put with_bottom here. This would handle the cases where we
   return memory_empty which is not catched first by an alarm. *)
module Make
  (Value: Memory_sig.FIXED_SIZE_VALUE_DOMAIN)
  (Block: Memory_sig.BLOCK
   with module Scalar = Value.Scalar
     and module Value = Value
  )
  (Memory:Memory_sig.MEMORY
   with module Scalar = Value.Scalar
    and module Address := Value
    and module Block := Block
  )
  :sig
    include Memory_sig.Base
      with type binary = Value.binary
      and type boolean = Value.boolean
      and type enum = Value.enum
      and type block = Block.block
      and type offset = Block.offset
      and module Context = Memory.Scalar.Context
  end
= struct
  include Value.Scalar
  module Operable_Value = Value
  include Operable_Value


  let name () = "Memory_domain(" ^ Scalar.name() ^ ")";;
  let unique_id () = Sig.Fresh_id.fresh (name ());;

  type size = int;;
  type boolean_mapping = int
  type integer_mapping = int

  module Types = struct
    type boolean = Operable_Value.boolean
    type binary = Operable_Value.binary
    type enum = Operable_Value.enum
    type memory = { mem: Memory.memory } [@@unboxed]
    type block = Block.block
    type offset = Block.Offset.offset
  end
  include Types

  (**************** Context ****************)
  module Context = Scalar.Context

  (************** Global symbols **************)

  (* let global_symbol ctx symb = Operable_Value.global_symbol ctx symb
  let add_global_symbol ~size ctx symb value = Operable_Value.add_global_symbol ~size ctx symb value *)

  (**************** Pretty printing ****************)

  let memory_pretty ctx fmt mem = (Memory.pretty ctx) fmt mem.mem
  let boolean_pretty _ctx = Boolean.pretty
  let block_pretty = Block.pretty
  let offset_pretty = Block.Offset.offset_pretty

  (**************** Tuple fonctions ****************)

  let typed_serialize_mem: widens:bool -> Context.t -> memory -> Context.t -> memory -> 'a Context.in_acc -> (memory,'a) Context.result =
    fun ~widens ctxa a ctxb b (included, acc) ->
      let Context.Result(included,acc,d_mem) = Memory.serialize ~widens ctxa a.mem ctxb b.mem
        (included, acc) in
      Context.Result(included,acc,fun ctx x ->
          let mem,x = d_mem ctx x in
          {mem},x
        )
  ;;

  let memory_empty ctx = {
      mem = Memory.memory_empty ctx;
    }
  ;;

  let block_empty ctx = assert false

  (**************** Fixpoint computation ****************)
  let serialize_memory = typed_serialize_mem;;
  let serialize_binary = Operable_Value.serialize;;
  let serialize_block = Block.serialize;;

  (**************** Operation on values. ****************)
  let binary_pretty = Operable_Value.binary_pretty
  let binary_unknown_typed = Operable_Value.binary_unknown_typed

  (******************* Operations on memory blocks *****************)

  let block_unknown _ = assert false

  (**************** Transfer functions. ****************)

  module Block_Forward = struct
    let sizeof ctx a = Block.sizeof ctx a
    let concat ctx a b = Block.concat ctx a b
    let load ~size ctx a = Block.load ~size ctx a
    let store ~size ctx a b = Block.store ~size ctx a b
    let binary_to_block ~size ctx a = Block.binary_to_block ~size ctx a
  end

  module Memory_Forward = struct

    let load ~size ctx mem at =
      let res = Memory.load ~size ctx mem.mem at in
      res, mem

    (* TODO: Implement memcpy. *)
    let memcpy ~size ctx mem from to_ = assert false

    let store ~size ctx mem (at:binary) (value:binary) =
      (* Codex_log.feedback "################storing %a" (Operable_Value.binary_pretty ~size ctx) at; *)
      try
        { mem = Memory.store ~size ctx mem.mem at value }
      with Sig.Memory_Empty ->
        Codex_log.feedback "Memory_domain.after store: memory empty";
        raise Sig.Memory_Empty

    let _store ~size ctx mem (at:binary) (value:binary) =
      let res = store ~size ctx mem at value in
        Codex_log.feedback "AFter store %a <- %a on@ %a:@\nResult= %a@."
          (binary_pretty ~size ctx) at (binary_pretty ~size ctx) value
          (memory_pretty ctx) mem (memory_pretty ctx) res;
        res
    ;;

    let load_block ctx mem at =
      let res = Memory.load_block ctx mem.mem at in
      res, mem

    let store_block ctx mem at region =
      try
        { mem = Memory.store_block ctx mem.mem at region }
      with Sig.Memory_Empty ->
        Codex_log.feedback "Memory_domain.after store block: memory empty";
        raise Sig.Memory_Empty

    let malloc ~id ~(malloc_size:Units.In_bytes.t) ctx mem =
      let ptr,mem' = Memory.malloc ~id ~malloc_size ctx mem.mem in
      ptr, {mem=mem'}
    ;;

    let free ctx mem ptr =
      { mem = Memory.free ctx mem.mem ptr
      }
    ;;

    let unknown ~level ctx =
      (* assert(level == ctx.level); *)
      { mem = Memory.unknown ~level ctx }
  end

  module Binary_Forward = Operable_Value.Binary_Forward

  (**************** Queries ****************)

  module Query = struct
    include Operable_Value.Query
    let reachable ctx mem = Lattices.Quadrivalent.Top
  end

  let memory_is_empty _ = assert false
  let integer_is_empty _ = assert false
  let boolean_is_empty _ = assert false

  (**************** Builtins. ****************)

  let binary_empty ~size ctx = Operable_Value.binary_empty ~size ctx

  let reachable ctx mem = Operable_Value.satisfiable ctx (Operable_Value.Boolean_Forward.true_ ctx)
  let satisfiable ctx b = Operable_Value.satisfiable ctx b

  let union _ = assert false


  let should_focus : size:_ -> Context.t -> memory -> binary -> (binary * offset) option =
    fun ~size ctx memory address -> Memory.should_focus ~size ctx memory.mem address

  let may_alias : ptr_size:_ -> Context.t -> size1:offset -> size2:offset -> binary -> binary -> bool =
    fun ~ptr_size ctx ~size1 ~size2 addr1 addr2 ->
    Memory.may_alias ~ptr_size ctx ~size1 ~size2 addr1 addr2

  let is_weak : size:_ -> Context.t -> binary -> bool = fun ~size ctx address ->
    Memory.is_weak ~size ctx address

  let addresses_in_binary : size:_ -> Context.t -> binary -> Context.t -> binary -> (binary * binary) list =
    fun ~size ctxa a ctxb b -> Operable_Value.addresses_in_binary ~size ctxa a ctxb b

  let addresses_in_block : Context.t -> block -> Context.t -> block -> (binary * binary) list =
    fun ctxa a ctxb b -> Block.addresses_in_block ctxa a ctxb b

  let addresses_in_memory : Context.t -> memory -> Context.t -> memory -> (binary * binary) list =
    fun ctxa mema ctxb memb -> Memory.addresses_in_memory ctxa mema.mem ctxb memb.mem

  (** Function relative to offsets (to be removed) **)

  let offset_pretty = Block.Offset.offset_pretty
  let offset_zero = Block.Offset.offset_zero
  let offset_shift = Block.Offset.offset_shift

  let offset_le ctx a b =
    let res = Block.Offset.offset_le ctx a b in
    match Scalar.query_boolean ctx @@ Block.Offset.boolean2scalar_bool ctx res with
    | Lattices.Quadrivalent.True -> Boolean_Forward.true_ ctx
    | Lattices.Quadrivalent.False -> Boolean_Forward.false_ ctx
    | Lattices.Quadrivalent.Top -> boolean_unknown ctx
    | Lattices.Quadrivalent.Bottom -> boolean_empty ctx

  let offset_eq ctx a b =
    let res = Block.Offset.offset_eq ctx a b in
    match Scalar.query_boolean ctx @@ Block.Offset.boolean2scalar_bool ctx res with
    | Lattices.Quadrivalent.True -> Boolean_Forward.true_ ctx
    | Lattices.Quadrivalent.False -> Boolean_Forward.false_ ctx
    | Lattices.Quadrivalent.Top -> boolean_unknown ctx
    | Lattices.Quadrivalent.Bottom -> boolean_empty ctx

end
