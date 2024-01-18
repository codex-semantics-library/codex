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

(* A memory domain using the modules corresponding to Memory_sig. The
   main requirement for this functor is that the values in memory
   correspond to the addresses, as it is applied when all the
   addresses domains and memory functors have been constructed. *)


(* MAYBE: put with_bottom here. This would handle the cases where we
   return memory_empty which is not catched first by an alarm. *)
module Make
    (Common:Memory_sig.Fixed_size_value_domain)
    (Whole_Memory:Memory_sig.Memory
     with module Address.Context = Common.Context
      and module Value.Context = Common.Context
      and type Value.binary = Common.binary
      and type Address.binary = Common.binary
      and type boolean = Common.boolean
    )
  :sig
    include Domain_sig.Base
      with type binary = Whole_Memory.Value.binary
      and type boolean = Whole_Memory.boolean
      and module Context = Whole_Memory.Context
  end
= struct
  module Operable_Value = Common
  include Operable_Value


  let name = "Memory_domain(" ^ Scalar.name ^ ")";;
  let unique_id = Domain_sig.Fresh_id.fresh name;;

  type size = int;;  
  type boolean_mapping = int
  type integer_mapping = int
  
  module Types = struct
    type boolean = Whole_Memory.boolean
    type binary = Operable_Value.binary
    type memory = { mem: Whole_Memory.memory } [@@unboxed]
  end
  include Types

  (**************** Context ****************)
  module Context = Whole_Memory.Context

  (************** Global symbols **************)

  (* let global_symbol ctx symb = Operable_Value.global_symbol ctx symb
  let add_global_symbol ~size ctx symb value = Operable_Value.add_global_symbol ~size ctx symb value *)

  (**************** Pretty printing ****************)

  let memory_pretty ctx fmt mem = (Whole_Memory.pretty ctx) fmt mem.mem
  let boolean_pretty _ctx = Boolean.pretty

  (**************** Tuple fonctions ****************)

  let typed_serialize_mem: Context.t -> memory -> Context.t -> memory -> 'a Context.in_acc -> (memory,'a) Context.result =
    fun ctxa a ctxb b (included, acc) ->
      let Context.Result(included,acc,d_mem) = Whole_Memory.serialize ctxa a.mem ctxb b.mem
        (included, acc) in
      Context.Result(included,acc,fun ctx x ->
          let mem,x = d_mem ctx x in          
          {mem},x
        )
  ;;
                                
  let memory_empty ctx = {
      mem = Whole_Memory.memory_empty ctx;
    }
  ;;

  (**************** Fixpoint computation ****************)
  let serialize_memory = typed_serialize_mem;;
  let serialize_binary = Operable_Value.serialize

  (**************** Operation on values. ****************)
  let binary_pretty = Operable_Value.binary_pretty
  let binary_unknown_typed = Operable_Value.binary_unknown_typed
  
  (**************** Transfer functions. ****************)

  module Memory_Forward = struct

    let load ~size ctx mem at =
      let res = Whole_Memory.load ~size ctx mem.mem at in
      res, mem
    
    (* TODO: Implement memcpy. *)
    let memcpy ~size ctx mem from to_ = assert false
    
    let store ~(size:int) ctx mem (at:binary) (value:binary) =
      (* Codex_log.feedback "################storing %a" (Operable_Value.binary_pretty ~size ctx) at; *)
      try
        { mem = Whole_Memory.store ~size ctx mem.mem at value }
      with Memory_sig.Memory_Empty ->
        Codex_log.feedback "Memory_domain.after store: memory empty";
        raise Memory_sig.Memory_Empty
    ;;

    let _store ~(size:int) ctx mem (at:binary) (value:binary) =
      let res = store ~size ctx mem at value in
        Codex_log.feedback "AFter store %a <- %a on@ %a:@\nResult= %a@."
          (binary_pretty ~size ctx) at (binary_pretty ~size ctx) value
          (memory_pretty ctx) mem (memory_pretty ctx) res;
        res
    ;;
          
    
    
    let malloc ~id ~malloc_size ctx mem =
      let ptr,mem' = Whole_Memory.malloc ~id ~malloc_size ctx mem.mem in
      ptr, {mem=mem'}
    ;;

    let free ctx mem ptr =
      { mem = Whole_Memory.free ctx mem.mem ptr
      }
    ;;

    let unknown ~level ctx =
      (* assert(level == ctx.level); *)
      { mem = Whole_Memory.unknown ~level ctx }
  end

  module Binary_Forward = Operable_Value.Binary_Forward
  
  (**************** Queries ****************)

  module Query = struct
    include Operable_Value.Query
    let reachable ctx mem = Lattices.Quadrivalent.Top
  end

  let memory_is_empty _ = assert false
  let binary_is_empty ~size ctx a = Query.(binary_is_empty ~size (binary ~size ctx a))
  let integer_is_empty _ = assert false
  let boolean_is_empty _ = assert false

  (**************** Builtins. ****************)

  include Transfer_functions.Builtin.Make(Types)(Context)

  let builtin_show_each_in s ctx args mem =
    let pp fmt =
      Format.fprintf fmt "Finite_memory: show each%s:@[<hv>" s;
      args |> List.iter (fun (size,x) ->
          Format.fprintf fmt "%a@\n" (binary_pretty ~size ctx) x
        );
      Format.fprintf fmt "@]"
    in
    Codex_log.result "%t" pp;
    mem
  ;;

  let binary_empty ~size ctx = Operable_Value.binary_empty ~size ctx

  let reachable ctx mem = Operable_Value.satisfiable ctx (Operable_Value.Boolean_Forward.true_ ctx)
  let satisfiable ctx b = Operable_Value.satisfiable ctx b

  let union _ = assert false
                     

  let should_focus : size:int -> Context.t -> memory -> binary -> (binary * int * int) option = fun ~size ctx memory address ->
    Whole_Memory.should_focus ~size ctx memory.mem address

  let may_alias : ptr_size:int -> Context.t -> size1:int -> size2:int -> binary -> binary -> bool =
    fun ~ptr_size ctx ~size1 ~size2 addr1 addr2 ->
    Whole_Memory.may_alias ~ptr_size ctx ~size1 ~size2 addr1 addr2

end
