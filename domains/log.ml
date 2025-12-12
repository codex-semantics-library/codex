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
  
module Make_Conversion(Log:Tracelog.S)(Sub:Sig.BASE) = struct
  let ar0 ppret pp f  = fun ctx ->
    Log.trace (fun p -> p "%t" pp) ~pp_ret:(ppret ctx)
    @@ fun () -> f ctx

  let ar1 ppa ppret pp f ctx a =
    Log.trace (fun p -> p "%t %a" pp (ppa ctx) a) ~pp_ret:(ppret ctx)
    @@ fun () -> f ctx a

  let ar2 ppa ppb ppret pp f ctx a b =
    Log.trace (fun p -> p "%t %a %a" pp (ppa ctx) a (ppb ctx) b) ~pp_ret:(ppret ctx)
    @@ fun () -> f ctx a b

  let ar3 ppa ppb ppc ppret pp f ctx a b c =
    Log.trace (fun p -> p "%t %a %a %a" pp (ppa ctx) a (ppb ctx) b (ppc ctx) c) ~pp_ret:(ppret ctx)
    @@ fun () -> f ctx a b c

  module Arity = Sig.Context_Arity_Forward(Sub.Context)
  type boolean = Sub.boolean
  type integer = unit
  type enum = Sub.enum
  type bitvector = Sub.binary

  type 'a pp = Sub.Context.t -> Format.formatter -> 'a -> unit

  let bool_printer = Sub.boolean_pretty
  let enum_printer = Sub.enum_pretty
  let integer_printer _ = assert false
  let bv_printer = Sub.binary_pretty

  let prod_printer ppa ppb ctx fmt (a,b) =
    Format.fprintf fmt "(%a,%a)" (ppa ctx) a (ppb ctx) b
end



module Log_Domain(Log : Tracelog.S)(Sub : Sig.BASE):Sig.BASE
  with type Context.t = Sub.Context.t
   and type binary = Sub.binary
   and type boolean = Sub.boolean
   and module Context = Sub.Context
=
struct
  let name = Sub.name
  let unique_id = Sub.unique_id

  type binary = Sub.binary
  type enum = Sub.enum
  type boolean = Sub.boolean

  module Boolean = Sub.Boolean
  module Binary = Sub.Binary
  module Enum = Sub.Enum

  module Context= Sub.Context

  let context_pretty = Sub.context_pretty

  let root_context() =
    Log.trace (fun p -> p "root_context") ~pp_ret:context_pretty @@
    fun () -> Sub.root_context()

  let mu_context_open ctx =
    Log.trace (fun p -> p "mu_context_open %a" context_pretty ctx) ~pp_ret:context_pretty @@
    fun () -> Sub.mu_context_open ctx

  let typed_nondet2 ctxa ctxb in_tup =
    Log.trace (fun p -> p "typed_nondet2" (* "%a %a" *) (* context_pretty ctxa context_pretty ctxb *))
      (* ~pp_ret:(fun fmt (ctx,out_tup) -> context_pretty fmt ctx) *) @@
    fun () -> Sub.typed_nondet2 ctxa ctxb in_tup

  let nondet_same_context ctx in_tup =
    Log.trace (fun p -> p "nondet_same_context" (* context_pretty ctx *)) @@
    fun () -> Sub.nondet_same_context ctx in_tup

  let typed_fixpoint_step ~iteration ~init ~arg ~body =
    Log.trace (fun p -> p "typed_fixpoint_step") @@
    fun () -> Sub.typed_fixpoint_step ~iteration ~init ~arg ~body

  let widened_fixpoint_step ~widening_id ~previous ~next =
    Log.trace (fun p -> p "widened_fixpoint_step") @@
    fun () -> Sub.widened_fixpoint_step ~widening_id ~previous ~next

  let serialize_boolean ctxa a ctxb b in_tup =
    Log.trace (fun p -> p "serialize_boolean %a %a" (Sub.boolean_pretty ctxa) a (Sub.boolean_pretty ctxb) b) @@
    fun () -> Sub.serialize_boolean ctxa a ctxb b in_tup

  let serialize_enum ctxa a ctxb b in_tup =
    Log.trace (fun p -> p "serialize_enum %a %a" (Sub.enum_pretty ctxa) a (Sub.enum_pretty ctxb) b) @@
    fun () -> Sub.serialize_enum ctxa a ctxb b in_tup

  let serialize_binary ~widens ~size ctxa a ctxb b in_tup =
    Log.trace (fun p -> p "serialize_binary ~widens:%b ~size:%d %a %a" widens (In_bits.to_int size)
                  (Sub.binary_pretty ~size ctxa) a (Sub.binary_pretty ~size ctxb) b) @@
    fun () -> Sub.serialize_binary ~widens ~size ctxa a ctxb b in_tup

 (**************** Pretty printing ****************)

  let binary_pretty  = Sub.binary_pretty
  let boolean_pretty = Sub.boolean_pretty
  let enum_pretty = Sub.enum_pretty

  (**************** Tuple fonctions ****************)

  let assume ctx bool =
    Log.trace (fun p -> p "assume %a" (boolean_pretty ctx) bool) @@
    fun () -> Sub.assume ctx bool

  (**************** Queries ****************)

  module Query = struct
    module Binary_Lattice = Sub.Query.Binary_Lattice
    module Enum_Lattice = Sub.Query.Enum_Lattice
    let binary = Sub.Query.binary
    let enum = Sub.Query.enum
  end

  let binary_empty ~size ctx =
    Log.trace (fun p -> p "binary_empty ~size:%d" @@ In_bits.to_int size)
    ~pp_ret:(Sub.binary_pretty ~size ctx) @@
    fun () -> Sub.binary_empty ~size ctx
  let boolean_empty ctx =
    Log.trace (fun p -> p "boolean_empty")
    ~pp_ret:(Sub.boolean_pretty ctx) @@
    fun () -> Sub.boolean_empty ctx
  let enum_empty ctx =
    Log.trace (fun p -> p "enum_empty")
    ~pp_ret:(Sub.enum_pretty ctx) @@
    fun () -> Sub.enum_empty ctx

  let binary_unknown ~size ctx =
    Log.trace (fun p -> p "binary_unknown ~size:%d" @@ In_bits.to_int size)
    ~pp_ret:(Sub.binary_pretty ~size ctx) @@
    fun () -> Sub.binary_unknown ~size ctx
  let binary_unknown_typed ~size ctx typ =
    Log.trace (fun p -> p "binary_unknown_typed ~size:%d typ:%a"
                  (In_bits.to_int size) Types.TypedC.pp typ)
    ~pp_ret:(Sub.binary_pretty ~size ctx) @@
    fun () -> Sub.binary_unknown_typed ~size ctx typ


  let boolean_unknown ctx =
    Log.trace (fun p -> p "boolean_unknown")
    ~pp_ret:(Sub.boolean_pretty ctx) @@
    fun () -> Sub.boolean_unknown ctx
  let enum_unknown ~enumsize ctx =
    Log.trace (fun p -> p "enum_unknown ~enumsize:%d" enumsize)
    ~pp_ret:(Sub.enum_pretty ctx) @@
    fun () -> Sub.enum_unknown ~enumsize ctx

  let satisfiable ctx bool =
    Log.trace (fun p -> p "satisfiable %a" (boolean_pretty ctx) bool) @@
    fun () -> Sub.satisfiable ctx bool

  let union cond ctx tup =
    Log.trace (fun p -> p "union") @@
    fun () -> Sub.union cond ctx tup
  let query_boolean = Sub.query_boolean

  module Conversion = Make_Conversion(Log)(Sub)

  module Boolean_Forward = struct
    include Operator.Autolog.Log_Boolean_Backward(Conversion)(Sub.Boolean_Forward)
    let true_ = Sub.Boolean_Forward.true_
    let false_ = Sub.Boolean_Forward.false_
  end
  module Binary_Forward = Operator.Autolog.Log_Binary_Forward(Conversion)(Sub.Binary_Forward)
  module Enum_Forward = Operator.Autolog.Log_Enum_Forward(Conversion)(Sub.Enum_Forward)
end


(* MAYBE: Should disappear? *)
module Log_With_Focusing(Log : Tracelog.S)(Sub : Memory_domains.With_focusing.S_with_types):Memory_domains.With_focusing.S_with_types
  with type Context.t = Sub.Context.t
   and type binary = Sub.binary
   and type boolean = Sub.boolean
= struct
  (* TODO: Also log blocks and offsets? *)
  include Sub
  include Log_Domain(Log)(Sub)

  module Conversion = struct
    include Make_Conversion(Log)(Sub)
    type memory = Sub.memory
    type binary = Sub.binary
    type address = Sub.binary
    type value = Sub.binary
    type block = Sub.block

    let block_printer = Sub.block_pretty
    let address_printer = (Sub.binary_pretty ~size:(Codex_config.ptr_size()))
    let memory_printer = Sub.memory_pretty
    let value_printer = Sub.binary_pretty
  end

  module Memory_Forward = Operator.Autolog.Log_Memory_Forward(Conversion)(Sub.Memory_Forward)

  (* TODO: Something to fix here. *)
  module Query = struct
    include Query
    let reachable = Sub.Query.reachable
  end


end
