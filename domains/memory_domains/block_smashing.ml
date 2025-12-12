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

module Log = Tracelog.Make(struct let category = "Domains.Block_smashing" end);;
module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree
module In_bytes = Units.In_bytes
module In_bits = Units.In_bits                    

module Make 
    (Value : Memory_sig.FIXED_SIZE_VALUE_DOMAIN)
    (Offset : Memory_sig.OFFSET with module Scalar = Value.Scalar)
= struct

  include Value
  
  module Scalar = Offset.Scalar
  module Offset = Offset
  module Value = Value

  type offset = Offset.offset

  type block = {length : Scalar.binary ; elem_size : In_bytes.t ; content : Value.binary}

  let pretty (ctx : Context.t) fmt {length; elem_size; content} = 
    let ptr_size = Codex_config.ptr_size () in  
    let open Format in
    fprintf fmt "{[%a of size %d] of length %a}" 
      (Value.binary_pretty ~size:(In_bytes.in_bits elem_size) ctx) content
      (elem_size:>int)
      (Scalar.binary_pretty ~size:ptr_size ctx) length

  let block_empty (ctx : Context.t) =
    let ptr_size = Codex_config.ptr_size () in
    let elem_size = In_bytes.one in
    let length = Scalar.binary_empty ~size:ptr_size ctx in
    let content = Value.binary_empty ~size:(In_bytes.in_bits elem_size) ctx in
    {length; elem_size; content}
    
  let unknown ~level (ctx : Context.t) =
    let ptr_size = Codex_config.ptr_size () in  
    let elem_size = In_bytes.one in
    let length = Scalar.binary_empty ~size:ptr_size ctx in
    let content = Value.binary_unknown ~size:(In_bytes.in_bits elem_size) ctx in
    {length ; elem_size; content}


  let index_zero (ctx : Context.t) = 
    Offset.offset_zero ~max:None ctx

  let initial (ctx : Context.t) (byte_size:In_bytes.t) = 
    let ptr_size = Codex_config.ptr_size () in  
    let elem_size = In_bytes.one in
    let length = Scalar.Binary_Forward.biconst ~size:ptr_size (Z.of_int (byte_size:>int)) ctx in
    let content = Value.binary_unknown ~size:(In_bytes.in_bits elem_size) ctx in
    {length; elem_size; content}


  (* Utility functions *)
  let join_binary_values ~size (ctx : Context.t) list =
    match list with
    | [] -> Value.binary_empty ~size ctx
    | [x]-> x
    | a::b ->
      let nondet_binary v1 v2 = 
        let Context.Result(_,tup,deserialize2) = Value.serialize ~widens:false ~size ctx v1 ctx v2
            (true, Context.empty_tuple ()) in
        let res_tup = Value.Scalar.nondet_same_context ctx tup in
        fst @@ deserialize2 ctx res_tup
      in List.fold_left nondet_binary a b

  let join_binary_content ~size (ctx : Context.t) list =
    match list with
    | [] -> Value.binary_empty ~size ctx
    | [x]-> x
    | a::b ->
      let nondet_binary v1 v2 = 
        let Context.Result(_,tup,deserialize2) = Value.serialize ~widens:false ~size ctx v1 ctx v2
            (true, Context.empty_tuple ()) in
        let res_tup = Value.union (Operator.Condition.fresh ()) ctx tup in
        fst @@ deserialize2 ctx res_tup
      in List.fold_left nondet_binary a b


  module Block_Forward = struct
    
    let sizeof (ctx : Context.t) {length; elem_size} : offset =
      Offset.offset_index (elem_size:>int) ctx (index_zero ctx) length

    (** Repeats the concatenation of [value] [n] times **)
    let rec repeat_concat ~size num ctx value = 
      if num <= 1 then value
      else 
        let num = num - 1 in
        Binary_Forward.bconcat ~size1:size ~size2:(In_bits.of_int (num * (size:>int))) ctx value @@ repeat_concat ~size num ctx value

    let concat (ctx : Context.t) a b =
      let ptr_size = Codex_config.ptr_size () in  
      assert (a.elem_size = b.elem_size) ;
      let elem_size = a.elem_size in
      let content = join_binary_values ~size:(In_bytes.in_bits elem_size) ctx [a.content; b.content] in
      let length = Scalar.Binary_Forward.biadd ~size:ptr_size
          ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx a.length b.length in
      { length; elem_size; content }


    exception Invalid_memory_access

    let index_le (ctx : Context.t) x y = 
      let res = Offset.offset_le ctx x y in
      match Scalar.query_boolean ctx @@ Offset.boolean2scalar_bool ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false
    ;;

    (** Loads a binary value of size [size] at index [index] in a block [block], returning a binary value **)     
    let load ~(size:In_bits.t) ctx block (index : offset) : Value.binary =
      let ptr_size = Codex_config.ptr_size () in  
      Log.debug (fun p -> p "Block_smashing.load ~size:%d block:%a index:%a"
        (size:>int)
        (pretty ctx) block
        (Offset.offset_pretty ctx) index
      ) ;

      (* This code seems very wrong. Why loading something of size
         different than the element size would return bottom? *)
      assert ((size:>int) mod 8 = 0) ;

      (* let zero = index_zero ctx in *)
      let block_size = sizeof ctx block in
      let zero = Offset.offset_zero ~max:None ctx in

      try
        let offset = Offset.offset_sub ctx index zero in
        let elem_size_b = Scalar.Binary_Forward.biconst ~size:ptr_size (Z.of_int @@ In_bytes.to_int block.elem_size) ctx in
        let offset = Scalar.Binary_Forward.bismod ~size:ptr_size ctx offset elem_size_b in
        match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptr_size @@ binary ~size:ptr_size ctx offset) with
        | Some o ->
          let ofs = Z.to_int o |> In_bytes.of_int in
          if In_bits.(size > In_bytes.(in_bits @@ block.elem_size - ofs)) then raise Invalid_memory_access
          else 
            let oldsize = block.elem_size |> In_bytes.in_bits in
            let value = Value.Binary_Forward.bchoose ~size (Operator.Choice.fresh ()) ctx block.content in 
            Value.Binary_Forward.bextract ~size ~index:(In_bytes.in_bits ofs) ~oldsize ctx value
       
        | _ -> raise Invalid_memory_access

      with Invalid_memory_access -> 
        Emit_alarm.emit_alarm Operator.Alarm.Array_offset_access ;
        Codex_log.error "@[<hov 2>Out-of-bounds access at index %a with size of %d in block of size %a.@]"
          (Offset.offset_pretty ctx) index (size:>int) (Offset.offset_pretty ctx) block_size ;
        Value.binary_empty ~size ctx

    let binary_is_zero (ctx : Context.t) a = 
      let ptr_size = Codex_config.ptr_size () in  
      let zero = Scalar.Binary_Forward.biconst ~size:ptr_size Z.zero ctx in
      let res = Scalar.Binary_Forward.beq ~size:ptr_size ctx zero a in
      match Scalar.query_boolean ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false

    (** Store a binary value [value] of size [size] in a block [block] at index [index] **)
    let store ~(size:In_bits.t) ctx block index value =
      let ptr_size = Codex_config.ptr_size () in  
      Log.debug (fun p -> p "Block_smashing.store ~size:%d block:%a index:%a value:%a"
        (size:>int)
        (pretty ctx) block
        (Offset.offset_pretty ctx) index
        (Value.binary_pretty ~size ctx) value
      ) ;

      assert ((size:>int) mod 8 = 0) ;
      
      let block_size = sizeof ctx block in
      let zero = Offset.offset_zero ~max:None ctx in

      try
        let offset = Offset.offset_sub ctx index zero in
        let elem_size_b = Scalar.Binary_Forward.biconst ~size:ptr_size (Z.of_int @@ In_bytes.to_int block.elem_size) ctx in
        let offset = Scalar.Binary_Forward.bismod ~size:ptr_size ctx offset elem_size_b in
        match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptr_size @@ binary ~size:ptr_size ctx offset) with
        | Some o ->
          let ofs = Z.to_int o |> In_bytes.of_int in
          if In_bits.(size >  In_bytes.((block.elem_size - ofs) |> in_bits)) then raise Invalid_memory_access
          else
            let ofs = ofs |> In_bytes.in_bits in
            let oldsize = block.elem_size |> In_bytes.in_bits in
            let loaded = Value.Binary_Forward.bchoose ~size (Operator.Choice.fresh ()) ctx block.content in 
            let to_store =
              if ofs > In_bits.zero then
                let v1 = Value.Binary_Forward.bextract ~size:ofs ~index:In_bits.zero ~oldsize ctx loaded in
                Value.Binary_Forward.bconcat ~size1:size ~size2:ofs ctx value v1
              else value
            in
            let to_store =
              if In_bits.(ofs + size < oldsize) then
                let v2 = Value.Binary_Forward.bextract ~size:In_bits.(oldsize - ofs - size) ~index:In_bits.(ofs + size) ~oldsize ctx loaded in
                Value.Binary_Forward.bconcat ~size1:In_bits.(oldsize - ofs - size) ~size2:In_bits.(ofs + size) ctx v2 to_store
              else to_store
            in
            let content = join_binary_content ~size ctx [block.content; to_store] in
            {block with content}

        | _ -> raise Invalid_memory_access
      
      with Invalid_memory_access -> 
        begin 
          Emit_alarm.emit_alarm Operator.Alarm.Array_offset_access ;
          Codex_log.error "@[<hov 2>Out-of-bounds access at index %a with a size of %d in block of size %a.@]"
            (Offset.offset_pretty ctx) index (size:>int) (Offset.offset_pretty ctx) block_size ;
          block_empty ctx
        end

    let binary_to_block ~(size:In_bits.t) (ctx : Context.t) value =
      let ptr_size = Codex_config.ptr_size () in  
      assert ((size:>int) mod 8 = 0) ;
      let one = Scalar.Binary_Forward.biconst ~size:ptr_size Z.one ctx in 
      {length = one; elem_size = size |> In_bits.in_bytes; content = value}

  end

  include Block_Forward

  let serialize ~widens (ctxa : Context.t) (a : block) (ctxb : Context.t) (b : block) (acc : 'a Context.in_acc) : (block, 'a) Context.result =
    Log.debug (fun p -> p "Block_smashing.serialize %a %a" (pretty ctxa) a (pretty ctxb) b) ;  
    let ptr_size = Codex_config.ptr_size () in  
    assert (a.elem_size = b.elem_size) ;
    let Context.Result (inc, tup, d_length) = Scalar.serialize_binary ~widens ~size:ptr_size ctxa a.length ctxb b.length acc in
    let Context.Result (inc, tup, d_content) = Value.serialize ~widens ~size:(a.elem_size |> In_bytes.in_bits) ctxa a.content ctxb b.content (inc,tup) in
    Context.Result (inc, tup, fun ctx out ->
      let content, out = d_content ctx out in
      let length, out = d_length ctx out in
      {length; elem_size = a.elem_size; content}, out
    )

  let block_unknown_typed ctx typ = 
    let ptr_size = Codex_config.ptr_size () in  
    let open TypedC in
    let typ = inlined typ in
    match typ.descr with
    | Array (elem_typ, (Variable_length sz)) -> 
      let size,length = Value.global_symbol ctx sz in
      let length = Value.binary2scalar_binary ~size:ptr_size ctx length in
      assert (size = ptr_size) ;
      let elem_size = sizeof elem_typ in
      let content = Value.binary_unknown_typed ~size:(elem_size |> In_bytes.in_bits) ctx elem_typ in
      {length; elem_size; content}
      
    | _ -> assert false

  let check_type ctx typ ({ length; elem_size; content } as block) =
    Log.trace (fun p -> p "check_type %a %a" TypedC.pp typ (pretty ctx) block)
    @@ fun () ->
    let open Type_check_tree in 
    let block_data = create_typing_node block (pretty ctx) typ in
    let typing_store () =
      Codex_log.error "Type %a possibly does not hold for block %a" TypedC.pp
  typ (pretty ctx) block;
      type_error block_data Operator.Alarm.Typing_store in
    let ptr_size = Codex_config.ptr_size () in
    let open TypedC in
    let typ = inlined typ in
    match typ.descr with
    | Array (elem_typ, Variable_length sz) ->
        let sz, len = Value.global_symbol ctx sz in
        let len = Value.binary2scalar_binary ~size:ptr_size ctx len in
        assert (sz = ptr_size);
        if sizeof elem_typ = elem_size then
          let res = Scalar.Binary_Forward.beq ~size:ptr_size ctx len length in
          match Scalar.query_boolean ctx res with
          | Lattices.Quadrivalent.True -> no_error block_data
          | _ -> typing_store ()
        else typing_store ()
    | Array (elem_typ, Fixed_length sz) ->
        let len = Scalar.Binary_Forward.biconst ~size:ptr_size sz ctx in
        if sizeof elem_typ = elem_size then
          let res = Scalar.Binary_Forward.beq ~size:ptr_size ctx len length in
          match Scalar.query_boolean ctx res with
          | Lattices.Quadrivalent.True ->
              check_type ~size:(elem_size |> In_bytes.in_bits) ctx elem_typ content
              |> sub_check block_data
          | _ -> typing_store ()
        else typing_store ()
    | _ -> typing_store ()

  let addresses_in_block (ctxa : Context.t) a (ctxb : Context.t) b =
    assert (a.elem_size = b.elem_size) ;
    let size = a.elem_size |> In_bytes.in_bits in
    Value.addresses_in_binary ~size ctxa a.content ctxb b.content

end
