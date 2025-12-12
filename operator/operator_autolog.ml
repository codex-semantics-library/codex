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

(* Helpers to automatically log calls to transfer functions.   *)

open Operator_sig
module In_bits = Units.In_bits

module type BOOLEAN_CONVERSION = sig
  module Arity : ARITY
  type boolean
  type 'a pp
  val bool_printer : boolean pp
  val ar0: 'r pp -> (Format.formatter -> unit) -> 'r Arity.ar0 -> 'r Arity.ar0
  val ar1: 'a pp -> 'r pp -> (Format.formatter -> unit) ->
    ('a,'r) Arity.ar1 -> ('a,'r) Arity.ar1
  val ar2: 'a pp -> 'b pp -> 'r pp -> (Format.formatter -> unit) ->
    ('a,'b,'r) Arity.ar2 -> ('a,'b,'r) Arity.ar2
end

module type INTEGER_CONVERSION = sig
  include BOOLEAN_CONVERSION
  type integer
  val integer_printer : integer pp
end

module type ENUM_CONVERSION = sig
  include BOOLEAN_CONVERSION
  type enum
  val enum_printer : enum pp
end

module type BITVECTOR_CONVERSION = sig
  include BOOLEAN_CONVERSION
  type bitvector
  val bv_printer : size:In_bits.t -> bitvector pp    
end

module type MEMORY_CONVERSION = sig
  include BOOLEAN_CONVERSION
  val ar3: 'a pp -> 'b pp -> 'c pp -> 'r pp -> (Format.formatter -> unit) ->
    ('a,'b,'c,'r) Arity.ar3 -> ('a,'b,'c,'r) Arity.ar3
  
  type block
  type memory
  type address
  type value
  val block_printer: block pp    
  val address_printer: address pp
  val memory_printer: memory pp
  val value_printer: size:In_bits.t -> value pp
  val prod_printer: 'a pp -> 'b pp -> ('a * 'b) pp
  (* type bitvector *)
  (* val bv_printer : size:int -> bitvector pp     *)
end


module Log_Boolean_Backward
      (C : BOOLEAN_CONVERSION)
      (F : BOOLEAN_BACKWARD
             with module Arity := C.Arity
              and type boolean := C.boolean) :
    BOOLEAN_BACKWARD with module Arity := C.Arity and type boolean := C.boolean =
  struct
    open C

    let ar1_bool_bool = ar1 C.bool_printer C.bool_printer
    let ar2_bool_bool_bool = ar2 C.bool_printer C.bool_printer C.bool_printer

    let (&&) =
      ar2_bool_bool_bool (fun fmt -> Format.fprintf fmt "(&&)") F.(&&)

    let (||) =
      ar2_bool_bool_bool (fun fmt -> Format.fprintf fmt "(||)") F.(||)

    let not = ar1_bool_bool (fun fmt -> Format.fprintf fmt "not") F.not
  end

  module Log_Integer_Backward
      (C : INTEGER_CONVERSION)
      (F : INTEGER_BACKWARD
             with module Arity := C.Arity
              and type boolean := C.boolean
              and type integer := C.integer) :
    INTEGER_BACKWARD
      with module Arity := C.Arity
       and type boolean := C.boolean
       and type integer := C.integer = struct
    open C

    let ar1_int_int = ar1 C.integer_printer C.integer_printer

    let ar2_int_int_int =
      ar2 C.integer_printer C.integer_printer C.integer_printer

    let ar2_int_int_bool =
      ar2 C.integer_printer C.integer_printer C.bool_printer

    let itimes z =
      ar1_int_int
        (fun fmt -> Format.fprintf fmt "itimes %s" (Z.to_string z))
        (F.itimes z)

    let iadd = ar2_int_int_int (fun fmt -> Format.fprintf fmt "iadd") F.iadd
    let imul = ar2_int_int_int (fun fmt -> Format.fprintf fmt "imul") F.imul
    let isub = ar2_int_int_int (fun fmt -> Format.fprintf fmt "isub") F.isub
    let idiv = ar2_int_int_int (fun fmt -> Format.fprintf fmt "idiv") F.idiv
    let imod = ar2_int_int_int (fun fmt -> Format.fprintf fmt "imod") F.imod
    let ishl = ar2_int_int_int (fun fmt -> Format.fprintf fmt "ishl") F.ishl
    let ishr = ar2_int_int_int (fun fmt -> Format.fprintf fmt "ishr") F.ishr
    let iand = ar2_int_int_int (fun fmt -> Format.fprintf fmt "iand") F.iand
    let ior = ar2_int_int_int (fun fmt -> Format.fprintf fmt "ior") F.ior
    let ixor = ar2_int_int_int (fun fmt -> Format.fprintf fmt "ixor") F.ixor
    let ieq = ar2_int_int_bool (fun fmt -> Format.fprintf fmt "ieq") F.ieq
    let ile = ar2_int_int_bool (fun fmt -> Format.fprintf fmt "ile") F.ile
  end


  module Log_Bitvector_Forward
      (C : BITVECTOR_CONVERSION)
      (F : BITVECTOR_FORWARD
             with module Arity := C.Arity
              and type boolean := C.boolean
              and type bitvector := C.bitvector) :
    BITVECTOR_FORWARD
      with module Arity := C.Arity
       and type boolean := C.boolean
       and type bitvector := C.bitvector = struct
    open C

    let ar2_bin_bin_bin ~size =
      ar2 (C.bv_printer ~size) (C.bv_printer ~size) (C.bv_printer ~size)

    let ar2_bin_bin_bool ~size =
      ar2 (C.bv_printer ~size) (C.bv_printer ~size) C.bool_printer

    let biconst ~size z =
      ar0 (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "biconst ~size:%d %s" (size:>int) (Z.to_string z))
        (F.biconst ~size z)

    let buext ~size ~oldsize =
      ar1 (C.bv_printer ~size:oldsize)  (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "buext ~size:%d ~oldsize:%d" (size:>int) (oldsize:>int))
        (F.buext ~size ~oldsize)

    let bsext ~size ~oldsize =
      ar1 (C.bv_printer ~size:oldsize)  (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "bsext ~size:%d oldsize:%d" (size:>int) (oldsize:>int))
        (F.bsext ~size ~oldsize)    

    let bextract ~size ~index ~oldsize =
      ar1 (C.bv_printer ~size:oldsize)  (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "bextract ~size:%d ~index:%d ~oldsize:%d"
            (size:>int) (In_bits.to_int index)  (oldsize:>int))
        (F.bextract ~size ~index ~oldsize)    

    let bofbool ~size =
      ar1 (C.bool_printer)  (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "bofbool ~size:%d" (size:>int))
        (F.bofbool ~size)

    let biadd ~size ~flags =
      let Flags.Bisub.{nsw;nuw;nusw} = Flags.Biadd.unpack flags in
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "biadd ~size:%d ~nsw:%b ~nuw:%b ~nusw:%b" (size:>int) nsw nuw nusw)
        (F.biadd ~size ~flags)

    let bisub ~size ~flags =
      let Flags.Bisub.{nsw;nuw;nusw} = Flags.Bisub.unpack flags in
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bisub ~size:%d ~nsw:%b ~nuw:%b ~nusw:%b" (size:>int) nsw nuw nusw)
        (F.bisub ~size ~flags)
    
    let bimul ~size ~flags =
      let Flags.Bimul.{nsw;nuw} = Flags.Bimul.unpack flags in
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bimul ~size:%d ~nsw:%b ~nuw:%b" (size:>int)
            nsw nuw)
        (F.bimul ~size ~flags)

    let bshl ~size ~flags =
      let Flags.Bshl.{nsw;nuw} = Flags.Bshl.unpack flags in
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bshl ~size:%d ~nsw:%b ~nuw:%b" (size:>int) nsw nuw)
        (F.bshl ~size ~flags)

    let bisdiv ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bisdiv ~size:%d" (size:>int))
        (F.bisdiv ~size)

    let bismod ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bismod ~size:%d" (size:>int))
        (F.bismod ~size)

    let biudiv ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "biudiv ~size:%d" (size:>int))
        (F.biudiv ~size)

    let biumod ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "biumod ~size:%d" (size:>int))
        (F.biumod ~size)

    let bashr ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bashr ~size:%d" (size:>int))
        (F.bashr ~size)

    let blshr ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "blshr ~size:%d" (size:>int))
        (F.blshr ~size)

    let band ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "band ~size:%d" (size:>int))
        (F.band ~size)

    let bor ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bor ~size:%d" (size:>int))
        (F.bor ~size)

    let bxor ~size =
      ar2_bin_bin_bin ~size
        (fun fmt -> Format.fprintf fmt "bxor ~size:%d" (size:>int))
        (F.bxor ~size)

    let beq ~size =
      ar2_bin_bin_bool ~size
        (fun fmt -> Format.fprintf fmt "beq ~size:%d" (size:>int))
        (F.beq ~size)

    let bisle ~size =
      ar2_bin_bin_bool ~size
        (fun fmt -> Format.fprintf fmt "bisle ~size:%d" (size:>int))
        (F.bisle ~size)

    let biule ~size =
      ar2_bin_bin_bool ~size
        (fun fmt -> Format.fprintf fmt "biule ~size:%d" (size:>int))
        (F.biule ~size)

    let bconcat ~size1 ~size2 =
      ar2 (C.bv_printer ~size:size1) (C.bv_printer ~size:size2)
        (C.bv_printer ~size:In_bits.(size1 + size2))
        (fun fmt -> Format.fprintf fmt "bconcat ~~size1:%d ~size2:%d"
            (size1:>int) (size2:>int))
        (F.bconcat ~size1 ~size2)

  end

  module Log_Bitvector_Forward_With_Bimul_add
      (C : BITVECTOR_CONVERSION)
      (F : BITVECTOR_FORWARD_WITH_BIMUL_ADD
             with module Arity := C.Arity
              and type boolean := C.boolean
              and type bitvector := C.bitvector) :
    BITVECTOR_FORWARD_WITH_BIMUL_ADD
      with module Arity := C.Arity
       and type boolean := C.boolean
       and type bitvector := C.bitvector = struct
    include Log_Bitvector_Forward(C)(F)
    let bimul_add ~size ~prod ~offset =
      C.ar1 (C.bv_printer ~size) (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "bextract ~size:%d ~prod:%s ~offset:%s" (size:>int)
            (Z.to_string prod) (Z.to_string offset))
        (F.bimul_add ~size ~prod ~offset)        
  end


  module Log_Binary_Forward
      (C : BITVECTOR_CONVERSION)
      (F : BINARY_FORWARD
             with module Arity := C.Arity
              and type boolean := C.boolean
              and type binary := C.bitvector) :
    BINARY_FORWARD
      with module Arity := C.Arity
       and type boolean := C.boolean
       and type binary := C.bitvector = struct
    include Log_Bitvector_Forward(C)(F)
    let buninit ~size =
      C.ar0 (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "buninit ~size:%d" (size:>int))
        (F.buninit ~size)
    let valid ~size access_type =
      C.ar1 (C.bv_printer ~size) (C.bool_printer)
        (fun fmt -> Format.fprintf fmt "valid ~size:%d %s" (size:>int) (match access_type with Read -> "read" | Write -> "write"))
        (F.valid ~size access_type)        
    let valid_ptr_arith ~size arith_type =
      C.ar2 (C.bv_printer ~size) (C.bv_printer ~size) (C.bool_printer)
        (fun fmt -> Format.fprintf fmt "valid_ptr_arith ~size:%d %s" (size:>int)
            (match arith_type with Plus -> "plus" | Minus -> "minus"))
        (F.valid_ptr_arith ~size arith_type)
    let bshift ~size ~offset ~max =
      C.ar1 (C.bv_printer ~size) (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "bshift ~size:%d %d %s" (size:>int) offset
            (match max with None -> "None" | Some x -> string_of_int x))
        (F.bshift ~size ~offset ~max)
    let bindex ~size (scale:int) =
      C.ar2 (C.bv_printer ~size) (C.bv_printer ~size) (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "bindex ~size:%d %d" (size:>int) scale)
        (F.bindex ~size scale)
    let bchoose ~size (cond:Operator_ids.Choice.t) =
      C.ar1 (C.bv_printer ~size) (C.bv_printer ~size)
        (fun fmt -> Format.fprintf fmt "bchoose ~size:%d %d" (size:>int) (cond :> int))
        (F.bchoose ~size cond)
  end




  module Log_Enum_Forward
      (C : ENUM_CONVERSION)
      (F : ENUM_FORWARD
             with module Arity := C.Arity
              and type boolean := C.boolean
              and type enum := C.enum) :
    ENUM_FORWARD
      with module Arity := C.Arity
       and type boolean := C.boolean
       and type enum := C.enum = struct
    let caseof ~case =
      C.ar1 C.enum_printer C.bool_printer
        (fun fmt -> Format.fprintf fmt "caseof ~case:%d" case)
        (F.caseof ~case)

    let enum_const ~case =
      C.ar0 C.enum_printer
        (fun fmt -> Format.fprintf fmt "enum_const ~case:%d" case)
        (F.enum_const ~case)
  end


  module Log_Memory_Forward
      (C : MEMORY_CONVERSION)
      (F : MEMORY_FORWARD
             with module Arity := C.Arity
              and type boolean := C.boolean
              and type memory := C.memory
              and type block := C.block
              and type address := C.address
              and type value := C.value) :
    MEMORY_FORWARD
             with module Arity := C.Arity
              and type boolean := C.boolean
              and type memory := C.memory
              and type block := C.block
              and type value := C.value
              and type address := C.address
= struct
  let load ~size =
    C.ar2 C.memory_printer C.address_printer
      C.(prod_printer (value_printer ~size) memory_printer)
      (fun fmt -> Format.fprintf fmt "load ~size:%d" (In_bits.to_int size))
      (F.load ~size)

  let load_block =
    C.ar2 C.memory_printer C.address_printer
      C.(prod_printer block_printer memory_printer)
      (fun fmt -> Format.fprintf fmt "load_block")
      F.load_block

  
  let store ~size =
    C.ar3 C.memory_printer C.address_printer (C.value_printer ~size) C.memory_printer
      (fun fmt -> Format.fprintf fmt "store ~size:%d" (In_bits.to_int size))
      (F.store ~size)

  let store_block =
    C.ar3 C.memory_printer C.address_printer C.block_printer C.memory_printer
      (fun fmt -> Format.fprintf fmt "store_block")
      F.store_block

  let memcpy ~size =
    C.ar3 C.memory_printer C.address_printer C.address_printer C.memory_printer
      (fun fmt -> Format.fprintf fmt "memcpy size:%d" (In_bits.to_int size))
      (F.memcpy ~size)

  let malloc ~id ~malloc_size =
    C.ar1 C.memory_printer C.(prod_printer address_printer memory_printer)
      (fun fmt -> Format.fprintf fmt "malloc id:%s malloc_size:%d"
          (Operator_ids.Malloc_id.to_string id) (Units.In_bytes.to_int malloc_size))
      (F.malloc ~id ~malloc_size)
  
  let free =
    C.ar2 C.memory_printer C.address_printer C.memory_printer
      (fun fmt -> Format.fprintf fmt "free")
      F.free
  
  let unknown ~level =
    C.ar0 C.memory_printer
      (fun fmt -> Format.fprintf fmt "unknown level:%d" level)
      (F.unknown ~level)
  end
