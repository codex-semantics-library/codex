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

open Transfer_functions_sig

module type Conversion = sig
  module From_Arity:Arity
  module To_Arity:Arity
  val ar0: 'r From_Arity.ar0 -> 'r To_Arity.ar0
  val ar1: ('a,'r) From_Arity.ar1 -> ('a,'r) To_Arity.ar1
  val ar2: ('a,'b,'r) From_Arity.ar2 -> ('a,'b,'r) To_Arity.ar2
  val ar3: ('a,'b,'c,'r) From_Arity.ar3 -> ('a,'b,'c,'r) To_Arity.ar3
end

module Convert_Boolean_Forward
  (C:Conversion)
  (F:Boolean_Forward with module Arity := C.From_Arity) =
struct
  type boolean = F.boolean
  let true_ = C.ar0 F.true_
  let false_ = C.ar0 F.false_
  let not = C.ar1 F.not
  let (&&) = C.ar2 F.(&&)
  let (||) = C.ar2 F.(||)
end

module Convert_Integer_Forward
  (C:Conversion)
  (F:Integer_Forward with module Arity := C.From_Arity) =
struct
  type boolean = F.boolean
  type integer = F.integer
  let iconst k = C.ar0 (F.iconst k)
  let iadd = C.ar2 F.iadd
  let isub = C.ar2 F.isub
  let imul = C.ar2 F.imul
  let idiv = C.ar2 F.idiv
  let imod = C.ar2 F.imod
  let ishl = C.ar2 F.ishl
  let ishr = C.ar2 F.ishr
  let ior = C.ar2 F.ior
  let ixor = C.ar2 F.ixor
  let iand = C.ar2 F.iand

  (* Redundant *)
  let zero = C.ar0 (F.zero)
  let one = C.ar0 (F.one)

  let ile = C.ar2 F.ile
  let ieq = C.ar2 F.ieq
  let itimes k = C.ar1 (F.itimes k)
  
end


module Convert_Binary_Forward
  (C:Conversion)
  (F:Binary_Forward with module Arity := C.From_Arity) =
struct
  type boolean = F.boolean
  type binary = F.binary
  let biadd ~size ~nsw ~nuw ~nusw = C.ar2 (F.biadd ~size ~nsw ~nuw ~nusw)
  let bisub ~size ~nsw ~nuw ~nusw = C.ar2 (F.bisub ~size ~nsw ~nuw ~nusw)    
  let bisdiv ~size = C.ar2 (F.bisdiv ~size)
  let bismod ~size = C.ar2 (F.bismod ~size)
  let biudiv ~size = C.ar2 (F.biudiv ~size)
  let biumod ~size = C.ar2 (F.biumod ~size)
  let bimul ~size ~nsw ~nuw = C.ar2 (F.bimul ~nsw ~nuw ~size)
  let beq ~size = C.ar2 (F.beq ~size)
  let bisle ~size = C.ar2 (F.bisle ~size)
  let biule ~size = C.ar2 (F.biule ~size)
  let bconcat ~size1 ~size2 = C.ar2 @@ F.bconcat ~size1 ~size2
  let bextract ~size ~index ~oldsize = C.ar1 (F.bextract ~size ~index ~oldsize)
  let band ~size = C.ar2 (F.band ~size)
  let bor ~size = C.ar2 (F.bor ~size)
  let bxor ~size = C.ar2 (F.bxor ~size)
  let buext ~size ~oldsize = C.ar1 (F.buext ~size ~oldsize)
  let bsext ~size ~oldsize = C.ar1 (F.bsext ~size ~oldsize)
  let bofbool ~size = C.ar1 (F.bofbool ~size)
  let bchoose ~size cond = C.ar1 (F.bchoose ~size cond)
  let bshl ~size ~nsw ~nuw  = C.ar2 (F.bshl ~size ~nsw ~nuw)
  let bashr ~size = C.ar2 (F.bashr ~size)
  let blshr ~size = C.ar2 (F.blshr ~size)
  let biconst ~size k = C.ar0 (F.biconst ~size k)
  let buninit ~size = C.ar0 (F.buninit ~size)
  let valid ~size access_type = C.ar1 (F.valid ~size access_type)
  let valid_ptr_arith ~size arith_type = C.ar2 (F.valid_ptr_arith ~size arith_type)
  let bshift ~size ~offset ~max = C.ar1 (F.bshift ~size ~offset ~max)
  let bindex ~size scale = C.ar2 (F.bindex ~size scale)
end

module Convert_Block_Forward
  (C:Conversion)
  (F:Block_Forward with module Arity := C.From_Arity) =
struct
  type boolean = F.boolean
  type value = F.value
  type block = F.block
  type offset = F.offset

  let sizeof = C.ar1 F.sizeof
  let concat ~size1 ~size2 = C.ar2 (F.concat ~size1 ~size2)
  
  let load ~size = C.ar2 (F.load ~size)
  let store ~size = C.ar3 (F.store ~size)     
  let binary_to_block ~size = C.ar1 (F.binary_to_block ~size)
end

module Convert_Memory_Forward
  (C:Conversion)
  (F:Memory_Forward with module Arity := C.From_Arity) =
struct
  type boolean = F.boolean
  type address = F.address
  type memory = F.memory
  type block = F.block
  type value = F.value
  
  let load ~size= C.ar2 (F.load ~size)
  let store ~size= C.ar3 (F.store ~size)
  let load_block = C.ar2 F.load_block
  let store_block = C.ar3 F.store_block
  let memcpy ~size= C.ar3 (F.memcpy ~size)      
  let free = C.ar2 F.free      
  let malloc ~id ~malloc_size = C.ar1 (F.malloc ~id ~malloc_size)
  let unknown ~level = C.ar0 (F.unknown ~level)
end

(* module Convert_Binary_Forward_Monomorphic *)
(*   (C:sig *)
(*     module From_Arity:Arity *)
(*     module To_Arity:Arity *)
(*     type from_binary *)
(*     type from_boolean *)
(*     type from_world *)
(*     type to_binary *)
(*     type to_boolean *)
(*     type to_world *)
(*     val ar0_bin: from_binary From_Arity.ar0 -> to_binary To_Arity.ar0 *)
(*     val ar1_world_bin: (from_world,from_binary) From_Arity.ar1 ->  (to_world,to_binary) To_Arity.ar1       *)
(*     val ar1_bin_bin: (from_binary,from_binary) From_Arity.ar1 ->  (to_binary,to_binary) To_Arity.ar1 *)
(*     val ar1_bin_bool: (from_binary,from_boolean) From_Arity.ar1 ->  (to_binary,to_boolean) To_Arity.ar1       *)
(*     val ar2_bool_bin_bin: (from_boolean,from_binary,from_binary) From_Arity.ar2 ->  (to_boolean,to_binary,to_binary) To_Arity.ar2       *)
(*     val ar2_bin_bin_bin: (from_binary,from_binary,from_binary) From_Arity.ar2 ->  (to_binary,to_binary,to_binary) To_Arity.ar2 *)
(*     val ar2_bin_bin_bool: (from_binary,from_binary,from_boolean) From_Arity.ar2 ->  (to_binary,to_binary,to_boolean) To_Arity.ar2       *)
(*     val variadic: (from_binary,from_binary) From_Arity.variadic -> (to_binary,to_binary) To_Arity.variadic *)
(*   end) *)
(*   (F:Binary_Forward with module Arity := C.From_Arity *)
(*                     and type world = C.from_world *)
(*                     and type binary = C.from_binary *)
(*                     and type boolean = C.from_boolean) = *)
(* struct *)
(*   let nondet ~size = C.variadic (F.nondet ~size) *)
(*   let assume ~size = C.ar2_bool_bin_bin (F.assume ~size) *)
(*   let bitimes k = C.ar1_bin_bin (F.bitimes k) *)
(*   let biadd = C.ar2_bin_bin_bin F.biadd *)
(*   let bisdiv ~size = C.ar2_bin_bin_bin (F.bisdiv ~size) *)
(*   let bismod ~size = C.ar2_bin_bin_bin (F.bismod ~size) *)
(*   let bimul ~size = C.ar2_bin_bin_bin (F.bimul ~size) *)
(*   let beq = C.ar2_bin_bin_bool F.beq *)
(*   let bisle = C.ar2_bin_bin_bool F.bisle *)
(*   let bislt = C.ar2_bin_bin_bool F.bislt *)
(*   let biule = C.ar2_bin_bin_bool F.biule *)
(*   let biult = C.ar2_bin_bin_bool F.biult *)
(*   let bconcat = C.variadic F.bconcat *)
(*   let bextract ~size ~index = C.ar1_bin_bin (F.bextract ~size ~index) *)
(*   let band ~size = C.ar2_bin_bin_bin (F.band ~size) *)
(*   let bor ~size = C.ar2_bin_bin_bin (F.bor ~size) *)
(*   let bxor ~size = C.ar2_bin_bin_bin (F.bxor ~size) *)
(*   let buext ~size = C.ar1_bin_bin (F.buext ~size) *)
(*   let bsext ~size = C.ar1_bin_bin (F.bsext ~size) *)
(*   let bshl ~size = C.ar2_bin_bin_bin (F.bshl ~size) *)
(*   let bashr ~size = C.ar2_bin_bin_bin (F.bashr ~size) *)
(*   let blshr ~size = C.ar2_bin_bin_bin (F.blshr ~size) *)
(*   let biconst ~size k = C.ar0_bin (F.biconst ~size k) *)
(*   let baddr ~size addr = C.ar0_bin (F.baddr ~size addr) *)
(*   let bunknown ~size = C.ar1_world_bin (F.bunknown ~size) *)
(*   let valid ~size = C.ar1_bin_bool (F.valid ~size) *)
(* end *)
  
