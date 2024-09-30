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

open Transfer_functions_ids;;

(* Abstract domains and their operations.  *)
(* These can be viewed as signatures in the Tagless final approach
   [http://okmij.org/ftp/tagless-final/] *)

type access_type =
| Read                          (* Loading from memory. *)
| Write                         (* Storing into memory. *)

type arith_type =
| Plus                          (* Addition operation *)
| Minus                         (* Substraction operation *)


(* Symbols and their arities. 'r represents the result type. *)
module type Arity = sig
  type ('r) ar0
  type ('a,'r) ar1
  type ('a,'b,'r) ar2
  type ('a,'b,'c,'r) ar3
end

(* Standard arities. *)
module Forward_Arity = struct
  type 'r ar0 = 'r
  type ('a,'r) ar1 = 'a -> 'r
  type ('a,'b,'r) ar2 = 'a -> 'b -> 'r
  type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r
end

module Backward_Arity = struct
  type 'r ar0 = 'r -> unit
  type ('a,'r) ar1 = 'a -> 'r -> ('a option)
  type ('a,'b,'r) ar2 = 'a -> 'b -> 'r -> ('a option * 'b option)
  type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r -> ('a option * 'b option * 'c option)
end
                  

(* Note: in the following, we distinguish between backward and forward
   because there is no need to implement backward transfer functions
   for symbols with arity 0. *)


(****************************************************************)
(* Boolean transfer functions.  *)

module type Boolean_Backward = sig
  type boolean
  module Arity:Arity
  val not: (boolean,boolean) Arity.ar1
  val (&&): (boolean,boolean,boolean) Arity.ar2
  val (||): (boolean,boolean,boolean) Arity.ar2
end

module type Boolean_Forward = sig
  include Boolean_Backward
  val true_: boolean Arity.ar0
  val false_: boolean Arity.ar0
end

(****************************************************************)
(* Integer transfer functions.  *)

module type Integer_Backward = sig
  type integer
  type boolean

  module Arity:Arity

  (* Minimum versions. *)
  val itimes: Z.t -> (integer,integer) Arity.ar1
  val iadd: (integer,integer,integer) Arity.ar2
  val imul: (integer,integer,integer) Arity.ar2
  (* Note: this is truncated (C99-like) integer division. 
     MAYBE: rename to itdiv/itmod? *)
  val idiv: (integer,integer,integer) Arity.ar2
  val imod: (integer,integer,integer) Arity.ar2
  val ishl: (integer,integer,integer) Arity.ar2
  val ishr: (integer,integer,integer) Arity.ar2
  val iand: (integer,integer,integer) Arity.ar2
  val ior: (integer,integer,integer) Arity.ar2
  val ixor: (integer,integer,integer) Arity.ar2            
  val isub: (integer,integer,integer) Arity.ar2
  val ieq: (integer,integer,boolean) Arity.ar2
  val ile: (integer,integer,boolean) Arity.ar2
end

module type Integer_Forward_Min = sig
  include Integer_Backward
  val iconst: Z.t -> integer Arity.ar0
end

module type Integer_Forward = sig
  include Integer_Forward_Min
  (* These can be defined from the others, but it may be more
     efficient to implement them directly (no need to build temporary
     values...). They are also often convenient to use directly. *)
  (* val ilt: (integer,integer,boolean) Arity.ar2 *)
  val zero: integer Arity.ar0
  val one: integer Arity.ar0
end

(****************************************************************)
(* Bitvector transfer functions: a purely numeric abstract domain.  *)

module type Bitvector_Backward = sig
  type bitvector
  type boolean
  module Arity: Arity
  (* nsw means no signed overflow, nuw means no unwrapped overflow. No
     (signed) overflow means that when taking the signed integer value
     of the variable, then performing the operation remains in the
     signed range (and respectively for unsigned overflow). Transfer
     functions can take advantage of this fact to improve their
     precision. *)

  (** Bitvector Integer ADDition.  *)
  val biadd: size:int -> nsw:bool -> nuw:bool -> nusw:bool -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector Integer SUBtraction.  *)      
  val bisub: size:int -> nsw:bool -> nuw:bool -> nusw:bool -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector Integer MULtiplication. *)
  val bimul: size:int -> nsw:bool -> nuw:bool -> (bitvector,bitvector,bitvector) Arity.ar2

  (* IF second argument is larger than size, bshl returns 0. *)
  val bshl: size:int  -> nsw:bool -> nuw:bool -> (bitvector,bitvector,bitvector) Arity.ar2
  (* Arithmetic shift: fill with the most significant bit. *)
  val bashr: size:int -> (bitvector,bitvector,bitvector) Arity.ar2
  (* Logical shift: fill with 0. *)
  val blshr: size:int -> (bitvector,bitvector,bitvector) Arity.ar2

  (* =, and signed/unsigned <= *)
  val beq:   size:int -> (bitvector,bitvector,boolean) Arity.ar2
  val bisle: size:int -> (bitvector,bitvector,boolean) Arity.ar2
  val biule: size:int -> (bitvector,bitvector,boolean) Arity.ar2

  (* First argument become most significant. *)
  val bconcat: size1:int -> size2:int -> (bitvector,bitvector,bitvector) Arity.ar2
  val bextract: size:int -> index:int -> oldsize:int -> (bitvector,bitvector) Arity.ar1
  val band: size:int -> (bitvector,bitvector,bitvector) Arity.ar2
  val bor: size:int -> (bitvector,bitvector,bitvector) Arity.ar2
  val bxor: size:int -> (bitvector,bitvector,bitvector) Arity.ar2

  (* TODO:  buext is just concatenation with zero. *)
  val buext: size:int -> oldsize:int -> (bitvector,bitvector) Arity.ar1
  val bsext: size:int -> oldsize:int -> (bitvector,bitvector) Arity.ar1
  val bisdiv: size:int -> (bitvector,bitvector,bitvector) Arity.ar2
  val bismod: size:int -> (bitvector,bitvector,bitvector) Arity.ar2
  val biudiv: size:int -> (bitvector,bitvector,bitvector) Arity.ar2
  val biumod: size:int -> (bitvector,bitvector,bitvector) Arity.ar2

  (* Returns the bitvector 0 for false, and 1 for true, for a given
     size. TODO: We could also add bitector <-> integer conversions.  *)
  val bofbool: size:int -> (boolean,bitvector) Arity.ar1

end

module type Bitvector_Forward = sig
  include Bitvector_Backward
  val biconst: size:int -> Z.t -> bitvector Arity.ar0
end

(****************************************************************)
(* "Binary" transfer functions.  THey correspond to the values handled
   by C or machine-level programs.  *)

(* Note: the size argument, when provided, refers to the size of the
   result. *)
module type Binary_Backward = sig

  type binary
  include Bitvector_Backward with type bitvector := binary

  (* TODO: Should be a predicate on memory, and take both a binary and memory.
     Returns true if the access to the pointer in memory is valid. *)  
  val valid: size:int -> access_type -> (binary,boolean) Arity.ar1
  (* valid_ptr_arith(a,b) where a is a ptr and b an integer returns true if a + b is in-bound. 
     MAYBE: Could be replaced by a new \valid access types, and overflow assertions on addition. *)
  val valid_ptr_arith: size:int -> arith_type -> (binary,binary,boolean) Arity.ar2

  (* [max], if not None, limits further pointer arithmetics: one cannot go beyond max. *)
  (* MAYBE: separate this between bshift and a new operator [bnarrow]. *)

  (* Note: offset and max are in bytes, not in bits. *)
  val bshift: size:int -> offset:int -> max:int option -> (binary,binary) Arity.ar1 (* b + numeric offset. *)
  val bindex: size:int -> int -> (binary,binary,binary) Arity.ar2 (* b + k * exp. *)

  (* If s is a set, and c is a "choice" (i.e. valuation of conditions),
     choose an element of set according to choice. 

     MAYBE: A more proper solution could be to have types for
     collections, i.e. arrays and sets, but this would require
     duplicating the work on the single elements to collections.  *)
  val bchoose: size:int -> Choice.t -> (binary,binary) Arity.ar1
end

module type Binary_Forward = sig
  include Binary_Backward
  val biconst: size:int -> Z.t -> binary Arity.ar0
  val buninit: size:int -> binary Arity.ar0
end

(****************************************************************)
(* Block transfer functions.  *)

module type Block_Backward = sig
  type boolean
  type offset
  type value
  type block
  module Arity: Arity

  (** Size of a block in bytes *)
  val sizeof: (block,value) Arity.ar1
  (** Concatenates two blocks *)
  val concat: size1:offset -> size2:offset -> (block,block,block) Arity.ar2
  (** Loads (extracts) a value of a fixed size at a given index from a block *)
  val load: size:int -> (block,offset,value) Arity.ar2
  (** Stores (writes) a fixed size value of a given index in a block *)
  val store: size:int -> (block,offset,value,block) Arity.ar3
  (** Converts a fixed size value to a block *)
  val binary_to_block: size:int -> (value,block) Arity.ar1
end

module type Block_Forward = sig
  include Block_Backward
end 

(****************************************************************)
(* Memory transfer functions.  *)
  
module type Memory_Backward = sig
  type block
  type memory
  type address
  type value
  type boolean
  module Arity: Arity
  val load: size:int -> (memory, address, value * memory) Arity.ar2
  val store: size:int -> (memory,address,value,memory) Arity.ar3
  val load_block: (memory, address, block * memory) Arity.ar2
  val store_block: (memory,address,block,memory) Arity.ar3
  (* Fixed-size memcpy(store,from_,to_). *)
  val memcpy: size:int -> (memory,address,address,memory) Arity.ar3
  val malloc: id:Malloc_id.t -> malloc_size:int ->
    (memory,address * memory) Arity.ar1
  val free: (memory,address,memory) Arity.ar2
  val unknown: level:int -> memory Arity.ar0
end

module type Memory_Forward = sig
  include Memory_Backward
end

