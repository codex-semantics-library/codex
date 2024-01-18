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

(* Typed (with GADT), easily reusable term constructors. *)

(* The declarations here are necessary so that types are unique.
   This could be replaced by type boolean = | in newer OCamls. *)
type boolean = private TypeBoolean
type integer = private TypeInteger
type binary = private TypeBinary
type memory = private TypeMemory
type ar0 = private Ar0
type 'a ar1 = private Ar1
type ('a, 'b) ar2 = private Ar2

type size = int
type 'a typ =
  | Boolean : boolean typ
  | Integer : integer typ
  | Binary : size -> binary typ
  | Memory : memory typ

type id = private int
type any_type = Any_type : 'a typ -> any_type [@@unboxed]

type ('arg, 'ret) term =
  | True : (ar0, boolean) term
  | False : (ar0, boolean) term
  | And : ((boolean, boolean) ar2, boolean) term
  | Or : ((boolean, boolean) ar2, boolean) term
  | Not : (boolean ar1, boolean) term
  | BoolUnion: ((boolean,boolean) ar2,boolean) term        
  | Biconst : size * Z.t -> (ar0, binary) term
  | Biadd : {size:size;nsw:bool;nuw:bool;nusw:bool} -> ((binary, binary) ar2, binary) term
  | Bisub : {size:size;nsw:bool;nuw:bool;nusw:bool} -> ((binary, binary) ar2, binary) term
  | Bimul : {size:size;nsw:bool;nuw:bool} -> ((binary, binary) ar2, binary) term
  | Biudiv : size -> ((binary, binary) ar2, binary) term
  | Bisdiv : size -> ((binary, binary) ar2, binary) term
  | Biumod : size -> ((binary, binary) ar2, binary) term
  | Bismod : size -> ((binary, binary) ar2, binary) term
  | Bshl : {size:size;nsw:bool;nuw:bool} -> ((binary, binary) ar2, binary) term
  | Bashr : size -> ((binary, binary) ar2, binary) term
  | Blshr : size -> ((binary, binary) ar2, binary) term
  | Band : size -> ((binary, binary) ar2, binary) term
  | Bor : size -> ((binary, binary) ar2, binary) term
  | Bxor : size -> ((binary, binary) ar2, binary) term
  | Beq : size -> ((binary, binary) ar2, boolean) term
  | Bisle : size -> ((binary, binary) ar2, boolean) term
  | Biule : size -> ((binary, binary) ar2, boolean) term
  | Bconcat : size * size -> ((binary, binary) ar2, binary) term
  | Bextract : { size : size; index : int; oldsize : size; } -> (binary ar1, binary) term
  | Bsext : size -> (binary ar1, binary) term
  | Buext : size -> (binary ar1, binary) term
  | Bofbool: size -> (boolean ar1,binary) term
  | Bunion: Transfer_functions_ids.Condition.t * size -> ((binary,binary) ar2,binary) term
  | Bchoose: Transfer_functions_ids.Choice.t * size -> (binary ar1,binary) term
  | Iconst : Z.t -> (ar0, integer) term
  | Iadd : ((integer, integer) ar2, integer) term
  | Isub : ((integer, integer) ar2, integer) term
  | Imul : ((integer, integer) ar2, integer) term
  | Idiv : ((integer, integer) ar2, integer) term
  | Imod : ((integer, integer) ar2, integer) term
  | Ishl : ((integer, integer) ar2, integer) term
  | Ishr : ((integer, integer) ar2, integer) term
  | Ior : ((integer, integer) ar2, integer) term
  | Ixor : ((integer, integer) ar2, integer) term
  | Iand : ((integer, integer) ar2, integer) term
  | Ieq : ((integer, integer) ar2, boolean) term
  | Ile : ((integer, integer) ar2, boolean) term
  | Itimes : Z.t -> (integer ar1, integer) term


(* Builder for these terms; allows to share their representation 
   (which was degrading performance a bit)  *)
module Build:sig

  module Arity:sig
    type nonrec 'r ar0 = (ar0,'r) term
    type nonrec ('a,'r) ar1 = ('a ar1,'r) term
    type nonrec  ('a,'b,'r) ar2 = (('a,'b) ar2,'r) term
    type nonrec ('a,'b,'c,'r) ar3 = unit
  end
  
  module Binary:sig
    include Transfer_functions_sig.Binary_Forward
      with module Arity := Arity
       and type boolean := boolean
       and type binary := binary
    val bunion: size:int -> Transfer_functions_ids.Condition.t -> ((binary,binary) ar2, binary) term
  end

end


val type_of : ('a, 'b) term -> 'b typ
val hash : ('a, 'b) term -> int
val equal : ('a, 'b) term -> ('c, 'd) term -> bool


(* Helper to build pretty-printers. *)
module type Pretty_Arg = sig
  type 'a t
  type 'a pack
  val pretty : Format.formatter -> 'a t -> unit
  val extract1 : 'a ar1 pack -> 'a t
  val extract2 : ('a, 'b) ar2 pack -> 'a t * 'b t
end

module type Pretty_Result = sig
  type 'a t
  type 'a pack
  val pretty_destruct :
    Format.formatter -> ('arg, 'a) term -> 'arg pack -> unit
end

module Pretty(M : Pretty_Arg):Pretty_Result
  with type 'a t = 'a M.t and type 'a pack = 'a M.pack
