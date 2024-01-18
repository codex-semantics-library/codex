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

(* Ctypes: Grammar for describing the types for the type-based domain, 
   and operations on concrete types. *)

type value =
  | Const of Z.t
  | Sym of string

val pp_value : Format.formatter -> value -> unit

module Pred : sig
  type unop =
    | Extract of int * int (** start index, length *)

  type binop =
    | Add | Sub | Mul | And
    | Or
    | Concat of int * int (** Size 1, size 2. First argument is the most significant *)

  type expr =
    | Val of value
    | Self
    | Unop of unop * expr
    | Binop of binop * expr * expr

  type cmpop = Equal | NotEqual | ULt | SLt | ULeq | SLeq | UGeq | SGeq | UGt | SGt

  (** Predicate on a structure field or array element, which is supposed to be
      true at all times. *)
  type t =
    | True
    | Cmp of cmpop * expr * expr
    | And of t * t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  (** Helpers. *)
  val eq : value -> t
  val neq : value -> t
  val slt : value -> t
  val sleq : value -> t
  val sgeq : value -> t
  val sgt : value -> t
  val ult : value -> t
  val uleq : value -> t
  val ugeq : value -> t
  val ugt : value -> t

  exception Undefined_symbol of string

  (** Evaluate a predicate. May throw {! Undefined_symbol}. *)
  val eval : symbols:(Z.t Datatype_sig.StringMap.t) -> self:Z.t -> t -> bool

  (** Conjoint two predicate. Ignores true (void) predicates *)
  val conjunction : t -> t -> t
end

(* Size in bytes * name. TODO: rename to "word", as in the formalisation. *)
type basic = int * string
type enum = {
  en_name : string option;
  en_byte_size : int;
  en_values : (string * int) list;
}

type index_value =
  { idx : string ; (** Symbol to the abstraction of index in array *)
    fe : string ; (** Symbol to the abstraction of reverse index from the end, i.e.
                      [- size + idx]. *)
    ofs : int (** Offset in the pointed object *)
  }

type index =
  | ValidUnknown
  | PastEnd
  | Zero
  | Idx of index_value

val pp_index : Format.formatter -> index -> unit
val join_indices : index -> index -> index

type structure = {
  st_byte_size : int option;    (* Size of the structure *)
  st_members : (int * string * typ) list; (* offset, fieldname, and type of the field *)
}

and union = {
  un_byte_size : int option;    (* Size of the union *)
  un_types : (string * typ) list; (* typename, and type of the field *)
}

and pointer = { pointed : typ; index : index }

and descr =
  | Void
  | Base of basic
  | Structure of structure
  | Ptr of pointer
  | Enum of enum
  | Array of typ * value option
  (** Arguments: element type, and the number of elements, if statically
      known. *)
  | Function of typ * typ list
  | Name of string
  | Application of constr * (Pred.expr list)
  | Existential of typ * string * typ
  | Union of union
  | Weak of typ

and constr =
  | ConstrName of string
  (** Type and its parameters *)
  | Constructor of typ * (string list)

and typ =
  (** Descr is mutable because of weak types, which lazily resolve the type.  *)
  { mutable descr : descr;
    pred : Pred.t;
  }

val pp_descr : Format.formatter -> descr -> unit
val pp : Format.formatter -> typ -> unit

val pp_constr : Format.formatter -> constr -> unit

(* Only descr means: ignore the predicates.  *)
(* Do not use the _descr version. *)
(* val compare_descr : only_descr:bool -> descr -> descr -> int *)
val compare : only_descr:bool -> typ -> typ -> int

(* TODO: Remove the only_descr argument here. *)
val equal : only_descr:bool -> typ -> typ -> bool

(** equiv a b returns true if it can show that a is a subtype of b and b is a subtype a. *)
val equiv_descr: only_descr:bool -> descr -> descr -> bool [@@@ocaml.deprecated "Please use equiv only."]
val equiv: only_descr:bool -> typ -> typ -> bool

val is_pointer_type : typ -> bool

(** [contains t u] determines whether t contains u. (Definition: t contains u
    iff t = u or a component of t (e.g. a structure member) contains u. *)
val contains : typ -> typ -> bool
(*
(** [contains_after_offset ofs t u] determines whether t contains u at an
    offset greater or equal to [ofs]. *)
val contains_after_offset : int -> typ -> typ -> bool
*)

exception Unsizeable_type

(** Returns the size of the type (in bytes), if possible *)
val sizeof: typ -> int

(** These functions allow to update or query the binding from type names to types. *)

(** Maps a name to a type. *)
val add_type_name_definition: string -> typ -> unit

exception Undefined_type of string

(** Retrieve the type associated to a name. raise [Undefined_type] if not found. *)
val type_of_name: string -> typ [@@@ocaml.deprecated "Please use inline instead."]

val pp_ctype : out_channel -> typ -> unit
val pp_ctype_list : out_channel -> typ list -> unit
val pp_function_type : out_channel -> string -> unit

val get_type_definitions : unit -> string list

(** Dump the type map, for debug purposes. *)
val print_type_map: unit -> unit

(** Unroll the definitions of a type (i.e. replace a name with the corresponding type) *)
val inlined: typ -> typ 

(** Maps a name to a type constructor. *)
val add_constr_name_definition: string -> constr -> unit

(** Retrieve the type constructor associated to a name. raise [Undefined_type] if not found. *)
val constr_of_name: string -> constr

(** Dump the constructor map, for debug purposes. *)
val print_constr_map: unit -> unit

(** Unroll the definitions of a constructor (i.e. replace a name with the corresponding constructor) *)
val inlined_constr: constr -> (typ * string list)

(** Applies a constructor by substituting parameters in a constructor by some arguments.
    raise [Application_error] if too many or not enough arguments *)
val apply: constr -> Pred.expr list -> typ

val substitute_symbol: typ -> string -> string -> typ

val add_function_name_definition: string -> typ -> unit

(** Retrieve the type associated to a function name. raise [Undefined_type] if not found. *)
val function_of_name: string -> typ

(** Dump the function map, for debug purposes. *)
val print_function_map: unit -> unit

(** Common base types.  *)
val char: typ
val short: typ
val int: typ
val long: typ
val long_long: typ
val word: byte_size:int -> typ

(* Tuple is an anonymous structure. *)
val tuple: typ list -> typ

(** Additionnal functions used for type domains *)
val concat: size1:int -> typ -> size2:int -> typ -> typ

val extract: size:int -> index:int -> oldsize:int -> typ -> typ option

(* TODO: Should probably go else where. *)
module type Type_settings = sig
  (** Root type of the parameters, and symbol to find it in the ELF file. *)
  val root : string * typ

  (** List of data addresses that should be considered to be an extension of
      parameter memory. *)
  val special_data_addrs : (int * (typ * int)) list

  (** List of every type [t] such that there exists only one array of [t]s in
      the parameters. *)
  val unique_array_types : typ list

  (** An attempt to store to a pointer to this type should raise an alarm. *)
  val non_modifiable_types : typ list

  (** List of global symbolic constants, with their sizes in bits and
      predicates that hold on them. *)
  val global_symbols : (string * int * Pred.t) list
end
