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

(** TypedC: Grammar for describing the types for the type-based domain,
    and operations on concrete types. *)

open Units

(* TODO: Get rid of the dynamic add_symbols function by just allowing
    extension of what a value is (using extensible types). *)
type value_symbol = string

type value =
  | Sym of value_symbol

type array_length =
  | Fixed_length of Z.t
  | Variable_length of value_symbol

val pp_value : Format.formatter -> value -> unit
val pp_array_length : Format.formatter -> array_length -> unit

module Pred : sig
  type unop =
    | Extract of In_bits.t * In_bits.t (** start index, length *)

  type binop =
    | Add | Sub | Mul | Div
    | And | Or | Mod
    | Concat of In_bits.t * In_bits.t (** Size 1, size 2. First argument is the most significant *)

  type expr =
    | Const of Z.t
    | Val of value
    | Self
    | Unop of unop * expr
    | Binop of binop * expr * expr

  type cmpop = Equal | NotEqual | ULt | SLt | ULeq | SLeq | UGeq | SGeq | UGt | SGt

  (*mutable flag used to update predicates, use ONLY WHEN YOU KNOW WHAT YOU ARE DOING*)
  type mutval = {id : int; mutable value : bool}

  (** Predicate on a structure field or array element, which is supposed to be
      true at all times. *)
  type t =
    | True
    | Cmp of cmpop * expr * expr
    | And of t * t
    | Mutval of mutval * t


  (**given a mutval, evaluate the value of the associated predicate*)
  val evaluate_mutval : mutval -> t -> t
  val init_mutval : t -> t

  (**check if a predicate can be immediately reduced to true, this is not trivial since mutval exists*)
  val is_true : t -> bool

  (**returns the true predicate*)
  val true_ : t

  val equal : t -> t -> bool
  val pp_binop : Format.formatter -> binop -> unit
  val pp_cmpop : Format.formatter -> cmpop -> unit
  val pp : Format.formatter -> t -> unit

  val array_length_to_expr : array_length -> expr

  (** Conjoint two predicate. Ignores true (void) predicates *)
  val conjunction : t -> t -> t
end

(* Size in bytes * name. TODO: rename to "word", as in the formalisation. *)
type basic = In_bytes.t * string

type structure = {
  st_byte_size : In_bytes.t;    (* Size of the structure *)
  st_members : (In_bytes.t * string * typ) list; (* offset, fieldname, and type of the field *)
}

and union = {
  un_byte_size : In_bytes.t option;    (* Size of the union *)
  un_types : (string * typ) list; (* typename, and type of the field *)
}

and descr =
  | Void

  | Base of basic
  (** Base types. Note that even if they have a name, they are not a definition (e.g., you cannot have
      pointers to byte. *)

  | Structure of structure

  | StructureFAM of {structure:typ; array:typ}
  (** Structure with flexible array member, decomposed as fix prefix * array. *)

  | Ptr of pointer
  (** Todo: we should always have a size (possibly an existentially-qualified variable). *)
  | Array of typ * array_length
  (** Arguments: element type, and the number of elements, if statically
      known. *)
  | Function of funtyp
  (* TODO: Just use higher-order abstract syntax for substitution.
     Also, it makes more sense to use arrays instead of lists here. *)
  | Application of name
  (* TODO: Also use HOAS: Existential of typ * (value -> typ). *)
  (** ∃ bound_var : bound_typ, inner*)
  | Existential of {bound_var:string;bound_typ:typ;body:typ}
  | Union of union
  (* TODO: This should be weak of typ option. *)
  | Weak of typ

(* TODO: We should be pointing to a constructor name (and its arguments). *)
(* aka as a name*)
and pointer = { pointed : typ }
(* A name is a constructor called with matching arguments.
   Invariant: the arity of the constructor matches the length of the list. *)
and name = { constr : constr; args : Pred.expr list }

(** Called type name in OOPSLA, but is the name of a family of region
    names when there are arguments. *)
and constr_name =
  | ConstrName of string
  | ConstrNameFunc of string
  | ConstrNameUnion of string
  | ConstrNameStruct of string
  | ConstrNameEnum of string
  | ConstrNameArray of constr_name (* Arity 1. Printed as name[a]* instead of name[](a)* *)
  (* Maybe:ConstrNameStar *)

(** Each [constr_name] is associated to a unique id and arity to give a [constr].  *)
and constr = private {id: int; name : constr_name; arity: int }

and funtyp = { ret : typ ; args : typ list ; pure : bool}

(* TODO: WE should just have refinement type, no need to put a predicate everywhere. *)
and typ =
  (* Descr and pred are mutable because of weak types, which lazily resolve the type.  *)
  { mutable descr : descr;
    mutable pred : Pred.t;
  }

type fundef = { funtyp : typ; inline : bool }

val pp : Format.formatter -> typ -> unit

val pp_constr : Format.formatter -> constr -> unit

(* Only descr means: ignore the predicates.  *)
(* Do not use the _descr version. *)
val compare : typ -> typ -> int

(* TODO: Remove the only_descr argument here. *)
val equal : typ -> typ -> bool

(** equiv a b returns true if it can show that a is a subtype of b and b is a subtype a. *)
val equiv: typ -> typ -> bool


module Constr:sig
  type t = constr

  (** [make name arity] create a constructor with given name and arity. *)
  val make: constr_name -> int -> constr
  val pp: Format.formatter -> constr -> unit
  val equal: constr -> constr -> bool
  val hash: constr -> int
  val to_string: constr -> string
end


module Build:sig

  (** Build a pointer type. Deprecated: use ptr_to_name instead. *)
  val ptr: typ -> Pred.t -> typ

  (** Build a pointer type. *)
  val ptr_to_name: name -> typ

  (** Refinement type: t with pred. *)
  val refine: typ -> Pred.t -> typ
end


val is_flexible_array : typ -> bool

val is_function_pure: typ -> bool

(** [contains t u] determines whether t contains u. (Definition: t contains u
    iff t = u or a component of t (e.g. a structure member) contains u. *)
val contains : typ -> typ -> bool

exception Unsizeable_type

(** Returns the size of the type (in bytes), if possible *)
val sizeof: typ -> In_bytes.t

(** These functions allow to update or query the binding from type names to types. *)
exception Undefined_type_constructor of string


val get_type_definitions : unit -> string list

(** Unroll the definitions of a type (i.e. replace a name with the corresponding type) *)
val inlined: typ -> typ

(** Substitutes the symbolic variables in the type [typ] by the corresponding
    expressions in [bindings] *)
val substitute_in_type: typ -> (value_symbol * Pred.expr) list -> typ

val substitute_symbol: typ -> string -> string -> typ

val word: byte_size:In_bytes.t -> typ

(* Tuple is an anonymous structure. *)
val tuple: typ list -> typ

(** Additionnal functions used for type domains *)
(* val concat: size1:int -> size2:int -> typ -> typ -> typ *)

(* val extract: size:int -> index:int -> oldsize:int -> typ -> typ option *)


(* Mapping from strings to typ, where strings should be substituted.  *)
type constr_def = typ * string list

(** Maps a name to a type constructor. *)
val add_constr_definition: constr -> constr_def -> unit

(** find the [constr_def] associated to a [constr], or [None] if none. *)
val constr_of_name : constr -> constr_def option

(** print the constructor map, for debug purposes. *)
val print_constr_map: Format.formatter -> unit -> unit

(**print all the stored maps, for debug purposes*)
val print_spec : Format.formatter -> unit -> unit

val add_function_name_definition: string -> typ -> bool -> unit

(** Retrieve the type associated to a function name, or None if there is none. *)
val function_of_name: string -> typ option

(** Retrieve the type associated to a function name, and whether it
    was declared inline; or [None] if not found. *)
val function_definition_of_name: string -> fundef option

(** print the function map, for debug purposes. *)
val print_function_map: Format.formatter -> unit -> unit

val get_function_names : unit -> string list

val add_global_name_definition: string -> typ -> unit

(** Retrieve the type associated to a function name, or [None] if not found. *)
val global_of_name: string -> typ option

(** print the function map, for debug purposes. *)
val print_global_map: Format.formatter -> unit -> unit
