open Union_Find.Parameters
open Transfer_functions.Term

(** {1 Group Action} *)

(** Computes the action of a group of relation on some numeic values *)
module type GROUP_ACTION = sig
  type binary
  type integer
  type boolean
  type ('a, 'b) relation

  (** One-to-one correspodance between value types and term types *)
  type (_, _) mapping =
    | BinaryMapping: (binary,  Transfer_functions.Term.binary) mapping
    | IntegerMapping: (integer, Transfer_functions.Term.integer) mapping
    | BooleanMapping: (boolean, Transfer_functions.Term.boolean) mapping

  (** Existential wrapper for mappings *)
  type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

  (** [apply_relation vp rel proof] is the value [vc] obtained by applying
      relation [rel] to value [vp]. This is a simple forward transfer function.

      The {!proof} terms are there to ensure correspondance between the types
      of values ({!B.binary}, {!B.boolean}, {!B.integer}) and
      of terms ({!TC.binary}, {!TC.boolean}, {!TC.integer}) *)
    val apply_relation:
      'value_parent ->
      ('term_child, 'term_parent) relation ->
      ('value_parent, 'term_parent) mapping ->
      'term_child wrapper

    (** [refine_relation ~size vp vc rel proof_p proof_c] refines the value [vp]
        into, using knowledge that applying [rel] to [vp] must be in [vc].
        This is a simple backward transfer function. [None] is returned when
        no new information can be deduced.

        {b Note:} this returns the newly learned value. It should be
        intersected with [vp] for best precision.

        The {!proof} terms are there to ensure correspondance between the types
        of values ({!B.binary}, {!B.boolean}, {!B.integer}) and
        of terms ({!TC.binary}, {!TC.boolean}, {!TC.integer}) *)
    val refine_relation:
      'value_parent ->
      'value_child ->
      ('term_child, 'term_parent) relation ->
      ('value_parent, 'term_parent) mapping ->
      ('value_child, 'term_child) mapping ->
      'value_parent option
end

(** {1 Some implementations} *)

(** The simplest relation: strict equality [y = x] between variables *)
module Equality: sig
  type (_, _) t = Equal: ('a, 'a) t
  (** Simplest possible relation: equality (singleton group) *)

  include Union_Find.Parameters.GENERIC_GROUP with type ('a, 'b) t := ('a, 'b) t
  (** Group operation (compose, inverse, identity, equal) and printer *)

  module Action(B: sig type binary type integer type boolean end):
    GROUP_ACTION
    with type binary = B.binary
      and type integer = B.integer
      and type boolean = B.boolean
      and type ('a, 'b) relation = ('a, 'b) t
end

(** Simple additive relation: [y = delta*x + b] where [delta] is +/- 1 (indicated
    by a boolean with true -> positive)
    - On {!integer} is is the usual unbounded addition
    - On {!binary} it is modulo addition
    - On {!boolean} is is simply negation *)
module Additive: sig
  type delta = PlusOne | MinusOne

  type (_, _) t = private
    | Identity: ('a, 'a) t
    | Add_Modulo: { factor: delta; size: int; amount: Z.t } -> (binary, binary) t
        (** Invariant: is not identity
            either [factor] is [MinusOne] or [amount] is non-zero (modulo 2^size)
            [amount] is in [-2^(size-1) .. 2^(size-1)-1] *)
    | Add_Unbounded: delta * Z.t -> (integer, integer) t
        (** Invariant: is not identity
            either the [delta] is [MinusOne] (negation) or the value is non-zero *)
    | Bool_Not: (boolean, boolean) t
  (** @see {!Additive} for description of this relation

      The type is made private to ensure invariant are respected,
      use the constructors below to build terms *)

  val additive_identity: ('a, 'a) t
  val additive_binary: size:int -> delta -> Z.t -> (binary, binary) t
  val additive_integer: delta -> Z.t -> (integer, integer) t
  val boolean_not: (boolean, boolean) t

  include GENERIC_GROUP with type ('a, 'b) t := ('a, 'b) t

  module Action(B: Single_value_abstraction.Sig.Numeric_Basis):
    GROUP_ACTION
    with type binary = B.binary
      and type integer = B.integer
      and type boolean = B.boolean
      and type ('a, 'b) relation = ('a, 'b) t
end

module XOR_Rotate: sig
  type (_, _) t = private
    | XR_Identity: ('a, 'a) t
    | XR_BNot: (boolean, boolean) t
      (** Boolean not *)
    | XR_XOR_rotate: { rotate: int; xor: Z.t; size: int } -> (binary, binary) t
      (** On bounded integers, represents a rotation then a xor
          Invariant: [xor] fits on [size] bits, [0 <= rotate < size] *)
  (** Bitwise focused relation: xor and rotate. *)

  val xr_identity: ('a, 'a) t
  val xr_bnot: (boolean, boolean) t
  val xr_xor_rotate: rotate:int -> xor:Z.t -> size:int -> (binary, binary) t

  include GENERIC_GROUP with type ('a, 'b) t := ('a, 'b) t
end
