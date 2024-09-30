
module type IMPERATIVE_GENERIC_RELATIONAL = sig
  type 'a t
  type ('a, 'b) relation

  (** Existential wrapper for returning the representative *)
  type 'a node_through_relation =
    | NodeThoughRelation : 'b t * ('a, 'b) relation -> 'a node_through_relation

  val find_representative : 'a t -> 'a node_through_relation
  (** Find the representative of a node, and the associated relation *)

  val check_related : 'a t -> 'b t -> ('a, 'b) relation option
  (** [check_related a b] returns the relation between [a] and [b]
      if they are in the same class. *)

  val union : 'a t -> 'b t -> ('a, 'b) relation -> (unit, ('a, 'b) relation) result
  (** [union m n r] adds the [m--(r)-->n] relation to the union find
      returns [Ok ()] on success, [Error rel] if [m] and [n] are already related
      via another relation [rel] and [r != rel]. *)
end

module type IMPERATIVE_GENERIC_RELATIONAL_VALUED = sig
  include IMPERATIVE_GENERIC_RELATIONAL

  type 'a value

  (** Existential wrapper for returning the value *)
  type 'a value_through_relation =
    ValueThroughRelation : 'b value * ('a, 'b) relation -> 'a value_through_relation

  (** Existential wrapper for returning the value and the representative *)
  type 'a node_and_value_through_relation =
    NodeValueThroughRelation : 'b t * 'b value * ('a, 'b) relation
        -> 'a node_and_value_through_relation

  (** {3 Find operations} *)

  val find_value : 'a t -> 'a value_through_relation
  (** Find the value of a node, and the associated relation *)

  val find : 'a t -> 'a node_and_value_through_relation
  (** Find the value and representative of a node, and the associated relation *)

  (** {3 Other misc operations} *)

  val add_value : 'a t -> 'a value -> unit
  (** [add_value a v] adds with the value [v] added to [a]
      (Or more precisely, the value is added to the representative of [a],
      via the relation between [a] and its representative).
      Intersects with previous value via [Value.meet] if one is present *)
end
