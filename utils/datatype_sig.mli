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

(** Generic operations that must be provided by every data type. *)
module type S = sig
  type t

  (** Any notion of equality is allowed, as long as it is an
      equivalence relation, and that if [a == b], then [equal a b]. *)
  val equal: t -> t -> bool

  (** [compare] is a total order, and should be compatible with equal. *)
  val compare: t -> t -> int

  (** [hash] requires that equal values have the same hash. *)
  val hash: t -> int
  val pretty: Format.formatter -> t -> unit
end

(** If you do not want to define all datatype operations
   immediately. *)
module Undefined(Name:sig val name:string end):sig
  val equal: 'a -> 'a -> bool
  val compare: 'a -> 'a -> int
  val hash: 'a -> int
  val pretty: Format.formatter -> 'a -> unit
end



(** {1 Generic products and sums of bases}
    You an use this with conversion from records and sum types.               *)
(******************************************************************************)

module Conv(B1:S)(C:sig type t val conv: t -> B1.t end):S with type t = C.t
(** When a type [t] can be converted (ideally with an injection to [B1.t]),
    we can define its oprations in terms of those of [B1] *)

module Prod2(B1:S)(B2:S):S with type t = B1.t * B2.t
module Prod3(B1:S)(B2:S)(B3:S):S with type t = B1.t * B2.t * B3.t

module Sum2(BA:S)(BB:S):S with type t = (BA.t,BB.t) Either.t

(** Three constructor version of [Either] *)
type ('a,'b,'c) sum3 = Sum3A of 'a | Sum3B of 'b | Sum3C of 'c
module Sum3(BA:S)(BB:S)(BC:S):S with type t = (BA.t,BB.t,BC.t) sum3


(** {1 Usual types and type operators}                                        *)
(******************************************************************************)

module Int: S with type t = int
module Unit: S with type t = unit
module String: S with type t = string

module Option(B:S):sig
  include S with type t = B.t option
  val the: B.t option -> B.t
end

module List(B:S):sig
  include module type of List
  include S with type t = B.t list
end

module Set(B:S):sig
  open Extstdlib
  include Set.S with type elt = B.t
  include S with type t := t
end

module Map(B:S):sig
  open Extstdlib
  include Map.S with type key = B.t
  module With_Value(V:S):S with type t = V.t t
end

module Hashtbl(B:S):sig
  include Hashtbl.S with type key = B.t
  module With_Value(V:S):S with type t = V.t t
end

module StringMap : module type of Map(String)
module StringHash : module type of Hashtbl(String)
