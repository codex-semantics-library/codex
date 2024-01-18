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

(* Extends datatype: elements now have a lattice structure. *)
module type Join_Semi_Lattice = sig
  include Datatype_sig.S
  val join: t -> t -> t

  (* In a fixpoint iteration of a function F, [previous] is the
     previous element of the fixpoint iteration sequence. The other
     element is the newly computed (tentative) element,
     i.e. F(previous).

     If the new element is included in [previous], [true] is returned,
     together with [new] (the smaller element): the post-fixpoint of F
     has been found (further calls to F can decrease the sequence).

     Else, the returned element should be used as the next element of
     the fixpoint iteration sequence; the operator guarantees its
     convergence. 

     For lattices of finite height, the widening part can just perform
     an over-approximation of the join; however note that it is not
     required that the sequence is monotonic. *)
  val includes_or_widen: previous:t -> t -> (bool * t)

  val includes: t -> t -> bool
  val widen: previous:t -> t -> t
end

(* Optional, standard, operations on lattices. *)

(* Over-approximation of intersection. *)
module type With_Inter = sig
  type t
  val inter: t -> t -> t
end

module type With_Bottom = sig
  type t
  val bottom: t                 (* One of the bottoms. *)
  val is_bottom: t -> bool      (* Can be imprecise; e.g. always returning false is correct. *)
end

module type With_Top = sig
  type t
  val top: t                 (* One of the tops. *)
end


module type Join_Semi_Lattice_With_Bottom = sig
  include Join_Semi_Lattice;;
  include With_Bottom with type t:= t
end

module type Join_Semi_Lattice_With_Inter_Bottom = sig
  include Join_Semi_Lattice;;
  include With_Inter with type t:= t
  include With_Bottom with type t:= t
end

module type Complete_Lattice = sig
  include Join_Semi_Lattice;;
  include With_Inter with type t:= t
  include With_Bottom with type t:= t
  include With_Top with type t:=t
end

(* Many Integer_Lattices do not have a top, as they cannot represent
   an infinite set. *)
module type Integer_Lattice = sig
  include Join_Semi_Lattice;;
  include With_Inter with type t:= t
  include With_Bottom with type t:= t
  val singleton: Z.t -> t
end

(* This helps communicating information between lattices. *)
module type Bitvector_Standard_Conversions = sig
  type t
  (* Export information. *)
  val to_unsigned_interval: size:int -> t -> Bitvector_standard.Unsigned_Interval.t
  val to_signed_interval: size:int -> t -> Bitvector_standard.Signed_Interval.t
  val to_congruence: size:int -> t -> Bitvector_standard.Congruence.t
  val to_known_bits: size:int -> t -> Bitvector_standard.Known_Bits.t
  val to_bvset: size:int -> t -> Bitvector_standard.BVSet.t

  (* Import information. *)
  val inter_unsigned_interval: size:int -> t -> Bitvector_standard.Unsigned_Interval.t -> t
  val inter_signed_interval: size:int -> t -> Bitvector_standard.Signed_Interval.t -> t
  val inter_congruence: size:int -> t -> Bitvector_standard.Congruence.t -> t
  val inter_known_bits: size:int -> t -> Bitvector_standard.Known_Bits.t -> t
  val inter_bvset: size:int -> t -> Bitvector_standard.BVSet.t -> t
end

(* Bitvector Lattice are Complete Lattice, but we don't store the size
   in them; thus they are special because they require to be passed
   the size for all their operations. *)
module type Bitvector_Lattice_No_Conversion = sig
  include Datatype_sig.S
  val bottom: size:int -> t
  val is_bottom: size:int -> t -> bool    
  val top: size:int -> t
  val inter: size: int -> t -> t -> t
  val join: size: int -> t -> t -> t    
  val pretty: size:int -> Format.formatter -> t -> unit
  val widen: size:int -> previous:t -> t -> t    
  val includes: size:int -> t -> t -> bool
  val includes_or_widen: size:int -> previous:t -> t -> (bool * t)    
  val singleton: size:int -> Z.t -> t


end

module type Bitvector_Lattice = sig
  include Bitvector_Lattice_No_Conversion
  (* include Bitvector_Standard_Conversions *)
end
