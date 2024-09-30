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

(* Dummy domain; start here for a new implementation. *)
module Make
    (Constraints:Constraints.Constraints_sig.Constraints):
  Constraint_domains_sig.Domain_S
  with module Constraints = Constraints = struct


  let name = "Assert_False_Domain"

  module Constraints = Constraints;;
  module TC = Transfer_functions.Term;;


  type binary = TC.binary Constraints.t
  type integer = TC.integer Constraints.t
  type boolean = TC.boolean Constraints.t


  type t = unit

  let pretty fmt t = assert false
  let equal = (==);;

  let top = ()

  let inter a b = assert false

  let join a b = assert false

  (* We do not know the constraints. *)
  let to_constraint _ = assert false;;

  module Query = struct
    let boolean _ = assert false
    let binary ~size _ = assert false
    let integer _ = assert false
    let convert_to_ival _ = assert false
    let convert_to_quadrivalent _ = assert false
    let binary_to_ival ~signed ~size _ = assert false
    let binary_to_known_bits ~size _ = assert false
    let binary_is_empty ~size _ = assert false
    let binary_fold_crop ~size bin ~inf ~sup f acc = assert false
    let is_singleton_int _ = assert false
    let binary_is_singleton ~size _ = assert false
    module Boolean_Lattice = Lattices.Quadrivalent
    module Integer_Lattice = Lattices.Unit
    module Binary_Lattice = struct
      include Lattices.Unit
      let includes ~size = assert false
      let is_bottom ~size = assert false
      let bottom ~size = assert false
      let top ~size = assert false
      let inter ~size = assert false
      let join ~size = assert false
      let pretty ~size = assert false
      let widen ~size ~previous _ = assert false
      let includes_or_widen ~size ~previous _ = assert false
      let singleton ~size _ = assert false
    end
  end


  let boolean_pretty dom fmt x = assert false;;

  let integer_pretty dom fmt x = assert false;;

  let binary_pretty ~size dom fmt x = assert false;;

  let nondet ~doma ~tupa ~domb ~tupb ~tupres = assert false;;

  let widened_fixpoint_step ~previous ~previous_tup ~next ~next_tup _bool ~res_tup = assert false

  let fixpoint_open() = ()

  module Sum = struct
    type ('a,'b) t =
      | Integer of 'a
      | Boolean of 'b
  end;;


  let fixpoint_step ~lvl ~iteration actual_dom ~actuals arg_dom ~args final_dom ~finals = assert false
  ;;


  (**************** Forward propagation. ****************)

  module Domain_Arity = struct
    type 'r ar0 = t -> 'r -> t
    type ('a,'r) ar1 = t -> 'a -> 'r -> t
    type ('a,'b,'r) ar2 = t -> 'a -> 'b -> 'r -> t
    type ('a,'b,'c,'r) ar3 = t -> 'a -> 'b -> 'c -> 'r -> t
    type ('a,'r) variadic = t -> 'a list -> t
  end


  module Boolean_Forward = struct
    include Assert_False_Transfer_Functions.Boolean.Boolean_Forward;;

    (* let (||) = ar2_boolean_boolean_boolean B.Boolean_Forward.(||)
     * let (&&) = ar2_boolean_boolean_boolean B.Boolean_Forward.(&&)
     * let (not) = ar1_boolean_boolean B.Boolean_Forward.not
     * let assume _ = assert false
     * let true_ = ar0_boolean B.Boolean_Forward.true_
     * let false_  = ar0_boolean B.Boolean_Forward.false_
     * let unknown ?level = ar0_boolean (B.Boolean_Forward.unknown ?level) *)
  end

  module Integer_Forward = struct
    include Assert_False_Transfer_Functions.Integer.Integer_Forward
    (* let ile = ar2_integer_integer_boolean B.Integer_Forward.ile
     * let ieq = ar2_integer_integer_boolean B.Integer_Forward.ieq
     * let iconst k = ar0_integer (B.Integer_Forward.iconst k)
     * let one  = iconst Z.one
     * let zero = iconst Z.zero
     *
     * let assume _ = assert false
     * let iunknown () = ar0_integer (B.Integer_Forward.iunknown ())
     * let ixor = ar2_integer_integer_integer B.Integer_Forward.ixor
     * let ior  = ar2_integer_integer_integer B.Integer_Forward.ior
     * let iand = ar2_integer_integer_integer B.Integer_Forward.iand
     * let ishr = ar2_integer_integer_integer B.Integer_Forward.ishr
     * let ishl = ar2_integer_integer_integer B.Integer_Forward.ishl
     * let imod = ar2_integer_integer_integer B.Integer_Forward.imod
     * let idiv = ar2_integer_integer_integer B.Integer_Forward.idiv
     * let imul = ar2_integer_integer_integer B.Integer_Forward.imul
     * let iadd = ar2_integer_integer_integer B.Integer_Forward.iadd
     * let isub = ar2_integer_integer_integer B.Integer_Forward.isub
     * let itimes k = ar1_integer_integer (B.Integer_Forward.itimes k) *)
  end

  module Binary_Forward = struct
    include Assert_False_Transfer_Functions.Binary.Binary_Forward
  end




  (* let integer_empty = ar0_integer B.Integer_Lattice.bottom
   * let boolean_empty = ar0_boolean B.Boolean_Lattice.bottom *)

  let binary_empty ~size _ = assert false;;
  let integer_empty _ = assert false;;
  let boolean_empty _ = assert false;;

  let binary_unknown ~size _ = assert false;;
  let integer_unknown _ = assert false;;
  let boolean_unknown _ = assert false;;


  let assume dom cond = assert false;;

end
