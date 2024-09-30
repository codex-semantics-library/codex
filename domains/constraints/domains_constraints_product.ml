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

module Make
    (Constraints:Constraints.Constraints_sig.Constraints)
    (A:Constraint_domains_sig.Domain_S with module Constraints = Constraints)
    (B:Constraint_domains_sig.Domain_S with module Constraints = Constraints):
  Constraint_domains_sig.Domain_S
  with module Constraints = Constraints = struct


  let name = "Product_Domain(" ^ A.name ^ "," ^ B.name ^ ")"

  module Constraints = Constraints;;
  module TC = Transfer_functions.Term;;


  type binary = TC.binary Constraints.t
  type integer = TC.integer Constraints.t
  type boolean = TC.boolean Constraints.t


  type t = A.t * B.t

  let pretty fmt (a,b) = Format.fprintf fmt "<%a,%a>" A.pretty a B.pretty b
  let equal (a1,b1) (a2,b2) = (A.equal a1 a2 && B.equal b1 b2)

  let top = (A.top,B.top)

  (* let join (a1,b1) (a2,b2) = (A.join a1 a2,B.join b1 b2) *)

  module Query = struct
    let boolean (a,b) x =
      Lattices.Quadrivalent.inter (A.Query.boolean a x) (B.Query.boolean b x)
    let binary ~size _ = assert false
    let integer (a,b) x = A.Query.integer a x, B.Query.integer b x
    let convert_to_ival (a,b)  = (assert false)(* Ival_basis.Integer_Lattice.inter *) (A.Query.convert_to_ival a) (B.Query.convert_to_ival b)
    let convert_to_quadrivalent x = x
    let binary_to_ival ~signed ~size _ = assert false
    let binary_to_known_bits ~size _ = assert false
      (* (a,b) =
       * Single_value_abstraction.Known_bits.inter ~size
       *   (A.Query.binary_to_known_bits ~size a) (B.Query.binary_to_known_bits ~size b)       *)
    let binary_fold_crop ~size bin ~inf ~sup f acc =
      (* Need to build an intermediary version with sets of integers. *)
      assert false
    let is_singleton_int (a,b) =
      match A.Query.is_singleton_int a with
      | None -> B.Query.is_singleton_int b
      | x -> x
    ;;

    let binary_is_empty ~size _ = assert false
    let binary_is_singleton ~size _ = assert false(* (a,b) =
     *   match A.Query.binary_is_singleton ~size a with
     *   | None -> B.Query.binary_is_singleton ~size b
     *   | x -> x
     * ;; *)


    module Boolean_Lattice = Lattices.Quadrivalent
    module Integer_Lattice = struct
      include Lattices.Prod.Prod2_With_Inter_Bottom(A.Query.Integer_Lattice)(B.Query.Integer_Lattice)
      let top = (A.Query.Integer_Lattice.top,B.Query.Integer_Lattice.top)
      let singleton x = A.Query.Integer_Lattice.singleton x, B.Query.Integer_Lattice.singleton x
    end
    (* module Integer_Lattice = Lattices.Dummy *)
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


  let boolean_pretty (a,b) fmt x =
    Format.fprintf fmt "<%a,%a>" (A.boolean_pretty a) x (B.boolean_pretty b) x
  ;;

  let integer_pretty (a,b) fmt x =
    Format.fprintf fmt "<%a,%a>" (A.integer_pretty a) x (B.integer_pretty b) x
  ;;

  let binary_pretty ~size (a,b) fmt x =
    Format.fprintf fmt "<%a,%a>" (A.binary_pretty ~size a) x (B.binary_pretty ~size b) x
  ;;


  let nondet ~doma:(a1,a2) ~tupa ~domb:(b1,b2) ~tupb ~tupres =
    A.nondet ~doma:a1 ~tupa ~domb:b1 ~tupb ~tupres,
    B.nondet ~doma:a2 ~tupa ~domb:b2 ~tupb ~tupres
  ;;

  let widened_fixpoint_step
      ~previous:(previousa,previousb) ~previous_tup
      ~next:(nexta,nextb) ~next_tup
      bool ~res_tup =
    let doma,bool =
      A.widened_fixpoint_step ~previous:previousa ~previous_tup ~next:nexta ~next_tup bool ~res_tup in
    let domb,bool =
      B.widened_fixpoint_step ~previous:previousb ~previous_tup ~next:nextb ~next_tup bool ~res_tup in
    (doma,domb), bool

  let fixpoint_open() = begin
    A.fixpoint_open();
    B.fixpoint_open()
  end

  module Sum = struct
    type ('a,'b) t =
      | Integer of 'a
      | Boolean of 'b
  end;;


  let fixpoint_step ~lvl ~iteration (actual_doma,actual_domb) ~actuals (arg_doma,arg_domb) ~args (final_doma,final_domb) ~finals =
    let resa,conta = A.fixpoint_step ~lvl ~iteration actual_doma ~actuals arg_doma ~args final_doma ~finals in
    let resb,contb = B.fixpoint_step ~lvl ~iteration actual_domb ~actuals arg_domb ~args final_domb ~finals in
    resa && resb, (fun ~close constres ->
        (* Codex_log.feedback "################ %b %b %b" resa resb close; *)
        conta ~close constres, contb ~close constres)
  ;;


  (**************** Forward propagation. ****************)

  module Domain_Arity = struct
    type 'r ar0 = t -> 'r -> t
    type ('a,'r) ar1 = t -> 'a -> 'r -> t
    type ('a,'b,'r) ar2 = t -> 'a -> 'b -> 'r -> t
    type ('a,'b,'c,'r) ar3 = t -> 'a -> 'b -> 'c -> 'r -> t
    type ('a,'r) variadic = t -> 'a list -> t
  end


  let ar0 af bf = fun (a,b) constr -> af a constr, bf b constr
  let ar1 af bf = fun (a,b) arg res -> af a arg res, bf b arg res
  let ar2 af bf = fun (a,b) arg1 arg2 res -> af a arg1 arg2 res, bf b arg1 arg2 res

  module Boolean_Forward = struct
    let true_ = ar0 A.Boolean_Forward.true_ B.Boolean_Forward.true_
    let false_ = ar0 A.Boolean_Forward.false_ B.Boolean_Forward.false_
    let (||) = ar2 A.Boolean_Forward.(||) B.Boolean_Forward.(||)
    let (&&) = ar2 A.Boolean_Forward.(&&) B.Boolean_Forward.(&&)
    let (not) = ar1 A.Boolean_Forward.(not) B.Boolean_Forward.(not)
    let assume _ = assert false
  end

  module Integer_Forward = struct
    let ile = ar2 A.Integer_Forward.ile B.Integer_Forward.ile
    let ieq = ar2 A.Integer_Forward.ieq B.Integer_Forward.ieq
    let iconst k = ar0 (A.Integer_Forward.iconst k) (B.Integer_Forward.iconst k)
    let one  = iconst Z.one
    let zero = iconst Z.zero
    let assume _ = assert false
    let ixor = ar2 A.Integer_Forward.ixor B.Integer_Forward.ixor
    let ior  = ar2 A.Integer_Forward.ior  B.Integer_Forward.ior
    let iand = ar2 A.Integer_Forward.iand B.Integer_Forward.iand
    let ishr = ar2 A.Integer_Forward.ishr B.Integer_Forward.ishr
    let ishl = ar2 A.Integer_Forward.ishl B.Integer_Forward.ishl
    let imod = ar2 A.Integer_Forward.imod B.Integer_Forward.imod
    let idiv = ar2 A.Integer_Forward.idiv B.Integer_Forward.idiv
    let imul = ar2 A.Integer_Forward.imul B.Integer_Forward.imul
    let iadd = ar2 A.Integer_Forward.iadd B.Integer_Forward.iadd
    let isub = ar2 A.Integer_Forward.isub B.Integer_Forward.isub
    let itimes k = ar1 (A.Integer_Forward.itimes k) (B.Integer_Forward.itimes k)
  end

  module Binary_Forward = struct
    include Assert_False_Transfer_Functions.Binary.Binary_Forward
  end


  let binary_empty ~size = ar0 (A.binary_empty ~size) (B.binary_empty ~size)
  let integer_empty = ar0 A.integer_empty B.integer_empty
  let boolean_empty = ar0 A.boolean_empty B.boolean_empty

  let binary_unknown ~size = ar0 (A.binary_unknown ~size) (B.binary_unknown ~size)
  let integer_unknown = ar0 A.integer_unknown B.integer_unknown
  let boolean_unknown = ar0 A.boolean_unknown B.boolean_unknown


  let assume (a,b) cond =
    match A.assume a cond, B.assume b cond with
    | None, _ | _, None -> None
    | Some a, Some b -> Some(a,b)
  ;;


end
