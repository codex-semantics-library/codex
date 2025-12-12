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

(* Note: the idea here is that we put the wraps and wrapu lazily. Only
   when some operations require a pre-wrap, like division and
   comparison (all the signed operations). addition, subtraction,
   maybe multiply do not.

*)


(* TODO: A version where we add wraps and wrapu operations
   manually. And somewhere (in the term domain?) remove those for
   which we prove that no wrapping occurs. *)

type size = int

module Make
    (I: Sig.BASE_WITH_INTEGER):Sig.BASE_WITH_INTEGER
  with type binary = I.integer
   and type boolean = I.boolean
   and type integer = I.integer
   and module Context = I.Context
   and module Integer_Query = I.Integer_Query
   (* and module Query.Binary_Lattice = I.Query.Integer_Lattice *)
= struct

  let name() = "Lift_integer_domain_to_binary_domain(" ^ (I.name()) ^ ")";;
  let unique_id() = Sig.Fresh_id.fresh @@ name();;

  (* open Term_types *)

  module Types = struct
    type integer = I.integer
    type binary = I.integer
    type boolean = I.boolean
    type enum = I.enum
  end

  include Types

  module Context = I.Context
  (*open Context*)
  (* include Operator.Builtin.Make(Types)(Context) *)

  let mu_context_open = I.mu_context_open

  let typed_nondet2 = I.typed_nondet2
  let nondet_same_context = I.nondet_same_context

  let root_context = I.root_context
  let context_pretty = I.context_pretty

  let serialize_integer = I.serialize_integer
  let serialize_boolean = I.serialize_boolean
  let serialize_enum = I.serialize_enum
  let serialize_binary ~widens ~size = I.serialize_integer ~widens
  (* let serialize_binary ~widens ~size ctxa a ctxb b acc = *)
  (*   let Result(included,acc,d) = I.serialize_integer ~widens ctxa a ctxb b acc in *)
  (*   Result(included,acc,fun ctx tup -> let res,tup = d ctx tup in res,tup) *)
  (* ;; *)

  let typed_fixpoint_step = I.typed_fixpoint_step
  let widened_fixpoint_step = I.widened_fixpoint_step

  let assume = I.assume

  module Boolean_Forward = I.Boolean_Forward
  module Integer_Forward = I.Integer_Forward
  module Enum_Forward = I.Enum_Forward

  (* Note: if we have nowrap signs, we want to put an alarm instead of wrapping. *)
  let wraps ~size ctx x = assert false;;
  let wrapu ~size ctx x = assert false;;

  (* TODO: this model assumes that no overflow occurs (this should be
     checked separately), and that bitvectors are not re-interpreted
     (e.g. memcopying a signed int to an int). Thus we should add
     checks (i.e. ACSL terms) that the input of signed operands
     (e.g. division) is within the correct range.

     To see where to put wrapping operations, see e.g. Jacques-Henri
     Jourdan PhD thesis. *)
  module Binary_Forward = struct

    module IF = Integer_Forward

    let pred2 f = fun ~size ctx a b ->
      f ctx a b;;

    let op2 f = fun ~size ctx a b ->
      (* Codex_log.feedback "size %d size1 %d size2 %d" size size1 size2; *)
      f ctx a b
    ;;

    let op2_flags f = fun ~size ~flags ctx a b ->
      (* Codex_log.feedback "size %d size1 %d size2 %d" size size1 size2; *)
      f ctx a b
    ;;


    let op2_all_flags f = fun ~size ~nsw ~nuw ~nusw ctx a b ->
      (* Codex_log.feedback "size %d size1 %d size2 %d" size size1 size2; *)
      f ctx a b
    ;;

    (* XXX: Commencer par ici. les comparaisons doivent introduire des modulos, ca depend de la taille. *)

    (* Pour beq, on a le choix. Regardons les intervalles des arguments, et faisons au mieux pour savoir comment wrapper.   Notons que si les intervalles sont complètement disjoints, on peut parfois faire juste une soustraction.  *)
    (* On peut aussi faire un case split: soit on est égal et on est sur l'intervalle signé, soit on est égal et on est sur l'intervalle non-signé. Pas forcément le plus arrangeant. *)

    let beq = pred2 IF.ieq
    let biule = pred2 IF.ile
    let bisle = pred2 IF.ile
    let biadd = op2_flags IF.iadd
    let bisub = op2_flags IF.isub
    let bimul = op2_flags IF.imul
    let bxor = op2 IF.ixor
    let band = op2 IF.iand
    let bor = op2 IF.ior

    (* TODO: maybe we need to wrap here. e.g. bsext is a noop only if
       the old value is wrapped, and likewise for buext. But, if
       oldsize == size, we don't need to wrap. *)
    let bsext ~size ~oldsize ctx x = if (size == oldsize) then x else x (* assert false *)
    let buext ~size ~oldsize ctx x = if (size == oldsize) then x else x (* assert false *)
    let bofbool ~size _ = assert false
    let bchoose ~size cond _ = assert false
    let bashr = op2 IF.ishr
    let blshr = op2 IF.ishr
    let bshl = op2_flags IF.ishl
    let bisdiv = op2 IF.idiv
    let biudiv = op2 IF.idiv

    open Units
    let bconcat ~(size1:In_bits.t) ~(size2:In_bits.t) ctx a b =
      let result = IF.iadd ctx b @@ IF.itimes (Z.shift_left Z.one (size2:>int)) ctx a in
      result

    let bismod = op2 IF.imod
    let biumod ~size ctx a b =
      let a = wrapu ~size ctx a in
      let b = wrapu ~size ctx b in
      op2 IF.imod ~size ctx a b

    let bextract ~(size:In_bits.t) ~(index:In_bits.t) ~(oldsize:In_bits.t) ctx b =
      (* Codex_log.feedback "lift_integer.bextract oldsize %d index %d size %d sizeb %d\n"  oldsize index size sizeb; *)
      assert In_bits.(index + size <= oldsize);
      (* assert(size < 64); *)
      (* Fast path *)
      if size == oldsize then (assert(index == In_bits.zero); b)
      else
        let div =
          (* We use shr instead of division because:
             - This avoids having too large number (when extracting parts of a big array)
             - We need euclidian division, not truncated div. *)
          if index == In_bits.zero then b else IF.ishr ctx b (IF.iconst (Z.of_int (index:>int)) ctx)
        in
        (* XXX: Should be euclidian modulo, not truncated one. *)
        let modu =
          if In_bits.(size + index == oldsize) then div
          else
            (* XXX: iand renvoie aussi un resultat positif, donc ne convient pas.
             * MAYBE: do a bextract on integers? *)
            (* IF.iand ctx div (IF.iconst (Z.pred @@ Z.shift_left Z.one size) ctx) *)
            IF.imod ctx div (IF.iconst (Z.shift_left Z.one (size:>int)) ctx)
        in
        modu
    ;;

    let biconst ~size k ctx =
      (* Kernel.feedback "biconst size %d" size;  *)
      (IF.iconst k ctx)

    (* TODO: The address-specific transfer functions should all be
       handled by the enclosing domain.  And we should be able to
       write assert false for them here.

       TODO: We should have a world parameter for boolean unknowns
       too, that we would use here.
    *)
    let buninit ~size = assert false (* MAYBE: empty? *)
    let valid ~size:_ _acc_typ _ctx _v = assert false (* boolean_unknown? *)
      (* IF.ieq ctx (IF.iunknown () ctx) (IF.zero ctx) *)

    let valid_ptr_arith ~size:_ _ = assert false

    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
  end

  let binary_pretty ~size ctx fmt i =
    I.integer_pretty ctx fmt i
  let binary_is_empty ~size ctx i = I.integer_is_empty ctx i
  let _binary_pretty ~size ctx fmt (i,_size) =
    Format.fprintf fmt "{size:%d;contents:%a}" _size (I.integer_pretty ctx) i
  ;;


  let integer_is_empty = I.integer_is_empty
  let integer_pretty = I.integer_pretty

  let boolean_pretty = I.boolean_pretty
  (* let boolean_is_empty = I.boolean_is_empty *)

  let enum_pretty = I.enum_pretty

  let assume_binary ~size ctx cond (x,sizex) =
    (* Codex_log.feedback "size %d sizex %d" size sizex; *)
    assert (size == sizex);
    assert false;;
    (* (I.assume_integer ctx cond x, size) *)


  module Binary = I.Integer
  module Integer = I.Integer
  module Boolean = I.Boolean
  module Enum = I.Enum

  module Query = struct
    include (I.Query:(module type of I.Query
                       with module Binary_Lattice := I.Query.Binary_Lattice))
    module Binary_Lattice = Lattices.Bitvector_Of_Integer.Make(I.Integer_Query.Integer_Lattice)


    let binary_fold_crop ~size bin ~inf ~sup acc f = I.Integer_Query.Integer_Lattice.fold_crop bin ~inf ~sup f acc

    (* The fact that we know something about the binary does not mean
       that we know something about the integer. For instance, &a[0]
       != &b[0] does not imply that 0 != 0. When correct, it is still
       possible to call directly I.Query.inject_boolean. *)
    let binary ~size ctx id = I.Integer_Query.query ctx id
  end
  module Integer_Query = I.Integer_Query

  let satisfiable ctx boolean = I.satisfiable ctx boolean

  let binary_empty ~size = I.integer_empty
  let integer_empty = I.integer_empty
  let boolean_empty = I.boolean_empty
  let enum_empty = I.enum_empty

  let binary_unknown ~size ctx = I.integer_unknown ctx
  let integer_unknown = I.integer_unknown
  let boolean_unknown = I.boolean_unknown
  let enum_unknown = I.enum_unknown


  let union = I.union
  let binary_unknown_typed ~size:_ _ = assert false
  let query_boolean = I.query_boolean

end
