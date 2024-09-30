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

(* TODO: A version where we add wraps and wrapu operations
   manually. And somewhere (in the constraint domain?) remove those for
   which we prove that no wrapping occurs. *)

type size = int

module Make
    (I: Domain_sig.Base_with_integer):Domain_sig.Base_with_integer
  with type binary = I.integer * size
   and type boolean = I.boolean
   and type integer = I.integer
   and module Context = I.Context
   and module Integer_Lattice = I.Integer_Lattice
   (* and module Query.Binary_Lattice = I.Query.Integer_Lattice *)
= struct

  let name() = "Lift_integer_domain_to_binary_domain(" ^ (I.name()) ^ ")";;
  let unique_id() = Domain_sig.Fresh_id.fresh @@ name();;

  (* open Term_types *)

  module Types = struct
    type integer = I.integer
    type binary = (I.integer * size)
    (* The value and the size. NOTE: The size is always given in the
       arguments, and is thus not be required. It is useful only
       because it sometimes allows to catch bugs. *)
    type boolean = I.boolean
  end

  include Types

  module Context = I.Context
  open Context
  (* include Transfer_functions.Builtin.Make(Types)(Context) *)

  let mu_context_open = I.mu_context_open

  let typed_nondet2 = I.typed_nondet2
  let nondet_same_context = I.nondet_same_context

  let root_context = I.root_context
  let context_pretty = I.context_pretty

  let serialize_integer = I.serialize_integer
  let serialize_boolean = I.serialize_boolean
  let serialize_binary ~size ctxa (a,sa) ctxb (b,sb) acc =
    assert(size == sa && size == sb);
    let Result(included,acc,d) = I.serialize_integer ctxa a ctxb b acc in
    Result(included,acc,fun ctx tup -> let res,tup = d ctx tup in (res,size),tup)
  ;;
    
  let typed_fixpoint_step = I.typed_fixpoint_step
  let widened_fixpoint_step = I.widened_fixpoint_step

  let assume = I.assume
  let imperative_assume = I.imperative_assume
  let imperative_assign_context = I.imperative_assign_context                 
  
  
  module Boolean_Forward = I.Boolean_Forward
  module Integer_Forward = I.Integer_Forward                             
  module Memory_Forward = Assert_False_Transfer_Functions.Memory.Memory_Forward

  (* TODO: this model assumes that no overflow occurs (this should be
     checked separately), and that bitvectors are not re-interpreted
     (e.g. memcopying a signed int to an int). Thus we should add
     checks (i.e. ACSL terms) that the input of signed operands
     (e.g. division) is within the correct range. 

     To see where to put wrapping operations, see e.g. Jacques-Henri
     Jourdan PhD thesis. *)
  module Binary_Forward = struct
    
    module IF = Integer_Forward

    let pred2 f = fun ~size ctx (a,size1) (b,size2) ->
      assert(size1 == size2);
      assert (size == size1);
      f ctx a b;;

    let op2 f = fun ~size ctx (a,size1) (b,size2) ->
      (* Codex_log.feedback "size %d size1 %d size2 %d" size size1 size2; *)
      assert(size == size1);
      assert(size == size2);
      (f ctx a b, size)
    ;;

    let op2_flags f = fun ~size ~nsw ~nuw ctx (a,size1) (b,size2) ->
      (* Codex_log.feedback "size %d size1 %d size2 %d" size size1 size2; *)
      assert(size == size1);
      assert(size == size2);
      (f ctx a b, size)
    ;;

    let op2_all_flags f = fun ~size ~nsw ~nuw ~nusw ctx (a,size1) (b,size2) ->
      (* Codex_log.feedback "size %d size1 %d size2 %d" size size1 size2; *)
      assert(size == size1);
      assert(size == size2);
      (f ctx a b, size)
    ;;
    
    
    let beq = pred2 IF.ieq
    let biule = pred2 IF.ile
    let bisle = pred2 IF.ile
    let biadd = op2_all_flags IF.iadd
    let bisub = op2_all_flags IF.isub
    let bimul = op2_flags IF.imul
    let bxor = op2 IF.ixor
    let band = op2 IF.iand
    let bor = op2 IF.ior

    let bsext ~size ~oldsize ctx (x,sizex) = assert(size >= sizex); (x,size)
    let buext ~size ~oldsize ctx (x,sizex) = assert(size >= sizex); (x,size)
    let bofbool ~size _ = assert false
    let bchoose ~size cond _ = assert false
    let bashr = op2 IF.ishr
    let blshr = op2 IF.ishr
    let bshl = op2_flags IF.ishl
    let bisdiv = op2 IF.idiv
    let biudiv = op2 IF.idiv

    let bconcat ~size1 ~size2 ctx (a,_) (b,_) =
      let result = IF.iadd ctx b @@ IF.itimes (Z.shift_left Z.one size2) ctx a in
      (result,size1 + size2)

    let bismod = op2 IF.imod
    let biumod = op2 IF.imod

    let bextract ~size ~index ~oldsize ctx ((b,sizeb) as allb) =
      (* Codex_log.feedback "lift_integer.bextract oldsize %d index %d size %d sizeb %d\n"  oldsize index size sizeb; *)
      assert (oldsize == sizeb);
      assert(index + size <= sizeb);
      (* assert(size < 64); *)
      (* Fast path *)
      if size == sizeb then (assert(index == 0); allb)
      else
        let div =
          (* We use shr instead of division because:
             - This avoids having too large number (when extracting parts of a big array)
             - We need euclidian division, not truncated div. *)
          if index == 0 then b else IF.ishr ctx b (IF.iconst (Z.of_int index) ctx)
        in
        (* XXX: Should be euclidian modulo, not truncated one. *)
        let modu =
          if size + index == sizeb then div
          else
            (* XXX: iand renvoie aussi un resultat positif, donc ne convient pas.
             * MAYBE: do a bextract on integers? *)                                                               
            (* IF.iand ctx div (IF.iconst (Z.pred @@ Z.shift_left Z.one size) ctx) *) 
            IF.imod ctx div (IF.iconst (Z.shift_left Z.one size) ctx)
        in
        (modu,size)
    ;;

    let biconst ~size k ctx =
      (* Kernel.feedback "biconst size %d" size;  *)
      (IF.iconst k ctx,size)

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

  let binary_pretty ~size ctx fmt (i,_size) =
    I.integer_pretty ctx fmt i
  let binary_is_empty ~size ctx (i,_size) = I.integer_is_empty ctx i
  let _binary_pretty ~size ctx fmt (i,_size) =
    Format.fprintf fmt "{size:%d;contents:%a}" _size (I.integer_pretty ctx) i
  ;;

  
  let integer_is_empty = I.integer_is_empty
  let integer_pretty = I.integer_pretty

  let boolean_pretty = I.boolean_pretty
  (* let boolean_is_empty = I.boolean_is_empty *)

  let assume_binary ~size ctx cond (x,sizex) =
    (* Codex_log.feedback "size %d sizex %d" size sizex; *)
    assert (size == sizex);
    assert false;;
    (* (I.assume_integer ctx cond x, size) *)

  
  module Binary = Datatype_sig.Prod2(I.Integer)(Datatype_sig.Int)
  module Integer = I.Integer
  module Boolean = I.Boolean

  module Query = struct
    include (I.Query:(module type of I.Query
                       with module Binary_Lattice := I.Query.Binary_Lattice))
    module Binary_Lattice = struct
      include I.Integer_Lattice
      let bottom ~size = bottom
      let is_bottom ~size = is_bottom
      let includes ~size = includes
      let top ~size = top
      let inter ~size = inter
      let join ~size = join
      let pretty ~size = I.Integer_Lattice.pretty
      let widen ~size = widen
      let includes_or_widen ~size = includes_or_widen
      let singleton ~size k = singleton k
    end

    let binary_to_ival ~signed ~size x = (assert signed (*unsigned unimplemented*); I.convert_to_ival x)
    let binary_to_known_bits ~size x = assert false
    let binary_fold_crop ~size bin ~inf ~sup f acc = assert false
    let binary_is_empty ~size x = assert false              
    let binary_is_singleton ~size = I.is_singleton_int 
        
    (* The fact that we know something about the binary does not mean
       that we know something about the integer. For instance, &a[0]
       != &b[0] does not imply that 0 != 0. When correct, it is still
       possible to call directly I.Query.inject_boolean. *)
    let binary ~size ctx (id,_) = I.query_integer ctx id
  end
  module Integer_Lattice = I.Integer_Lattice
  let query_integer = I.query_integer
  let convert_to_ival = I.convert_to_ival
  let is_singleton_int = I.is_singleton_int

  let satisfiable ctx boolean = I.satisfiable ctx boolean    

  let binary_empty ~size ctx = (I.integer_empty ctx,size)
  let integer_empty = I.integer_empty
  let boolean_empty = I.boolean_empty

  let binary_unknown ~size ctx = (I.integer_unknown ctx,size)
  let integer_unknown = I.integer_unknown
  let boolean_unknown = I.boolean_unknown

  
  let union _ = assert false
  let binary_unknown_typed ~size:_ _ = assert false
  let query_boolean = I.query_boolean
end
