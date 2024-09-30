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
  (Sub : Constraint_domains_sig.Domain_S) =
struct
  include Sub

  (* TODO: Ideally, we would emit alarms here otherwise some assumptions could go unnoticed. This requires
     that every new pointer is assumed to be in the correct range, which we don't do for now.         *)
  let emit_alarm = false;;

  module Binary_Forward = struct
    include Sub.Binary_Forward

    (** [check_overflow ~signed ~small_size ~wide_size value]
        Adds assertion ensuring that [value], a binary value of size [~wide_size]
        bits, can correctly fit on [~small_size] bits without over/underflow.
        - If [~signed], this means [-2^{small_size-1} <=s value <=s 2^{small_size-1} - 1]
        - If not [~signed], this means [0 <=u value <=u 2^small_size - 1] *)
    let check_overflow ~signed ~small_size ~wide_size value dom =
      let compare_builder = Constraints.Build.Binary.(if signed then bisle else biule) in
      let compare_func = Binary_Forward.(if signed then bisle else biule) in
      let two_pow_size = Z.shift_left Z.one (if signed then small_size-1 else small_size) in
      (* Upper bound *)
      let upper_bound = Z.sub two_pow_size Z.one in
      let upper_bound_val = Constraints.Build.Binary.biconst ~size:wide_size upper_bound in
      let upper_overflow = compare_builder ~size:wide_size value upper_bound_val in
      let dom' = compare_func ~size:wide_size dom value upper_bound_val upper_overflow in
      let dom = match Query.boolean dom' upper_overflow with
      | Lattices.Quadrivalent.True -> dom (* The assertion is proven, we can forget the extra terms *)
      | Lattices.Quadrivalent.Bottom
      | Lattices.Quadrivalent.(False | Top) as x ->
        if emit_alarm then
          Codex_log.alarm (if signed then "signed integer overflow" else "unsigned integer overflow");
        if x = Lattices.Quadrivalent.False then raise Domain_sig.Bottom
        else match assume dom' upper_overflow with
          | Some dom -> dom
          | None -> raise Domain_sig.Bottom
      in
      (* lower bound *)
      if signed then
        let lower_bound = Z.neg two_pow_size in
        let lower_bound_val = Constraints.Build.Binary.biconst ~size:wide_size lower_bound in
        let lower_underflow = compare_builder ~size:wide_size lower_bound_val value in
        let dom' = compare_func ~size:wide_size dom lower_bound_val value lower_underflow in
        match Query.boolean dom' lower_underflow with
        | Lattices.Quadrivalent.True -> dom (* The assertion is proven, we can forget the extra terms *)
        | Lattices.Quadrivalent.Bottom
        | Lattices.Quadrivalent.(False|Top) ->
          if emit_alarm then
            Codex_log.alarm "possible signed integer underflow";
            match assume dom' lower_underflow with
            | Some dom -> dom
            | None -> raise Domain_sig.Bottom
        else dom

    (** [term_builder ~signed dom ~small_size ~wide_size binop_builder binop_func v1 v2 r]
        extends [v1], [v2] from [~small_size] to [~wide_size], computes the
        [binop_func] on [~wide_size], then assumes there are no overflows. *)
    let term_builder ~signed dom ~small_size ~wide_size binop_builder binop_func v1 v2 =
      let extend_builder = Constraints.Build.Binary.(if signed then bsext else buext) in
      let extend_func = Binary_Forward.(if signed then bsext else buext) in
      let v1_wide = extend_builder ~size:wide_size ~oldsize:small_size v1 in
      let v2_wide = extend_builder ~size:wide_size ~oldsize:small_size v2 in
      let res = binop_builder ~size:wide_size v1_wide v2_wide in
      let dom_with_check_terms = extend_func ~size:wide_size ~oldsize:small_size dom v1 v1_wide in
      let dom_with_check_terms = extend_func ~size:wide_size ~oldsize:small_size dom_with_check_terms v2 v2_wide in
      let dom_with_check_terms = binop_func ~size:wide_size dom_with_check_terms v1_wide v2_wide res in
      let dom_with_checks = check_overflow ~signed ~small_size ~wide_size res dom_with_check_terms in
      if dom_with_checks == dom_with_check_terms
      then dom (* If check has no changes, drop the extra terms *)
      else dom_with_checks

    (** Variant of [term_builder] (with inlined [check_overflow])
        for the [~nusw] case (adding an unsigned (pointer) and a signed [offset])
        result must fit on [unsigned] without overflow or underflow. *)
    let check_nusw dom ~small_size ~wide_size binop_builder binop_func v1 v2 =
      let v1_wide = Constraints.Build.Binary.buext ~size:wide_size ~oldsize:small_size v1 in
      let v2_wide = Constraints.Build.Binary.bsext ~size:wide_size ~oldsize:small_size v2 in
      let res = binop_builder ~size:wide_size v1_wide v2_wide in
      let dom_with_check_terms = Binary_Forward.buext ~size:wide_size ~oldsize:small_size dom v1 v1_wide in
      let dom_with_check_terms = Binary_Forward.bsext ~size:wide_size ~oldsize:small_size dom_with_check_terms v2 v2_wide in
      let dom_with_check_terms = binop_func ~size:wide_size dom_with_check_terms v1_wide v2_wide res in
      let two_pow_size = Z.shift_left Z.one small_size in
      (* Upper bound *)
      let upper_bound = Z.sub two_pow_size Z.one in
      let upper_bound_val = Constraints.Build.Binary.biconst ~size:wide_size upper_bound in
      let upper_overflow = Constraints.Build.Binary.bisle ~size:wide_size res upper_bound_val in
      let dom_with_checks = Binary_Forward.bisle ~size:wide_size dom_with_check_terms res upper_bound_val upper_overflow in
      let dom, dom_with_check_terms = match Query.boolean dom_with_checks upper_overflow with
      | Lattices.Quadrivalent.True -> dom, dom_with_check_terms (* The assertion is proven, we can forget the extra terms *)
      | Lattices.Quadrivalent.Bottom
      | Lattices.Quadrivalent.(False | Top) ->
        if emit_alarm then
          Codex_log.alarm "possible unsigned-signed integer overflow";
        match assume dom_with_checks upper_overflow with
        | Some dom -> dom, dom
        | None -> raise Domain_sig.Bottom in
      (* lower bound *)
      let lower_bound_val = Constraints.Build.Binary.biconst ~size:wide_size Z.zero in
      let lower_underflow = Constraints.Build.Binary.bisle ~size:wide_size lower_bound_val res in
      let dom_with_checks = Binary_Forward.bisle ~size:wide_size dom_with_check_terms lower_bound_val res lower_underflow in
        match Query.boolean dom_with_checks lower_underflow with
        | Lattices.Quadrivalent.True -> dom (* The assertion is proven, we can forget the extra terms *)
        | Lattices.Quadrivalent.Bottom
        | Lattices.Quadrivalent.(False | Top) ->
          if emit_alarm then
            Codex_log.alarm "possible unsigned-signed integer underflow";
          match assume dom_with_checks lower_underflow with
          | Some dom -> dom
          | None -> raise Domain_sig.Bottom

    (** [binop_with_overflow_guard ~small_size ~wide_size binop_func v1 v2]
        returns [binop_func ~size:small_size v1 v2], but first it ensures that no
        overflow occurs by computing [binop_func ~size:wide_size v1 v2] and checking
        that it fits on [~small_size] bits.

        @param ~small_size should be the size of terms [v1] and [v2]
        @param ~wide_size should be large enough to ensure that
               [binop_func ~size:wide_size v1 v2] does not overflow *)
    let binop_with_overflow_guard ~nsw ~nuw ~nusw dom ~small_size ~wide_size binop_builder binop_func v1 v2 r =
      let dom = if nsw
        then term_builder ~signed:true dom ~small_size ~wide_size binop_builder binop_func v1 v2
        else dom
      in let dom = if nuw
        then term_builder ~signed:false dom ~small_size ~wide_size binop_builder binop_func v1 v2
        else dom
      in let dom = if nusw (* We need size+1 here to be able to sign compare unsigned values *)
        then check_nusw dom ~small_size ~wide_size:(wide_size+1) binop_builder binop_func v1 v2
        else dom
      in binop_func ~size:small_size dom v1 v2 r

      (*
      (** Same as {!binop_with_overflow_guard}, but for a unary operator *)
      let unop_with_overflow_guard exp ~small_size ~wide_size unop v =
        let* v_wide = Binary_Forward.bsext ~size:wide_size ~oldsize:small_size v in
        let* full_op = unop ~size:wide_size v_wide in
        let* () = assert_no_signed_overflow exp ~small_size ~wide_size full_op in
        unop ~size:small_size v       *)

    let biadd ~size ~nsw ~nuw ~nusw dom a b res =
      (* if nsw || nuw || nusw then
        Codex_log.feedback "OverflowAddition size:%d nsw:%b nuw:%b nusw:%b (%a + %a = %a)"
          size nsw nuw nusw
          Constraints.pretty a Constraints.pretty b Constraints.pretty res; *)
      binop_with_overflow_guard ~nsw ~nuw ~nusw dom
        ~small_size:size ~wide_size:(size+1) (* No overflow on n+1 bits *)
        (Constraints.Build.Binary.biadd ~nsw ~nuw ~nusw)
        (Binary_Forward.biadd ~nsw ~nuw ~nusw)
        a b res

    let bisub ~size ~nsw ~nuw ~nusw dom a b res =
      (* if nsw || nuw || nusw then
        Codex_log.feedback "OverflowSubtraction size:%d nsw:%b nuw:%b nusw:%b (%a + %a = %a)"
          size nsw nuw nusw
          Constraints.pretty a Constraints.pretty b Constraints.pretty res; *)
      binop_with_overflow_guard ~nsw ~nuw ~nusw dom
        ~small_size:size ~wide_size:(size+1) (* No overflow on n+1 bits *)
        (Constraints.Build.Binary.bisub ~nsw ~nuw ~nusw)
        (Binary_Forward.bisub ~nsw ~nuw ~nusw)
        a b res
  end
end
