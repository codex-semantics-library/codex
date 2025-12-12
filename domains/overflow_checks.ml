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

module Log = Tracelog.Make(struct let category = "Domains.Overflow_checks" end);;
module In_bits = Units.In_bits
open Operator.Alarm

let increase_size = Codex_config.extend_size_for_additive_operations

module Make
  (Sub : Sig.BASE_WITH_INTEGER) =
struct
  include Sub

  (* TODO: Ideally, we would emit alarms here otherwise some assumptions could go unnoticed. This requires
     that every new pointer is assumed to be in the correct range, which we don't do for now.         *)
  let should_emit_alarm = false
  let emit_unsigned_signed_alarms = false

  module Binary_Forward = struct
    include Sub.Binary_Forward

    (* TODO: Should be provided in the domain signature; assume true
       is a workaround. *)
    let clone dom =
      let dom' = match Sub.assume dom (Sub.Boolean_Forward.true_ dom) with
        | Some x -> x
        | None -> assert false in
      dom'

    (* Evaluate a condition in a copy of dom (to not pollute it with
       new terms), that is removed if the condition is
       valid. Otherwise, calls [emit_alarm] then refine dom assuming
       that the condition is true.

       "valid" means that the condition does not return false if [id_or_not] is id,
       and does not return true if it is Quadrivalent.not. *)
    let check_cond  mkcond emit_alarm dom =
      let dom' = clone dom in
      let cond = mkcond dom' in
      match query_boolean dom' cond with
      | Lattices.Quadrivalent.True -> ()
      | Lattices.Quadrivalent.Bottom ->
        Log.fatal (fun p -> p "Evaluation of an overflow condition returned a bottom. \
This is probably an analyzer error that should not have happened \
(the bottom should have been catched earlier).  Please report.")
      | Lattices.Quadrivalent.(False | Top) ->
        emit_alarm ();
        begin
          match assume dom' cond (* Further refine the error. *) with
          | None -> raise Sig.Bottom
          | Some dom'' -> Sub.Context.assign dom dom''
        end


    (** [check_overflow ~signed ~small_size ~wide_size value]
        Adds assertion ensuring that [value], a binary value of size [~wide_size]
        bits, can correctly fit on [~small_size] bits without over/underflow.
        - If [~signed], this means [-2^{small_size-1} <=s value <=s 2^{small_size-1} - 1]
        - If not [~signed], this means [0 <=u value <=u 2^small_size - 1] *)
    let check_overflow ~signed ~(small_size:In_bits.t) ~wide_size operation value dom =
      let compare = (if signed then bisle else biule) in
      let two_pow_size = Z.shift_left Z.one (if signed then (small_size:>int)-1 else (small_size:>int)) in
      (* Upper bound *)
      let upper_bound = Z.sub two_pow_size Z.one in
      let mkcond dom =
        let upper_bound_val = biconst ~size:wide_size upper_bound dom in
        compare ~size:wide_size dom value upper_bound_val in
      let emit_alarm () =
        if should_emit_alarm then
          let size = small_size in
          let overflow_type = if signed then Signed else Unsigned in
          Emit_alarm.emit_alarm  (Integer_overflow{size;operation;overflow_type})
      in
      check_cond mkcond emit_alarm dom;
      (* lower bound *)
      if signed then
        let lower_bound = Z.neg two_pow_size in
        let mkcond dom =
          let lower_bound_val = biconst ~size:wide_size lower_bound dom in
          compare ~size:wide_size dom lower_bound_val value in
        let emit_alarm () =
          if should_emit_alarm then begin
            let size = small_size in
            let overflow_type = Signed in
            Emit_alarm.emit_alarm (Integer_underflow {size;operation;overflow_type})
          end in
        check_cond mkcond emit_alarm dom

    (** [term_builder ~signed dom ~small_size ~wide_size binop v1 v2 r]
        extends [v1], [v2] from [~small_size] to [~wide_size], computes the
        [binop] on [~wide_size], then assumes there are no overflows. *)
    let term_builder ~signed dom ~small_size ~wide_size binop op v1 v2 =
      let dom = clone dom in
      let extend = if signed then bsext else buext in
      let v1_wide = extend ~size:wide_size ~oldsize:small_size dom v1 in
      let v2_wide = extend ~size:wide_size ~oldsize:small_size dom v2 in
      let res = binop ~size:wide_size dom v1_wide v2_wide in
      check_overflow ~signed ~small_size ~wide_size op res dom

    (** Variant of [term_builder] (with inlined [check_overflow])
        for the [~nusw] case (adding an unsigned (pointer) and a signed [offset])
        result must fit on [unsigned] without overflow or underflow. *)
    let check_nusw orig_dom ~small_size ~wide_size binop operation v1 v2 =
      let dom = clone orig_dom in
      let overflow_type = Operator.Alarm.Unsigned_signed in
      let v1_wide = buext ~size:wide_size ~oldsize:small_size dom v1 in
      let v2_wide = bsext ~size:wide_size ~oldsize:small_size dom v2 in
      let res = binop ~size:wide_size dom v1_wide v2_wide in
      let two_pow_size = Z.shift_left Z.one (small_size:>int) in

      (* Upper bound *)
      let mkcond dom =
        let upper_bound = Z.sub two_pow_size Z.one in
        let upper_bound_val = biconst ~size:wide_size upper_bound dom in
        bisle ~size:wide_size dom res upper_bound_val in
      let emit_alarm() =
        let size = small_size in
        if emit_unsigned_signed_alarms then
          Emit_alarm.emit_alarm (Integer_overflow { overflow_type; size; operation })
      in
      check_cond mkcond emit_alarm dom;

      (* lower bound *)
      let mkcond dom =
        let lower_bound_val = biconst ~size:wide_size Z.zero dom in
        bisle ~size:wide_size dom lower_bound_val res in
      let emit_alarm() =
        let size = small_size in
        if emit_unsigned_signed_alarms then
          Emit_alarm.emit_alarm (Integer_underflow {overflow_type; size; operation });
      in
      check_cond mkcond emit_alarm dom



    (** [binop_with_overflow_guard ~small_size ~wide_size binop_func v1 v2]
        returns [binop_func ~size:small_size v1 v2], but first it ensures that no
        overflow occurs by computing [binop_func ~size:wide_size v1 v2] and checking
        that it fits on [~small_size] bits.

        @param ~small_size should be the size of terms [v1] and [v2]
        @param ~wide_size should be large enough to ensure that
               [binop_func ~size:wide_size v1 v2] does not overflow *)
    let binop_with_overflow_guard ~nsw ~nuw ~nusw dom ~small_size ~wide_size binop op v1 v2 =
      if nsw then term_builder ~signed:true dom ~small_size ~wide_size binop op v1 v2;
      if nuw then term_builder ~signed:false dom ~small_size ~wide_size binop op v1 v2;
      if nusw (* We need size+1 here to be able to sign compare unsigned values *)
      then check_nusw dom ~small_size ~wide_size:(increase_size wide_size) binop op v1 v2;
      binop ~size:small_size dom v1 v2

    let biadd ~size ~flags dom a b =
      let Operator.Flags.Biadd.{nsw;nuw;nusw} = Operator.Flags.Biadd.unpack flags in
      (* if nsw || nuw || nusw then
        Emit_alarm.feedback "OverflowAddition size:%d nsw:%b nuw:%b nusw:%b (%a + %a = %a)"
          size nsw nuw nusw
          Terms.pretty a Terms.pretty b Terms.pretty res; *)
      binop_with_overflow_guard ~nsw ~nuw ~nusw dom
        ~small_size:size ~wide_size:(increase_size size) (* No overflow on n+1 bits *)
        (Binary_Forward.biadd ~flags) Biadd
        a b

    let bisub ~size ~flags dom a b =
      let Operator.Flags.Bisub.{nsw;nuw;nusw} = Operator.Flags.Bisub.unpack flags in
      (* if nsw || nuw || nusw then
        Emit_alarm.feedback "OverflowSubtraction size:%d nsw:%b nuw:%b nusw:%b (%a + %a = %a)"
          size nsw nuw nusw
          Terms.pretty a Terms.pretty b Terms.pretty res; *)
      binop_with_overflow_guard ~nsw ~nuw ~nusw dom
        ~small_size:size ~wide_size:(increase_size size) (* No overflow on n+1 bits *)
        (Binary_Forward.bisub ~flags) Bisub
        a b

    let bimul ~size ~flags dom a b = (* Binary_Forward.bimul ~size ~nsw ~nuw dom a b *)
      let Operator.Flags.Bimul.{nsw;nuw} = Operator.Flags.Bimul.unpack flags in
      binop_with_overflow_guard ~nsw ~nuw ~nusw:false dom
        ~small_size:size ~wide_size:In_bits.(double size) (* No overflow on 2n bits *)
        (Binary_Forward.bimul ~flags) Bimul
        a b

    (* There is only one case whe div overflows: MIN_INT / -1 *)
    let bisdiv ~size dom a b =
      let mkcond dom =
        let minus_one = biconst ~size Z.minus_one dom in
        let div_is_neg_one = beq ~size dom b minus_one in
        let min_int = biconst ~size (Z.neg (Z.shift_left Z.one ((size:>int)-1))) dom in
        let num_is_min_int = beq ~size dom a min_int in
        let cond = Boolean_Forward.(&&) dom div_is_neg_one num_is_min_int in
        Boolean_Forward.not dom cond in
      let emit_alarm () =
        if should_emit_alarm then begin
          let overflow_type = Signed in
          let alarm = Integer_overflow { overflow_type; size; operation = Bisdiv } in
          Emit_alarm.emit_alarm alarm
        end in
      check_cond mkcond emit_alarm dom;
      bisdiv ~size dom a b
    ;;
  end
end
