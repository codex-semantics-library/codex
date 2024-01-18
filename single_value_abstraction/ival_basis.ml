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

module Ival = Framac_ival.Ival
module Abstract_interp = Framac_ival.Abstract_interp
open Ival

let name = "Ival"

module Quadrivalent_Lattice = Lattices.Quadrivalent;;         
module B = Quadrivalent_basis

type integer = t

let is_top x = match x with
  | Top(None,None,r,m) 
      when Z.equal r Z.zero && Z.equal m Z.one -> true
  | _ -> false

let pp_opt f fmt = function
  | None -> Format.fprintf fmt "None"
  | Some x -> Format.fprintf fmt "Some(%a)" f x
;;


module Integer_Forward = struct
  (* Predicates. *)
  let ieq0 a =
    let open Ival in
    match a with
    | Set [| |] -> B.boolean_bottom
    | Set [| a |] when Z.equal a Z.zero -> B.Boolean_Forward.true_
    | _ when not (Ival.contains_zero a) -> B.Boolean_Forward.false_
    | _ -> B.Top

  let ieq a b =
    let open Ival in
    match a,b with
    | Set [| |], _ | _, Set [| |] -> B.boolean_bottom
    | Set [| a |], Set [| b |] when Z.equal a b -> B.Boolean_Forward.true_
    | _ when not (Ival.intersects a b) -> B.Boolean_Forward.false_
    | _ -> B.Top
  ;;
       
  let ige0 a =
    if Ival.is_bottom a then B.Bottom
    else
      let min_a,max_a = Ival.min_and_max a in
      let may_be_false = match min_a with
        | None -> true
        | Some x when Z.lt x Z.zero -> true
        | _ -> false
      in
      let may_be_true = match max_a with
        | None -> true
        | Some x when Z.geq x Z.zero -> true
        | _ -> false
      in
      B.of_bools ~may_be_false ~may_be_true
  ;;
  
  let icmp op1 op2 a b =
    if Ival.is_bottom a || Ival.is_bottom b then
      B.Bottom
    else
      let min_a,max_a = Ival.min_and_max a in
      let min_b,max_b = Ival.min_and_max b in

      let may_be_false = match max_a, min_b with
        | None, _ -> true
        | _, None -> true
        | Some ia, Some ib -> op1 ia ib in

      let may_be_true = match min_a, max_b with
        | None, _ -> true
        | _, None -> true
        | Some ia, Some ib -> op2 ia ib in

      B.of_bools ~may_be_false ~may_be_true
  ;;
  let ile = icmp Z.gt Z.leq;;
  let ilt = icmp Z.geq Z.lt;;
  
  let idiv  = Ival.div
  let imul  = Ival.mul
  let imod = Ival.c_rem

  let iand = Ival.bitwise_and
  let ior = Ival.bitwise_or
  let ixor = Ival.bitwise_xor

  let iadd  = Ival.add_int
  let isub = Ival.sub_int

  let itimes k  = Ival.scale k
  let iconst k  = Ival.inject_singleton k

  let zero  = Ival.zero
  let one  = Ival.one

  let iunknown _ = Ival.top
  let assume cond x = match cond with
    | B.False | B.Bottom -> Ival.bottom
    | _ -> x

  let ishr = Ival.shift_right
  let ishl = Ival.shift_left

  let ishr a b =
    if Ival.is_zero a then a
    else if Ival.is_singleton_int a && Ival.is_singleton_int b
    then Ival.inject_singleton @@ Z.shift_right (Ival.project_int a) (Z.to_int (Ival.project_int b))
    else
      let res = ishr a b in
      (* Codex_log.feedback "ishr %a %a %a" Ival.pretty a Ival.pretty b Ival.pretty res; *)
      res
  ;;

  (* let imod a b =
   *   let res = imod a b in
   *   Codex_log.feedback "imod %a %a %a" Ival.pretty a Ival.pretty b Ival.pretty res;
   *   res *)

  
       
end
  
let inter = narrow

let integer_fold_crop t ~inf ~sup f acc =
  if Z.lt sup inf then acc
  else
  (* Also allow [0,negative] to represent the empty interval. *)
  (* Codex_log.feedback "inf %s sup %s" (Z.to_string inf) (Z.to_string sup); *)
  (* assert (Z.leq inf sup || (Z.equal inf Z.zero && Z.equal sup Z.minus_one)); *)
  (* assert (Z.leq inf sup || (Z.equal inf Z.zero && Z.lt sup Z.zero)); *)   
  match t with
  | Set s ->
     Array.fold_left (fun acc x ->
       if Z.leq inf x && Z.leq x sup
       then f x acc
       else acc) acc s
  | Float _ -> assert false
  | Top(mn,mx,_,step) ->
     let inf = match mn with
       | None -> inf
       | Some mn -> Z.max mn inf in
     let sup = match mx with
       | None -> sup
       | Some mx -> Z.min mx sup in
     Abstract_interp.Int.fold f ~inf ~sup ~step acc

let integer_is_bottom = function
  | Set [| |] -> true
  | _ -> false

let integer_bottom = bottom

let integer_is_singleton x =
  try Some(Ival.project_int x)
  with Ival.Not_Singleton_Int -> None

module Integer_Backward' = struct
  (* XXX: really do it. *)
  let check_improvement old new_ = Some new_ 

  (* a-A <= b-B ==> a <= b et A <= B (grandir b et baisser A) *)

  let ile v1 v2 =
    try 
      let min_v1 = Ival.min_int v1 in
      let max_v2 = Ival.max_int v2 in
      (Ival.narrow v1 (Ival.inject_range None max_v2),
       Ival.narrow v2 (Ival.inject_range min_v1 None))
    with Framac_ival.Abstract_interp.Error_Bottom -> v1, v2 (* TODO: unchanged. *)
  ;;

  let opt1 f m =
    match m with
      None -> None
    | Some m -> Some (f m)

  
  let ilt v1 v2 =
    try 
      let min_v1 = Ival.min_int v1 in
      let max_v2 = Ival.max_int v2 in
      (Ival.narrow v1 (Ival.inject_range None (opt1 Z.pred max_v2))),
      (Ival.narrow v2 (Ival.inject_range (opt1 Z.succ min_v1) None))

    with Framac_ival.Abstract_interp.Error_Bottom -> v1, v2 (* TODO: unchanged. *)
  ;;

end

(* Computes an over-approximation of old and new, and check whether
   the result improves the precision.  *)
let inter_refines old new_ =
  let inter = Ival.narrow old new_ in
  (* Codex_log.feedback "inter a %a b %a res %a"
     Ival.pretty old Ival.pretty new_ Ival.pretty inter; *)
  assert (Ival.is_included inter old);
  assert (Ival.is_included inter new_);
  (* Note: we suppose that the result of narrow is always better
     than old. *)
  if Ival.equal inter old then None else Some inter


module Integer_Backward = struct
  include Basis_noop_transfer_functions.Integer_Backward

  module IF = Integer_Forward;;
  
  (* TODO: Ideally, should be done in the Ival lattice; would be faster. *)

      
  let ieq0 a bool = 
    match bool with
    | B.True -> inter_refines a Ival.zero
    | B.False -> inter_refines a @@ Ival.diff_if_one a Ival.zero
    | B.Bottom -> inter_refines a @@ Ival.bottom
    | B.Top -> None
  ;;

  let ieq a b bool = 
    match bool with
    | B.True -> inter_refines a b, inter_refines b a
    | B.False -> inter_refines a @@ Ival.diff_if_one a b, inter_refines b @@ Ival.diff_if_one b a
    | B.Bottom -> inter_refines a Ival.bottom, inter_refines b Ival.bottom
    | B.Top -> None, None
  ;;

  let iadd i1 i2 res =
    inter_refines i1 (Ival.sub_int res i2),
    inter_refines i2 (Ival.sub_int res i1)
  ;;

  let isub i1 i2 res =
    inter_refines i1 (Ival.add_int res i2),
    inter_refines i2 (Ival.sub_int i1 res)
  ;;

  (* More precise, but does not show up in practice and slower. *)
  let _ishr i1 i2 res =
    (* if Ival.is_bottom i2 then *)
    let ni1 = 
      if Ival.is_singleton_int i2
      then
        let v = Ival.project_int i2 in
        let v = Z.(lsl) Z.one (Z.to_int v) in
        let retrieve_i1 =
          let high_bits = Ival.scale v res in
          let low_bits = Ival.inject_range (Some Z.zero) (Some (Z.pred v)) in
          Ival.bitwise_or high_bits low_bits
        in
        inter_refines i1 retrieve_i1
      else
        None
    in ni1,None
    (* MAYBE: refine with the size of the operation. *)
  ;;
    
  
  let ile a b bool =
    match bool with
    | B.True -> inter_refines a (Ival.backward_comp_int_left Abstract_interp.Comp.Le a b),
                inter_refines b (Ival.backward_comp_int_left Abstract_interp.Comp.Ge b a)
    | B.False -> inter_refines a (Ival.backward_comp_int_left Abstract_interp.Comp.Gt a b),
                 inter_refines b (Ival.backward_comp_int_left Abstract_interp.Comp.Lt b a)
    | B.Bottom -> inter_refines a Ival.bottom, inter_refines b Ival.bottom
    | B.Top -> None, None
  ;;

  (* Note: incorrect when overflows, and Value has better backward multipliers. *)
  let imul i1 i2 res =
    match Ival.project_int i1 with
    | exception Ival.Not_Singleton_Int -> begin
        match Ival.project_int i2 with
        | exception Ival.Not_Singleton_Int -> (None,None)    
        | k -> (inter_refines i1 (Ival.scale_div ~pos:false k res),None)
      end
    | k -> (None,inter_refines i2 (Ival.scale_div ~pos:false k res))
  ;;

  
  let itimes k a res = inter_refines a @@ Ival.scale_div ~pos:true k res

  (* Note: Ival does not handle itimes minus one very well. *)
  let itimes k a res =
    if Z.equal k (Z.minus_one) then
      inter_refines a @@ Ival.neg_int res
    else itimes k a res

  let imod a div res =
    (* Codex_log.feedback "imod %a %a %a" Ival.pretty a Ival.pretty div Ival.pretty res; *)
      (* Note that we cannot just conclude that we can intersect [a]
         with inject_top (None,None,res,div), because those may be
         negative, and this operation is truncated modulo, not
         euclidian modulo.

         Instead, we use the following equality, which holds only
         when v1 does not change sign, which is why we split its range:
         res == (res / div) * div + res *)
      let a_pos = Ival.narrow a Ival.positive_integers in
      let a_neg = Ival.narrow a Ival.negative_integers in

      let res_pos = Ival.narrow res Ival.positive_integers in
      let res_neg = Ival.narrow res Ival.negative_integers in

      let new_a a res = Ival.add_int (Ival.mul (Ival.div a div) div) res in

      let new_a = Ival.join (new_a a_pos res_pos) (new_a a_neg res_neg) in
      (* Codex_log.feedback "new_a %a %a" Ival.pretty new_a Ival.pretty a; *)

      let new_div =
        if Ival.intersects a res then None
        else
          let new_div =
            if Ival.is_bottom a  || Ival.is_bottom res then Ival.bottom
            else Ival.div (Ival.sub_int a res) (Ival.div a div) in
          inter_refines div new_div
      in

      inter_refines a new_a,
      new_div
  ;;

  
  module Debug = struct
      
    let iadd i1 i2 res =
      let (newi1,newi2) = iadd i1 i2 res in
      let affnewi1 = match newi1 with None -> i1 | Some v -> v in
      let affnewi2 = match newi2 with None -> i2 | Some v -> v in    
      Codex_log.debug "iadd: i1 %a i2 %a res %a newi1 %a newi2 %a"
        Ival.pretty i1 Ival.pretty i2 Ival.pretty res Ival.pretty affnewi1 Ival.pretty affnewi2;
      (newi1,newi2)
    ;;
        
    let itimes k a res =
      let (newa) = itimes k a res in
      let affnewa = match newa with None -> a | Some v -> v in
      Codex_log.debug "itimes: k %s a  %a res %a newa %a"
        (Z.to_string k) Ival.pretty a Ival.pretty res Ival.pretty affnewa;
      newa
    ;;

    let imod a div res =
      Codex_log.debug "imod start";
      let res = imod a div res in
      Codex_log.debug "imod end";
      res
    ;;

    
  end
  (* include Debug *)
    
       
end

module Integer_Lattice = struct
  include Ival

  let widen_hints = Widen_Hints.default_widen_hints
  (* let widen_hints = Ival.Widen_Hints.of_list [] *)
  let thirtytwo = Z.of_int 32

  (* Note: not having these widen hints greatly improves performance. *)
  let size_hint = thirtytwo     (* Z.zero *)  
  let widen ~previous next = widen (size_hint, widen_hints) previous next

  let includes_or_widen ~previous next =
    if is_included next previous then (true,next)
    else (false,widen ~previous next)

  
  let includes_or_widen' ~previous next =
    let (bool,res) = includes_or_widen ~previous next in
    (* Codex_log.debug "ival widens %a %a %b %a" Ival.pretty previous Ival.pretty next bool Ival.pretty res; *)
    (bool,res)
  ;;
    
  let includes a b = is_included b a

  let inter = narrow

  let singleton = Ival.inject_singleton
end  

(**************** Binary. ****************)

(* MAYBE: Do nothing if already good. Not sure about what Ival.cast does.*)

let wrapped_u ~size value =
  try
    (match Ival.min_int value with
     | None -> false
     | Some x -> Z.geq x Z.zero) &&
    (match Ival.max_int value with
     | None -> false
     | Some x -> Z.equal Z.zero @@ Z.shift_right_trunc x size)
  with Framac_ival.Abstract_interp.Error_Bottom -> true


(* Slower than above. *)
let _wrapped_u ~size value =
  try
    (match Ival.min_int value with
     | None -> false
     | Some x -> Z.geq x Z.zero) &&
    (match Ival.max_int value with
     | None -> false
     | Some x -> Z.numbits x <= size)
  with Framac_ival.Abstract_interp.Error_Bottom -> true

(* Seems slower. *)
let _wrapped_s ~size value =
  try
    let bound = Z.shift_left Z.one @@ pred size in
    (match Ival.min_int value with
     | None -> false
     | Some x -> Z.geq x (Z.neg bound)) &&
    (match Ival.max_int value with
     | None -> false
     | Some x -> Z.lt x @@ bound)
  with Framac_ival.Abstract_interp.Error_Bottom -> true

let wrapped_s ~size value =
  try
    let sizem1 = size - 1 in
    (* let bound = Z.shift_left Z.one @@ pred size in *)
    (* Cannot use shift_right_trunc here, because of -2^{size-1}. *)
    (match Ival.min_int value with
     | None -> false
     | Some x -> Z.leq Z.minus_one @@ Z.shift_right x sizem1)  &&
    (match Ival.max_int value with
     | None -> false
     | Some x -> Z.geq Z.zero @@ Z.shift_right x sizem1)
  with Framac_ival.Abstract_interp.Error_Bottom -> true

let wrapu ~size value =
  try
    if wrapped_u ~size value then value
    else Ival.cast_int_to_int ~size:(Z.of_int size) ~signed:false value
  with Z.Overflow -> Codex_log.warning "Overflow wrapu: TODO"; top

let wraps ~size value =
  try
    (* Codex_log.feedback "wraps %a" pretty value; *)
    if wrapped_s ~size value then value
    else Ival.cast_int_to_int ~size:(Z.of_int size) ~signed:true value
  with Z.Overflow -> Codex_log.warning "Overflow wraps: TODO"; top

(* Wrapper around wrapu to increase sharing of "top" *)
let wrapu =
  let all32 = Ival.create_all_values ~signed:false ~size:32 in
  let all8 = Ival.create_all_values ~signed:false ~size:8 in  
  fun ~size value ->
    let res = wrapu ~size value in
    match size with
    | 32 ->  if Ival.equal all32 res then all32 else res
    | 8 ->  if Ival.equal all8 res then all8 else res    
    | _ -> res
;;
  
let wraps =
  let all32 = Ival.create_all_values ~signed:true ~size:32 in
  let all8 = Ival.create_all_values ~signed:false ~size:8 in
  fun ~size value ->
    let res = wraps ~size value in
    match size with
    | 32 ->  if Ival.equal all32 res then all32 else res
    | 8 ->  if Ival.equal all8 res then all8 else res        
    | _ -> res
;;


let wraps_or_narrow ~size ~nsw value =
  if nsw then
    let bound = Z.shift_left Z.one @@ pred size in
    let interval = Ival.inject_range (Some (Z.neg bound)) (Some (Z.pred bound)) in
    let res = Ival.narrow value interval in
    (* assert(not @@ Ival.is_bottom res); *)
    res
  else wraps ~size value
;;

let wrapu_or_narrow ~size ~nuw value =
  if nuw then
    let bound = Z.pred @@ Z.shift_left Z.one size in
    let interval = Ival.inject_range (Some Z.zero) (Some bound) in
    let res = Ival.narrow value interval in
    (* assert(not @@ Ival.is_bottom res); *)
    res
  else wrapu ~size value
;;
  



(* Note: Ival is not the best fit for this; e.g. we always have a
   minimum and a maximum value; the congruence information could be
   shared between the signed and unsigned representations, etc. *)




module Unsigned = struct

  (* Invariant: arguments and return of every operation is in the [0,2^size[ range. *)
  module Binary_Forward = struct

    let ar2 op ~size a b =
      assert(wrapped_u ~size a);
      assert(wrapped_u ~size b);
      let res = wrapu ~size @@ op a b in
      (* assert (wrapped_u ~size res); *)
      res
    ;;

    let biadd = ar2 Ival.add_int
    let bisub = ar2 Ival.sub_int
    let bimul = ar2 Ival.mul

    (* Signed operation: wrap to signed, to the operation, wrap to unsigned. *)
    let bisdiv ~size a b = wrapu ~size @@ Integer_Forward.idiv (wraps ~size a) (wraps ~size b)
    let bismod ~size a b = wrapu ~size @@ Integer_Forward.imod (wraps ~size a) (wraps ~size b)        
    let biudiv = ar2 Integer_Forward.idiv
    let biumod = ar2 Integer_Forward.imod

    let band = ar2 Ival.bitwise_and        
    let bor = ar2 Ival.bitwise_or
    let bxor = ar2 Ival.bitwise_xor
    let bshl = ar2 Ival.shift_left
    let bashr = ar2 Ival.shift_right
    let blshr = ar2 Ival.shift_right

    let beq ~size a b = Integer_Forward.ieq a b
    let biule ~size a b = Integer_Forward.ile a b
    let bisle ~size a b =
      let res = Integer_Forward.ile (wraps ~size a) (wraps ~size b) in
      (* Codex_log.feedback "bisle res %a" Quadrivalent_basis.Boolean_Lattice.pretty res; *)
      res

    let bsext ~size ~oldsize x = wrapu ~size @@ wraps ~size:oldsize x
    let buext ~size ~oldsize x = assert (wrapped_u ~size x); x
                                 
    let bofbool ~size x = assert false
    let bchoose ~size cond x = x


    
  (* module Debug = struct *)
  
  (* let _bisdiv ~size a b = *)
  (*   let res = bisdiv ~size a b in *)
  (*   Codex_log.feedback "bisdiv %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res; *)
  (*   res *)


  (* let _biudiv ~size a b = *)
  (*   let res = biudiv ~size a b in *)
  (*   Codex_log.feedback "biudiv %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res; *)
  (*   res *)

  (* let _bismod ~size a b = *)
  (*   let res = bismod ~size a b in *)
  (*   Codex_log.feedback "bismod %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res; *)
  (*   res *)


  (* let _biumod ~size a b = *)
  (*   let res = biumod ~size a b in *)
  (*   Codex_log.feedback "biumod %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res; *)
  (*   res *)

  
  (* let _bisle ~size a b = *)
  (*   let res = bisle ~size a b in *)
  (*   Codex_log.feedback "bisle %d: %a %a %a" size Ival.pretty a Ival.pretty b Quadrivalent_basis.Boolean_Lattice.pretty res; *)
  (*   res *)

  
  (* let _biule ~size a b = *)
  (*   let res = biule ~size a b in *)
  (*   Codex_log.feedback "biule %d: %a %a %a" size Ival.pretty a Ival.pretty b Quadrivalent_basis.Boolean_Lattice.pretty res; *)
  (*   res *)
  (* ;; *)

  (* let _beq ~size a b = *)
  (*   let res = beq ~size a b in *)
  (*   Codex_log.feedback "beq %d: %a %a %a" size Ival.pretty a Ival.pretty b Quadrivalent_basis.Boolean_Lattice.pretty res; *)
  (*   res *)
  (* ;; *)
  
  (* end *)

  (* include Debug *)
  
      
  let buninit ~size = Ival.bottom
  let bunknown ~size = wrapu ~size Ival.top
  let bextract ~size ~index ~oldsize x =
    (* Codex_log.feedback "bextracat %d %d %d %a" size index oldsize Ival.pretty x; *)
    wrapu ~size @@
    Ival.extract_bits ~size:(Z.of_int oldsize) ~start:(Z.of_int index)
      ~stop:(Z.of_int @@ index + size - 1) x
  
  let bconcat ~size1 ~size2 v1 v2 =
    wrapu ~size:(size1 + size2) @@ Ival.add_int (Ival.scale (Z.shift_left Z.one size2) v1) v2

  let _bconcat_ ~size1 ~size2 v1 v2 =
    let res = bconcat ~size1 ~size2 v1 v2 in
    Codex_log.feedback "bconcat %d %d %a %a res %a" size1 size2 Ival.pretty v1 Ival.pretty v2 Ival.pretty res;
    res
    
  let bitimes ~size k x  = wrapu ~size @@ Ival.scale k x
  let assume ~size cond x = match cond with
    | B.False | B.Bottom -> Ival.bottom
    | _ -> x
  let valid ~size _ = assert false
  let biconst ~size k  = wrapu ~size @@ Ival.inject_singleton k

  let bshift ~size ~offset ~max _ = assert false
  let bindex ~size _ = assert false
  
  end

  module Binary_Backward = struct
    include Basis_noop_transfer_functions.Binary_Backward

    (* Invariant: the arguments should have been already wrapped
       unsigned, so this should work directly. *)
    let beq ~size = Integer_Backward.ieq
    let biule ~size = Integer_Backward.ile
    let biumod ~size = Integer_Backward.imod

    let bisle ~size a b bool =
      let wsa = wraps ~size a and wsb = wraps ~size b in
      match bool with
      | B.True ->  inter_refines a (wrapu ~size @@ Ival.backward_comp_int_left Abstract_interp.Comp.Le wsa wsb),
                   inter_refines b (wrapu ~size @@ Ival.backward_comp_int_left Abstract_interp.Comp.Ge wsb wsa)
      | B.False -> inter_refines a (wrapu ~size @@ Ival.backward_comp_int_left Abstract_interp.Comp.Gt wsa wsb),
                   inter_refines b (wrapu ~size @@ Ival.backward_comp_int_left Abstract_interp.Comp.Lt wsb wsa)
      | B.Bottom -> inter_refines a Ival.bottom, inter_refines b Ival.bottom
      | B.Top -> None, None

    let _bisle ~size a b bool =
      let resa,resb = bisle ~size a b bool in
      let f fmt =
        Format.fprintf fmt "backward bisle %d %a %a %a" size Ival.pretty a Ival.pretty b Quadrivalent_basis.Boolean_Lattice.pretty bool;
        (match resa with
        | None -> Format.fprintf fmt " none"
        | Some a -> Format.fprintf fmt " %a" Ival.pretty a);
        (match resb with
        | None -> Format.fprintf fmt " none"
        | Some b -> Format.fprintf fmt " %a" Ival.pretty b)
      in
      Codex_log.feedback "%t" f;
      resa,resb
    ;;
      
  end
  
end

(* NoWrap: unsound version. *)
module NoWrap = struct

  (* A version where we assume that nothing wraps. *)
  module Binary_Forward = struct


    let _wraps ~size value =
      let res = wraps ~size value in
      Codex_log.feedback "wraps %d: %a -> %a" size Ival.pretty value Ival.pretty res;
      res
    ;;

    let _wrapu ~size value =
      let res = wrapu ~size value in
      Codex_log.feedback "wrapu %d: %a -> %a" size Ival.pretty value Ival.pretty res;
      res
    ;;


    let biadd ~size = Ival.add_int
    let bisub ~size = Ival.sub_int
    let bimul ~size = Ival.mul
    let bisdiv ~size a b = Integer_Forward.idiv (wraps ~size a) (wraps ~size b)
    let biudiv ~size a b = Integer_Forward.idiv (wrapu ~size a) (wrapu ~size b)
    let bismod ~size a b = Integer_Forward.imod (wraps ~size a) (wraps ~size b)
    let biumod ~size a b = Integer_Forward.imod (wrapu ~size a) (wrapu ~size b)
    (* let bisdiv ~size = Integer_Forward.idiv
     * let biudiv ~size = Integer_Forward.idiv
     * let bismod ~size = Integer_Forward.imod
     * let biumod ~size = Integer_Forward.imod *)
    
    let bsext ~size ~oldsize x = x
    let buext ~size ~oldsize x = x

    let bofbool ~size = assert false
    let bchoose ~size cond x = x
                               
    let band ~size = Ival.bitwise_and
    let beq ~size a b = Quadrivalent_basis.Boolean_Lattice.inter
        (Integer_Forward.ieq (wrapu ~size a) (wrapu ~size b))
        (Integer_Forward.ieq (wraps ~size a) (wraps ~size b))
    let biule ~size a b = Integer_Forward.ile (wrapu ~size a) (wrapu ~size b)
    let bisle ~size a b = Integer_Forward.ile (wraps ~size a) (wraps ~size b)
    (* let beq ~size = Integer_Forward.ieq
     * let biule ~size = Integer_Forward.ile
     * let bisle ~size = Integer_Forward.ile *)
    
    module Debug = struct

      let _bisdiv ~size a b =
        let res = bisdiv ~size a b in
        Codex_log.feedback "bisdiv %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res;
        res


      let _biudiv ~size a b =
        let res = biudiv ~size a b in
        Codex_log.feedback "biudiv %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res;
        res

      let _bismod ~size a b =
        let res = bismod ~size a b in
        Codex_log.feedback "bismod %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res;
        res


      let _biumod ~size a b =
        let res = biumod ~size a b in
        Codex_log.feedback "biumod %d: %a %a %a" size Ival.pretty a Ival.pretty b Ival.pretty res;
        res


      let _bisle ~size a b =
        let res = bisle ~size a b in
        Codex_log.feedback "bisle %d: %a %a %a" size Ival.pretty a Ival.pretty b Quadrivalent_basis.Boolean_Lattice.pretty res;
        res


      let _biule ~size a b =
        let res = biule ~size a b in
        Codex_log.feedback "biule %d: %a %a %a" size Ival.pretty a Ival.pretty b Quadrivalent_basis.Boolean_Lattice.pretty res;
        res
      ;;

      let _beq ~size a b =
        let res = beq ~size a b in
        Codex_log.feedback "beq %d: %a %a %a" size Ival.pretty a Ival.pretty b Quadrivalent_basis.Boolean_Lattice.pretty res;
        res
      ;;

    end

    include Debug

    let bor ~size = Ival.bitwise_or
    let bxor ~size = Ival.bitwise_xor
    let buninit ~size = Ival.bottom
    let bunknown ~size = Ival.top
    let bextract ~size ~index ~oldsize x =
      (* Codex_log.feedback "bextracat %d %d %d %a" size index oldsize Ival.pretty x; *)
      Ival.extract_bits ~size:(Z.of_int oldsize) ~start:(Z.of_int index)
        ~stop:(Z.of_int @@ index + size - 1) x

    let bextract ~size ~index ~oldsize x =
      let res = bextract ~size ~index ~oldsize x in
      Codex_log.feedback "Ival_basis.bextract %d %d %d %a %a" size index oldsize Ival.pretty x Ival.pretty res;
      res

    let bconcat ~size1 ~size2 v1 v2 = Ival.add_int (Ival.scale (Z.shift_left Z.one size2) v1) v2

    (* let bconcat ~size1 ~size2 v1 v2 =
     *   wrapu ~size:(size1 + size2) @@ Ival.add_int (Ival.scale (Z.shift_left Z.one size2) v1) v2
     * let bconcat_ ~size1 ~size2 v1 v2 =
     *   let res = bconcat ~size1 ~size2 v1 v2 in
     *   Codex_log.feedback "bconcat %d %d %a %a res %a" size1 size2 Ival.pretty v1 Ival.pretty v2 Ival.pretty res;
     *   res *)

    
    let bitimes ~size k  = Ival.scale k    
    let assume ~size cond x = match cond with
      | B.False | B.Bottom -> Ival.bottom
      | _ -> x
    let valid ~size _ = assert false
    let biconst ~size k  = Ival.inject_singleton k
    let bshl ~size = Ival.shift_left
    let bashr ~size = Ival.shift_right
    let blshr ~size = Ival.shift_right

    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
  end
  module Binary_Backward = struct
    include Basis_noop_transfer_functions.Binary_Backward

    let beq ~size = Integer_Backward.ieq
    let bisle ~size = Integer_Backward.ile
    let biule ~size = Integer_Backward.ile
    let biadd ~size = Integer_Backward.iadd
    let bisub ~size = Integer_Backward.isub
    let bimul ~size = Integer_Backward.imul
    let bitimes ~size = Integer_Backward.itimes
    let bismod ~size = Integer_Backward.imod
    let biumod ~size = Integer_Backward.imod
    let bisdiv ~size = Integer_Backward.idiv
    let biudiv ~size = Integer_Backward.idiv
                          
    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
  end

  type binary = Ival.t
  let binary_to_ival ~size ~signed:_ x = x

  let binary_is_singleton ~size = is_singleton_int

  module Binary_Lattice = struct
    include Integer_Lattice
    let widen ~size ~previous next =
      let res1 = Ival.widen (Z.of_int size,widen_hints) previous next in
      (* Note: with this wrapu, we sometimes enter an infinite loop. *)
      (* let res2 = wrapu ~size @@ res1 in *)
      (* Codex_log.feedback "size %d prev %a next %a res1 %a res2 %a" size Ival.pretty previous Ival.pretty next Ival.pretty res1 Ival.pretty res2; *)
      res1
    let includes_or_widen ~size ~previous next =
      if is_included next previous then (true,next)
      else (false,widen ~size ~previous next)
    let singleton ~size = singleton
  end
end

(* include Unsigned *)
(* include NoWrap *)

module BothWrap = struct

  (* We try to use == whenever possible to avoid consuming too much memory. *)
  type binary = { signed: Ival.t; unsigned: Ival.t }

  (* Reduced product between the signed and unsigned abstraction. Also
     tries to share both when possible. *)
  let reduce ~size x =
    if Integer_Lattice.equal x.signed x.unsigned then x
    else
      (* Fast, common case: we are in the intersection. *)
    if wrapped_u ~size x.signed || wrapped_s ~size x.unsigned
    then
      let res = Ival.narrow x.signed x.unsigned in {signed=res;unsigned=res}
    else
      (* Sometimes ival can learn things, and it is hard to predict why. 
         We need to perform the reduction three times, because each reduction may 
         be beneficial to the other. *)
      let signed = Ival.narrow x.signed @@ wraps ~size x.unsigned in
      let unsigned = Ival.narrow x.unsigned @@ wrapu ~size signed in
      let signed = Ival.narrow signed @@ wraps ~size unsigned in
      if Integer_Lattice.equal signed unsigned
      then {signed; unsigned = signed}
      else {signed;unsigned}
  ;;
  
  let reduce ~size x =
    let res = reduce ~size x in
    assert(Ival.is_included res.signed x.signed && Ival.is_included res.unsigned x.unsigned);
    res
  ;;
  
  module Binary_Lattice = struct

    let is_top_signed ~size x =
      Ival.equal x @@ wraps ~size Ival.top

    let is_top_unsigned ~size x =
      Ival.equal x @@ wrapu ~size Ival.top

    
    let ival_pretty ~size = Ival.pretty;;

    let pretty ~size fmt x = 
      if Ival.equal x.signed x.unsigned then
        ival_pretty ~size fmt x.signed
      else
        match is_top_signed ~size x.signed, is_top_unsigned ~size x.unsigned with
        | true, true -> Format.fprintf fmt "[--..--]"
        | false,true -> ival_pretty  ~size fmt x.signed
        | true, false -> ival_pretty ~size fmt x.unsigned
        | false, false ->
          if Ival.equal x.signed @@ wraps ~size x.unsigned &&
             Ival.equal x.unsigned @@ wrapu ~size x.signed
          then ival_pretty ~size fmt x.signed
          else
            Format.fprintf fmt "{signed: %a; unsigned: %a}"
              (ival_pretty ~size) x.signed (ival_pretty ~size) x.unsigned
    ;;

    let _pretty ~size fmt x =
      Format.fprintf fmt "{size: %d; signed: %a; unsigned: %a}" 
        size Ival.pretty x.signed Ival.pretty x.unsigned
    ;;
    
    let widen_hints = Widen_Hints.default_widen_hints
    let widen ~size ~previous:a b =
      let size_ = Z.of_int size in
      let signed = Ival.widen (size_,widen_hints) a.signed b.signed in
      if a.signed == a.unsigned && b.signed == b.unsigned
      then { signed; unsigned = signed}
      else reduce ~size {signed ; unsigned = Ival.widen (size_,widen_hints)  a.unsigned b.unsigned }

    (* As we have an intersection semantics for signed and unsigned,
       we can usr || on the inclusion tests. *)
    let includes a b =
      Integer_Lattice.includes a.signed b.signed || Integer_Lattice.includes a.unsigned b.unsigned
    
    let includes_or_widen ~size ~previous next =
      if includes previous next then (true,next)
      else (false,widen ~size ~previous next)

    (* Note: we cannot call reduce in join and inter without size
       information; this causes some expressions to be evaluated
       without the maximum precision in some cases. Unfortunately,
       passing the size information here is hard. *)
    let join ~size a b =
      (* Important special case. *)
      if Ival.is_bottom a.signed then b (* assuming b is already reduced. *)
      else
      let signed = Integer_Lattice.join a.signed b.signed in
      if a.signed == a.unsigned && b.signed == b.unsigned
      then { signed; unsigned = signed}
      else  reduce ~size {signed ; unsigned = Integer_Lattice.join a.unsigned b.unsigned }

    let _join ~size a b =
      let res = join ~size a b in
      if not @@ Ival.is_bottom a.signed then Codex_log.feedback "join %a %a res %a" (pretty ~size)a  (pretty ~size) b (pretty ~size) res;
      res
    ;;
    
    let inter ~size a b =
      let signed = Integer_Lattice.inter a.signed b.signed in
      if a.signed == a.unsigned && b.signed == b.unsigned
      then {signed; unsigned = signed}
      else
        reduce ~size {signed;unsigned = Integer_Lattice.inter a.unsigned b.unsigned }
    ;;

    let includes ~size a b = Integer_Lattice.includes a.signed b.signed || Integer_Lattice.includes a.unsigned b.unsigned
    let hash _ = assert false
    let compare a b =
      let x = compare a.signed b.signed in
      if x == 0 then compare a.unsigned b.unsigned
      else x
    ;;

    let bottom ~size = {signed = Ival.bottom; unsigned = Ival.bottom }

    (* TODO: Pre-compute for the most common sizes. *)
    let top ~size =
      let res = { signed = wraps ~size Ival.top;
                  unsigned = wrapu ~size Ival.top; } in
      res
    ;;

    let singleton ~size k =
      (* This test allows to share the value when feasible. *)
      let ku = Z.extract k 0 size in
      let ks = Z.signed_extract k 0 size in
      if Z.equal ku ks
      then let v = Integer_Forward.iconst ks in { signed = v; unsigned = v }
      else
        { signed = Integer_Forward.iconst ks;
          unsigned = Integer_Forward.iconst ku }
    ;;

    type t = binary

    let is_bottom ~size {signed; unsigned} =
      Ival.is_bottom signed || Ival.is_bottom unsigned

    let equal a b = Ival.equal a.signed b.signed && Ival.equal a.unsigned b.unsigned
  end

  let binary_is_singleton ~size {signed;unsigned} =
    try Some (Ival.project_int unsigned)
    with Ival.Not_Singleton_Int -> None

  (* TODO: This should be used as little as possible; at least we
     should give an argument when this is used, to say which
     representation we need.  *)
  let binary_to_ival ~signed ~size x = if signed then x.signed else x.unsigned;;

  let binary_is_empty ~size x =
    (* If the value is reduced testing on either is equivalent. *)
    Ival.is_bottom x.signed
  ;;

  let binary_to_known_bits ~size x =
    let module Known_bits = Lattices.Known_Bits in
    if Ival.is_bottom x.unsigned then
      Known_bits.bottom ~size
    else match Ival.project_int x.unsigned with
    | x -> Known_bits.singleton ~size x (* (x,x) *)
    | exception Not_Singleton_Int -> begin
    match Ival.min_max_r_mod x.unsigned with
      | exception Framac_ival.Abstract_interp.Error_Bottom -> Lattices.Known_Bits.bottom ~size
      | None, _, _, _ | _, None, _, _ -> assert false
      | (Some min,Some max,r,modu) -> begin
          (* (min,max) give an information about the common most significant bits. *)
          let from_min_max =
            (* Find the first bit that differs, and mask the lower bits. *)        
            let xorminmax = Z.(lxor) min max in
            let highest_bit = Z.log2 xorminmax in
            let mask = Z.sub (Z.(lsl) Z.one @@ 1 + highest_bit) Z.one in
            (Z.(lor) min mask, Z.(land) min @@ Z.(~!) mask)
          in
          (* Congruence gives an information about the least significant bits. *)
          let from_congruence =
            (* If modu is some n * 2^k *)
            let k = Z.trailing_zeros modu in
            let mask = Z.sub (Z.(lsl) Z.one k) Z.one in
            (Z.(lor) r @@ Z.(~!) mask, Z.(land) r @@ mask)
          in
          Lattices.Known_Bits.inter ~size from_min_max from_congruence
        end
  end
  ;;
  
  
  (* On the part common to signed and unsigned representation (the
     most common one), we narrow.  *)
  let binary_fold_crop ~size x ~inf ~sup acc f =
    let acc = if Z.lt inf Z.zero then
        integer_fold_crop x.signed ~inf ~sup:Z.zero f acc
      else acc in
    let bound = Z.shift_left Z.one (pred size) in
    let max_signed_int = Z.pred bound in
    let ival = Ival.inject_range (Some Z.zero) (Some max_signed_int) in
    let ival = Ival.narrow ival x.signed in
    let ival = Ival.narrow ival x.unsigned in
    let acc = integer_fold_crop ival ~inf ~sup f acc in
    let acc = integer_fold_crop x.unsigned ~inf:bound ~sup f acc in
    acc
  ;;    
    
  module Binary_Forward = struct
    (* MAYBE: try specializing for zero and one. *)
    let biconst = Binary_Lattice.singleton      
    let beq   ~size a b = Quadrivalent_basis.Boolean_Lattice.inter (Integer_Forward.ieq a.signed b.signed) (Integer_Forward.ieq a.unsigned b.unsigned)
    let biule ~size a b = Integer_Forward.ile a.unsigned b.unsigned
    let bisle ~size a b = Integer_Forward.ile a.signed b.signed        
    let bitimes ~size _ _ = assert false

    let biadd ~size ~nsw ~nuw ~nusw a b =
      Codex_log.debug "Ival_basis.biadd ~nsw:%b ~nuw:%b ~nusw:%b %a %a" nsw nuw nusw (Binary_Lattice.pretty ~size) a (Binary_Lattice.pretty ~size) b ;
      let signed = Integer_Forward.iadd a.signed b.signed in
      if a.signed == a.unsigned && b.signed == b.unsigned
      then reduce ~size { signed = wraps_or_narrow ~nsw ~size signed;
                          unsigned = wrapu_or_narrow ~nuw ~size signed }

      else if nusw then
        let unsigned = Integer_Forward.iadd a.unsigned b.signed in
        reduce ~size { signed = wraps_or_narrow ~nsw ~size signed;
                          unsigned = wrapu_or_narrow ~nuw ~size unsigned }
      else 
        let unsigned = Integer_Forward.iadd a.unsigned b.unsigned in      
        reduce ~size { signed = wraps_or_narrow ~nsw ~size signed;
                       unsigned = wrapu_or_narrow ~nuw ~size unsigned }
    ;;

    let bisub ~size ~nsw ~nuw ~nusw a b =
      let signed = Integer_Forward.isub a.signed b.signed in
      if a.signed == a.unsigned && b.signed == b.unsigned
      then reduce ~size { signed = wraps_or_narrow ~nsw ~size signed;
                          unsigned = wrapu_or_narrow ~nuw ~size signed }
      else 
        let unsigned = Integer_Forward.isub a.unsigned b.unsigned in      
        reduce ~size { signed = wraps_or_narrow ~nsw ~size signed;
                       unsigned = wrapu_or_narrow ~nuw ~size unsigned }
    ;;

    let bimul ~size ~nsw ~nuw a b =
      let signed = Integer_Forward.imul a.signed b.signed in
      if a.signed == a.unsigned && b.signed == b.unsigned
      then reduce ~size { signed = wraps_or_narrow ~nsw ~size signed;
                          unsigned = wrapu_or_narrow ~nuw ~size signed }
      else 
        let unsigned = Integer_Forward.imul a.unsigned b.unsigned in      
        reduce ~size { signed = wraps_or_narrow ~nsw ~size signed;
                       unsigned = wrapu_or_narrow ~nuw ~size unsigned }
    ;;

    
    let bxor ~size a b =
      let signed = Integer_Forward.ixor a.signed b.signed in
      let unsigned = Integer_Forward.ixor a.unsigned b.unsigned in
      (* Ival does not respect this invariant. *)
      (* assert(wrapped_s ~size signed); *)      
      let signed = wraps ~size signed in

      assert(wrapped_u ~size unsigned);
      reduce ~size { signed; unsigned }

    
    let band ~size a b =
      let signed = Integer_Forward.iand a.signed b.signed in
      let unsigned = Integer_Forward.iand a.unsigned b.unsigned in
      (* Ival does not respect this invariant. *)
      (* assert(wrapped_s ~size signed); *)
      let signed = wraps ~size signed in
      assert(wrapped_u ~size unsigned);
      reduce ~size { signed; unsigned }

    
    let bor ~size a b =
      let signed = Integer_Forward.ior a.signed b.signed in
      let unsigned = Integer_Forward.ior a.unsigned b.unsigned in
      (* Ival does not respect this invariant. *)
      (* assert(wrapped_s ~size signed); *)
      let signed = wraps ~size signed in
      assert(wrapped_u ~size unsigned);
      reduce ~size { signed; unsigned }

    let assume ~size _ _ = assert false

    let bsext ~size ~oldsize x =
      let signed = x.signed in
      let unsigned = wrapu ~size signed in
      { signed; unsigned}

    let buext ~size ~oldsize x =
      let unsigned = x.unsigned in
      let signed = wraps ~size unsigned in
      { signed; unsigned}
      
    let bashr ~size x y  =
      let orig_signed = Ival.shift_right x.signed y.signed in
      (* Codex_log.feedback "bashr %a %a res %a" Ival.pretty x.signed Ival.pretty y.signed  Ival.pretty signed; *)
      (* assert(wrapped_s ~size orig_signed); *)  
      (* Not verified by the implementation in Ival, even if we restrict y to be signed.*)
      let signed = wraps ~size orig_signed in
      let unsigned = wrapu ~size orig_signed in
      { signed; unsigned}
    ;;

    let blshr ~size x y  =
      (* when y may be bigger than size, Ival.shift_right returns top *)
      let max = Ival.max_int y.unsigned in
      let may_be_too_big = match max with
        | None -> true
        | Some x -> Z.geq x (Z.of_int size)
      in
      let shift = if may_be_too_big then
          let small = Ival.inject_range (Some Z.zero) (Some (Z.of_int (size - 1))) in
          Ival.narrow y.unsigned small
        else y.unsigned
      in
      let unsigned = Ival.shift_right x.unsigned shift in
      let unsigned = if may_be_too_big then Ival.join Ival.zero unsigned else unsigned in
      (* Codex_log.feedback "blshr %a %a res %a" Ival.pretty x.unsigned Ival.pretty y.unsigned  Ival.pretty unsigned; *)
      assert(wrapped_u ~size unsigned);      
      let signed = wraps ~size unsigned in
      { signed; unsigned}
    ;;

    let bshl ~size ~nsw ~nuw x y  =
      let unsigned = wrapu_or_narrow ~nuw ~size @@ Ival.shift_left x.unsigned y.unsigned in
      let signed = wraps_or_narrow ~nsw ~size @@ Ival.shift_left x.signed y.signed in      
      reduce ~size { signed; unsigned}
    ;;

    let bconcat ~size1 ~size2 a b  =
      let unsigned = Ival.add_int (Ival.scale (Z.shift_left Z.one size2) a.unsigned) b.unsigned in
      assert( wrapped_u ~size:(size1 + size2) unsigned);
      let signed = wraps ~size:(size1 + size2) unsigned in
      { signed; unsigned; }
    ;;

    let bisdiv ~size a b =
      let signed = Ival.div a.signed b.signed in
      assert(wrapped_s ~size signed);
      let unsigned = wrapu ~size signed in
      { signed; unsigned}

    let biudiv ~size a b =
      let unsigned = Ival.div a.unsigned b.unsigned in
      assert(wrapped_u ~size unsigned);
      let signed = wraps ~size unsigned in
      { signed; unsigned}

    let bismod ~size a b =
      let signed = Ival.c_rem a.signed b.signed in
      assert(wrapped_s ~size signed);
      let unsigned = wrapu ~size signed in
      { signed; unsigned}

    let biumod ~size a b =
      let unsigned = Ival.c_rem a.unsigned b.unsigned in
      assert(wrapped_u ~size unsigned);
      let signed = wraps ~size unsigned in
      { signed; unsigned}

    let bextract ~size ~index ~oldsize x =
      if size == oldsize then x
      else
        let stop = (Z.of_int @@ index + size - 1) in
        let oldsize = Z.of_int oldsize in
        let start = Z.of_int index in
        if x.signed == x.unsigned
        then
          let res = Ival.extract_bits ~size:oldsize ~start ~stop x.signed in
          reduce ~size { signed = wraps ~size res; unsigned = wrapu ~size res }
        else
          let ress = wraps ~size @@ Ival.extract_bits ~size:oldsize ~start ~stop x.signed in
          let resu = wrapu ~size @@ Ival.extract_bits ~size:oldsize ~start ~stop x.unsigned in
          reduce ~size { signed = ress; unsigned = resu }
    ;;

    (* let bextract ~size ~index ~oldsize x =
     *   let r = bashr ~size:oldsize x (biconst ~size:oldsize @@ Z.of_int index) in
     *   band ~size:oldsize r (biconst ~size:oldsize @@ Z.pred @@ Z.shift_left Z.one size)
     * ;; *)
      

    

    let _bextract ~size ~index ~oldsize x =
      let res = bextract ~size ~index ~oldsize x in
      Codex_log.feedback "bextract %d %d %d %a res %a"
        size index oldsize
        (Binary_Lattice.pretty ~size:oldsize) x
        (Binary_Lattice.pretty ~size) res;
      res

    let valid ~size _ = assert false
    let valid_ptr_arith ~size _ = assert false
    let buninit ~size = assert false



    let baddr ~size _ = assert false
    let bshift ~size ~offset ~max = assert false
    let bindex ~size _ = assert false

    let bofbool =
      (* For 1, the signed version of 1 is-1. For all others, it will be 1. *)
      let zero= biconst ~size:1 Z.zero in
      let one1 = biconst ~size:1 Z.one in
      let top1 = Binary_Lattice.join ~size:1 zero one1 in

      let one_larger = biconst ~size:2 Z.one in
      let top_larger = Binary_Lattice.join ~size:2 zero one_larger in

      fun ~size x ->
      if size == 1 then
        match x with
        | Quadrivalent_basis.Bottom -> Binary_Lattice.bottom ~size
        | Quadrivalent_basis.Top -> top1
        | Quadrivalent_basis.True -> one1
        | Quadrivalent_basis.False -> zero
      else match x with
        | Quadrivalent_basis.Bottom -> Binary_Lattice.bottom ~size
        | Quadrivalent_basis.Top -> top_larger
        | Quadrivalent_basis.True -> one_larger
        | Quadrivalent_basis.False -> zero
    ;;

    let bchoose ~size cond x = x
    
  end


  
  module Binary_Backward = struct

    include Basis_noop_transfer_functions.Binary_Backward

    let ival_inter_refines = inter_refines
    
    let inter_refines ~size old newer =
      let signed = Ival.narrow old.signed newer.signed in
      assert (Ival.is_included signed old.signed);
      assert (Ival.is_included signed newer.signed);
      if old.signed == old.unsigned && newer.signed == newer.unsigned
      then
        if Ival.equal signed old.signed then None else Some { signed; unsigned = signed}
      else
        let unsigned = Ival.narrow old.unsigned newer.unsigned in
        assert (Ival.is_included unsigned old.unsigned);
        assert (Ival.is_included unsigned newer.unsigned);
        if Ival.equal signed old.signed && Ival.equal unsigned old.unsigned
        then None
        else Some(reduce ~size { signed; unsigned })
    ;;

    
    let bisle ~size a b bool =
      (* Codex_log.feedback "new bisle"; *)
      let g comp1 comp2 = 
        let signeda = Ival.backward_comp_int_left comp1 a.signed b.signed in
        (* Codex_log.feedback "signeda %a" Ival.pretty signeda; *)
        let signedb = Ival.backward_comp_int_left comp2 b.signed a.signed in
        let f signedx x =
          match ival_inter_refines x.signed signedx with
          | None -> None 
          | Some y -> Some (reduce ~size {signed=y;unsigned=x.unsigned}) 
        in
        f signeda a, f signedb b
      in
      match bool with
      | B.True -> g Abstract_interp.Comp.Le Abstract_interp.Comp.Ge
      | B.False -> g Abstract_interp.Comp.Gt Abstract_interp.Comp.Lt
      | B.Bottom -> (if Binary_Lattice.is_bottom ~size a then None else Some (Binary_Lattice.bottom ~size)),
                   (if Binary_Lattice.is_bottom ~size b then None else Some (Binary_Lattice.bottom ~size))
      | B.Top -> None, None
    ;;

    let _bisle ~size a b bool =
      let resa,resb = bisle ~size a b bool in
      Codex_log.feedback "bisle %a %a %a res %a %a"
        (Binary_Lattice.pretty ~size) a (Binary_Lattice.pretty ~size) b Quadrivalent_basis.Boolean_Lattice.pretty bool
        (fun fmt -> function | None -> Format.fprintf fmt "None" | Some x -> (Binary_Lattice.pretty ~size) fmt x) resa
        (fun fmt -> function | None -> Format.fprintf fmt "None" | Some x -> (Binary_Lattice.pretty ~size) fmt x) resb        
      ;
      resa,resb
    ;;
        

    let biule ~size a b bool =
      let g comp1 comp2 = 
        let unsigneda = Ival.backward_comp_int_left comp1 a.unsigned b.unsigned in
        let unsignedb = Ival.backward_comp_int_left comp2 b.unsigned a.unsigned in
        let f unsignedx x =
          match ival_inter_refines x.unsigned unsignedx with
          | None -> None
          | Some y -> Some (reduce ~size {unsigned=y;signed=x.signed})
        in
        f unsigneda a, f unsignedb b
      in
      match bool with
      | B.True -> g Abstract_interp.Comp.Le Abstract_interp.Comp.Ge
      | B.False -> g Abstract_interp.Comp.Gt Abstract_interp.Comp.Lt
      | B.Bottom -> (if  Binary_Lattice.is_bottom ~size a then None else Some (Binary_Lattice.bottom ~size)),
                    (if Binary_Lattice.is_bottom ~size b then None else Some (Binary_Lattice.bottom ~size))
      | B.Top -> None, None
    ;;


    let _biule ~size a b res =
      let newa,newb = biule ~size a b res in
      Codex_log.feedback "biule size %d a %a b %a res %a new_a %a new_b %a"
        size (Binary_Lattice.pretty ~size) a  (Binary_Lattice.pretty ~size) b  (Quadrivalent_Lattice.pretty) res
        (pp_opt @@ Binary_Lattice.pretty ~size) newa  (pp_opt @@ Binary_Lattice.pretty ~size) newb;
      newa,newb
    ;;

    
    let beq ~size a b res = match res with
      | B.True -> inter_refines ~size a b, inter_refines ~size b a
      | B.False -> inter_refines ~size a @@ {signed = Ival.diff_if_one a.signed b.signed;
                                             unsigned = Ival.diff_if_one a.unsigned b.unsigned },
                   inter_refines ~size b @@ {signed = Ival.diff_if_one b.signed a.signed;
                                             unsigned = Ival.diff_if_one b.unsigned a.unsigned }
      | B.Bottom -> inter_refines ~size a @@ Binary_Lattice.bottom ~size,
                   inter_refines ~size b @@ Binary_Lattice.bottom ~size
      | B.Top -> None, None
    ;;

    let biadd ~size ~nsw ~nuw ~nusw a b res =
      inter_refines ~size a (Binary_Forward.bisub ~nsw ~nuw ~size ~nusw res b),
      inter_refines ~size b (Binary_Forward.bisub ~nsw ~nuw ~size ~nusw res a);;

    let biadd ~size ~nsw ~nuw ~nusw a b res =
      if nusw then
        inter_refines ~size a {signed = Ival.diff_if_one a.signed b.signed;
                                unsigned = Ival.diff_if_one a.unsigned b.unsigned},
        inter_refines ~size a {signed = Ival.diff_if_one a.signed b.signed;
                              unsigned = Ival.diff_if_one a.unsigned b.unsigned}

      else biadd ~size ~nsw ~nuw ~nusw a b res

    let _biadd ~size ~nsw ~nuw ~nusw a b res =
      let ra,rb = biadd ~size ~nsw ~nuw ~nusw a b res in
      Codex_log.feedback "Backward biadd %a %a %a res %a %a"
        (Binary_Lattice.pretty ~size) a
        (Binary_Lattice.pretty ~size) b
        (Binary_Lattice.pretty ~size) res
        (fun fmt -> function | None -> Format.fprintf fmt "None" | Some x -> (Binary_Lattice.pretty ~size) fmt x) ra
        (fun fmt -> function | None -> Format.fprintf fmt "None" | Some x -> (Binary_Lattice.pretty ~size) fmt x) rb;
      ra,rb
    ;;

    

    let bisub ~size ~nsw ~nuw ~nusw a b res =
      inter_refines ~size a (Binary_Forward.biadd ~nsw ~nuw ~nusw ~size res b),
      inter_refines ~size b (Binary_Forward.bisub ~nsw ~nuw ~nusw ~size a res);;

    let bsext ~size ~oldsize a res =
      match ival_inter_refines a.signed res.signed with
      | None -> None
      | Some signed -> Some (reduce ~size:oldsize {signed;unsigned=a.unsigned})

    let buext ~size ~oldsize a res =
      match ival_inter_refines a.unsigned res.unsigned with
      | None -> None
      | Some unsigned ->
        Some (reduce ~size:oldsize {unsigned;signed=a.signed})

    (* We assume that we are reduced. *)
    let bofbool ~size a res =
      let rev =
        if Ival.is_bottom res.unsigned then Quadrivalent_Lattice.Bottom
        else match Ival.project_int res.unsigned with
             | exception Ival.Not_Singleton_Int -> Quadrivalent_Lattice.Top
             | x when Z.equal x Z.zero -> Quadrivalent_Lattice.False
             | x when Z.equal x Z.one -> Quadrivalent_Lattice.True
             | _ -> Quadrivalent_Lattice.Bottom
      in
      Quadrivalent_basis.refine ~older:a ~newer:rev
    ;;

        
    let _buext ~size ~oldsize a res =
      let newa = buext ~size ~oldsize a res in
      Codex_log.feedback "buext  size %d oldsize %d a %a res %a new_a %a"
        size oldsize (Binary_Lattice.pretty ~size) a   (Binary_Lattice.pretty ~size) res
        (pp_opt @@ Binary_Lattice.pretty ~size) newa  ;
      newa
    ;;

    (* There are too many references to this one. *)
    let bextract_once = ref true;;
    let bextract ~size ~index ~oldsize x res =
      if !bextract_once then begin
        bextract_once := false;
        Codex_log.warning "No backpropagation for 'bextract'"
      end;
      None
    ;;

    let _bextract ~size ~index ~oldsize x res =
      (* Special case: bextract is a no-op operation because the most
         significant bits are already zero. *)
      if index == 0 && wrapped_u ~size x.unsigned && wrapped_s ~size x.signed then
        inter_refines ~size:oldsize x res
      else begin
      if !bextract_once then begin
        bextract_once := false;
        Codex_log.warning "No backpropagation for 'bextract'"
      end;
      None
    end
    ;;

    let bconcat ~size1 ~size2 a b res =
      let oldsize = size1 + size2 in
      let newb = Binary_Forward.bextract ~oldsize ~size:size2 ~index:0 res in
      let newa = Binary_Forward.bextract ~oldsize ~size:size1 ~index:size2 res in
      inter_refines ~size:size1 a newa, inter_refines ~size:size2 b newb 
    ;;

    let bitimes ~size ~nsw ~nuw k a res =
      let nsw = nsw || wrapped_s ~size @@ Ival.scale k a.signed in
      let nuw = nuw || wrapped_u ~size @@ Ival.scale k a.unsigned in
      if not (nsw || nuw) then None
      else
        let signed =
          if nsw then Ival.scale_div ~pos:false k res.signed else a.signed in
        let unsigned =
          if nuw then Ival.scale_div ~pos:false k res.unsigned else a.unsigned in
        inter_refines ~size a { signed;unsigned}
    ;;
    
    let bimul ~size ~nsw ~nuw a b res =
      (* Bitimes is faster (which shows on the benchmarks) and more
         precise when it applies. *)
      if Binary_Lattice.is_bottom ~size res then None,None
      else if Ival.is_singleton_int a.signed then
        None, bitimes ~size ~nsw ~nuw (Ival.project_int a.signed) b res
      else if Ival.is_singleton_int b.signed then
        bitimes ~size ~nsw ~nuw (Ival.project_int b.signed) a res, None
      else
        (* backward_mult_int_left cannot be called when wrapping. 
           Note: meaning that the abstract computation has not wrapped, independently
           of the passed ~nsw and ~nuw flags. *)
        let nsw = (* nsw || *) wrapped_s ~size @@ Ival.mul a.signed b.signed in
        let nuw = (* nuw || *) wrapped_u ~size @@ Ival.mul a.signed b.signed in
        if not (nsw || nuw)
        then None,None
        else
          let convert = function `Bottom -> Some Ival.bottom | `Value x -> x in
          let asigned,bsigned =
            if nsw then
              let asigned = convert @@ Ival.backward_mult_int_left ~right:b.signed ~result:res.signed in
              let bsigned = convert @@ Ival.backward_mult_int_left ~right:a.signed ~result:res.signed in
              (* Codex_log.feedback "a.signed %a res.signed %a asigned
                 %a bsigned %a" Ival.pretty a.signed Ival.pretty
                 res.signed (pp_opt Ival.pretty) asigned (pp_opt
                 Ival.pretty) bsigned; *)
              asigned,bsigned
            else
              None,None
          in
          let aunsigned,bunsigned =
            if nuw then
              let aunsigned = convert @@ Ival.backward_mult_int_left ~right:b.unsigned ~result:res.unsigned in
              let bunsigned = convert @@ Ival.backward_mult_int_left ~right:a.unsigned ~result:res.unsigned in
              (* Codex_log.feedback "aunsigned %a bsunigned %a" (pp_opt Ival.pretty) aunsigned (pp_opt Ival.pretty) bunsigned; *)
              aunsigned,bunsigned
            else
              None, None
          in
          let f x xsigned xunsigned =
            match xsigned,xunsigned with
            | None,None -> None
            | Some signed, None -> inter_refines ~size x @@ (* reduce ~size *) {signed;unsigned=x.unsigned}
            | None, Some unsigned -> inter_refines ~size x @@ (* reduce ~size *) {signed=x.signed;unsigned}              
            | Some signed, Some unsigned -> inter_refines ~size x @@ (* reduce ~size *) {signed;unsigned}
          in
          f a asigned aunsigned, f b bsigned bunsigned
    ;;

    let _bimul ~size ~nsw ~nuw a b res =
      let ra,rb = bimul ~size ~nsw ~nuw a b res in
      Codex_log.feedback "Backward imul %a %a %a res %a %a"
        (Binary_Lattice.pretty ~size) a
        (Binary_Lattice.pretty ~size) b
        (Binary_Lattice.pretty ~size) res
        (fun fmt -> function | None -> Format.fprintf fmt "None" | Some x -> (Binary_Lattice.pretty ~size) fmt x) ra
        (fun fmt -> function | None -> Format.fprintf fmt "None" | Some x -> (Binary_Lattice.pretty ~size) fmt x) rb;
      ra,rb
    ;;


    let bismod ~size a b res =
      let newa,newb = Integer_Backward.imod a.signed b.signed res.signed in
      let newa = match newa with
        | None -> None
        | Some signed -> Some (reduce ~size {signed;unsigned=a.unsigned})
      in
      let newb = match newb with
        | None -> None
        | Some signed -> Some (reduce ~size {signed;unsigned=b.unsigned})
      in
      newa,newb
    ;;

    let _bismod ~size a b res =
      let newa,newb = bismod ~size a b res in
      Codex_log.feedback "bismod size %d a %a b %a res %a new_a %a new_b %a"
        size (Binary_Lattice.pretty ~size) a  (Binary_Lattice.pretty ~size) b  (Binary_Lattice.pretty ~size) res
        (pp_opt @@ Binary_Lattice.pretty ~size) newa  (pp_opt @@ Binary_Lattice.pretty ~size) newb;
      newa,newb

    let biumod ~size a b res =
      let newa,newb = Integer_Backward.imod a.unsigned b.unsigned res.unsigned in
      let newa = match newa with
        | None -> None
        | Some unsigned -> Some (reduce ~size {unsigned;signed=a.signed})
      in
      let newb = match newb with
        | None -> None
        | Some unsigned -> Some (reduce ~size {unsigned;signed=b.signed})
      in
      newa,newb
    ;;



    let band ~size a b res =
      let bnot a = Binary_Forward.bxor ~size (Binary_Lattice.singleton ~size Z.minus_one) a in
      let refine_a_from_b a b =
        (* For each bit, if a & _ = 1 then a = 1. [a1] is [a] with all such bits at 1
           (for the others, res = 0 and this bitwise_or has no effect on a). *)
        let a1 = Binary_Forward.bor ~size res a in
        (* For each bit, if a & 1 = 0 then a = 0. [a2] is [a] with all such bits at 0
           (for the others, (not b or res) = 1 and this bitwise_and has no effect on a). *)
        (* This could work too. MAYBE: use both? *)        
        (* let a2 = Binary_Forward.band ~size a (bnot (Binary_Forward.bxor ~size res b)) in *)
        let a2 = Binary_Forward.band ~size a @@ Binary_Forward.bor ~size (bnot b) res in
        let a' = Binary_Lattice.inter ~size a1 a2 in
        inter_refines ~size a a'
      in
      refine_a_from_b a b, refine_a_from_b b a

    let bor ~size a b res =
      let bnot a = Binary_Forward.bxor ~size (Binary_Lattice.singleton ~size Z.minus_one) a in
      let refine_a_from_b a b =
        (* For each bit, if a | _ = 0 then a = 0. [a1] is [a] with all such bits at 0
           (for the others, res = 1 and this bitwise_and has no effect on a). *)
        let a1 = Binary_Forward.band ~size res a in
        (* For each bit, if a | 0 = 1 then a = 1. [a2] is [a] with all such bits at 1
           (for the others, (not b and res) = 0 and this bitwise_or has no effect on a). *)
        (* This could work too. MAYBE: use both? *)
        (* let a2 = Binary_Forward.bor ~size a (Binary_Forward.bxor ~size res b) in *)
        let a2 = Binary_Forward.bor ~size a @@ Binary_Forward.band ~size (bnot b) res in
        let a' = Binary_Lattice.inter ~size a1 a2 in
        inter_refines ~size a a'
      in
      refine_a_from_b a b, refine_a_from_b b a
  end


  
  
end

include BothWrap


let convert_to_ival x = x
let is_singleton_int x = 
  try Some( Ival.project_int x )
  with Ival.Not_Singleton_Int -> None

module Integer:Datatype_sig.S with type t = Ival.t = Ival

include Quadrivalent_basis
