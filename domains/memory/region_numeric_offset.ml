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

open Memory_sig;;

(* A simple Address that directly uses the Scalar.binary to
   represent memory locations. *)  
module Make_Address(Scalar:Domain_sig.Base) = struct

  module Scalar = Scalar
  include Scalar
  type offset = binary
  module Offset = struct
    include Scalar.Binary
    let query_boolean = Scalar.query_boolean              
  end
  
  let binary_pretty ~size ctx fmt x =
    Format.fprintf fmt " + %a" (Scalar.binary_pretty ~size ctx) x
  let offset_pretty = binary_pretty
  let offset_empty = Scalar.binary_empty
  
  let serialize ~size ctx a b acc =
    (* Codex_log.feedback "Region_numeric.serialize2 %a %a " *)
    (*   (binary_pretty ~size ctx) a  (binary_pretty ~size ctx) b; *)
    Scalar.serialize_binary ~size ctx a b acc
  let serialize_offset = serialize

  let boolean2scalar_bool _ctx x = x
  let scalar_bool2boolean _ctx x = x

  let ble = Scalar.Binary_Forward.biule
  let beq = Scalar.Binary_Forward.beq
  let zero_offset ~size ~max ctx = Scalar.Binary_Forward.biconst ~size Z.zero ctx

  let is_precise ~size ctx offset =
    let x = Query.binary ~size ctx offset in
    match Query.binary_is_singleton ~size x with
    | None -> Imprecise
    | Some x -> Singleton x

  let in_bounds ~size ctx offset ~inf ~sup =
    let open Framac_ival.Ival in
    let x = Query.binary ~size ctx offset in
    match min_and_max @@ Query.binary_to_ival ~size ~signed:false x with
    | Some i, Some j -> Z.leq inf i && Z.leq j sup
    | _ -> false

  let fold_crop ~size ctx (offset:binary) ~inf ~sup f acc =
    let x = Query.binary ~size ctx offset in
    Query.binary_fold_crop ~size x ~inf:Z.zero ~sup acc (fun int acc ->
        f int acc)

  let bisub = Scalar.Binary_Forward.bisub ~nsw:false ~nuw:false ~nusw:false

  let bshift ~size ~offset ~max ctx x =
    Scalar.Binary_Forward.biadd ~size ~nsw:false ~nuw:false ~nusw:true ctx x
      (Scalar.Binary_Forward.biconst ~size (Z.of_int offset) ctx)

  let bchoose ~size cond ctx x = Scalar.Binary_Forward.bchoose ~size cond ctx x

  let bindex ~size k ctx x i =
    let v =
      if k == 1 then i
      else
        let k = Scalar.Binary_Forward.biconst ~size (Z.of_int k) ctx in
        Scalar.Binary_Forward.bimul ~size ~nsw:false ~nuw:false ctx k i
    in          
    Scalar.Binary_Forward.biadd ~size ~nsw:false ~nuw:false ~nusw:true ctx x v    

  let within_bounds ~size ctx _ = Scalar.Boolean_Forward.true_ ctx

  let binary_unknown_typed ~size:_ _ctx _typ = assert false
  let has_type ~size:_ _ctx _typ = assert false
  
  let binary2scalar_binary ~size _ctx value = assert false
  let assume_type ~size _ctx value typ = assert false
  let type_of ~size _ctx value = assert false

  let global_symbol ctx symb = assert false
  let add_global_symbol ~size ctx name binary = assert false

  let analyze_summary _ctx funtyp args = assert false

  module Offset_transfer_functions = struct
    let offset_eq = beq
    let offset_le = ble
    let offset_zero = zero_offset
    let offset_sub = bisub
    let offset_index = bindex
    let offset_shift = bshift
    let offset_within_bounds = within_bounds
    let offset_choose = bchoose
  end
  include Offset_transfer_functions  
  let query_boolean = Scalar.query_boolean

  let contained_addresses ~size:_ _ = assert false
end


module Make(Scalar:Domain_sig.Base)(* :Memory_domain with module Scalar = Scalar *) = struct
  module Scalar = Scalar
  module Offset = Make_Address(Scalar)
  module Address = Offset
  module Memory = Fully_expanded_finite_region.Memory(Offset)
end
