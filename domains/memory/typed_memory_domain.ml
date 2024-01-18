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

module Ctypes = Types.Ctypes
module List = Extstdlib.List

module type S = sig
  module Sub : Memory_sig.Memory_domain
 (* module TS : Ctypes.Type_settings *)

  include Memory_sig.Memory_domain with module Address.Context = Sub.Address.Context

  type binary = Sub.Address.binary

end

module Make
    (Scalar:Domain_sig.Base)
    (Sub:Memory_sig.Memory_domain with module Address.Context = Scalar.Context
                                   and module Address.Scalar = Scalar)
    (* (TS : Ctypes.Type_settings) *)
: S with module Sub = Sub (* and module TS = TS *) = struct

  module Sub = Sub
  (* module TS = TS *)

  type binary = Sub.Address.binary

  (* module SubAddress = Sub.Address *)
  (* module SubMemory = Sub.Memory *)

  module Type = struct

    module BR = Scalar (* Sub.Address.Scalar *)
    module Context = BR.Context      

    type t = PtrT of {
      ptyp : Ctypes.typ;
      idx : BR.binary;
      fe : BR.binary;
      ofs : int;
      non_aligned: BR.binary;
      ppred : Ctypes.Pred.t;
    }

    let offset_size = 32
    let array_index_size = 32

    exception Type_error of string

    let lift_unop ~size ctx op =
      let open Ctypes.Pred in
      let open BR.Binary_Forward in
      match op with
      | Extract (index,len) -> fun x ->
        bextract ~size:len ~index ~oldsize:size ctx x
        |> buext ~size ~oldsize:len ctx
  
    let lift_binop ~size ctx op =
      let open Ctypes.Pred in
      let open BR.Binary_Forward in
      match op with
      | Add -> biadd ~size ctx ~nsw:false ~nuw:false ~nusw:false
      | Sub -> bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false
      | Mul -> bimul ~size ctx ~nsw:false ~nuw:false
      | And -> band ~size ctx
      | Concat (size1,size2) -> bconcat ~size1 ~size2 ctx
      | Or -> bor ~size ctx
  
    let cond_of_cmp ~size ctx cmpop v1 v2 =
      let open Ctypes.Pred in
      match cmpop with
      | Equal ->
          BR.Binary_Forward.beq ~size ctx v1 v2
      | NotEqual ->
          BR.Boolean_Forward.not ctx @@ BR.Binary_Forward.beq ~size ctx v1 v2
      | ULt ->
          BR.Boolean_Forward.not ctx @@ BR.Binary_Forward.biule ~size ctx v2 v1
      | SLt ->
          BR.Boolean_Forward.not ctx @@ BR.Binary_Forward.bisle ~size ctx v2 v1
      | ULeq ->
          BR.Binary_Forward.biule ~size ctx v1 v2
      | SLeq ->
          BR.Binary_Forward.bisle ~size ctx v1 v2
      | UGeq ->
          BR.Binary_Forward.biule ~size ctx v2 v1
      | SGeq ->
          BR.Binary_Forward.bisle ~size ctx v2 v1
      | UGt ->
          BR.Boolean_Forward.not ctx @@ BR.Binary_Forward.biule ~size ctx v1 v2
      | SGt ->
          BR.Boolean_Forward.not ctx @@ BR.Binary_Forward.bisle ~size ctx v1 v2
  
    exception Global_symbol_not_found
  
    let rec global_constants =
      let table = ref ([] : (string * (int * BR.binary)) list) in
      let initialized = ref false in
      fun ctx ->
        if !initialized then table
        else begin
          initialized := true;
          table
        end
  
    and symbol ctx s = (* TODO : some with and some without assume caution *)
      try List.assoc s (!(global_constants ctx))
      with Not_found ->
        Format.printf "cannot find global symbolic variable : %s\n" s ;
        raise Global_symbol_not_found

    and add_symbol ~size ctx name binary =
      let table = global_constants ctx in
      table := (name, (size, binary))::(!table)
  
    and binary_of_value ~size ctx v =
      let open Ctypes in
      match v with
      | Const x -> BR.Binary_Forward.biconst ~size x ctx
      | Sym s ->
          let sz,v = symbol ctx s in
          assert (sz = size);
          v
  
    and binary_of_expr ~size ctx self e =
      let open Ctypes.Pred in
      match e with
      | Self -> self
      | Val v -> binary_of_value ~size ctx v
      | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx self e)
      | Binop (op, e1, e2) -> lift_binop ~size ctx op
          (binary_of_expr ~size ctx self e1) (binary_of_expr ~size ctx self e2)
  
    and cond_of_pred_subdomain ~size ctx pred v =
      let open Ctypes.Pred in
      match pred with
      | True -> BR.Boolean_Forward.true_ ctx
      | Cmp (op, e1, e2) ->
          cond_of_cmp ~size ctx op (binary_of_expr ~size ctx v e1) (binary_of_expr ~size ctx v e2)
      | And (p,q) ->
          let c1 = cond_of_pred_subdomain ~size ctx p v in
          let c2 = cond_of_pred_subdomain ~size ctx q v in
          BR.Boolean_Forward.(&&) ctx c1 c2
  
    and check_invariant_subdomain ~size ctx pred v =
      let c = cond_of_pred_subdomain ~size ctx pred v in
      let is_true = BR.query_boolean ctx c in
      match is_true with
      | Lattices.Quadrivalent.True -> true
      | _ -> false (* Possibly false *)
  
    and use_invariant_subdomain ~size ctx pred (value : BR.binary) =
      let open Ctypes.Pred in
      match pred with
      | True -> value
      | _ ->
          BR.imperative_assume ctx (cond_of_pred_subdomain ~size ctx pred value) ;
          value
    

    let pp_pointed_type ctx fmt ptyp =
      let open Ctypes in
      match ptyp.descr with
      | Array (typ, Some (Sym s)) ->
          let sz = snd @@ symbol ctx s in 
          Format.fprintf fmt "@[<hov 2>%a[%a]@]"
              Ctypes.pp typ (BR.binary_pretty ~size:offset_size ctx) sz
      
      | _ -> Ctypes.pp fmt ptyp


    let pp ctx fmt (PtrT{ptyp;idx;fe=_;ofs;non_aligned;ppred}) =
      let size = offset_size in
      let na_is_zero =
        match BR.Query.(binary_is_singleton ~size @@ binary ~size ctx non_aligned) with
        | None -> false | Some x -> Z.equal x Z.zero
      in
      match ppred, na_is_zero with
      | Ctypes.Pred.True, true ->
            Format.fprintf fmt "@[<hov 2>(%a)[%a].%d*@]"
            (* Ctypes.pp ptyp *)
            (pp_pointed_type ctx) ptyp
            (BR.binary_pretty ~size:offset_size ctx) idx
            ofs
      | Ctypes.Pred.True, false ->
            Format.fprintf fmt "@[<hov 2>(%a)[%a].%d* + %a@]"
            (* Ctypes.pp ptyp *)
            (pp_pointed_type ctx) ptyp
            (BR.binary_pretty ~size:offset_size ctx) idx
            ofs
            (BR.binary_pretty ~size:offset_size ctx) non_aligned
      | _, true ->
        Format.fprintf fmt "@[<hov 2>({self :@ %a[%a].(%d)*@ |@ %a})@]"
          (* Ctypes.pp ptyp *)
          (pp_pointed_type ctx) ptyp
          (BR.binary_pretty ~size:offset_size ctx) idx
          ofs
          Ctypes.Pred.pp ppred
      | _, false ->
        Format.fprintf fmt "@[<hov 2>({self :@ %a[%a].(%d)*@ |@ %a} + %a)@]"
          (* Ctypes.pp ptyp *)
          (pp_pointed_type ctx) ptyp
          (BR.binary_pretty ~size:offset_size ctx) idx
          ofs
          Ctypes.Pred.pp ppred
          (BR.binary_pretty ~size:offset_size ctx) non_aligned


    let index_zero ctx =
      BR.Binary_Forward.biconst ~size:array_index_size Z.zero ctx

    let index_minus_one ctx =
      BR.Binary_Forward.biconst ~size:array_index_size (Z.sub Z.zero Z.one) ctx


    (** Returns the type at a given offset of a non-scalar type.
      @param binary zero a [BR.binary] of value zero and of size
      [Type.offset_size].
      @return a triple (subtyp, idx, ofs) such that [offset] is in [subtyp] at
      index [idx] (if [subtyp] is an array) and offset [ofs].
    *)
    let type_at_offset typ offset : Ctypes.typ * int =
      Codex_log.debug ~level:2 "type_at_offset.@ typ =@ %a.@ offset =@ %d"
        Ctypes.pp typ
        offset;
      let open Ctypes in
      let typ = inlined typ in
      match typ.descr with
      | Structure st ->
        begin
          let members =
            List.sort_uniq (fun (x,_,_) (y,_,_) -> Stdlib.compare x y) st.st_members in
          let struct_size =
            match st.st_byte_size with Some s -> s | None -> assert false in
          let rec loop cur_field = function
            | (ofs,_,_) as field :: fields when offset >= ofs -> loop field fields
            | _ ->
                let (prec_ofs,fname,ftyp) = cur_field in
                if offset >= struct_size then begin
                  let msg = Format.asprintf "Offset %d not found in structure %a."
                    offset Ctypes.pp typ in
                  raise (Type_error msg)
                end;
                Codex_log.debug ~level:1 "Access to field %s : %a (offset %d) of struct %a"
                  fname
                  Ctypes.pp ftyp
                  prec_ofs
                  Ctypes.pp typ;
                ftyp, offset - prec_ofs
          in
          let fst_field = try List.hd members with Failure _ ->
            raise (Invalid_argument "type_at_offset: empty structure") in
          loop fst_field (List.tl members)
        end
      | Array (elem_typ, Some (Const nb_elem)) ->
          let elem_sz = Ctypes.sizeof elem_typ in
          let array_index = offset / elem_sz in
          (* Already emitted in Dba2Codex *)
          (*Codex_log.check "array_offset_access";*)
          if not (array_index >= 0 && Z.lt (Z.of_int array_index) nb_elem) then begin
            Codex_log.alarm "array_offset_access";
            Codex_log.error "@[<hov 2>Out-of-bounds access at index %d in array of \
              size %a.@]" array_index Z.pp_print nb_elem
          end else
            Codex_log.debug ~level:1 "@[<hov 2>Access at index %d in array of size %a@]"
              array_index Z.pp_print nb_elem
          ;
          let offset_in_elem = offset mod elem_sz in
          elem_typ,
          offset_in_elem
      | Array (_, Some (Sym _)) ->
          raise (Invalid_argument "type_at_offset: Unhandled array with symbolic size")
      | Array (_, None) ->
          raise (Invalid_argument "type_at_offset: Array without a size")

      | Name name -> failwith ("Use of a non-inlined named type " ^ name)
      | Application _ -> failwith ("Use of a non-applied constructor type")

      | _ when offset = 0 ->
          (* This is a scalar type and does not have any fields. *)
          raise Exit
      | _ ->
          let msg = Format.asprintf "@[<hov 2>Access in type %a \
            with non-zero offset %d is not allowed@]" Ctypes.pp typ offset in
          raise (Type_error msg)

    (** Like {!type_at_offset} but also takes an index and an [fe] and does
      array bound checking, if need be. This will typically be needed for the
      first loss of precision on a pointer, but not for the following steps,
      because structures do not contain arrays of unknown size directly.
      Returns optionally an alarm message if out-of-array occurred. *)
    let type_at_offset_complex ctx typ idx fe offset : Ctypes.typ * int * string option =
      let open Ctypes in
      Codex_log.debug ~level:1 "type_at_offset_complex.@ typ =@ %a.@ idx =@ %a.@ fe =@ %a.@ offset =@ %d"
        Ctypes.pp typ
        (BR.binary_pretty ~size:offset_size ctx) idx
        (BR.binary_pretty ~size:offset_size ctx) fe
        offset;
      match (inlined typ).descr with
      | Array (elem_typ, None) ->
          let elem_sz = Ctypes.sizeof elem_typ in
          let array_index_diff = offset / elem_sz in
          let offset_in_elem = offset mod elem_sz in
          (* Perform true euclidean division in case [offset] is negative. *)
          let array_index_diff, offset_in_elem =
            if offset < 0 then array_index_diff - 1, offset_in_elem + elem_sz
            else array_index_diff, offset_in_elem in
          Codex_log.debug ~level:2 "array_index_diff = %d,@ offset_in_elem =@ %d" array_index_diff offset_in_elem;
          (* I don't remember why I restricted {offset_in_elem} to zero. *)
          (*
          if offset_in_elem = 0 then begin
            Codex_log.result "@[<hov 2>Array access at index %d yields %a@]"
              array_index Ctypes.pp elem_typ;
            elem_typ, offset_in_elem
          end else
            let msg = Format.asprintf "@[<hov 2>Access at \
              non-congruent offset %d (elem size is %d, elem type is %a)@]"
              offset elem_sz Ctypes.pp elem_typ in
            raise (Type_error msg)
          *)
          let array_index = BR.Binary_Forward.(biadd ~size:offset_size ctx idx
            ~nsw:false ~nuw:false ~nusw:false
            (biconst ~size:offset_size (Z.of_int array_index_diff) ctx)) in
          let index_zero = check_invariant_subdomain ~size:array_index_size ctx
            (Ctypes.Pred.eq (Const Z.zero)) array_index in
          let alarm =
            if not index_zero then begin
              (* Already emitted in Dba2Codex *)
              (*Codex_log.check "array_offset_access";*)
              let msg = Format.asprintf "Access at non-zero index %a in array of unknown size, \
                assuming safe" (BR.binary_pretty ~size:offset_size ctx) array_index in
              Some msg
            end else
              None
          in
          Codex_log.debug ~level:1 "@[<hov 2>Array access at index %a yields %a@]"
            (BR.binary_pretty ~size:offset_size ctx) array_index Ctypes.pp elem_typ;
          elem_typ, offset_in_elem, alarm
      | Array (elem_typ, Some nb_elem) ->
          let elem_sz = Ctypes.sizeof elem_typ in
          let array_index_diff = offset / elem_sz in
          let offset_in_elem = offset mod elem_sz in
          (* Perform true euclidean division in case [offset] is negative. *)
          let array_index_diff, offset_in_elem =
            if offset < 0 then array_index_diff - 1, offset_in_elem + elem_sz
            else array_index_diff, offset_in_elem in
          Codex_log.debug ~level:2 "array_index_diff = %d,@ offset_in_elem =@ %d" array_index_diff offset_in_elem;
          let array_index = BR.Binary_Forward.(biadd ~size:offset_size ctx idx
            ~nsw:false ~nuw:false ~nusw:false
            (biconst ~size:offset_size (Z.of_int array_index_diff) ctx)) in
          let new_fe = BR.Binary_Forward.(biadd ~size:offset_size ctx fe
            ~nsw:false ~nuw:false ~nusw:false
            (biconst ~size:offset_size (Z.of_int array_index_diff) ctx)) in

          let lt_size =
            check_invariant_subdomain ~size:array_index_size ctx
              Ctypes.Pred.(slt (Const Z.zero)) new_fe ||
            (* [i <u s] implies that [i <s s], provided that [s >=s 0].
            * Here we assume [s >=s 0].
            * [i <u s] also implies [i <=s s], which is what we
            * need to verify. Incidentally, [i <u s] is the
            * condition that will be the easiest to verify with our
            * constraint domain and hypotheses on array sizes. *)
            (* XXX: factorize this by using [valid] here. *)
            check_invariant_subdomain ~size:array_index_size ctx 
              Ctypes.Pred.(ult nb_elem) array_index
            || check_invariant_subdomain ~size:array_index_size ctx 
              Ctypes.Pred.(slt nb_elem) array_index
          in
          let geq_zero =
            check_invariant_subdomain ~size:array_index_size ctx
              Ctypes.Pred.(sgeq (Const Z.zero)) array_index in

          let lt_size2 =
            check_invariant_subdomain ~size:array_index_size ctx
              Ctypes.Pred.(slt (Const Z.zero)) new_fe ||
            (* [i <u s] implies that [i <s s], provided that [s >=s 0].
              * Here we assume [s >=s 0].
              * [i <u s] also implies [i <=s s], which is what we
              * need to verify. Incidentally, [i <u s] is the
              * condition that will be the easiest to verify with our
              * constraint domain and hypotheses on array sizes. *)
            (* XXX: factorize this by using [valid] here. *)
            check_invariant_subdomain ~size:array_index_size ctx 
              (* Ctypes.Pred.(ult nb_elem) array_index *)
              Ctypes.Pred.(slt nb_elem) array_index
          in

          Codex_log.debug "lt_size (unsigned) : %b" lt_size ;
          Codex_log.debug "lt_size (signed) : %b" lt_size2 ;
          Codex_log.debug "geq_zero : %b" geq_zero ;

          let out_of_bounds = not (geq_zero && lt_size) in
          Codex_log.debug "is array access out of bounds : %b" out_of_bounds ;

          let alarm =
            if not (geq_zero && lt_size) then begin
              let msg = Format.asprintf "@[<hov 2>Out-of-bounds access at index %a in array of \
                size %a.@]" (BR.binary_pretty ~size:offset_size ctx) array_index
                (fun fmt n -> match n with
                  | Const x -> Z.pp_print fmt x
                  | Sym s -> BR.binary_pretty ~size:array_index_size ctx fmt (snd @@ symbol ctx s)) nb_elem in
              Some msg
            end else begin
              Codex_log.debug ~level:1 "@[<hov 2>Access at index %a in array of size %a@]"
                (BR.binary_pretty ~size:offset_size ctx) array_index
                Ctypes.pp_value nb_elem;
              None
            end
          in
          elem_typ,
          offset_in_elem,
          alarm

      | Name name -> failwith ("Use of a non-inlined named type " ^ name)
      | Application _ -> failwith ("Use of a non-applied constructor type")
      (* | Existential _ -> failwith ("Use of non-instantiated existential type") *)

      | _ ->
          let typ, ofs = type_at_offset typ offset in
          typ, ofs, None

    let focus_idx ctx upper_bound index =
    let size = array_index_size in
    let open Ctypes in
    match index, upper_bound with
    | Zero, None ->
        BR.Binary_Forward.biconst ~size Z.zero ctx,
        use_invariant_subdomain ~size ctx Pred.(slt (Const Z.zero)) @@
          BR.binary_unknown ~size ctx
    | Zero, Some upper ->
        let zero = BR.Binary_Forward.biconst ~size Z.zero ctx in
        zero,
        BR.Binary_Forward.bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false zero @@
          binary_of_value ~size ctx upper
    | ValidUnknown, None ->
        use_invariant_subdomain ~size ctx Pred.(sgeq (Const Z.zero)) @@
        BR.binary_unknown ~size ctx,
        use_invariant_subdomain ~size ctx Pred.(slt (Const Z.zero)) @@
        BR.binary_unknown ~size ctx
    | ValidUnknown, Some upper ->
        let upper = binary_of_value ~size ctx upper in
        Codex_log.debug ~level:3 "upper = %a" (BR.binary_pretty ~size ctx) upper;
        let idx = BR.binary_unknown ~size ctx in
        Codex_log.debug ~level:3 "idx = %a" (BR.binary_pretty ~size ctx) idx;
        let idx = use_invariant_subdomain ~size ctx Pred.(sgeq (Const Z.zero)) idx in
        Codex_log.debug ~level:3 "idx = %a" (BR.binary_pretty ~size ctx) idx;
        BR.imperative_assume ctx (BR.Boolean_Forward.not ctx @@ BR.Binary_Forward.bisle ~size ctx upper idx) ;
        Codex_log.debug ~level:3 "idx = %a" (BR.binary_pretty ~size ctx) idx;
        let fe = BR.Binary_Forward.bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false idx upper in
        Codex_log.debug ~level:3 "fe = %a" (BR.binary_pretty ~size ctx) fe;
        let fe = use_invariant_subdomain ~size ctx Pred.(slt (Const Z.zero)) fe in
        Codex_log.debug ~level:3 "fe = %a" (BR.binary_pretty ~size ctx) fe;
        idx, fe
    | PastEnd, None ->
        use_invariant_subdomain ~size ctx Pred.(sgt (Const Z.zero)) @@
          BR.binary_unknown ~size ctx,
        index_zero ctx
    | PastEnd, Some upper ->
        let upper = binary_of_value ~size ctx upper in
        let idx = use_invariant_subdomain ~size ctx Pred.(sgt (Const Z.zero)) @@
          BR.binary_unknown ~size ctx in
        BR.imperative_assume ctx (BR.Binary_Forward.beq ~size ctx upper idx) ;
        idx,
        index_zero ctx

    | _ -> assert false

    let binary_min ~size b =
      let inf = Z.sub Z.zero (Z.shift_left Z.one size) in
      let sup = Z.shift_left Z.one size in
      let exception E of Z.t in
      try BR.Query.binary_fold_crop ~size b ~inf ~sup None (fun x ->
        raise (E x)
      )
      with E x -> Some x

    let add_offset' ctx pointed_t old_index old_fe old_offset old_non_aligned (offset : BR.binary) : Ctypes.typ * BR.binary * BR.binary * int * BR.binary =
      Codex_log.debug ~level:2 "add_offset' %a ~old_offset:%d ~old_index:%a ~old_fe:%a ~old_non_aligned:%a %a"
          Ctypes.pp pointed_t
          old_offset
          (BR.binary_pretty ~size:offset_size ctx) old_index
          (BR.binary_pretty ~size:offset_size ctx) old_fe
          (BR.binary_pretty ~size:offset_size ctx) old_non_aligned
          (BR.binary_pretty ~size:offset_size ctx) offset;
      let open Ctypes in
      match pointed_t.descr with
      | Array (elem_t, size) ->
          let ofs_size = offset_size in
          let elem_sz = Ctypes.sizeof elem_t in
          let elem_sz_b = BR.Binary_Forward.biconst ~size:ofs_size
            (Z.of_int elem_sz) ctx in
          let delta = BR.Binary_Forward.(biadd ~size:ofs_size ctx ~nsw:false ~nuw:false ~nusw:false (* delta = old_offset + offset *)
            offset
            (biconst ~size:ofs_size (Z.of_int old_offset) ctx)) in
          Codex_log.debug "in TMD.add_offset, delta = %a" (BR.binary_pretty ~size:ofs_size ctx) delta ;
          let delta = BR.Binary_Forward.biadd ~size:ofs_size ctx ~nsw:false ~nuw:false ~nusw:false delta old_non_aligned in (* delta = old_offset + offset + non_aligned *)
          Codex_log.debug "in TMD.add_offset, delta = %a" (BR.binary_pretty ~size:ofs_size ctx) delta ;
          let index_diff = BR.Binary_Forward.bisdiv ~size:ofs_size ctx delta elem_sz_b in (* index_diff = (old_offset + offset + non_aligned) / size *)
          Codex_log.debug "in TMD.add_offset, index_diff = %a" (BR.binary_pretty ~size:ofs_size ctx) index_diff ;
          let new_offset = BR.Binary_Forward.bismod ~size:ofs_size ctx delta elem_sz_b in (* new_offset = (old_offset ° offset + non_aligned) mod size *)
          Codex_log.debug "in TMD.add_offset, new_offset = %a" (BR.binary_pretty ~size:ofs_size ctx) new_offset ;
          let new_offset_lattice = BR.Query.binary ~size:ofs_size ctx new_offset in
          let zero = BR.Binary_Forward.biconst ~size:ofs_size Z.zero ctx in
          (* let new_offset =
            match BR.Query.(binary_is_singleton ~size:ofs_size new_offset_lattice) with
            | None ->
                Codex_log.check "array_offset_add";
                Codex_log.alarm "array_offset_add";
                let msg =
                  let size = ofs_size in
                  Format.asprintf "@[<hov 2>Dereference with imprecise offset \
                    (index diff %a, new offset in type %a) is not allowed@]"
                    (BR.binary_pretty ctx ~size) index_diff
                    (BR.binary_pretty ctx ~size) new_offset in
                raise (Type_error msg)
            | Some x -> Z.to_int x
          in *)
          Codex_log.debug "in TMD.add_offset, new_offset = %a" (BR.binary_pretty ~size:ofs_size ctx) new_offset ;

          let new_offset, new_non_aligned, imprecise_add = 
            match BR.Query.(binary_is_singleton ~size:ofs_size new_offset_lattice) with
            | None -> 0, delta, true
            | Some x -> Z.to_int x (* @@ Z.signed_extract x 0 ofs_size *), BR.Binary_Forward.biconst ~size:offset_size Z.zero ctx, false
          in

          Codex_log.debug "in TMD.add_offset, new_offset = %d" new_offset ;
          Codex_log.debug "in TMD.add_offset, new_non_aligned = %a" (BR.binary_pretty ~size:ofs_size ctx) new_non_aligned ;

          (* If [new_offset] represents a negative integer (on 32 bits),
          * first compute that negative integer. Then, it means that [delta]
          * was negative, and we adjust the values to reflect a real
          * euclidean division. *)
          (* XXX: this code only works in a 64-bit OCaml runtime. *)
          let new_offset, index_diff =
            if new_offset >= 1 lsl 31 then
              assert false
              (* elem_sz + (new_offset - (1 lsl 32)), BR.Binary_Forward.(bisub ~size:ofs_size
                ~nsw:false ~nuw:false ctx index_diff (biconst ~size:ofs_size Z.one ctx)) *)
            else new_offset, index_diff
          in

          Codex_log.debug "in TMD.add_offset, index_diff = %a" (BR.binary_pretty ~size:ofs_size ctx) index_diff ;

          let index_diff_zero = BR.Binary_Forward.beq ~size:ofs_size ctx
            index_diff zero |> BR.query_boolean ctx in
          let index_diff_zero = match index_diff_zero with
            | Lattices.Quadrivalent.True -> true
            | _ -> false
          in
          (* In this function, we allow the new pointer to point past the end
          * of the array, but no further. I.e., if ar is of size s, the
          * pointer may point between ar[s] (included) and ar[s+1]
          * (excluded), but no further, without triggering an alarm. Of
          * course, it is still an invalid pointer, and trying to
          * dereference it will cause an alarm. *)
          (* We assume that the addition does not wrap. If it does, this will
          * be caught by the "array_offset_add" alarm below. *)
          
          if index_diff_zero || imprecise_add then
            pointed_t, old_index, old_fe, new_offset, new_non_aligned
          else begin
            let new_index = BR.Binary_Forward.biadd ~size:ofs_size ~nsw:true
              ~nuw:false ~nusw:false ctx old_index index_diff in
            let new_fe = BR.Binary_Forward.biadd ~size:ofs_size ~nsw:false
              ~nuw:false ~nusw:false ctx old_fe index_diff in

            Codex_log.debug "in TMD.add_offset, new_index = %a" (BR.binary_pretty ~size:ofs_size ctx) new_index ;
            Codex_log.debug "in TMD.add_offset, new_fe = %a" (BR.binary_pretty ~size:ofs_size ctx) new_fe ;
            
            Codex_log.check "array_offset_add";
            
            let c1 = check_invariant_subdomain ~size:array_index_size ctx
              Pred.(sgeq (Const Z.zero)) new_index in
            (* Just past the end is allowed, so this is Leq and not Lt *)
            let c2 =
              check_invariant_subdomain ~size:array_index_size ctx
                Pred.(sleq (Const Z.zero)) new_fe
              || match size with
                | Some s ->
                    (* [i <u s] implies that [i <s s], provided that [s >=s 0].
                      * Here we assume [s >=s 0].
                      * [i <u s] also implies [i <=s s], which is what we
                      * need to verify. Incidentally, [i <u s] is the
                      * condition that will be the easiest to verify with our
                      * constraint domain and hypotheses on array sizes. *)
                    (* check_invariant_subdomain ~size:array_index_size ctx Pred.(ult s) new_index *)
                    (* If that it false, we will also try to see if this is
                      * true (useful only for [s_stack_descriptor.size]. *)
                    (* || check_invariant_subdomain ~size:array_index_size ctx (Pred.eq s) @@
                        BR.Binary_Forward.biadd ~size:array_index_size ~nsw:false ~nuw:false ~nusw:false ctx new_index @@
                        BR.Binary_Forward.biconst ~size:array_index_size Z.one ctx *)
                    check_invariant_subdomain ~size:array_index_size ctx Pred.(ult s) new_index
                    || check_invariant_subdomain ~size:array_index_size ctx Pred.(slt s) new_index (* TODO : simply by checking (slt s) and (s > 0) directly in ult *)
                    || check_invariant_subdomain ~size:array_index_size ctx Pred.(eq s) new_index
                    (* check_invariant_subdomain ~size:array_index_size ctx (Pred.sleq s) new_index *)
                | None -> false
            in
            (* if c1 && c2
            then pointed_t, new_index, new_fe, new_offset
            else begin
              Codex_log.alarm "array_offset_add";
              Codex_log.error "Array index %a (fe %a) possibly out of bounds."
                (BR.binary_pretty ctx ~size:ofs_size) new_index
                (BR.binary_pretty ctx ~size:ofs_size) new_fe;
              let new_index_sng = BR.Query.binary_is_singleton ~size:offset_size @@
                BR.Query.binary ~size:offset_size ctx new_index in
              let new_fe_sng = BR.Query.binary_is_singleton ~size:offset_size @@
                BR.Query.binary ~size:offset_size ctx new_fe in
              let new_index', new_fe' =
                (* We assume the index is in the bounds or just past the end,
                * except if its cardinal is one, in which case the problem
                * is probably that this code is not well understood (e.g.
                * hardcoded index -1 at 1200535c. *)
                if (new_index_sng, new_fe_sng) = (None,None) then begin
                  Codex_log.warning "Assuming index is in bounds.";
                  use_invariant_subdomain ~size:array_index_size ctx Pred.(sgeq (Const Z.zero))
                    new_index,
                  use_invariant_subdomain ~size:array_index_size ctx Pred.(sleq (Const Z.zero))
                    new_fe
                end else new_index, new_fe
              in
              pointed_t, new_index', new_fe', new_offset
            end
            *)
            if c1 && c2 then (
              (* Codex_log.alarm "array_offset_add";
              Codex_log.error "Array index %a (fe %a) possibly out of bounds." *)
              Codex_log.warning "Array index %a (fe %a) possibly out of bounds."
                (BR.binary_pretty ctx ~size:ofs_size) new_index
                (BR.binary_pretty ctx ~size:ofs_size) new_fe;
            );
            pointed_t, new_index, new_fe, new_offset, new_non_aligned
            
          end
      | stripped_pointed_t ->
          let t_sz = 8 * Ctypes.sizeof pointed_t in
          let ofs_size = offset_size in
          let t_sz_b = BR.Binary_Forward.biconst ~size:ofs_size (Z.of_int t_sz) ctx in
          let zero = BR.Binary_Forward.biconst ~size:ofs_size Z.zero ctx in
          let total_b =
            let size = ofs_size in BR.Binary_Forward.(
              biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx offset @@
              biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx old_non_aligned @@
              biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx (biconst ~size (Z.of_int old_offset) ctx) @@
              bimul ~size ~nsw:false ~nuw:false ctx old_index t_sz_b
            ) in
          let total_lattice = BR.Query.binary ~size:ofs_size ctx total_b in
          begin match BR.Query.binary_is_singleton ~size:ofs_size total_lattice with
          | None ->
            (*
            Codex_log.debug ~level:3 "Imprecise offset. Attempt to weaken the type \
              to find an array...";
            (* Can be improved. Notably, this weakening could be performed more
            * than once and only give up when reaching Top. *)
            let give_up () =
              let msg = Format.asprintf "@[<hov 2>Adding imprecise offset %a is \
                not  allowed@]" (BR.binary_pretty ctx ~size:ofs_size) total_b in
              raise (Type_error msg)
            in
            let zero = BR.Binary_Forward.biconst ~size:offset_size Z.zero ctx in
            begin match binary_min ~size:ofs_size total_lattice with
            | Some min ->
              begin try
                let new_t, _ = type_at_offset pointed_t (Z.to_int min) in
                begin match new_t.Ctypes.descr with
                | Ctypes.(Array (_, ar_size)) ->
                    let new_ofs = BR.Binary_Forward.(bisub ~size:ofs_size ctx ~nsw:false ~nuw:false total_b @@ biconst ~size:ofs_size min ctx) in
                    let idx, fe = focus_idx ctx ar_size Ctypes.Zero in
                    (*
                    let new_t = Ptr{pointed=new_t;index=Zero} in
                    let new_t = {descr=new_t;pred=Pred.True} in
                    *)
                    add_offset' ctx new_t idx fe 0 zero new_ofs
                | _ -> give_up ()
                end
              with Exit -> give_up ()
              end
            | None -> give_up ()
            end *)
            Codex_log.warning "Imprecise offset %a when added. Could be out of bounds" (BR.binary_pretty ~size:offset_size ctx) offset  ;
            let new_non_aligned = BR.Binary_Forward.biadd ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctx old_non_aligned offset in
            pointed_t, old_index, old_fe, old_offset, new_non_aligned
              
          
          | Some total ->
            (* let total = Z.to_int total in *)
            let total = Z.to_int @@ Z.signed_extract total 0 ofs_size in
            pointed_t, old_index, old_fe, total, zero
            (*
            let lesser_sz = BR.Boolean_Forward.not ctx @@
              BR.Binary_Forward.bisle ~size:ofs_size ctx t_sz_b total_b in
            let geq_zero = BR.Binary_Forward.bisle ~size:ofs_size ctx zero total_b in
            let in_bounds = BR.Boolean_Forward.(&&) ctx geq_zero lesser_sz in
            let in_bounds = BR.Query.(boolean ctx in_bounds |> convert_to_quadrivalent) in
            begin match in_bounds with
            | Lattices.Quadrivalent.True -> pointed_t, old_index, old_fe, total, zero
            | _ ->
                let msg = Format.asprintf "Offset %a is out of the bounds of the pointed type (%a)"
                  (BR.binary_pretty ~size:ofs_size ctx) total_b  Ctypes.pp pointed_t in
                raise (Type_error msg)
            end
            *)


          (* | Some ofs ->
            Codex_log.warning "Precise offset %a. Could still be out of bounds" Z.pp_print ofs  ;
            pointed_t, old_index, old_fe, (old_offset + Z.to_int ofs), old_non_aligned *)
              
          end

    let add_offset' ctx pointed_t old_index old_fe old_offset old_non_aligned offset  =
      let open Ctypes in 
      match pointed_t.descr with
      | Structure {st_byte_size = None; _} ->
        let size = offset_size in
        let total_b =
          BR.Binary_Forward.(
            biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx offset @@
            biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx old_non_aligned @@
              (biconst ~size (Z.of_int old_offset) ctx)
          ) in
        let total_lattice = BR.Query.binary ~size ctx total_b in
        begin match BR.Query.binary_is_singleton ~size total_lattice with
        | None ->
            Codex_log.warning "Imprecise offset %a when added. Could be out of bounds" (BR.binary_pretty ~size:offset_size ctx) offset  ;
            let new_non_aligned = BR.Binary_Forward.biadd ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctx old_non_aligned offset in
            pointed_t, old_index, old_fe, old_offset, new_non_aligned

        | Some total ->
            (* let total = Z.to_int total in *)
            let zero = BR.Binary_Forward.biconst ~size Z.zero ctx in
            let total = Z.to_int @@ Z.signed_extract total 0 size in
            pointed_t, old_index, old_fe, total, zero
        end

      | _ -> add_offset' ctx pointed_t old_index old_fe old_offset old_non_aligned offset 


    let add_offset ctx incr (PtrT{ptyp;idx;fe;ofs;non_aligned;ppred}) =
      let ptyp,idx,fe,ofs,non_aligned = add_offset' ctx ptyp idx fe ofs non_aligned incr in
      PtrT{ptyp;idx;fe;ofs;non_aligned;ppred}


    (** Utility functions for refining a value using type information following a load *)

    let break_into_scalars (typs : Ctypes.typ list) : (int * Ctypes.typ) list =
      let open Ctypes in
      let rec aux (t : typ) : (int * typ) list =
      let t = Ctypes.inlined t in match t.descr with
      | Base _ | Ptr _ | Enum _ -> [(sizeof t, t)]
      | Structure {st_members;_} ->
          List.concat @@ List.map (fun (ofs,_,ftyp) ->
            aux ftyp
          ) st_members
      | Array (elem_typ, Some (Const length)) ->
          List.concat @@ List.init (Z.to_int length) (fun _ ->
            aux elem_typ
          )
      | Array (_, (None | Some (Sym _))) | Void | Function _ ->
          raise (Invalid_argument "break_into_scalars")

      | Existential (typ,_,_) -> [(sizeof t, t)]

      | Weak _ -> [(sizeof t, t)]

      | Union _ -> [(sizeof t, t)]

      | Name _ -> assert false
      | _ -> assert false
      in
      List.concat @@ List.map aux typs
  
    (** Inspect composite types (including nested ones) to retrieve a sequence of
        member types of total byte size [size] at offset [ofs] in [t]. Raises
        {!Not_found} if no such sequence exists. *)
    let rec members_at_offset_of_size ~ofs ~size t =
      Codex_log.debug ~level:2 "members_at_offset_of_size ~size:%d \
        ~ofs:%d %a" size ofs Ctypes.pp t;
      let open Ctypes in
      let t = inlined t in
      match t.descr with
      | Void -> raise Not_found
      | Base (sz,_) -> if ofs = 0 && size = sz then [t] else raise Not_found
      | Ptr _ ->
          if ofs = 0 && size = Codex_config.ptr_size () / 8 then [t] else raise Not_found
      | Enum {en_byte_size;_} ->
          if ofs = 0 && size = en_byte_size then [t] else raise Not_found
      | Function (_, _) ->
          (* Function types do not make sense, only pointers to functions do. *)
          raise Not_found
      | Array (elem_t, Some (Const ar_length)) ->
          let elem_size = sizeof elem_t in
          let ar_size = Z.to_int ar_length * elem_size in
          if ofs = 0 && size = ar_size then [t]
          else if ofs >= ar_size || size > ar_size then raise Not_found
          else if ofs mod elem_size = 0 && size mod elem_size = 0 then
            List.init (size / elem_size) (fun _ -> elem_t)
          else
            let ofs_in_elem = ofs mod elem_size in
            members_at_offset_of_size ~ofs:ofs_in_elem ~size elem_t
      | Array (_, Some (Sym _)) ->
          raise (Invalid_argument "members_at_offset_of_size: Unhandled array with symbolic size")
      | Array (_, None) ->
          raise (Invalid_argument "members_at_offset_of_size: Array without a size")
      | Structure{st_byte_size = Some st_size; st_members;_} ->
          if ofs = 0 && size = st_size then [t]
          else if ofs >= st_size || size > st_size then raise Not_found
          else
            (* Break struct into a sequence of scalars, and work with that. It's
             * simpler, and we lose no precision doing it at the moment. *)
            let scalars = break_into_scalars [t] in
            (* Discard fields that are before [ofs]. *)
            let rec discard discarded_size = function
            | (scalar_sz, _typ) :: tl as fields ->
                if discarded_size < ofs then discard (discarded_size + scalar_sz) tl
                else if discarded_size = ofs then fields
                else (* discarded_size > ofs *) raise Not_found
            | [] -> raise Not_found
            in
            let members = discard 0 scalars in
            (* Discard fields after [ofs + size]. *)
            let rec take taken_size acc = function
            | ((scalar_sz, _typ) as hd) :: tl ->
                if taken_size < size then take (taken_size + scalar_sz) (hd :: acc) tl
                else if taken_size = size then List.rev acc
                else (* taken_size > size *) raise Not_found
            | [] ->
                if taken_size = size then List.rev acc
                else (* taken_size <> size *) raise Not_found
            in
            List.map snd @@ take 0 [] members
      | Ctypes.Structure{st_byte_size = None;_} ->
          raise (Invalid_argument "members_ats_offset_of_size: Struct without a size")

      | Existential (typ, _, _) -> 
          let sz = sizeof typ in
          if ofs = 0 && size = sz then [t] else raise Not_found

      | Weak typ -> 
          let sz = sizeof typ in
          if ofs = 0 && size = sz then [t] else raise Not_found
  
      | Name _ -> assert false
      | _ -> assert false
  
    (** Like {!members_at_offset_of_size} but also takes an index and an [fe] and
        does array bound checking, if need be. Returns optionally an alarm
        message if out of the array bounds. *)
    let members_at_offset_of_size_complex ~size ctx t ~idx ~fe ~ofs =
      Codex_log.debug ~level:2 "members_at_offset_of_size_complex ~size:%d ~idx:(...) \
        ~fe:(...) ~ofs:%d %a" size ofs Ctypes.pp t;
      let open Ctypes in
      match (inlined t).descr with
      | Array (_, None) ->
          raise (Invalid_argument "members_at_offset_of_size_complex: Array without a size")
      | Array (_, Some Const sz) ->
          members_at_offset_of_size ~ofs ~size t, None
      | Array (_, Some _) ->
          let elem_t, offset_in_elem, alarm = type_at_offset_complex ctx t idx fe ofs in
          members_at_offset_of_size ~ofs:offset_in_elem ~size elem_t, alarm
      | Void | Base _
      | Structure _ | Ptr _ | Enum _ | Function (_, _) ->
          members_at_offset_of_size ~ofs ~size t, None

      | Existential _ ->
          members_at_offset_of_size ~ofs ~size t, None

      | Weak _ -> members_at_offset_of_size ~ofs ~size t, None 
  
      | Name _ -> assert false
      | _ -> assert false


    let rec members_at_offset_of_size_imprecise ~size ctx t ~na : Ctypes.typ list =
      let ptr_size = Codex_config.ptr_size () in
      Codex_log.debug ~level:2 "members_at_offset_imprecise ~size:%d %a ~non_aligned:%a" size Ctypes.pp t (BR.binary_pretty ~size:ptr_size ctx) na;
      let open Ctypes in
      (* match (inlined t).descr with
      | Structure{st_byte_size = _; st_members;_} ->
        begin
          let res = extract_from_struct ~size ~index:ofs (* @@ List.rev *) st_members in
          match res with
          | Some ({descr = Array (ptyp, Some (Sym s)); _} as t) ->
              let sz = snd @@ symbol ctx s in
              let ofs_size = offset_size in
              let elem_sz = sizeof ptyp in
              let elem_sz_bin = BR.Binary_Forward.biconst ~size:ofs_size (Z.of_int elem_sz) ctx in
              let idx = BR.Binary_Forward.bisdiv ~size:ofs_size ctx non_aligned elem_sz_bin in
              let fe = BR.Binary_Forward.bisub ~size:ofs_size ~nsw:false ~nuw:false ctx idx sz in
              let elem_t, offset_in_elem, alarm = type_at_offset_complex ctx t idx fe 0 in
              members_at_offset_of_size ~ofs:offset_in_elem ~size elem_t, alarm
          | Some t -> [t], None
          | None -> raise Not_found
        end
      | _ -> assert false *)
      let na_is_zero =
        match BR.Query.(binary_is_singleton ~size @@ binary ~size ctx na) with
        | None -> false | Some x -> Z.equal x Z.zero
      in 
      let na_geq_sz sz = 
        let res = BR.Binary_Forward.biule ~size:ptr_size ctx sz na in
        match BR.query_boolean ctx res with
        | Lattices.Quadrivalent.True -> true
        | _ -> false
      in
      let t = inlined t in
      match t.descr with
      | Void -> raise Not_found
      | Base (sz,_) -> if na_is_zero then [t] else raise Not_found
      | Ptr _ ->
          if na_is_zero && size = Codex_config.ptr_size () / 8 then [t] else raise Not_found
      | Enum {en_byte_size;_} ->
          if na_is_zero && size = en_byte_size then [t] else raise Not_found
      | Function (_, _) ->
          (* Function types do not make sense, only pointers to functions do. *)
          raise Not_found
      | Array (elem_t, Some (Const ar_length)) ->
          let elem_size = sizeof elem_t in
          let ar_size = Z.to_int ar_length * elem_size in
          let ar_size_b = BR.Binary_Forward.biconst ~size:ptr_size (Z.of_int ar_size) ctx in 
          if na_is_zero && size = ar_size then [t]
          else if na_geq_sz ar_size_b || size > ar_size then raise Not_found
          else
            let ofs_in_elem_b = BR.Binary_Forward.biumod ~size:ptr_size ctx na ar_size_b in
            let ofs_in_elem = BR.Query.(binary_is_singleton ~size:ptr_size @@ binary ~size:ptr_size ctx ofs_in_elem_b) in
            if Option.equal Z.equal ofs_in_elem (Some Z.zero) && size mod elem_size = 0 then
              List.init (size / elem_size) (fun _ -> elem_t)
            else
              members_at_offset_of_size_imprecise ~na:ofs_in_elem_b ~size ctx elem_t
      | Array (_, Some (Sym _)) ->
          raise (Invalid_argument "members_at_offset_of_size: Unhandled array with symbolic size")
      | Array (_, None) ->
          raise (Invalid_argument "members_at_offset_of_size: Array without a size")
      | Structure{st_byte_size = Some st_size; st_members;_} ->
          let st_size_b = BR.Binary_Forward.biconst ~size:ptr_size (Z.of_int st_size) ctx in
          if na_is_zero && size = st_size then [t]
          else if na_geq_sz st_size_b || size > st_size then raise Not_found
          else
            (* Break struct into a sequence of scalars, and work with that. It's
             * simpler, and we lose no precision doing it at the moment. *)
            let scalars = break_into_scalars [t] in
            (* Discard fields that are before [ofs]. *)
            let res = BR.Query.binary_fold_crop ~size:ptr_size (BR.Query.binary ~size:ptr_size ctx na) ~inf:Z.zero ~sup:(Z.of_int st_size) None
            (fun i acc -> 
              let ofs = Z.to_int i in
              let rec discard discarded_size = function
              | (scalar_sz, _typ) :: tl as fields ->
                  if discarded_size < ofs then discard (discarded_size + scalar_sz) tl
                  else if discarded_size = ofs then fields
                  else (* discarded_size > ofs *) raise Not_found
              | [] -> raise Not_found
              in
              let members = discard 0 scalars in
              (* Discard fields after [ofs + size]. *)
              let rec take taken_size acc = function
              | ((scalar_sz, _typ) as hd) :: tl ->
                  if taken_size < size then take (taken_size + scalar_sz) (hd :: acc) tl
                  else if taken_size = size then List.rev acc
                  else (* taken_size > size *) raise Not_found
              | [] ->
                  if taken_size = size then List.rev acc
                  else (* taken_size <> size *) raise Not_found
              in
              let res = (List.map snd @@ take 0 [] members) in
              match acc with
              | None -> Some res
              | Some typs when List.equal (Ctypes.equal ~only_descr:false) typs res -> acc
              | _ -> raise Not_found 
            ) in
            (match res with Some typs -> typs | None -> assert false)
      | Structure{st_byte_size = None;_} ->
          raise (Invalid_argument "members_ats_offset_of_size_imprecise: Struct without a precise size")

      | Existential (typ, _, _) -> 
          let sz = sizeof typ in
          if na_is_zero && size = sz then [t] else raise Not_found
  
      | Name _ -> assert false
      | _ -> assert false

    let members_at_offset_of_size_complex_complex ~size ctx pointed_t ~idx ~fe ~ofs ~na =
      let ptr_size = Codex_config.ptr_size () in
      Codex_log.debug ~level:2 "members_at_offset_of_size_complex_complex ~size:%d ~idx:(...) \
      ~fe:(...) ~ofs:%d ~non_aligned:%a %a" size ofs (BR.binary_pretty ~size:ptr_size ctx) na Ctypes.pp pointed_t;
      let na = BR.Binary_Forward.(biadd ~size:ptr_size ~nsw:false ~nuw:false ~nusw:false ctx na @@ biconst ~size:ptr_size (Z.of_int ofs) ctx) in
      match pointed_t.descr with
      | Ctypes.Structure{st_byte_size = None; st_members;_} ->
          let region_t = members_at_offset_of_size_imprecise ~size ctx pointed_t ~na in
          region_t, None

      | _ -> 
        let non_aligned_sng = BR.Query.binary_is_singleton ~size:offset_size @@ BR.Query.binary ~size:offset_size ctx na in
        match non_aligned_sng with
          | None ->
              (* Codex_log.alarm "array_offset_access";
              Codex_log.error "Imprecise offset %a is not allowed" (BR.binary_pretty ~size:offset_size ctx) non_aligned ;
              assert false ;
              raise @@ Type_error ("Non aligned offset is not constant")
              *)
              let region_t = members_at_offset_of_size_imprecise ~size ctx pointed_t ~na in
              region_t, None
          | Some o ->
              (* let ofs = Z.to_int o + ofs in *)
              members_at_offset_of_size_complex ~size ctx pointed_t ~idx ~fe ~ofs
              (* let region_t, alarm = members_at_offset_of_size_complex ~size ctx
              ptyp ~idx ~fe ~ofs *)


    type dereferenced_value = IsPtr of t | NonPtr of Ctypes.typ

    (* Turn a sequence of types into a list of {!dereferenced_value}s. Undefined if
      [size] does not equal the total size of the types. In particular, all sizes
      must be known (i.e. not symbolic).
      Note that this ignores predicates on composite types (arrays and
      structures), if any. At the moment, such predicates are devoid of interest
      anyway, since there is no practical way to specify relations between
      fields. *)

    let dereferenced_value_of_typ ~size ctx typs =
      (* If type is pointer-sized, weaken it until obtaining either a pointer or
         a scalar type.  Consider e.g. if {pointed_t} is an array or a struct
         containing a single pointer type {p}, then this method enables to return
         {p} rather than losing the type of the pointer. *)
      let zero = BR.Binary_Forward.biconst ~size:offset_size Z.zero ctx in
      let rec weaken_until_ptr (t : Ctypes.typ) : dereferenced_value =
        let t = Ctypes.inlined t in
        match t.descr with
        | Ctypes.(Ptr {pointed = {descr=Array (_,ar_size);_} as pointed; index}) ->
            (* Here we extract the index from the type into a
             * [BR.binary]. We "focus" it, so to say *)
            let idx, fe = focus_idx ctx ar_size index in
            let res = PtrT{ptyp=pointed; idx;fe; ofs = 0; non_aligned = zero; ppred = t.Ctypes.pred} in
            Codex_log.debug ~level:1 "Dereferenced type: %a" (pp ctx) res;
            IsPtr res
        | Ctypes.(Ptr{pointed;index=_}) ->
            let idx = index_zero ctx in
            let fe = index_minus_one ctx in
            let res = PtrT{ptyp=pointed;idx;fe; ofs = 0; non_aligned = zero; ppred = t.Ctypes.pred} in
            Codex_log.debug ~level:1 "Dereferenced type: %a" (pp ctx) res;
            IsPtr res
        | _ ->
          begin try
            Codex_log.debug "before, typ : %a" Ctypes.pp t ;
            let new_t, new_ofs = type_at_offset t 0 in
            Codex_log.debug "in dereferenced_value_of_typ, typ %a at offset %d" Ctypes.pp new_t new_ofs ;
            assert (new_ofs = 0);
            weaken_until_ptr new_t
          with Exit ->
            (* Apply potential invariants for this non-pointer type *)
            NonPtr t
          end
      in
      match typs with
      | [t] when size = Codex_config.ptr_size () / 8 -> [(size, weaken_until_ptr t)]
      | _ ->
        (* Return the predicates associated with each member of the type. *)
        let scalars = break_into_scalars typs in
        List.map (fun (sz,t) -> sz, (weaken_until_ptr t)) scalars

    let deref_to_ctypes ~size ctx typ = match typ with
    | PtrT{ptyp = pointed_t; idx;fe;ofs; non_aligned} ->
        Codex_log.debug ~level:1 "Dereferencing type %a" (pp ctx) typ;
        (* The algorithm goes:
          1. Find a sequence of members totalizing size [size] at offset [ofs]
              in the pointed structure, if possible
          2. Turn the into a {!dereferenced_value}.
        *)
        
        begin try
          (* let region_t, alarm = members_at_offset_of_size_complex ~size ctx
            pointed_t ~idx ~fe ~ofs in *)
          let region_t, alarm = members_at_offset_of_size_complex_complex ~size ctx
              pointed_t ~idx ~fe ~ofs ~na:non_aligned in
          let () = match alarm with
          | Some msg ->
              Codex_log.alarm "array_offset_access";
              Codex_log.error "%s" msg
          | None -> () in
          region_t
        with Not_found ->
          raise @@ Type_error (Format.asprintf "No type of size %d is pointed by pointer %a" size (pp ctx) typ)
        end
    ;;

    let deref_to_ctype ~size ctx typ =
      let types = deref_to_ctypes ~size ctx typ in
      Ctypes.tuple types
    ;;
    
    (** Returns a (type, predicate) pair. The type is the result of
      dereferencing the type in argument. The predicate represents the possible
      numeric constraints associated with the type returned, i.e.
        invariants that must hold on the associated value. *)
    let deref ~size ctx typ =
      let region_t = deref_to_ctypes ~size ctx typ in
      dereferenced_value_of_typ ~size ctx region_t
    ;;

    (* -------------------------- serialize operations -------------------------*)

    let in_bounds ctx (PtrT{ptyp;idx;fe;ofs;non_aligned}) =
      let open Ctypes in
      match ptyp.descr with
      | Array (x, Some ar_size) ->
          check_invariant_subdomain ~size:array_index_size ctx Pred.(sgeq (Const Z.zero)) idx
          || check_invariant_subdomain ~size:array_index_size ctx Pred.(slt (Const Z.zero)) fe
      | Array (_, None) ->
          raise (Invalid_argument "in_bounds: Array without a length")
      | _ -> true
  
    let tmp_count = ref 0
  
    type inclusion = LeftIncluded | RightIncluded | NoInclusion
  
    let pp_opt f fmt = function
    | None -> Format.pp_print_string fmt "None"
    | Some x -> Format.fprintf fmt "Some %a" f x
  
    (* Also returns a boolean telling whether [p2] is included in [p1]. *)
    let join_pred p1 p2 =
      if Ctypes.Pred.(equal p1 True) || p1 = p2 then true, p1
      else false, Ctypes.Pred.True
  
    let query_min ~size b =
      let inf = Z.sub Z.zero (Z.shift_left Z.one size) in
      Codex_log.debug "inf : %d" (Z.to_int inf) ;
      let sup = Z.shift_left Z.one size in
      Codex_log.debug "sup : %d" (Z.to_int sup) ;
      let exception Exit of Z.t in
      try BR.Query.binary_fold_crop ~size b ~inf ~sup None (fun x acc ->
        raise (Exit x)
      )
      with Exit x -> Codex_log.debug "x : %d" (Z.to_int x) ; Some x
  
    let query_max ~size ctx b =
      let minus_b = BR.Binary_Forward.(bisub ~size ~nsw:false ~nuw:false ~nusw:false ctx
        (biconst ~size Z.zero ctx) b) in
      Codex_log.debug "minus_b : %a" (BR.binary_pretty ~size ctx) minus_b ;
      let minus_b = BR.Query.binary ~size ctx minus_b in
      match query_min ~size minus_b with
      | None -> None
      | Some x -> Some (Z.neg x)
  
    let serialize ctxt t ctxu u (included, acc) =
      incr tmp_count;
      let count = !tmp_count in
      Codex_log.debug ~level:2 "serialize no. %d: %a %a" count (pp ctxt) t (pp ctxu) u;
      Codex_log.debug ~level:2 "serialize no. %d: initial included = %b" count included;
      match t, u with
      (* This is only an optimization. Is it safe? Apparently not, got
      * [Never_refined] exceptions using it. *)
      (*
      | PtrT _, PtrT _ when eq ctx t u ->
          BR.Scalar.Result (acc, fun output -> t, output)
      *)
      | PtrT{ptyp = ({descr=Array (elem_t, m_sz1);_}); idx=t_idx; fe=t_fe; ofs=t_ofs; non_aligned=t_na; ppred=pred_t},
        PtrT{ptyp = ({descr=Array (elem_u, m_sz2);_}); idx=u_idx; fe=u_fe; ofs=u_ofs; non_aligned=u_na; ppred=pred_u}
        when Ctypes.equiv ~only_descr:false elem_t elem_u
        && t_ofs = u_ofs ->
          let inc0, new_pred = join_pred pred_t pred_u in
          Codex_log.debug ~level:2 "predicate inclusion: %B" inc0;
          (* Join indices *)
          Codex_log.debug ~level:2 "joining of indices...";
          let map2 f default x y = match x,y with
            | Some x, Some y -> f x y
            | _ -> default
          in
          (* We don't want to test inclusion of indices, but compare their minima or maxima. *)
          (*
          let wrap_z = function
          | Some z -> Some (if Z.geq z (Z.shift_left Z.one 31) then Z.sub z (Z.shift_left Z.one 32)
              else z)
          | None -> None
          in
          *)
          let BR.Context.Result (_, acc, deserialize_idx) = BR.serialize_binary ~size:offset_size
            ctxt t_idx ctxu u_idx (true, acc) in
          let inc1 =
            let iu = BR.Query.binary ~size:array_index_size ctxu u_idx in
            let it = BR.Query.binary ~size:array_index_size ctxt t_idx in
            map2 Z.leq false (query_min ~size:array_index_size it) (query_min ~size:array_index_size iu)
          in
          Codex_log.debug ~level:2 "index inclusion: %B" inc1;
          let Context.Result (_, acc, deserialize_fe) = BR.serialize_binary ~size:offset_size
            ctxt t_fe ctxu u_fe (true, acc) in
          
          let Context.Result (_, acc, deserialize_non_aligned) = BR.serialize_binary ~size:offset_size
            ctxt t_na ctxu u_na (true, acc) in
          let inc2 =
            map2 Z.leq false (query_max ~size:array_index_size ctxu u_fe) (query_max ~size:array_index_size ctxt t_fe)
          in
          Codex_log.debug ~level:2 "fe inclusion: %B" inc2;
          (* XXX: compute new size. Is this sound? *)
          let inc3, m_new_size = let open Ctypes in
          begin match m_sz1, m_sz2 with
          | Some (Const sz1), Some (Const sz2) when sz1 = sz2 ->
              true, Some (Const sz1)
          | Some (Sym s1), Some (Sym s2) when s1 = s2 ->
              true, Some (Sym s1)
          | _ -> false, None
          end in
          Codex_log.debug ~level:2 "size inclusion: %B" inc3;
          let included' = inc0 && inc1 && inc2 && inc3 in
          Codex_log.debug ~level:2 "inclusion is %B" included';
          let included = included && included' in
          Context.Result (included, acc, fun ctx output ->
            let new_na, output = deserialize_non_aligned ctx output in
            let new_fe, output = deserialize_fe ctx output in
            let new_index, output = deserialize_idx ctx output in
            let ret =
              if m_new_size = None
              then Some (
                PtrT{ptyp = elem_t; ppred = new_pred;
                  idx = index_zero ctx; fe = index_minus_one ctx; ofs = t_ofs; non_aligned = new_na}
                )
              else Some Ctypes.(
                PtrT{ptyp = ({descr=Array (elem_t, m_new_size);
                                      pred = Pred.True });
                  ppred = new_pred; idx = new_index; fe = new_fe; ofs = t_ofs; non_aligned = new_na}
                )
            in
            Codex_log.debug ~level:2 "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret;
            ret, output)
  
      | PtrT{ptyp=pt;ppred=pred_t; idx=t_idx; fe=t_fe; ofs=t_ofs; non_aligned=t_na},
        PtrT{ptyp=pu;ppred=pred_u; idx=u_idx; fe=u_fe; ofs=u_ofs; non_aligned=u_na}
        when Ctypes.(equiv_descr ~only_descr:false pt.descr pu.descr)
        && t_ofs = u_ofs ->
          let inc0, new_pred = join_pred pred_t pred_u in
          Codex_log.debug ~level:2 "predicate inclusion: %B" inc0;
          Codex_log.debug ~level:2 "joining of indices...";
          (* Join indices. This should only happen between indices zero and
          * bottom, since it is an invariant that all indices of non-array
          * types should be included in the singleton zero. *)
          let Context.Result (inc1, acc, deserialize_idx) = BR.serialize_binary ~size:offset_size
            ctxt t_idx ctxu u_idx (true,acc) in
          Codex_log.debug ~level:2 "idx inclusion: %B" inc1;
          let Context.Result (inc2, acc, deserialize_fe) = BR.serialize_binary ~size:offset_size
            ctxt t_fe ctxu u_fe (true,acc) in
          Codex_log.debug ~level:2 "fe inclusion: %B" inc2;
          let Context.Result (inc3, acc, deserialize_non_aligned) = BR.serialize_binary ~size:offset_size
            ctxt t_na ctxu u_na (true, acc) in
          Codex_log.debug ~level:2 "non_aligned inclusion: %B" inc3;
          let included' = inc0 && inc1 && inc2 && inc3 in
          Codex_log.debug ~level:2 "inclusion is %B" included';
          let included = included && included' in
          Context.Result (included, acc, fun ctx output ->
            let new_na, output = deserialize_non_aligned ctx output in
            (* Codex_log.debug "in deserialize, new_na : %a" (BR.binary_pretty ~size:offset_size ctx) new_na ; *)
            let new_fe, output = deserialize_fe ctx output in
            let new_index, output = deserialize_idx ctx output in
            let ret = Some (PtrT{ptyp=pt; ppred=new_pred; idx=new_index;
              fe=new_fe; ofs=t_ofs; non_aligned=new_na}) in
            (* Codex_log.debug ~level:2 "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret; *)
            ret, output)

    | PtrT{ptyp=pt;ppred=pred_t; idx=t_idx; fe=t_fe; ofs=t_ofs; non_aligned=t_na},
        PtrT{ptyp=pu;ppred=pred_u; idx=u_idx; fe=u_fe; ofs=u_ofs; non_aligned=u_na}
        when Ctypes.(equiv_descr ~only_descr:false pt.descr pu.descr) ->
          let inc0, new_pred = join_pred pred_t pred_u in
          Codex_log.debug ~level:2 "predicate inclusion: %B" inc0;
          Codex_log.debug ~level:2 "joining of indices...";
          (* Join indices. This should only happen between indices zero and
          * bottom, since it is an invariant that all indices of non-array
          * types should be included in the singleton zero. *)
          let Context.Result (inc1, acc, deserialize_idx) = BR.serialize_binary ~size:offset_size
            ctxt t_idx ctxu u_idx (true,acc) in
          Codex_log.debug ~level:2 "idx inclusion: %B" inc1;
          let Context.Result (inc2, acc, deserialize_fe) = BR.serialize_binary ~size:offset_size
            ctxt t_fe ctxu u_fe (true,acc) in
          Codex_log.debug ~level:2 "fe inclusion: %B" inc2;
          let size = array_index_size in
          let t_na = BR.Binary_Forward.(biadd ~size ~nsw:false ~nuw:false ~nusw:false ctxt t_na @@ biconst ~size (Z.of_int t_ofs) ctxt) in
          let u_na = BR.Binary_Forward.(biadd ~size ~nsw:false ~nuw:false ~nusw:false ctxu u_na @@ biconst ~size (Z.of_int u_ofs) ctxu) in
          let Context.Result (inc3, acc, deserialize_non_aligned) = BR.serialize_binary ~size:offset_size
            ctxt t_na ctxu u_na (true, acc) in
          Codex_log.debug ~level:2 "non_aligned inclusion: %B" inc3;
          let included' = inc0 && inc1 && inc2 && inc3 in
          Codex_log.debug ~level:2 "inclusion is %B" included';
          let included = included && included' in
          Context.Result (included, acc, fun ctx output ->
            let new_na, output = deserialize_non_aligned ctx output in
            (* Codex_log.debug "in deserialize, new_na : %a" (BR.binary_pretty ~size:offset_size ctx) new_na ; *)
            let new_fe, output = deserialize_fe ctx output in
            let new_index, output = deserialize_idx ctx output in
            let ret = Some (PtrT{ptyp=pt; ppred=new_pred; idx=new_index;
              fe=new_fe; ofs=0; non_aligned=new_na}) in
            (* Codex_log.debug ~level:2 "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret; *)
            ret, output)
            
      (* Here we ignore the indices (should be zero) *)
    | PtrT{ptyp=pointed_t; idx=idx_t; fe=fe_t; ofs=t_ofs; ppred=pred_t; non_aligned=t_na},
      PtrT{ptyp=pointed_u; idx=idx_u; fe=fe_u; ofs=u_ofs; ppred=pred_u; non_aligned=u_na} ->
      begin try
        Codex_log.check "serialize";
        Codex_log.check "serialize";
        let inc0, new_pred = join_pred pred_t pred_u in
        Codex_log.debug ~level:2 "predicate inclusion: %B" inc0;
        (* The lattice {Ctypes.t} has a tree structure (once excluded
        * {ZeroT}), so all we need to do to find the lub is to find their
        * lowest common ancestor. We implement this algorithm in a naive
        * way: compute the list of parents up to the root for {t} and {u},
        * then reverse these two lists and take the last element of their
        * common prefix. *)
        (* Computes list of parents in descending order, including {x},
        * excluding {Top}. *)
        
        let t_ofs =
          match BR.Query.binary_is_singleton ~size:offset_size @@ BR.Query.binary ~size:offset_size ctxt t_na with
          | Some ofs -> Z.to_int ofs + t_ofs
          | None -> raise (Type_error "Non aligned offset is not constant")
        in

        let u_ofs = 
          match BR.Query.binary_is_singleton ~size:offset_size @@ BR.Query.binary ~size:offset_size ctxu u_na with
          | Some ofs -> Z.to_int ofs + u_ofs
          | None -> raise (Type_error "Non aligned offset is not constant")
        in

        let parents ctx (x : Ctypes.typ) idx fe ofs =
          let rec aux x ofs =
            try
              let parent_t, parent_ofs = type_at_offset x ofs in
              (x,ofs) :: aux parent_t parent_ofs
            with Exit -> [(x, ofs)]
          in
          (try
            let parent_t, parent_ofs, alarm =
              type_at_offset_complex ctx x idx fe ofs in
            let res = match alarm with
            | Some msg ->
                Codex_log.warning "Invalid pointer has no parent. Origin: %s" msg;
                [(x, ofs)]
            | None -> (x, ofs) :: aux parent_t parent_ofs
            in res
          with Exit -> [(x, ofs)])
        in
        let parents_t = parents ctxt pointed_t idx_t fe_t t_ofs in
        Codex_log.debug ~level:2 "parents_t = [%a]" (Format.pp_print_list (fun fmt (t,ofs) -> Format.fprintf fmt "(%a,%d)" Ctypes.pp t ofs)) parents_t;
        let parents_u = parents ctxu pointed_u idx_u fe_u u_ofs in
        Codex_log.debug ~level:2 "parents_t = [%a]" (Format.pp_print_list (fun fmt (t,ofs) -> Format.fprintf fmt "(%a,%d)" Ctypes.pp t ofs)) parents_u;
        let rec walk prev l1 l2 = match l1,l2 with
          | [], _ ->
              prev, LeftIncluded
          | _, [] ->
              prev, RightIncluded
          | (x,ox) :: r1, (y,oy) :: r2 ->
              if Ctypes.equiv ~only_descr:false x y && ((=) : int -> int -> bool) ox oy
              then walk (Some (x,ox)) r1 r2
              else prev, NoInclusion
        in
        (match walk None (List.rev parents_t) (List.rev parents_u) with
        | Some (t,o), inclusion ->
            let inc1 = inclusion = LeftIncluded in
            let included = included && inc0 && inc1 in
            let d ctx output = 
              let zero = index_zero ctx in
              let res = Some (PtrT{ptyp=t; ppred=new_pred;
                                  idx = index_zero ctx; fe = index_minus_one ctx; ofs=o;non_aligned=zero}) in
              Codex_log.debug ~level:2 "inclusion is %B" included;
              Codex_log.debug ~level:2 "serialize no. %d returns %a" count (pp_opt (pp ctx)) res;
              res,output
            in
            Context.Result (included, acc, d)
        | None, _ ->
            Codex_log.debug ~level:2 "serialize no. %d: inclusion is false" count;
            Codex_log.debug ~level:2 "serialize no. %d returns Top" count;
            Context.Result (false, acc, fun ctx output -> None, output)
        )
      with Type_error s ->
        Codex_log.alarm "serialize";
        Codex_log.error "Type error during serialize no. %d: %s" count s;
        Codex_log.debug ~level:2 "serialize no. %d: inclusion is false" count;
        Context.Result (false, acc, fun ctx output -> None, output)
      end
  
    let serialize_with_bottom_left ctxdummy ctxb (PtrT{ptyp;idx;fe;ofs;non_aligned;ppred} as t)
        (included,acc) =
      incr tmp_count;
      let count = !tmp_count in
      Codex_log.debug ~level:2 "serialize no. %d (serialize_with_bottom_left): %a" count (pp ctxb) t;
      Codex_log.debug ~level:2 "serialize no. %d: initial included = %b" count included;
      let dummy_idx = BR.binary_empty ~size:array_index_size ctxdummy in
      let Context.Result(inc,acc,deserialize_idx) = BR.serialize_binary
        ~size:array_index_size ctxdummy dummy_idx ctxb idx (included,acc) in
      let dummy_fe = BR.binary_empty ~size:array_index_size ctxdummy in
      let Context.Result(inc,acc,deserialize_fe) = BR.serialize_binary
        ~size:array_index_size ctxdummy dummy_fe ctxb fe (included,acc) in
      let dummy_na = BR.binary_empty ~size:array_index_size ctxdummy in
      let Context.Result(inc,acc,deserialize_non_aligned) = BR.serialize_binary
        ~size:array_index_size ctxdummy dummy_na ctxb non_aligned (included,acc) in
      Context.Result (inc, acc, fun ctx output ->
        let non_aligned, output = deserialize_non_aligned ctx output in
        let fe, output = deserialize_fe ctx output in
        let idx, output = deserialize_idx ctx output in
        let ret = PtrT{ptyp;idx;fe;ofs;non_aligned;ppred} in
        (* Codex_log.debug ~level:2 "serialize no. %d (serialize_with_bottom_left) returns %a%!" count (pp ctx) ret; *)
        ret, output
      )

    let serialize_with_bottom_right ctxa ctxdummy (PtrT{ptyp;idx;fe;ofs;non_aligned;ppred} as t)
        (included,acc) =
      incr tmp_count;
      let count = !tmp_count in
      Codex_log.debug ~level:2 "serialize no. %d (serialize_with_bottom_right): %a" count (pp ctxa) t;
      Codex_log.debug ~level:2 "serialize no. %d: initial included = %b" count included;
      let dummy_idx = BR.binary_empty ~size:array_index_size ctxdummy in
      let Context.Result(inc,acc,deserialize_idx) = BR.serialize_binary
        ~size:array_index_size ctxa idx ctxdummy dummy_idx (included,acc) in
      let dummy_fe = BR.binary_empty ~size:array_index_size ctxdummy in
      let Context.Result(inc,acc,deserialize_fe) = BR.serialize_binary
        ~size:array_index_size ctxa fe ctxdummy dummy_fe (included,acc) in
      let dummy_na = BR.binary_empty ~size:array_index_size ctxdummy in
      let Context.Result(inc,acc,deserialize_non_aligned) = BR.serialize_binary
        ~size:array_index_size ctxa non_aligned ctxdummy dummy_na (included,acc) in
      Context.Result (inc, acc, fun ctx output ->
        let non_aligned, output = deserialize_non_aligned ctx output in
        let fe, output = deserialize_fe ctx output in
        let idx, output = deserialize_idx ctx output in
        let ret = PtrT{ptyp;idx;fe;ofs;non_aligned;ppred} in
        (* Codex_log.debug ~level:2 "serialize no. %d (serialize_with_bottom_right) returns %a%!" count (pp ctx) ret; *)
        ret, output
      )

    let subtype ctx t u =
      (* Careful to the order of t and u: [included] is true iff u includes t. *)
      let Context.Result (included,_,_) =
        serialize ctx u ctx t (true, Context.empty_tuple) in
      included
          
  end

  (* module BinaryMap = Map.Make(Scalar.Binary) *)
  module BinaryMap = Map.Make(Sub.Address.Binary)
    
  let state : (Type.t BinaryMap.t) ref = ref BinaryMap.empty

  module Address(* :Memory_sig.Address *) = struct

    module SubAddress = Sub.Address
    module Domain:Domain_sig.Minimal
      with module Context = SubAddress.Context
       and type boolean = SubAddress.boolean
      = SubAddress
    include Domain      
    (* let boolean_pretty = Sub.Address.boolean_pretty *)
    (* let query_boolean = Sub.Address.query_boolean *)
    
    
    module Scalar = SubAddress.Scalar

    (* type binary = private T of { typ : Type.t option; value : SubAddress.binary option; } *)

    type binary = SubAddress.binary

    let global_symbol ctx symb = assert false
    let add_global_symbol ~size ctx symb value = assert false
    let add_global_scalar_symbol ~size ctx symb value =
      Type.add_symbol ~size ctx symb value

    module Query = SubAddress.Query
    (* module Query = struct

      module Boolean_Lattice = Scalar.Query.Boolean_Lattice
      let boolean ctx value = assert false

      module Binary_Lattice = Scalar.Query.Binary_Lattice
      let binary ~size ctx value = assert false

      module Integer_Lattice = Scalar.Query.Integer_Lattice
      let integer ctx value = assert false

      let reachable _ = assert false
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

    end *)

    let binary2scalar_binary ~size ctx value = SubAddress.binary2scalar_binary ~size ctx value

    let binary_pretty ~size ctx fmt value =
      if Query.binary_is_empty ~size @@ Query.binary ~size ctx value then 
        Format.pp_print_string fmt "Bottom"
      else
        try
          let typ = BinaryMap.find value !state in
          Format.fprintf fmt "@[<hov 2>(%a :@ %a)@]"
              (SubAddress.binary_pretty ~size ctx) value 
              (Type.pp ctx) typ

        with Not_found -> SubAddress.binary_pretty ~size ctx fmt value

    let binary_empty = SubAddress.binary_empty

    let satisfiable = SubAddress.satisfiable

    (* TODOML: In this code, we care about the type only when it is a pointer type.
       I think this is no coincidence, and possibly the type of the typ field
       should reflect that. *)
    
    (*
    let serialize_typ ~size ctxa a ctxb b ((inc,acc) as in_acc) =
      Codex_log.debug "Typed_memory_domain.serialize_typ %a %a" (binary_pretty ~size ctxa) a (binary_pretty ~size ctxb) b ;
      let a = SubAddress.binary2scalar_binary ~size ctxa a in
      let b = SubAddress.binary2scalar_binary ~size ctxb b in
      let r1 = BinaryMap.mem a !state in
      let r2 = BinaryMap.mem b !state in
      if r1 && r2 then
        begin
          let ta = BinaryMap.find a !state in 
          let tb = BinaryMap.find a !state in 
          let Scalar.Result (inc, acc, deserialize_typ) = Type.serialize ctxa ta ctxb tb in_acc in
          Scalar.Result (inc, acc, fun ctx output ->
            let typ,output = deserialize_typ ctx output in
            typ, output)
        end
      else
      if r1 then 
          begin
            let typ = BinaryMap.find a !state in
            let Scalar.Result (inc, acc, d_typ) = Type.serialize_with_bottom_right ctxa ctxb typ in_acc in
              Scalar.Result (inc, acc, fun ctx output ->
                let typ,output = d_typ ctx output in
                Some typ, output
              )
          end
        else
          if r2 then
            begin
              let typ = BinaryMap.find b !state in
              let Scalar.Result (inc, acc, d_typ) = Type.serialize_with_bottom_left ctxa ctxb typ in_acc in
              Scalar.Result (inc, acc, fun ctx output ->
                let typ,output = d_typ ctx output in
                Some typ, output
              )
            end

          else Scalar.Result (inc, acc, fun ctx output -> None, output)
    *)
    
    let serialize_typ ~size ctxa a ctxb b ((inc,acc) as in_acc) =
      let ta = BinaryMap.find_opt a !state in 
      let tb = BinaryMap.find_opt b !state in
      match ta,tb with
      | Some typa, Some typb -> Type.serialize ctxa typa ctxb typb in_acc
      | Some typa, None
        when Query.binary_is_empty ~size @@ Query.binary ~size ctxb b ->
            let Context.Result(inc, acc, des) = Type.serialize_with_bottom_right ctxa ctxb typa in_acc in
            Context.Result(inc, acc, fun ctx out ->
              let typ, out = des ctx out in
              Some typ, out
            )
      | None, Some typb
        when Query.binary_is_empty ~size @@ Query.binary ~size ctxa a -> 
            let Context.Result(inc, acc, des) = Type.serialize_with_bottom_left ctxa ctxb typb in_acc in
            Context.Result(inc, acc, fun ctx out ->
              let typ, out = des ctx out in
              Some typ, out
            )
      | _ -> Context.Result(inc, acc, fun ctx out -> None, out)
      (*
      | Some typa, _ -> 
          let Context.Result(inc, acc, des) = Type.serialize_with_bottom_right ctxa ctxb typa in_acc in
          Context.Result(inc, acc, fun ctx out ->
            let typ, out = des ctx out in
            Some typ, out
          )
      | _, Some typb ->
          let Context.Result(inc, acc, des) = Type.serialize_with_bottom_left ctxa ctxb typb in_acc in
          Context.Result(inc, acc, fun ctx out ->
            let typ, out = des ctx out in
            Some typ, out
          )
      *)    

    let serialize ~size ctxa a ctxb b acc = 
      let Context.Result(inc, acc, deserialize_typ) = serialize_typ ~size ctxa a ctxb b acc in
      let Context.Result(inc, acc, deserialize_val) = SubAddress.serialize ~size ctxa a ctxb b (inc,acc) in
      Context.Result (inc, acc, fun ctx output ->
          let value, output = deserialize_val ctx output in
          let typ, output = deserialize_typ ctx output in
          Option.iter (fun t -> state := BinaryMap.add value t !state) typ ;
          value, output
        )
    

    let bchoose ~size choice ctx value = SubAddress.bchoose ~size choice ctx value

    let ble ~size ctx a b = SubAddress.ble ~size ctx a b

    let beq ~size ctx a b = SubAddress.beq ~size ctx a b

    let boolean_unknown ctx = SubAddress.boolean_unknown ctx

    let type_concretization_included ~size ctx a u =
      try (
        let t = BinaryMap.find a !state in 
        Type.subtype ctx t u
      )
      with Not_found -> true
    ;;

    let has_type ~size ctx typ addr =
      let typ = Ctypes.{typ with pred = Pred.True} in
      let region_t = Type.dereferenced_value_of_typ ~size ctx [typ] in
      match region_t with
      | [(sz, IsPtr (PtrT {ptyp=t'} as t))] ->
          assert (8 * sz = size) ;
          let PtrT{ptyp = addr_typ} = BinaryMap.find addr !state in
          begin match addr_typ.descr with
          | Weak wk_typ -> addr_typ.descr <- t'.descr ; true
          | _ -> begin
            let res = type_concretization_included ~size ctx addr t in
            if not res then begin
              Codex_log.alarm "typing_store";
              Codex_log.error "Storing value@ %a to region of type@ %a: value type not included in region type"
                ((* Binary_Representation. *)binary_pretty ctx ~size) addr
                (Type.pp ctx) t
            end ;
            res
            end
          end
      | _ -> assert false

    
    let within_bounds ~size ctx addr = SubAddress.within_bounds ~size ctx addr

    module Boolean = SubAddress.Boolean

    module Binary = SubAddress.Binary

    (* module Binary = struct
      type t = binary

      let pretty = assert false

      let hash _ = assert false

      let compare x y = assert false

      let equal x y = assert false
    end *)

    module Boolean_Forward = SubAddress.Boolean_Forward

    let add ~size result ctx a b =
      Codex_log.debug "TMD.add" ;
      if BinaryMap.mem a !state then (
        let typ = BinaryMap.find a !state in
        Codex_log.debug "@[<hov 2>addition of offset %a to param. addr. %a@]"
            (Scalar.binary_pretty ~size:Type.offset_size ctx) b
            (binary_pretty ~size ctx) a;
          
        let new_typ = Type.add_offset ctx b typ in
        if not @@ BinaryMap.mem result !state then state := BinaryMap.add result new_typ !state ;
        (* result *)()
      )  

    let bindex ~size offset ctx a b =
      Codex_log.debug "TMD.bindex offset:%d %a %a" offset (binary_pretty ~size ctx) a (Scalar.binary_pretty ~size ctx) b ;
      Codex_log.debug "in TMD.bindex, a : %a" (SubAddress.binary_pretty ~size ctx) a ;
      let ofs = Scalar.Binary_Forward.(bimul ~size ~nsw:false ~nuw:false ctx (biconst ~size (Z.of_int offset) ctx) b) in
      let result = SubAddress.bindex ~size offset ctx a b in
      Codex_log.debug "in TMD.bindex, result : %a" (SubAddress.binary_pretty ~size ctx) result ;
      add ~size result ctx a ofs ;
      Codex_log.debug "TMD.bindex returning %a" (binary_pretty ~size ctx) result ;
      result

    let bisub ~size ctx a b = SubAddress.bisub ~size ctx a b

    let bshift ~size ~offset ~max ctx value =
      Codex_log.debug "TMD.bshift" ;
      let ofs = Scalar.Binary_Forward.biconst ~size (Z.of_int offset) ctx in
      let result = SubAddress.bshift ~size ~offset ~max ctx value in
      add ~size result ctx value ofs ;
      result

    let binary_unknown ~size ctx = SubAddress.binary_unknown ~size ctx

    (*--------------- Utility functions to call "assume" with the correct sizes ---------------*)

    type expr_size = Const of int | Min of int

    let eq_size sz1 sz2 =
      match sz1, sz2 with
      | Const i, Const j when i = j -> sz1
      | Min i, Const j when i < j -> sz2
      | Const j, Min i when i < j -> sz1
      | Min i, Min j -> Min (max i j)
      | Const i, Min j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Min " ^ (string_of_int j)))
      | Min i, Const j ->
          raise (Failure ("invalid expr size : Min " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))
      | Const i, Const j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))

    let rec expr_size ~size ctx expr =
      let open Ctypes.Pred in
      match expr with
      | Self -> Const size
      | Val(Const c) -> Min (Z.numbits c)
      | Val(Sym s) -> assert false (* let sz, _, _ = Type.symbol ctx s in Const sz *)
      | Binop(op, e1, e2) ->
          let sz1 = expr_size ~size ctx e1
          and sz2 = expr_size ~size ctx e2
          in eq_size sz1 sz2
      | Unop(Extract (index, len), e) ->
          let sz = expr_size ~size ctx e
          in eq_size sz (Const len)
      (* | Unop(op, e) -> *)

    let cmp_size ~size ctx expr1 expr2 =
      let size1 = expr_size ~size ctx expr1
      and size2 = expr_size ~size ctx expr2 in
      match eq_size size1 size2 with
      | Const sz | Min sz -> sz

    (* ------------------------------ Assume operations -------------------------------*)

    let assume_ptr_type ~size ctx value addr_typ =
      Codex_log.debug "TMD.assume_ptr_type %a" (Type.pp ctx) addr_typ ;
      assert (size mod 8 = 0);
      Codex_log.check "load_param_nonptr_old";
      let region_t =
        try Type.deref ctx ~size:(size / 8) addr_typ
        with Type.Type_error msg ->
          Codex_log.alarm "load_param_nonptr_old";
          Codex_log.error "%s" msg; [(size / 8, Type.NonPtr (Ctypes.word ~byte_size:(size / 8)))]          
      in
      match region_t with
      | [(_, IsPtr (PtrT{ppred;_} as region_t))] -> state := BinaryMap.add value region_t !state
      | [(_, NonPtr pred)] -> ()
      | [] -> assert false
      | _ -> raise (Failure "Load of memory region with composite type not yet supported")

    let assume_type ~size ctx value typ =
      let open Ctypes in
      match typ.descr with
      | Ptr { pointed ; index } ->
        (* imperative_use_invariant ~size ctx typ.pred value ; *)
        assume_ptr_type ~size ctx value
        Type.(PtrT{ptyp=typ; idx = index_zero ctx; fe = index_minus_one ctx;
                    ppred=Ctypes.Pred.True; ofs=0; non_aligned=index_zero ctx}) 
                    
      | _ -> assert false

    (** Helper function for [binary_unknown_typed]
        creates unknowns for a list of types (of things turned into types by [get_types])
        creates unknown values for all elements, returns their concatenatation
        @raise [Failure] if [~size] doesn't match the type's size *)
    (* let rec binary_unknown_typed_from_record ~size ~level ctx list =
        let folder (acc_value, acc_sz) typ =
          let sz = 8*(Ctypes.sizeof typ) in
          let value = binary_unknown_typed ~size:sz ~level ctx typ in
          let concat_value = Binary_Forward.bconcat ~size1:acc_sz ~size2:sz ctx acc_value value in
          concat_value, acc_sz + sz
        in
        let value, sz = List.fold_left folder (binary_empty ~size:0 ctx, 0) list in
        assert (size = sz);
        value
    *)

    (** Create a new binary unknown with given type.
        @raise [Failure] if [~size] doesn't match the type's size
        /!\ [~size] should be the bit size, not the byte size *)
    (* and binary_unknown_typed ~size ~level ctx typ =
      assert (size mod 8 = 0);
      let open Ctypes in
      let new_unknown () = binary_unknown ~size ~level ctx in
      let bin = match typ.descr with
        | Name n -> binary_unknown_typed ~size ~level ctx (inlined typ)
        | Void -> new_unknown ()
        | Base (byte_size, _) ->
            assert (size  = 8*byte_size);
            new_unknown ()
        | Enum enum ->
            assert (size = 8*enum.en_byte_size);
            new_unknown ()
        | Structure struc ->
            binary_unknown_typed_from_record ~size ~level ctx
            (List.map (fun (_offset, _name, typ) -> typ) struc.st_members)
        | Ptr{pointed; index} ->
            assert (size = Codex_config.ptr_size ());
            (* TODO change this...
               this should create a new pointer value which is either null
               or points to something of type pointed
               (whose value can be computed recursively) *)
            (* old_load_from_typed_region ~size ctx
            Type.(PtrT{ptyp=typ; idx = index_zero ctx; fe = index_minus_one ctx;
                        ppred=Ctypes.Pred.True; ofs=0}) *)
            new_unknown ()
        | Array (tp, Some (Const t)) ->
            let array_size = Z.to_int t in
            assert (array_size > 0);
            binary_unknown_typed_from_record ~size ~level ctx
            (List.init array_size (function _ -> tp))
        | Array (tp, _) ->
            failwith "Not implemented binary unknown of non-constant array"
        | Function (res, args) ->
            failwith "Not implemented binary unknown of function"

        | _ -> assert false
      in
      (* Add constraints if any *)
      assume_type ~size ctx bin typ ; bin
    *)


    (* let binary_unknown_typed ~size ~level ctx typ = assert false *)

    let binary_unknown_typed ~size ctx typ =
      Codex_log.debug "TMD.binary_unknown_typed %a" Ctypes.pp typ ;
      assert (size = Codex_config.ptr_size ()) ;
      let open Ctypes in
      let new_unknown () = binary_unknown ~size ctx in
      let bin = match typ.descr with
        | Ptr{pointed; index} ->
            assert (size = Codex_config.ptr_size ());
            new_unknown ()

        | _ -> assert false

      in
      (* Add constraints if any *)
      assume_type ~size ctx bin typ ; bin

  end

  type address = Address.binary

  module Memory
      (Value:Memory_sig.Value)
      (Lift:Memory_sig.Value_to_address with module Value := Value and module Address := Address)
    : Memory_sig.Memory
    with module Value = Value
     and module Address = Address
     and type boolean = Sub.Memory(Value)(Lift).boolean
  = struct

    let to_sub x = fst @@ Lift.ctx x
    module Address = Address
    module SubMemory = Sub.Memory(Value)(Lift)

    module Domain:Domain_sig.Minimal
      with module Context = SubMemory.Context
       and type boolean = SubMemory.boolean
      = SubMemory
    include Domain      

    
    module Scalar = Address.Scalar
    module Value = SubMemory.Value

    type address = Address.binary

    module Memory = SubMemory
    type memory = Memory.memory

    let pretty ctx fmt mem = SubMemory.pretty ctx fmt mem

    (* Load from a typed address, and return the corresponding value. *)
    let load_from_typed_region ~size (ctx:Context.t) (mem : Memory.memory) addr_typ addr =
      assert (size mod 8 = 0);
      let sub_ctx = to_sub ctx in
      Codex_log.check "load_param_nonptr";
      let region_t =
        try Type.deref sub_ctx ~size:(size / 8) addr_typ
        with Type.Type_error msg ->
          Codex_log.alarm "load_param_nonptr";
          Codex_log.error "%s" msg; [(size / 8, Type.NonPtr (Ctypes.word ~byte_size:(size / 8)))]          
      in
      
      (* let level = Scalar.Context.level ctx in *)
      
      let result = match region_t with
        | [(_, IsPtr (PtrT{ppred;ptyp;idx;fe;non_aligned;ofs}))] ->
          assert (ofs = 0);
          assert (Type.(check_invariant_subdomain ~size:offset_size sub_ctx Ctypes.Pred.(eq (Const Z.zero)) non_aligned)) ;
          (* TODO: change ~size to byte_size. *)
          let typ = Type.deref_to_ctype ~size:(size/8) sub_ctx addr_typ in
          (* let result = load_from_type ~size ~level 0 ctx mem typ addr in *)
          let result = SubMemory.typed_load ~size ctx mem addr typ in
          result
        | [(_, NonPtr typ)] ->
          (* let typ = Ctypes.word ~byte_size:(size/8) in *)
          (* let result = load_from_type ~size ~level 0 ctx mem typ addr in *)
          let result = SubMemory.typed_load ~size ctx mem addr typ in
          result
        | [] -> assert false
        | _ ->
          let typ = Type.deref_to_ctype ~size:(size/8) sub_ctx addr_typ in
          (* let result = load_from_type ~size ~level 0 ctx mem typ addr in *)
          let result = SubMemory.typed_load ~size ctx mem addr typ in
          result
          (* raise (Failure "Load of memory region with composite type not yet supported") *)
      in
      (* Codex_log.feedback "load_from_typed_region: result = %a" (DataReg.Value.binary_pretty ~size ctx) result; *)
      result

    
    let load ~size (ctx:Context.t) (mem : memory) addr_typ addr =
      let sub_ctx = to_sub ctx in

      Codex_log.debug ~level:3 "Typed_memory_domain.load ~size:%d addr_t:%a addr_val:%a" size
          (Type.pp sub_ctx) addr_typ
          (Address.binary_pretty ~size:32 sub_ctx) addr ;
      (* let ptr_size = Codex_config.ptr_size () in
       Codex_log.check "load_param_nonzero";
      if Address.is_null ~size:ptr_size ctx addr then (
        Codex_log.alarm "load_param_nonzero";
        Codex_log.error "Possibly dereferencing Zero." ;
      ) ;
      *)

      load_from_typed_region ~size ctx mem addr_typ addr

    let load ~size ctx mem addr : Value.binary =
      (* TODO : check if addr or mem are bottom ? *)
      if BinaryMap.mem addr !state then (
        let typ = BinaryMap.find addr !state in
        load ~size ctx mem typ addr
      ) else Memory.load ~size ctx mem addr
        (* raise (Failure "load: precision of address too low") *)
     

    let typed_load ~size ctx mem x typ = assert false

    let store ~size (ctx:Context.t) _mem addr_typ addr value =
      let sub_ctx = to_sub ctx in
      (*
      Codex_log.feedback "Typed_memory_domain.store value %a at addr. %a"
        (Binary_Representation.binary_pretty ~size ctx) value
        (Binary_Representation.binary_pretty ~size:32 ctx) addr;
      *)
      
      Codex_log.check "store_param_nonptr";
      let loaded_typ = try Type.deref_to_ctype ~size:(size/8) sub_ctx addr_typ
        with Type.Type_error msg ->
          Codex_log.alarm "store_param_nonptr"; Codex_log.error "%s" msg;
          Ctypes.word ~byte_size:(size/8)
      in
      SubMemory.typed_store ~size ctx _mem addr loaded_typ value

    let store ~size ctx mem addr value =
      if BinaryMap.mem addr !state then (
        let typ = BinaryMap.find addr !state in
        store ~size ctx mem typ addr value 
      ) else Memory.store ~size ctx mem addr value
      (* else
        raise (Failure "store: precision of address too low") *)
        (* Memory.store ~size ctx mem addr value *)

    let typed_store ~size ctx mem at typ value = assert false

    let serialize ctxa mema ctxb memb acc = SubMemory.serialize ctxa mema ctxb memb acc

    let initial ctx size = assert false

    let malloc ~id ~malloc_size ctx =
        Memory.malloc ~id ~malloc_size ctx

    (*
    let malloc ~id ~malloc_size ctx mem =
      let ptr_size = Codex_config.ptr_size () in
      let weak_typ = Ctypes.{descr = Weak (Ctypes.word ~byte_size:malloc_size); pred = Pred.True} in
      let ptr_typ = Ctypes.{descr = Ptr {pointed = weak_typ; index = Zero}; pred = Pred.neq (Const Z.zero)} in
      (* TODO : see if this works, otherwise initialize everything without going through "binary_unknown_typed" *)
      let addr = Address.binary_unknown_typed ~size:ptr_size ~level:(Scalar.Context.level ctx) ctx ptr_typ in
      addr, mem
    *)

    let free ctx mem addr = 
      if not (BinaryMap.mem addr !state) then
        Memory.free ctx mem addr
      else mem

    let unknown ~level ctx = Memory.unknown ~level ctx

    let memory_empty ctx = Memory.memory_empty ctx

    let should_focus ~size (ctx:Context.t) mem addr =
      let sub_ctx = to_sub ctx in
      Codex_log.debug ~level:2 "Typed_memory_domain.should_focus ~size:%d %a" size (Address.binary_pretty ~size sub_ctx) addr;
      let open Ctypes in
      if Address.Query.binary_is_empty ~size @@ Address.Query.binary ~size sub_ctx addr then
        begin
          Codex_log.debug ~level:2 "Typed_memory_domain.should_focus: returning None";
          None
        end
      else 
        try
          let Type.PtrT{ptyp;ofs;idx=_;fe=_;non_aligned;ppred=_} as addr_typ = BinaryMap.find addr !state in
          (* let typ = match ptyp.descr with
          | Ctypes.(Array (elt_typ, Some Const _)) -> Some elt_typ
          | Ctypes.Array (elt_typ, Some Sym _) -> None
          | other -> Some ptyp
          in *)
          let typ = ptyp in
          let typ_size =
            try Some (sizeof ptyp)
            with Unsizeable_type -> None 
          in

          let ofs_size = Type.offset_size in
          let non_aligned_sng = Scalar.Query.binary_is_singleton ~size:ofs_size @@
              Scalar.Query.binary ~size:ofs_size sub_ctx non_aligned in
          (* match non_aligned_sng, typ with
          | Some o, Some typ -> *)
          match non_aligned_sng, typ_size with
          | Some o, Some sz ->
              let ofs = Z.to_int o + ofs in
              let base = if ofs = 0 then addr else
                  Address.bshift ~size ~offset:(- ofs) ~max:None sub_ctx addr in
              
              Codex_log.debug ~level:2 "@[<v 2>Typed_memory_domain.should_focus: returning@ Some (%a,@.%d,@.%d)@]"
                (Address.binary_pretty ~size sub_ctx) base (sz * 8) (ofs * 8);
              Some (base, Ctypes.sizeof typ * 8, ofs * 8)

          | _ ->
              Codex_log.debug ~level:2 "Typed_memory_domain.should_focus: returning None";
              None

        with Not_found ->
          Codex_log.debug ~level:2 "Typed_memory_domain.should_focus: returning None";
          None
      

    let type_may_alias ctx ~size1 ~size2 t u =
      assert (size1 > 0);
      assert (size2 > 0);
      try
        let x_end_t = Type.add_offset ctx Scalar.Binary_Forward.(biconst
          ~size:Type.offset_size (Z.of_int @@ size1 - 1) ctx) t in
        not (Type.in_bounds ctx x_end_t)
        || let y_end_t = Type.add_offset ctx Scalar.Binary_Forward.(biconst
          ~size:Type.offset_size (Z.of_int @@ size2 - 1) ctx) u in
        not (Type.in_bounds ctx y_end_t)
        || let PtrT{ptyp = ptypx;_} = t in
        let PtrT{ptyp = ptypy;_} = u in
        Ctypes.contains ptypx ptypy
        || Ctypes.contains ptypy ptypx
      with Type.Type_error _ ->
        true

    let may_alias ~ptr_size (ctx:Context.t) ~size1 ~size2 x y =
      let sub_ctx = to_sub ctx in
      Codex_log.debug ~level:2 "@[<hov 2>WSD.may_alias@ ~ptr_size:%d@ ~size1:%d@ ~size2:%d@ \
        %a@ %a@]" ptr_size size1 size2 (Address.binary_pretty ~size:size1 sub_ctx) x (Address.binary_pretty ~size:size2 sub_ctx) y;
      let res =
      if not (BinaryMap.mem x !state && BinaryMap.mem y !state) then assert false
      else
        begin 
          let xt = BinaryMap.find x !state in
          let yt = BinaryMap.find y !state in
          let xval = (Address.binary2scalar_binary ~size:ptr_size sub_ctx x) in
          let yval = (Address.binary2scalar_binary ~size:ptr_size sub_ctx y) in
          (* If both pointers may be null, return true *)
          match xt, yt with
          | PtrT {ptyp={descr = Weak _; _}; _}, _ | _, PtrT {ptyp = {descr = Weak _; _}; _} -> false
          | _ ->
            begin
          (
          (not (Type.check_invariant_subdomain ~size:ptr_size sub_ctx
            Ctypes.Pred.(neq (Const Z.zero)) xval)
          && not (Type.check_invariant_subdomain ~size:ptr_size sub_ctx
            Ctypes.Pred.(neq (Const Z.zero)) yval))
          ||
          (assert (size1 > 0);
          assert (size2 > 0);
          let PtrT{ptyp = ptypx;_} = xt in
          let PtrT{ptyp = ptypy;_} = yt in
          (match ptypx.descr, ptypy.descr with (_,Weak _ | Weak _,_) -> false | _ -> true) 
          &&
          try
            let x_end_t = Type.add_offset sub_ctx Scalar.Binary_Forward.(biconst
              ~size:Type.offset_size (Z.of_int @@ size1 - 1) sub_ctx) xt in
            not (Type.in_bounds sub_ctx x_end_t)
            || let y_end_t = Type.add_offset sub_ctx Scalar.Binary_Forward.(biconst
              ~size:Type.offset_size (Z.of_int @@ size2 - 1) sub_ctx) yt in
            not (Type.in_bounds sub_ctx y_end_t)
            ||
            (*
            let PtrT{ptyp = ptypx;_} = xt in
            let PtrT{ptyp = ptypy;_} = yt in
            *)
            Ctypes.contains ptypx ptypy || Ctypes.contains ptypy ptypx
          with Type.Type_error _ ->
            true
          ))
          (* Since this a product with a numeric abstraction, we use numeric
           * information as well. In practice, this will always be true, except
           * if some specific numeric predicate was retained (which I guess
           * could happen, although I don't remember in which cases). *)
          && (
            let quadri = Address.beq ~size:ptr_size sub_ctx x y
              |> Address.query_boolean sub_ctx in
            match quadri with
            | Lattices.Quadrivalent.False -> false
            | Lattices.Quadrivalent.Top | Lattices.Quadrivalent.True
            | Lattices.Quadrivalent.Bottom-> true
          )
          end
        end
      in Codex_log.debug "WSD.may_alias returning %b" res ; res
    
  end

end
