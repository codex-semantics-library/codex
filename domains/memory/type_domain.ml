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

module type S = sig
  module BR : Memory_sig.Fixed_size_value_domain
  type t = Ctypes.typ
  val array_index_size : int
  val index_zero : BR.Scalar.Context.t -> BR.binary
  val index_minus_one : BR.Scalar.Context.t -> BR.binary
  val offset_size : int
  val eq : only_descr:bool -> BR.Scalar.Context.t -> t -> t -> bool
  val imprecise_eq : t -> t -> bool
  exception Global_symbol_not_found
  val fresh_symbol : unit -> string
  val symbol : BR.Scalar.Context.t -> string -> int * BR.binary * t
  val simple_symbol : BR.Scalar.Context.t -> string -> int * BR.binary * t
  val add_symbol : BR.Scalar.Context.t -> string -> int -> BR.binary -> t -> unit
  val is_symbol : BR.Scalar.Context.t -> string -> bool
  val type_of_binary : BR.Scalar.Context.t -> BR.binary -> t option
  val cond_of_pred_subdomain : size:int -> BR.Scalar.Context.t -> Ctypes.Pred.t
    -> BR.binary -> BR.boolean
  val check_invariant_subdomain : size:int -> BR.Scalar.Context.t -> Ctypes.Pred.t
    -> BR.binary -> bool
  val compare : t -> t -> int
  val pp : BR.Scalar.Context.t -> Format.formatter -> t -> unit
  exception Type_error of string
  val serialize : BR.Scalar.Context.t -> t -> BR.Scalar.Context.t -> t -> 'a BR.Scalar.Context.in_acc
    -> (t option, 'a) BR.Scalar.Context.result
  val join : BR.Scalar.Context.t -> t -> t -> t option
  val serialize_with_bottom_left : BR.Scalar.Context.t -> BR.Scalar.Context.t -> t -> 'a BR.Scalar.Context.in_acc
    -> (t, 'a) BR.Scalar.Context.result
  val serialize_with_bottom_right : BR.Scalar.Context.t -> t -> BR.Scalar.Context.t -> 'a BR.Scalar.Context.in_acc
    -> (t, 'a) BR.Scalar.Context.result
  type dereferenced_value = Ctypes.typ
  val deref : size:int -> BR.Scalar.Context.t -> t -> (int * Ctypes.typ) list
  val deref_to_ctype : size:int -> BR.Scalar.Context.t -> t -> Ctypes.typ
  val dereferenced_value_of_typ: size:int ->
    BR.Scalar.Context.t -> Ctypes.typ list -> (int * Ctypes.typ) list
  val subtype : BR.Scalar.Context.t -> t -> t -> bool
  val may_subtype : BR.Scalar.Context.t -> t -> t -> bool
  val add_offset : BR.Scalar.Context.t -> BR.Binary.t -> t -> t
  val in_bounds : BR.Scalar.Context.t -> t -> bool
  val is_empty : BR.Scalar.Context.t -> t -> bool
end

module Make (BR : Memory_sig.Fixed_size_value_domain)
    (TS : Ctypes.Type_settings)
: S with module BR = BR
= struct
  type offset = int
  let offset_size = 32

  module BR = BR

  type t = Ctypes.typ

  let array_index_size = 32

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
    | Or -> bor ~size ctx
    | Concat (size1,size2) -> bconcat ~size1 ~size2 ctx

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

  let index_zero ctx =
    BR.Binary_Forward.biconst ~size:array_index_size Z.zero ctx

  let index_minus_one ctx =
    BR.Binary_Forward.biconst ~size:array_index_size (Z.sub Z.zero Z.one) ctx

  exception Global_symbol_not_found

  module String_map = Extstdlib.Map.Make(String);;

  let fresh_int =
    let fresh_counter = ref (0 : int) in
    fun () ->
      incr fresh_counter ;
      !fresh_counter
      (* var ^ (string_of_int !fresh_counter) *)

  let fresh_symbol () = string_of_int (fresh_int ())
  
  let rec global_constants =
    let table = ref ([] : (string * (int * BR.binary * t)) list) in
    let initialized = ref false in
    fun ctx ->
      if !initialized then table
      else begin
        table := List.map (fun (name,size,pred) ->
          let b = BR.binary_unknown ~size ctx in
          (name, (size, use_invariant_subdomain ~size ctx pred b, Ctypes.word ~byte_size:(size / 8)))
        ) TS.global_symbols;
        initialized := true;
        table
      end

  and symbol ctx s = (* TODO : some with and some without assume caution *)
    try List.assoc s (!(global_constants ctx))
    with Not_found -> Format.printf "cannot find global symbolic variable : %s\n" s ; raise Global_symbol_not_found

  and simple_symbol ctx s = (* TODO : some with and some without assume caution *)
    try
      List.assoc s (!(global_constants ctx))
    with Not_found -> Format.printf "cannot find global symbolic variable : %s\n" s ; raise Global_symbol_not_found

  and add_symbol ctx name size binary typ =
    let table = global_constants ctx in
      table := (name, (size, binary, typ))::(!table)

  and is_symbol ctx s =
    List.mem_assoc s (!(global_constants ctx))

  and type_of_binary ctx bin = 
    match List.find_opt (fun (_, (_, binary, _)) -> BR.Binary.equal binary bin) (!(global_constants ctx)) with
    | None -> None
    | Some (_, (_, _, typ)) -> Some typ
  
  and binary_of_value ~size ctx v =
    let open Ctypes in
    match v with
    | Const x -> BR.Binary_Forward.biconst ~size x ctx
    | Sym s ->
        let sz,v,t = symbol ctx s in
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
    let is_true = (BR.query_boolean ctx c) in
    match is_true with
    | Lattices.Quadrivalent.True -> true
    | _ -> false (* Possibly false *)

  and use_invariant_subdomain ~size ctx pred (value : BR.binary) =
    let open Ctypes.Pred in
    match pred with
    | True -> value
    | _ ->
      let cond = cond_of_pred_subdomain ~size ctx pred value in
      BR.imperative_assume ctx cond ;
      value

  let get_index ctx s = 
    let (_, bin, _) = symbol ctx s in
    bin

  let get_simple_index ctx s = 
    let (_, bin, _) = simple_symbol ctx s in
    bin

  let is_empty ctx typ =
    let open Ctypes in
    match (inlined typ).descr with
    | Ptr {pointed= _; index = Idx {idx;fe;ofs=_}} ->
      let size = array_index_size in
      let idx = get_index ctx idx
      and fe = get_index ctx fe in
      BR.Query.(binary_is_empty ~size (binary ~size ctx idx))
      || BR.Query.(binary_is_empty ~size (binary ~size ctx fe))

    | _ -> false (* assert false *)

  (* let pp_times = ref 0 ;; *)

  let pp ctx fmt typ =
    match typ.Ctypes.descr with
    | Ptr {pointed={descr = Array (ptyp, Some (Sym s)); pred}; index = Idx {idx; fe; ofs}} ->
      begin
        let idx = get_simple_index ctx idx in
        let fe = get_simple_index ctx fe in
        let array_size, sz, _ = simple_symbol ctx s in
        assert (array_size = offset_size) ;
        match typ.pred with
        | Ctypes.Pred.True ->
            Format.fprintf fmt "@[<hov 2>(%a[%a])[%a].%d*@]"          
            (* Format.fprintf fmt "@[<hov 2>(%a[%a])[%a][fe:%a].%d*@]" *)
              Ctypes.pp ptyp
              (BR.binary_pretty ~size:offset_size ctx) sz
              (BR.binary_pretty ~size:offset_size ctx) idx
              (* (BR.binary_pretty ~size:offset_size ctx) fe *)
              ofs
        | _ ->
            (* Format.fprintf fmt "@[<hov 2>({self :@ (%a[%a])[%a][fe:%a].(%d)*@ |@ %a})@]"           *)
            Format.fprintf fmt "@[<hov 2>({self :@ (%a[%a])[%a].(%d)*@ |@ %a})@]"
              Ctypes.pp ptyp
              (BR.binary_pretty ~size:offset_size ctx) sz
              (BR.binary_pretty ~size:offset_size ctx) idx
              (* (BR.binary_pretty ~size:offset_size ctx) fe *)
              ofs
              Ctypes.Pred.pp typ.pred

        (* ; (** Query to check when we lose the constraint on fe *)
        if !pp_times > 3 then (
          (* let c1 = check_invariant_subdomain ~size:array_index_size ctx (Ctypes.Pred.sleq (Const Z.zero)) fe in *)
          let c2 = check_invariant_subdomain ~size:array_index_size ctx (Ctypes.Pred.ult (Sym s)) idx in
          let c3 = check_invariant_subdomain ~size:array_index_size ctx (Ctypes.Pred.sgeq (Const Z.zero)) idx in
          (* assert c1 ; *)
          assert c2 ;
          assert c3 ) ;
        incr pp_times *)
      end

    | Ptr {pointed=ptyp; index = Idx {idx; fe; ofs}} ->
      begin
        let idx = get_simple_index ctx idx in
        let fe = get_simple_index ctx fe in
        match typ.pred with
        | Ctypes.Pred.True ->
            Format.fprintf fmt "@[<hov 2>(%a)[%a].%d*@]"          
            (* Format.fprintf fmt "@[<hov 2>(%a)[%a][fe:%a].%d*@]" *)
              Ctypes.pp ptyp
              (BR.binary_pretty ~size:offset_size ctx) idx
              (* (BR.binary_pretty ~size:offset_size ctx) fe *)
              ofs
        | _ ->
            (* Format.fprintf fmt "@[<hov 2>({self :@ %a[%a][fe:%a].(%d)*@ |@ %a})@]"           *)
            Format.fprintf fmt "@[<hov 2>({self :@ %a[%a].(%d)*@ |@ %a})@]"
              Ctypes.pp ptyp
              (BR.binary_pretty ~size:offset_size ctx) idx
              (* (BR.binary_pretty ~size:offset_size ctx) fe *)
              ofs
              Ctypes.Pred.pp typ.pred
      end
    
    | _ -> Ctypes.pp fmt typ

    

  let eq ~only_descr ctx a b =
    let open Ctypes in
    match Ctypes.(a.descr, b.descr) with
    | Ptr {pointed=t1;index=Idx {idx=i1;fe=fe1;ofs=j1}}, Ptr {pointed=t2;index=Idx {idx=i2;fe=fe2;ofs=j2}} when
          Ctypes.equiv ~only_descr t1 t2 && ((=) : int -> int -> bool) j1 j2
          && let i1 = get_index ctx i1 and fe1 = get_index ctx fe1
          and i2 = get_index ctx i2 and fe2 = get_index ctx fe2 in 
          BR.Binary.equal i1 i2 && BR.Binary.equal fe1 fe2 ->
        true
    | Ptr {pointed=t1;index=Idx {idx=i1;fe=fe1;ofs=j1}}, Ptr {pointed=t2;index=Idx {idx=i2;fe=fe2;ofs=j2}} ->
        if Ctypes.equiv ~only_descr t1 t2 && ((=) : int -> int -> bool) j1 j2 then
          let size = offset_size in
          let i1 = get_index ctx i1 and fe1 = get_index ctx fe1
          and i2 = get_index ctx i2 and fe2 = get_index ctx fe2 in
          let i1 = BR.Query.binary_is_singleton ~size @@ BR.Query.binary ~size ctx i1 in
          let i2 = BR.Query.binary_is_singleton ~size @@ BR.Query.binary ~size ctx i2 in
          match i1,i2 with
          | None,_ | _,None -> false
          | Some x, Some y ->
              let c = Z.equal x y in
              if not c then false else
                let fe1 =
                  BR.Query.binary_is_singleton ~size @@ BR.Query.binary ~size ctx fe1 in
                let fe2 =
                  BR.Query.binary_is_singleton ~size @@ BR.Query.binary ~size ctx fe2 in
                match fe1,fe2 with
                | None,_ | _,None -> false
                | Some x, Some y -> Z.equal x y
        else false

    | _ -> equiv ~only_descr a b

  let imprecise_eq a b =
    let open Ctypes in
    match a.descr, b.descr with
    | Ptr {pointed=t1;index=Idx {idx=i1;fe=fe1;ofs=j1}}, Ptr {pointed=t2;index=Idx {idx=i2;fe=fe2;ofs=j2}} ->
        if Ctypes.equiv ~only_descr:false t1 t2 && ((=) : int -> int -> bool) j1 j2
          then
            (* TODOML: This is probably wrong. *)
            let ctx = (BR.Scalar.root_context ()) in
            let i1 = get_index ctx i1 and fe1 = get_index ctx fe1
            and i2 = get_index ctx i2 and fe2 = get_index ctx fe2 in
            BR.Binary.equal i1 i2 && BR.Binary.equal fe1 fe2
          else false

    | _ -> equal ~only_descr:false a b

  let compare (x : t) (y : t) =
    match Ctypes.(x.descr, y.descr)  with
    | Ptr {pointed=t1;index=Idx {idx=i1;fe=fe1;ofs=j1}}, Ptr {pointed=t2;index=Idx {idx=i2;fe=fe2;ofs=j2}} ->
        let c = Ctypes.compare ~only_descr:false t1 t2 in
        if c <> 0 then c else
          (* TODOML: calling root_ctx is probably wrong. *)
          let ctx = (BR.Scalar.root_context ()) in
          let i1 = get_index ctx i1 and fe1 = get_index ctx fe1
          and i2 = get_index ctx i2 and fe2 = get_index ctx fe2 in
          let c = (compare : int -> int -> int) j1 j2 in
          if c <> 0 then c
          else
            let c = BR.Binary.compare i1 i2 in
            if c <> 0 then c
            else
              let c = BR.Binary.compare fe1 fe2 in
              if c <> 0 then c
              else Ctypes.Pred.compare x.pred y.pred

    | _ -> Ctypes.compare ~only_descr:false x y

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
    | Or -> bor ~size ctx
    | Concat (size1,size2) -> bconcat ~size1 ~size2 ctx

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
    match (inlined typ).descr with
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

        (* let lt_size2 =
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
        in *)
        
        let alarm =
          if not (geq_zero && lt_size) then begin
            let msg = Format.asprintf "@[<hov 2>Out-of-bounds access at index %a in array of \
              size %a.@]" (BR.binary_pretty ~size:offset_size ctx) array_index
              (fun fmt n -> match n with
                | Const x -> Z.pp_print fmt x
                | Sym s -> BR.binary_pretty ~size:array_index_size ctx fmt (get_index ctx s)) nb_elem in
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
        let idx = BR.binary_unknown ~size ctx in
        let idx = use_invariant_subdomain ~size ctx Pred.(sgeq (Const Z.zero)) idx in
        (* BR.Scalar.imperative_assume ctx (BR.Boolean_Forward.not ctx @@ *)
        (*     BR.Binary_Forward.bisle ~size ctx upper idx); *)
        assert false;
        let fe = BR.Binary_Forward.bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false idx upper in
        let fe = use_invariant_subdomain ~size ctx Pred.(slt (Const Z.zero)) fe in
        idx, fe
    | PastEnd, None ->
        use_invariant_subdomain ~size ctx Pred.(sgt (Const Z.zero)) @@
          BR.binary_unknown ~size ctx,
        index_zero ctx
    | PastEnd, Some upper ->
        let upper = binary_of_value ~size ctx upper in
        let idx = use_invariant_subdomain ~size ctx Pred.(sgt (Const Z.zero)) @@
          BR.binary_unknown ~size ctx in
        (* BR.Scalar.imperative_assume ctx (BR.Binary_Forward.beq ~size ctx upper idx); *)
        assert false;
        idx,
        index_zero ctx

    (* | Idx {idx; fe; ofs = _}, _ -> (get_index ctx idx), (get_index ctx fe) *)
    | _ -> assert false

  let binary_min ~size b =
    let inf = Z.sub Z.zero (Z.shift_left Z.one size) in
    let sup = Z.shift_left Z.one size in
    let exception E of Z.t in
    try BR.Query.binary_fold_crop ~size b ~inf ~sup None (fun x ->
      raise (E x)
    )
    with E x -> Some x

  let rec add_offset' ctx pointed_t old_index old_fe old_offset (offset : BR.binary) =
    (* Codex_log.debug ~level:2 "add_offset' %a ~old_offset:%d ~old_index:%a %a" Ctypes.pp pointed_t old_offset (BR.binary_pretty ~size:offset_size ctx) old_index (BR.binary_pretty ~size:offset_size ctx) offset; *)
    Codex_log.debug ~level:2 "add_offset' %a ~old_offset:%d ~old_index:%a ~old_fe:%a %a"
        Ctypes.pp pointed_t
        old_offset
        (BR.binary_pretty ~size:offset_size ctx) old_index
        (BR.binary_pretty ~size:offset_size ctx) old_fe
        (BR.binary_pretty ~size:offset_size ctx) offset;
    let open Ctypes in
    match pointed_t.descr with
    | Array (elem_t, size) ->
        let ofs_size = offset_size in
        let elem_sz = Ctypes.sizeof elem_t in
        let elem_sz_b = BR.Binary_Forward.biconst ~size:ofs_size
          (Z.of_int elem_sz) ctx in
        let delta = BR.Binary_Forward.(biadd ~size:ofs_size ctx ~nsw:false ~nuw:false ~nusw:false
          offset
          (biconst ~size:ofs_size (Z.of_int old_offset) ctx)) in
        let index_diff = BR.Binary_Forward.bisdiv ~size:ofs_size ctx delta elem_sz_b in
        let new_offset = BR.Binary_Forward.bismod ~size:ofs_size ctx delta elem_sz_b in
        let new_offset_lattice = BR.Query.binary ~size:ofs_size ctx new_offset in
        let zero = BR.Binary_Forward.biconst ~size:ofs_size Z.zero ctx in
        let new_offset =
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
        in
        (* If [new_offset] represents a negative integer (on 32 bits),
         * first compute that negative integer. Then, it means that [delta]
         * was negative, and we adjust the values to reflect a real
         * euclidean division. *)
        (* XXX: this code only works in a 64-bit OCaml runtime. *)
        let new_offset, index_diff =
          if new_offset >= 1 lsl 31 then
            elem_sz + (new_offset - (1 lsl 32)), BR.Binary_Forward.(bisub ~size:ofs_size
              ~nsw:false ~nuw:false ~nusw:false ctx index_diff (biconst ~size:ofs_size Z.one ctx))
          else new_offset, index_diff
        in

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
        let new_index = BR.Binary_Forward.biadd ~size:ofs_size ~nsw:true
          ~nuw:false ~nusw:false ctx old_index index_diff in
        let new_fe = BR.Binary_Forward.biadd ~size:ofs_size ~nsw:false
          ~nuw:false ~nusw:false ctx old_fe index_diff in
        
        if index_diff_zero then
          pointed_t, new_index, new_fe, new_offset
        else begin
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
                      BR.Binary_Forward.biadd ~size:array_index_size ~nsw:false ~nuw:false ctx new_index @@
                      BR.Binary_Forward.biconst ~size:array_index_size Z.one ctx *)
                  check_invariant_subdomain ~size:array_index_size ctx Pred.(ult s) new_index
                  || check_invariant_subdomain ~size:array_index_size ctx Pred.(slt s) new_index (* TODO : simply by checking (slt s) and (s > 0) directly in ult *)
                  || check_invariant_subdomain ~size:array_index_size ctx Pred.(eq s) new_index
                  (* check_invariant_subdomain ~size:array_index_size ctx (Pred.sleq s) new_index *)
               | None -> false
          in
          Codex_log.debug "c1 && c2 : %b && %b" c1 c2 ;
          if c1 && c2
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
        end
    | stripped_pointed_t ->
        let t_sz = Ctypes.sizeof pointed_t in
        let ofs_size = offset_size in
        let t_sz_b = BR.Binary_Forward.biconst ~size:ofs_size (Z.of_int t_sz) ctx in
        let zero = BR.Binary_Forward.biconst ~size:ofs_size Z.zero ctx in
        let total_b =
          let size = ofs_size in BR.Binary_Forward.(
            biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx offset @@
            biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx (biconst ~size (Z.of_int old_offset) ctx) @@
            bimul ~size ~nsw:false ~nuw:false ctx old_index t_sz_b
          ) in
        let total_lattice = BR.Query.binary ~size:ofs_size ctx total_b in
        begin match BR.Query.binary_is_singleton ~size:ofs_size total_lattice with
        | None ->
          Codex_log.debug ~level:3 "Imprecise offset. Attempt to weaken the type \
            to find an array...";
          (* Can be improved. Notably, this weakening could be performed more
           * than once and only give up when reaching Top. *)
          let give_up () =
            let msg = Format.asprintf "@[<hov 2>Adding imprecise offset %a is \
              not  allowed@]" (BR.binary_pretty ctx ~size:ofs_size) total_b in
            raise (Type_error msg)
          in
          begin match binary_min ~size:ofs_size total_lattice with
          | Some min ->
            begin try
              let new_t, _ = type_at_offset pointed_t (Z.to_int min) in
              begin match new_t.Ctypes.descr with
              | Ctypes.(Array (_, ar_size)) ->
                  let new_ofs = BR.Binary_Forward.(bisub ~size:ofs_size ctx ~nsw:false ~nuw:false ~nusw:false total_b @@ biconst ~size:ofs_size min ctx) in
                  let idx, fe = focus_idx ctx ar_size Ctypes.Zero in
                  (*
                  let new_t = Ptr{pointed=new_t;index=Zero} in
                  let new_t = {descr=new_t;pred=Pred.True} in
                  *)
                  add_offset' ctx new_t idx fe 0 new_ofs
              | _ -> give_up ()
              end
            with Exit -> give_up ()
            end
          | None -> give_up ()
          end
        | Some total ->
          let total = Z.to_int total in
          let lesser_sz = BR.Boolean_Forward.not ctx @@
            BR.Binary_Forward.bisle ~size:ofs_size ctx t_sz_b total_b in
          let geq_zero = BR.Binary_Forward.bisle ~size:ofs_size ctx zero total_b in
          let in_bounds = BR.Boolean_Forward.(&&) ctx geq_zero lesser_sz in
          let in_bounds = BR.query_boolean ctx in_bounds in
          begin match in_bounds with
          | Lattices.Quadrivalent.True -> pointed_t, old_index, old_fe, total
          | _ ->
              let msg = Format.asprintf "Offset %a is out of the bounds of the pointed type (%a)"
                (BR.binary_pretty ~size:ofs_size ctx) total_b  Ctypes.pp pointed_t in
              raise (Type_error msg)
          end
        end

  let add_offset ctx incr t =
    let open Ctypes in
    match t.descr with
    | Ptr {pointed; index = Idx {idx;fe;ofs}} ->
      (* let idx = get_index ctx idx
      and fe = get_index ctx fe in *)
      let _, idx, t_idx = symbol ctx idx in
      let _, fe, t_fe = symbol ctx fe in
      let pointed,idx,fe,ofs = add_offset' ctx pointed idx fe ofs incr in
      let new_vi = fresh_symbol () in
      let new_vf = fresh_symbol () in
      add_symbol ctx new_vi offset_size idx t_idx ;
      add_symbol ctx new_vf offset_size fe t_fe ;
      {t with descr = Ptr{pointed; index = Idx {idx = new_vi; fe = new_vf;ofs}}}

    | _ -> assert false (* t *)


  let in_bounds ctx t =
    let open Ctypes in
    match t.descr with
    | Ptr {pointed; index = Idx {idx;fe;ofs}} ->
      begin
        let idx = get_index ctx idx
        and fe = get_index ctx fe in
        match pointed.descr with
        | Array (x, Some ar_size) ->
            check_invariant_subdomain ~size:array_index_size ctx Pred.(sgeq (Const Z.zero)) idx
            || check_invariant_subdomain ~size:array_index_size ctx Pred.(slt (Const Z.zero)) fe
        | Array (_, None) ->
            raise (Invalid_argument "in_bounds: Array without a length")
        | _ -> true
      end

    | _ -> assert false

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
    let sup = Z.shift_left Z.one size in
    let exception Exit of Z.t in
    try BR.Query.binary_fold_crop ~size b ~inf ~sup None (fun x acc ->
      raise (Exit x)
    )
    with Exit x -> Some x

  let query_max ~size ctx b =
    let minus_b = BR.Binary_Forward.(bisub ~size ~nsw:false ~nuw:false ~nusw:false ctx
      (biconst ~size Z.zero ctx) b) in
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
    | {descr = Ptr {pointed = ({descr=Array (elem_t, m_sz1);_}); index = Idx {idx=t_idx; fe=t_fe; ofs=t_ofs}}; pred=pred_t},
      {descr = Ptr {pointed = ({descr=Array (elem_u, m_sz2);_}); index = Idx {idx=u_idx; fe=u_fe; ofs=u_ofs}}; pred=pred_u}
      when Ctypes.equiv ~only_descr:false elem_t elem_u
      && t_ofs = u_ofs ->
        let t_idx = get_simple_index ctxt t_idx and t_fe = get_simple_index ctxt t_fe
        and u_idx = get_simple_index ctxu u_idx and u_fe = get_simple_index ctxu u_fe in
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
        let BR.Scalar.Context.Result (_, acc, deserialize_idx) = BR.serialize ~size:offset_size
          ctxt t_idx ctxu u_idx (true, acc) in
        let inc1 =
          let iu = BR.Query.binary ~size:array_index_size ctxu u_idx in
          let it = BR.Query.binary ~size:array_index_size ctxt t_idx in
          map2 Z.leq false (query_min ~size:array_index_size it) (query_min ~size:array_index_size iu)
        in
        Codex_log.debug ~level:2 "index inclusion: %B" inc1;
        let BR.Scalar.Context.Result (_, acc, deserialize_fe) = BR.serialize ~size:offset_size
          ctxt t_fe ctxu u_fe (true, acc) in
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
        BR.Scalar.Context.Result (included, acc, fun ctx output ->
          let new_fe, output = deserialize_fe ctx output in
          let new_index, output = deserialize_idx ctx output in
          let zero = index_zero ctx
          and minus_one = index_minus_one ctx in
          let ret =
            if m_new_size = None
            then 
              let new_vi = fresh_symbol ()
              and new_vf = fresh_symbol () in
              add_symbol ctx new_vi offset_size zero Ctypes.int ;
              add_symbol ctx new_vf offset_size minus_one Ctypes.int ;
              Some Ctypes.({
                descr = Ptr{pointed = elem_t; index = Idx {idx = new_vi; fe = new_vf; ofs = t_ofs}};
                pred = new_pred
              })
            else
              let new_vi = fresh_symbol ()
              and new_vf = fresh_symbol () in
              add_symbol ctx new_vi offset_size new_index Ctypes.int ;
              add_symbol ctx new_vf offset_size new_fe Ctypes.int ;
              Some Ctypes.({
                descr = Ptr {
                  pointed = ({descr=Array (elem_t, m_new_size); pred = Pred.True });
                  index = Idx {idx = new_vi; fe = new_vf; ofs = t_ofs}};
                pred = new_pred;
              })
          in
          (* let ctx = assert false in *)
          Codex_log.debug ~level:2 "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret;
          ret, output)

    (* | {descr = Ptr {pointed = ({descr=Array (elem_t, m_sz1);_}); index = Idx {idx=t_idx; fe=t_fe; ofs=t_ofs}}; pred=pred_t},
      {descr = Ptr {pointed = ({descr=Array (elem_u, m_sz2);_}); index = Idx {idx=u_idx; fe=u_fe; ofs=u_ofs}}; pred=pred_u}
      when Ctypes.equiv ~only_descr:false elem_t elem_u
      && t_ofs <> u_ofs ->
        begin
          let elem_t = Ctypes.inlined elem_t in
          let elem_u = Ctypes.inlined elem_u in
          match elem_t.descr, elem_u.descr with
          | Array (elem_t', Some (Const sz_t)),
            Array (elem_u', Some (Const sz_u))
            when sz_t = sz_u
            && t_ofs mod (Ctypes.sizeof elem_t') = 0
            && u_ofs mod (Ctypes.sizeof elem_u') = 0 ->

              let elem_size = Ctypes.sizeof elem_t in

              let t_idx = get_index ctxt t_idx and t_fe = get_index ctxt t_fe
              and u_idx = get_index ctxu u_idx and u_fe = get_index ctxu u_fe in

              (* let elem_t_size_b = BR.Binary_Forward.biconst ~size:offset_size (Z.of_int elem_size) ctxt in

              let t_idx = BR.Binary_Forward.bimul ~size:offset_size ~nsw:false ~nuw:false ctxt t_idx elem_t_size_b in
              let t_fe = BR.Binary_Forward.bimul ~size:offset_size ~nsw:false ~nuw:false ctxt t_fe elem_t_size_b in

              let t_ofs_b = BR.Binary_Forward.biconst ~size:offset_size (Z.of_int t_ofs) ctxt in
              let t_idx = BR.Binary_Forward.biadd ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctxt t_idx t_ofs_b in
              let t_fe = BR.Binary_Forward.biadd ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctxt t_fe t_ofs_b in

              let elem_u_size_b = BR.Binary_Forward.biconst ~size:offset_size (Z.of_int elem_size) ctxu in

              let u_idx = BR.Binary_Forward.bimul ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctxu u_idx elem_u_size_b in
              let u_fe = BR.Binary_Forward.bimul ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctxt u_fe elem_u_size_b in

              let u_ofs_b = BR.Binary_Forward.biconst ~size:offset_size (Z.of_int u_ofs) ctxt in
              let u_idx = BR.Binary_Forward.biadd ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctxt u_idx u_ofs_b in
              let u_fe = BR.Binary_Forward.biadd ~size:offset_size ~nsw:false ~nuw:false ~nusw:false ctxt u_fe u_ofs_b in *)

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
              let BR.Scalar.Result (_, acc, deserialize_idx) =
                BR.serialize ~size:offset_size ctxt t_idx ctxu u_idx (true, acc) in
              let inc1 =
                let iu = BR.Query.binary ~size:array_index_size ctxu u_idx in
                let it = BR.Query.binary ~size:array_index_size ctxt t_idx in
                map2 Z.leq false (query_min ~size:array_index_size it) (query_min ~size:array_index_size iu)
              in
              Codex_log.debug ~level:2 "index inclusion: %B" inc1;
              let BR.Scalar.Result (_, acc, deserialize_fe) =
                BR.serialize ~size:offset_size ctxt t_fe ctxu u_fe (true, acc) in
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
              BR.Scalar.Result (included, acc, fun ctx output ->
                let new_fe, output = deserialize_fe ctx output in
                let new_index, output = deserialize_idx ctx output in
                let zero = index_zero ctx
                and minus_one = index_minus_one ctx in
                let ret =
                  match  m_new_size with
                  | None ->
                    let new_vi = fresh_symbol ()
                    and new_vf = fresh_symbol () in
                    add_symbol ctx new_vi offset_size zero ;
                    add_symbol ctx new_vf offset_size minus_one ;
                    Some Ctypes.({
                      descr = Ptr{pointed = elem_t; index = Idx {idx = new_vi; fe = new_vf; ofs = t_ofs}};
                      pred = new_pred
                    })
                  | Some size ->
                    let new_vi = fresh_symbol ()
                    and new_vf = fresh_symbol () in
                    let new_s = fresh_symbol () in

                    (* let new_size =
                      match size with
                      | Const sz -> Some (Ctypes.Const (Z.mul sz (Z.of_int elem_size)))
                      | Sym s ->
                          let old_size = get_index ctx s in
                          let elem_size_b = BR.Binary_Forward.biconst ~size:offset_size (Z.of_int elem_size) ctx in
                          let new_size = BR.Binary_Forward.bimul ~size:offset_size ~nsw:false ~nuw:false ctx old_size elem_size_b in
                          add_symbol ctx new_s offset_size new_size ;
                          Some (Ctypes.Sym new_s)
                    in *)
                    let new_size = m_new_size in

                    add_symbol ctx new_vi offset_size new_index ;
                    add_symbol ctx new_vf offset_size new_fe ;
                    let res =
                    Some Ctypes.({
                      descr = Ptr {
                        pointed = ({descr=Array (elem_t', new_size); pred = Pred.True });
                        index = Idx {idx = new_vi; fe = new_vf; ofs = 0}};
                      pred = new_pred;
                    })
                    in Option.iter (fun r -> Codex_log.debug "Type.serialize returning %a" (pp ctx) r) res ; res
                in
                (* let ctx = assert false in *)
                Codex_log.debug ~level:2 "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret;
                ret, output)

          | _ ->
            Codex_log.alarm "serialize";
            Codex_log.error "Type error during serialize no. %d" count ;
            Codex_log.debug ~level:2 "serialize no. %d: inclusion is false" count;
            BR.Scalar.Result (false, acc, fun ctx output -> None, output)

        end *)

    | {descr = Ptr {pointed=pt; index = Idx {idx=t_idx; fe = t_fe; ofs=t_ofs}}; pred = pred_t},
      {descr = Ptr {pointed=pu; index = Idx {idx=u_idx; fe = u_fe; ofs=u_ofs}}; pred = pred_u}
      when Ctypes.(equiv_descr ~only_descr:false pt.descr pu.descr)
      && t_ofs = u_ofs ->
        let t_idx = get_simple_index ctxt t_idx and t_fe = get_simple_index ctxt t_fe
        and u_idx = get_simple_index ctxu u_idx and u_fe = get_simple_index ctxu u_fe in
        let inc0, new_pred = join_pred pred_t pred_u in
        Codex_log.debug ~level:2 "predicate inclusion: %B" inc0;
        Codex_log.debug ~level:2 "joining of indices...";
        (* Join indices. This should only happen between indices zero and
         * bottom, since it is an invariant that all indices of non-array
         * types should be included in the singleton zero. *)
        let BR.Context.Result (inc1, acc, deserialize_idx) = BR.serialize ~size:offset_size
          ctxt t_idx ctxu u_idx (true,acc) in
        Codex_log.debug ~level:2 "idx inclusion: %B" inc1;
        let BR.Context.Result (inc2, acc, deserialize_fe) = BR.serialize ~size:offset_size
          ctxt t_fe ctxu u_fe (true,acc) in
        Codex_log.debug ~level:2 "fe inclusion: %B" inc2;
        let included' = inc0 && inc1 && inc2 in
        Codex_log.debug ~level:2 "inclusion is %B" included';
        let included = included && included' in
        BR.Context.Result (included, acc, fun ctx output ->
          let new_fe, output = deserialize_fe ctx output in
          let new_index, output = deserialize_idx ctx output in
          let new_vi = fresh_symbol ()
          and new_vf = fresh_symbol () in
          add_symbol ctx new_vi offset_size new_index  Ctypes.int ;
          add_symbol ctx new_vf offset_size new_fe Ctypes.int ;
          let ret = Some Ctypes.(
            {descr = Ptr {pointed=pt; index = Idx {idx=new_vi; fe=new_vf; ofs=t_ofs}}; pred=new_pred}) in
          (* let ret = Some (PtrT{ptyp=pt; ppred=new_pred; idx=Values (new_index, new_fe); ofs=t_ofs}) in *)
          (* Codex_log.debug ~level:2 "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret; *)
          ret, output)
    (* Here we ignore the indices (should be zero) *)
    | {descr = Ptr {pointed=pointed_t; index = Idx {idx=idx_t; fe=fe_t; ofs=t_ofs}}; pred=pred_t},
      {descr = Ptr {pointed=pointed_u; index = Idx {idx=idx_u; fe=fe_u; ofs=u_ofs}}; pred=pred_u} ->
      let idx_t = get_simple_index ctxt idx_t and fe_t = get_simple_index ctxt fe_t
      and idx_u = get_simple_index ctxu idx_u and fe_u = get_simple_index ctxu fe_u in
      begin try
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
              let zero = index_zero ctx
              and minus_one = index_minus_one ctx in
              let new_vi = fresh_symbol ()
              and new_vf = fresh_symbol () in
              add_symbol ctx new_vi offset_size zero Ctypes.int ;
              add_symbol ctx new_vf offset_size minus_one Ctypes.int ;
              (* let res = Some (PtrT{ptyp=t; ppred=new_pred;
                                   idx = Values (index_zero ctx, index_minus_one ctx); ofs=o}) in *)
              let res = Some Ctypes.({
                descr = Ptr {pointed=t; index = Idx {idx = new_vi; fe = new_vf; ofs=o}};
                pred = new_pred
              }) in
              Codex_log.debug ~level:2 "inclusion is %B" included;
              Codex_log.debug ~level:2 "serialize no. %d returns %a" count (pp_opt (pp ctx)) res;
              res,output
            in
            BR.Context.Result (included, acc, d)
        | None, _ ->
            Codex_log.debug ~level:2 "serialize no. %d: inclusion is false" count;
            Codex_log.debug ~level:2 "serialize no. %d returns Top" count;
            BR.Context.Result (false, acc, fun ctx output -> None, output)
        )
      with Type_error s ->
        Codex_log.alarm "serialize";
        Codex_log.error "Type error during serialize no. %d: %s" count s;
        Codex_log.debug ~level:2 "serialize no. %d: inclusion is false" count;
        BR.Context.Result (false, acc, fun ctx output -> None, output)
      end
      

    | {descr = Ptr _; _}, _ when 8 * Ctypes.sizeof u = Codex_config.ptr_size () ->
        assert false

    | _, {descr = Ptr _; _} when 8 * Ctypes.sizeof t = Codex_config.ptr_size () ->
        assert false

    | _ ->
      if Ctypes.equal ~only_descr:false t u then
        BR.Context.Result (included, acc, fun ctx output -> Some t, output)
      else (
        let size_t = Ctypes.sizeof t and size_u = Ctypes.sizeof u in
        if size_t = size_u then
          BR.Context.Result (included, acc, fun ctx output -> Some Ctypes.(word ~byte_size:size_t), output)
        else (
          Codex_log.alarm "serialize";
          Codex_log.error "Type error during serialize no. %d" count;
          Codex_log.debug ~level:2 "serialize no. %d: inclusion is false" count;
          BR.Context.Result (false, acc, fun ctx output -> None, output)
        )
      )

  let serialize_with_bottom_left ctxdummy ctxb t
      (included,acc) =
    match t.Ctypes.descr with
    | Ptr {pointed; index = Idx {idx;fe;ofs}} ->
      let idx = get_simple_index ctxb idx and fe = get_simple_index ctxb fe in
      incr tmp_count;
      let count = !tmp_count in
      Codex_log.debug ~level:2 "serialize no. %d (serialize_with_bottom_left): %a" count (pp ctxb) t;
      Codex_log.debug ~level:2 "serialize no. %d: initial included = %b" count included;
      let dummy_idx = BR.binary_empty ~size:array_index_size ctxdummy in
      let BR.Context.Result(inc,acc,deserialize_idx) = BR.serialize
        ~size:array_index_size ctxdummy dummy_idx ctxb idx (included,acc) in
      let dummy_fe = BR.binary_empty ~size:array_index_size ctxdummy in
      let BR.Context.Result(inc,acc,deserialize_fe) = BR.serialize
        ~size:array_index_size ctxdummy dummy_fe ctxb fe (included,acc) in
      BR.Context.Result (inc, acc, fun ctx output ->
        let fe, output = deserialize_fe ctx output in
        let idx, output = deserialize_idx ctx output in
        let new_vi = fresh_symbol ()
        and new_vf = fresh_symbol () in
        add_symbol ctx new_vi offset_size idx Ctypes.int ;
        add_symbol ctx new_vf offset_size fe Ctypes.int ;
        {t with descr = Ptr {pointed; index = Idx {idx = new_vi; fe = new_vf;ofs}}}, output
        (* PtrT{ptyp;idx = Values (idx, fe);ofs;ppred}, output *)
      )
    | _ -> BR.Context.Result (included, acc, fun ctx output -> t, output)

  let serialize_with_bottom_right ctxa t ctxdummy
      (included,acc) =
    match t.Ctypes.descr with
    | Ptr {pointed; index = Idx {idx;fe;ofs}} -> 
      let idx = get_simple_index ctxa idx and fe = get_simple_index ctxa fe in
      incr tmp_count;
      let count = !tmp_count in
      Codex_log.debug ~level:2 "serialize no. %d (serialize_with_bottom_right): %a" count (pp ctxa) t;
      Codex_log.debug ~level:2 "serialize no. %d: initial included = %b" count included;
      let dummy_idx = BR.binary_empty ~size:array_index_size ctxdummy in
      let BR.Context.Result(inc,acc,deserialize_idx) = BR.serialize
        ~size:array_index_size ctxa idx ctxdummy dummy_idx (included,acc) in
      let dummy_fe = BR.binary_empty ~size:array_index_size ctxdummy in
      let BR.Context.Result(inc,acc,deserialize_fe) = BR.serialize
        ~size:array_index_size ctxa fe ctxdummy dummy_fe (included,acc) in
      BR.Context.Result (inc, acc, fun ctx output ->
        let fe, output = deserialize_fe ctx output in
        let idx, output = deserialize_idx ctx output in
        (* PtrT{ptyp;idx = Values (idx, fe);ofs;ppred}, output *)
        let new_vi = fresh_symbol ()
        and new_vf = fresh_symbol () in
        add_symbol ctx new_vi offset_size idx Ctypes.int ;
        add_symbol ctx new_vf offset_size fe Ctypes.int ;
        {t with descr = Ptr {pointed; index = Idx {idx = new_vi; fe = new_vf;ofs}}}, output
      )
    | _ -> BR.Context.Result (included, acc, fun ctx output -> t, output)

  let join ctx t u =
    let BR.Context.Result (_, acc, deserialize) =
      serialize ctx t ctx u (true, BR.Context.empty_tuple) in
    let out = BR.nondet_same_context ctx acc in
    fst @@ deserialize ctx out

  let break_into_scalars (typs : Ctypes.typ list) : (int * Ctypes.typ) list =
    let open Ctypes in
    let rec aux (t : typ) : (int * typ) list = match (inlined t).descr with
    | Base _ | Ptr _ | Enum _ -> [(Ctypes.sizeof t, t)]
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

    | Name name -> failwith ("Use of a non-inlined named type " ^ name)
    | Application _ -> failwith ("Use of a non-applied constructor type")
    | Existential _ -> [(Ctypes.sizeof t, t)]
    | Union _ -> [(Ctypes.sizeof t, t)]

    in
    List.concat @@ List.map aux typs

  (** Inspect composite types (including nested ones) to retrieve a sequence of
      member types of total byte size [size] at offset [ofs] in [t]. Raises
      {!Not_found} if no such sequence exists. *)
  let rec members_at_offset_of_size ~ofs ~size t =
    Codex_log.debug ~level:2 "members_at_offset_of_size ~size:%d \
      ~ofs:%d %a" size ofs Ctypes.pp t;
    let open Ctypes in
    match (inlined t).descr with
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
    | Structure{st_byte_size = None;_} ->
        raise (Invalid_argument "members_ats_offset_of_size: Struct without a size")

    | Name name -> failwith ("Use of a non-inlined named type " ^ name)
    | Application _ -> failwith ("Use of a non-applied constructor type")

    | Existential _ ->
        if ofs = 0 && size = sizeof t then [t] else failwith ("Use of a partionned existential type")

    | Union _ ->
        if ofs = 0 && size = sizeof t then [t] else failwith ("Use of an ambiguous union type")

  (** Like {!members_at_offset_of_size} but also takes an index and an [fe] and
      does array bound checking, if need be. Returns optionally an alarm
      message if out of the array bounds. *)
  let members_at_offset_of_size_complex ~size ctx t ~idx ~fe ~ofs =
    Codex_log.debug ~level:2 "members_at_offset_of_size_complex ~size:%d ~idx:(...) \
      ~fe:(...) ~ofs:%d %a" size ofs Ctypes.pp t;
    let open Ctypes in
    let t = inlined t in
    match t.descr with
    | Array (_, None) ->
        raise (Invalid_argument "members_at_offset_of_size_complex: Array without a size")
    | Array (_, Some _) ->
        let elem_t, offset_in_elem, alarm = type_at_offset_complex ctx t idx fe ofs in
        let elem_t_size = sizeof elem_t in
        let elem_t =
          if size > elem_t_size then (
            assert (size mod elem_t_size = 0) ;
            let new_size = Z.of_int (size / elem_t_size) in
            {descr = Array (elem_t, Some (Const new_size)); pred = t.pred})
          else elem_t in 
        (* TODO : check if outside array bounds, if so raise an alarm *)
        members_at_offset_of_size ~ofs:offset_in_elem ~size elem_t, alarm
    | Void | Base _
    | Structure _ | Ptr _ | Enum _ | Function (_, _) ->
        members_at_offset_of_size ~ofs ~size t, None

    | Name name -> failwith ("Use of a non-inlined named type " ^ name)
    | Application _ -> failwith ("Use of a non-applied constructor type")

    | Existential _ -> members_at_offset_of_size ~ofs ~size t, None

    | Union _ -> members_at_offset_of_size ~ofs ~size t, None (* failwith ("Use of an ambiguous union type") *)

  type dereferenced_value = Ctypes.typ

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
    let rec weaken_until_ptr (t : Ctypes.typ) : dereferenced_value =
      let t = Ctypes.inlined t in
      match t.descr with
      | Ctypes.(Ptr{pointed; index= Idx _}) -> (* when it is already initialized *)
        Codex_log.debug ~level:1 "Dereferenced type: %a" (pp ctx) t;
        t
      | Ctypes.(Ptr {pointed = {descr=Array (_,ar_size);_} as pointed; index}) ->
          (* Here we extract the index from the type into a
           * [BR.binary]. We "focus" it, so to say *)
          let idx, fe = focus_idx ctx ar_size index in
          let new_vi = fresh_symbol () in
          let new_vf = fresh_symbol () in
          add_symbol ctx new_vi offset_size idx Ctypes.int ;
          add_symbol ctx new_vf offset_size fe Ctypes.int ;
          let res = {t with descr = Ptr{pointed; index = Idx {idx = new_vi; fe = new_vf;ofs = 0}}} in
          (* let res = PtrT{ptyp=pointed; idx = Values (idx, fe); ofs = 0; ppred = t.Ctypes.pred} in *)
          Codex_log.debug ~level:1 "Dereferenced type: %a" (pp ctx) res;
          res
      | Ctypes.(Ptr{pointed;index=_}) ->
          let idx = index_zero ctx in
          let fe = index_minus_one ctx in
          let new_vi = fresh_symbol () in
          let new_vf = fresh_symbol () in
          add_symbol ctx new_vi offset_size idx Ctypes.int ;
          add_symbol ctx new_vf offset_size fe Ctypes.int ;
          let res = {t with descr = Ptr {pointed; index = Idx {idx = new_vi; fe = new_vf;ofs = 0}}} in
          (* let res = PtrT{ptyp=pointed;idx = Values (idx, fe); ofs = 0; ppred = t.Ctypes.pred} in *)
          Codex_log.debug ~level:1 "Dereferenced type: %a" (pp ctx) res;
          res
      | _ ->
        begin try
          let new_t, new_ofs = type_at_offset t 0 in
          assert (new_ofs = 0);
          weaken_until_ptr new_t
        with Exit ->
          (* Apply potential invariants for this non-pointer type *)
          t
        end
    in
    let typs = break_into_scalars typs in
    List.map (fun (sz,t) -> sz, (weaken_until_ptr t)) typs


  let deref_to_ctypes ~size ctx typ = match typ.Ctypes.descr with
  | Ptr {pointed = pointed_t; index = Idx {idx; fe; ofs}} ->
      let idx = get_index ctx idx and fe = get_index ctx fe in
      Codex_log.debug ~level:1 "Dereferencing type %a" (pp ctx) typ;
      (* The algorithm goes:
         1. Find a sequence of members totalizing size [size] at offset [ofs]
            in the pointed structure, if possible
         2. Turn the into a {!dereferenced_value}.
      *)
      begin try
        let region_t, alarm = members_at_offset_of_size_complex ~size ctx
          pointed_t ~idx ~fe ~ofs in
        let () = match alarm with
        | Some msg ->
            Codex_log.alarm "array_offset_access";
            Codex_log.error "%s" msg
        | None -> () in
        region_t
      with Not_found ->
        raise @@ Type_error (Format.asprintf "No type of size %d is pointed by pointer %a" size (pp ctx) typ)
      end

  | Ptr {pointed = _; index = Zero} -> Codex_log.debug ~level:1 "Trying to dereference uninitialized pointer type %a" (pp ctx) typ; assert false
  | _ -> Codex_log.debug ~level:1 "Trying to dereference type %a" (pp ctx) typ; assert false
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

  let subtype ctx t u =
    (* Careful to the order of t and u: [included] is true iff u includes t. *)
    let BR.Context.Result (included,_,_) =
      serialize ctx u ctx t (true, BR.Context.empty_tuple) in
    included

  let may_subtype ctx t u =
    let open Ctypes in
    let zero = index_zero ctx in
    let exists_valid_index ctx idx fe =
      (* Is there at least one valid index in the concretization? *)
      let cond = BR.Binary_Forward.(BR.Boolean_Forward.(&&) ctx
        (bisle ctx ~size:offset_size zero idx)
        (BR.Boolean_Forward.not ctx @@ bisle ~size:offset_size ctx zero fe)
      ) in
      BR.query_boolean ctx cond
    in
    match t,u with
    | {descr = Ptr {pointed={descr = Array (elem_t, _);_}; index = Idx {idx=idx_t; fe=fe_t; ofs=ofs_t}}; pred=ppred_t},
      {descr = Ptr {pointed={descr = Array (elem_u, _);_}; index = Idx {idx=idx_u; fe=fe_u; ofs=ofs_u}}; pred=ppred_u} ->
        let idx_t = get_simple_index ctx idx_t and fe_t = get_simple_index ctx fe_t
        and idx_u = get_simple_index ctx idx_u and fe_u = get_simple_index ctx fe_u in
        (* Return true if there may be at least one valid index in each
         * concretization and the subtyping between pointers to elements
         * holds. *)
        let ret = match exists_valid_index ctx idx_t fe_t with
        | Lattices.Quadrivalent.False -> false
        | _ ->
            let ret2 = match exists_valid_index ctx idx_u fe_u with
            | Lattices.Quadrivalent.False -> false
            | _ ->
                (* NOTE: we lose an eventual predicate on the array. *)
                let minus_one = index_minus_one ctx in
                let new_vi = fresh_symbol ()
                and new_vf = fresh_symbol () in
                add_symbol ctx new_vi offset_size zero Ctypes.int ;
                add_symbol ctx new_vf offset_size minus_one Ctypes.int ;
                let new_t = {
                  descr = Ptr {pointed=elem_t; index = Idx {idx=new_vi; fe=new_vf; ofs = ofs_t}};
                  pred = ppred_t} in
                let new_u = {
                  descr = Ptr {pointed=elem_u; index = Idx {idx=new_vi; fe=new_vf; ofs = ofs_u}};
                  pred = ppred_u} in
                subtype ctx new_t new_u
            in ret2
        in ret
    | {descr = Ptr {pointed={descr=Array (elem_typ, _);_}; index = Idx {idx; fe; ofs}}; pred = ppred},
      {descr = Ptr {pointed=_; index = Idx {idx=_;fe=_;ofs=_}}; pred=_} ->
        let idx = get_simple_index ctx idx and fe = get_simple_index ctx fe in
        let ret = match exists_valid_index ctx idx fe with
        | Lattices.Quadrivalent.False -> false
        | _ ->
            (* There may be at least one valid index. *)
            let minus_one = index_minus_one ctx in
            let new_vi = fresh_symbol ()
            and new_vf = fresh_symbol () in
            add_symbol ctx new_vi offset_size zero Ctypes.int ;
            add_symbol ctx new_vf offset_size minus_one Ctypes.int ;
            let new_t = {
              descr = Ptr {pointed=elem_typ; index = Idx {idx=new_vi; fe=new_vf; ofs}};
              pred = ppred}  in
            subtype ctx new_t u
        in
        ret
    | {descr = Ptr {pointed=_; index=Idx {idx=_;fe=_;ofs=_}}; pred=_},
      {descr = Ptr {pointed={descr=Array (elem_typ, _);_}; index = Idx {idx; fe; ofs}}; pred = ppred} ->
        (* Is there at least one valid index in the concretization? *)
        let idx = get_simple_index ctx idx and fe = get_simple_index ctx fe in
        let ret = match exists_valid_index ctx idx fe with
        | Lattices.Quadrivalent.False -> false
        | _ ->
            (* There may be at least one valid index. *)
            let minus_one = index_minus_one ctx in
            let new_vi = fresh_symbol ()
            and new_vf = fresh_symbol () in
            add_symbol ctx new_vi offset_size zero Ctypes.int ;
            add_symbol ctx new_vf offset_size minus_one Ctypes.int ;
            let new_u = {
              descr = Ptr {pointed=elem_typ; index = Idx {idx=new_vi; fe=new_vf; ofs}};
              pred = ppred} in
            subtype ctx t new_u
        in
        ret
    | _ ->
        (* Both concretizations are singletons, so this case boils down to
         * the concrete operator (and {!subtype} acts as the concrete
         * operator on non-array types. *)
        subtype ctx t u

end

module EvalPred = struct

  module type Arg = sig
    include Domain_sig.Base
    val symbol : Context.t -> string -> binary
  end

  module type Sig = sig
    module Domain : Domain_sig.Base
    val check_invariant : size:int -> Domain.Context.t -> Ctypes.Pred.t
      -> Domain.binary -> bool
    val use_invariant : size:int -> Domain.Context.t -> Ctypes.Pred.t
      -> Domain.binary -> Domain.Context.t option      
  end

  module Make (A : Arg) = struct

    open Ctypes.Pred
    open A.Binary_Forward

    let binary_of_value ~size ctx v =
      let open Ctypes in
      match v with
      | Const x -> A.Binary_Forward.biconst ~size x ctx
      | Sym s -> A.symbol ctx s

    let cond_of_cmp ~size ctx cmpop v1 v2 =
      match cmpop with
      | Equal ->
          beq ~size ctx v1 v2
      | NotEqual ->
          A.Boolean_Forward.not ctx @@ beq ~size ctx v1 v2
      | ULt ->
          A.Boolean_Forward.not ctx @@ biule ~size ctx v2 v1
      | SLt ->
          A.Boolean_Forward.not ctx @@ bisle ~size ctx v2 v1
      | ULeq ->
          biule ~size ctx v1 v2
      | SLeq ->
          bisle ~size ctx v1 v2
      | UGeq ->
          biule ~size ctx v2 v1
      | SGeq ->
          bisle ~size ctx v2 v1
      | UGt ->
          A.Boolean_Forward.not ctx @@ biule ~size ctx v1 v2
      | SGt ->
          A.Boolean_Forward.not ctx @@ bisle ~size ctx v1 v2

    let lift_unop ~size ctx op =
      match op with
      | Extract (index,len) -> fun x ->
        bextract ~size:len ~index ~oldsize:size ctx x
        |> buext ~size ~oldsize:len ctx

    let lift_binop ~size ctx op =
      match op with
      | Add -> biadd ~size ctx ~nsw:false ~nuw:false ~nusw:false
      | Sub -> bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false
      | Mul -> bimul ~size ctx ~nsw:false ~nuw:false
      | And -> band ~size ctx
      | Or -> bor ~size ctx
      | Concat (size1,size2) -> bconcat ~size1 ~size2 ctx

    let rec binary_of_expr ~size ctx ~self e =
      match e with
      | Self -> self
      | Val v -> binary_of_value ~size ctx v
      | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx ~self e)
      | Binop (op, e1, e2) -> lift_binop ~size ctx op
          (binary_of_expr ~size ctx ~self e1) (binary_of_expr ~size ctx ~self e2)

    let rec cond_of_pred ~size ctx pred ~self =
      match pred with
      | True -> A.Boolean_Forward.true_ ctx
      | Cmp (op, e1, e2) ->
          cond_of_cmp ~size ctx op (binary_of_expr ~size ctx ~self e1)
          (binary_of_expr ~size ctx ~self e2)
      | And (p,q) ->
          let c1 = cond_of_pred ~size ctx p ~self in
          let c2 = cond_of_pred ~size ctx q ~self in
          A.Boolean_Forward.(&&) ctx c1 c2

    let check_invariant ~size ctx pred v =
      let c = cond_of_pred ~size ctx pred ~self:v in
      let is_true = A.query_boolean ctx c in
      match is_true with
      | Lattices.Quadrivalent.True -> true
      | _ -> false (* Possibly false *)

    let use_invariant ~size ctx pred v =
      match pred with
      | True -> Some ctx
      | _ -> A.assume ctx (cond_of_pred ~size ctx pred ~self:v)
  end

end
