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

module Log = Tracelog.Make(struct let category = "Typed_address" end);;
module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree
module In_bits = Units.In_bits
module In_bytes = Units.In_bytes

let option_merge merge left right =
  match left, right with
  | None, None -> None
  | Some v, None
    | None, Some v -> Some v
  | Some l, Some r -> Some (merge l r)

let option_map2 merge left right =
  match left, right with
  | Some l, Some r -> Some (merge l r)
  | _ -> None

module MakeType
    (Scalar: Sig.BASE)
    (Symb: Memory_sig.FIXED_SIZE_VALUE_DOMAIN
     with module Scalar = Scalar
      and module Context = Scalar.Context) : sig

  (** Representation of a symbolic pointer to ptyp. *)
  (* TODO: We want to represent a pointer to ptyp, which may be
     pointing to a single element or an array of element (the later
     being possible only if ptyp is constant). If ptyp is constant,
     then ofs could represent an "offset in ptyp".

     This means that we should have a better abstraction for what an
     "offset in a type" is (either an offset in an array, or an offset
     in another type); and the type of the offset should match the
     type of the pointed typ.

     TODO: In the theory, pointers can only be pointer to named types,
     so probably we should simplify here so that ptyp can only be a
     named type, possibly with some instantiated parameters. *)
  type t = {
    ptyp : TypedC.typ;
    idx : Scalar.binary;
    elem_size : int;
    ofs : Scalar.binary;
  }

  (* We want [module Context = Scalar.Context], but it does not work. *)
  module Context: sig
    type t = Scalar.Context.t
    type 'a in_tuple = 'a Scalar.Context.in_tuple
    type 'a out_tuple = 'a Scalar.Context.out_tuple
    type ('a, 'b) result =
      ('a, 'b) Scalar.Context.result =
      Result : bool * 'some in_tuple *
      (t -> 'some out_tuple -> 'a * 'b out_tuple) -> ('a, 'b)
      result
  end

  (** pretty-printer. *)
  val pp: Context.t -> Format.formatter -> t -> unit

  (** Serialization; see {!Sig.BASE}. *)
  val serialize :
    widens:bool ->
    Context.t ->
    t ->
    Context.t ->
    t -> bool * 'a Context.in_tuple -> (t option, 'a) Context.result

  (** Serialization in the case where the left part is not a typed address. *)
  val serialize_with_bottom_left :
    widens:bool ->
    Context.t ->
    Context.t ->
    t -> bool * 'a Context.in_tuple -> (t, 'a) Context.result

  (** Serialization in the case where the right part is not a typed address. *)
  val serialize_with_bottom_right :
    widens:bool ->
    Context.t ->
    Context.t ->
    t -> bool * 'a Context.in_tuple -> (t, 'a) Context.result

  (** [subtype a b] returns true if we can prove that a is a subtype of b. *)
  val subtype: Context.t -> t -> t -> bool

  (** Returns a boolean which is true if the address is in bounds, i.e. points to a value. *)
  val in_bounds: Context.t -> t -> Scalar.boolean

  (** Like [in_bounds] but perform a query on the resulting boolean.
      TODO: Suppress, not needed, we can do the query above. *)
  val is_in_bounds: Context.t -> t -> bool

  (** Pointer addition/substraction on an address. *)
  val add_offset: Context.t -> Scalar.binary -> t -> t


  exception Type_error of string

  (** Given a type which is a structure with flexible array member,
      returns a triple [(offset,elt_size,len)] where:
      - [offset] is the offset of the flexible array member in the structure;
      - [elt_size] is the size of each element in the array
      - [len] is the length of the array. *)
  val to_flexible: TypedC.typ -> In_bytes.t * In_bytes.t * TypedC.array_length

  (** Check if a Scalar.binary is zero. TODO: This code does not
      depend on pointers at all, we can suppress it from the
      interface. *)
  val index_is_zero: Context.t -> Scalar.binary -> bool
end
= struct

  module Context = Scalar.Context

  (* If ptyp = Array (tp,len) then idx ∈ [0,len[, elem_size = size of tp and ofs ∈ [0,elem_size[
   * If ptyp is a flexible array member of the form struct { t1 ; t2 = Array (tp,len) } then idx ∈ [0,len], elem_size = size of tp and ofs ∈ [0, sz + elem_size[
   * Otherwise idx = 0, elem_size = 1 and ofs ∈ [0, size of ptyp]
   *
   * total_offset = (idx * elem_size) + ofs
   *)
  type t = {
    ptyp : TypedC.typ;
    idx : Scalar.binary;
    elem_size : int;
    ofs : Scalar.binary;
  }

  exception Type_error of string

  let lift_unop ~size ctx op =
    let open TypedC.Pred in
    let open Scalar.Binary_Forward in
    match op with
    | Extract (index,len) -> fun x ->
      bextract ~size:len ~index ~oldsize:size ctx x
      |> buext ~size ~oldsize:len ctx

  let lift_binop ~size ctx op =
    let open TypedC.Pred in
    let open Scalar.Binary_Forward in
    match op with
    | Add -> biadd ~size ctx ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
    | Sub -> bisub ~size ctx ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false)
    | Mul -> bimul ~size ctx ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false)
    | Div -> bisdiv ~size ctx
    | And -> band ~size ctx
    | Concat (size1,size2) -> bconcat ~size1 ~size2 ctx
    | Or -> bor ~size ctx
    | Mod -> bismod ~size ctx

  let cond_of_cmp ~size ctx cmpop v1 v2 =
    let open TypedC.Pred in
    match cmpop with
    | Equal ->
      Scalar.Binary_Forward.beq ~size ctx v1 v2
    | NotEqual ->
      Scalar.Boolean_Forward.not ctx @@ Scalar.Binary_Forward.beq ~size ctx v1 v2
    | ULt ->
      Scalar.Boolean_Forward.not ctx @@ Scalar.Binary_Forward.biule ~size ctx v2 v1
    | SLt ->
      Scalar.Boolean_Forward.not ctx @@ Scalar.Binary_Forward.bisle ~size ctx v2 v1
    | ULeq ->
      Scalar.Binary_Forward.biule ~size ctx v1 v2
    | SLeq ->
      Scalar.Binary_Forward.bisle ~size ctx v1 v2
    | UGeq ->
      Scalar.Binary_Forward.biule ~size ctx v2 v1
    | SGeq ->
      Scalar.Binary_Forward.bisle ~size ctx v2 v1
    | UGt ->
      Scalar.Boolean_Forward.not ctx @@ Scalar.Binary_Forward.biule ~size ctx v1 v2
    | SGt ->
      Scalar.Boolean_Forward.not ctx @@ Scalar.Binary_Forward.bisle ~size ctx v1 v2

  let symbol ctx s =
    let sz,symb = Symb.global_symbol ctx s in
    sz, Symb.binary2scalar_binary ~size:sz ctx symb

  let rec binary_of_value ~size ctx v =
    let open TypedC in
    match v with
    | Sym s ->
      let sz,v = symbol ctx s in
      assert (sz = size);
      v

  and binary_of_expr ~size ctx ~self e =
    let open TypedC.Pred in
    match e with
    | Const x -> Scalar.Binary_Forward.biconst ~size x ctx
    | Self -> self
    | Val v -> binary_of_value ~size ctx v
    | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx ~self e)
    | Binop (op, e1, e2) -> lift_binop ~size ctx op
      (binary_of_expr ~size ctx ~self e1) (binary_of_expr ~size ctx ~self e2)

  and cond_of_pred_subdomain ~size ctx pred v =
    let open TypedC.Pred in
    match pred with
    | True -> Scalar.Boolean_Forward.true_ ctx
    | Cmp (op, e1, e2) ->
      cond_of_cmp ~size ctx op (binary_of_expr ~size ctx ~self:v e1) (binary_of_expr ~size ctx ~self:v e2)
    | And (p,q) ->
      let c1 = cond_of_pred_subdomain ~size ctx p v in
      let c2 = cond_of_pred_subdomain ~size ctx q v in
      Scalar.Boolean_Forward.(&&) ctx c1 c2
    | Mutval (mut,p) -> cond_of_pred_subdomain ~size ctx (evaluate_mutval mut p) v

  and check_invariant_subdomain ~size ctx pred v =
    let c = cond_of_pred_subdomain ~size ctx pred v in
    let is_true = Scalar.query_boolean ctx c in
    match is_true with
    | Lattices.Quadrivalent.True -> true
    | _ -> false (* Possibly false *)

  let rec pp_expr ctx fmt (expr : TypedC.Pred.expr) =
    let open TypedC.Pred in
    match expr with
    | Val (Sym s) ->
      let size,value = Symb.global_symbol ctx s in
      let v = Symb.binary2scalar_binary ~size ctx value in
      Format.fprintf fmt "@[<hov 2>%a@]" (Scalar.binary_pretty ~size ctx) v
    | Const c -> Format.fprintf fmt "@[<hov 2>%a@]" Z.pp_print c
    | Self -> Format.fprintf fmt "@[<hov 2>Self@]"
    | Unop (op, e) -> assert false
    | Binop (op, e1, e2) ->
      Format.fprintf fmt "@[<hov 2>%a %a %a@]"
        (pp_expr ctx) e1
        pp_binop op
        (pp_expr ctx) e2

  let pp_pointed_type ctx fmt ptyp =
    let ptrsize = Codex_config.ptr_size () in
    let open TypedC in
    match ptyp.descr with
    | Array (typ, Variable_length s) ->
      let sz = snd @@ symbol ctx s in
      Format.fprintf fmt "@[<hov 2>%a[%a]@]"
        TypedC.pp typ (Scalar.binary_pretty ~size:ptrsize ctx) sz
    | Application {constr;args=[]}  -> Format.fprintf fmt "%a" TypedC.Constr.pp constr
    (*
    | Application (constr, exprs) ->
      (* let bottom = Scalar.binary_empty ~size:32 ctx in
      let args = List.map (fun e -> binary_of_expr ~size:32 ctx ~self:bottom e) exprs in *)
      Format.fprintf fmt "@[<hov 2>%a(%a)@]"
        TypedC.pp_constr constr
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") (pp_expr ctx)) exprs
    *)
    | _ -> Format.fprintf fmt "(%a)" TypedC.pp ptyp


  let pp ctx fmt {ptyp;idx;elem_size;ofs} =
    let ptrsize = Codex_config.ptr_size () in

    let idx_pretty fmt idx =
      match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptrsize @@ binary ~size:ptrsize ctx ofs) with
      | Some o when Z.(equal o zero) -> ()
      | _ -> Format.fprintf fmt "[%a]" (Scalar.binary_pretty ~size:ptrsize ctx) idx
    in

    let ofs_pretty fmt ofs =
      let ofs_sng = Scalar.Query.(Binary_Lattice.is_singleton ~size:ptrsize @@ binary ~size:ptrsize ctx ofs) in
      match ofs_sng with
      | Some o when Z.(equal o zero) -> ()
      | Some o -> Format.fprintf fmt ".%a" Z.pp_print o
      | None -> Format.fprintf fmt ".%a" (Scalar.binary_pretty ~size:ptrsize ctx) ofs
    in
    Format.fprintf fmt "@[<hov 2>%a%a%a*@]"
      (* TypedC.pp ptyp *)
      (pp_pointed_type ctx) ptyp
      idx_pretty idx
      ofs_pretty ofs

  let index_zero ctx =
    let ptrsize = Codex_config.ptr_size () in
    Scalar.Binary_Forward.biconst ~size:ptrsize Z.zero ctx

  let index_is_zero ctx index =
    let ptrsize = Codex_config.ptr_size () in
    match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptrsize @@ binary ~size:ptrsize ctx index) with
    | Some idx -> Z.equal Z.zero idx
    | None -> false

  let rec to_flexible typ =
    let open TypedC in
    let typ = inlined typ in
    match typ.descr with
    | Array (tp, len) -> (In_bytes.zero, sizeof tp, len)
    | StructureFAM {structure;array} ->
      let ofs = TypedC.sizeof structure in
      let typ = array in
      let o,sz,len = to_flexible typ in
      In_bytes.(o + ofs, sz, len)

    | _ -> assert false


  (** Returns the type at a given offset of a non-scalar type.
      @param binary zero a [Scalar.binary] of value zero and of size
      [Type.offset_size].
      @return a triple (subtyp, idx, ofs) such that [offset] is in [subtyp] at
      index [idx] (if [subtyp] is an array) and offset [ofs].
  *)
  let type_at_offset typ offset : TypedC.typ * int =
    Log.trace (fun p -> p "type_at_offset.@ typ =@ %a.@ offset =@ %d"
      TypedC.pp typ
      offset) @@ fun () ->
    let open TypedC in
    let typ = inlined typ in
    match typ.descr with
    | Structure st ->
      begin
        let members =
          List.sort_uniq (fun (x,_,_) (y,_,_) -> Stdlib.compare x y) st.st_members in
        let struct_size = st.st_byte_size in
        let rec loop cur_field = function
          | ((ofs:Units.In_bytes.t),_,_) as field :: fields when offset >= (ofs:>int) -> loop field fields
          | _ ->
            let (prec_ofs,fname,ftyp) = cur_field in
            if offset >= (struct_size:>int) then begin
              let msg = Format.asprintf "Offset %d not found in structure %a."
                offset TypedC.pp typ in
              raise (Type_error msg)
              end;
            Log.debug (fun p -> p "Access to field %s : %a (offset %d) of struct %a"
              fname
              TypedC.pp ftyp
              (prec_ofs:>int)
              TypedC.pp typ);
            ftyp, offset - (prec_ofs:>int)
        in
        let fst_field = try List.hd members with Failure _ ->
        raise (Invalid_argument "type_at_offset: empty structure") in
        loop fst_field (List.tl members)
        end
    | Array (elem_typ, Fixed_length nb_elem) ->
      let elem_sz = TypedC.sizeof elem_typ in
      let array_index = offset / (elem_sz:>int) in
      if not (array_index >= 0 && Z.lt (Z.of_int array_index) nb_elem) then begin
        Log.debug (fun p -> p "reaching alarm array_offset_access");
        Emit_alarm.emit_alarm Operator.Alarm.Array_offset_access;
        Codex_log.error "@[<hov 2>Out-of-bounds access at index %d in array of \
                         size %a.@]" array_index Z.pp_print nb_elem
        end else
        Log.debug (fun p -> p "@[<hov 2>Access at index %d in array of size %a@]"
          array_index Z.pp_print nb_elem);
      let offset_in_elem = offset mod (elem_sz:>int) in
      elem_typ,
      offset_in_elem
    | Array (_, Variable_length _) ->
      raise (Invalid_argument "type_at_offset: Unhandled array with symbolic size")
    | Application _ -> failwith ("Use of a non-applied constructor type")

    | _ when offset = 0 ->
      (* This is a scalar type and does not have any fields. *)
      raise Exit
    | _ ->
      let msg = Format.asprintf "@[<hov 2>Access in type %a \
                                 with non-zero offset %d is not allowed@]" TypedC.pp typ offset in
      raise (Type_error msg)

  (** Like {!type_at_offset} but also takes an index and an [fe] and does
      array bound checking, if need be. This will typically be needed for the
      first loss of precision on a pointer, but not for the following steps,
      because structures do not contain arrays of unknown size directly.
      Returns optionally an alarm message if out-of-array occurred. *)
  let type_at_offset_complex ctx typ idx offset : TypedC.typ * int * string option =
    let open TypedC in
    let ptrsize = Codex_config.ptr_size () in
    Log.debug (fun p -> p "type_at_offset_complex.@ typ =@ %a.@ idx =@ %a .@ offset =@ %d"
      TypedC.pp typ
      (Scalar.binary_pretty ~size:ptrsize ctx) idx
      offset
    );
    match (inlined typ).descr with
    | Array (elem_typ, nb_elem) ->
      let elem_sz = TypedC.sizeof elem_typ in
      let array_index_diff = offset / (elem_sz:>int) in
      let offset_in_elem = offset mod (elem_sz:>int) in
      (* Perform true euclidean division in case [offset] is negative. *)
      let array_index_diff, offset_in_elem =
        if offset < 0 then array_index_diff - 1, offset_in_elem + (elem_sz:>int)
        else array_index_diff, offset_in_elem in
      Log.debug (fun p -> p "array_index_diff = %d,@ offset_in_elem =@ %d" array_index_diff offset_in_elem);
      let array_index =
        Scalar.Binary_Forward.(biadd ~size:ptrsize ctx idx
                                 ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
        (biconst ~size:ptrsize (Z.of_int array_index_diff) ctx)) in

      let nb_elem_expr = TypedC.Pred.array_length_to_expr nb_elem in
      let lt_size =
        (* check_invariant_subdomain ~size:offset_size ctx TypedC.Pred.(slt (Const Z.zero)) new_fe *)
        (* [i <u s] implies that [i <s s], provided that [s >=s 0].
         * Here we assume [s >=s 0].
         * [i <u s] also implies [i <=s s], which is what we
         * need to verify. Incidentally, [i <u s] is the
         * condition that will be the easiest to verify with our
         * constraint domain and hypotheses on array sizes. *)
        (* XXX: factorize this by using [valid] here. *)
        check_invariant_subdomain ~size:ptrsize ctx TypedC.Pred.(Cmp(SLt, Self, nb_elem_expr)) array_index
      (* || check_invariant_subdomain ~size:offset_size ctx TypedC.Pred.(slt nb_elem) array_index *)
      in
      let geq_zero =
        check_invariant_subdomain ~size:ptrsize ctx TypedC.Pred.(Cmp(SGeq,Self,Const Z.zero)) array_index in

      let alarm =
        if not (geq_zero && lt_size) then begin
          let msg = Format.asprintf "@[<hov 2>Out-of-bounds access at index %a in array of \
                                     size %a.@]" (Scalar.binary_pretty ~size:ptrsize ctx) array_index
            (fun fmt n -> match n with
              | Fixed_length x -> Z.pp_print fmt x
              | Variable_length s -> Scalar.binary_pretty ~size:ptrsize ctx fmt (snd @@ symbol ctx s)) nb_elem in
          Some msg
          end else begin
          Log.debug (fun p -> p "@[<hov 2>Access at index %a in array of size %a@]"
            (Scalar.binary_pretty ~size:ptrsize ctx) array_index TypedC.pp_array_length nb_elem);
          None
          end
      in
      elem_typ,
      offset_in_elem,
      alarm
    | Application _ -> failwith ("Use of a non-applied constructor type")
    (* | Existential _ -> failwith ("Use of non-instantiated existential type") *)

    | _ ->
      let typ, ofs = type_at_offset typ offset in
      typ, ofs, None

  let add_offset' ctx ptyp idx elem_size ofs (offset : Scalar.binary) : TypedC.typ * Scalar.binary * int * Scalar.binary =
    let size = Codex_config.ptr_size () in
    Log.debug (fun p -> p "add_offset' %a idx:%a elem_size:%d ofs:%a %a"
      TypedC.pp ptyp (Scalar.binary_pretty ~size ctx) idx elem_size
      (Scalar.binary_pretty ~size ctx) ofs (Scalar.binary_pretty ~size ctx) offset);

    let open TypedC in
    match (inlined ptyp).descr with
    | Array (elem_t, len) ->
      let elem_sz = sizeof elem_t in

      (* assert (elem_size = elem_sz) ; *)
      let elem_sz_b = Scalar.Binary_Forward.biconst ~size (Z.of_int (elem_sz:>int)) ctx in

      let idx =
        if elem_size = (elem_sz:>int) then idx
        else
          (* Updates idx with a new elem_size if possible *)
          let index_rem = Scalar.Binary_Forward.biumod ~size ctx idx elem_sz_b in
          assert (index_is_zero ctx index_rem) ;
          Scalar.Binary_Forward.biudiv ~size ctx idx elem_sz_b
      in

      let delta = Scalar.Binary_Forward.biadd ~size ctx
          ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ofs offset in
      let index_diff = Scalar.Binary_Forward.bisdiv ~size ctx delta elem_sz_b in (* index_diff = (old_offset + offset + non_aligned) / size *)
      let new_offset = Scalar.Binary_Forward.bismod ~size ctx delta elem_sz_b in (* new_offset = (old_offset ° offset + non_aligned) mod size *)
      let new_offset_lattice = Scalar.Query.binary ~size ctx new_offset in

      let zero = Scalar.Binary_Forward.biconst ~size Z.zero ctx in

      let imprecise_add =
        match Scalar.Query.Binary_Lattice.is_singleton ~size new_offset_lattice with
        | None -> true
        | Some x -> false
      in

      (* If [new_offset] represents a negative integer (on 32 bits),
       * first compute that negative integer. Then, it means that [delta]
       * was negative, and we adjust the values to reflect a real
       * euclidean division. *)
      (* XXX: this code only works in a 64-bit OCaml runtime. *)
      let index_diff_zero = Scalar.Binary_Forward.beq ~size ctx
        index_diff zero |> Scalar.query_boolean ctx in
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
        ptyp, idx, (elem_sz:>int), new_offset
      else begin
        let new_index = Scalar.Binary_Forward.biadd ~size
            ~flags:(Operator.Flags.Biadd.pack ~nsw:true ~nuw:false ~nusw:false)
            ctx idx index_diff in
        let c1 = check_invariant_subdomain ~size ctx Pred.(Cmp(SGeq,Self,Const Z.zero)) new_index in
        (* Just past the end is allowed, so this is Leq and not Lt *)
        let c2 = match len with
          | s ->
            let s = TypedC.Pred.array_length_to_expr s in
            (* [i <u s] implies that [i <s s], provided that [s >=s 0].
              * Here we assume [s >=s 0].
              * [i <u s] also implies [i <=s s], which is what we
              * need to verify. Incidentally, [i <u s] is the
              * condition that will be the easiest to verify with our
              * constraint domain and hypotheses on array sizes. *)
            check_invariant_subdomain ~size ctx Pred.(Cmp(SLt,Self,s)) new_index
            || check_invariant_subdomain ~size ctx Pred.(Cmp(Equal,Self,s)) new_index

        in
        if c1 && c2 then Codex_log.warning "Array index %a possibly out of bounds." (Scalar.binary_pretty ctx ~size) new_index ;
        ptyp, new_index, (elem_sz:>int), new_offset
        end

    (* Case for flexible array members *)
    | StructureFAM _ ->
      begin
        let prefix_size, elem_sz, len = to_flexible ptyp in
        let elem_sz_b = Scalar.Binary_Forward.biconst ~size (Z.of_int (elem_sz:>int)) ctx in

        (* let zero = Scalar.Binary_Forward.biconst ~size Z.zero ctx in *)

        Log.debug (fun p -> p "in Typed_address.add_offset', in case flexible array members") ;

        let idx, ofs, imprecise_add =
          if elem_size = (elem_sz:>int) then idx, ofs, false
          else
            (* Updates idx with a new elem_size if necessary *)
            let remainder = Scalar.Binary_Forward.biumod ~size ctx idx elem_sz_b in
            match Scalar.Query.(Binary_Lattice.is_singleton ~size @@ binary ~size ctx remainder) with
            | None -> idx, offset, true
            | Some x ->
              Scalar.Binary_Forward.biudiv ~size ctx idx elem_sz_b,
              Scalar.Binary_Forward.(biadd ~size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx ofs @@ biconst ~size x ctx),
              false
        in

        let new_offset = Scalar.Binary_Forward.biadd ~size ctx
            ~flags:(Operator.Flags.Biadd.pack ~nsw:true ~nuw:false ~nusw:false) ofs offset in

        Log.debug (fun p -> p "in Typed_address.add_offset', new_offset = %a" (Scalar.binary_pretty ~size ctx) new_offset) ;

        let is_suffix_b = cond_of_pred_subdomain ~size ctx (TypedC.Pred.(Cmp(UGeq,Self,Const (Z.of_int (prefix_size:>int))))) new_offset in
        let is_suffix, imprecise_suffix =
          match Scalar.query_boolean ctx is_suffix_b with
          | Bottom | Top -> false, true
          | False -> false, false
          | True ->
            let is_preffix_b = cond_of_pred_subdomain ~size ctx (TypedC.Pred.(Cmp(ULeq,Self,Const (Z.of_int (prefix_size:>int))))) new_offset in
            begin match Scalar.query_boolean ctx is_preffix_b with
              | Bottom -> false, true (* should not happen *)
              | True -> false, false
              | False | Top -> true, false
              end
        in

        Log.debug (fun p -> p "in Typed_address.add_offset', is_suffix = %b" is_suffix) ;
        Log.debug (fun p -> p "in Typed_address.add_offset', imprecise_suffix = %b" imprecise_suffix) ;

        if imprecise_add || imprecise_suffix then ptyp, idx, elem_size, new_offset
        else if is_suffix then

          let prefix_size_b = Scalar.Binary_Forward.biconst ~size (Z.of_int (prefix_size:>int)) ctx in
          let delta = Scalar.Binary_Forward.bisub ~size ctx ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) new_offset prefix_size_b in

          let index_diff = Scalar.Binary_Forward.bisdiv ~size ctx delta elem_sz_b in
          let new_offset = Scalar.Binary_Forward.bismod ~size ctx delta elem_sz_b in
          let new_offset_lattice = Scalar.Query.binary ~size ctx new_offset in

          let zero = Scalar.Binary_Forward.biconst ~size Z.zero ctx in

          let imprecise_add =
            match Scalar.Query.Binary_Lattice.is_singleton ~size new_offset_lattice with
            | None -> true
            | Some x -> false
          in
          let index_diff_zero = Scalar.Binary_Forward.beq ~size ctx
            index_diff zero |> Scalar.query_boolean ctx in
          let index_diff_zero = match index_diff_zero with
            | Lattices.Quadrivalent.True -> true
            | _ -> false
          in

          let new_offset = Scalar.Binary_Forward.biadd ~size
              ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx new_offset prefix_size_b in
          if index_diff_zero || imprecise_add then
            ptyp, idx, (elem_sz:>int), new_offset
          else begin
            let new_index = Scalar.Binary_Forward.biadd ~size
                ~flags:(Operator.Flags.Biadd.pack ~nsw:true ~nuw:false ~nusw:false)
                ctx idx index_diff in
            let c1 = check_invariant_subdomain ~size ctx Pred.(Cmp(SGeq,Self,Const Z.zero)) new_index in
            (* Just past the end is allowed, so this is Leq and not Lt *)
            let c2 = match len with
              | s ->
                let s = TypedC.Pred.array_length_to_expr s in
                check_invariant_subdomain ~size ctx Pred.(Cmp(SLt,Self,s)) new_index
                || check_invariant_subdomain ~size ctx Pred.(Cmp(Equal,Self,s)) new_index
            in
            if c1 && c2 then Codex_log.warning "Array index %a possibly out of bounds." (Scalar.binary_pretty ctx ~size) new_index ;
            ptyp, new_index, (elem_sz:>int), new_offset
            end

        else
          let total_b =
            Scalar.Binary_Forward.(
            biadd ~size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx new_offset @@
            bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx idx elem_sz_b
            ) in
          let total_lattice = Scalar.Query.binary ~size ctx total_b in
          begin match Scalar.Query.Binary_Lattice.is_singleton ~size total_lattice with
            | None ->
              Codex_log.warning "Imprecise offset %a when added. Could be out of bounds" (Scalar.binary_pretty ~size ctx) offset ;
              let new_offset = Scalar.Binary_Forward.biadd ~size
                  ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx ofs offset in
              ptyp, idx, elem_size, new_offset

            | Some total ->
              let total = Z.signed_extract total 0 (size:>int) in
              let total_b = Scalar.Binary_Forward.biconst ~size total ctx in
              ptyp, idx, elem_size, total_b
            end

        end

    | stripped_pointed_t ->
      (* let typ_size = 8 * TypedC.sizeof typ in
      let typ_size_b = Scalar.Binary_Forward.biconst ~size (Z.of_int typ_size) ctx in *)
      let elem_size_b = Scalar.Binary_Forward.biconst ~size (Z.of_int elem_size) ctx in
      let total_b =
        Scalar.Binary_Forward.(
        biadd ~size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx ofs @@
        biadd ~size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx offset @@
        bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx idx elem_size_b
        ) in
      let total_lattice = Scalar.Query.binary ~size ctx total_b in
      begin match Scalar.Query.Binary_Lattice.is_singleton ~size total_lattice with
        | None ->
          Codex_log.warning "Imprecise offset %a when added. Could be out of bounds" (Scalar.binary_pretty ~size ctx) offset ;
          let new_offset = Scalar.Binary_Forward.biadd ~size
              ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx ofs offset in
          ptyp, idx, elem_size, new_offset

        | Some total ->
          (* let total = Z.to_int total in *)
          let total = (* Z.to_int @@ *) Z.signed_extract total 0 (size:>int) in
          let total_b = Scalar.Binary_Forward.biconst ~size total ctx in
          ptyp, idx, elem_size, total_b
        end


  let add_offset ctx incr {ptyp;idx;elem_size;ofs} =
    let ptyp,idx,elem_size,ofs = add_offset' ctx ptyp idx elem_size ofs incr in
    {ptyp;idx;elem_size;ofs}

  let add_offset ctx incr t =
    let ptrsize = Codex_config.ptr_size () in
    Log.trace (fun p -> p "add_offset %a %a" (Scalar.binary_pretty ~size:ptrsize ctx) incr (pp ctx) t)
    (*~pp_ret:(pp ctx)*)
    @@ fun () -> add_offset ctx incr t

  (* -------------------------- serialize operations -------------------------*)

  let make_boolean ctx b =
    if b then Scalar.Boolean_Forward.true_ ctx else Scalar.boolean_unknown ctx

  let in_bounds ctx {ptyp;idx;elem_size;ofs} =
    let open TypedC in
    let ptrsize = Codex_config.ptr_size () in
    let ptyp = inlined ptyp in
    Log.debug (fun p -> p "in Typed_address.in_bounds, ptyp = %a and elem_size = %d" pp ptyp elem_size) ;
    match ptyp.descr with
    | Array (tp, ar_size) ->
      let elem_sz = sizeof tp in
      assert(elem_size = (elem_sz:>int));
      let ar_size_expr = Pred.array_length_to_expr ar_size in
      Scalar.Boolean_Forward.(&&) ctx
        (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SGeq,Self,(Const Z.zero))) idx) @@
      Scalar.Boolean_Forward.(&&) ctx
        (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SLt,Self,ar_size_expr)) idx) @@
      Scalar.Boolean_Forward.(&&) ctx
        (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SGeq,Self,Const Z.zero)) ofs) @@
      (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SLt,Self,Const (Z.of_int (elem_sz:>int)))) ofs)
    | StructureFAM _  ->
      let offset, elem_sz, len = to_flexible ptyp in
      begin match len with
        | s ->
          Scalar.Boolean_Forward.(&&) ctx
            (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SGeq,Self,Const Z.zero)) idx) @@
          Scalar.Boolean_Forward.(&&) ctx
            (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SLt,Self,Pred.array_length_to_expr s)) idx) @@
          Scalar.Boolean_Forward.(&&) ctx
            (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SGeq,Self,Const Z.zero)) ofs) @@
          Scalar.Boolean_Forward.(&&) ctx
            (* TODO: isn't elem_size = elem_sz an invariant? *)
            (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SLeq,Self,Const (Z.of_int (offset:>int)))) ofs)
            (if elem_size = (elem_sz:>int) then Scalar.Boolean_Forward.true_ ctx else Scalar.boolean_unknown ctx)
        end

    | Base (size,_)
      | Structure {st_byte_size = size} ->
      Scalar.Boolean_Forward.(&&) ctx
        (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(Equal,Self,Const Z.zero)) idx) @@
      Scalar.Boolean_Forward.(&&) ctx
        (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SGeq,Self,Const Z.zero)) ofs) @@
      (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SLt,Self,Const (Z.of_int (size:>int)))) ofs)

    | Ptr _ ->
      let size = (Codex_config.ptr_size():>int) / 8 in
      Scalar.Boolean_Forward.(&&) ctx
        (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(Equal,Self,Const Z.zero)) idx) @@
      Scalar.Boolean_Forward.(&&) ctx
        (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SGeq,Self,Const Z.zero)) ofs) @@
      (cond_of_pred_subdomain ~size:ptrsize ctx Pred.(Cmp(SLt,Self,Const (Z.of_int size))) ofs)

    | _ -> Scalar.Boolean_Forward.true_ ctx

  let in_bounds ctx ptyp =
    Log.trace (fun p -> p "in_bounds %a" (pp ctx) ptyp)
    (*~pp_ret:(Scalar.boolean_pretty ctx)*)
    @@ fun () -> in_bounds ctx ptyp

  let is_in_bounds ctx ptyp =
    match Scalar.query_boolean ctx @@ in_bounds ctx ptyp with
    | True -> true
    | _ -> false

  let tmp_count = ref 0

  type inclusion = LeftIncluded | RightIncluded | NoInclusion

  let pp_opt f fmt = function
    | None -> Format.pp_print_string fmt "None"
    | Some x -> Format.fprintf fmt "Some %a" f x

  (* Also returns a boolean telling whether [p2] is included in [p1]. *)
  let join_pred p1 p2 =
    if TypedC.Pred.is_true p1 || p1 = p2 then true, p1
    else false, TypedC.Pred.true_

  let join_size m_sz1 m_sz2 =
    let open TypedC in
    match m_sz1, m_sz2 with
    | (Fixed_length sz1), (Fixed_length sz2) when sz1 = sz2 ->
      true, (Fixed_length sz1)
    | (Variable_length s1), (Variable_length s2) when s1 = s2 ->
      true, (Variable_length s1)
    (* TODO: we should merge here. *)
    | _ -> false, assert false

  let query_min ~(size:In_bits.t) b =
    let inf = Z.sub Z.zero (Z.shift_left Z.one (size:>int)) in
    Log.debug (fun p -> p "inf : %d" (Z.to_int inf) );
    let sup = Z.shift_left Z.one (size:>int) in
    Log.debug (fun p -> p "sup : %d" (Z.to_int sup) );
    let exception Exit of Z.t in
    try Scalar.Query.Binary_Lattice.fold_crop_unsigned ~size b ~inf ~sup None (fun x acc ->
      raise (Exit x)
    )
    with Exit x -> Log.debug (fun p -> p "x : %d" (Z.to_int x) ); Some x

  let query_max ~size ctx b =
    let minus_b = Scalar.Binary_Forward.(bisub ~size ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ctx
      (biconst ~size Z.zero ctx) b) in
    Log.debug (fun p -> p "minus_b : %a" (Scalar.binary_pretty ~size ctx) minus_b );
    let minus_b = Scalar.Query.binary ~size ctx minus_b in
    match query_min ~size minus_b with
    | None -> None
    | Some x -> Some (Z.neg x)

  module Pred_to_Symb = struct

    open Symb

    (*--------------- Utility functions to compute the correct sizes for symbolic expressions ---------------*)

    type expr_size = Const of int | Min of int

    let eq_size sz1 sz2 =
      match sz1, sz2 with
      | Const i, Const j when i = j -> sz1
      | Min i, Const j when i <= j -> sz2
      | Const j, Min i when i <= j -> sz1
      | Min i, Min j -> Min (max i j)
      | Const i, Min j ->
        raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Min " ^ (string_of_int j)))
      | Min i, Const j ->
        raise (Failure ("invalid expr size : Min " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))
      | Const i, Const j ->
        raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))

    let rec expr_size (* ~size *) ctx expr =
      let open TypedC.Pred in
      match expr with
      (* | Self -> Const size *)
      | Self -> assert false
      | Const c -> Min (Z.numbits c)
      | Val(Sym s) -> let sz = fst @@ Symb.global_symbol ctx s in Const (sz:>int)
      | Binop(op, e1, e2) ->
        let sz1 = expr_size (* ~size *) ctx e1
        and sz2 = expr_size (* ~size *) ctx e2
        in eq_size sz1 sz2
      | Unop(Extract (index, len), e) ->
        let sz = expr_size (* ~size *) ctx e
        in eq_size sz (Const (len:>int))
    (* | Unop(op, e) -> *)

    let cmp_size (* ~size *) ctx expr1 expr2 =
      let size1 = expr_size (* ~size *) ctx expr1
      and size2 = expr_size (* ~size *) ctx expr2 in
      match eq_size size1 size2 with
      | Const sz | Min sz -> sz

    let size_of_expr ctx expr =
      In_bits.of_int @@ match expr_size ctx expr with Const sz | Min sz -> sz

    (* Function used to computed the value of a symbolic expression *)

    let rec binary_of_value ~size ctx v =
      let open TypedC in
      match v with
      | Sym s -> snd @@ global_symbol ctx s

    and cond_of_cmp ~size ctx cmpop v1 v2 =
      let open TypedC.Pred in
      match cmpop with
      | Equal ->
        beq ~size ctx v1 v2
      | NotEqual ->
        Boolean_Forward.not ctx @@ Binary_Forward.beq ~size ctx v1 v2
      | ULt ->
        Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v2 v1
      | SLt ->
        Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v2 v1
      | ULeq ->
        Binary_Forward.biule ~size ctx v1 v2
      | SLeq ->
        Binary_Forward.bisle ~size ctx v1 v2
      | UGeq ->
        Binary_Forward.biule ~size ctx v2 v1
      | SGeq ->
        Binary_Forward.bisle ~size ctx v2 v1
      | UGt ->
        Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v1 v2
      | SGt ->
        Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v1 v2

    and lift_unop ~size ctx op =
      let open TypedC.Pred in
      match op with
      | Extract (index,len) -> fun x ->
        Binary_Forward.bextract ~size:len ~index ~oldsize:size ctx x
        |> Binary_Forward.buext ~size ~oldsize:len ctx

    and lift_binop ~size ctx op =
      let open TypedC.Pred in
      match op with
      | Add -> Binary_Forward.biadd ~size ctx ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
      | Sub -> Binary_Forward.bisub ~size ctx ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false)
      | Mul -> Binary_Forward.bimul ~size ctx ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false)
      | Div -> Binary_Forward.bisdiv ~size ctx
      | And -> Binary_Forward.band ~size ctx
      | Or -> Binary_Forward.bor ~size ctx
      | Concat (size1,size2) -> Binary_Forward.bconcat ~size1 ~size2 ctx
      | Mod -> Binary_Forward.bismod ~size ctx

    (* XXX: Cette fonction apparait partout et ce n'est par normal. Il faut sans doute avoir un AADT representant les types et les operations sur les types. *)
    and binary_of_expr ~size ctx (* ~self *) e =
      let open TypedC.Pred in
      match e with
      (* | Self -> self *)
      | Const x -> Binary_Forward.biconst ~size x ctx
      | Self -> assert false
      | Val v -> binary_of_value ~size ctx v
      | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx (* ~self *) e)
      | Binop (op, e1, e2) -> lift_binop ~size ctx op
        (binary_of_expr ~size ctx (* ~self *) e1) (binary_of_expr ~size ctx (* ~self *) e2)

  end

  (* Utility function for dealing with serialize of parametrized types *)

  let fresh_id =
    let counter = ref 0 in
    fun () -> incr counter; !counter

  let new_symbol () =
    "t#" ^ (string_of_int @@ fresh_id ())

  let rec serialize_arguments ~widens ctxt args_t ctxu args_u ((inc, acc) as in_tup) =
    match args_t, args_u with
    | [],[] -> Context.Result (inc, acc, fun ctx out -> Some [], out)
    | arg_t :: args_t, arg_u :: args_u ->
      let size_t = Pred_to_Symb.size_of_expr ctxt arg_t in
      let size_u = Pred_to_Symb.size_of_expr ctxu arg_u in
      if size_t <> size_u then Context.Result (inc, acc, fun ctx out -> None, out)
      else
        let val_t = Pred_to_Symb.binary_of_expr ~size:size_t ctxt arg_t in
        let val_u = Pred_to_Symb.binary_of_expr ~size:size_u ctxt arg_u in
        let Context.Result (inc, acc, deserialize_args) = serialize_arguments ~widens ctxt args_t ctxu args_u in_tup in
        let Context.Result (inc, acc, deserialize_arg) = Symb.serialize_binary ~widens ~size:size_t ctxt val_t ctxu val_u (inc,acc) in
        Context.Result (inc, acc, fun ctx out ->
          let arg, out = deserialize_arg ctx out in
          let args, out = deserialize_args ctx out in
          match args with
          | None -> None, out
          | Some lst ->
            let new_symb = new_symbol () in
            Symb.add_global_symbol ~size:size_t ctx new_symb arg ;
            Some (TypedC.Pred.(Val (Sym new_symb)) :: lst), out
        )
    | _ -> Context.Result (inc, acc, fun ctx out -> None, out)

  let rec serialize_arguments_with_bottom_left ~widens ctxdummy ctxu args ((inc, acc) as in_tup) =
    match args with
    | [] -> Context.Result (inc, acc, fun ctx out -> [], out)
    | arg :: args ->
      let size = Pred_to_Symb.size_of_expr ctxu arg in
      let value = Pred_to_Symb.binary_of_expr ~size ctxu arg in
      let bottom = Symb.binary_empty ~size ctxdummy in
      let Context.Result (inc, acc, deserialize_args) = serialize_arguments_with_bottom_left ~widens ctxdummy ctxu args in_tup in
      let Context.Result (inc, acc, deserialize_arg) = Symb.serialize_binary ~widens ~size ctxdummy bottom ctxu value (inc,acc) in
      Context.Result (inc, acc, fun ctx out ->
        let arg, out = deserialize_arg ctx out in
        let args, out = deserialize_args ctx out in
        let new_symb = new_symbol () in
        Symb.add_global_symbol ~size ctx new_symb arg ;
        TypedC.Pred.(Val (Sym new_symb)) :: args, out
      )

  let rec serialize_arguments_with_bottom_right ~widens ctxt ctxdummy args ((inc, acc) as in_tup) =
    match args with
    | [] -> Context.Result (inc, acc, fun ctx out -> [], out)
    | arg :: args ->
      let size = Pred_to_Symb.size_of_expr ctxt arg in
      let value = Pred_to_Symb.binary_of_expr ~size ctxt arg in
      let bottom = Symb.binary_empty ~size ctxdummy in
      let Context.Result (inc, acc, deserialize_args) = serialize_arguments_with_bottom_right ~widens ctxt ctxdummy args in_tup in
      let Context.Result (inc, acc, deserialize_arg) = Symb.serialize_binary ~widens ~size ctxt value ctxdummy bottom (inc,acc) in
      Context.Result (inc, acc, fun ctx out ->
        let arg, out = deserialize_arg ctx out in
        let args, out = deserialize_args ctx out in
        let new_symb = new_symbol () in
        Symb.add_global_symbol ~size ctx new_symb arg ;
        TypedC.Pred.(Val (Sym new_symb)) :: args, out
      )

  let rec serialize_flexible ctxt typ_t ctxu typ_u (inc,acc) =
    let open TypedC in
    let inc0, new_pred = join_pred typ_t.pred typ_u.pred in
    match typ_t.descr, typ_u.descr with
    | Array (tp_t, m_sz1), Array (tp_u, m_sz2) when equiv tp_t tp_u ->
      let inc1, m_new_size = join_size m_sz1 m_sz2 in
      Context.Result (inc && inc0 && inc1, acc, fun ctx out ->
        (Some {descr = Array (tp_t, m_new_size); pred = new_pred}), out
      )

    | StructureFAM {structure=s_t;array=a_t},
      StructureFAM {structure=s_u;array=a_u} ->
      let ofs_t = TypedC.sizeof s_t in
      let ofs_u = TypedC.sizeof s_u in
      assert(ofs_t = ofs_u);    (* Not handled otherwise. *)
      assert(equiv s_t s_u);
      assert(equiv a_t a_u);
      (* let (ofs_t, name, typ_t) = List.hd @@ List.rev typs_t in *)
      (* let (ofs_u, _, typ_u) = List.hd @@ List.rev typs_u in *)
      (* let prefix_t = List.tl typs_t in *)
      (* let prefix_u = List.tl typs_u in *)
      let inc1 = (ofs_t = ofs_u) && equiv s_t s_u && equiv a_t a_u in
      if inc1 then
        let Context.Result (inc, tup, des_last) = serialize_flexible ctxt a_t ctxu a_u (inc,acc) in
        Context.Result (inc, tup, fun ctx out ->
          let last_typ, out = des_last ctx out in
          match last_typ with
          | None -> None, out
          | Some typ ->
            (Some {descr = StructureFAM {structure=s_t;array=typ}; pred = new_pred}), out
        )

      else
        Context.Result (inc && inc1, acc, fun ctx out -> None, out)
    | _ -> Context.Result (inc, acc, fun ctx out -> None, out)

  let serialize ~widens ctxt t ctxu u (included, acc) =
    incr tmp_count;
    let count = !tmp_count in
    let ptrsize = Codex_config.ptr_size () in
    Log.debug (fun p -> p "serialize no. %d: %a %a" count (pp ctxt) t (pp ctxu) u);
    Log.debug (fun p -> p "serialize no. %d: initial included = %b" count included);
    match t, u with
    (* This is only an optimization. Is it safe? Apparently not, got
     * [Never_refined] exceptions using it. *)
    (*
      | PtrT _, PtrT _ when eq ctx t u ->
          Scalar.Scalar.Result (acc, fun output -> t, output)
      *)
    | {ptyp = ({descr=Array (elem_t, m_sz1);_}); idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp = ({descr=Array (elem_u, m_sz2);_}); idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs}
      when TypedC.equiv elem_t elem_u
      && t_elem_sz = u_elem_sz ->

      let inc0 = true in
      Log.debug (fun p -> p "predicate inclusion: %B" inc0);
      (* Join indices *)
      Log.debug (fun p -> p "joining of indices...");
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
      let Scalar.Context.Result (_, acc, deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_idx ctxu u_idx (true, acc) in
      let inc1 =
        let iu = Scalar.Query.binary ~size:ptrsize ctxu u_idx in
        let it = Scalar.Query.binary ~size:ptrsize ctxt t_idx in
        map2 Z.leq false (query_min ~size:ptrsize it) (query_min ~size:ptrsize iu)
      in
      Log.debug (fun p -> p "index inclusion: %B" inc1);
      let Context.Result (_, acc, deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_ofs ctxu u_ofs (true, acc) in
      let inc2 =
        let ou = Scalar.Query.binary ~size:ptrsize ctxu u_ofs in
        let ot = Scalar.Query.binary ~size:ptrsize ctxt t_ofs in
        map2 Z.leq false (query_min ~size:ptrsize ot) (query_min ~size:ptrsize ou)
      in
      Log.debug (fun p -> p "ofs inclusion: %B" inc2);
      (* XXX: compute new size. Is this sound? *)
      let inc3, m_new_size = join_size m_sz1 m_sz2 in
      Log.debug (fun p -> p "size inclusion: %B" inc3);
      let included' = inc0 && inc1 && inc2 && inc3 in
      Log.debug (fun p -> p "inclusion is %B" included');
      let included = included && included' in
      Context.Result (included, acc, fun ctx output ->
        let new_offset, output = deserialize_ofs ctx output in
        let new_index, output = deserialize_idx ctx output in
        let ret =
          Some {ptyp = ({descr=Array (elem_t, m_new_size); pred=TypedC.Pred.true_});
            idx = new_index; elem_size = t_elem_sz; ofs = new_offset}
        in
        Log.debug (fun p -> p "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret);
        ret, output)

    | {ptyp={descr = (StructureFAM {structure = s_t; array = a_t})} as tt;  idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp={descr = (StructureFAM {structure = s_u; array = a_u})} as tu;  idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs}
      when t_ofs = u_ofs && t_elem_sz = u_elem_sz ->
      Log.debug (fun p -> p "in Typed_address.serialize, in flexible array member case") ;
      let inc0 = true in
      Log.debug (fun p -> p "predicate inclusion: %B" inc0);
      Log.debug (fun p -> p "joining of indices...");
      (* Join indices. This should only happen between indices zero and
       * bottom, since it is an invariant that all indices of non-array
       * types should be included in the singleton zero. *)
      let Context.Result (inc1, acc, deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_idx ctxu u_idx (true,acc) in
      Log.debug (fun p -> p "idx inclusion: %B" inc1);
      let Context.Result (inc2, acc, deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_ofs ctxu u_ofs (true, acc) in
      Log.debug (fun p -> p "ofs inclusion: %B" inc2);
      let Context.Result (inc3, acc, deserialize_ptyp) = serialize_flexible ctxt tt ctxu tu (true,acc) in
      Log.debug (fun p -> p "ptyp inclusion: %B" inc3);
      let included' = inc0 && inc1 && inc2 && inc3 in
      Log.debug (fun p -> p "inclusion is %B" included');
      let included = included && included' in
      Context.Result (included, acc, fun ctx output ->
        let new_ptyp, output = deserialize_ptyp ctx output in
        let new_offset, output = deserialize_ofs ctx output in
        let new_index, output = deserialize_idx ctx output in
        match new_ptyp with
        | None -> None, output
        | Some pt -> Some {ptyp=pt; idx=new_index; elem_size=t_elem_sz; ofs=new_offset}, output
      )

    | {ptyp=pt;idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp=pu;idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs}
      when TypedC.(equiv pt pu)
      && t_ofs == u_ofs && t_elem_sz = u_elem_sz ->
      let inc0 = true in
      Log.debug (fun p -> p "predicate inclusion: %B" inc0);
      Log.debug (fun p -> p "joining of indices...");
      (* Join indices. This should only happen between indices zero and
       * bottom, since it is an invariant that all indices of non-array
       * types should be included in the singleton zero. *)
      let Context.Result (inc1, acc, deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_idx ctxu u_idx (true,acc) in
      Log.debug (fun p -> p "idx inclusion: %B" inc1);
      let Context.Result (inc2, acc, deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_ofs ctxu u_ofs (true, acc) in
      Log.debug (fun p -> p "non_aligned inclusion: %B" inc2);
      let included' = inc0 && inc1 && inc2 in
      Log.debug (fun p -> p "inclusion is %B" included');
      let included = included && included' in
      Context.Result (included, acc, fun ctx output ->
        let new_offset, output = deserialize_ofs ctx output in
        let new_index, output = deserialize_idx ctx output in
        let ret = Some {ptyp=pt; idx=new_index; elem_size=t_elem_sz; ofs=new_offset} in
        (* Log.debug (fun p -> p "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret); *)
        ret, output)

    | {ptyp=pt; idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp=pu; idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs}
      when TypedC.(equiv pt pu) ->
      let inc0 = true in
      Log.debug (fun p -> p "predicate inclusion: %B" inc0);
      Log.debug (fun p -> p "joining of indices...");
      (* Join indices. This should only happen between indices zero and
       * bottom, since it is an invariant that all indices of non-array
       * types should be included in the singleton zero. *)
      let Context.Result (inc1, acc, deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_idx ctxu u_idx (true,acc) in
      Log.debug (fun p -> p "idx inclusion: %B" inc1);
      let Context.Result (inc2, acc, deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_ofs ctxu u_ofs (true,acc) in
      Log.debug (fun p -> p "ofs inclusion: %B" inc2);
      let included' = inc0 && inc1 && inc2 in
      Log.debug (fun p -> p "inclusion is %B" included');
      let included = included && included' in
      Context.Result (included, acc, fun ctx output ->
        let new_offset, output = deserialize_ofs ctx output in
        let new_index, output = deserialize_idx ctx output in
        let ret = Some {ptyp=pt; idx=new_index; elem_size=t_elem_sz; ofs=new_offset} in
        (* Log.debug (fun p -> p "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret); *)
        ret, output)

    | {ptyp={descr=Application{constr = t_constr;args = t_exprs};pred=t_pred}; idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp={descr=Application{constr = u_constr;args = u_exprs};pred=u_pred}; idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs}
      when TypedC.(Constr.equal t_constr u_constr) && TypedC.Pred.equal t_pred u_pred ->
      let inc0 = true in
      Log.debug (fun p -> p "predicate inclusion: %B" inc0);
      Log.debug (fun p -> p "joining of indices...");
      let Context.Result (inc1, acc, deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_idx ctxu u_idx (true,acc) in
      Log.debug (fun p -> p "idx inclusion: %B" inc1);
      let Context.Result (inc2, acc, deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_ofs ctxu u_ofs (true,acc) in
      Log.debug (fun p -> p "ofs inclusion: %B" inc2);
      let Context.Result (inc3, acc, deserialize_arguments) = serialize_arguments ~widens ctxt t_exprs ctxu u_exprs (true, acc) in
      Log.debug (fun p -> p "parametrized arguments inclusion : %B" inc3 );
      let included' = inc0 && inc1 && inc2 && inc3 in
      Log.debug (fun p -> p "inclusion is %B" included');
      let included = included && included' in
      Context.Result (included, acc, fun ctx output ->
        let new_exprs, output = deserialize_arguments ctx output in
        let new_offset, output = deserialize_ofs ctx output in
        let new_index, output = deserialize_idx ctx output in
        match new_exprs with
        | None -> None, output
        | Some new_args ->
          let ret = Some {ptyp={descr = Application{constr = t_constr; args = new_args}; pred=t_pred};
            idx=new_index; elem_size=t_elem_sz; ofs=new_offset} in
          (* Log.debug (fun p -> p "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret); *)
          ret, output)

    (* Joining a weak type on one side with another *)
    (* TODO : with will cause a problems if we join an application type with a weak type as the argument will not be serialized on one side *)
    | {ptyp=({descr = Weak wtyp} as wk_ptyp); idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp; idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs}
      | {ptyp; idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp=({descr = Weak wtyp} as wk_ptyp); idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs} ->

      if t_ofs <> u_ofs then raise (Type_error "Non equal constant offsets") ;
      let inc0 = true in
      (* Updating weak type if the types are compatible *)
      if TypedC.sizeof wtyp = TypedC.sizeof ptyp then begin
        wk_ptyp.descr <- ptyp.descr ;
        wk_ptyp.pred <- ptyp.pred ;
        end ;

      Log.debug (fun p -> p "predicate inclusion: %B" inc0);
      Log.debug (fun p -> p "joining of indices...");
      let Context.Result (inc1, acc, deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_idx ctxu u_idx (true,acc) in
      Log.debug (fun p -> p "idx inclusion: %B" inc1);
      let Context.Result (inc2, acc, deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxt t_ofs ctxu u_ofs (true,acc) in
      Log.debug (fun p -> p "ofs inclusion: %B" inc2);
      let included' = inc0 && inc1 && inc2 in
      Log.debug (fun p -> p "inclusion is %B" included');
      let included = included && included' in
      Context.Result (included, acc, fun ctx output ->
        let new_offset, output = deserialize_ofs ctx output in
        let new_index, output = deserialize_idx ctx output in
        let ret = Some {ptyp=ptyp; idx=new_index; elem_size = t_elem_sz; ofs=new_offset} in
        (* Log.debug (fun p -> p "serialize no. %d returns %a%!" count (pp_opt (pp ctx)) ret); *)
        ret, output)

    (* Here we ignore the indices (should be zero) *)
    | {ptyp=pointed_t; idx=t_idx; elem_size=t_elem_sz; ofs=t_ofs},
      {ptyp=pointed_u; idx=u_idx; elem_size=u_elem_sz; ofs=u_ofs} ->
      begin try
        let inc0 = true in
        Log.debug (fun p -> p "predicate inclusion: %B" inc0);
        (* The lattice {TypedC.t} has a tree structure (once excluded
           * {ZeroT}), so all we need to do to find the lub is to find their
           * lowest common ancestor. We implement this algorithm in a naive
           * way: compute the list of parents up to the root for {t} and {u},
           * then reverse these two lists and take the last element of their
           * common prefix. *)
        (* Computes list of parents in descending order, including {x},
           * excluding {Top}. *)

        (* Checks if "t_elem_sz" and "u_elem_sz" are compatible *)
        let t_idx,t_ofs,u_idx,u_ofs =
          if t_elem_sz = u_elem_sz then t_idx, t_ofs, u_idx, u_ofs
          else
            let t_ofs = Scalar.Binary_Forward.biadd ~size:ptrsize ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctxt t_ofs @@
              Scalar.Binary_Forward.bimul ~size:ptrsize ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctxt t_idx @@
              Scalar.Binary_Forward.biconst ~size:ptrsize (Z.of_int t_elem_sz) ctxt in
            let t_idx = Scalar.Binary_Forward.biconst ~size:ptrsize Z.zero ctxt in

            let u_ofs = Scalar.Binary_Forward.biadd ~size:ptrsize ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctxu u_ofs @@
              Scalar.Binary_Forward.bimul ~size:ptrsize ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctxu u_idx @@
              Scalar.Binary_Forward.biconst ~size:ptrsize (Z.of_int u_elem_sz) ctxu in
            let u_idx = Scalar.Binary_Forward.biconst ~size:ptrsize Z.zero ctxu in
            t_idx, t_ofs, u_idx, u_ofs
        in

        let t_ofs =
          match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptrsize @@ binary ~size:ptrsize ctxt t_ofs) with
          | Some ofs -> Z.to_int ofs
          | None -> raise (Type_error "Non aligned offset is not constant")
        in

        let u_ofs =
          match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptrsize @@ binary ~size:ptrsize ctxu u_ofs) with
          | Some ofs -> Z.to_int ofs
          | None -> raise (Type_error "Non aligned offset is not constant")
        in

        let parents ctx (x : TypedC.typ) idx ofs =
          let rec aux x ofs =
            try
              let parent_t, parent_ofs = type_at_offset x ofs in
              (x,ofs) :: aux parent_t parent_ofs
            with Exit -> [(x, ofs)]
          in
          (try
            let parent_t, parent_ofs, alarm =
              type_at_offset_complex ctx x idx ofs in
            let res = match alarm with
              | Some msg ->
                Codex_log.warning "Invalid pointer has no parent. Origin: %s" msg;
                [(x, ofs)]
              | None -> (x, ofs) :: aux parent_t parent_ofs
            in res
          with Exit -> [(x, ofs)])
        in
        let parents_t = parents ctxt pointed_t t_idx t_ofs in
        Log.debug (fun p -> p "parents_t = [%a]" (Format.pp_print_list (fun fmt (t,ofs) -> Format.fprintf fmt "(%a,%d)" TypedC.pp t ofs)) parents_t);
        let parents_u = parents ctxu pointed_u u_idx u_ofs in
        Log.debug (fun p -> p "parents_u = [%a]" (Format.pp_print_list (fun fmt (t,ofs) -> Format.fprintf fmt "(%a,%d)" TypedC.pp t ofs)) parents_u);
        let rec walk prev l1 l2 = match l1,l2 with
          | [], _ ->
            prev, LeftIncluded
          | _, [] ->
            prev, RightIncluded
          | (x,ox) :: r1, (y,oy) :: r2 ->
            if TypedC.equiv x y && ((=) : int -> int -> bool) ox oy
            then walk (Some (x,ox)) r1 r2
            else prev, NoInclusion
        in
        (match walk None (List.rev parents_t) (List.rev parents_u) with
          | Some (t,o), inclusion ->
            let inc1 = inclusion = LeftIncluded in
            let included = included && inc0 && inc1 in
            let d ctx output =
              let zero = index_zero ctx in
              let new_offset = Scalar.Binary_Forward.biconst ~size:ptrsize (Z.of_int o) ctx in
              let res = Some {ptyp=t; idx = zero; elem_size = t_elem_sz; ofs=new_offset} in
              Log.debug (fun p -> p "inclusion is %B" included);
              Log.debug (fun p -> p "serialize no. %d returns %a" count (pp_opt (pp ctx)) res);
              res,output
            in
            Context.Result (included, acc, d)
          | None, _ ->
            Log.debug (fun p -> p "serialize no. %d: inclusion is false" count);
            Log.debug (fun p -> p "serialize no. %d returns Top" count);
            Context.Result (false, acc, fun ctx output -> None, output)
        )
      with Type_error s ->
        (* Emit_alarm.emit_alarm "serialize";
          Codex_log.error "Type error during serialize no. %d: %s" count s; *) (* Not necessary, it simply signals a loss in precision *)
        Codex_log.warning "Type error during serialize no. %d: %s" count s;
        Log.debug (fun p -> p "serialize no. %d: inclusion is false" count);
        Context.Result (false, acc, fun ctx output -> None, output)
        end


  let serialize_with_bottom_left ~widens ctxdummy ctxb ({ptyp;idx;elem_size;ofs} as t) (included,acc) =
    incr tmp_count;
    let count = !tmp_count in
    let ptrsize = Codex_config.ptr_size () in
    Log.debug (fun p -> p "serialize no. %d (serialize_with_bottom_left): %a" count (pp ctxb) t);
    Log.debug (fun p -> p "serialize no. %d: initial included = %b" count included);
    match ptyp with
    | {descr = Application {constr; args = exprs}} ->
      let dummy = Scalar.binary_empty ~size:ptrsize ctxdummy in
      let Context.Result(inc,acc,deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxdummy dummy ctxb idx (included,acc) in
      let Context.Result(inc,acc,deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxdummy dummy ctxb ofs (included,acc) in
      let Context.Result(inc,acc, deserialize_arguments) = serialize_arguments_with_bottom_left ~widens ctxdummy ctxb exprs (included,acc) in
      Context.Result (inc, acc, fun ctx output ->
        let args, output = deserialize_arguments ctx output in
        let ofs, output = deserialize_ofs ctx output in
        let idx, output = deserialize_idx ctx output in
        let ret = {ptyp = {ptyp with descr = Application {constr; args}};idx; elem_size; ofs} in
        (* Log.debug (fun p -> p "serialize no. %d (serialize_with_bottom_left) returns %a%!" count (pp ctx) ret); *)
        ret, output
      )
    | _ ->
      let dummy = Scalar.binary_empty ~size:ptrsize ctxdummy in
      let Context.Result(inc,acc,deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxdummy dummy ctxb idx (included,acc) in
      let dummy = Scalar.binary_empty ~size:ptrsize ctxdummy in
      let Context.Result(inc,acc,deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxdummy dummy ctxb ofs (included,acc) in
      Context.Result (inc, acc, fun ctx output ->
        let ofs, output = deserialize_ofs ctx output in
        let idx, output = deserialize_idx ctx output in
        let ret = {ptyp; idx; elem_size; ofs} in
        (* Log.debug (fun p -> p "serialize no. %d (serialize_with_bottom_left) returns %a%!" count (pp ctx) ret); *)
        ret, output
      )

  let serialize_with_bottom_right ~widens ctxa ctxdummy ({ptyp;idx;elem_size;ofs} as u) (included,acc) =
    incr tmp_count;
    let count = !tmp_count in
    let ptrsize = Codex_config.ptr_size () in
    Log.debug (fun p -> p "serialize no. %d (serialize_with_bottom_right): %a" count (pp ctxa) u);
    Log.debug (fun p -> p "serialize no. %d: initial included = %b" count included);
    match ptyp with
    | {descr = Application {constr;args = exprs};} ->
      let dummy = Scalar.binary_empty ~size:ptrsize ctxdummy in
      let Context.Result(inc,acc,deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxa idx ctxdummy dummy (included,acc) in
      let Context.Result(inc,acc,deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxa ofs ctxdummy dummy (included,acc) in
      let Context.Result(inc,acc, deserialize_arguments) = serialize_arguments_with_bottom_right ~widens ctxa ctxdummy exprs (included,acc) in
      Context.Result (inc, acc, fun ctx output ->
        let args, output = deserialize_arguments ctx output in
        let ofs, output = deserialize_ofs ctx output in
        let idx, output = deserialize_idx ctx output in
        let ret = {ptyp = {ptyp with descr = Application {constr; args}}; idx; elem_size; ofs} in
        (* Log.debug (fun p -> p "serialize no. %d (serialize_with_bottom_right) returns %a%!" count (pp ctx) ret); *)
        ret, output
      )
    | _ ->
      let dummy = Scalar.binary_empty ~size:ptrsize ctxdummy in
      let Context.Result(inc,acc,deserialize_idx) = Scalar.serialize_binary ~widens ~size:ptrsize ctxa idx ctxdummy dummy (included,acc) in
      let Context.Result(inc,acc,deserialize_ofs) = Scalar.serialize_binary ~widens ~size:ptrsize ctxa ofs ctxdummy dummy (included,acc) in
      Context.Result (inc, acc, fun ctx output ->
        let ofs, output = deserialize_ofs ctx output in
        let idx, output = deserialize_idx ctx output in
        let ret = {ptyp; idx; elem_size; ofs} in
        (* Log.debug (fun p -> p "serialize no. %d (serialize_with_bottom_right) returns %a%!" count (pp ctx) ret); *)
        ret, output
      )

  let subtype ctx t u =
    (* Careful to the order of t and u: [included] is true iff u includes t. *)
    let Context.Result (included,_,_) =
      serialize ~widens:false ctx u ctx t (true, Context.empty_tuple ()) in
    included

end


module MakeAddressOnly
(Sub:Memory_sig.ADDRESS)
(Symb:Memory_sig.FIXED_SIZE_VALUE_DOMAIN with module Scalar = Sub.Scalar)(* : Memory_sig.ADDRESS *) = struct

  module Type = MakeType(Sub.Scalar)(Symb)

  module Address(* :Memory_sig.ADDRESS *) = struct

    module Sub = Sub

    (** Possibly add (flow-insensitive) type information to a value. *)
    type binary = Sub.binary * Type.t option

    (** Simple boolean AST with leafs being scalar or sub boolean *)
    (* type boolean = *)
    (*   | BSub of Sub.boolean *)
    (*   | BScalar of Scalar.boolean *)
    (*   | BAnd of boolean * boolean *)
    (*   | BOr of boolean * boolean *)

    module Binary = struct
      type t = binary
      let pretty fmt (bin,typ) =
        Format.fprintf fmt "(%a:%s)"
          Sub.Binary.pretty bin
          (match typ with None -> "notype" | Some _ -> "sometype")

      let equal (bina,typa) (binb,typb) = Sub.Binary.equal bina binb
      let compare (bina,typa) (binb,typb) = Sub.Binary.compare bina binb
      let hash (bina,typa) = Sub.Binary.hash bina
    end
module Scalar = Sub.Scalar
      type abstract_boolean =
        | BSub of Sub.boolean
        | BScalar of Scalar.boolean



    module ABoolean : Symbolic_boolean.AbstractBoolean with type boolean = abstract_boolean and module Scalar = Scalar = struct

      module Scalar = Scalar

      type boolean = abstract_boolean

      let compare a b = match a,b with
        |BSub a,BSub b -> Sub.Boolean.compare a b
        |BScalar a,BScalar b -> Scalar.Boolean.compare a b
        |BSub _ ,_ -> -1
        |_ ,BSub _ -> 1

      let pretty fmt = let open Format in function
        |BSub b-> fprintf fmt "%a" Sub.Boolean.pretty b
        |BScalar b -> fprintf fmt "%a" Scalar.Boolean.pretty b

      let not ctx = function
        |BSub b -> BSub (Sub.Boolean_Forward.not ctx b)
        |BScalar b -> BScalar (Scalar.Boolean_Forward.not ctx b)


      let query_boolean ctx = function
        | BSub b -> Sub.query_boolean ctx b
        | BScalar b -> Scalar.query_boolean ctx b

      let assume ctx = function
        | BSub b -> Sub.assume ctx b
        | BScalar b -> Scalar.assume ctx b

      let true_ ctx = BSub( Sub.Boolean_Forward.true_ ctx )
      let false_ ctx = BSub( Sub.Boolean_Forward.false_ ctx )

    let boolean_empty ctx = BSub(Sub.boolean_empty ctx )
    let boolean_unknown ctx = BSub(Sub.boolean_unknown ctx )
    let satisfiable ctx _ = assert false
    let serialize_boolean ctx _ = assert false
    end

    module SymbB = Symbolic_boolean.Make(ABoolean)
    include SymbB

    let imperative_assume ctx b =
      match assume ctx b with
        | None -> ()
        | Some ctx' -> Sub.Scalar.Context.assign ctx ctx'

    module Query = struct
      include Sub.Query
      let binary ~size ctx (b,_) = binary ~size ctx b
    end

    let binary_empty ~size ctx = (Sub.binary_empty ~size ctx, None)
    (* let boolean_empty ctx = BSub (Sub.boolean_empty ctx) *)

    let binary_unknown ~size ctx = (Sub.binary_unknown ~size ctx,None)
    let binary = Query.binary

    let binary2scalar_binary ~size ctx (value,_) = Sub.binary2scalar_binary ~size ctx value

    let binary_pretty ~size ctx fmt ((value,typ):binary) =
      if Query.Binary_Lattice.is_empty ~size @@ binary ~size ctx (value,typ) then
        Format.pp_print_string fmt "Bottom"
      else
        match typ with
        |None -> Sub.binary_pretty ~size ctx fmt value
        |Some typ -> Format.fprintf fmt "@[<hov 2>(%a :@ %a)@]"
          (Sub.binary_pretty ~size ctx) value
          (Type.pp ctx) typ


    (* TODOML: In this code, we care about the type only when it is a pointer type.
       I think this is no coincidence, and possibly the type of the typ field
       should reflect that. *)
    let serialize_typ ~widens ~size ctxa ((va,ta) as a) ctxb ((vb,tb) as b) ((inc,acc) as in_acc) =
      Log.debug (fun p -> p "serialize_typ %a %a" (binary_pretty ~size ctxa) a  (binary_pretty ~size ctxb) b) ;
      match ta,tb with
      | Some typa, Some typb -> Type.serialize ~widens ctxa typa ctxb typb in_acc
      | Some typa, None
        when Query.Binary_Lattice.is_empty ~size @@ binary ~size ctxb b ->
        let Context.Result(inc, acc, des) = Type.serialize_with_bottom_right ~widens ctxa ctxb typa in_acc in
        Context.Result(inc, acc, fun ctx out ->
          let typ, out = des ctx out in
          Some typ, out
        )
      | None, Some typb
        when Query.Binary_Lattice.is_empty ~size @@ binary ~size ctxa a ->
        let Context.Result(inc, acc, des) = Type.serialize_with_bottom_left ~widens ctxa ctxb typb in_acc in
        Context.Result(inc, acc, fun ctx out ->
          let typ, out = des ctx out in
          Some typ, out
        )
      | _ -> Context.Result(inc, acc, fun ctx out -> None, out)


    let serialize ~widens ~size ctxa ((va,_) as a) ctxb ((vb,_) as b) acc =
      let Context.Result(inc, acc, deserialize_typ) = serialize_typ ~widens ~size ctxa a ctxb b acc in
      let Context.Result(inc, acc, deserialize_val) = Sub.serialize ~widens ~size ctxa va ctxb vb (inc,acc) in
      Context.Result (inc, acc, fun ctx output ->
        let value, output = deserialize_val ctx output in
        let typ, output = deserialize_typ ctx output in
        (value,typ), output
      )

    let bchoose ~size choice ctx (addr,typ) =
      let res = Sub.bchoose ~size choice ctx addr in
      res,typ

    let decomp ~size ctx addr Type.{idx; elem_size; ofs} =
      let base = Sub.bindex ~size (-elem_size) ctx addr idx in
      let offset = Scalar.Binary_Forward.bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx idx
        @@ Scalar.Binary_Forward.biconst ~size (Z.of_int elem_size) ctx in
      let base =
        match Scalar.Query.(Binary_Lattice.is_singleton ~size @@ binary ~size ctx ofs) with
        | None -> Sub.bindex ~size (-1) ctx base ofs
        | Some k -> Sub.bshift ~size ~offset:(-Z.to_int k) ~max:None ctx base
      in base, Scalar.Binary_Forward.biadd ~size
           ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx offset ofs


    let ble ~size ctx (va,ta) (vb,tb) =
      match ta, tb with
      | None, _ | _, None -> lift @@ BSub(Sub.ble ~size ctx va vb)
      | Some typa, Some typb ->
        Log.debug (fun p -> p "in Typed_address.ble, typa = %a and typb = %a" (Type.pp ctx) typa (Type.pp ctx) typb) ;
        let basea,offa = decomp ~size ctx va typa in
        let baseb,offb = decomp ~size ctx vb typb in
        (* Log.debug (fun p -> p "in Typed_address.ble, basea = %a, offa = %a" (binary_pretty ~size ctx) basea (Scalar.binary_pretty ~size ctx) offa) ; *)
        (* Log.debug (fun p -> p "in Typed_address.ble, baseb = %a, offb = %a" (binary_pretty ~size ctx) baseb (Scalar.binary_pretty ~size ctx) offb) ;  *)
        let is_eq = Sub.beq ~size ctx basea baseb in
        begin match Sub.query_boolean ctx is_eq with
          | True -> lift @@ BScalar (Scalar.Binary_Forward.bisle ~size ctx offa offb)
          | _ -> lift @@ BSub (Sub.ble ~size ctx va vb)
          end

    let ble ~size ctx a b =
      Log.trace (fun p -> p "ble %a %a" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b)
      (*~pp_ret:(boolean_pretty ctx)*)
      @@ fun () -> ble ~size ctx a b
    let beq ~size ctx (va,_) (vb,_) = lift @@ BSub (Sub.beq ~size ctx va vb)

    let beq ~size ctx a b =
      Log.trace (fun p -> p "beq %a %a" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b)
        (*~pp_ret:(boolean_pretty ctx)*)
        (fun () -> beq ~size ctx a b)

    (* let rec assume_tree ctx tree = *)
    (*   match tree with *)
    (*   | BScalar x -> Some x, None *)
    (*   | BSub x -> None, Some x *)
    (*   | BAnd(l,r) -> *)
    (*       let s_l, a_l = assume_tree  ctx l in *)
    (*       let s_r, a_r = assume_tree  ctx r in *)
    (*       option_merge (Scalar.Boolean_Forward.(&&) ctx) s_l s_r, *)
    (*       option_merge (Sub.Boolean_Forward.(&&) ctx) a_l a_r *)
    (*   | BOr(l,r) -> *)
    (*       let s_l, a_l = assume_tree  ctx l in *)
    (*       let s_r, a_r = assume_tree  ctx r in *)
    (*       (* eturn None if one is none (assume (cond || true)) *) *)
    (*       option_map2 (Scalar.Boolean_Forward.(||) ctx) s_l s_r, *)
    (*       option_map2 (Sub.Boolean_Forward.(||) ctx) a_l a_r *)

    (* Assume on boolean from address types *)
    (* let assume ctx cond = *)
    (*   Log.debug (fun p -> p "Typed_address.assume %a" (boolean_pretty ctx) cond) ; *)
    (*   let a,b = assume_tree  ctx cond in *)
    (*   let ctx = *)
    (*     match a with *)
    (*     | None -> Some ctx *)
    (*     | Some a -> Scalar.assume ctx a *)
    (*   in *)
    (*   match ctx with *)
    (*   | None -> None *)
    (*   | Some ctx ->  *)
    (*     begin match b with *)
    (*       | None -> Some ctx *)
    (*       | Some b -> Sub.assume ctx b *)
    (*     end *)

    (* let imperative_assume ctx cond = *)
    (*   let a,b = assume_tree ctx cond in *)
    (*   match a with None -> () | Some a -> Scalar.imperative_assume ctx a; *)
    (*   match b with None -> () | Some b -> Sub.imperative_assume ctx b *)
    (**)
    (* let query_boolean = Query.boolean *)
    (**)
    (* let satisfiable_quadri ctx x = *)
    (*   let open Lattices.Quadrivalent in *)
    (*   match x with *)
    (*   | Bottom | False -> Smtbackend.Smtlib_sig.Unsat *)
    (*   | Top | True -> Scalar.satisfiable ctx (Scalar.Boolean_Forward.true_ ctx) *)
    (**)
    (* let satisfiable ctx boolean = *)
    (*   match Query.boolean_tree ctx boolean with *)
    (*   | BR_Known x -> satisfiable_quadri ctx x *)
    (*   | BR_Scal s -> Scalar.satisfiable ctx s *)
    (*   | BR_Sub a -> Sub.satisfiable ctx a *)

    (* module Boolean_Forward = struct  *)
    (**)
    (*   module Arity = Domain_sig.Context_Arity_Forward(Scalar.Context) *)
    (**)
    (*   let rec not ctx = function *)
    (*     | BScalar b -> BScalar (Scalar.Boolean_Forward.not ctx b) *)
    (*     | BSub b -> BSub (Sub.Boolean_Forward.not ctx b) *)
    (*     | BAnd (l,r) -> BOr (not ctx l, not ctx r) *)
    (*     | BOr (l,r) -> BAnd (not ctx l, not ctx r) *)
    (**)
    (*   let (&&) ctx a b = BAnd (a,b) *)
    (*   let (||) ctx a b = BOr (a,b) *)
    (**)
    (*   let true_ ctx = BSub (Sub.Boolean_Forward.true_ ctx) *)
    (*   let false_ ctx = BSub (Sub.Boolean_Forward.false_ ctx) *)
    (* end *)

    let check_type ~size ctx (typ : TypedC.typ) ((vaddr, curtyp) as addr) =
      Log.trace (fun p ->
        p "check_type %a %a" TypedC.pp typ
          (binary_pretty ~size ctx) addr)
      @@ fun () ->
      let open Type_check_tree in
      let typing_node = create_typing_node addr (binary_pretty ~size ctx) typ in
      let pointed =
        match typ.descr with Ptr { pointed } -> pointed | _ -> assert false
      in
      let zero =
        Scalar.Binary_Forward.biconst ~size:(Codex_config.ptr_size ())
          Z.zero ctx
      in
      let t =
        Type.{ ptyp = pointed; idx = zero; ofs = zero; elem_size = 1 }
      in
      match curtyp with
      | None -> (
        Codex_log.error "Storing untyped address@ %a to region of type@ %a"
          (binary_pretty ctx ~size) addr (Type.pp ctx) t;
        type_error typing_node Operator.Alarm.Typing_store)

      | Some ({ ptyp = addr_typ } as curtyp) -> (
        (* True almost everywhere, but in em3d there are conversions between array and element. *)
        (* assert(TypedC.equal pointed t'); *)
        (* assert(Type.index_is_zero ctx t.idx && Type.index_is_zero ctx t.ofs); *)
        (* TODO: maybe we should reuse binary_unknown_type here. *)

        match addr_typ.descr with
        | Weak wk_typ ->
          addr_typ.descr <- pointed.descr;
          no_error typing_node
        | _ ->
          let res = Type.subtype ctx curtyp t in
          if not res then (
            Codex_log.error
              "Storing value@ %a to region of type@ %a: value type not \
                   included in region type"
              (binary_pretty ctx ~size) addr (Type.pp ctx) t;
            type_error typing_node Operator.Alarm.Typing_store)
          else no_error typing_node)


    let type_of ~size:_ _ctx (va, typ) =
      match typ with
      | None -> None
      | Some Type.{ptyp} -> Some TypedC.(Build.ptr ptyp Pred.true_)

    let within_bounds ~size ctx (va,typ) =
      match typ with
      | None -> lift @@ BSub(Sub.within_bounds ~size ctx va)
      | Some ptyp -> Boolean_Forward.(&&) ctx  (lift @@ BScalar (Type.in_bounds ctx ptyp)) (lift @@ BSub( Sub.within_bounds ~size ctx va))
    (* | None -> BSub (Sub.within_bounds ~size ctx va) *)
    (* | Some ptyp -> BAnd (ABoolean.BScalar (Type.in_bounds ctx ptyp), BSub (Sub.within_bounds ~size ctx va)) *)

    let add ~size result ctx ((_,atyp) as a) b =
      Log.trace (fun p -> p "add" ) @@ fun () ->
      let ptrsize = Codex_config.ptr_size () in
      atyp |> Option.map (fun typ ->
        Log.debug (fun p -> p "@[<hov 2>addition of offset %a to param. addr. %a@]"
          (Scalar.binary_pretty ~size:ptrsize ctx) b
          (binary_pretty ~size ctx) a);
        Type.add_offset ctx b typ
        (* if not @@ BinaryMap.mem result !state then state := BinaryMap.add result new_typ !state ; *)
      )

    let bindex ~size offset ctx ((aval,atyp) as a) b =
      Log.trace (fun p -> p "bindex offset:%d %a %a" offset (binary_pretty ~size ctx) a (Scalar.binary_pretty ~size ctx) b )
        ~pp_ret:Binary.pretty
      @@ fun () ->
      let ofs = Scalar.Binary_Forward.(bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:true ~nuw:false) ctx (biconst ~size (Z.of_int offset) ctx) b) in
      let result = Sub.bindex ~size offset ctx aval b in
      let typ = add ~size result ctx a ofs in
      (result,typ)

    let bisub ~size ctx (va,_) (vb,_) = Sub.bisub ~size ctx va vb

    let bshift ~size ~offset ~max ctx ((aval,atyp) as value)  =
      Log.trace (fun p -> p "bshift offset:%d" offset)
        ~pp_ret:Binary.pretty
      @@ fun () ->
      let ofs = Scalar.Binary_Forward.biconst ~size (Z.of_int offset) ctx in
      let result = Sub.bshift ~size ~offset ~max ctx aval in
      let typ = add ~size result ctx value ofs in
      (result,typ)

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
      let open TypedC.Pred in
      match expr with
      | Const c -> Min (Z.numbits c)
      | Self -> Const size
      | Val(Sym s) -> assert false (* let sz, _, _ = Type.symbol ctx s in Const sz *)
      | Binop(op, e1, e2) ->
        let sz1 = expr_size ~size ctx e1
        and sz2 = expr_size ~size ctx e2
        in eq_size sz1 sz2
      | Unop(Extract (index, len), e) ->
        let sz = expr_size ~size ctx e
        in eq_size sz (Const (len:>int))
    (* | Unop(op, e) -> *)

    let cmp_size ~size ctx expr1 expr2 =
      let size1 = expr_size ~size ctx expr1
      and size2 = expr_size ~size ctx expr2 in
      match eq_size size1 size2 with
      | Const sz | Min sz -> sz

    (* ------------------------------ Assume operations -------------------------------*)

    let assume_ptr_type ~size ctx va addr_typ =
      (va,Some addr_typ)

    (* TODO: We should only allow name types here. *)
    let assume_type ~size ctx value typ =
      let open TypedC in
      Log.trace (fun p -> p "assume_type value has type %a" pp typ ) @@ fun () ->
      let zero = Scalar.Binary_Forward.biconst ~size Z.zero ctx in
      let pointed = match typ.descr with Ptr {pointed} -> pointed | _ -> assert false in
      (* TODO: The offset and block should have corresponding structures. *)
      let open TypedC in
      let elem_size =
        try
          let ptyp = TypedC.inlined pointed in
          match ptyp.descr with
          | Array(tp,_) -> sizeof tp
          | StructureFAM _ ->
            let _, elem_size, _ = Type.to_flexible ptyp in elem_size
          | _ -> In_bytes.one
        with Types.TypedC.Undefined_type_constructor _ -> In_bytes.one
      in
      assume_ptr_type ~size ctx value
        Type.{ptyp=pointed; idx = zero; elem_size=(elem_size:>int); ofs = zero}

    let type_of ~size ctx (value,addr) =
      let binder (Type.{ptyp; idx; elem_size; ofs}) =
        if Type.index_is_zero ctx idx && Type.index_is_zero ctx ofs
        then Some Types.TypedC.(Build.ptr ptyp Pred.true_)
        else None
      in Option.bind addr binder

    let valid_function_type _ctx funtyp args = assert false

    let binary_unknown_typed ~size ctx typ =
      let ptrsize = Codex_config.ptr_size () in
      assert (size = ptrsize) ;
      let open TypedC in
      let new_unknown () = binary_unknown ~size ctx in
      let bin = match typ.descr with
        | Ptr{pointed } ->
          (match new_unknown () with | (va,None) -> va |_-> assert false)
        | _ -> assert false

      in
      assume_type ~size ctx bin typ

    let contained_addresses ~size _ = assert false

  end

  include Address

end

module Make
(Sub:Memory_sig.ADDRESS_AND_MAKE_MEMORY)
(Symb:Memory_sig.FIXED_SIZE_VALUE_DOMAIN with module Scalar = Sub.Scalar)
(* (TS : TypedC.Type_settings) *)
(* : S with module Sub = Sub and module Symb = Symb (\* and module TS = TS *\)  *)= struct

  module Sub = Sub
  module Symb = Symb
  module Scalar = Sub.Scalar

  type binary = Sub.Address.binary

  (* module Sub = Sub.Address *)
  (* module SubMemory = Sub.Memory *)



  (* module BinaryMap = Map.Make(Scalar.Binary) *)

  module Address = MakeAddressOnly(Sub.Address)(Symb)

  type address = Address.binary

  module Make_Memory
      (Block: Memory_sig.BLOCK with module Scalar = Scalar )
    : Memory_sig.MEMORY
      with module Scalar = Scalar
       and module Address = Address
       and module Block = Block
  (* and type boolean = Sub.Memory(Block)(Lift).boolean *)
  = struct
    module Address = Address
    module Block = Block
    module SubMemory = Sub.Make_Memory(Block)

    module M:Memory_sig.WITH_BOOLEAN_REDEFINITION
      with module Context = Scalar.Context
       and type boolean = SubMemory.boolean =
      SubMemory
    include M

    module Scalar = Address.Scalar
    module Value = Block.Value
    module Offset = Block.Offset

    type address = Address.binary

    type offset = Offset.offset

    module Type = Address.Type

    module Memory = SubMemory
    type memory = Memory.memory

    let pretty ctx fmt mem = SubMemory.pretty ctx fmt mem

    (* ----------- Dereferencing functions for whole memory blocks ----------- *)

    let index_is_zero ctx index =
      let ptrsize = Codex_config.ptr_size () in
      let size = ptrsize in
      match Scalar.Query.(Binary_Lattice.is_singleton ~size @@ binary ~size ctx index) with
      | Some idx -> Z.equal Z.zero idx
      | None -> false

    let has_offset_zero ctx Type.{ptyp;idx;elem_size;ofs} =
      index_is_zero ctx idx && index_is_zero ctx ofs

    let load_from_typed_block ctx mem addr_typ addr =
      assert(has_offset_zero ctx addr_typ);
      Block.block_unknown_typed ctx addr_typ.ptyp

    let load_block (ctx:Value.Context.t) mem ((va,typ) as addr) =
      let ptr_size = Codex_config.ptr_size () in
      match typ with
      | Some addr_typ ->
        Log.trace
          (fun p ->
            p "load_block addr_t:%a addr_val:%a" (Type.pp ctx)
              addr_typ
              (Address.binary_pretty ~size:ptr_size ctx)
              addr)
        (*~pp_ret:(Block.pretty ctx)*)
        @@ fun () ->
        load_from_typed_block ctx mem addr_typ addr
      | None ->
        Log.trace
          (fun p ->
            p "load_block (addr untyped) add_val:%a"
              (Address.binary_pretty ~size:ptr_size ctx)
              addr)
        (*~pp_ret:(Block.pretty ctx)*)
        @@ fun () -> Memory.load_block ctx mem va
    ;;

    let store_typed_block ctx mem addr_typ block =
      assert(has_offset_zero ctx addr_typ);
      Log.trace (fun p -> p "store_typed_block %a %a" (Memory.pretty ctx) mem (Block.pretty ctx) block)
      @@ fun _ ->
      let loaded_typ = addr_typ.ptyp in
      Block.check_type ctx loaded_typ block |> Type_check_tree.save;
      mem

    let store_block (ctx : Value.Context.t) mem (vaddr,typ) block =
      match typ with
      | Some addr_typ ->  store_typed_block ctx mem addr_typ block
      | None -> Memory.store_block ctx mem vaddr block

    let serialize ~widens ctxa mema ctxb memb acc =
      SubMemory.serialize ~widens ctxa mema ctxb memb acc

    let initial ctx size = assert false

    let malloc ~id ~malloc_size ctx mem =
      let address, mem  = Memory.malloc ~id ~malloc_size ctx mem in
      (address,None), mem

    let free ctx mem (vaddr,typ) =
      (* This seems wrong. We also need to free typed addresses. *)
      match typ with
      | None -> Memory.free ctx mem vaddr
      | Some _ -> mem

    let unknown ~level ctx = Memory.unknown ~level ctx

    let memory_empty ctx = Memory.memory_empty ctx

    let should_focus ~size (ctx:Context.t) mem ((vaddr,typ) as addr) : (address * offset) option =
      let pp_ret = fun p -> function
        | None -> Format.fprintf p "None"
        | Some (base, offset) ->
          Format.fprintf p "(%a,@;@;%a)"
            (Address.binary_pretty ~size ctx)
            base
            (Offset.offset_pretty ctx)
            offset
      in
      Log.trace (fun p ->
        p "should_focus ~size:%d %a" (size:>int)
          (Address.binary_pretty ~size ctx)
          addr) ~pp_ret
      @@ fun () ->
      if Address.Query.(Binary_Lattice.is_empty ~size @@ binary ~size ctx addr) then None
      else
        match typ with
        | Some (Type.{ptyp;idx;elem_size;ofs}) ->
          (* Construct a pair vaddr (by subtraction) and offset (by
             addition) from the offset in the type.
             TODO: Addition of negative offsets is wrong. But the focusing system should be
             revisited anyway using a notion of heap graphs, so we keep it like this for now. *)
          let zero = Offset.offset_zero ~max:None ctx in
          let vaddr = Sub.Address.bindex ~size (-elem_size) ctx vaddr idx in
          let offset = Offset.offset_index elem_size ctx zero idx in
          let vaddr,offset =
            match Scalar.Query.(Binary_Lattice.is_singleton ~size @@ binary ~size ctx ofs) with
            | Some o ->
              let off = Z.to_int o in
              Sub.Address.bshift ~size ~offset:(-off) ~max:None ctx vaddr,
              Offset.offset_shift ~offset:off ~max:None ctx offset

            | None ->
              Sub.Address.bindex ~size (-1) ctx vaddr ofs,
              Offset.offset_index 1 ctx offset ofs
          in
          (* Adding and subtracting a variable offset may not set the index to zero, so we do it explicitly here. *)
          let ptrsize = Codex_config.ptr_size () in
          let zero = Scalar.Binary_Forward.biconst ~size:ptrsize Z.zero ctx in
          let typ = Some(Type.{ptyp;idx=zero;elem_size;ofs=zero}) in
          let base = (vaddr, typ) in
          Some (base, offset)
        | None -> None


    let load ~(size:In_bits.t) ctx mem ((va,typ) as addr) : Value.binary =
      match typ with
      | None -> Memory.load ~size ctx mem va
      | Some typ -> (
        (* Decompose into base and offset. *)
        match should_focus ~size:In_bits.one ctx mem addr with
        | Some (((vaddr, Some typ) as base_addr), offset) ->
          (* let offset = convert_offset ctx typ in *)
          let block = load_from_typed_block ctx mem typ base_addr in
          Block.load ~size ctx block offset
        | _ -> assert false)


    let store ~(size:In_bits.t) ctx mem ((va,typ) as addr) value =
      match typ with
      | None -> Memory.store ~size ctx mem va value
      | Some typ -> (
        (* Decompose into base and offset. *)
        match should_focus ~size:In_bits.one ctx mem addr with
        | Some (((vaddr, Some typ) as base_addr), offset) ->
          let block = load_from_typed_block ctx mem typ base_addr in
          let block = Block.store ~size ctx block offset value in
          store_block ctx mem base_addr block
        | _ -> assert false)

    let type_may_alias ctx ~size1 ~size2 t u =
      assert (size1 > 0);
      assert (size2 > 0);
      let ptrsize = Codex_config.ptr_size () in
      try
        let x_end_t = Type.add_offset ctx Scalar.Binary_Forward.(biconst
          ~size:ptrsize (Z.of_int @@ size1 - 1) ctx) t in
        not (Type.is_in_bounds ctx x_end_t)
        || let y_end_t = Type.add_offset ctx Scalar.Binary_Forward.(biconst
          ~size:ptrsize (Z.of_int @@ size2 - 1) ctx) u in
        not (Type.is_in_bounds ctx y_end_t)
        || let Type.{ptyp = ptypx;_} = t in
        let Type.{ptyp = ptypy;_} = u in
        TypedC.contains ptypx ptypy
        || TypedC.contains ptypy ptypx

      with Type.Type_error _ ->
        true

    let may_alias ~(ptr_size:In_bits.t) (ctx:Context.t) ~(size1:offset) ~(size2:offset) ((xv,xtyp) as x:address) ((yv,ytyp) as y:address) =
      Log.debug (fun p -> p "@[<hov 2>WSD.may_alias@ ~ptr_size:%d@ ~size1:%a@ ~sizea:%a@ %a@ %a@]"
        (ptr_size:>int)
        (Offset.offset_pretty ctx) size1
        (Address.binary_pretty ~size:ptr_size ctx) x
        (Offset.offset_pretty ctx) size2
        (Address.binary_pretty ~size:ptr_size ctx) y
      );
      let res =
        match xtyp,ytyp with
        | None, _ | _, None -> assert false
        | Some xt, Some yt ->
          (*let xval = (Address.binary2scalar_binary ~size:ptr_size ctx x) in*)
          (*let yval = (Address.binary2scalar_binary ~size:ptr_size ctx y) in*)
          (* If both pointers may be null, return true *)
          match xt, yt with
          | {ptyp={descr = Weak _}}, _ | _, {ptyp = {descr = Weak _}} -> false
          | _ ->
            begin
              (
                let Type.{ptyp = ptypx;_} = xt and Type.{ptyp = ptypy;_} = yt in
                (match ptypx.descr, ptypy.descr with (_,Weak _ | Weak _,_) -> false | _ -> true)
                &&
                try
                  TypedC.contains ptypx ptypy || TypedC.contains ptypy ptypx

                with Type.Type_error _ -> true
              )
              (* Since this a product with a numeric abstraction, we use numeric
            * information as well. In practice, this will always be true, except
            * if some specific numeric predicate was retained (which I guess
            * could happen, although I don't remember in which cases). *)
              && (
                let quadri = Address.beq ~size:ptr_size ctx x y
                  |> Address.query_boolean ctx in
                match quadri with
                | Lattices.Quadrivalent.False -> false
                | Lattices.Quadrivalent.Top | Lattices.Quadrivalent.True
                  | Lattices.Quadrivalent.Bottom-> true
              )
              end
      in Log.debug (fun p -> p "WSD.may_alias returning %b" res ); res

    let is_weak ~size (ctx : Context.t) ((xv,xtyp) as addr) =
      Log.debug (fun p -> p "Typed_address.is_weak %a" (Address.binary_pretty ~size ctx) addr );
      match xtyp with
      | Some (Type.{ptyp = {descr = Weak _}; idx; elem_size; ofs}) -> true
      | _ -> false

    let addresses_in_memory ctxa mema ctxb memb =
      SubMemory.addresses_in_memory ctxa mema ctxb memb

  end

end
