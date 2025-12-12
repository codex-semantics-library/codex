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

module Log = Tracelog.Make(struct let category = "Domains.Region_separation" end);;
open Units
module BaseAddress = struct

  type t =
    (* malloc_size. TODO: Should probably be stored in the region. *)
    | Malloc of Operator.Malloc_id.t * In_bytes.t
    | Null

  let compare a b = match a, b with
    | Malloc (a,_), Malloc (b,_) -> Stdlib.compare a b
    | Null, Null -> 0
    | Malloc _, Null  -> 1
    | Null, (Malloc _ ) -> -1
  ;;

  let to_int = function
    | Null -> 0
    | Malloc (id,_) ->
      let i = Operator.Malloc_id.to_int id in
      assert(i != 0);
      i

  let pretty fmt = function
    | Null -> Format.fprintf fmt "null"
    | Malloc (id,_) -> Format.fprintf fmt "&%s" @@ Operator.Malloc_id.to_string id
  ;;

  let equal a b = (compare a b) == 0;;

  let size_of = function
    | Null ->
      begin match Codex_config.valid_absolute_addresses() with
        | None ->
          Log.fatal (fun p -> p "Trying to use an integer as an address \
without setting the range of valid absolute addresses.")
        | Some (_,max) -> (Z.to_int max) |> In_bytes.of_int
      end
    | Malloc(id,size) -> size
  ;;

end

module BaseAddressMap = struct
  include Extstdlib.Map.Make(BaseAddress)

end

module BaseAddressSet = PatriciaTree.MakeSet(BaseAddress);;


open Memory_sig

module type FIXED_SIZE_VALUE_DOMAIN = sig
  include FIXED_SIZE_VALUE_DOMAIN
  type offset
  val is_precise:  Context.t -> binary -> (BaseAddress.t * offset) precision
  val new_baddr: BaseAddress.t -> Context.t -> max:int option -> binary
end


module MakePrev(BR:OFFSET) = struct

  module Scalar = BR.Scalar
  
  module Operable_Value:sig
    include FIXED_SIZE_VALUE_DOMAIN
      with module Scalar = BR.Scalar        
      with type boolean = BR.boolean
    val fold:BR.Scalar.Context.t ->
      binary -> (BaseAddressMap.key -> BR.offset -> 'a -> 'a) -> 'a -> 'a
  end
  =
  struct
    include BR
    include Scalar              (* TMP: We should remove Domain.Minimal from the signature instead. *)
    module Context = BR.Scalar.Context

    let offset2scalar = Fun.id

    type offset = BR.offset

    (* A map from region to offset. MAYBE: add the condition for which
       the value has this offset; this could allow to gain more
       precision at a low cost. *)
    type binary = {
      null: Scalar.binary option;

      map: BR.offset BaseAddressMap.t;

      unknown: bool;

      (* We could remove this if we tested, for every operation, that we
         are not combining pointers together. This ensures that we warn
         when such a pointer is used. *)
      invalid: bool;

      id: int; (** Unique identifier for comparisons. *)
    }

    type enum = Scalar.enum

    let fresh_id =
      let counter = ref 0 in
      fun () -> incr counter; !counter

    (* let equal a b =
     *   a == b ||
     *   (a.unknown == b.unknown &&
     *    a.invalid == b.invalid &&
     *    (match a.null,b.null with
     *    | None, None -> true
     *    | Some a, Some b -> Scalar.Binary.equal a b
     *    | None, Some _ | Some _, None -> false) &&
     *    (a.map == b.map || BaseAddressMap.equal BR.equal a.map b.map))
     *    ;; *)


    (* Common pretty-printer, with or without context. *)
    let pp scalar_pretty br_pretty fmt x =
      let map_pretty fmt map =
        let first = ref true in
        Format.fprintf fmt "{{ @[<v>";
        map |> BaseAddressMap.iter (fun addr v ->
                   (if !first then first := false else Format.pp_print_cut fmt ());
                   Format.fprintf fmt "%a%a" BaseAddress.pretty addr br_pretty v);
        Format.fprintf fmt "@] }}"
      in

      begin match x.null, BaseAddressMap.is_empty x.map with
      | None, true -> Format.fprintf fmt "<bottom>"
      | Some x, true -> scalar_pretty fmt x
      | Some v, false -> Format.fprintf fmt "%a or %a"
                           scalar_pretty v
                           map_pretty x.map
      | None, false -> map_pretty fmt x.map
      end;
      (* Format.fprintf fmt "(id %d)" x.id *)
    ;;



    module Binary = struct
      type t = binary

      let pretty = pp Scalar.Binary.pretty BR.Offset.pretty
      let hash {id; _} = Hashtbl.hash id

      (* Note: If compare is used only for inserting in a Map, do we
         need to be this precise, or could we just compare the ids? *)
      let compare x y =
        match x.null, BaseAddressMap.is_empty x.map, y.null, BaseAddressMap.is_empty y.map with
        | None,true,None,true -> 0 (* bottom and bottom *)
        | None,true,_,_ -> -1
        | _,_,None,true -> 1
        | Some xb, _, Some yb, _
          when (x.map == y.map || (BaseAddressMap.is_empty x.map && BaseAddressMap.is_empty y.map))
            && x.invalid = y.invalid && x.unknown = y.unknown ->
          Scalar.Binary.compare xb yb
        | _ ->
          (* Fall back to the id field, imprecise but correct, for the
             other forms of [binary]. *)
            Stdlib.compare x.id y.id

      let equal x y = compare x y = 0
    end

    module Enum = Scalar.Enum

    (* TODO: we can see here that Query.Binary_Lattice is not used. *)
    module Query = struct
      module Boolean_Lattice = Lattices.Quadrivalent
      let convert_to_quadrivalent x = x
      let boolean = BR.Scalar.query_boolean

      module Binary_Lattice = struct

        type br_query_binarylattice_t = unit (* BR.Query.Binary_Lattice.t  *)

        type t = {
          null: Scalar.Query.Binary_Lattice.t option;
          map: br_query_binarylattice_t BaseAddressMap.t;
          invalid: bool;
          unknown: bool;
        }

        module L  = Lattices.Unimplemented.Bitvector_Lattice(struct
            type nonrec t = t
            let loc = __LOC__
          end)
        include (L:module type of L with type t := t)

        let top ~size = assert false
        let singleton ~size = assert false
        let hash _ = assert false
        let equal _ _ = assert false
        let compare _ _ = assert false
        let is_included _ _ = assert false
        let inter ~size _ _ = assert false
        let includes ~size a b = is_included b a
        let includes_or_widen ~size ~previous _ = assert false
        let widen ~size ~previous _ = assert false
        let is_bottom ~size x =
          x.null == None &&
            x.invalid = false &&
              x.unknown = false &&
                BaseAddressMap.is_empty x.map
        ;;

        let bottom ~size = {map=BaseAddressMap.empty;unknown=false;invalid=false;null=None}

        let pretty ~size fmt x = assert false
        (*   if is_bottom ~size x *)
        (*   then Format.fprintf fmt "{{  }}" *)
        (*   else *)
        (*     let smth = "" in *)
        (*     let smth = if x.invalid then (Format.fprintf fmt "invalid"; " or ") else smth in *)
        (*     let smth = if x.unknown then (Format.fprintf fmt "unknown"; " or ") else smth in *)
        (*     let smth = (match x.null with *)
        (*                 | None -> smth *)
        (*                 | Some x -> Format.fprintf fmt "%s%a" smth (Scalar.Query.Binary_Lattice.pretty ~size) x; " or ") *)
        (*     in *)
        (*     if not @@ BaseAddressMap.is_empty x.map then *)
        (*       ( let first = ref false in *)
        (*         Format.fprintf fmt "%s@[<hv 3>{{ " smth; *)
        (*         x.map |> BaseAddressMap.iter (fun addr v -> *)
        (*                      (if !first then Format.fprintf fmt ";@ " *)
        (*                       else first := true);           *)
        (*                      if (match BR.Query.binary_is_singleton ~size v with Some x when Z.equal x Z.zero -> true | _ -> false) *)
        (*                      then Format.fprintf fmt "%a@ " BaseAddress.pretty addr *)
        (*                      else Format.fprintf fmt "%a + %a@ " BaseAddress.pretty addr (BR.Query.Binary_Lattice.pretty ~size) v); *)
        (*         Format.fprintf fmt "@]}}") *)
        (* ;; *)

        let join ~size a b = assert false
          (* let f a b = match a,b with *)
          (*   | None, x | x, None -> x *)
          (*   | Some a, Some b -> Some(Scalar.Query.Binary_Lattice.join ~size a b) *)
          (* in *)
          (* { null = f a.null b.null; *)
          (*   map = BaseAddressMap.union (fun _ a b -> Some (BR.Query.Binary_Lattice.join ~size a b)) a.map b.map; *)
          (*   invalid = a.invalid || b.invalid; *)
          (*   unknown = a.unknown || b.unknown *)
          (* } *)

        let is_empty = is_bottom
      end



      let binary ~size ctx x =
        let scalar_ctx = offset2scalar ctx in
        let null = match x.null with
          | None -> None
          | Some off -> Some (Scalar.Query.binary ~size scalar_ctx off)
        in
        let map = x.map |> BaseAddressMap.map (fun off -> ()
            (* BR.Query.binary ~size ctx off *)) in
        Binary_Lattice.{null;map;invalid=x.invalid;unknown=x.unknown}

      let binary_to_ival ~signed ~size _ = assert false
      let binary_to_known_bits  ~size x =
        let exception Top in
        let open Binary_Lattice in
        try if x.invalid || x.unknown || not (BaseAddressMap.is_empty x.map) then raise Top
        else
          match x.null with
          | None -> raise Top
          | Some x -> Scalar.Query.Binary_Lattice.to_known_bits ~size x
        with Top -> Lattices.Known_Bits.top ~size
      ;;

      let binary_is_singleton ~size x =
        let open Binary_Lattice in
        if x.invalid = false && x.unknown = false && BaseAddressMap.is_empty x.map
        then match x.null with
          | None -> None
          | Some x -> Scalar.Query.Binary_Lattice.is_singleton ~size x
        else None
        ;;

      module Enum_Lattice = Scalar.Query.Enum_Lattice
      let enum = Scalar.Query.enum

    end

    let binary_pretty ~size ctx = pp (Scalar.binary_pretty ~size @@ offset2scalar ctx) (BR.offset_pretty ctx)

    let enum_pretty = Scalar.enum_pretty

    let serialize ~widens ~size ctxa a ctxb b (included, acc) =
      (* let scalar_ctxa = offset2scalar ctxa in
      let scalar_ctxb = offset2scalar ctxb in *)
      (* Codex_log.feedback "Region_separation serialize2 %a %a"
       *   (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b; *)
      let f ctx = function
        | None -> BR.offset_empty ctx
        | Some a -> a
      in
      let f' ctx = function
        | None -> Scalar.binary_empty ~size ctx
        | Some a -> a
      in
      let Context.Result(included,acc,d_map) =
        BaseAddressMap.fold_on_diff2 a.map b.map (Context.Result(included,acc,fun _ctx tup -> a.map,tup))
          (fun addr a b (Context.Result(inc,acc,d_map) as res) ->
             match a,b with
             | Some a, Some b when a == b || BR.Offset.equal a b -> res
             | None, None -> res
             | _ ->
               let a = f ctxa a and b = f ctxb b in
               let Context.Result(inc,acc,d_offset) = BR.serialize_offset ~widens ctxa a ctxb b
                (inc,acc) in
               Context.Result(inc, acc, fun ctx tup ->
                 let offset,tup = d_offset ctx tup in
                 let map,tup = d_map ctx tup in
                 BaseAddressMap.add addr offset map,tup)) in
      let Context.Result(included,acc,d_null) = match a.null,b.null with
        | None, None -> Context.Result(included, acc, (fun _ctx tup -> None,tup))
        | (Some a as res), Some b when a == b || Scalar.Binary.equal a b ->
          Context.Result(included, acc, (fun _ctx tup -> res,tup))
        | _ ->
          let Context.Result(inc,acc,d) =
            Scalar.serialize_binary ~widens ~size ctxa (f' (offset2scalar ctxa) a.null) ctxb (f' (offset2scalar ctxb) b.null)
              (included,acc)
          in Context.Result(inc,acc,fun ctx tup -> let res,tup = d ctx tup in Some res,tup)
      in
      Context.Result(included,acc,fun ctx tup ->
          let null,tup = d_null ctx tup in
          let map,tup = d_map ctx tup in
          let res = {map;null; id = fresh_id ();
                     unknown = a.unknown || b.unknown;
                     invalid = a.invalid || b.invalid} in
        (* Codex_log.feedback "OV.deserialize2 @\na:%a@\nb:%a@\nres:%a" *)
          (* (binary_pretty ~size:32 ctx) a (binary_pretty ~size:32 ctx) b (binary_pretty ~size:32 ctx) res; *)
        res,tup)
    ;;
    let serialize_binary = serialize

    let serialize_enum = Scalar.serialize_enum

    let is_precise ctx x =
      if x.unknown then Imprecise
      else match x.null with
        | Some v when BaseAddressMap.is_empty x.map ->
          let off = BR.offset_zero ~max:None ctx in
          let off = BR.offset_index 1 ctx off v in
          Singleton(BaseAddress.Null, off)
        | Some _ -> Imprecise
        | None when BaseAddressMap.is_empty x.map -> Empty
        | None -> match BaseAddressMap.is_singleton x.map with None -> Imprecise | Some x -> Singleton x
    ;;

    let fold ctx x f acc =
      let acc = match x.null with
        | None -> acc
        | Some v ->
          let off = BR.offset_zero ~max:None ctx in
          let off = BR.offset_index 1 ctx off v in
          f BaseAddress.Null off acc
      in
      BaseAddressMap.fold f x.map acc

    module Binary_Forward = struct

      let bottom = {id = fresh_id (); null = None; map = BaseAddressMap.empty; unknown = false; invalid = false }
      let buninit ~size ctx = bottom
      (* Operations on pointers. *)

      let pred =
        fun pred1 pred init ctx a b ->
        begin
          let common = BaseAddressMap.merge (fun addr a b ->
              match(a,b) with
              | None,None -> assert false
              | None, _ | _, None -> None
              | Some a, Some b -> Some (pred ctx a b)
            ) a.map b.map in
          let list = BaseAddressMap.fold (fun _ bool acc -> bool::acc) common init in
          let list = match a.null,b.null with
            | Some a, Some b -> (pred1 ctx a b)::list
            | _, _ -> list
          in
          match list with
            | [] -> BR.Scalar.boolean_empty ctx (* MAYBE: assume false of boolean_empty? *)
            | [x] -> x
            | a::b ->
              let nondet_boolean x y =
                let Context.Result(_,tup,d) =
                  BR.Scalar.serialize_boolean ctx x ctx y (true,Context.empty_tuple ())
                in
                fst @@ d ctx @@ BR.Scalar.nondet_same_context ctx tup
              in
              List.fold_left nondet_boolean a b
        end
      ;;

      (* Here, comparing different bases is forbidden. *)
      let biule ~size ctx a b =
        if a.invalid || b.invalid || a.unknown && b.unknown
        then BR.Scalar.boolean_unknown ctx
        else pred (
          fun ctx l r ->
            scalar_bool2boolean ctx @@ BR.Scalar.Binary_Forward.biule ~size (offset2scalar ctx) l r)
        BR.offset_le [] ctx a b

      let bisle ~size ctx a b =
        (* Kernel.feedback "invalid %b %b %b %b" a.invalid b.invalid a.unknown b.unknown; *)
        if a.invalid || b.invalid || a.unknown && b.unknown
        then BR.Scalar.boolean_unknown ctx
        else pred (
          fun ctx l r ->
            scalar_bool2boolean ctx @@ BR.Scalar.Binary_Forward.bisle ~size (offset2scalar ctx) l r)
          BR.offset_le [] ctx a b;;

      module Boolean_Forward = BR.Scalar.Boolean_Forward
      let beq ~size ctx a b =
        let scalar_ctx = offset2scalar ctx in
        let beq ~size = (fun ctx l r -> scalar_bool2boolean ctx @@ BR.Scalar.Binary_Forward.beq ~size scalar_ctx l r) in
        if a.invalid || b.invalid || a.unknown || b.unknown
        then (* assert false *) BR.Scalar.boolean_unknown ctx
        else begin
          let false_ = Boolean_Forward.false_ ctx in
          match a.null, BaseAddressMap.is_empty a.map, b.null, BaseAddressMap.is_empty b.map with
          | None, true, _, _ | _, _, None, true -> Boolean_Forward.false_ ctx (* XXX: empty *)
          | Some a, true, Some b, true ->
              scalar_bool2boolean ctx @@ Scalar.Binary_Forward.beq ~size scalar_ctx a b
          | None, false, None, false ->
            (match BaseAddressMap.is_singleton a.map, BaseAddressMap.is_singleton b.map
             with
             | Some (adda,offa), Some(addb,offb) ->
               if BaseAddress.equal adda addb
               then BR.offset_eq ctx offa offb
               else false_
            (* Else: We cannot rule false_ out, which happens when comparing different bases. *)
             | _ -> pred (beq ~size) BR.offset_eq [false_] ctx a b)
          | _ ->
            pred (beq ~size) BR.offset_eq [false_] ctx a b
        end
      ;;

      (* This version represent the cases where pointer addition and
         indices is done using biadd/bisub, rather than
         bshift/bindex. *)
      (* module Untyped_pointers = struct
       *
       *
       *
       *   let biadd ~size ctx (a:binary) (b:binary) =
       *     (\* Kernel.feedback "adding %a %b %a %b"
       *        (binary_pretty ~size ctx) a a.invalid (binary_pretty ~size ctx) b b.invalid; *\)
       *     (\* We remove null from b, because we don't want to add Null->a to Null->b twice. *\)
       *     let null = match a.null,b.null with
       *       | Some a, Some b -> Some(Scalar.Binary_Forward.biadd ~size ctx a b)
       *       | _,_ -> None in
       *     let add_to null map =
       *       match null with
       *       | None -> BaseAddressMap.empty
       *       | Some x -> map |> BaseAddressMap.map (fun off ->
       *           BR.Binary_Forward.biadd ~size ctx off x)
       *     in
       *     (\* Assertion is unlikely in real programs. *\)
       *     let map = BaseAddressMap.union (fun addr v1 v2 -> assert false)
       *         (add_to a.null b.map) (add_to b.null a.map)
       *     in
       *     let invalid =
       *       a.invalid || b.invalid || not @@ BaseAddressMap.inter_empty b.map a.map in
       *     (\* Kernel.feedback "result: invalid = %b" invalid; *\)
       *     { null; map;
       *       unknown = a.unknown || b.unknown;
       *       invalid }
       *   ;;
       *
       *   let bisub ~size ctx a b =
       *     let sub a b = BR.Binary_Forward.bisub ~size ctx a b in
       *     let sub_from null map f =
       *       match null with
       *       | None -> BaseAddressMap.empty
       *       | Some x -> map |> BaseAddressMap.map (fun off -> f x off)
       *     in
       *     (\* Assertion is unlikely in real programs. *\)
       *     let map = BaseAddressMap.union (fun addr v1 v2 -> assert false)
       *         (sub_from a.null b.map sub) (sub_from b.null a.map @@ fun a b -> sub b a)
       *     in
       *     let inter = BaseAddressMap.merge (fun addr a b -> match a,b with
       *         | Some a, Some b -> Some(sub a b)
       *         | _,_ -> None) a.map b.map
       *     in
       *     let null =
       *       let list = BaseAddressMap.fold (fun _addr x acc -> x::acc) inter [] in
       *       let list = match a.null, b.null  with
       *         | Some a, Some b -> (Scalar.Binary_Forward.bisub ~size ctx a b)::list
       *         | _,_ -> list in
       *       match list with
       *       | [] -> None
       *       | [x] -> Some x
       *       | list -> assert false
       *     in
       *     {null;map;unknown=a.unknown||b.unknown;invalid=a.invalid||b.invalid}
       *   ;;
       * end *)


      (* Supports both integer and pointer substraction. Always returns an integer. *)
      let bisub ~size ~flags ctx a b =
        let inter = BaseAddressMap.merge (fun addr a b -> match a,b with
            | Some a, Some b -> Some(BR.offset_sub ctx a b)
            | _,_ -> None) a.map b.map
        in
        let l = BaseAddressMap.fold (fun _ x acc -> x::acc) inter [] in
        let l = match a.null,b.null with
          | Some a, Some b -> (Scalar.Binary_Forward.bisub ~size ~flags (offset2scalar ctx) a b)::l
          | _ -> l
        in
        match l with
          | [] -> bottom
          | [x] -> {id = fresh_id (); null=Some x;map=BaseAddressMap.empty;unknown=false;invalid=false}
          | a::b -> assert false               (* nondet *)
      ;;



      let ptrsize = Codex_config.ptr_size()

      let valid ~(size:In_bits.t) _acc_type ctx (ptr:binary) =

        (* For each region, we check two things: that the pointer is
           within the bounds of the region, but also that each
           internal index of an array in the region is within bounds.

           (At least currently) the latter does imply the former, as
           it is possible to perform dummy pointer shifts (e.g. using
           cast to a structure that is too big) that are checked only
           here.  *)
        (* min <= off < max *)
        let between_min_max min off max =
          let zero = BR.offset_zero ctx ~max:None in
          assert (Z.equal min Z.zero);
          Boolean_Forward.(&&) ctx
            (BR.offset_le ctx zero off) @@
          (BR.offset_le ctx off
             (BR.offset_index 1 ctx zero @@ Scalar.Binary_Forward.biconst ~size:ptrsize max @@ offset2scalar ctx))
        in

        BaseAddressMap.fold (fun key off acc ->
            match key with
            | BaseAddress.Null -> assert false
            | BaseAddress.Malloc (id,malloc_size) ->
              let max = Z.of_int @@ (malloc_size:>int) - ((In_bits.in_bytes size):>int) in
              (* Codex_log.feedback "valid var %s %s %d %d" (Term_constructors.Malloc_id.to_string id) (Z.to_string max) malloc_size size; *)
              Boolean_Forward.(&&) ctx acc @@
              Boolean_Forward.(&&) ctx
                (between_min_max Z.zero off max)
                (BR.offset_within_bounds ~size ctx off)

              (* | BaseAddress.Unknown -> assert false *)
          ) ptr.map @@ Boolean_Forward.true_ ctx
      ;;

      let valid_ptr_arith ~size:_ _arith_type ctx _x _y = (* Scalar.Boolean_Forward.true_ ctx *) assert false

      let bshift ~size ~offset ~max ctx x =
        let scalar_ctx = offset2scalar ctx in
        (* Codex_log.debug "Region_separation.bshift offset:%d %a" offset (binary_pretty ~size ctx) x ; *)
      { null = (match x.null with
            | None -> None
            | Some x -> Some(
                if offset < 0 then
                  let k = Scalar.Binary_Forward.biconst ~size (Z.of_int (-offset)) scalar_ctx in
                  Scalar.Binary_Forward.bisub ~size
                    ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:true) scalar_ctx x k
                else
                  let k = Scalar.Binary_Forward.biconst ~size (Z.of_int offset) scalar_ctx in
                  Scalar.Binary_Forward.biadd ~size
                    ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:true) scalar_ctx x k));
        map = BaseAddressMap.map (BR.offset_shift ~offset ~max ctx) x.map;
        unknown = x.unknown;
        invalid = x.invalid;
        id = fresh_id ();
      }


      let bindex ~size k ctx x e =
        (* let scalar_ctx = offset2scalar ctx in *)
        (* Codex_log.debug "Region_separation.bindex %a %a" (binary_pretty ~size ctx) x (binary_pretty ~size ctx) e ;         *)
        let invalid = x.invalid || not (BaseAddressMap.is_empty e.map) in
        match e.null with
          | None -> { id = fresh_id (); invalid; unknown = false;
              map = BaseAddressMap.empty; null = None }
          | Some e ->
            {
              id = fresh_id ();
              null = (match x.null with
                  | None -> None
                  | Some x -> Some(
                      let scalar_ctx = offset2scalar ctx in
                      if k < 0 then
                        let k = Scalar.Binary_Forward.biconst ~size (Z.of_int (-k)) scalar_ctx in
                        let off = Scalar.Binary_Forward.bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:true ~nuw:false) scalar_ctx k e in
                        Scalar.Binary_Forward.bisub ~size ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:true) scalar_ctx x off
                      else
                        let k = Scalar.Binary_Forward.biconst ~size (Z.of_int k) scalar_ctx in
                        let off = Scalar.Binary_Forward.bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:true ~nuw:false) scalar_ctx k e in
                        Scalar.Binary_Forward.biadd ~size
                          ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:true) scalar_ctx x off));
              map = BaseAddressMap.map (fun x -> BR.offset_index k ctx x e) x.map;
              unknown = x.unknown;
              invalid = x.invalid;
            }

      (**************** Arithmetic operations. ****************)

      let biconst ~size k ctx =
        { null = Some(Scalar.Binary_Forward.biconst ~size k @@ offset2scalar ctx);
          id = fresh_id ();
          map = BaseAddressMap.empty;
          unknown = false; invalid = false }

      let bofbool ~size ctx x =
        let scalar_ctx = offset2scalar ctx in
        { null = Some(Scalar.Binary_Forward.bofbool ~size scalar_ctx (boolean2scalar_bool ctx x));
          id = fresh_id ();
          map = BaseAddressMap.empty;
          unknown = false; invalid = false }
      ;;

      let bchoose ~size cond ctx x =
        { null = Extstdlib.Option.map (Scalar.Binary_Forward.bchoose ~size cond @@ offset2scalar ctx) x.null;
          id = fresh_id ();
          map = BaseAddressMap.map (BR.offset_choose cond ctx) x.map;
          unknown = x.unknown; invalid = x.invalid }

      (* Common to arithmetical unary operations. *)
      let ar1 op1 ~size ctx x =
        let invalid = x.invalid || not @@ BaseAddressMap.is_empty x.map in
        let null = match x.null with None -> None | Some x -> Some(op1 ~size ctx x) in
        {id = fresh_id (); null;map=BaseAddressMap.empty;invalid;unknown=x.unknown}
      ;;

      let buext ~size ~oldsize ctx x =
        if size == oldsize then x
        else ar1 (Scalar.Binary_Forward.buext ~oldsize) ~size (offset2scalar ctx) x;;

      let bsext ~size ~oldsize ctx x =
        if size == oldsize then x
        else ar1 (Scalar.Binary_Forward.bsext ~oldsize) ~size (offset2scalar ctx) x;;

      let bextract ~size ~index ~oldsize ctx x =
        (* Codex_log.feedback "region_separation.bextract"; *)
        if size == oldsize then x
        else ar1 (Scalar.Binary_Forward.bextract ~index ~oldsize) ~size (offset2scalar ctx) x
      ;;

      let ar2 op1 ~size ctx a b =
        let invalid = a.invalid || b.invalid || (not @@ BaseAddressMap.is_empty a.map) || (not @@ BaseAddressMap.is_empty b.map) in
        let unknown = a.unknown || b.unknown in
        let null = match a.null, b.null with
          | Some a, Some b -> Some(op1 ~size (offset2scalar ctx) a b)
          | _ -> None
        in
        if invalid && not (a.invalid || b.invalid)
        then Codex_log.warning "invalid operation";
        {null;map=BaseAddressMap.empty;invalid;unknown; id = fresh_id ()}
      ;;


      let biadd ~size ~flags ctx a b = ar2 (Scalar.Binary_Forward.biadd ~flags) ~size ctx a b
      let bimul ~size ~flags ctx a b = ar2 (Scalar.Binary_Forward.bimul ~flags) ~size ctx a b
      let bxor ~size ctx a b = ar2 (Scalar.Binary_Forward.bxor) ~size ctx a b
      let band ~size ctx a b = ar2 (Scalar.Binary_Forward.band) ~size ctx a b
      let bor ~size ctx a b = ar2 (Scalar.Binary_Forward.bor) ~size ctx a b

      let bashr ~size ctx a b = ar2 (Scalar.Binary_Forward.bashr) ~size ctx a b
      let blshr ~size ctx a b = ar2 (Scalar.Binary_Forward.blshr) ~size ctx a b
      let bshl ~size ~flags ctx a b = ar2 (Scalar.Binary_Forward.bshl ~flags)   ~size ctx a b

      let bisdiv ~size ctx a b = ar2 (Scalar.Binary_Forward.bisdiv) ~size ctx a b
      let bismod ~size ctx a b = ar2 (Scalar.Binary_Forward.bismod) ~size ctx a b
      let biudiv ~size ctx a b = ar2 (Scalar.Binary_Forward.biudiv) ~size ctx a b
      let biumod ~size ctx a b = ar2 (Scalar.Binary_Forward.biumod) ~size ctx a b

      let bconcat ~size1 ~size2 ctx a b =
        let invalid = a.invalid || b.invalid || (not @@ BaseAddressMap.is_empty a.map) || (not @@ BaseAddressMap.is_empty b.map) in
        let unknown = a.unknown || b.unknown in
        let null = match a.null, b.null with
          | Some a, Some b -> Some(Scalar.Binary_Forward.bconcat ~size1 ~size2 (offset2scalar ctx) a b)
          | _ -> None
        in {null;map=BaseAddressMap.empty;invalid;unknown; id = fresh_id ()}
      ;;
    end

    module Enum_Forward = struct
      let caseof ~case _ = assert false
      let enum_const ~case _ = assert false
    end


    let binary_empty ~size ctx = Binary_Forward.bottom

    let enum_empty = Scalar.enum_empty

    (* An unknown (i.e. outside-constructed) value may either be a
         constant, or point to the unknown part of the memory. *)
    (* XXX: The offset for unknown is essentially useless, and we create a lot of them.
         Maybe it is important to optimize unknown, i.e. add a special "Unknown" variable. *)
    (* XXX: I should generate assumptions based on the type:
         if an integer, state that it must be in some interval, and the base is null;
         if a pointer, keep it that way. *)
    let binary_unknown ~size ctx =
      let scalar_ctx = offset2scalar ctx in
      (* BaseAddressMap.add BaseAddress.Unknown (Scalar.Binary_Forward.bunknown ~size ctx) @@ *)
      { null = Some(Scalar.binary_unknown ~size scalar_ctx);
        id = fresh_id ();
        map = BaseAddressMap.empty;
        (* XXX: si je met true, tous les entiers generes deviennent des valeurs inconnues...
             Il faudrait au moins separer bunknown_int et bunknown_ptr. *)
        unknown = false;
        invalid = false }

    let enum_unknown = Scalar.enum_unknown

    let binary_unknown_typed ~size ctx _typ = binary_unknown ~size ctx;;
    let check_type ~size _ = assert false
    let new_baddr addr ctx ~max =
      { null = None;
        id = fresh_id ();
        map = BaseAddressMap.singleton addr @@
          BR.offset_zero ~max ctx;
        invalid = false; unknown = false }

    module BaseAddress = struct
      let beq = Binary_Forward.beq
      let ble = Binary_Forward.biule
      let bshift = Binary_Forward.bshift
      let bisub ~size _ = assert false (* Binary_Forward.bisub *)
      let bisub ~size ctx a b =
        let res = Binary_Forward.bisub ~size ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ctx a b in (* TODO : change the flags here ? *)
        match res.null with
        | Some r -> r
        | None -> Scalar.binary_empty ~size @@ offset2scalar ctx
      ;;

      (* For now, I copied that from bindex. *)
      let bindex ~size k ctx x e =
        (* assert(k != 0); *)
        (* Codex_log.debug "Region_separation.bindex offset:%d %a %a" k (binary_pretty ~size ctx) x (Scalar.binary_pretty ~size ctx) e ; *)
        let invalid = x.invalid in
          {
            id = fresh_id ();
            null = (match x.null with
                | None -> None
                | Some x -> Some(
                    let scalar_ctx = offset2scalar ctx in
                    if k < 0 then
                      let k = Scalar.Binary_Forward.biconst ~size (Z.of_int (-k)) scalar_ctx in
                      let off = Scalar.Binary_Forward.bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:true ~nuw:false) scalar_ctx k e in
                      Scalar.Binary_Forward.bisub ~size
                        ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:true) scalar_ctx x off
                    else
                      let k = Scalar.Binary_Forward.biconst ~size (Z.of_int k) scalar_ctx in
                      let off = Scalar.Binary_Forward.bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:true ~nuw:false) scalar_ctx k e in
                      Scalar.Binary_Forward.biadd ~size
                        ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:true) scalar_ctx x off
                    ));
            map = BaseAddressMap.map (fun x -> BR.offset_index k ctx x e) x.map;
            unknown = x.unknown;
            invalid = invalid;
          }
      ;;

      let bchoose = Binary_Forward.bchoose
      let within_bounds ~size = Binary_Forward.valid ~size ()
      let zero_offset ~size ~max ctx = assert false
    end
    include BaseAddress
    let union cond ctx = Scalar.union cond (offset2scalar ctx)

    let binary2scalar_binary ~size ctx value =
      match value.null with
      | None -> Scalar.binary_empty ~size @@ offset2scalar ctx
      | Some v -> v

    let assume_type ~size _ctx value typ = assert false
    let type_of ~size _ctx value = None

    let analyze_summary _ctx funtyp args = assert false

    let global_symbol ctx symb = assert false
    let add_global_symbol ~size ctx name binary = assert false
    let satisfiable ctx cond = Scalar.satisfiable ctx (boolean2scalar_bool ctx cond)

    let addresses_in_binary ~size _ = assert false

  end
  module Address = Operable_Value

  module Memory
      (AddressBlock:Memory_sig.BLOCK with module Scalar = Scalar and  module Offset = BR)
      (Block: Memory_sig.BLOCK with module Scalar = Scalar and  module Value = AddressBlock.Value)
    : Memory_sig.MEMORY
      with module Scalar = Scalar
       and module Address = Operable_Value
       and module Block.Value = AddressBlock.Value
       and module Block = Block
       and type boolean = AddressBlock.boolean
  = struct
    module Address = Address

    module M:Memory_sig.WITH_BOOLEAN_REDEFINITION
      with module Context = Scalar.Context
       and type boolean = AddressBlock.boolean =
      AddressBlock
    include M

    module Scalar = BR.Scalar
    module Value = AddressBlock.Value
    type address = Operable_Value.binary

    type block = AddressBlock.block
    type offset = Block.offset

    type memory = {
      (* TODO: for recency: a pair of recent,older regions. *)
      known: block BaseAddressMap.t;
      escaped: unit (* set of bases that escaped. *);
      (* TODO: as we have regions here, I can associate to each region
         the addresses that may have been written in this region. And
         implement efficiently load(store(x)=x) *)
    }

    let pretty ctx fmt {known} = BaseAddressMap.mk_pretty BaseAddress.pretty (AddressBlock.pretty ctx) fmt known

    let memory_empty ctx = {
      known = BaseAddressMap.empty;
      escaped = ()
    }
    module Context = Scalar.Context

    let serialize_known ~widens ctxa a ctxb b (included, acc) =
      let f ctx addr = function
        | Some x -> x
        | None -> AddressBlock.initial ctx (BaseAddress.size_of addr)
      in
      (* Note: we do not reconstruct in the order in which we iterate,
         which is perfectly OK. *)
      BaseAddressMap.fold_on_diff2 a b (Context.Result(included,acc,fun _ctx tup -> a,tup))
        (fun addr a b (Context.Result(inc,acc,d_map)) ->
           let Context.Result(inc,acc,d_region) = AddressBlock.serialize ~widens ctxa (f ctxa addr a) ctxb (f ctxb addr b)
            (inc,acc) in
           Context.Result(inc,acc,fun ctx tup ->
               let region,tup = d_region ctx tup in
               let map,tup = d_map ctx tup in
               BaseAddressMap.add addr region map,tup)
        )
    ;;

    let serialize ~widens ctxa mema ctxb memb acc =
      let Context.Result(included,acc,d_known) = serialize_known ~widens ctxa mema.known ctxb memb.known acc in
      Context.Result(included, acc, fun ctx tup ->
        let known,tup = d_known ctx tup in
        {known;escaped=()},tup)

    let join_values ~size ctx v1 v2 =
      let Context.Result(_,tup,deserialize2) = Value.serialize ~widens:false ~size ctx v1 ctx v2
        (true, Context.empty_tuple ()) in
      let res_tup = Scalar.nondet_same_context ctx tup in
      fst @@ deserialize2 ctx res_tup
    ;;

    let load ~size ctx mem at =
      let r = Operable_Value.fold ctx at (fun addr offset acc ->
          (* Codex_log.feedback "load addr %a"  (Operable_Value.binary_pretty ~size ctx) at; *)
          let v =
            match BaseAddressMap.find addr mem.known with
            | exception Not_found -> Value.binary_empty ~size ctx
            | region -> AddressBlock.load ~size ctx region offset
          in v::acc
        ) [] in
      match r with
      | [] -> Value.binary_empty ~size ctx
      | [hd] -> hd  (* Most common case *)
      | hd::tl -> List.fold_left (join_values ~size ctx) hd tl
    ;;

    let _load ~size ctx mem at =
      let res = load ~size ctx mem at in
      (* Codex_log.feedback "load result: %a" (Value.binary_pretty ~size ctx) res; *)
      Log.debug (fun p -> p "load result: %a" (Value.binary_pretty ~size ctx) res);
      res
    ;;

    let typed_load ~size ctx mem at typ =
      Log.debug (fun p -> p "Region_separation.typed_load of type %a" Types.TypedC.pp typ);
      let r = Operable_Value.fold ctx at (fun addr offset acc ->
          (* Codex_log.feedback "load addr %a"  (Operable_Value.binary_pretty ~size ctx) at; *)
          let v =
            match BaseAddressMap.find addr mem.known with
            | exception Not_found -> Value.binary_unknown_typed ~size ctx typ
            | region -> AddressBlock.load ~size ctx region offset
          in v::acc
        ) [] in
      match r with
      | [] -> Value.binary_empty ~size ctx
      | [hd] -> hd  (* Most common case *)
      | hd::tl -> List.fold_left (join_values ~size ctx) hd tl
    ;;

    let _typed_load ~size ctx mem at typ =
      let res = load ~size ctx mem at in
      Codex_log.feedback "load result: %a" (Value.binary_pretty ~size ctx) res;
      res

    let store_single ~size ctx mem addr offset value =
        let region =
          try BaseAddressMap.find addr mem.known
          with Not_found -> AddressBlock.initial ctx (BaseAddress.size_of addr)
        in
        let region = AddressBlock.store ~size ctx region offset value in
        let result = { mem with known = BaseAddressMap.add addr region mem.known } in
        result
    ;;

    let join_memories ctx v1 v2 =
      let Context.Result(_,tup,deserialize2) = serialize ~widens:false ctx v1 ctx v2
        (true,Context.empty_tuple ()) in
      let res_tup = Scalar.nondet_same_context ctx tup in
      fst @@ deserialize2 ctx res_tup
    ;;


    let store ~(size:In_bits.t) ctx mem at value =
      (* Simple way: we perform different writes, and join the
         whole memory. We could be joining the regions instead,
         but it's probably not worth it. *)
      let r = Operable_Value.fold ctx at (fun addr offset acc ->
          try
            let v = store_single ~size ctx mem addr offset value in
            v::acc
          with Sig.Memory_Empty -> acc
        ) [] in
      match r with
       | [] -> (* memory_empty ctx *) raise Sig.Memory_Empty
       | [hd] -> hd         (*  Most common case. *)
       | hd::tl -> List.fold_left (join_memories ctx) hd tl
    ;;

    let store ~size ctx mem at value =
      (* Codex_log.feedback "before store value %a at: %a " (pretty ctx) mem (Operable_Value.binary_pretty ~size:32 ctx) at; *)
      let res = store ~size ctx mem at value in
      (* Codex_log.feedback "storing result %a" (pretty ctx) res; *)
      res
    ;;


    let typed_store_single ~size (ctx:Context.t) mem addr offset typ value =
      try
        let region = BaseAddressMap.find addr mem.known in
        let region = AddressBlock.store ~size ctx region offset value in
        let result = { mem with known = BaseAddressMap.add addr region mem.known } in
        result

      with Not_found ->
        Value.check_type ~size ctx typ value |> Types.Type_check_tree.save ;
        (* This should not be done here *)
        mem

    let typed_store ~size ctx mem at typ value =
      let r = Operable_Value.fold ctx at (fun addr offset acc ->
        try
          let v = typed_store_single ~size ctx mem addr offset typ value in
          v::acc
        with Sig.Memory_Empty -> acc
      ) [] in
    match r with
     | [] -> (* memory_empty ctx *) raise Sig.Memory_Empty
     | [hd] -> hd         (*  Most common case. *)
     | hd::tl -> List.fold_left (join_memories ctx) hd tl
    ;;


    let malloc ~id ~malloc_size ctx mem =
      let addr = BaseAddress.Malloc (id,malloc_size) in
      (if BaseAddressMap.mem addr mem.known
       then Codex_log.fatal "Malloc in a loop is not yet supported");
      let mem' = {
        known = BaseAddressMap.add addr (AddressBlock.initial ctx malloc_size) mem.known;
        escaped = mem.escaped;
      }
      in
      Operable_Value.new_baddr addr ctx ~max:(Some (malloc_size:>int)), mem'
    ;;

    let free ctx mem ptr =

      match Operable_Value.is_precise ctx ptr with
      | Empty -> mem
      | Imprecise -> Codex_log.fatal "Can only handle precise frees for now"
      | Singleton(addr,_) -> begin
          (if not @@ BaseAddressMap.mem addr mem.known
           then Codex_log.fatal "Free on an object not previously allocated (%a)"
               BaseAddress.pretty addr);
          {
            known = BaseAddressMap.remove addr mem.known;
            escaped = mem.escaped
          }
        end
    ;;

    let unknown ~level ctx = {known = BaseAddressMap.empty; escaped = () }

    (* We should focus if not a singleton. *)
    let should_focus ~size:_ ctx mem addr =
      match Operable_Value.is_precise ctx addr with
      | Empty | Singleton _ -> None
      | Imprecise -> assert false


    let may_alias ~ptr_size:_ _ctx ~size1:_ ~size2:_ (_addr1 : address) (_addr2 : address) =
      assert false
      (*addr1,addr2*)

    let is_weak ~size _ctx value = assert false

    let initial _ = assert false

    let addresses_in_memory ctxa mema ctxb memb : (Value.binary * Value.binary) list =
      let a = mema.known in
      let b = memb.known in
      BaseAddressMap.fold2 a b []
        (fun addr a b lst ->
           match a,b with
           | Some x, Some y -> (AddressBlock.addresses_in_block ctxa x ctxb y) @ lst
           | _ -> lst
        )
    ;;

    module Block = Block

    let load_block _ = assert false

    let store_block _ = assert false
  end
end

(* Functor to help the instanciation. *)
module Make
    (Sub:Memory_sig.OFFSET_AND_MAKE_BLOCK) :
  Memory_sig.WHOLE_MEMORY_DOMAIN
  with module Scalar = Sub.Scalar =
struct

  module M = MakePrev(Sub.Offset)
  module Address = M.Operable_Value
  module Scalar = Sub.Offset.Scalar
  type address = Address.binary

  module Make_Memory
      (Block:Memory_sig.BLOCK with module Scalar = Scalar)
    :Memory_sig.MEMORY
      with module Scalar = Block.Scalar
       and module Address = Address
       and module Block = Block
       and type boolean = Sub.Make_Block(Block.Value).boolean
  = struct
    module SubMemory = Sub.Make_Block(Block.Value)
    include M.Memory(SubMemory)(Block)
  end
end
