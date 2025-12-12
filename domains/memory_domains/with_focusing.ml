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


module Log = Tracelog.Make(struct let category = "Domains.With_focusing" end);;
module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree
module In_bits = Units.In_bits

module type BASE_WITH_TYPES = sig
  include Memory_sig.Base

  val flush_cache : Context.t -> memory -> memory
end

module type S = sig
  include Memory_sig.Base
end

module type S_with_types = sig
  include BASE_WITH_TYPES

  val analyze_summary : Context.t -> TypedC.typ -> (In_bits.t * binary) list -> memory -> bool * (In_bits.t * binary) option * memory
  val serialize_memory_and_cache : 'a. widens:bool -> Context.t -> memory -> Context.t -> memory -> (binary * binary) list -> 'a Context.in_acc -> (memory, 'a) Context.result
end

module Make (D : Memory_sig.Base)  = struct

  let name() = "With_focusing(" ^ (D.name()) ^ ")";;
  let unique_id() = Sig.Fresh_id.fresh @@ name();;

  (** TODO: An issue with this domain is that the biadd flags are lost when converted
      to offset. Either add them back here, or use Offset only for addresses. *)
  type binary =
    | Const of Z.t
    | Offset of D.binary * Z.t
        (** Addition of a subdomain value and a precisely known number. *)

  module Binary = struct
    type t = binary

    let compare x y =
      match x,y with
      | Const i, Const j -> Z.compare i j
      | Offset (b1, o1), Offset (b2, o2) ->
          let c = D.Binary.compare b1 b2 in
          if c <> 0 then c else Z.compare o1 o2
      | Const _, Offset _ -> -1
      | Offset _, Const _ -> 1

    let equal x y =
      match x,y with
      | Const i, Const j -> Z.equal i j
      | Offset (b1, o1), Offset (b2, o2) ->
          D.Binary.equal b1 b2 && Z.equal o1 o2
      | Const _, Offset _ | Offset _, Const _ -> false

    let hash = function
      | Const z -> Z.hash z
      | Offset(b,o) -> Hashing.hash2 (D.Binary.hash b) (Z.hash o)

    let to_sub ~size ctx = function
    | Const c ->
        D.Binary_Forward.biconst ~size c ctx
    | Offset (b, c) ->
        Log.debug (fun p -> p "before to_sub: Offset (%a, %s)" (D.binary_pretty ~size ctx) b (Z.format "%X" c));
        let res = if Z.equal c Z.zero then b else
          D.Binary_Forward.(bshift ~size ~offset:(Z.to_int c) ~max:None ctx b)
        in
        Log.debug (fun p -> p "after to_sub: %a" (D.binary_pretty ~size ctx) res);
        res

    let of_sub ~size:_ _ctx b =
      (* XXX: optimize const case *)
      Offset (b, Z.zero)

    let pretty fmt x =
      let open Format in
      match x with
      | Const i -> fprintf fmt "{0x%s}" @@ Z.format "%X" i
      | Offset (base, ofs) ->
          if Z.equal ofs Z.zero then
            D.Binary.pretty fmt base
          else
            fprintf fmt "@[<hov 2>%a + 0x%s@]" D.Binary.pretty base
            (Z.format "%X" ofs)
  end

  module Bin_map = Extstdlib.Map.Make(D.Binary)
  type cache = D.block Bin_map.t

  module Types = struct
    type nonrec binary = binary

    type memory =
      { sub_mem : D.memory; cache : cache }

    type boolean = D.boolean
    type enum = D.enum

    type block = D.block
    type offset = D.offset
  end
  include (Types : module type of Types with type binary := binary)

  module Boolean = D.Boolean
  module Enum = D.Enum

  let binary_pretty ~size ctx fmt x =
    let open Format in
    match x with
    | Const i -> fprintf fmt "{0x%s}" @@ Z.format "%X" i
    | Offset (base, ofs) ->
        if Z.equal ofs Z.zero then
          D.binary_pretty ~size ctx fmt base
        else
          fprintf fmt "@[<hov 2>%a + 0x%s@]" (D.binary_pretty ~size ctx) base
            (Z.format "%X" ofs)

  let boolean_pretty = D.boolean_pretty
  let enum_pretty = D.enum_pretty

  module Context = D.Context

  let root_context = D.root_context
  let context_pretty = D.context_pretty

  let mu_context_open = D.mu_context_open

  let assume = D.assume

  module Block_Forward = struct
    let ptr_size = Codex_config.ptr_size() ;;

    let sizeof = D.Block_Forward.sizeof

    let concat = D.Block_Forward.concat

    let load ~size ctx arr index =
      let res = D.Block_Forward.load ~size ctx arr index in
      Binary.of_sub ~size ctx res

    let store ~size ctx arr index value =
      let value = Binary.to_sub ~size ctx value in
      D.Block_Forward.store ~size ctx arr index value

    let binary_to_block ~size ctx a =
      D.Block_Forward.binary_to_block ~size ctx @@ Binary.to_sub ~size ctx a
  end

  module Boolean_Forward = D.Boolean_Forward
  module Enum_Forward = D.Enum_Forward

  let boolean_unknown = D.boolean_unknown
  let enum_unknown = D.enum_unknown

  let [@inline always] ar0 ~size f = fun ctx ->
    Binary.of_sub ~size ctx @@ f ctx
  let [@inline always] ar1 ~size f = fun ctx a ->
    Binary.of_sub ~size ctx @@ f ctx @@ Binary.to_sub ~size ctx a
  let [@inline always] ar2 ~size f = fun ctx a b ->
    Binary.of_sub ~size ctx @@ f ctx
      (Binary.to_sub ~size ctx a) (Binary.to_sub ~size ctx b)
  let [@inline always] pred2 ~size f = fun ctx a b ->
    f ctx (Binary.to_sub ~size ctx a) (Binary.to_sub ~size ctx b)
  let [@inline always] _ar3 f = fun ctx a b c -> match a,b,c with
    | Some a, Some b,  Some c -> Some (f ctx a b c)
    | _ -> None

  (**************** Pretty printing ****************)

  let memory_pretty ctx pp mem =
    let open Format in
    fprintf pp "@[<hov 2>{ sub_mem =@ %a;@ cache =@ %a@ }@]"
      (D.memory_pretty ctx) mem.sub_mem
      (Bin_map.mk_pretty
        (D.binary_pretty ~size:(Codex_config.ptr_size ()) ctx)
        (fun pp region -> fprintf pp "@[<hov 2><[[%a]]>@]" (D.block_pretty ctx) region)
      ) mem.cache

  let block_pretty = D.block_pretty
  let offset_pretty = D.offset_pretty


  module Binary_Forward = struct
    let beq   ~size ctx a b = pred2 ~size (D.Binary_Forward.beq   ~size) ctx a b
    let biule ~size = pred2 ~size (D.Binary_Forward.biule ~size)
    let bisle ~size = pred2 ~size (D.Binary_Forward.bisle ~size)

    let const_add ~(size:In_bits.t) x y = Z.erem (Z.add x y) @@ Z.shift_left Z.one (size:>int)
    let const_sub ~(size:In_bits.t) x y = Z.erem (Z.sub x y) @@ Z.shift_left Z.one (size:>int)
    let const_mul ~(size:In_bits.t) x y = Z.erem (Z.mul x y) @@ Z.shift_left Z.one (size:>int)

    let biadd ~size ~flags ctx x y =
      match x,y with
      | Const i, Const j -> Const (const_add ~size i j)
      (* | Offset (b,ofs), Const i | Const i, Offset (b,ofs) -> Offset (b, const_add ~size ofs i) *)
      | Offset (b,ofs), Const i | Const i, Offset (b,ofs) ->
          let offset = Z.signed_extract (const_add ~size ofs i) 0 (size:>int) in
          let one = D.Binary_Forward.biconst ~size Z.one ctx in
          Offset (D.Binary_Forward.bindex ~size (Z.to_int offset) ctx b one, Z.zero)
      | Offset _, Offset _ ->
          (* Let's not try to simplify this. The case happened where it
             prevented simplifications in the subdomain. The DBA was:
             b := a - 1
             c := b + 4*b
             We expected the subdomain to simplify c into 5*b, but with the
             rewrite below uncommented, the subdomain would see:
             c := Offset (a + 4*b, -1)
             which it was unable to simplify.
          *)
          (*Offset(D.Binary_Forward.biadd ~size ~nsw ~nuw ctx b1 b2, const_add ~size o1 o2)*)
          ar2 ~size (D.Binary_Forward.biadd ~size ~flags) ctx x y

    let bisub ~size ~flags ctx x y =
      match x,y with
      | Const i, Const j -> Const (const_sub ~size i j)
      (* | Offset (b,ofs), Const i -> Offset (b, const_sub ~size ofs i) *)
      | Offset (b,ofs), Const i ->
          let offset = Z.signed_extract (const_sub ~size ofs i) 0 (size:>int) in
          let one = D.Binary_Forward.biconst ~size Z.one ctx in
          Offset (D.Binary_Forward.bindex ~size (Z.to_int offset) ctx b one, Z.zero)
      | Const _, Offset (_,_) ->
          (* Let's just give up on this. *)
          Offset (D.Binary_Forward.bisub ~size ~flags ctx
              (Binary.to_sub ~size ctx x) (Binary.to_sub ~size ctx y),
            Z.zero)
      | Offset (b1,o1), Offset (b2,o2) ->
          (* This is weird. *)
          Offset(D.Binary_Forward.bisub ~size ~flags ctx b1 b2, const_sub ~size o1 o2)

    let bimul ~size ~flags ctx x y =
      match x,y with
      | Const i, Const j -> Const (const_mul ~size i j)
      | _ -> ar2 ~size (D.Binary_Forward.bimul ~size ~flags) ctx x y

    let bxor ~size = ar2 ~size (D.Binary_Forward.bxor ~size)
    let band ~size = ar2 ~size (D.Binary_Forward.band ~size)
    let bor ~size = ar2 ~size (D.Binary_Forward.bor ~size)

    let bsext ~size ~oldsize ctx x =
      D.Binary_Forward.bsext ~size ~oldsize ctx (Binary.to_sub ~size:oldsize ctx x)
      |> Binary.of_sub ~size ctx

    let buext ~size ~oldsize ctx x =
      D.Binary_Forward.buext ~size ~oldsize ctx (Binary.to_sub ~size:oldsize ctx x)
      |> Binary.of_sub ~size ctx

    let bofbool ~size ctx bool =
      D.Binary_Forward.bofbool ~size ctx bool |> Binary.of_sub ~size ctx

    let bashr ~size = ar2 ~size (D.Binary_Forward.bashr ~size)
    let blshr ~size = ar2 ~size (D.Binary_Forward.blshr ~size)
    let bshl ~size ~flags = ar2 ~size (D.Binary_Forward.bshl ~size ~flags)
    let bisdiv ~size = ar2 ~size (D.Binary_Forward.bisdiv ~size)
    let biudiv ~size = ar2 ~size (D.Binary_Forward.biudiv ~size)
    let bconcat ~size1 ~size2 ctx x1 x2 =
      Binary.(of_sub ~size:In_bits.(size1+size2) ctx @@ D.Binary_Forward.bconcat ctx
      ~size1 ~size2 (to_sub ctx ~size:size1 x1) (to_sub ctx ~size:size2 x2))
    let bismod ~size = ar2 ~size (D.Binary_Forward.bismod ~size)
    let biumod ~size = ar2 ~size (D.Binary_Forward.biumod ~size)

    let bextract ~size ~index ~oldsize ctx x =
      D.Binary_Forward.bextract ~size ~index ~oldsize ctx (Binary.to_sub ~size:oldsize ctx x)
      |> Binary.of_sub ~size ctx

    let valid ~size access_type ctx x =
      Log.trace (fun p -> p "valid %a" (binary_pretty ~size ctx) x)
      (*~pp_ret:(boolean_pretty ctx)*)
      @@ fun () ->
      D.Binary_Forward.valid ~size access_type ctx @@ Binary.to_sub ~size:((Codex_config.ptr_size())) ctx x

    let valid_ptr_arith ~size arith_typ ctx x y =
      D.Binary_Forward.valid_ptr_arith ~size arith_typ ctx (Binary.to_sub ~size ctx x)
        (Binary.to_sub ~size ctx y)

    let biconst ~size:_ k _ctx = Const k

    let buninit ~size = ar0 ~size @@ D.Binary_Forward.buninit ~size

    let bshift ~size ~offset ~max ctx x =
      match x with
      (* | Offset (b,ofs) -> Offset (b, const_add ~size ofs (Z.of_int offset)) *)
      | Offset (b,ofs) ->
          let new_ofs = Z.signed_extract (const_add ~size ofs (Z.of_int offset)) 0 (size:>int) in
          Offset (D.Binary_Forward.bshift ~size ~offset:(Z.to_int new_ofs) ~max ctx b, Z.zero)
      | _ -> ar1 ~size (D.Binary_Forward.bshift ~size ~offset ~max) ctx x

    let bindex ~size k ctx x idx =
      assert(k != 0);
      match x,idx with
      | Offset (b,ofs), Const i ->
        (* Codex_log.feedback "ofs %s k %d i %s\n" (Z.to_string ofs) k (Z.to_string i); *)
        let new_ofs = Z.signed_extract (const_add ~size ofs @@ const_mul ~size (Z.of_int k) i) 0 (size:>int) in
        Offset (b, new_ofs)
      | _ -> ar2 ~size (D.Binary_Forward.bindex ~size k) ctx x idx

    let bindex ~size k ctx x idx =
      Log.trace (fun p -> p "bindex %d %a %a" k (binary_pretty ~size ctx) x (binary_pretty ~size ctx) idx)
      (*~pp_ret:(binary_pretty ~size ctx)*)
      (fun () -> bindex ~size k ctx x idx)

    let bchoose ~size:_ _ = assert false
  end

  let binary_unknown ~size =
    ar0 ~size @@ D.binary_unknown ~size

  let block_unknown _ = assert false

  module Memory_Forward = struct
    (* For (probably) unused [free], [malloc] and [memcpy]. *)

    let malloc ~id ~malloc_size ctx mem =
      let a, sub_mem = D.Memory_Forward.malloc ~id ~malloc_size ctx mem.sub_mem in
      Binary.of_sub ~size:(Codex_config.ptr_size ()) ctx a, { mem with sub_mem }

    let free ctx mem a =
      let sub_mem = D.Memory_Forward.free ctx mem.sub_mem @@ Binary.to_sub
        ~size:(Codex_config.ptr_size ()) ctx a in
      { mem with sub_mem }

    let memcpy ~size:_ _ctx mem a1 a2 =
      assert false

    (* let unfocus ctx base region mem =
      let ptr_size = Codex_config.ptr_size () in
      Codex_log.debug ~level:3 "Aliasing region found. Unfocusing. @[<v 2> base = \
        %a" (D.binary_pretty ~size:ptr_size ctx) base;
      let size_reg = Region.get_size region in
      let to_store = Region.load ~size:size_reg
          ~extract:(fun b ~idx ~size ~oldsize ->
            Binary_op.lift (Binary_Forward.bextract ~size ~index:idx
            ~oldsize ctx) b) region Z.zero
      in
      let sub_mem = List.fold_left (fun sub_mem (size_v, ofs, v) ->
        match v with
        | Binary_op.Unfetched -> sub_mem
        | Binary_op.B v ->
          let v = Binary.to_sub ~size:size_v ctx v in
          Codex_log.debug ~level:3 "@[<hov 2>Storing at ofs %d w/ size %d value: %a@]" ofs size_v (D.binary_pretty ~size:size_v ctx) v;
          assert (ofs mod 8 = 0);
          let ofs_bytes = Z.(ediv (of_int ofs) (of_int 8)) in
          let addr = if Z.equal ofs_bytes Z.zero then base else
            D.Binary_Forward.(biadd ~size:ptr_size ~nsw:true ~nuw:true ctx base @@
          (* We know that there will be no overflow, all stores in a focused
           * region are guarded by alarms. So we can take it for granted
           * and set the [nsw] and [nuw] flags. This avoids some incorrect
           * writes to address zero, which can generate horrible
           * fixpoint-finding bugs.  *)
            biconst ~size:ptr_size ofs_bytes ctx) in
          D.Memory_Forward.store ~size:size_v ctx sub_mem addr v
      ) mem.sub_mem to_store in
      { sub_mem;
        cache = Bin_map.remove base mem.cache;
      } *)

    let unfocus ctx base region mem =
      let ptr_size = Codex_config.ptr_size () in
      Log.debug (fun p -> p "Aliasing region found. Unfocusing. @[<v 2> base = %a]@"
        (D.binary_pretty ~size:ptr_size ctx) base);

      (* We know that there will be no overflow, all stores in a focused
        * region are guarded by alarms. So we can take it for granted
        * and set the [nsw] and [nuw] flags. This avoids some incorrect
        * writes to address zero, which can generate horrible
        * fixpoint-finding bugs.  *)
      (* let size = D.Block_Forward.sizeof ctx region in *)
      let sub_mem = D.Memory_Forward.store_block ctx mem.sub_mem base region
      in { sub_mem; cache = Bin_map.remove base mem.cache; }


    (* Like [find_first_opt] from {!Extstdlib.Map.S} but the predicate also
     * takes a value in argument. *)
    (* let find_first_opt'
      : (Bin_map.key -> 'a -> bool) -> 'a Bin_map.t -> (Bin_map.key * 'a) option
      = fun pred map ->
        Bin_map.find_first_opt (fun k -> pred k @@ Bin_map.find k map) map *)

    (* Focus a region, after unfocusing any aliased one, if any. *)
    let do_focus ctx focus_base focus_size mem =
      let ptr_size = Codex_config.ptr_size () in
      Log.debug (fun p -> p "focusing. @[<v 2>base = %a@.focus size = %a@]"
        (D.binary_pretty ~size:ptr_size ctx) focus_base (D.offset_pretty ctx) focus_size);
      (* We must unfocus the currently focused
       * instance (if any) and focus this one. *)
      (* XXX: this is unsound: we would need to test aliasing of regions,
       * not individual memory cells. *)
      let pred other_addr region =
        let region_size = D.Block_Forward.sizeof ctx region in
        D.may_alias ~ptr_size ctx ~size1:region_size ~size2:focus_size other_addr focus_base in

      let filtered = Bin_map.filter (fun bin region -> pred bin region) mem.cache in
      let mem_unfocused = Bin_map.fold (fun other_b region mem -> unfocus ctx other_b region mem) filtered mem in
      (* let region = D.block_unknown ctx in *)
      mem_unfocused


    let index_le ctx x y =
      let res = D.offset_le ctx x y in
      match D.query_boolean ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false
    ;;

    let index_eq ctx x y =
      let res = D.offset_eq ctx x y in
      match D.query_boolean ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false
    ;;


    let load ~(size:In_bits.t) ctx mem address =
      let ptr_size = Codex_config.ptr_size () in
      (* Codex_log.debug ~level:3 "@[<v 2>With_focusing.load ~size:%d@.address %a" size (binary_pretty ~size:ptr_size ctx) address; *)
      Log.trace (fun p -> p "load ~size:%d add@.ress %a" (size:>int) (binary_pretty ~size:ptr_size ctx) address)
      (*~pp_ret:(fun p (bin,mem) -> Format.fprintf p "(%a,%a)" (binary_pretty ~size ctx) bin (memory_pretty ctx) mem)*)
      @@ fun () ->
      let address_sub = Binary.to_sub ~size:ptr_size ctx address in
      Log.debug (fun p -> p "address_sub = %a" (D.binary_pretty ~size:ptr_size ctx) address_sub);
      (* Is the address focused? *)
      match address with
      | Const _ ->
          let res, sub_mem = D.Memory_Forward.load ~size ctx mem.sub_mem address_sub in
          Binary.of_sub ~size ctx res, { mem with sub_mem }
      | Offset (b, c) ->
          begin match D.should_focus ~size:ptr_size ctx mem.sub_mem b with
          | Some (focus_base, base_ofs_in_focus) ->
            let ofs_in_focus = D.offset_shift ~offset:(Z.to_int c) ~max:None ctx base_ofs_in_focus in
            (* let region_end = D.offset_shift ~size:ptr_size ~offset:(size/8) ~max:None ctx ofs_in_focus in
            if not @@ index_le ctx region_end focus_size then (
              Codex_log.alarm "array_offset_access" ;
              Codex_log.error "@[<hov 2>Out-of-bounds access at index %a in block of size %a.@]"
                (D.offset_pretty ~size:ptr_size ctx) region_end (D.offset_pretty ~size:ptr_size ctx) focus_size ;
              raise Sig.Memory_Empty
            ) ; *)
            if Bin_map.mem focus_base mem.cache then begin
              Log.debug (fun p -> p "@[<v 2>focused.@.base = %a@.offset = %a@]" (D.binary_pretty ~size:ptr_size ctx) focus_base (D.offset_pretty ctx) ofs_in_focus);
              let region = Bin_map.find focus_base mem.cache in
              let result = D.Block_Forward.load ~size ctx region ofs_in_focus in
              let result = Binary.of_sub ~size ctx result in
              result, mem
            end
            else begin
              Log.debug (fun p -> p "Not focused, but focusable.");
              (* TODO : the do_focus should be done before the load !! *)
              (* let focus_size = Stdlib.max focus_size size in *)
              let region, sub_mem = D.Memory_Forward.load_block ctx mem.sub_mem focus_base in
              let focus_size = D.Block_Forward.sizeof ctx region in
              let mem_unfocused = do_focus ctx focus_base focus_size {mem with sub_mem} in
              (* Load the required value from the subdomain's memory. *)

              Log.debug (fun p -> p "in load, region = %a" (D.block_pretty ctx) region) ;
              let result = D.Block_Forward.load ~size ctx region ofs_in_focus in
              let result = Binary.of_sub ~size ctx result in
              (* Now update the cache with this new information. *)
              let cache = Bin_map.add focus_base region mem_unfocused.cache in
              result, {mem_unfocused with cache}
            end
          | None ->
              let res, sub_mem =
                D.Memory_Forward.load ~size ctx mem.sub_mem address_sub in
              Binary.of_sub ~size ctx res, {mem with sub_mem}
          end

    let store ~(size:In_bits.t) ctx mem address value =
      let ptr_size = Codex_config.ptr_size () in
      Log.trace (fun p -> p "@[<v 2>store ~size:%d@.address %a@.value %a" (size:>int) (binary_pretty ~size:ptr_size ctx) address (binary_pretty ~size ctx) value)
      @@ fun () ->
      (* If the address is already focused? *)
      match address with
      | Offset (b, c) ->
        (* Codex_log.debug "in With_focusing.store, checking should focus with address %a" (binary_pretty ~size ctx) address ;         *)
        begin match D.should_focus ~size:ptr_size ctx mem.sub_mem b with
        | Some (focus_base, base_ofs_in_focus) ->
          let ofs_in_focus = D.offset_shift ~offset:(Z.to_int c) ~max:None ctx base_ofs_in_focus in
          (* let region_end = D.offset_shift ~size:ptr_size ~offset:(size/8) ~max:None ctx ofs_in_focus in
          Log.debug (fun p -> p "in With_focusing.store, ofs_in_focus <= focus_size = %b" (index_le ctx ofs_in_focus focus_size)) ;
          if not @@ index_le ctx region_end focus_size then (
            Codex_log.alarm "array_offset_access" ;
            Codex_log.error "@[<hov 2>Out-of-bounds access at index %a in block of size %a.@]"
              (D.offset_pretty ~size:ptr_size ctx) region_end (D.offset_pretty ~size:ptr_size ctx) focus_size ;
            raise Sig.Memory_Empty
          ) ; *)
          begin try
            let region = Bin_map.find focus_base mem.cache in
            Log.debug (fun p -> p "@[<v 2>focused.@.base = %a@.offset = %a@]"
              (D.binary_pretty ~size:ptr_size ctx) focus_base (D.offset_pretty ctx) ofs_in_focus);

            let value = Binary.to_sub ~size ctx value in
            let new_region = D.Block_Forward.store ~size ctx region ofs_in_focus value in
            let new_cache = Bin_map.add focus_base new_region mem.cache in
            (* No mutation of the cache field here, but a functional update *)
            { mem with cache = new_cache }

          with Not_found ->
            Log.debug (fun p -> p "Not focused, but focusable.");
            (* let focus_size = Stdlib.max focus_size size in *)
            let region, sub_mem = D.Memory_Forward.load_block ctx mem.sub_mem focus_base in
            let focus_size = D.Block_Forward.sizeof ctx region in
            let mem_unfocused = do_focus ctx focus_base focus_size {mem with sub_mem} in
            Log.debug (fun p -> p "in store, region = %a" (D.block_pretty ctx) region) ;
            let value = Binary.to_sub ~size ctx value in
            let new_region = D.Block_Forward.store ~size ctx region ofs_in_focus value in
            { mem_unfocused with cache = Bin_map.add focus_base new_region mem.cache }
          end
        | None ->
          { mem with sub_mem = D.Memory_Forward.store ~size ctx
              mem.sub_mem (Binary.to_sub ctx ~size:(Codex_config.ptr_size ()) address)
              (Binary.to_sub ~size ctx value) }
        end
      | Const _ ->
          { mem with sub_mem = D.Memory_Forward.store ~size ctx mem.sub_mem
              (Binary.to_sub ctx ~size:(Codex_config.ptr_size ()) address)
              (Binary.to_sub ~size ctx value) }


    let load_block ctx mem address =
      let ptr_size = Codex_config.ptr_size () in
      Log.debug (fun p -> p "<v 2>load_block add@.ress %a" (binary_pretty ~size:ptr_size ctx) address);
      let address_sub = Binary.to_sub ~size:ptr_size ctx address in
      Log.debug (fun p -> p "address_sub = %a" (D.binary_pretty ~size:ptr_size ctx) address_sub);
      (* Is the address focused? *)
      match address with
      | Const _ ->
          let res, sub_mem = D.Memory_Forward.load_block ctx mem.sub_mem address_sub in
          res, { mem with sub_mem }
      | Offset (b, c) ->
          begin match D.should_focus ~size:ptr_size ctx mem.sub_mem b with
          | Some (focus_base, base_ofs_in_focus) ->
            let ofs_in_focus = D.offset_shift ~offset:(Z.to_int c) ~max:None ctx base_ofs_in_focus in
            let zero = D.offset_zero ~max:None ctx in

            assert (index_eq ctx zero ofs_in_focus) ;

            if Bin_map.mem focus_base mem.cache then begin
              Log.debug (fun p -> p "@[<v 2>focused.@.base = %a@.offset = %a@]" (D.binary_pretty ~size:ptr_size ctx) focus_base (D.offset_pretty ctx) ofs_in_focus);
              let region = Bin_map.find focus_base mem.cache in
              region, mem
            end
            else begin
              Log.debug (fun p -> p "Not focused, but focusable.");
              (* Load the required value from the subdomain's memory. *)
              let region, sub_mem = D.Memory_Forward.load_block ctx mem.sub_mem focus_base in
              let focus_size = D.Block_Forward.sizeof ctx region in
              let mem_unfocused = do_focus ctx focus_base focus_size {mem with sub_mem} in
              (* Now update the cache with this new information. *)
              let cache = Bin_map.add focus_base region mem_unfocused.cache in
              region, {mem_unfocused with cache}
            end
          | None ->
              let res, sub_mem =
                D.Memory_Forward.load_block ctx mem.sub_mem address_sub in
              res, {mem with sub_mem}
          end

    let store_block ctx mem address region =
      let ptr_size = Codex_config.ptr_size () in
      (* Codex_log.debug ~level:3 "@[<v 2>With_focusing.store ~size:%d@.address %a@.value %a" size (binary_pretty ~size:ptr_size ctx) address (binary_pretty ~size ctx) value; *)
      (* If the address is already focused? *)
      match address with
      | Offset (b, c) ->
        (* Codex_log.debug "in With_focusing.store, checking should focus with address %a" (binary_pretty ~size ctx) address ;         *)
        begin match D.should_focus ~size:ptr_size ctx mem.sub_mem b with
        | Some (focus_base, base_ofs_in_focus) ->
          let ofs_in_focus = D.offset_shift ~offset:(Z.to_int c) ~max:None ctx base_ofs_in_focus in
          let zero = D.offset_zero ~max:None ctx in

          assert (index_eq ctx zero ofs_in_focus) ;

          begin try
            let region = Bin_map.find focus_base mem.cache in
            Log.debug (fun p -> p "@[<v 2>focused.@.base = %a@.offset = %a@]"
              (D.binary_pretty ~size:ptr_size ctx) focus_base (D.offset_pretty ctx) ofs_in_focus);

            let new_cache = Bin_map.add focus_base region mem.cache in
            (* No mutation of the cache field here, but a functional update *)
            { mem with cache = new_cache }

          with Not_found ->
            Log.debug (fun p -> p "Not focused, but focusable.");
            let focus_size = D.Block_Forward.sizeof ctx region in
            let mem_unfocused = do_focus ctx focus_base focus_size mem in
            (* let focus_size = Stdlib.max focus_size size in *)
            (* let region, sub_mem = D.Memory_Forward.load_block ~size ctx mem_unfocused.sub_mem focus_base in *)
            let sub_mem = D.Memory_Forward.store_block ctx mem_unfocused.sub_mem focus_base region in
            { sub_mem ; cache = Bin_map.add focus_base region mem.cache }
          end
        | None ->
          let addr = Binary.to_sub ctx ~size:ptr_size address in
          { mem with sub_mem = D.Memory_Forward.store_block ctx mem.sub_mem addr region }
        end
      | Const _ ->
          let addr = Binary.to_sub ctx ~size:ptr_size address in
          { mem with sub_mem = D.Memory_Forward.store_block ctx mem.sub_mem addr region }


    let unknown ~level ctx =
      {sub_mem = D.Memory_Forward.unknown ~level ctx; cache = Bin_map.empty}
  end

  (*let memcpy ~size = D.Memory_Forward.memcpy ~size*)

  (**************** Serialization, fixpoind and nondet. ****************)

  (* Higher-ranked polymorphism is required here, and we need a record for that. *)
  type 'elt higher = {subf: 'tl. D.Context.t -> 'elt -> D.Context.t -> 'elt -> 'tl D.Context.in_acc -> ('elt,'tl) D.Context.result  } [@@unboxed]

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let serialize (type elt) (type c)
      {subf} ctxa a ctxb b (included, (acc : c Context.in_tuple)) : (elt,c) Context.result =
    let Context.Result (included, in_tup, deserialize) = subf ctxa a ctxb b (included, acc) in
    Result (included, in_tup, deserialize)

  let serialize_boolean ctxa a ctxb b acc = serialize {subf=D.serialize_boolean} ctxa a ctxb b acc

  let serialize_enum ctxa a ctxb b acc = serialize {subf=D.serialize_enum} ctxa a ctxb b acc

  let serialize_block ~widens ctxa a ctxb b acc =
    serialize {subf=fun ctxa a ctxb b acc -> D.serialize_block ~widens ctxa a ctxb b acc} ctxa a ctxb b acc

  let serialize_binary ~widens ~size ctxa a ctxb b acc =
    let xa = Binary.to_sub ~size ctxa a in
    let xb = Binary.to_sub ~size ctxb b in
    let Context.Result (included, acc, deserialize) = D.serialize_binary ~widens ~size ctxa xa ctxb xb acc in
    Context.Result (included, acc, fun ctx out ->
      let r, out = deserialize ctx out in
      Binary.of_sub ~size () r, out)

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let typed_nondet2 (type c) ctxa ctxb (acc : c Context.in_tuple) =
    D.typed_nondet2 ctxa ctxb acc

  let nondet_same_context = D.nondet_same_context

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let typed_fixpoint_step (type c) ~iteration ~init ~arg ~body (included, (acc : c Context.in_tuple)) : bool * (close:bool -> (c Context.out_tuple * Context.t)) =
    let bool,continuef = D.typed_fixpoint_step ~iteration ~init ~arg ~body (included, acc) in
    bool,fun ~close -> continuef ~close

    let widened_fixpoint_step = D.widened_fixpoint_step


  (**************** Queries ****************)

   module Query = struct
    include D.Query
    let reachable ctx mem = D.Query.reachable ctx mem.sub_mem

    let binary ~size ctx b =
      D.Query.binary ~size ctx @@ Binary.to_sub ~size ctx b
  end

  let memory_is_empty _ = assert false

  (* let binary_is_empty ~size ctx a = *)
  (*   match a with *)
  (*   | Offset (b,_) -> D.binary_is_empty ~size ctx b *)
  (*   | _ -> false *)

  let integer_is_empty _ = assert false
  let boolean_is_empty _ = assert false

  let binary_empty ~size = ar0 ~size (D.binary_empty ~size)
  let boolean_empty = D.boolean_empty
  let enum_empty = D.enum_empty
  let block_empty = D.block_empty

  let reachable ctx mem =
    D.reachable ctx mem.sub_mem

  let satisfiable = D.satisfiable

  let union _cond _ctx _tuple = assert false

  let should_focus : size:In_bits.t -> Context.t -> memory -> binary ->
      (binary * offset) option = fun ~size:_ _ctx _memory _address ->
    assert false

  let may_alias
    : ptr_size:In_bits.t -> Context.t -> size1:offset -> size2:offset -> binary -> binary -> bool
    = fun ~ptr_size:_ _ctx ~size1:_ ~size2:_ _addr1 _addr2 ->
    assert false

  let is_weak ~size ctx addr = assert false

  let binary_unknown_typed ~size ctx typ =
    Binary.of_sub ~size ctx @@ D.binary_unknown_typed ~size ctx typ

  let rec flush_cache_rec ctx mem =
    let size = Codex_config.ptr_size () in
    let num_region = Bin_map.cardinal mem.cache in
    let mem =
      Bin_map.fold (fun base region acc ->
        if D.is_weak ~size ctx base then acc
        else Memory_Forward.unfocus ctx base region acc
      ) mem.cache mem
    in
    if num_region = 0 then mem
    else if num_region > Bin_map.cardinal mem.cache
    then flush_cache_rec ctx mem
    else mem

  let flush_cache ctx mem =
    if Bin_map.is_empty mem.cache then mem
    else begin
      let mem = flush_cache_rec ctx mem in
      if not @@ Bin_map.is_empty mem.cache then (
        Emit_alarm.emit_alarm Operator.Alarm.Weak_type;
        Codex_log.error "Could not flush the cache as some weak types are never unified"
      ) ;
      {mem with cache = Bin_map.empty}
    end


  let serialize_memory ~widens ctx1 m1 ctx2 m2 acc =
    (* let m1 = flush_cache ctx1 m1 in
    let m2 = flush_cache ctx2 m2 in *)
    let Context.Result (included, in_tup, deserialize) =
      D.serialize_memory ~widens ctx1 m1.sub_mem ctx2 m2.sub_mem acc in
    Context.Result (included, in_tup, fun ctx out ->
      let sub_mem, out = deserialize ctx out in
      {sub_mem; cache = Bin_map.empty}, out)

  let query_boolean = D.query_boolean

  let global_symbol ctx name =
    let size,v = D.global_symbol ctx name in
    size, Binary.of_sub ~size ctx v

  let add_global_symbol ~size ctx name value =
    D.add_global_symbol ~size ctx name (Binary.to_sub ~size ctx value)

  let check_type ~size ctx typ value =
    D.check_type ~size ctx typ @@ Binary.to_sub ~size ctx value
    |> Type_check_tree.(sub_check (create_typing_node value (binary_pretty ~size ctx) typ))

  let type_of ~size ctx value =
    D.type_of ~size ctx @@ Binary.to_sub ~size ctx value

  let analyze_summary ctx funtyp args =
    let res, ret = D.analyze_summary ctx funtyp @@ List.map (fun (size,value) -> (size, Binary.to_sub ~size ctx value)) args in
    match ret with
    | None -> res, None
    | Some (size, value) -> res, Some (size, Binary.of_sub ~size ctx value)


  (*
  let addresses_in_binary ~size ctxa a ctxb b =
    let ptr_size = Codex_config.ptr_size () in
    let res = D.addresses_in_binary ~size ctxa (Binary.to_sub ~size ctxa a) ctxb (Binary.to_sub ~size ctxa b) in
    List.map (fun (va,vb) -> Binary.of_sub ~size:ptr_size ctxa va, Binary.of_sub ~size:ptr_size ctxa va)
  *)

  let addresses_in_block ctxa a ctxb b =
    let ptr_size = Codex_config.ptr_size () in
    let res = D.addresses_in_block ctxa a ctxb b in
    List.map (fun (va,vb) -> Binary.of_sub ~size:ptr_size ctxa va, Binary.of_sub ~size:ptr_size ctxb vb) res

  let addresses_in_memory _ctxa _mema _ctxb _memb = assert false


   (** Function relative to offsets (to be removed) **)

   let offset_pretty = D.offset_pretty
   let offset_zero = D.offset_zero
   let offset_shift = D.offset_shift
   let offset_le = D.offset_le
   let offset_eq = D.offset_eq

end

module Make_with_types (D : BASE_WITH_TYPES) : S_with_types
  with module Context = D.Context
  and type boolean = D.boolean
  (* and module Type = D.Type *)
= struct

  include Make (D)


  (* All functions used for serializing the cache *)

  module BinaryPair = struct

    type t = D.Binary.t * D.Binary.t
    let compare (a1,b1) (a2,b2) =
      let c = D.Binary.compare a1 a2 in
      if c <> 0 then c
      else D.Binary.compare b1 b2

    let equal (a1,b1) (a2,b2) =
      D.Binary.equal a1 a2 && D.Binary.equal b1 b2

    let hash (a,b) =
      Hashtbl.hash (D.Binary.hash a, D.Binary.hash b)
  end
  module VisitedSet = Extstdlib.Set.Make((BinaryPair));;

  let rec serialize_cache
    : 'a. widens:bool -> Context.t -> memory -> Context.t -> memory -> (D.binary * D.binary) list -> (bool * 'a Context.in_tuple) -> VisitedSet.t -> (cache, 'a) Context.result
    = fun ~widens ctxa mema ctxb memb nodes ((inc,tup) as acc) visited ->
    let ptr_size = Codex_config.ptr_size () in
    match nodes with
    | [] ->
      let _ = flush_cache ctxa mema in
      let _ = flush_cache ctxb memb in
      Context.Result (inc, tup, fun ctx out -> Bin_map.empty, out)

    | (addr_a,addr_b) :: remaining ->
      let res_a = D.should_focus ~size:ptr_size ctxa mema.sub_mem addr_a in
      let res_b = D.should_focus ~size:ptr_size ctxb memb.sub_mem addr_b in
      begin match res_a, res_b with
        | Some (base_a, _), Some (base_b, _) ->
          if VisitedSet.mem (base_a, base_b) visited then serialize_cache ~widens ctxa mema ctxb memb remaining acc visited
          else
            let visited = VisitedSet.add (addr_a,addr_b) visited in
            if Bin_map.mem base_a mema.cache && Bin_map.mem base_b memb.cache then
              let region_a = Bin_map.find base_a mema.cache in
              let region_b = Bin_map.find base_b memb.cache in
              let parts = D.addresses_in_block ctxa region_a ctxb region_b in
              let mema = {mema with cache = Bin_map.remove base_a mema.cache} in
              let memb = {memb with cache = Bin_map.remove base_b memb.cache} in
              let new_nodes =  remaining @ parts in
              let Context.Result (inc, tup, d_cache) = serialize_cache ~widens ctxa mema ctxb memb new_nodes acc visited in
              let Context.Result (inc, tup, d_region) = serialize_block ~widens ctxa region_a ctxb region_b (inc,tup) in
              let Context.Result (inc, tup, d_base) = D.serialize_binary ~widens ~size:ptr_size ctxa base_a ctxb base_b (inc,tup) in
              Context.Result (inc, tup, fun ctx out ->
                let base, out = d_base ctx out in
                let region, out = d_region ctx out in
                let cache, out = d_cache ctx out in
                let new_cache = Bin_map.add base region cache in
                new_cache, out
              )

            (*
            else if Bin_map.mem base_b memb.cache then
              let size_b,value_b = load_region ctxb base_b memb.cache in
              let value_a = binary_empty ~size:size_b ctxa in
              let memb = {memb with cache = Bin_map.remove base_b memb.cache} in
              let Context.Result (inc, tup, d_cache) = serialize_cache ~widens ctxa mema ctxb memb remaining acc visited in
              let Context.Result (inc, tup, d_value) = serialize_binary ~widens ~size:size_b ctxa value_a ctxb value_b (inc,tup) in
              let Context.Result (inc, tup, d_base) = D.serialize_binary ~widens ~size:ptr_size ctxa base_a ctxb base_b (inc,tup) in
              Context.Result (inc, tup, fun ctx out ->
                let base, out = d_base ctx out in
                let value, out = d_value ctx out in
                let cache, out = d_cache ctx out in
                let new_region = Region.create ~size:size_b (B value) in
                let new_cache = Bin_map.add base new_region cache in
                new_cache, out
              )

            else if Bin_map.mem base_a mema.cache then
              let size_a,value_a = load_region ctxa base_a memb.cache in
              let value_b = binary_empty ~size:size_a ctxb in
              let mema = {mema with cache = Bin_map.remove base_a mema.cache} in
              let Context.Result (inc, tup, d_cache) = serialize_cache ~widens ctxa mema ctxb memb remaining acc visited in
              let Context.Result (inc, tup, d_value) = serialize_binary ~widens ~size:size_a ctxa value_b ctxb value_a (inc,tup) in
              let Context.Result (inc, tup, d_base) = D.serialize_binary ~widens ~size:ptr_size ctxa base_a ctxb base_b (inc,tup) in
              Context.Result (inc, tup, fun ctx out ->
                let base, out = d_base ctx out in
                let value, out = d_value ctx out in
                let cache, out = d_cache ctx out in
                let new_region = Region.create ~size:size_a (B value) in
                let new_cache = Bin_map.add base new_region cache in
                new_cache, out
              )
            *)
            else serialize_cache ~widens ctxa mema ctxb memb remaining acc visited

        | _ -> serialize_cache ~widens ctxa mema ctxb memb remaining acc visited
      end


  let serialize_memory_and_cache
    : 'a. widens:bool -> Context.t -> memory -> Context.t -> memory -> (binary * binary) list -> 'a Context.in_acc -> (memory, 'a) Context.result
    = fun ~widens ctxa mema ctxb memb entries acc ->
    Log.debug (fun p -> p "serialize_cache");
    let size = Codex_config.ptr_size () in
    let entries = List.map (fun (va,vb) -> (Binary.to_sub ~size ctxa va, Binary.to_sub ~size ctxb vb)) entries in
    let lst = D.addresses_in_memory ctxa mema.sub_mem ctxb memb.sub_mem in
    let entries = entries @ lst in
    let visited = VisitedSet.empty in
    let Context.Result (inc, tup, d_memory) = D.serialize_memory ~widens ctxa mema.sub_mem ctxb memb.sub_mem acc in
    let Context.Result (inc, tup, d_cache) = serialize_cache ~widens ctxa mema ctxb memb entries (inc,tup) visited in
    Context.Result (inc, tup, fun ctx out ->
      let cache, out = d_cache ctx out in
      let sub_mem, out = d_memory ctx out in
      {sub_mem; cache}, out
    )

  let analyze_summary ctx funtyp args mem =
    let res, ret = analyze_summary ctx funtyp args in
    let pure = TypedC.is_function_pure funtyp in
    let mem = if pure then mem else flush_cache_rec ctx mem in
    res, ret, mem

end
