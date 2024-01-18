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

module type Base_with_types = sig
  include Domain_sig.Base
  (* module Type : Type_domain.S
    with module BR.Scalar.Context = Context *)

  val global_symbol : Context.t -> string -> int * binary
  val add_global_symbol : size:int -> Context.t -> string -> binary -> unit
  val flush_cache : Context.t -> memory -> memory
  val has_type : size:int -> Context.t -> Types.Ctypes.typ -> binary -> bool
end

module type S = sig
  include Domain_sig.Base
end

module type S_with_types = sig
  include Base_with_types
end

module Make (D : Domain_sig.Base)  = struct

  let name = "With_focusing(" ^ D.name ^ ")";;
  let unique_id = Domain_sig.Fresh_id.fresh name;;

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

    let hash _ = assert false

    let to_sub ~size ctx = function
    | Const c ->
        D.Binary_Forward.biconst ~size c ctx
    | Offset (b, c) ->
        Codex_log.debug ~level:4 "before to_sub: Offset (%a, %s)" (D.binary_pretty ~size ctx) b (Z.format "%X" c);
        let res = if Z.equal c Z.zero then b else
          D.Binary_Forward.(bshift ~size ~offset:(Z.to_int c) ~max:None ctx b)
        in
        Codex_log.debug ~level:4 "after to_sub: %a" (D.binary_pretty ~size ctx) res;
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

  module Binary_op = struct
    type t = Unfetched | B of Binary.t

    let lift f = function
    | Unfetched -> Unfetched
    | B b -> B (f b)

    let concat f ~size1 ~size2 b1 b2 =
      match b1,b2 with
      | Unfetched,_ | _,Unfetched -> Unfetched
      | B b1, B b2 -> B (f ~size1 ~size2 b1 b2)
  end

  module Bin_map = Extstdlib.Map.Make(D.Binary)
  (* Interval map. Offsets are in bits, not bytes. *)
  module Region : sig
    include module type of Interval_map.With_Extract(Binary_op)
    val load : size:int
      -> extract:(Binary_op.t -> idx:int -> size:int -> oldsize:int -> Binary_op.t)
      -> t -> Z.t -> (int * int * Binary_op.t) list

    (** Same as load, but if the region to load contains several values,
        concatenate them. *)
    val load_oneval : size:int
      -> extract:(Binary_op.t -> idx:int -> size:int -> oldsize:int -> Binary_op.t)
      -> concat:(size1:int -> Binary_op.t -> size2:int -> Binary_op.t -> Binary_op.t)
      -> t -> Z.t -> Binary_op.t
  end = struct
    include Interval_map.With_Extract(Binary_op)

    let load ~size ~extract map key =
      let key = Z.to_int key in
      assert (size > 0);
      assert (key >= 0);
      let region_size = get_size map in
      assert (key + size <= region_size);
      fold_between ~size key map [] ~extract (fun ~size key v acc -> (size,key,v)::acc)

    let load_oneval ~size ~extract ~concat map key =
      let key = Z.to_int key in
      assert(size > 0);
      assert(key >= 0);
      let region_size = get_size map in
      (* Codex_log.feedback "loading offset %d size %d region_size %d %a" key size region_size (pretty ctx) {map}; *)                
      assert(key + size <= region_size);
      (* Kernel.feedback "load idx %d size %d" key size; *)
      let l = fold_between ~size key map [] ~extract
          (fun ~size _key value acc -> (size,value)::acc)
      in match l with
      | [] -> assert false    (* The address is correct, we must load something. *)
      | [_,v] -> v
      | (sizea,a)::rest ->
          (* TODO: this depends on endiannees *)
          let f (size,acc) (newsize,newv) =
            let newsize = newsize in
            let acc = concat ~size1:size acc ~size2:newsize newv in
            (size+newsize,acc)
          in
          let fsize,v = List.fold_left f (sizea,a) rest in
          assert (fsize == size);
          v
  end
  type cache = Region.t Bin_map.t

  module Types = struct
    type nonrec binary = binary

    type memory =
      { sub_mem : D.memory; cache : cache }

    type boolean = D.boolean
  end
  include (Types : module type of Types with type binary := binary)

  module Boolean = D.Boolean

  module Context = D.Context

  let root_context = D.root_context
  let context_pretty = D.context_pretty                       

  include Transfer_functions.Builtin.Make(Types)(Context)

  let mu_context_open = D.mu_context_open

  let assume = D.assume
  let imperative_assume = D.imperative_assume

  module Boolean_Forward = struct
    let (||) = D.Boolean_Forward.(||)
    let (&&) = D.Boolean_Forward.(&&)
    let not = D.Boolean_Forward.not
    let true_ = D.Boolean_Forward.true_
    let false_ = D.Boolean_Forward.false_
  end

  let boolean_unknown = D.boolean_unknown

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
      (D.memory_pretty ctx) mem.sub_mem (Bin_map.mk_pretty
        (D.binary_pretty ~size:(Codex_config.ptr_size ()) ctx)
        (fun pp reg -> fprintf pp "@[<hov 2><region of size %d bits>@]" (Region.get_size reg))) mem.cache

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

  
  module Binary_Forward = struct
    let beq   ~size ctx a b = pred2 ~size (D.Binary_Forward.beq   ~size) ctx a b
    let biule ~size = pred2 ~size (D.Binary_Forward.biule ~size)
    let bisle ~size = pred2 ~size (D.Binary_Forward.bisle ~size)

    let const_add ~size x y = Z.erem (Z.add x y) @@ Z.shift_left Z.one size
    let const_sub ~size x y = Z.erem (Z.sub x y) @@ Z.shift_left Z.one size
    let const_mul ~size x y = Z.erem (Z.mul x y) @@ Z.shift_left Z.one size

    let biadd ~size ~nsw ~nuw ~nusw ctx x y =
      match x,y with
      | Const i, Const j -> Const (const_add ~size i j)
      (* | Offset (b,ofs), Const i | Const i, Offset (b,ofs) -> Offset (b, const_add ~size ofs i) *)
      | Offset (b,ofs), Const i | Const i, Offset (b,ofs) ->
          let offset = Z.signed_extract (const_add ~size ofs i) 0 size in
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
          ar2 ~size (D.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw) ctx x y

    let bisub ~size ~nsw ~nuw ~nusw ctx x y =
      match x,y with
      | Const i, Const j -> Const (const_sub ~size i j)
      (* | Offset (b,ofs), Const i -> Offset (b, const_sub ~size ofs i) *)
      | Offset (b,ofs), Const i ->
          let offset = Z.signed_extract (const_sub ~size ofs i) 0 size in
          let one = D.Binary_Forward.biconst ~size Z.one ctx in
          Offset (D.Binary_Forward.bindex ~size (Z.to_int offset) ctx b one, Z.zero)
      | Const _, Offset (_,_) ->
          (* Let's just give up on this. *)
          Offset (D.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw ctx
              (Binary.to_sub ~size ctx x) (Binary.to_sub ~size ctx y),
            Z.zero)
      | Offset (b1,o1), Offset (b2,o2) ->
          (* This is weird. *)
          Offset(D.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw ctx b1 b2, const_sub ~size o1 o2)

    let bimul ~size ~nsw ~nuw ctx x y =
      match x,y with
      | Const i, Const j -> Const (const_mul ~size i j)
      | _ -> ar2 ~size (D.Binary_Forward.bimul ~size ~nsw ~nuw) ctx x y

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
    let bshl ~size ~nsw ~nuw = ar2 ~size (D.Binary_Forward.bshl ~size ~nsw ~nuw)
    let bisdiv ~size = ar2 ~size (D.Binary_Forward.bisdiv ~size)
    let biudiv ~size = ar2 ~size (D.Binary_Forward.biudiv ~size)
    let bconcat ~size1 ~size2 ctx x1 x2 =
      Binary.(of_sub ~size:(size1+size2) ctx @@ D.Binary_Forward.bconcat ctx
      ~size1 ~size2 (to_sub ctx ~size:size1 x1) (to_sub ctx ~size:size2 x2))
    let bismod ~size = ar2 ~size (D.Binary_Forward.bismod ~size)
    let biumod ~size = ar2 ~size (D.Binary_Forward.biumod ~size)

    let bextract ~size ~index ~oldsize ctx x =
      D.Binary_Forward.bextract ~size ~index ~oldsize ctx (Binary.to_sub ~size:oldsize ctx x)
      |> Binary.of_sub ~size ctx

    let valid ~size access_type ctx x =
      D.Binary_Forward.valid ~size access_type ctx @@ Binary.to_sub ~size:((Codex_config.ptr_size())) ctx x

    let valid_ptr_arith ~size ctx x y =
      D.Binary_Forward.valid_ptr_arith ~size ctx (Binary.to_sub ~size ctx x)
        (Binary.to_sub ~size ctx y)

    let biconst ~size:_ k _ctx = Const k

    let buninit ~size = ar0 ~size @@ D.Binary_Forward.buninit ~size

    let bshift ~size ~offset ~max ctx x =
      match x with
      (* | Offset (b,ofs) -> Offset (b, const_add ~size ofs (Z.of_int offset)) *)
      | Offset (b,ofs) ->
          let new_ofs = Z.signed_extract (const_add ~size ofs (Z.of_int offset)) 0 size in
          Offset (D.Binary_Forward.bshift ~size ~offset:(Z.to_int new_ofs) ~max ctx b, Z.zero)
      | _ -> ar1 ~size (D.Binary_Forward.bshift ~size ~offset ~max) ctx x

    let bindex ~size k ctx x idx =
      assert(k != 0);
      match x,idx with
      | Offset (b,ofs), Const i ->
        (* Codex_log.feedback "ofs %s k %d i %s\n" (Z.to_string ofs) k (Z.to_string i); *)
        let new_ofs = Z.signed_extract (const_add ~size ofs @@ const_mul ~size (Z.of_int k) i) 0 size in
        Offset (b, new_ofs)
      | _ -> ar2 ~size (D.Binary_Forward.bindex ~size k) ctx x idx

    let bchoose ~size:_ _ = assert false
  end

  let binary_unknown ~size =
    ar0 ~size @@ D.binary_unknown ~size

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
      Codex_log.debug ~level:3 "Aliasing region found. Unfocusing. @[<v 2> base = \
        %a" (D.binary_pretty ~size:ptr_size ctx) base;
      let size_reg = Region.get_size region in
      let to_store = Region.load_oneval ~size:size_reg ~extract:(fun b ~idx ~size ~oldsize ->
        Binary_op.lift (Binary_Forward.bextract ~size ~index:idx ~oldsize ctx) b)
      ~concat:(fun ~size1 b1 ~size2 b2 ->
        Binary_op.concat (Binary_Forward.bconcat ctx) ~size1 ~size2 b1 b2) region Z.zero
      in
      let sub_mem =
        match to_store with
        | Binary_op.Unfetched -> assert false (* mem.sub_mem *)
        | Binary_op.B v ->
          let value = Binary.to_sub ~size:size_reg ctx v in
          Codex_log.debug ~level:3 "@[<hov 2>Storing size %d value: %a@]" size_reg (D.binary_pretty ~size:size_reg ctx) value;
          (* We know that there will be no overflow, all stores in a focused
           * region are guarded by alarms. So we can take it for granted
           * and set the [nsw] and [nuw] flags. This avoids some incorrect
           * writes to address zero, which can generate horrible
           * fixpoint-finding bugs.  *)
          D.Memory_Forward.store ~size:size_reg ctx mem.sub_mem base value
      in
      { sub_mem;
        cache = Bin_map.remove base mem.cache;
      }
    

    (* Like [find_first_opt] from {!Extstdlib.Map.S} but the predicate also
     * takes a value in argument. *)
    (* let find_first_opt'
      : (Bin_map.key -> 'a -> bool) -> 'a Bin_map.t -> (Bin_map.key * 'a) option
      = fun pred map ->
        Bin_map.find_first_opt (fun k -> pred k @@ Bin_map.find k map) map *)

    (* Focus a region, after unfocusing any aliased one, if any. *)
    let do_focus ctx focus_base focus_size mem =
      let ptr_size = Codex_config.ptr_size () in
      Codex_log.debug ~level:3 "focusing. @[<v 2>base = %a@.focus size = %d@]" (D.binary_pretty ~size:ptr_size ctx) focus_base
        focus_size;
      (* We must unfocus the currently focused
       * instance (if any) and focus this one. *)
      (* XXX: this is unsound: we would need to test aliasing of regions,
       * not individual memory cells. *)
      let pred other_addr region = D.may_alias ~ptr_size ctx
        ~size1:(Region.get_size region / 8) ~size2:(focus_size/8) other_addr focus_base in
      (* let rec loop mem = match find_first_opt' pred mem.cache with
      | Some (other_b, region) -> let mem' = unfocus ctx other_b region mem
        in loop mem'
      | None -> mem
      in 
      let mem_unfocused = loop mem in *)
      let filtered = Bin_map.filter (fun bin region -> pred bin region) mem.cache in
      let mem_unfocused = Bin_map.fold (fun other_b region mem -> unfocus ctx other_b region mem) filtered mem in
      let region = Region.create ~size:focus_size Binary_op.Unfetched in
      (* TODO: apply type predicates to every field of the structure. *)
      mem_unfocused, region


    let load ~size ctx mem address =
      let ptr_size = Codex_config.ptr_size () in
      (* Codex_log.debug ~level:3 "@[<v 2>With_focusing.load ~size:%d@.address %a" size (binary_pretty ~size:ptr_size ctx) address; *)
      Codex_log.feedback "@[<v 2>With_focusing.load ~size:%d@.address %a" size (binary_pretty ~size:ptr_size ctx) address;
      let address_sub = Binary.to_sub ~size:ptr_size ctx address in
      Codex_log.debug ~level:3 "address_sub = %a" (D.binary_pretty ~size:ptr_size ctx) address_sub;
      (* Is the address focused? *)
      match address with
      | Const _ ->
          let res, sub_mem = D.Memory_Forward.load ~size ctx mem.sub_mem address_sub in
          Binary.of_sub ~size ctx res, { mem with sub_mem }
      | Offset (b, c) ->
          begin match D.should_focus ~size:ptr_size ctx mem.sub_mem b with
          | Some (focus_base, focus_size, base_ofs_in_focus) ->
            let ofs_in_focus = base_ofs_in_focus + 8 * Z.to_int c in
            if focus_size <= ofs_in_focus then (Codex_log.alarm "array_offset_access" ; raise Memory_sig.Memory_Empty) ;
            let ofs_in_focus = Z.of_int ofs_in_focus in
            if Bin_map.mem focus_base mem.cache then begin
              Codex_log.feedback "@[<v 2>focused.@.base = %a@.offset = %a@]" (D.binary_pretty ~size:(Codex_config.ptr_size ()) ctx) focus_base Z.pp_print ofs_in_focus;
              let region = Bin_map.find focus_base mem.cache in
              let load_res = Region.load_oneval ~size ~extract:(fun b ~idx ~size ~oldsize ->
                  Binary_op.lift (Binary_Forward.bextract ~size ~index:idx ~oldsize ctx) b)
                ~concat:(fun ~size1 b1 ~size2 b2 ->
                  Binary_op.concat (Binary_Forward.bconcat ctx) ~size1 ~size2 b1 b2) region ofs_in_focus
              in
              match load_res with
              | Binary_op.Unfetched -> assert false
                  (* Actually do the load in the subdomain's memory. *)
                  (* let result, new_sub_mem = D.Memory_Forward.load ~size ctx mem.sub_mem address_sub in *)
                  (* let loaded, new_sub_mem = D.Memory_Forward.load ~size:focus_size ctx mem.sub_mem focus_base in
                  let result = D.Binary_Forward.bextract ~size ~index:(Z.to_int ofs_in_focus) ~oldsize:focus_size ctx loaded in
                  let result = Binary.of_sub ~size ctx result in *)
                  (* Now update the cache with this new information. *)
                  (* let new_region = Region.store ~size (Z.to_int ofs_in_focus) region (Binary_op.B result) in
                  let cache = Bin_map.add focus_base new_region mem.cache in
                  result, {sub_mem = new_sub_mem; cache} *)
              | Binary_op.B result -> result, mem
            end
            else begin
              Codex_log.feedback "Not focused, but focusable.";
              let focus_size = Stdlib.max focus_size size in
              let mem_unfocused, region = do_focus ctx focus_base focus_size mem in
              (* Load the required value from the subdomain's memory. *)
              (* let result, new_sub_mem =
                D.Memory_Forward.load ~size ctx mem_unfocused.sub_mem address_sub in
              let result = Binary.of_sub ~size ctx result in *)
              let loaded, new_sub_mem = D.Memory_Forward.load ~size:focus_size ctx mem.sub_mem focus_base in
              let loaded = Binary.of_sub ~size ctx loaded in
              let result = Binary_Forward.bextract ~size ~index:(Z.to_int ofs_in_focus) ~oldsize:focus_size ctx loaded in
              (* Now update the cache with this new information. *)
              (* let new_region = Region.store ~size (Z.to_int ofs_in_focus) region (Binary_op.B result) in *)
              let new_region = Region.store ~size:focus_size 0 region (Binary_op.B loaded) in
              let cache = Bin_map.add focus_base new_region mem_unfocused.cache in
              result, {sub_mem = new_sub_mem; cache}
            end
          | None ->
              let res, sub_mem =
                D.Memory_Forward.load ~size ctx mem.sub_mem address_sub in
              Binary.of_sub ~size ctx res, {mem with sub_mem}
          end

    let store ~size ctx mem address value =
      let ptr_size = Codex_config.ptr_size () in
      (* Codex_log.debug ~level:3 "@[<v 2>With_focusing.store ~size:%d@.address %a@.value %a" size (binary_pretty ~size:ptr_size ctx) address (binary_pretty ~size ctx) value; *)
      (* If the address is already focused? *)
      match address with
      | Offset (b, c) ->
        (* Codex_log.debug "in With_focusing.store, checking should focus with address %a" (binary_pretty ~size ctx) address ;         *)
        begin match D.should_focus ~size:ptr_size ctx mem.sub_mem b with
        | Some (focus_base, focus_size, base_ofs_in_focus) ->
          let ofs_in_focus = base_ofs_in_focus + 8 * Z.to_int c in
          if focus_size <= ofs_in_focus then (Codex_log.alarm "array_offset_access" ; raise Memory_sig.Memory_Empty) ;
          let ofs_in_focus = Z.of_int ofs_in_focus in
          begin try
            let region = Bin_map.find focus_base mem.cache in
            Codex_log.feedback "@[<v 2>focused.@.base = %a@.offset = %a@]" (D.binary_pretty ~size:ptr_size ctx) focus_base Z.pp_print ofs_in_focus;
            if Z.geq ofs_in_focus Z.zero && Z.leq (Z.add ofs_in_focus (Z.of_int size)) (Z.of_int @@ Region.get_size region) then begin
              let new_region = Region.store ~size (Z.to_int ofs_in_focus) region (Binary_op.B value) in
              let new_cache = Bin_map.add focus_base new_region mem.cache in
              (* No mutation of the cache field here, but a functional update *)
              { mem with cache = new_cache }
            end
            else begin
              Codex_log.alarm "bad_focused_store";
              Codex_log.error "offset %a and size %d do not respect 0 <= offset && offset + size < %d. Focus not performed, direct store" Z.pp_print ofs_in_focus size (Region.get_size region);
              { mem with sub_mem = D.Memory_Forward.store ~size ctx
                  mem.sub_mem (Binary.to_sub ctx ~size:ptr_size
                  address) (Binary.to_sub ~size ctx value) }
            end
          with Not_found ->
            Codex_log.debug ~level:2 "Not focused, but focusable.";
            let focus_size = Stdlib.max focus_size size in
            let mem_unfocused, region = do_focus ctx focus_base focus_size mem in
            let loaded, new_sub_mem = D.Memory_Forward.load ~size:focus_size ctx mem.sub_mem focus_base in
            let loaded = Binary.of_sub ~size ctx loaded in
            let region = Region.store ~size:focus_size 0 region (Binary_op.B loaded) in
            let region = Region.store ~size (Z.to_int ofs_in_focus) region (Binary_op.B value) in
            { mem_unfocused with cache = Bin_map.add focus_base region mem.cache }
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


    let unknown ~level ctx =
      {sub_mem = D.Memory_Forward.unknown ~level ctx; cache = Bin_map.empty}
  end

  (*let memcpy ~size = D.Memory_Forward.memcpy ~size*)

  let boolean_pretty = D.boolean_pretty

  (**************** Serialization, fixpoind and nondet. ****************)

  (* Higher-ranked polymorphism is required here, and we need a record for that. *)
  type 'elt higher = {subf: 'tl. D.Context.t -> 'elt -> D.Context.t -> 'elt -> 'tl D.Context.in_acc -> ('elt,'tl) D.Context.result  } [@@unboxed]

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let serialize (type elt) (type c)
      {subf} ctxa a ctxb b (included, (acc : c Context.in_tuple)) : (elt,c) Context.result =
    let Context.Result (included, in_tup, deserialize) = subf ctxa a ctxb b (included, acc) in
    Result (included, in_tup, deserialize)

  let serialize_memory ctx1 m1 ctx2 m2 acc =
    let Context.Result (included, in_tup, deserialize) =
      D.serialize_memory ctx1 m1.sub_mem ctx2 m2.sub_mem acc in
    Context.Result (included, in_tup, fun ctx out ->
      let sub_mem, out = deserialize ctx out in
      {sub_mem; cache = Bin_map.empty}, out)

  let serialize_boolean ctx a b acc = serialize {subf=D.serialize_boolean} ctx a b acc

  let serialize_binary ~size ctxa a ctxb b acc =
    let xa = Binary.to_sub ~size ctxa a in
    let xb = Binary.to_sub ~size ctxb b in
    let Context.Result (included, acc, deserialize) = D.serialize_binary ~size ctxa xa ctxb xb acc in
    Context.Result (included, acc, fun ctx out ->
      let r, out = deserialize ctx out in
      Binary.of_sub ~size () r, out)

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let typed_nondet2 (type c) ctxa ctxb (acc : c Context.in_tuple) =
    D.typed_nondet2 ctxa ctxb acc

  let nondet_same_context = D.nondet_same_context

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let typed_fixpoint_step (type c) ~init ~arg ~body (included, (acc : c Context.in_tuple)) : bool * (close:bool -> (c Context.out_tuple * Context.t)) =
    let bool,continuef = D.typed_fixpoint_step ~init ~arg ~body (included, acc) in
    bool,fun ~close -> continuef ~close

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

  let builtin_show_each_in string ctx args memory =
    let args = List.map (fun (size,bin) -> (size,Binary.to_sub ~size ctx bin)) args in
    let mem = D.builtin_show_each_in string ctx args memory.sub_mem in
    {memory with sub_mem = mem }
      

  let binary_empty ~size = ar0 ~size (D.binary_empty ~size)
  let boolean_empty = D.boolean_empty

  let reachable ctx mem =
    D.reachable ctx mem.sub_mem

  let satisfiable = D.satisfiable

  let union _cond _ctx _tuple = assert false

  let should_focus : size:int -> Context.t -> memory -> binary ->
      (binary * int * int) option = fun ~size:_ _ctx _memory _address ->
    assert false

  let may_alias
    : ptr_size:int -> Context.t -> size1:int -> size2:int -> binary -> binary -> bool
    = fun ~ptr_size:_ _ctx ~size1:_ ~size2:_ _addr1 _addr2 ->
    assert false

  let binary_unknown_typed ~size ctx typ =
    Binary.of_sub ~size ctx @@ D.binary_unknown_typed ~size ctx typ

  let flush_cache ctx mem =
    Bin_map.fold (fun base region acc ->
      Memory_Forward.unfocus ctx base region acc
    )
    mem.cache mem

  let query_boolean = D.query_boolean
end

module Make_with_types (D : Base_with_types) : S_with_types
  with module Context = D.Context
  and type boolean = D.boolean
  (* and module Type = D.Type *)
= struct

  include Make (D)

  let global_symbol ctx name =
    let size,v = D.global_symbol ctx name in
    size, Binary.of_sub ~size ctx v

  let add_global_symbol ~size ctx name value =
    D.add_global_symbol ~size ctx name (Binary.to_sub ~size ctx value)

  let has_type ~size ctx typ value =
    D.has_type ~size ctx typ @@ Binary.to_sub ~size ctx value

end
