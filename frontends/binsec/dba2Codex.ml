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


module Log = Tracelog.Make(struct let category = "Dba2Codex" end)
module LogExpression = Tracelog.Make(struct let category = "Expression" end)
module LogDbaInstr = Tracelog.Make(struct let category = "DBA Instruction" end)

module In_bytes = Units.In_bytes
module In_bits = Units.In_bits
let in_bytes = Units.In_bytes.of_int
let in_bits = Units.In_bits.of_int


module VarMap = Codex.Extstdlib.Map.Make(String)

type jump_target =
  | Jump_Inner of Dba.id (** Some instruction in a block *)
  | Jump_Outer of Virtual_address.t (** Some instruction outside of the block. *)
  | Jump_Dynamic              (* TODO *)

open Codex
module TypedC = Types.TypedC

module Logger = Codex_logger


module type Address_sig = sig
  type t
  val create : int -> t

  module Set : Stdlib.Set.S with type elt = t
  module Htbl : Stdlib.Hashtbl.S with type key = t
end

module type RegionS = sig
  module Virtual_address : Address_sig

  val written_data_addrs : Virtual_address.Set.t ref
  val read_data_addrs : Virtual_address.Set.t ref
  val set_untyped_load : bool -> unit
  val set_check_store_intvl : bool -> unit
  val set_param_typing : (TypedC.typ * int) Virtual_address.Htbl.t -> unit
end

module M = Codex.Domains.Memory_domains


module type StateS = sig
  module Domain : M.With_focusing.S_with_types

  type t = {
    ctx: Domain.Context.t;
    vars: Domain.binary VarMap.t;
    memory: Domain.memory;
    instruction_count: int;
    is_bottom: bool;
    never_went_to_user_code : bool;
  }


  val initial : (Loader_elf.Img.t, 'a, 'b, 'c) Loader.t_pack -> Domain.Context.t ->
                t
  val initial_concrete : (Loader_elf.Img.t, 'a, 'b, 'c) Loader.t_pack
    (*-> (Loader_elf.Img.t, 'a, 'b) Loader.t_pack*) -> Domain.Context.t -> t
  val reset : (Loader_elf.Img.t, 'a, 'b, 'c) Loader.t_pack -> Domain.Context.t ->
              t
  val get : size:In_bits.t -> t -> string -> Domain.binary
  val set : t -> string -> Domain.binary -> t
  val assume : Domain.boolean -> t -> t
  (* val assume_pred : size:int -> TypedC.Pred.t
    -> Weak_shape_domain.BR.binary -> t -> t *)
  val bottom : Domain.Context.t -> t
  val dump_state : Format.formatter -> t -> unit
  val dump_state_diff : Format.formatter -> t -> t -> Virtual_address.t -> (string, string) Hashtbl.t -> unit
  val join : t -> t -> t
  val is_included : t -> t -> bool

  (** Serialize a state's variables and memory into a tuple. [to_tuple ctx
      state] returns the result of serialization, along with an inversion
      function, to turn a tuple back into a state. This function takes the
      original state and a tuple as arguments, and update the state's [vars]
      and [memory] fields with the tuple's contents. *)
  val serialize: widens:bool -> t -> t -> 'a Domain.Context.in_acc -> (t, 'a) Domain.Context.result
end

module Create ()  = struct
  module BinaryEnumBasis:Single_value_abstraction.Sig.NUMERIC_ENUM = struct
    include Codex.Single_value_abstraction.Ival
    include Codex.Single_value_abstraction.Bitfield
  end
  module Terms = Terms.Builder.Make(Terms.Condition.ConditionCudd)(Terms.Relations.Additive)()
  module Propag_domain = Domains.Term_based.Propagation.Make(Terms)(BinaryEnumBasis)
  (* module Propag_domain = Domains.Term_based.Nonrelational.Make
    (Terms)(Ival_basis) *)
  module Numeric_simple = Domains.Term_domain.Make(Terms)(Propag_domain)

  module Numeric_loop =
    (val if Codex_options.UseLoopDomain.get() then (module Domains.Loop_domain.Make(Terms)(Numeric_simple):Domains.Sig.BASE)
    else (module Numeric_simple))

  module Numeric = Domains.Bitwise.Make(Numeric_loop)
  (* module Numeric_simple = Loop_domain.Make(Terms)(Propag_domain)
  module Numeric = Bitwise_domain.Make(Numeric_simple) *)
  (*module Numeric = Numeric_simple*)

  (*
  module Region_numeric = Region_numeric_offset.Make(Numeric)
  module Numeric_OV : Codex.Memory_sig.Operable_Value_Whole
    with module Scalar = Numeric
    and type binary = Numeric.binary
    and type boolean = Numeric.boolean
  = Region_numeric.Address
  *)

  module Region_suffix_tree = M.Region_suffix_tree.Make(Numeric)
  module Region_separation = M.Region_separation.Make(Region_suffix_tree)

  module Offset = Region_suffix_tree.Offset

  module rec Typed_address_domain :
    (Domains.Memory_sig.ADDRESS_AND_MAKE_MEMORY
      with module Scalar = Numeric) =
    M.Typed_address.Make (Region_separation) (Flexible_array_member_domain.Value)

  and Wholified :
    (Domains.Memory_sig.WHOLE_MEMORY_DOMAIN
      with module Scalar = Numeric) =
    M.Wholify.Make (Typed_address_domain)

  and Value_union_concatenation_domain :
    (Domains.Memory_sig.WHOLE_MEMORY_DOMAIN
      with module Scalar = Numeric) =
    M.Value_union_concatenation.Make (Wholified)

  and Block_smashing_domain :
    (Domains.Memory_sig.BLOCK
     with module Scalar = Numeric) =
    M.Block_smashing.Make (Value_union_concatenation_domain.Address) (Offset)

  and Flexible_array_member_domain :
    (Domains.Memory_sig.COMPLETE_DOMAIN
      with module Scalar = Numeric) =
    M.Flexible_array_member.MakeComplete
      (Value_union_concatenation_domain)
      (M.Block_smashing.Make (Value_union_concatenation_domain.Address) (Offset))

  (* module Numeric_OV : Codex.Memory_sig.Operable_Value_Whole = Wholified *)
  module Numeric_OV = Flexible_array_member_domain.Value
  (* module Wholified_memory = Wholified.Memory(Numeric_OV) *)

  module Region : RegionS with module Virtual_address := Virtual_address = struct

    module Virtual_address = Virtual_address
    let written_data_addrs = ref Virtual_address.Set.empty
    let read_data_addrs = ref Virtual_address.Set.empty

    let untyped_load = ref false
    let set_untyped_load b = untyped_load := b
    let check_store_intvl = ref true
    let set_check_store_intvl b = check_store_intvl := b

    let param_typing : (TypedC.typ * int) Virtual_address.Htbl.t option ref = ref None
    let set_param_typing typing =
      param_typing := Some typing
  end

  module MemD = Flexible_array_member_domain
  module Scalar = Numeric
  module Value = MemD.Value
  module Block = MemD.Block
  module Memory = MemD.Memory

  module Without_focusing = struct
    (* include Memory_domain.Make(Numeric)(Weak_shape_domain.Binary_Representation)(Weak_shape_domain_memory) *)
    include M.Memory_domain.Make(Value)(Block)(Memory)
    let flush_cache _ctx mem = mem
  end

  module type DSIG = M.With_focusing.S_with_types

  let m_domain = if Codex_options.Focusing.get ()
    then (module M.With_focusing.Make_with_types (Without_focusing) : DSIG)
    else
      let module Without_focusing = struct
        include Without_focusing

        let analyze_summary ctx funtyp args mem =
          let res,ret = analyze_summary ctx funtyp args in res,ret,mem

        let serialize_memory_and_cache ~widens ctxa mema ctxb memb entries acc =
          serialize_memory ~widens ctxa mema ctxb memb acc
      end in
      (module Without_focusing : DSIG)

  module Domain = (val m_domain)

  module Ival = Codex.Ext.Framac_ival.Ival

  module type Registers = sig
    val registers : (string * In_bits.t) list
    val initial_value : Domain.Context.t -> string * In_bits.t -> Domain.binary
  end

  let bunknown ~size ctx =
    Domain.binary_unknown ~size ctx

  module Make (Reg : Registers) = struct

    module State = struct

      module Wholified = Wholified
      module Domain = Domain

      type t = {
        ctx: Domain.Context.t;
        vars: Domain.binary VarMap.t;
        memory: Domain.memory;
        instruction_count: int;
        is_bottom: bool;
        never_went_to_user_code : bool;
      }

      let read_u32 (mem : Loader_elf.Img.t) (offset : int) : Loader_types.u32 =
        let open Loader_elf in
        let b0 = read_offset mem offset in
        let b1 = read_offset mem (offset+1) in
        let b2 = read_offset mem (offset+2) in
        let b3 = read_offset mem (offset+3) in
        (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0

      let load_image ptrsize img ctx memory =
        let program_headers = Loader_elf.program_headers img in
        let memory = Array.fold_left (fun mem phdr ->
            let open Loader_elf in
            let vaddr = phdr.Phdr.vaddr in
            let filesize = phdr.Phdr.filesz in
            let filesize_mul_4 = filesize - (filesize mod 4) in
            let memsize = phdr.Phdr.memsz |> in_bytes in
            Logger.result "Region: address %x filesize %x filesize_mul_4 %x memsize %x"
              vaddr filesize filesize_mul_4 (memsize:>int);
            if memsize = In_bytes.zero then mem else
              let initial = Domain.Binary_Forward.biconst ~size:(memsize |> In_bytes.in_bits) Z.zero ctx in
              (* XXX: to test syscalls individually, and quickly. *)
              (*
              let initial =
                Region_binary_Operable_Value.param_unknown ~size:(memsize * 8) (Domain.get_subcontext ctx) in
              *)
              let address = Domain.Binary_Forward.biconst ~size:ptrsize (Z.of_int vaddr) ctx in
              let mem = Domain.Memory_Forward.store ~size:(memsize |> In_bytes.in_bits) ctx mem address initial in
              (* Logger.result "Memory %a"  (Domain.memory_pretty ctx) mem; *)
              let rec loop i mem =
                (* Logger.result "loop %x" i; *)
                (* TODO: lots of time, the loaded value is zero. We just
                   accumulate the zeroes, and write them as a big chunk,
                   using an inner loop. *)
                let rec inner_loop i idx size mem =
                  (* Logger.result "inner_loop %x %x %x" i idx size; *)
                  let u32 =
                    try read_u32 img (phdr.Phdr.offset + i)
                    with Invalid_argument _ -> 0 in
                  if u32 = 0 && i < filesize_mul_4
                  then inner_loop (i + 4) idx (size + 32) mem
                  else
                  if size == 0 then (i,u32,mem)
                  else
                    let size = size |> in_bits in
                    let address = Domain.Binary_Forward.biconst ~size:ptrsize (Z.of_int @@ vaddr + idx) ctx in
                    let zero = Domain.Binary_Forward.biconst ~size Z.zero ctx in
                    let mem = Domain.Memory_Forward.store ~size ctx mem address zero in
                    (i,u32,mem)
                in
                let (i,u32,mem) = inner_loop i i 0 mem in
                if i = filesize_mul_4 then mem
                else
                  (* let char = Loader_elf.read_offset img (phdr.p_offset + i) in *)
                  (* Logger.result "Char is %d %x" i char; *)
                  ((* Logger.result "Writing %x at address %x" char (vaddr + i); *)
                  let value = Domain.Binary_Forward.biconst ~size:(32 |> in_bits) (Z.of_int u32) ctx in
                  let address =
                    Domain.Binary_Forward.biconst ~size:ptrsize
                      (Z.of_int @@ vaddr + i) ctx in
                  let mem = Domain.Memory_Forward.store ~size:(32 |> in_bits) ctx mem address value in
                  loop (i + 4) mem)
              in
              let mem = loop 0 mem in
              let rec loop_rem i mem =
                (* Logger.result "loop_rem %x" i; *)
                if i = filesize then mem
                else
                  let value = Domain.Binary_Forward.biconst ~size:(8 |> in_bits) (Z.of_int @@ Loader_elf.read_offset img i) ctx in
                  let address = Domain.Binary_Forward.biconst ~size:ptrsize (Z.of_int @@ vaddr + i) ctx in
                  let mem = Domain.Memory_Forward.store ~size:(8 |> in_bits) ctx mem address value in
                  loop_rem (i+1) mem
              in
              loop_rem filesize_mul_4 mem

           (* XXX: Ecrire aussi le contenu de la memoire initiale. *)
          ) memory program_headers
        in
        memory

      let mk_initial_memory load_img ctx =
        let memory = Domain.Memory_Forward.unknown ~level:(Domain.Context.level ctx) ctx in
        let ptrsize = Codex.Codex_config.ptr_size() in
        (* Weak_shape_domain_memory.set_check_store_intvl false; *)
        Region.set_check_store_intvl false;
        let memory = load_img ctx memory in
        Logger.result "*** End of memory initialization. ***";
        (* Weak_shape_domain_memory.set_check_store_intvl true; *)
        Region.set_check_store_intvl true;
        memory

      let initial_memory img ctx =
        let ptrsize = Codex_config.ptr_size () in
        let img = match img with
          | Loader.PE _ | Loader.Raw _ | Loader.TI83 _ -> assert false
          | Loader.ELF img -> img in
        let load_img = load_image ptrsize img in
        let root_type_sym, root_type = assert false (* TSettingC.root *) in
        let root_type_addr = match Loader_utils.address_of_symbol_by_name ~name:root_type_sym
          (Loader.ELF img) with None -> assert false | Some a -> Virtual_address.create a in
        mk_initial_memory load_img ctx

      let concrete_initial_memory img_kernel (*img_app*) ctx =
        let ptrsize = Codex_config.ptr_size () in
        let img_kernel = match img_kernel with
          | Loader.PE _ | Loader.Raw _ | Loader.TI83 _ -> assert false
          | Loader.ELF img -> img in
        (*
        let img_app = match img_app with
          | Loader.PE _ | Loader.Raw _ | Loader.TI83 _ -> assert false
          | Loader.ELF img -> img in
        *)
        let load_img = load_image ptrsize img_kernel in
        mk_initial_memory load_img ctx

      let initial_vars ctx =
        List.fold_left (fun acc (name,size) ->
            let value = Reg.initial_value ctx (name,size) in
            (* Logger.result "creating reg %s of size %d" name size; *)
            VarMap.add name value acc
          ) VarMap.empty Reg.registers

      let initial img ctx =
        (* initialize symbol table. *)
        { ctx;
          vars = initial_vars ctx;
          memory = initial_memory img ctx;
          instruction_count = 0;
          is_bottom = false;
          never_went_to_user_code = true;
        }

      let initial_concrete img_kernel (*img_app*) ctx =
        { ctx;
          vars = initial_vars ctx;
          memory = concrete_initial_memory img_kernel (*img_app*) ctx;
          instruction_count = 0;
          is_bottom = false;
          never_went_to_user_code = true;
        }

      let empty_vars ctx =
        List.fold_left (fun acc (name,size) ->
            VarMap.add name (Domain.binary_empty ~size ctx) acc)
          VarMap.empty Reg.registers

      let reset img ctx =
        { ctx;
          vars = initial_vars ctx;
          memory = initial_memory img ctx;
          instruction_count = 0;
          is_bottom = false;
          never_went_to_user_code = true;
        }

      let is_singleton ctx ~size value =
        value
        |> Domain.Query.binary ~size ctx
        |> Domain.Query.Binary_Lattice.is_singleton ~size
        |> Option.is_some


      let get ~size state reg =
        if state.is_bottom then Domain.binary_empty ~size state.ctx else
        try VarMap.find reg state.vars
        with Not_found ->  failwith ("Could not find " ^ reg)

      let set state reg value =
        if not (VarMap.mem reg state.vars) then failwith ("Setting " ^ reg);
        {state with vars = VarMap.add reg value state.vars}

      let set state reg value =
        let size = List.assoc reg Reg.registers in
        Logger.debug ~level:2 "Setting %s to %a" reg (Domain.binary_pretty ~size state.ctx) value;
        if not state.is_bottom then
          if reg = "sp" then assert(is_singleton ~size:(32 |> Units.In_bits.of_int) state.ctx value);
          set state reg value

      let dump_state fmt state =
        let _ = Domain.Context.level state.ctx in
        if state.is_bottom then Format.fprintf fmt "Bottom" else
          Reg.registers |> List.iter (fun (reg,size) ->
              let value = VarMap.find reg state.vars in
              Format.fprintf fmt "%s -> %a@\n" reg (Domain.binary_pretty ~size state.ctx) value
            )

      let dump_state_diff fmt oldstate state address results =
        Reg.registers |> List.iter (fun (reg,size) ->
            let value = VarMap.find reg state.vars in
            let oldvalue = VarMap.find reg oldstate.vars in
            if not (Domain.Binary.equal value oldvalue)
            then
              let add = (Format.asprintf "%a" Virtual_address.pp address) in
              let res = (Format.asprintf "%s -> %a@\n" reg (Domain.binary_pretty ~size state.ctx) value) in
              Hashtbl.add results add res;
              Format.fprintf fmt "%s" res (* (Domain.binary_pretty ~size ctx) value *)
          )


      let assume cond state =
        match Domain.assume state.ctx cond with
        | None -> Log.debug (fun p -> p "Cond is %a, setting state to bottom" (Domain.boolean_pretty state.ctx) cond ); {state with is_bottom = true} (* TODO: We should probably replace is_bottom with an option type *)
        | Some ctx -> {state with ctx}

      (* let assume_pred ~size pred v state =
        let cond = Type.cond_of_pred_subdomain ~size state.ctx pred v in
        assume cond state *)

      let bottom ctx =
        (* assume ctx (Domain.Boolean_Forward.false_ ctx) *)
          { ctx;
            instruction_count = 0;
            vars = empty_vars ctx;
            memory = Domain.Memory_Forward.unknown ~level:(Domain.Context.level ctx) ctx;
            is_bottom = true;
            never_went_to_user_code = true;
          }


      (*
      let pointer_registers = [
        "eax"; "ebx"; "ecx"; "edx"; "edi"; "esp"; "ebp"; "esi";
        "res32"; "temp32"; "temp32_0"; "temp32_1"; "temp32_2"; "temp32_3";
        (* "gdt";
        ("ds_base",32); ("cs_base",32); ("ss_base",32); ("es_base",32);
        ("fs_base",32); ("gs_base",32);
        ("tr_base",32);
        ("idt",32); *)
      ] ;;
      *)

      let serialize ~widens state_a state_b (included,acc) =
        Log.debug (fun p -> p "State.serialize %a %a" dump_state state_a dump_state state_b);
        (* let state_a = if state_a.is_bottom then bottom state_a.ctx else state_a in
        let state_b = if state_b.is_bottom then bottom state_b.ctx else state_b in *)
        if state_a.is_bottom then Domain.Context.Result(included, acc, fun ctx out -> state_b, out)
        else
          if state_b.is_bottom then Domain.Context.Result(included, acc, fun ctx out -> state_a, out)
          else
          let Domain.Context.Result(included,acc,fvar) = VarMap.fold_on_diff2 state_a.vars state_b.vars
            (Domain.Context.Result(included,acc,fun _ctx out -> state_a.vars,out))
            (fun var a b (Domain.Context.Result(inc,acc,k)) ->
                match a,b with
                | Some a, Some b when a == b -> Domain.Context.Result(inc,acc,k)
                | Some a, Some b ->
                  let size = List.assoc var Reg.registers in
                  Logger.result "### register %s:@ a =@ %a,@ b =@ %a" var
                  (Domain.binary_pretty ~size state_a.ctx) a (Domain.binary_pretty ~size state_b.ctx) b;
                  let Domain.Context.Result(inc,acc,d) = Domain.serialize_binary ~widens ~size state_a.ctx a state_b.ctx b
                  (inc,acc) in
                  Domain.Context.Result(inc, acc, fun ctx out ->
                      let v,out = d ctx out in
                      let map,out = k ctx out in
                      (VarMap.add var v map, out))
                | _,_ -> assert false (* Variables should be defined in both cases *)
            ) in

          (* let entries = List.map (fun var -> (VarMap.find var state_a.vars, VarMap.find var state_b.vars)) pointer_registers in *)

          (* let Domain.Context.Result (included, acc, fmem) = Domain.serialize_memory_and_cache state_a.ctx state_a.memory state_b.ctx state_b.memory
            entries (included,acc) in *)
          let Domain.Context.Result(included,acc,fmem) = Domain.serialize_memory ~widens state_a.ctx state_a.memory state_b.ctx state_b.memory
            (included,acc) in
          Domain.Context.Result(included, acc, fun ctx out ->
              let memory,out = fmem ctx out in
              let vars,out = fvar ctx out in
              { ctx; memory;vars;
                instruction_count = max state_a.instruction_count state_b.instruction_count;
                is_bottom = state_a.is_bottom && state_b.is_bottom;
                never_went_to_user_code =
                state_a.never_went_to_user_code && state_b.never_went_to_user_code;
              }, out)
        ;;
      let join a b =
        if a.is_bottom then b else if b.is_bottom then a else
          let Domain.Context.Result(_,acc,f) = serialize ~widens:false a b
            (true, Domain.Context.empty_tuple ()) in
          let ctx, out = Domain.typed_nondet2 a.ctx b.ctx acc in
          let r, _ = f ctx out in
          (* {r with ctx} *)
          r

      let is_included a b =
        if a.is_bottom then true else if b.is_bottom then false else
          let Domain.Context.Result(included,_,_) = serialize ~widens:false b a (true, Domain.Context.empty_tuple ()) in
          included
    end


    let z_of_bitvector x = Z.of_string @@ Bitvector.to_hexstring x
    let bitvector_of_z x =
      let string = Z.format "#x" x in
      Bitvector.of_hexstring string

    let bin_of_bool ctx cond =
      Domain.Binary_Forward.bofbool ~size:Units.In_bits.one ctx cond

    let bool_of_bin ctx bin =
      let zero = Domain.Binary_Forward.biconst ~size:Units.In_bits.one Z.zero ctx in
      let condelse = Domain.Binary_Forward.beq ~size:Units.In_bits.one ctx bin zero in
      let condthen = Domain.Boolean_Forward.not ctx condelse in
      condthen

    let join_binary_and_state ~size a b =
      match a,b with
      | Some (bin_a, state_a), Some (bin_b, state_b) ->
        let Domain.Context.Result(inc, tup, fstate) = State.serialize ~widens:false state_a state_b (true, Domain.Context.empty_tuple ()) in
        let Domain.Context.Result(_, tup, fbin) = Domain.serialize_binary ~widens:false ~size state_a.ctx bin_a state_b.ctx bin_b (inc,tup) in
        let ctx, out = Domain.typed_nondet2 state_a.ctx state_b.ctx tup in
        let bin, out = fbin ctx out in
        let state, _ = fstate ctx out in
        Some (bin, state)

      | Some _ , _ -> b
      | _ -> a

    let binop_to_string bop =
      let open Dba in
      let open Binary_op in
      match bop with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | DivU -> "/u"
      | DivS -> "/s"
      | ModU -> "modu"
      | ModS -> "mod"
      | Or -> "||"
      | And -> "&&"
      | Xor -> "^"
      | Concat -> "::"
      | LShift -> "<<"
      | RShiftU -> ">>u"
      | RShiftS -> ">>"
      | LeftRotate -> "leftRotate"
      | RightRotate -> "rightRotate"
      | Eq -> "="
      | Diff -> "!="
      | LeqU -> "<=u"
      | LtU -> "<u"
      | GeqU -> ">=u"
      | GtU -> ">u"
      | LeqS -> "<="
      | LtS -> "<"
      | GeqS -> ">="
      | GtS -> ">"

    let rec print_expr fmt e =
      let size = Dba.Expr.size_of e in
      match e with
      | Var{name;_} -> Format.fprintf fmt "%s" name
      | Load(size,_,addr,_) -> Format.fprintf fmt "*(%a)" print_expr addr
      | Cst(bv) -> Bitvector.pp fmt bv
      | Unary(Dba.Unary_op.Restrict{lo;hi},e1) -> Format.fprintf fmt "%a[%d..%d]" print_expr e1 lo hi
      | Unary(Dba.Unary_op.Uext size, e1) -> Format.fprintf fmt "uext (%a)" print_expr e1
      | Unary(Dba.Unary_op.Sext size, e1) -> Format.fprintf fmt "sext (%a)" print_expr e1
      | Unary(Dba.Unary_op.Not,e) -> Format.fprintf fmt "not (%a)" print_expr e
      | Unary(Dba.Unary_op.UMinus,e) -> Format.fprintf fmt "-(%a)" print_expr e
      | Binary(bop,e1,e2) ->
          Format.fprintf fmt "(%a) %s (%a)" print_expr e1 (binop_to_string bop) print_expr e2
      | Ite (cond,e_then,e_else) ->
          Format.fprintf fmt "ite(%a, %a, %a)" print_expr cond print_expr e_then print_expr e_else


    module OptionM : sig
      val (==>) : 'a option -> ('a -> 'b list) -> 'b list
      val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
    end = struct
      let (==>) x f =
        match x with
        | None -> []
        | Some v -> (f v)

      let (>>=) = Option.bind
    end

    let binop ~size bop v1 v2 state =
      let open Dba in
      let open Binary_op in
      let open State in
      let f = match bop with
        | Plus -> Domain.Binary_Forward.biadd ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
        | Minus -> Domain.Binary_Forward.bisub ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false)
        | Mult -> Domain.Binary_Forward.bimul ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false)
        | DivU -> Domain.Binary_Forward.biudiv
        | DivS -> Domain.Binary_Forward.bisdiv
        | ModU -> Domain.Binary_Forward.biumod
        | ModS -> Domain.Binary_Forward.bismod
        | Or -> Domain.Binary_Forward.bor
        | And -> Domain.Binary_Forward.band
        | Xor -> Domain.Binary_Forward.bxor
        | Concat -> assert false
        | LShift -> Domain.Binary_Forward.bshl ~flags:(Operator.Flags.Bshl.pack ~nsw:false ~nuw:false)
        | RShiftU -> Domain.Binary_Forward.blshr
        | RShiftS -> Domain.Binary_Forward.bashr
        | LeftRotate -> assert false
        | RightRotate -> fun ~size ctx _ _ ->
          (* FIXME: right rotation is fully imprecise *)
          bunknown ~size ctx
        | Eq -> fun ~size ctx v1 v2 ->
          bin_of_bool ctx @@ Domain.Binary_Forward.beq ~size ctx v1 v2
        | Diff -> fun ~size ctx v1 v2 ->
          bin_of_bool ctx @@ Domain.Boolean_Forward.not ctx @@ Domain.Binary_Forward.beq ~size ctx v1 v2
        | LeqU ->
          fun ~size ctx v1 v2 ->
            bin_of_bool ctx @@ Domain.Binary_Forward.biule ~size ctx v1 v2
        | LtU ->
          fun ~size ctx v1 v2 ->
            let cond = Domain.Binary_Forward.biule ~size ctx v2 v1 in
            let cond = Domain.Boolean_Forward.not ctx cond in
            bin_of_bool ctx cond
        | GeqU ->
          fun ~size ctx v1 v2 ->
            let cond = Domain.Binary_Forward.biule ~size ctx v2 v1 in
            bin_of_bool ctx cond
        | GtU ->
          fun ~size ctx v1 v2 ->
            let cond = Domain.Binary_Forward.biule ~size ctx v1 v2 in
            let cond = Domain.Boolean_Forward.not ctx cond in
          bin_of_bool ctx cond
        | LeqS ->
          fun ~size ctx v1 v2 ->
            let cond = Domain.Binary_Forward.bisle ~size ctx v1 v2 in
            bin_of_bool ctx cond
        | LtS ->
          fun ~size ctx v1 v2 ->
            let cond = Domain.Binary_Forward.bisle ~size ctx v2 v1 in
            let cond = Domain.Boolean_Forward.not ctx cond in
            bin_of_bool ctx cond
        | GeqS -> assert false
        | GtS -> fun ~size ctx v1 v2 ->
            let cond = Domain.Binary_Forward.bisle ~size ctx v1 v2 in
            let cond = Domain.Boolean_Forward.not ctx cond in
            bin_of_bool ctx cond
      in
      match bop with
      | Plus ->
          let c = Domain.Binary_Forward.valid_ptr_arith ~size Operator.Plus state.ctx v1 v2 in
          Logger.check "ptr_arith";
          begin match Domain.query_boolean state.ctx c with
          | Lattices.Quadrivalent.(True | Bottom) ->
              Domain.Binary_Forward.biadd ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ~size state.ctx v1 v2, state
          | Lattices.Quadrivalent.(False | Top) ->
              Logger.alarm Operator.Alarm.Ptr_arith;
              let state = State.assume c state in
              let res = Domain.Binary_Forward.biadd ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ~size state.ctx v1 v2 in
              res, state
          end
      | Minus ->
          (* let minus_v2 = Domain.Binary_Forward.bisub ~size ~nsw:false ~nuw:false ~nusw:false state.ctx
            (Domain.Binary_Forward.biconst ~size Z.zero state.ctx) v2 in
          let c = Domain.Binary_Forward.valid_ptr_arith ~size state.ctx v1 minus_v2 in *)
          let c = Domain.Binary_Forward.valid_ptr_arith ~size Operator.Minus state.ctx v1 v2 in
          Logger.check "ptr_arith";
          begin match Domain.query_boolean state.ctx c with
          | Lattices.Quadrivalent.(True | Bottom) ->
              Domain.Binary_Forward.bisub ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ~size state.ctx v1 v2, state
          | Lattices.Quadrivalent.(False | Top) ->
              Logger.alarm Operator.Alarm.Ptr_arith;
              let state = State.assume  c state in
              let res = Domain.Binary_Forward.bisub ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ~size state.ctx v1 v2 in
              res, state
          end
      | _ -> f ~size state.ctx v1 v2, state

    let rec expr' (e,locid) state =
      let open OptionM in
      let open Dba.Expr in
      let open Dba.Binary_op in
      let open State in
      let open Basic_types in
      let size = Dba.Expr.size_of e |> Units.In_bits.of_int in
      let expr ~idx target_e state =
        let located_e = Binsec2syntax_tree.check_expr_idx (e,locid) ~idx target_e in
        expr located_e state
      in
      match e with
      | Var{name;_} -> Some (State.get ~size state name, state)
      | Load(size_in_bytes,_,addr,_) ->
        let size = Units.In_bytes.of_int size_in_bytes |> Units.In_bytes.in_bits in
        expr ~idx:0 addr state >>= fun (loc,state) ->
          Logger.check "array_offset_access";
        let valid = Domain.Binary_Forward.valid ~size
          Operator.Read state.ctx loc in
        let state =
          begin match Domain.query_boolean state.ctx valid with
          | Lattices.Quadrivalent.(True | Bottom) -> state
          | _ ->
              let state = State.assume valid state in
              (*
              (* VERY UGLY *)
              (* we also verify (and if not, assume) that the index is smaller than
               * the array size *)
              let open TypedC in
              let state = match loc with
              | Region_binary_inst.T {typ = Type.ParamT ({descr = Ptr{pointed={descr = Array (_, ar_sz);_};_};_}, idx, _, _); _} ->
                  let isize = Type.array_index_size in
                  let module BR = Region_numeric.Operable_Value in
                  let binary_of_value ~size ctx = function
                  | Const x -> BR.Binary_Forward.biconst ~size x ctx
                  | Sym s -> Type.symbol ctx s
                  in
                  begin match ar_sz with
                  | Some sz ->
                      (* [i <u s] implies that [i <s s], provided that [s >=s 0].
                       * Here we assume [s >=s 0].
                       * [i <u s] also implies [i <=s s], which is what we
                       * need to verify. Incidentally, [i <u s] is the
                       * condition that will be the easiest to verify with our
                       * term based domain and hypotheses on array sizes. *)
                      let sz_b = binary_of_value ~size:isize ctx sz in
                      let c3 = BR.Scalar.Boolean_Forward.not ctx @@
                        BR.Binary_Forward.biule ~size:isize ctx sz_b idx in
                      begin match Domain.Query.(convert_to_quadrivalent (boolean ctx c3)) with
                      | Lattices.Quadrivalent.True | Lattices.Quadrivalent.Bottom -> state
                      | _ ->
                          *)
                        Emit_alarm.emit_alarm Operator.Alarm.Invalid_load_access;
                        (*
                        State.assume ctx c3 state
                      end
                  | None -> state
                  end
              | _ -> assert false (* should not happen if condition was not verified *)
              in
              *)
              state
          end in
        let res, mem = Domain.Memory_Forward.load ~size state.ctx
          state.State.memory loc in
        Some (res, {state with State.memory = mem})
      | Cst(bv) ->
        let size = Bitvector.size_of bv |> in_bits in
        let value = z_of_bitvector bv in
        Some (Domain.Binary_Forward.biconst ~size value state.ctx, state)
      | Unary(Dba.Unary_op.Restrict{lo;hi},e1) ->
        let oldsize = Dba.Expr.size_of e1 |> in_bits in
        assert((size:>int) == 1 + hi - lo);
        expr ~idx:0 e1 state >>= fun (v,state) ->
        Some (Domain.Binary_Forward.bextract ~size ~index:(lo |> in_bits) ~oldsize state.ctx v, state)
      | Unary(Dba.Unary_op.Uext size, e1) ->
        let oldsize = Dba.Expr.size_of e1 |> in_bits in
        expr ~idx:0 e1 state >>= fun (v1,state) ->
        Some (Domain.Binary_Forward.buext ~size:(size |> in_bits) ~oldsize state.ctx v1,state)
      | Unary(Dba.Unary_op.Sext size, e1) ->
        let oldsize = Dba.Expr.size_of e1 |> in_bits in
        expr ~idx:0 e1 state >>= fun (v1,state) ->
        Some (Domain.Binary_Forward.bsext ~size:(size |> in_bits) ~oldsize state.ctx v1,state)
      | Unary(Dba.Unary_op.Not,e) ->
          expr ~idx:0 e state >>= fun (v,state) ->
          if (size:>int) = 1 then
            let v = Domain.Boolean_Forward.not state.ctx (bool_of_bin state.ctx v) in
            Some (bin_of_bool state.ctx v,state)
          else
            let zero = Domain.Binary_Forward.biconst ~size Z.zero state.ctx in
            let v = Domain.Binary_Forward.bxor ~size state.ctx v zero in
            Some (v,state)
      | Unary(Dba.Unary_op.UMinus,e) ->
          expr ~idx:0 e state >>= fun (v,state) ->
          let v = Domain.Binary_Forward.bisub ~size
              ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) state.ctx
              (Domain.Binary_Forward.biconst ~size Z.zero state.ctx) v in
          Some (v,state)
      | Binary((RightRotate|LeftRotate),e1,Dba.Expr.Cst(bv))
        when Bitvector.is_zeros bv -> expr ~idx:0 e1 state
      | Binary(Dba.Binary_op.Concat, e1, e2) ->
        expr ~idx:0 e1 state >>= fun (v1,state) ->
        expr ~idx:1 e2 state >>= fun (v2,state) ->
        let size1 = Dba.Expr.size_of e1 |> in_bits in
        let size2 = Dba.Expr.size_of e2 |> in_bits in
        Some (Domain.Binary_Forward.bconcat ~size1 ~size2 state.ctx v1 v2, state)
      | Binary(bop, (Cst (bv1) as e1), e2) ->
        expr ~idx:0 e1 state >>= fun (v1,state) ->
        expr ~idx:1 e2 state >>= fun (v2, state) ->
        let size1 = Dba.Expr.size_of e1 |> in_bits in
        let size2 = Dba.Expr.size_of e2 |> in_bits in
        let v1' = if size1 <> size2
          then
            let value1 = z_of_bitvector bv1 in
            Domain.Binary_Forward.biconst ~size:size2 value1 state.ctx
          else v1
        in
        Some (binop ~size:size2 bop v1' v2 state)
      | Binary(bop, e1, (Cst (bv2) as e2)) ->
        expr ~idx:0 e1 state >>= fun (v1,state) ->
        expr ~idx:1 e2 state >>= fun (v2,state) ->
        let size1 = Dba.Expr.size_of e1 |> in_bits in
        let size2 = Dba.Expr.size_of e2 |> in_bits in
        let v2' = if size1 <> size2
          then
            let value2 = z_of_bitvector bv2 in
            Domain.Binary_Forward.biconst ~size:size1 value2 state.ctx
          else v2
        in
        Some (binop ~size:size1 bop v1 v2' state)
      | Binary(bop,e1,e2) ->
        expr ~idx:0 e1 state >>= fun (v1,state) ->
        expr ~idx:1 e2 state >>= fun (v2,state) ->
        let size1 = Dba.Expr.size_of e1 |> in_bits in
        let size2 = Dba.Expr.size_of e2 |> in_bits in
        assert(size1 == size2);
        assert(size == In_bits.one || size == size1); (* Predicate, or regular binary operator. *)
        Some (binop ~size:size1 bop v1 v2 state)
      | Ite (cond,e_then,e_else) ->
        expr ~idx:0 cond state >>= fun (cond_abs, state) ->
        let zero = Domain.Binary_Forward.biconst ~size:In_bits.one Z.zero state.ctx in
        let condelse = Domain.Binary_Forward.beq ~size:In_bits.one state.ctx cond_abs zero in
        let condthen = Domain.Boolean_Forward.not state.ctx condelse in
        begin match Domain.query_boolean state.ctx condelse with
        | Lattices.Quadrivalent.Bottom -> None
        | Lattices.Quadrivalent.True ->
          (* Careful: we tested the value of the "else" condition, so in this
           * case we must take the "else" branch *)
          expr ~idx:2 e_else state
        | Lattices.Quadrivalent.False -> expr ~idx:1 e_then state
        | Lattices.Quadrivalent.Top ->
          (* Codex_log.debug "passing by If Then Else expression : joining paths" ; *)
          let state_then = State.assume condthen state in
          let state_else = State.assume condelse state in
          join_binary_and_state ~size (expr ~idx:1 e_then state_then) (expr ~idx:2 e_else state_else)
        end

    and expr (e,locid) state =
      LogExpression.trace (fun p -> p "%a" Dba_printer.Ascii.pp_expr e)
        ~loc:(Codex_options.Location.Dba_expr {locid; dba_expr=e})
        ~bintrace:locid
        ~pp_ret:(fun fmt res ->
            let size = Dba.Expr.size_of e |> in_bits in
            match res with
            | None -> Format.fprintf fmt "None"
            | Some (x,state) ->
              let ctx = state.State.ctx in
              Domain.binary_pretty ~size ctx fmt x)
        (fun () -> expr' (e,locid) state)

    let static_target_to_address =
      let open Dba in function
      | JInner id -> Jump_Inner id
      | JOuter {base;id}->
        assert(id == 0); Jump_Outer base

    let _start =
      let cache = ref None in
      function img ->
      match !cache with
      | Some a -> a
      | None ->
        begin match Loader_utils.address_of_symbol_by_name ~name:"_start" (Lazy.force img) with
        | None -> assert false
        | Some a -> cache := Some a; a
        end

    let _end_of_kernel =
      let cache = ref None in
      function img ->
      match !cache with
      | Some a -> a
      | None ->
        begin match Loader_utils.address_of_symbol_by_name ~name:"_end_of_kernel" (Lazy.force img) with
        | None -> assert false
        | Some a -> cache := Some a; a
        end

    let _idle =
      let cache = ref None in
      function img ->
      match !cache with
      | Some a -> a
      | None ->
        begin match Loader_utils.address_of_symbol_by_name ~name:"idle" (Lazy.force img) with
        | None -> assert false
        | Some a -> cache := Some a; a
        end

    let instr' state (instr,locid) =
      let open Dba in
      let open State in
      let open OptionM in
      let open Dba.Instr in
      let expr ~idx e state =
        (* Check that the idx correspond to the expression obtained by
           pattern matching. *)
        let located_e = Binsec2syntax_tree.check_instr_idx (instr,locid) ~idx e in
        expr located_e state
      in
      match instr with
        | Assign(LValue.Var{name=v;_},e,id) ->
          (*let bin,state = expr ctx e state in*)
          expr ~idx:0 e state ==> fun (bin,state) ->
          [Jump_Inner id, State.set state v bin]
        | Assign(LValue.Store(size,_,addr,_),e,id) ->
          expr ~idx:1 e state ==> fun (v,state) ->
          expr ~idx:0 addr state ==> fun (loc,state) ->
          let size = ((size * 8) |> in_bits) in
          let valid = Domain.Binary_Forward.valid ~size
              Operator.Write state.ctx loc in
          let state =
            begin match Domain.query_boolean state.ctx valid with
            | Lattices.Quadrivalent.(True | Bottom) -> state
            | _ ->
                let state = State.assume valid state in
                Emit_alarm.emit_alarm Operator.Alarm.Invalid_store_access ;
                (*
                (* VERY UGLY *)
                (* we also verify (and if not, assume) that the index is smaller than
                 * the array size *)
                let open TypedC in
                let state = match loc with
                | Region_binary_inst.T {typ = Type.ParamT ({descr = Ptr{pointed={descr = Array (_, ar_sz);_};_};_}, idx, _, _); _} ->
                    let isize = Type.array_index_size in
                    let module BR = Region_numeric.Operable_Value in
                    let binary_of_value ~size ctx = function
                    | Const x -> BR.Binary_Forward.biconst ~size x ctx
                    | Sym s -> Type.symbol ctx s
                    in
                    begin match ar_sz with
                    | Some sz ->
                        (* [i <u s] implies that [i <s s], provided that [s >=s 0].
                         * Here we assume [s >=s 0].
                         * [i <u s] also implies [i <=s s], which is what we
                         * need to verify. Incidentally, [i <u s] is the
                         * condition that will be the easiest to verify with our
                         * term based domain and hypotheses on array sizes. *)
                        let sz_b = binary_of_value ~size:isize ctx sz in
                        let c3 = BR.Scalar.Boolean_Forward.not ctx @@
                          BR.Binary_Forward.biule ~size:isize ctx sz_b idx in
                        begin match Domain.Query.(convert_to_quadrivalent (boolean ctx c3)) with
                        | Lattices.Quadrivalent.True | Lattices.Quadrivalent.Bottom -> state
                        | _ ->
                          Logger.error "-alarm- array_offset_access";
                          State.assume ctx c3 state
                        end
                    | None -> state
                    end
                | _ -> assert false (* should not happen if condition was not verified *)
                in
                *)
                state
            end in
          let memory =
            Domain.Memory_Forward.store ~size state.ctx state.State.memory loc v
          in
          [Jump_Inner id,{state with State.memory}]
        | Assign(LValue.Restrict({name=v;_}, Interval.({lo;hi})), e_rval, id) ->
          let size_v = List.assoc v Reg.registers in
          expr ~idx:0 e_rval state ==> fun (rval,state) ->
          Logger.debug ~level:3 "%s{%d,%d} := %a" v lo hi (Domain.binary_pretty ~size:size_v state.ctx) rval;
          let lo = lo |> in_bits and hi = hi |> in_bits in
          let initial = State.get ~size:size_v state v in
          let written_size = In_bits.(one + hi - lo) in
          let new_v =
            if lo = In_bits.zero then rval
            else
              let lsb = Domain.Binary_Forward.bextract ~size:lo ~index:In_bits.zero
                ~oldsize:size_v state.ctx initial in
              Domain.Binary_Forward.bconcat ~size1:written_size ~size2:lo state.ctx rval lsb
          in
          let new_v =
            if hi = In_bits.(size_v - one) then new_v
            else
              let msb = Domain.Binary_Forward.bextract ~size:In_bits.(size_v - hi - one)
                ~index:In_bits.(hi+one) ~oldsize:size_v state.ctx initial in
              Domain.Binary_Forward.bconcat ~size1:In_bits.(size_v - hi - one)
                ~size2:In_bits.(hi+one) state.ctx msb new_v
          in
          [Jump_Inner id, State.set state v new_v]
        | If(cond,target,id) ->
          expr ~idx:0 cond state ==> fun (bin,state) ->
          let zero = Domain.Binary_Forward.biconst ~size:In_bits.one Z.zero state.ctx in
          let condelse = Domain.Binary_Forward.beq ~size:In_bits.one state.ctx bin zero in
          let condthen = Domain.Boolean_Forward.not state.ctx condelse in
          (match Domain.query_boolean state.ctx condelse with
          | Lattices.Quadrivalent.Bottom -> []
          | Lattices.Quadrivalent.True ->
              (* Careful: we tested the value of the "else" condition, so in this
               * case we must take the "else" branch *)
              [Jump_Inner id,state]
          | Lattices.Quadrivalent.False -> [static_target_to_address target,state]
          | _ ->
            let state_then = State.assume condthen state in
            let state_else = State.assume condelse state in
            [(static_target_to_address target,state_then);(Jump_Inner id,state_else)]
          )
        | Stop _ -> Logger.result "Stop called"; []
        | Assert(Dba.Expr.Cst x,_) when Bitvector.is_zero x ->
            Logger.result "Warning: assert false"; []
        | Assert (cond, id) ->
          expr ~idx:0 cond state ==> fun (cond_v, state) ->
          let zero = Domain.Binary_Forward.biconst ~size:In_bits.one Z.zero state.ctx in
          let notc = Domain.Binary_Forward.beq ~size:In_bits.one state.ctx cond_v zero in
          let cond = Domain.Boolean_Forward.not state.ctx notc in
          let c = Domain.query_boolean state.ctx cond in
          let open Lattices.Quadrivalent in
          begin match c with
          | False ->
            Logger.fatal "assert %a may be false" (Domain.binary_pretty ~size:In_bits.one state.ctx) cond_v;
          | Top ->
            Log.debug (fun p -> p "assert %a may be false" (Domain.binary_pretty ~size:In_bits.one state.ctx) cond_v);
            Emit_alarm.emit_alarm Operator.Alarm.Possibly_false_assertion;
            [Jump_Inner id, State.assume cond state]
          | Bottom -> []
          | True -> [Jump_Inner id, state]
          end
        | Assume _ -> assert false
        | SJump(jt,_) ->
          let target_addr = static_target_to_address jt in
          (*
          begin match target_addr with
          | Jump_Outer target_addr ->
              let lazy_img = lazy (Kernel_functions.get_img ()) in
              if Virtual_address.to_int target_addr = idle lazy_img then
                (* Go to mock address where the abstract task simulator should be hooked *)
                [Jump_Outer (Virtual_address.create 0xcafecaf0), state]
              else [Jump_Outer target_addr, state]
          | Jump_Inner _ | Jump_Dynamic ->*) [target_addr, state]
          (*end*)
        | DJump(e,_) ->

          let jump (v, state) =
            let size = Dba.Expr.size_of e |> in_bits  in
            let inf = Z.zero and sup = Z.shift_left Z.one (size:>int) in
            (* Codex_log.debug "jump expression : %a" Dba_printer.Unicode.pp_expr e ; *)
            (* Codex_log.debug "in dynamic jump : %a" (Domain.binary_pretty ~size ctx) v ; *)
            let res = (Domain.Query.binary ~size state.ctx v) in
            Logger.check "unresolved_dynamic_jump";
            begin
              Domain.Query.Binary_Lattice.fold_crop_unsigned ~size res ~inf ~sup []
                (fun i acc ->
                  (Jump_Outer (Virtual_address.of_bitvector @@ bitvector_of_z i), state):: acc)
            end
          in
            (* Codex_log.debug "passing in DJump instruction" ; *)
            let ptr_size = Codex_config.ptr_size () in
            begin match e with
              | Load (size,_,(Binary(Plus, e1, e2) as plus_e),_) when size = 4 ->
                (* Possibly reading from a jump table. Do not join before going to each target,
                   and assume that the index is equal to the corresponding value in each case. *)
                Log.debug (fun p -> p "Possibly reading from a jump table");
                (* Log.trace ~loc:(Codex_options.Location.Dba_expr {dba_expr = plus_e}) (fun p -> p "In expression") @@ fun () -> *)
                (* TODO: the idx is wrong here. *)
              expr ~idx:(-1) e1 state ==> fun (idx, state1) ->
                let res =
                  Domain.Query.binary ~size:(size * 8 |> in_bits) state.ctx idx |> Domain.Query.Binary_Lattice.to_unsigned_interval ~size:ptr_size |> fun (min,max) -> Ival.inject_range (Some min) (Some max) in
                (* let res = Domain.Query.binary_to_ival ~signed:false ~size:32 @@ Domain.Query.binary ~size state.ctx idx in *)
                if (not @@ Ival.cardinal_zero_or_one res) && Ival.cardinal_is_less_than res 8  (* 1 < cardinal < 8 *)
                  && Ival.fold_int (fun elt acc -> (Z.equal Z.zero @@ Z.rem elt Z.(of_int 4)) && acc) res true then
                    let cases =
                    Ival.fold_int (fun elt cases ->
                      let value = Domain.Binary_Forward.biconst ~size:ptr_size elt state.ctx in
                      let cond = Domain.Binary_Forward.beq ~size:ptr_size state.ctx value idx in
                      (* let new_state = State.assume ctx cond state1 in *)
                      let new_state = State.assume cond state in
                      (expr ~idx:0 e new_state ==> jump) @ cases
                    ) res []
                    in cases

                else expr ~idx:0 e state ==> jump
            | _ ->
              expr ~idx:0 e state ==> jump
            end

        | Undef (Dba.LValue.Var{name=v;_}, id) ->
          let size = List.assoc v Reg.registers in
          let new_v = bunknown ~size state.ctx in
          [Jump_Inner id, State.set state v new_v]
        | _ -> assert false
    let instr state (instruction,locid) =
      LogDbaInstr.trace
      (fun p -> p "%a"
        Dba_printer.Ascii.pp_instruction instruction)
      ~loc:(Codex_options.Location.Dba_instr{locid; dba_instr=instruction})
      ~bintrace:locid
      (fun _ -> instr' state (instruction,locid))

    end


end
