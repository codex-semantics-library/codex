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

open Frama_c_kernel
module Codex = Codex
module VarMap = Codex.Extstdlib.Map.Make(Cil_datatype.Varinfo);;
module StringMap = Map.Make(String);;
let increase_size = Codex_config.extend_size_for_additive_operations

module In_bytes = Units.In_bytes
module In_bits = Units.In_bits
let in_bytes = Units.In_bytes.of_int
let in_bits = Units.In_bits.of_int

(* We use a kinstr because lval that do not have eids, and eids
   are sometimes incorrect... *)
module ExpInStmt =
  Datatype.Pair_with_collections
    (Cil_datatype.Kinstr)
    (Cil_datatype.Exp)

module Log = Tracelog.Make(struct let category = "C2Codex" end);;

(* Fresh symbolic symbols from existential functions *)
let fresh_int =
  let fresh_counter = ref (0 : int) in
  fun () ->
    incr fresh_counter ;
    !fresh_counter

let fresh_symbol () = Format.sprintf "&%d" (fresh_int ()) ;;

module Compile_type = struct

  (* Hashtbl.Make(struct include String let hash = Hashtbl.hash end) *)

  let being_built = Cil_datatype.Typ.Hashtbl.create 17;;

  let has_definition constr =
    let module TypedC = Codex.Types.TypedC in
    match TypedC.constr_of_name constr with Some _ -> true | None -> false

  let rec cil_type_to_ctype typ =
    (* Log.notice (fun p -> p "cil_type_to_ctype %a %a" Cil_datatype.Typ.pretty typ Cil_datatype.Typ.pretty (Ast_types.unroll typ)); *)
    let open Cil_types in
    let module TypedC = Codex.Types.TypedC in
    Cil_datatype.Typ.Hashtbl.replace being_built typ ();
    let t = Ast_types.unroll typ in
    match t.tnode with
    | TVoid ->
      TypedC.({descr = Void; pred=Pred.true_})
    | TInt (IInt | IUInt) ->
      let size = Cil.bytesSizeOf typ in
      assert (size = 4);
      let constr = TypedC.(Constr.make (ConstrName "int") 0) in
      TypedC.({descr=Application{constr;args = []};pred=Pred.true_})
    | TInt (IChar | IUChar) ->
      let constr = TypedC.(Constr.make (ConstrName "char") 0) in
      TypedC.({descr=Application{constr; args =[]};pred=Pred.true_})
    | TPtr ({tnode = TComp{cstruct;cname;cfields;_}} as comp_typ) ->
      (* Log.notice (fun p -> p "Ptr Struct case"); *)
      let cname = TypedC.(if cstruct then ConstrNameStruct cname else ConstrNameUnion cname) in
      let constr = TypedC.(Constr.make cname 0) in
      let pointed = TypedC.{descr=Application{constr;args = []}; pred=Pred.true_;} in
      (* Build the definition for the sub type if needed, and if the type is not abstract. *)
      (if cfields <> None
       && not (has_definition constr)
       && not (Cil_datatype.Typ.Hashtbl.mem being_built comp_typ)
       then
         let _ = cil_type_to_ctype comp_typ in ());
      TypedC.(Build.ptr_to_name {constr; args =[]})

    | TFloat FFloat ->
      let size = Cil.bytesSizeOf typ in
      TypedC.({descr = Base (size |> in_bytes, "float"); pred=Pred.true_})
    | TFloat FDouble ->
      let size = Cil.bytesSizeOf typ in
      TypedC.({descr = Base (size |> in_bytes, "double"); pred=Pred.true_})
    | TInt _ ->
      (* Pointers to incomplete types are seen as a special kind of word with no meaning. *)
      let size = Cil.bytesSizeOf typ in
      TypedC.({descr = Base (size |> in_bytes, "anyIntegerType"); pred=Pred.true_})
    | TEnum {ename; ekind=IUInt; eitems} ->
      let size = Cil.bytesSizeOf typ in
      assert (size = 4) ;
      let nb_items = List.length eitems in
      let pred = TypedC.Pred.(conjunction (Cmp(SGeq,Self,Const Z.zero)) (Cmp(SLt,Self,Const (Z.of_int nb_items)))) in
      TypedC.({descr = Base (size |> in_bytes, "int"); pred})

    | TPtr pointed ->
      let ptyp = cil_type_to_ctype pointed in
      let pred = if List.exists (function ("notnull",_) -> true | _ -> false) t.tattr
        then TypedC.Pred.(Cmp(NotEqual,Self,Const Z.zero))
        else TypedC.Pred.true_
      in
      TypedC.(Build.ptr ptyp pred)
    | TComp {cname;cfields;cstruct=true;_} ->
      begin
        let constr = TypedC.(Constr.make (ConstrNameStruct cname) 0) in
        let named_typ = TypedC.{descr=Application{constr;args = []}; pred=Pred.true_;} in
        (* Is it defined? *)
        match TypedC.constr_of_name constr with
        | Some _ -> named_typ
        | None ->
          (* Log.notice (fun p -> p "No definition for %a" TypedC.Constr.pp constr); *)
          (* In the C level, we attach a named type to each C struct. *)
          (* Then, in the type level, we attach a struct type to the named type. *)
          let st_name = Some cname in
          let st_byte_size = Cil.bytesSizeOf typ |> in_bytes in
          let st_members =        (* offset, name, type *)
            match cfields with
            | None -> assert false (* Not handled if not a pointer to it. *)
            | Some fields ->
              (* Put normal fields; add padding and replace bitfields by ints as needed. *)
              let (st_members,last_offset,_) =
                List.fold_left (fun (acc,(last_offset:In_bytes.t),num_padding) fi ->
                    if(fi.fbitfield <> None) then (acc,last_offset,num_padding)
                    (* we see bitfields like padding bytes. *)
                    else
                      let (field_offset,field_size) = Cil.fieldBitsOffset fi in
                      (* Codex_log.feedback "Field %s offset %d size %d " fi.fname field_offset field_size; *)
                      assert(field_offset mod 8 == 0 && field_size mod 8 == 0);
                      let field_offset = (field_offset / 8) |> in_bytes
                      and field_size = (field_size / 8) |> in_bytes in
                      let acc,num_padding =
                        if field_offset != last_offset
                        (* Need to add a padding or bitfield field. *)
                        then
                          let name = cname ^ "_padding_" ^ (string_of_int num_padding) in
                          let size = In_bytes.(field_offset - last_offset) in
                          let typ = TypedC.({descr = Base (size, name); pred=Pred.true_}) in
                          ((field_offset,name,typ)::acc,num_padding + 1)
                        else
                          acc,num_padding
                      in
                      let last_offset = In_bytes.(field_offset + field_size) in
                      ((field_offset,fi.fname,cil_type_to_ctype fi.ftype)::acc,last_offset,num_padding)
                  ) ([],In_bytes.zero,0) fields
              in
              let st_members =
                if(last_offset = st_byte_size)
                then st_members
                else
                  let name = cname ^ "_last_padding" in
                  let size = Units.In_bytes.(st_byte_size - last_offset) in
                  let typ = TypedC.({descr = Base (size, name); pred=Pred.true_}) in
                  (last_offset, name, typ)::st_members
              in
              List.rev st_members
          in
          (* TODO: Check attributes for better translation of predicates from C types. *)
          let struct_type = TypedC.{ descr = Structure { st_byte_size;
                                                         st_members};
                                     pred = Pred.true_ }
          in
          TypedC.add_constr_definition constr (struct_type,[]);
          named_typ
      end
    | TArray (typ,size) ->
      let size = match size with
        | None -> None
        | Some e -> try Some(TypedC.Fixed_length(Cil.lenOfArray64 size)) with Cil.LenOfArray _ -> None in
      begin match size with
        | Some size ->
          let descr = TypedC.Array(cil_type_to_ctype typ,size) in
          TypedC.{descr;pred=Pred.true_}
        | None ->
          let bound_var = "len" in
          let descr = TypedC.Array(cil_type_to_ctype typ,Variable_length bound_var) in
          let body = TypedC.{descr;pred=Pred.true_} in
          let bound_typ = Types.Parse_ctypes.type_of_string "int" in
          TypedC.{descr=Existential{bound_typ;bound_var;body};pred=Pred.true_}
      end
    | _ ->
      raise (Invalid_argument (Format.asprintf "cil_type_to_my_type: %a" Cil_types_debug.pp_typ t))

end


module type CallingContext = sig
  include Datatype_sig.S
  val get: unit -> t
end

(* module Make(CallingContext:CallingContext)(Domain:Codex.Domains.Domain_sig.Base) = struct *)
module Make(CallingContext:CallingContext)(Domain:Codex.Domains.Memory_domains.With_focusing.S_with_types) = struct

  let exploring_pure_function = ref false ;;

  (* The context in which an instruction is called. *)
  type context = {

    (* We follow a recursive iteration strategy. This is the number of loops in which we are iterating;
       0 means that we are not in a loop (and thus the results that we compute are definitive). *)
    loop_nesting_level: int;
    (* Current statement (or global if called outside of a statement). *)
    kinstr : Cil_types.kinstr;
    (* Analysis context, representing the current basic block in the current calling context. *)
    ctx: Domain.Context.t;
  }
  ;;

  (* Overrides actual domain module for it to deal with function pointers *)

  let function_to_ctype kf =
    let open Types.TypedC in
    let name = Kernel_function.get_name kf in
    match function_of_name name with
    | Some typ -> typ
    | None -> begin
        let formals = Kernel_function.get_formals kf in
        let ret = Compile_type.cil_type_to_ctype @@ Kernel_function.get_return_type kf in
        let args = formals |> List.map (fun vi ->
            let size = Cil.bitsSizeOf vi.Cil_types.vtype in
            let ctyp = Compile_type.cil_type_to_ctype vi.vtype in
            ctyp)
        in {descr = Function {ret; args; pure = false}; pred = Pred.True}
      end



  (* This hack consists in adding a mapping from Domain.binary to kf
     to retrieve functions from their values.  However, it only works
     if the function was not modified (e.g. no +1-1 on the function
     pointer, and more importantly, no join between several function
     pointers). Instead, we should be able to inspect a Domain.binary,
     list all the globals to which it points, to retrieve all the
     pointers. *)
  module Domain = struct
    include Domain

    (* Utility functions and map to deal notably with function pointers *)
    module H = Hashtbl.Make(Domain.Binary)
    let fun_map : (Cil_types.varinfo) H.t = H.create 12345 ;;

    let add_pointed_function ptr v = H.add fun_map ptr v
    let get_pointed_function ptr = H.find fun_map ptr
    let is_function_pointer ptr = H.mem fun_map ptr

    let binary_pretty ~size ctx fmt value =
      if is_function_pointer value then
        let v = get_pointed_function value in
        Format.fprintf fmt "(@[<hov 2>%a@] -> &%a}" (Domain.binary_pretty ~size ctx) value Cil_datatype.Varinfo.pretty v

      else Domain.binary_pretty ~size ctx fmt value

  end

  (* A state is either bottom, or a tuple with a memory, a mapping
     from variable names to their addresses, and a mapping from
     strings to their addresses. *)
  type state = { mem: Domain.memory;
                 var_addresses : Domain.binary VarMap.t;
                 string_addresses: Domain.binary StringMap.t;
                 context: context;
               }


  let pretty_state fmt state =
    Format.fprintf fmt "@[<v>loop nesting:%d@ kinstr:%a@ mem:%a@ ctx:%a@]"
      state.context.loop_nesting_level
      Cil_datatype.Kinstr.pretty state.context.kinstr
      (Domain.memory_pretty state.context.ctx) state.mem
      Domain.context_pretty state.context.ctx
  ;;

  let pretty_state_option fmt state =
    match state with
    | None -> Format.fprintf fmt "<no state>"
    | Some s -> pretty_state fmt s



  (* Table where to register assertions. Note that it is imperative, but
     it should be easy to switch to a functional version (using the
     state monad to pass it). *)
  module Assertion_Table = struct

    (* module KinstrHash = Hashtbl.Make(Cil_datatype.Kinstr);; *)
    (* let alarm_table = KinstrHash.create 17;; *)

    (* We want a lexical order with the locations first, for better display. *)
    module AlarmMap = Map.Make(struct
        include Datatype_sig.Prod2(Alarms)(Cil_datatype.Location)
        let compare (a1,loc1) (a2,loc2) =
          let r = Cil_datatype.Location.compare loc1 loc2 in
          if r == 0 then
            Alarms.compare a1 a2
          else
            r
      end);;

    module LocationMap = Cil_datatype.Location.Map;;

    (* We want a fix order, to display the list of alarms. *)
    let alarm_table = ref AlarmMap.empty;;
    let assertion_table = ref LocationMap.empty;;

    let register_alarm (b:Codex.Lattices.Quadrivalent.t) (a,loc,ki) state =
      let level = state.context.loop_nesting_level in
      if level > 0 then ()
      else
        match AlarmMap.find (a,loc) !alarm_table with
        | exception Not_found -> alarm_table := AlarmMap.add (a,loc) b !alarm_table
        | x -> alarm_table := AlarmMap.add (a,loc) (Lattices.Quadrivalent.join x b) !alarm_table
    ;;

    let register_assertion loc bool state =
      let ctx = state.context.ctx in
      let level = state.context.loop_nesting_level in
      if level > 0 then ()
      else begin
        let qbool = Domain.query_boolean ctx bool in
        let status,how =
          match qbool with
          | Lattices.Quadrivalent.True -> "TRUE (valid)", "abstract interpretation"
          | Lattices.Quadrivalent.Bottom -> "TRUE (dead)", "abstract interpretation"
          | Lattices.Quadrivalent.False -> "FALSE (invalid)", "abstract interpretation"
          | Lattices.Quadrivalent.Top -> begin
              if not @@ Codex_options.TryHard.get()
              then "UNKNOWN", "abstract interpretation only"
              else match Domain.satisfiable state.context.ctx
                           (Domain.Boolean_Forward.not state.context.ctx bool) with
              | Smtbackend.Smtlib_sig.Unsat -> "TRUE (valid)", "smt solving"
              | Smtbackend.Smtlib_sig.Unknown -> "UNKNOWN", "neither abstract interpretation nor smt solving"
              | Smtbackend.Smtlib_sig.Sat _ -> "FALSE (counter-example exists)", "smt solving"
            end
        in
        assertion_table := LocationMap.update loc (function
            | None -> Some [(status,how)]
            | Some l -> Some ((status,how)::l)
          ) !assertion_table
      end
        ;;

    let register_reachability_check loc state =
      let ctx = state.context.ctx in
      let level = state.context.loop_nesting_level in
      if level > 0 then ()
      else begin
        let qbool = Domain.Query.reachable ctx state.mem in
        let unreachable,how =
          match qbool with
          | Lattices.Quadrivalent.(False | Bottom) -> "TRUE (unreachable)", "abstract interpretation"
          | _ -> begin
              let res = Domain.reachable ctx state.mem in
              match res with
              | Smtbackend.Smtlib_sig.Unsat -> "TRUE (unreachable)", "smt solving"
              | Smtbackend.Smtlib_sig.Unknown ->
                "UNKNOWN", "neither abstract interpretation nor smt solving"
              | Smtbackend.Smtlib_sig.Sat _ -> "FALSE (reachable)", "smt solving"
            end
        in
        assertion_table := LocationMap.update loc (function
            | None -> Some [(unreachable,how)]
            | Some l -> Some ((unreachable,how)::l)
          ) !assertion_table
      end
    ;;




  end

  let iter_on_alarms f =
    Assertion_Table.AlarmMap.iter (fun (a,l) v  -> f a l v) !Assertion_Table.alarm_table;;

  let iter_on_assertions f =
    Assertion_Table.LocationMap.iter (fun loc l ->
        List.iter (fun (bool,how) ->
            f loc bool how
          ) l
      ) !Assertion_Table.assertion_table;;

  let reset_alarms () =
    Assertion_Table.alarm_table := Assertion_Table.AlarmMap.empty

  let reset_assertions () =
    Assertion_Table.assertion_table:= Assertion_Table.LocationMap.empty


  module Register_Table = struct

    module CallingContextHash = Hashtbl.Make(CallingContext)

    module ExpInStmtHash = struct
      include Hashtbl.Make(ExpInStmt)

      let orig_find = find
      let find a b =
        try find a b
        with Not_found ->
          let res =  (CallingContextHash.create 17) in
          replace a b res;
          res
    end

    let exptrie = ExpInStmtHash.create 17;;

  end

  let exp_to_value (ki,exp) =
    let open Register_Table in
    ExpInStmtHash.find exptrie (ki,exp);;

  let exp_has_value (ki,exp) =
    let open Register_Table in
    try
      let res = ExpInStmtHash.orig_find exptrie (ki,exp) in
      assert((CallingContextHash.length res) != 0);
      true
    with _ -> false
  ;;



  (* Give a unique malloc_id per hash. In particular, this allows that
     when there is a jump into a scope, both "malloced" local
     variables will have the same id, and thus can be strongly
     updated. *)
  let varinfo_id =
    let varinfo_id_hash = Cil_datatype.Varinfo.Hashtbl.create 17 in
    fun vi ->
      try Cil_datatype.Varinfo.Hashtbl.find varinfo_id_hash vi
      with Not_found ->
        let id =
          Operator.Malloc_id.fresh (Format.asprintf "%a" (* Varinfo_Enclosing_Function *)Cil_datatype.Varinfo.pretty vi) in
        Cil_datatype.Varinfo.Hashtbl.replace varinfo_id_hash vi id;
        id
  ;;

  (* Allocate variable vi, initialized by calling initf. *)
  let allocate_var state vi initf =
    (* Kernel.feedback "allocating %a" Cil_datatype.Varinfo.pretty vi; *)
    let id = varinfo_id vi in
    assert(not @@ VarMap.mem vi state.var_addresses);
    let ctx = state.context.ctx in
    let exception Function_Type in
    let exception Global_Var of Types.TypedC.typ in
    let addr,mem = try
        (match Types.TypedC.global_of_name vi.vname with
        | Some typ -> raise (Global_Var typ)
        | None -> ());
        let malloc_size =
          if Ast_types.is_fun vi.Cil_types.vtype
          then raise Function_Type
          else
            try Cil.bytesSizeOf vi.Cil_types.vtype
            with Cil.SizeOfError _ ->
                Kernel.warning "Could not compute the size of type %a for %a, defaulting  to 1"
                  Cil_datatype.Typ.pretty vi.Cil_types.vtype
                  Cil_datatype.Varinfo.pretty vi;
              1
        in
        let malloc_size = malloc_size |> in_bytes in
        let size = In_bytes.in_bits malloc_size in
        let addr,mem = Domain.Memory_Forward.malloc ~id ~malloc_size ctx state.mem in
        match initf with
        | None -> addr,mem       (* No initial write. *)
        | Some initf ->
          let to_store = initf ~size in
          let mem = Domain.Memory_Forward.store ctx ~size mem addr to_store in
          addr,mem
      (* Functions are not deallocated, so we don't have to allocate anything.
         MAYBE: do the same for every global variables of unknown size? *)
      with
        | Function_Type ->
          let kf = Globals.Functions.get vi in
          let size = Codex_config.ptr_size() in
          let ptr =
            match Types.TypedC.function_definition_of_name @@ Kernel_function.get_name kf with
            | Some{funtyp;inline=false} ->
              let ptyp = Types.TypedC.(Build.ptr funtyp Pred.(Cmp(NotEqual,Self,Const Z.zero))) in
              Domain.binary_unknown_typed ~size ctx ptyp
            | None | Some{inline=true} -> Domain.binary_unknown ~size ctx
          in
            Domain.add_pointed_function ptr vi;
            ptr, state.mem

        | Global_Var typ ->
          let malloc_size = Types.TypedC.sizeof typ in
          let size = In_bytes.in_bits malloc_size in
          let addr,mem = Domain.Memory_Forward.malloc ~id ~malloc_size ctx state.mem in
          let to_store = Domain.binary_unknown_typed ~size ctx typ in
          Domain.add_global_symbol ~size ctx vi.vname to_store ;
          let mem = Domain.Memory_Forward.store ctx ~size mem addr to_store in
          addr,mem
    in
    let var_addresses = VarMap.add vi addr state.var_addresses in
    {state with mem;var_addresses}
  ;;


  (* Deallocate variable vi. *)
  let deallocate_var state vi =
    (* Kernel.feedback "deallocating %a %d" Cil_datatype.Varinfo.pretty vi vi.vid; *)
    let addr = VarMap.find vi state.var_addresses in
    let mem = Domain.Memory_Forward.free state.context.ctx state.mem addr in
    let var_addresses = VarMap.remove vi state.var_addresses in
    {state with mem; var_addresses}
  ;;


  (* Enter a block (and allocate the corresponding variables). *)
  let block_entry state block =
    let ctx = state.context.ctx in
    let locals = block.Cil_types.blocals in
    let to_store ~size =
      if Codex_options.UnknownUninitialized.get() then
        (* The SVComp competition has a wrong interpretation of the C
           standard, and assumes that uninitialized local variables can have
           any value (instead of no value). This block of code allows to
           have this interpretation. *)
        Domain.binary_unknown ~size ctx
      else Domain.Binary_Forward.buninit ~size ctx
    in
    List.fold_left (fun state vi -> allocate_var state vi @@ Some to_store) state locals
  ;;


  (* Leave a block (and deallocate the corresponding variables) *)
  let block_close state block =
    (* Kernel.feedback "compile block close %a" Cil_datatype.Block.pretty block; *)
    let locals = block.Cil_types.blocals in
    List.fold_left (fun state vi ->
        deallocate_var state vi) state locals
  ;;

  let init_function_args state kf args =
    let formals = Kernel_function.get_formals kf in
    let state = try List.fold_left2 (fun state vi ((s:In_bits.t),arg) ->
        let state = allocate_var state vi @@ Some (fun ~size ->
            assert (size == s);
            arg)
        in
        state) state formals args
      with Invalid_argument _ ->
        Log.fatal (fun p ->
          p "Argument number mismatch:@.%a the function '%s' expects %d arguments (%a),@.but the type file '%s' specifies %d arguments."
          Frama_c_kernel.Cil_printer.pp_location (Kernel_function.get_location kf)
          (Kernel_function.get_name kf)
          (List.length formals)
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Frama_c_kernel.Cil_printer.pp_varinfo) formals
          (Codex_options.TypeConfigurationFile.get ())
          (List.length args)
          ) in
    state

  let free_function_args state kf =
    let formals = Kernel_function.get_formals kf in
    List.fold_left deallocate_var state formals
  ;;

  (* I use this to avoid some calls to valid, which needlessly
     call a lot of transfer functions.  *)
  let trivially_valid lv = match lv with
    | Cil_types.(Var _,NoOffset) -> true
    | _ -> false



  type bitfield = {
    bit_offset: In_bits.t;
    bit_size: In_bits.t;
  }

  (* Sometimes we need the address of a bitfield (e.g. when assigning
     bitfields), hence this type. *)
  type compiled_lvalue = {
    address: Domain.binary;
    size: In_bits.t;
    bitfield: bitfield option;
  }


  module Expression = struct

    (* Is Cil_const.voidPtrType on in recent Frama-C. *)
    let ptr_bit_size = Cil.bitsSizeOf Cil_const.voidPtrType |> in_bits

      let bofbool_join ~size ctx cond =
        let one = Domain.Binary_Forward.biconst ~size Z.one ctx in
        let zero = Domain.Binary_Forward.biconst ~size Z.zero ctx in
        let notcond = Domain.Boolean_Forward.not ctx cond in
        let thenctx = Domain.assume ctx cond in
        let elsectx = Domain.assume ctx notcond in
        match thenctx, elsectx with
        | None, None -> None
        | None, Some ctx -> Some(zero, ctx)
        | Some ctx, None -> Some(one, ctx)
        | Some thenctx, Some elsectx ->
        let Domain.Context.Result(_,tup,deserialize) =
          Domain.serialize_binary ~widens:false ~size thenctx one elsectx zero (true, Domain.Context.empty_tuple ())
        in
        let newctx,res_tup = Domain.typed_nondet2 thenctx elsectx tup in
        let res,_ = deserialize newctx res_tup in
        Some(res,newctx)
      ;;

      let bofbool_join ~(size:In_bits.t) ctx cond =
        Log.trace (fun p -> p "bofbool_join size:%d" (size:>int)) (fun () ->
            bofbool_join ~size ctx cond)
      ;;

      (* bofbool_join is actually faster (often) and more precise *)
      let bofbool_constr ~size ctx cond =
        Domain.Binary_Forward.bofbool ~size ctx cond


    (* In expressions, we use the state monad to avoid passing the
       state everywhere. *)
    module State_Monad:sig
      type 'a m;;
      val return: 'a -> 'a m
      val (>>=): 'a m -> ('a -> 'b m) -> 'b m
      val (let*): 'a m -> ('a -> 'b m) -> 'b m
      val run: state -> 'a m -> ('a * state) option
      (* val tracem: 'a Tracelog.log -> (Format.formatter -> 'b -> unit) -> 'b m -> 'b m *)
      val tracem_bin: size:In_bits.t -> ?loc:Codex_options.Location.t -> 'a Tracelog.log -> Domain.binary m -> Domain.binary m
      val tracem_bool: 'a Tracelog.log -> ?loc:Codex_options.Location.t -> Domain.boolean m -> Domain.boolean m
      val get_state: unit -> state m
      (* val get_context: context m           *)
      (* val get_ctx: Domain.Context.t m *)
      val load: size:In_bits.t -> Domain.binary -> Domain.binary m
      val value_of_truth: size:In_bits.t -> Domain.boolean -> Domain.binary m

      (** [add_assumption] and [register_alarm] both assume the boolean is true,
          but [register_alarm] also raises an alarm if the assumption is unsatified. *)
      val add_assumption: Domain.boolean -> unit m
      val register_alarm: (Alarms.alarm * Cil_types.location) -> Domain.boolean -> unit m
      val register_lvalue: Cil_types.lval -> Domain.binary -> unit m;;
      val register_expression: Cil_types.exp -> Domain.binary -> unit m;;
      val register_boolean_expression: Cil_types.exp -> Domain.boolean -> unit m;;

      (* Transfer functions *)
      module State_Monad_Arity:sig
        type 'r ar0 = 'r m
        type ('a,'r) ar1 = 'a -> 'r m
        type ('a,'b,'r) ar2 = 'a -> 'b -> 'r m
        type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r m
      end
      module Boolean_Forward:Operator.BOOLEAN_FORWARD
        with module Arity := State_Monad_Arity and type boolean := Domain.boolean
      module Binary_Forward:Operator.BINARY_FORWARD
        with module Arity := State_Monad_Arity
         and type boolean := Domain.boolean and type binary := Domain.binary
      val binary_unknown: size:In_bits.t -> Domain.binary m
    end = struct
      type 'a m = state -> ('a * state) option;;
      let return x state = Some (x,state)
      let (>>=) m f = fun state ->
        match m state with
        | Some (v,state) -> f v state
        | None -> None
      let ( let* ) = (>>=);;
      let run state x = x state
      let tracem log ?loc ~pp_ret (f:'a m) =
        (fun state ->
           let res = Log.trace log ?loc ~pp_ret (fun () ->
               let x = run state f in
               x)
           in res
        )
      ;;
      let tracem_bin ~size ?loc log (f:Domain.binary m) =
        let pp_ret_option fmt = function
          | None -> Format.fprintf fmt "None"
          | Some(ret,state) ->
            let ctx = state.context.ctx in
            Domain.binary_pretty ~size ctx fmt ret
        in
        tracem log ?loc ~pp_ret:pp_ret_option f;;

      let tracem_bool log ?loc (f:Domain.boolean m) =
        let pp_ret_option fmt = function
          | None -> Format.fprintf fmt "None"
          | Some(ret,state) ->
            let ctx = state.context.ctx in
            Domain.boolean_pretty ctx fmt ret
        in
        tracem log ?loc ~pp_ret:pp_ret_option f;;
      ;;



      let get_state () = fun (state) -> Some (state,state);;
      let get_context = fun (state) -> Some(state.context,state);;
      let get_ctx = fun (state) -> Some(state.context.ctx,state);;
      let load ~size address state =
        match Domain.Memory_Forward.load ~size state.context.ctx state.mem address with
        | exception Domains.Sig.Memory_Empty -> None
        | (bin, mem) -> Some(bin,{state with mem})
      ;;
      let value_of_truth ~size cond state =
        let ctx = state.context.ctx in
        match bofbool_join ~size ctx cond with
        | None -> None
        | Some(res,ctx) -> Some (res, {state with context = {state.context with ctx}})
      ;;

      (** [add_assumption bool qbool state] assumes that the boolean term [bool],
          whose queried value is [qbool], is [True] in [state]. *)
      let add_assumption bool qbool state =
        match qbool with
        | Lattices.Quadrivalent.False | Lattices.Quadrivalent.Bottom -> None
        | Lattices.Quadrivalent.True -> Some((), state)
        | Lattices.Quadrivalent.Top -> begin
            let ctx = Domain.assume state.context.ctx bool in
            match ctx with
            | None -> None
            | Some ctx ->
              let context = {state.context with ctx} in
              Some((),{state with context})
          end

      (** Adds the assumption that [bool] is true, also raises an alarm if that
          boolean can't be proved to be true. *)
      let register_alarm (alarm,loc) bool state =
        let ctx = state.context.ctx in
        (* Codex_log.feedback "register alarm boolean = %a" (Domain.boolean_pretty ctx) bool; *)
        let qbool = Domain.query_boolean ctx bool in
        Assertion_Table.register_alarm qbool (alarm,loc,state.context.kinstr) state;
        add_assumption bool qbool state

      (** Adds the assumption that [bool] is true, like {!register_alarm}, but
          doesn't add any alarm. *)
      let add_assumption bool state =
        let ctx = state.context.ctx in
        (* Codex_log.feedback "register alarm boolean = %a" (Domain.boolean_pretty ctx) bool; *)
        let qbool = Domain.query_boolean ctx bool in
        add_assumption bool qbool state

      let register_lvalue lval bin = return ();;    (* TODO *)
      let register_expression exp bin = fun state ->
        let level = state.context.loop_nesting_level in
        if level > 0 then ()
        else begin
          let open Register_Table in
          let h = ExpInStmtHash.find exptrie (state.context.kinstr,exp) in
          let size = Cil.(bitsSizeOf (typeOf exp)) |> in_bits in
          (* Codex_log.feedback "Registering %a (%a) with %s" Cil_datatype.Exp.pretty exp ExpInStmt.pretty (state.context.kinstr,exp)  (Format.asprintf "%a" (Domain.binary_pretty ~size state.context.ctx) bin); *)
          (* The way Interpreted_automata work, mean that conditions
             are evaluated several time. In particular for switch, we
             have several assume(e != case). Registering e after these
             statements yield the wrong value. Thus, we want to
             register the first values. *)
          let calling_context = CallingContext.get() in
          if not @@ (CallingContextHash.mem h calling_context)
          then CallingContextHash.replace h calling_context
              (Format.asprintf "%a" (Domain.binary_pretty ~size state.context.ctx) bin);
        end;
        Some((),state)
      ;;

      let register_boolean_expression exp bool = fun state ->
        let open Register_Table in
        let h = ExpInStmtHash.find exptrie (state.context.kinstr,exp) in
        let x = match (Format.asprintf "%a" (Domain.boolean_pretty state.context.ctx) bool) with
          | "{true}" -> "{1}"
          | "{false}" -> "{0}"
          | "{true;false}" -> "{0; 1}"
          | "{}" -> "{}"
          | x -> x
        in
        CallingContextHash.replace h (CallingContext.get()) x;
          (* (Format.asprintf "%a" (Domain.boolean_pretty state.context.ctx) bool); *)
        Some((),state)
      ;;


      (* This is more similar to what we had before. The advantage is that it forces
         propagation of the boolean expression, which is otherwise not done.
         The performance cost is not important (the precision increase exists, but is also rare). *)
      let register_boolean_expression exp bool = fun state ->
        let size = 32 |> in_bits in
        (* assert false *)
        match value_of_truth ~size bool state with
        | None -> None
        | Some(bin,state) ->  register_expression exp bin state
        (* let bin,state = bofbool_join ~size state.context.ctx bool in *)
        (* register_expression exp bin state *)
      ;;


      module State_Monad_Arity = struct
        type 'r ar0 = state -> ('r * state) option
        type ('a,'r) ar1 = 'a -> state -> ('r * state) option
        type ('a,'b,'r) ar2 = 'a -> 'b -> state -> ('r * state) option
        type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> state -> ('r * state) option
      end

      module Operator = struct

        module Conversion(* :Operator.Conversions.Conversion *) = struct
          module From_Arity = Domains.Sig.Context_Arity_Forward(Domain.Context);;
          module To_Arity = State_Monad_Arity
          let ar0 f = (fun state -> Some(f state.context.ctx, state))
          let ar1 f = (fun a state -> Some(f state.context.ctx a, state))
          let ar2 f = (fun a b state -> Some(f state.context.ctx a b, state))
          let ar3 f = (fun a b c state -> Some(f state.context.ctx a b c, state))
        end

        module Types = struct
          type boolean = Domain.boolean
          type binary = Domain.binary
          type memory = Domain.memory
        end

        module Boolean_Forward = Operator.Conversions.Convert_Boolean_Forward(Conversion)(struct include Types include Domain.Boolean_Forward end)
        module Binary_Forward = Operator.Conversions.Convert_Binary_Forward(Conversion)(struct include Types include Domain.Binary_Forward end)



        let binary_unknown ~size =
          let* state = get_state() in
          let level = state.context.loop_nesting_level in
          let ctx = state.context.ctx in
          return (Domain.binary_unknown ~size ctx)
      end

      include Operator


    end


    open State_Monad;;

    exception Flexible_array_members

    (* Change the size of a value. *)
    let cast_size ~sext ~(from_size:In_bits.t) ~(to_size:In_bits.t) v =
      if from_size > to_size
      then Binary_Forward.bextract ~index:(0 |> in_bits) ~size:to_size ~oldsize:from_size v
      else if from_size < to_size
      then (if sext then Binary_Forward.bsext
            else Binary_Forward.buext) ~size:to_size ~oldsize:from_size v
      else return v
    ;;

    let z_of_integer c = Z.of_string @@ Integer.to_string c;;

    let constant ~size cst =
      let open Cil_types in
        match cst with
        | CInt64(c,_,_) -> Binary_Forward.biconst ~size (z_of_integer c)
        | CChr c -> Binary_Forward.biconst ~size (z_of_integer (Cil.charConstToInt c))
        | CEnum ({eival;_} as enum) ->
          (match Cil.isInteger eival with
           | None ->
              begin match eival with
              | {enode=UnOp(Neg,eival,_)} -> (
                match Cil.isInteger eival with
                  | None -> assert false
                  | Some v -> Binary_Forward.biconst ~size (z_of_integer v)
                )
              | _ -> assert false
              end
           | Some v -> Binary_Forward.biconst ~size (z_of_integer v))
        | CStr str -> let* state = get_state () in return @@ StringMap.find str state.string_addresses
        | CWStr _ws -> (* return (Conv.loc_of_wstring env ws) *) assert false
        | CReal _ -> binary_unknown ~size
    ;;

    (** [assert_no_signed_overflow exp ~small_size ~wide_size value]
        Adds assertion ensuring that [value], a signed binary value of size
        [~wide_size] bits, can correctly fit on [~small_size] bits without
        over/underflow. *)
    let assert_no_signed_overflow exp ~(small_size:In_bits.t) ~(wide_size:In_bits.t) value =
      let two_pow_size = Z.shift_left Z.one ((small_size:>int)-1) in
      (* Upper bound *)
      let upper_bound = Z.sub two_pow_size Z.one in
      let* upper_bound_val = Binary_Forward.biconst ~size:wide_size upper_bound in
      let* upper_overflow = Binary_Forward.bisle ~size:wide_size value upper_bound_val in
      let* () =
        if Codex_options.OverflowAlarms.get ()
        then register_alarm (Alarms.Overflow(Alarms.Signed, exp, upper_bound, Alarms.Upper_bound), exp.eloc) upper_overflow
        else add_assumption upper_overflow in
      (* lower bound *)
      let lower_bound = Z.neg two_pow_size in
      let* lower_bound_val = Binary_Forward.biconst ~size:wide_size lower_bound in
      let* lower_underflow = Binary_Forward.bisle ~size:wide_size lower_bound_val value in
      if Codex_options.OverflowAlarms.get ()
      then register_alarm (Alarms.Overflow(Alarms.Signed, exp, lower_bound, Alarms.Lower_bound), exp.eloc) lower_underflow
      else add_assumption lower_underflow

    (** [binop_with_overflow_guard exp ~small_size ~wide_size binop v1 v2]
        returns [binop ~size:small_size v1 v2], but first it ensures that no
        overflow occurs by computing [binop ~size:wide_size v1 v2] and checking
        that it fits on [~small_size] bits.

        @param ~small_size should be the size of terms [v1] and [v2]
        @param ~wide_size should be large enough to ensure that
               [binop ~size:wide_size v1 v2] does not overflow
        @param exp is the full Cil expression (i.e. [exp.enode = BinOp(binop, v1, v2)])
               It is used for error reporting when creating overflow alarms. *)
    let binop_with_overflow_guard exp ~small_size ~wide_size binop v1 v2 =
      let* v1_wide = Binary_Forward.bsext ~size:wide_size ~oldsize:small_size v1 in
      let* v2_wide = Binary_Forward.bsext ~size:wide_size ~oldsize:small_size v2 in
      let* full_op = binop ~size:wide_size v1_wide v2_wide in
      let* () = assert_no_signed_overflow exp ~small_size ~wide_size full_op in
      binop ~size:small_size v1 v2

    (** Same as {!binop_with_overflow_guard}, but for a unary operator *)
    let unop_with_overflow_guard exp ~small_size ~wide_size unop v =
      let* v_wide = Binary_Forward.bsext ~size:wide_size ~oldsize:small_size v in
      let* full_op = unop ~size:wide_size v_wide in
      let* () = assert_no_signed_overflow exp ~small_size ~wide_size full_op in
      unop ~size:small_size v

    let rec expression' exp =
      let exp_size = Cil.(bitsSizeOf (typeOf exp)) |> in_bits in
      match exp.enode with
      | Const c -> constant ~size:exp_size c
      | Lval(lv) ->
        let* loc = lvalue lv in
        let size = loc.size in
        let address = loc.address in
        let* () =
          if trivially_valid lv
          then return ()
          else let* valid = Binary_Forward.valid ~size Operator.Read address in
          register_alarm (Alarms.Memory_access(lv,Alarms.For_reading),exp.eloc) valid
        in
        let* v =
          (* Translate volatile using bunknown. *)
          (if Ast_types.has_qualifier "volatile" @@ Cil.typeOfLval lv
           then binary_unknown ~size
           else State_Monad.load ~size address)
        in
        begin match loc.bitfield with
          | None -> return v
          | Some {bit_offset;bit_size} ->
            (* MAYBE: optimize this case. if bit_offset mod 8 == 0 && bit_size mod 8 == 0 -> *)
            (* Note: shouldn't this depend on some machdep?  *)
            let* res = Binary_Forward.bextract
                ~size:bit_size ~oldsize:loc.size ~index:bit_offset v
            in
            if bit_size == exp_size then return res
            else let op = match (Ast_types.unroll @@ Cil.typeOf exp).tnode with
                | TInt ikind -> if Cil.isSigned ikind then Binary_Forward.bsext ~oldsize:bit_size else Binary_Forward.buext ~oldsize:bit_size
                | _ -> assert false in
              op ~size:exp_size res
        end
      | AddrOf lv (* | StartOf lv *) ->
        let* loc = lvalue lv in return loc.address

      (* Most of the time this allows to merge indice 0 with the others
         in our abstract domain for offsets. *)
      | StartOf lv ->
        let* loc = lvalue lv in
        let* zero = Binary_Forward.biconst Z.zero ~size:ptr_bit_size in
        begin
          match Ast_types.unroll @@ Cil.typeOfLval lv with
          | {tnode = TArray(t,_);_} -> Binary_Forward.bindex ~size:ptr_bit_size (Cil.bytesSizeOf t) loc.address zero
          | _ -> assert false
        end
      | BinOp(bop, e1, e2, _) -> binop exp exp_size bop e1 e2
      | UnOp(uop, e1, _) -> unop exp exp_size uop e1
      | SizeOf(typ) -> Binary_Forward.biconst ~size:exp_size (Z.of_int (Cil.bytesSizeOf typ))
      | SizeOfE(exp) ->
        Binary_Forward.biconst ~size:exp_size (Z.of_int (Cil.bytesSizeOf (Cil.typeOf exp)))
      | CastE(to_typ,subexp) ->
        let from_typ = Cil.typeOf subexp in
        let* subexp = expression subexp in
        (match (Ast_types.unroll from_typ).tnode, (Ast_types.unroll to_typ).tnode with
         | (TInt from_kind | TEnum {ekind = from_kind;_}),
           (TEnum {ekind = to_kind;_} | TInt to_kind) ->
           (match Cil.bitsSizeOfInt from_kind, Cil.bitsSizeOfInt to_kind with
            | from,to_ when from == to_ -> return subexp
            | from,to_ when from < to_ ->
              let from_size = Cil.bitsSizeOf from_typ in
              assert(from == from_size);
              if Cil.isSigned from_kind
              then Binary_Forward.bsext ~size:(to_ |> in_bits) ~oldsize:(from_size |> in_bits) subexp
              else Binary_Forward.buext ~size:(to_ |> in_bits)  ~oldsize:(from_size |> in_bits) subexp
            | from,to_ -> Binary_Forward.bextract ~index:In_bits.zero ~size:(to_ |> in_bits) ~oldsize:(from |> in_bits) subexp)
         | TFloat _, (TInt _ | TEnum _)
         | (TInt _ | TEnum _), TFloat _
         | TFloat _, TFloat _ -> binary_unknown ~size:(Cil.bitsSizeOf to_typ |> in_bits)

         (* Conversion between pointers, and putting pointers in arrays *)
         | (TPtr _ | TArray _) , (TPtr _ | TArray _) -> return subexp
         | (TInt _ | TEnum _ | TPtr _), (TInt _ | TEnum _ | TPtr _)
           when Cil.bitsSizeOf from_typ == Cil.bitsSizeOf to_typ -> return subexp
         | _, _ -> Kernel.fatal "cast not handled: from %a to %a (sizes: %d and %d)"
                     Cil_datatype.Typ.pretty from_typ Cil_datatype.Typ.pretty to_typ
                     (Cil.bitsSizeOf from_typ) (Cil.bitsSizeOf to_typ))
      | _ -> Kernel.fatal "Expression not implemented: %a" Cil_datatype.Exp.pretty exp

    and expression: Cil_types.exp -> Domain.binary m = fun exp ->
      State_Monad.tracem_bin ~size:Cil.(bitsSizeOf (typeOf exp) |> in_bits) ~loc:(Codex_options.Location.Expression exp)
        (fun p -> p "Expression %a" Cil_datatype.Exp.pretty exp)  @@
      let* result = expression' exp in
      let* () = register_expression exp result in
      return result

    and apply_binpred bop typ v1 v2 =
      let exception Float in
      try
        let signed = match (Ast_types.unroll typ).tnode with
          | Cil_types.TInt ikind | Cil_types.TEnum {Cil_types.ekind = ikind;_} -> Cil.isSigned ikind
          | Cil_types.TPtr _ | Cil_types.TArray _ -> false (* Pointer is seen as unsigned integers. *)
          | Cil_types.TFloat _ -> raise Float
          | TFun _ -> assert false
          | TNamed _ -> assert false
          | TVoid -> assert false
          | TComp _ -> assert false
          | TBuiltin_va_list -> assert false
        in
        let size = Cil.bitsSizeOf typ |> in_bits in
        let open Cil_types in
        match bop with
        | Lt ->
          let* res = (if signed then Binary_Forward.bisle else Binary_Forward.biule) ~size v2 v1 in
          Boolean_Forward.not res
        | Le -> (if signed then Binary_Forward.bisle else Binary_Forward.biule) ~size v1 v2
        | Gt ->
          let* res = (if signed then Binary_Forward.bisle else Binary_Forward.biule) ~size v1 v2 in
          Boolean_Forward.not res
        | Ge -> (if signed then Binary_Forward.bisle else Binary_Forward.biule) ~size v2 v1
        | Ne -> let* res = Binary_Forward.beq ~size v1 v2 in Boolean_Forward.not res
        | Eq -> Binary_Forward.beq ~size v1 v2
        | _ -> assert false
      with Float ->
        let* state = get_state() in
        let ctx = state.context.ctx in
        return (Domain.boolean_unknown ctx)



    and binop exp exp_size bop e1 e2 =
      let open Cil_types in
      let* v1 = expression e1 in
      let* v2 = expression e2 in
      (* TODO:
         - Generate alarms (actually, do not do it here; use alarms
           generated by rte, so as to create a link to alarms on terms).
      *)
      let typ_e1 = Cil.typeOf e1 in
      let typ_e2 = Cil.typeOf e2 in

      (* This is the C standard default: unsigned overflow may occur on
         all integer operations, and operations on signed integers
         cannot perform a signed overflow. Pointer operations are
         different.*)
      let nuw = false in
      let nsw = Cil.isSignedInteger typ_e1 in
      let nusw = false in

      match bop with
        (* Filter arithmetical floating-point operations first. *)
        | PlusA | MinusA | Mult | Div | Mod when Ast_types.is_float typ_e1 ->
          assert(Ast_types.is_float typ_e2);
          binary_unknown ~size:exp_size

        (* Arithmetical operations. *)
        | PlusA ->
          let flags = Operator.Flags.Biadd.pack ~nsw ~nuw ~nusw in
          if nsw then
            (* if a and b fit on n bits, then a+b will always fit on n+1 bit *)
            binop_with_overflow_guard
              exp ~small_size:exp_size ~wide_size:(increase_size exp_size)
              (Binary_Forward.biadd ~flags) v1 v2
          else
            Binary_Forward.biadd ~size:exp_size ~flags v1 v2
        | MinusA ->
          if nsw then
            (* if a and b fit on n bits, then a+b will always fit on n+1 bit *)
            binop_with_overflow_guard
              exp ~small_size:exp_size ~wide_size:(increase_size exp_size)
              (Binary_Forward.bisub ~flags:(Operator.Flags.Bisub.pack ~nsw ~nuw ~nusw)) v1 v2
          else
            Binary_Forward.bisub ~size:exp_size ~flags:(Operator.Flags.Bisub.pack ~nsw ~nuw ~nusw) v1 v2
        | Mult ->
          let flags = Operator.Flags.Bimul.pack ~nsw ~nuw in
          if nsw && false then (* TODO: activate this overflow assertion and migrate tests *)
            (* if a and b fit on n bits, then a*b will always fit on 2*n bit *)
            binop_with_overflow_guard
              exp ~small_size:exp_size ~wide_size:(In_bits.double exp_size)
              (Binary_Forward.bimul ~flags) v1 v2
          else
            Binary_Forward.bimul ~size:exp_size ~flags v1 v2
        | Div | Mod ->
          let* zero = Binary_Forward.biconst ~size:exp_size Z.zero in
          let* bool = Binary_Forward.beq ~size:(exp_size) zero v2  in
          let* bool = Boolean_Forward.not bool in
          let* () = register_alarm (Alarms.Division_by_zero e2,e2.eloc) bool in
          let typ = Cil.typeOf e1 in
          (* Operations that depends on the sign. *)
          assert (Ast_types.is_integral typ); (* For now *)
          let size = Cil.bitsSizeOf typ |> in_bits in
          begin match (Cil.isSignedInteger typ, bop) with
            | (true,Div) -> Binary_Forward.bisdiv ~size v1 v2
            | (true,Mod) -> Binary_Forward.bismod ~size v1 v2
            | (false,Div) -> Binary_Forward.biudiv ~size v1 v2
            | (false,Mod) -> Binary_Forward.biumod ~size v1 v2
            | _ -> assert false
          end

        (* Bitwise operations. *)
        | BAnd -> Binary_Forward.band ~size:exp_size v1 v2
        | BXor -> Binary_Forward.bxor ~size:exp_size v1 v2
        | BOr -> Binary_Forward.bor ~size:exp_size v1 v2
        | Shiftlt | Shiftrt  as op ->
          let typ = Cil.typeOf e1 in
          assert (Ast_types.is_integral typ);
          assert (Cil.bitsSizeOf typ |> in_bits == exp_size);
          let flags = Operator.Flags.Bshl.pack ~nsw ~nuw in
          (* C operators do not mandate that operands have the same
             size, but Codex operand does. Cast the right operand to
             the correct size. *)
          let from_size = (Cil.bitsSizeOf @@ Cil.typeOf e2) |> in_bits in
          let* v2 = cast_size ~sext:false ~from_size ~to_size:exp_size v2 in
          (match (Cil.isSignedInteger typ,op) with
           (* Note: the behaviour of shifting negative values is actually unspecified. *)
           | (true,Shiftlt) ->
                if false (* TODO: activate this overflow assertion and migrate tests *)
                then binop_with_overflow_guard
                       exp ~small_size:exp_size ~wide_size:(In_bits.double exp_size)
                       (Binary_Forward.bshl ~flags) v1 v2
                else Binary_Forward.bshl ~size:exp_size ~flags v1 v2
           | (true,Shiftrt) ->
                if false (* TODO: activate this overflow assertion and migrate tests *)
                then binop_with_overflow_guard
                       exp ~small_size:exp_size ~wide_size:(In_bits.double exp_size)
                       Binary_Forward.bashr v1 v2
                else Binary_Forward.bashr ~size:exp_size v1 v2
           | (false,Shiftlt) -> Binary_Forward.bshl ~size:exp_size ~flags v1 v2
           | (false,Shiftrt) -> Binary_Forward.blshr ~size:exp_size v1 v2
           | _ -> assert false
          )

        (* Predicates on integers and pointers. *)
        | Lt | Gt | Le | Ge | Ne | Eq ->
          let* b = apply_binpred bop Ast_types.(unroll (Cil.typeOf e1)) v1 v2 in
          value_of_truth ~size:exp_size  b

        (* Pointer operations. *)
        | PlusPI ->
          let k = Cil.bytesSizeOf (Ast_types.direct_pointed_type (Cil.typeOf e1)) in
          let index_size = Cil.bitsSizeOf typ_e2 |> in_bits in
          let sext = (Cil.isSignedInteger typ_e2) in
          let* off = cast_size ~sext ~from_size:index_size ~to_size:ptr_bit_size v2 in
          Binary_Forward.bindex ~size:ptr_bit_size k v1 off
        | MinusPI ->
          let k = Cil.(bytesSizeOf (Ast_types.direct_pointed_type (Cil.typeOf e1))) in
          let index_size = Cil.bitsSizeOf typ_e2 |> in_bits in
          let* off = cast_size ~sext:(Cil.isSignedInteger typ_e2) ~from_size:index_size ~to_size:ptr_bit_size v2 in
          let* zero = Binary_Forward.biconst ~size:ptr_bit_size Z.zero in
          let* moff = Binary_Forward.bisub ~size:ptr_bit_size
              ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) zero off in
          Binary_Forward.bindex ~size:ptr_bit_size k v1 moff
        | MinusPP ->
          let k = Cil.(bytesSizeOf (Ast_types.direct_pointed_type (Cil.typeOf e1))) in
          let k = Z.of_int k in
          let* diff = Binary_Forward.bisub ~size:exp_size
              ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) v1 v2 in
          let* k = Binary_Forward.biconst ~size:exp_size k in
          Binary_Forward.biudiv ~size:exp_size diff k

        (* Boolean specials. *)
        | LAnd | LOr -> assert false

    and unop exp exp_size uop e1 =
      match uop with
      | Neg ->
        let* v1 = expression e1 in
        let unop ~size v =
          let* zero = Binary_Forward.biconst ~size Z.zero in
          Binary_Forward.bisub ~size ~flags:(Operator.Flags.Bisub.pack ~nsw:true ~nuw:false ~nusw:false) zero v in
        unop_with_overflow_guard exp ~small_size:exp_size ~wide_size:(increase_size exp_size) unop v1
      | LNot ->
        let* (v1:Domain.boolean) = cond_node e1 in
        let* notv1 = Boolean_Forward.not v1 in
        value_of_truth ~size:exp_size notv1
      | BNot ->
        let* v1 = expression e1 in
        let* ffff = Binary_Forward.biconst ~size:exp_size Z.minus_one in
        Binary_Forward.bxor ~size:exp_size ffff v1

    and lhost = let open Cil_types in function
        | Var(var) ->
          let* state = get_state () in
          return @@
          (try VarMap.find var state.var_addresses
           with Not_found -> Codex_log.fatal "Could not find %a" Cil_datatype.Varinfo.pretty var)
        | Mem exp -> expression exp

    (* loffset specifically for (unsized) flexible array members *)
    and loffset_array typ (v:Domain.binary) offs =
      Log.debug (fun p -> p "Evaluation loffset_array with typ : %a and v : %a and off : %a"
        Cil_types_debug.pp_typ typ
        Domain.Binary.pretty v
        Cil_types_debug.pp_offset offs);
      let open Cil_types in
      (* Note: max should be None for C99 flexible array members. But even then, pointer arithmetics
         cannot go before the array. *)
      let do_field fi ~byte_offset ~byte_size remaining_offset =
        let* v = Binary_Forward.bshift
            ~size:ptr_bit_size ~offset:byte_offset ~max:None v in
        loffset fi.ftype v remaining_offset
      in

      match offs with
      | NoOffset -> assert false
      | Field(fi,NoOffset) when fi.fbitfield <> None -> assert false

      (* Cannot be a bitfield. *)
      | Field(fi,offs) -> begin
          let (bit_offset,bit_size) = Cil.bitsOffset typ (Field(fi,NoOffset)) in
          assert(bit_offset mod 8 == 0);
          assert(bit_size mod 8 == 0);
          do_field fi ~byte_offset:(bit_offset / 8) ~byte_size:(bit_size / 8) offs
        end

      | Index(exp,offs) ->
        let pointed_typ = Ast_types.direct_element_type typ in
        let* off = expression exp in
        let typ_exp = Cil.typeOf exp in
        let size = Cil.(bitsSizeOf typ_exp) |> in_bits in
        let k = Cil.bytesSizeOf pointed_typ in
        let* off = cast_size ~sext:(Cil.isSignedInteger typ_exp)
          ~from_size:size ~to_size:ptr_bit_size off in
        let next = fun () ->
          let* v = Binary_Forward.bindex ~size:ptr_bit_size k v off in
          loffset pointed_typ v offs
        in

        (* This is to help the analysis. We already have \valid, but
           these stricter conditions help reduce the abstract state. *)
        (match (Ast_types.unroll typ).tnode with
         | TArray (_elt_typ,length) -> begin
             match length, Kernel.SafeArrays.get() with
             | Some length, true ->
               let alarm1 = Alarms.Index_out_of_bound(exp,None) in
               let alarm2 = Alarms.Index_out_of_bound(exp,Some length) in
               let length = z_of_integer @@
                 match (Cil.constFoldToInt ~machdep:true length)
                 with None -> assert false | Some x -> x in
               let* zero = Binary_Forward.biconst ~size:ptr_bit_size Z.zero in
               let* boolean1 = Binary_Forward.bisle ~size:ptr_bit_size zero off in
               let* () = register_alarm (alarm1,exp.eloc) boolean1 in
               let* bound = Binary_Forward.biconst ~size:ptr_bit_size length in
               let* boolean2 = Binary_Forward.bisle ~size:ptr_bit_size bound off in
               let* boolean2 = Boolean_Forward.not boolean2 in
               let* () = register_alarm (alarm2,exp.eloc) boolean2 in
               next ()
             | _ -> next ()
           end
         | TPtr _ -> assert false
         | _ -> assert false
        );


    (* TODO: On renvoie (addresse,taille de la lvalue en bits,offset en bits,taille en bits) *)
    and loffset typ (v:Domain.binary) offs =
      let open Cil_types in
      try
      let size =
        try Cil.bitsSizeOf typ
        with Cil.SizeOfError _ ->
        match (Ast_types.unroll typ).tnode with
        | TFun _ ->
          Codex_config.function_size()
        | TArray(_,None) -> raise Flexible_array_members
        | _ -> assert false
      in
      let size = size |> in_bits in
      (* Note: max should be None for C99 flexible array members. But even then, pointer arithmetics
         cannot go before the array. *)
      let do_field fi ~byte_offset ~byte_size remaining_offset =
        let* v = Binary_Forward.bshift
            ~size:ptr_bit_size ~offset:byte_offset ~max:(Some (byte_size)) v in
        loffset fi.ftype v remaining_offset
      in

      match offs with
      | NoOffset -> return {address = v; size; bitfield = None}

      (* A bitfield. *)
      | Field(fi,NoOffset) when fi.fbitfield <> None ->
        let (bit_offset,bit_size) = Cil.bitsOffset typ offs in
        let bit_offset = bit_offset |> in_bits in
        let bit_size = bit_size |> in_bits in
        return {address = v; size; bitfield = Some({bit_offset;bit_size})}

      (* Cannot be a bitfield. *)
      | Field(fi,offs) -> begin
          let (bit_offset,bit_size) = Cil.bitsOffset typ (Field(fi,NoOffset)) in
          assert(bit_offset mod 8 == 0);
          assert(bit_size mod 8 == 0);
          do_field fi ~byte_offset:(bit_offset / 8) ~byte_size:(bit_size / 8) offs
        end

      | Index(exp,offs) ->
        let pointed_typ = Ast_types.direct_element_type typ in
        let* off = expression exp in
        let typ_exp = Cil.typeOf exp in
        let size = Cil.(bitsSizeOf typ_exp) |> in_bits in
        let k = Cil.bytesSizeOf pointed_typ in
        let* off = cast_size ~sext:(Cil.isSignedInteger typ_exp)
          ~from_size:size ~to_size:ptr_bit_size off in
        let next = fun () ->
          let* v = Binary_Forward.bindex ~size:ptr_bit_size k v off in
          loffset pointed_typ v offs
        in

        (* This is to help the analysis. We already have \valid, but
           these stricter conditions help reduce the abstract state. *)
        (match (Ast_types.unroll typ).tnode with
         | TArray(_elt_typ,length) -> begin
             match length, Kernel.SafeArrays.get() with
             | Some length, true ->
               let alarm1 = Alarms.Index_out_of_bound(exp,None) in
               let alarm2 = Alarms.Index_out_of_bound(exp,Some length) in
               let length = z_of_integer @@
                 match (Cil.constFoldToInt ~machdep:true length)
                 with None -> assert false | Some x -> x in
               let* zero = Binary_Forward.biconst ~size:ptr_bit_size Z.zero in
               let* boolean1 = Binary_Forward.bisle ~size:ptr_bit_size zero off in
               let* () = register_alarm (alarm1,exp.eloc) boolean1 in
               let* bound = Binary_Forward.biconst ~size:ptr_bit_size length in
               let* boolean2 = Binary_Forward.bisle ~size:ptr_bit_size bound off in
               let* boolean2 = Boolean_Forward.not boolean2 in
               let* () = register_alarm (alarm2,exp.eloc) boolean2 in
               next ()
             | _ -> next ()
           end
         | TPtr _ -> assert false
         | _ -> assert false
        )
      with Flexible_array_members -> loffset_array typ v offs

    and lvalue' (host,offs) =
      let* v = lhost host in
      loffset (Cil.typeOfLhost host) v offs

    and lvalue: Cil_types.lval -> compiled_lvalue State_Monad.m = fun lval ->
      let* result = lvalue' lval in
      let* () = register_lvalue lval result.address in
      return result

    (* Evaluate an expression to obtain its truth value. *)
    and cond_node' (e:Cil_types.exp) =
      match e.enode with
      (* If e.enode is already a boolean expression, we can return a
         truth value directly. This avoids a "double conversion" (from
         boolean to int and back), which results in translating the
         condition in "if(x <= 3)" as "value_of_truth(x <= 3) != 0". *)
      | BinOp((Eq|Ne|Gt|Lt|Ge|Le as op),e1,e2,_) ->
        let* v1 = expression e1 in
        let* v2 = expression e2 in
        let* res = apply_binpred op (Ast_types.unroll (Cil.typeOf e1)) v1 v2 in
        let* () = register_boolean_expression e res in
        return res

      | UnOp(LNot,e1,_) ->
        let* v = cond_node e1 in
        let* res = Boolean_Forward.not v in
        let* () = register_boolean_expression e res in
        return res

      | CastE(to_typ,e) when Ast_types.is_integral @@ Ast_types.unroll to_typ -> cond_node e

      | _ ->

        (* The conversion to boolean depends on the static type of e. In
           every case we compare against 0, but 0 means "null pointer" for
           pointer types, and "0.0" in float types. *)
        (* Note: we do not register the boolean expression here; we prefer to register
           the non-boolean one.*)
        match Ast_types.unroll (Cil.typeOf e) with
        {tnode = TInt _ | TEnum _ | TPtr _;_} as typ ->
          let* cond = expression e in
          let size = Cil.bitsSizeOf typ |> in_bits in
          let* izero = Binary_Forward.biconst ~size Z.zero in
          let* eqzero = Binary_Forward.beq ~size cond izero in
          Boolean_Forward.not eqzero
        | _ -> Log.fatal (fun p -> p "Not yet implemented cond_node %a" Cil_datatype.Exp.pretty e)

    and cond_node e =
      State_Monad.tracem_bool
        ~loc:(Codex_options.Location.Expression e)
        (fun p -> p "Condition node %a" Cil_datatype.Exp.pretty e)
        (cond_node' e)

  end

  open Expression.State_Monad


  (* Insert the contents of a bitfield in a previous value. *)
  let bitfield_replace ctx old oldsize {bit_offset;bit_size} newv newvsize =
    (* Newv may be too big to enter in the bitfield; take only the last bits. *)
    let newv =
      if bit_size == newvsize then newv
      else Domain.Binary_Forward.bextract ~oldsize:newvsize ~index:In_bits.zero ~size:bit_size ctx newv
    in
    let previous =
      (if bit_offset == In_bits.zero then newv
       else
         let prev = Domain.Binary_Forward.bextract ~oldsize ~index:In_bits.zero ~size:bit_offset ctx old in
         Domain.Binary_Forward.bconcat ~size1:bit_size ~size2:bit_offset ctx newv prev
      )
    in
    let end_index = In_bits.(bit_offset + bit_size) in
    if end_index == oldsize
    then previous
    else
      let next =
        Domain.Binary_Forward.bextract ~oldsize ~index:end_index ~size:In_bits.(oldsize - end_index) ctx old
      in
      (* Kernel.feedback "concat2 term %a size2 %d" Term.pretty (Obj.magic previous) end_index; *)
      Domain.Binary_Forward.bconcat ~size1:In_bits.(oldsize - end_index) ~size2:end_index ctx next previous
  ;;


  module AddrSet = Set.Make(Domain.Binary) ;;
  let local_addresses = ref AddrSet.empty ;;

  (* Note: could be in the monad, as a unit m. *)
  let store_lvalue ~instr_loc lv newv newvsize state =
    match run state @@
      (let* loc = Expression.lvalue lv in
       let* () =
         if trivially_valid lv
         then return ()
         else
           let* valid = Binary_Forward.valid ~size:loc.size Operator.Write loc.address in
           register_alarm (Alarms.Memory_access(lv,Alarms.For_writing),instr_loc) valid
  in
       return loc)
    with
    | None -> None
    | Some(loc,state) -> begin
        if !exploring_pure_function then begin
          Log.debug (fun p -> p "Checking if lval %a causes impure store" Cil_types_debug.pp_lval lv);
          match lv with
          | Var var, NoOffset when not var.vglob -> ()
          | Mem {enode=Lval (Var var, _)} as base, _ ->
            let addr = (try VarMap.find var state.var_addresses with Not_found -> Codex_log.fatal "Could not find %a" Cil_datatype.Varinfo.pretty var) in
            (* let addr = return @@ run state @@ Expression.lhost base in *)
            let ptrsize = Codex_config.ptr_size() in
            let ptr,_ = Domain.Memory_Forward.load ~size:ptrsize state.context.ctx state.mem addr in
            if AddrSet.mem ptr !local_addresses then ()
            else
              Emit_alarm.emit_alarm Operator.Alarm.Impure_store;
              Codex_log.error "Store on expression lhost %a is impure" Cil_types_debug.pp_lhost base ;
          | _ ->
              Emit_alarm.emit_alarm Operator.Alarm.Impure_store ;
              Codex_log.error "Store on expression %a is impure" Cil_types_debug.pp_lval lv ;
        end ;
        let ctx = state.context.ctx in
        match loc.bitfield with
        | None -> begin
            match Domain.Memory_Forward.store ~size:loc.size ctx (state.mem) loc.address newv with
            | exception Domains.Sig.Memory_Empty -> None
            | mem ->
              (* Kernel.feedback "store at location %a made on term %a" pp_location instr_loc L.Memory.pp (the_mem state); *)
              Some {state with mem}
          end
        | Some bitfield ->
          (* MAYBE: optimize the case bitfield.bit_offset mod 8 == 0 && bitfield.bit_size mod 8 == 0:  *)
          let mem = state.mem in
          let oldv,mem = Domain.Memory_Forward.load  ~size:loc.size ctx mem loc.address in
          let tostore = bitfield_replace ctx oldv loc.size bitfield newv newvsize in
          Some {state with mem = Domain.Memory_Forward.store ~size:loc.size ctx mem loc.address tostore }
      end
  ;;


  let initialize_variable var state init =
    Log.trace (fun p -> p "initialize_variable %a" Cil_datatype.Varinfo.pretty var) @@ fun () ->
    let open Cil_types in

    let rec doit lv init typ state =
      Log.trace (fun p -> p "initialize_variable at %a" Cil_datatype.Lval.pretty lv) @@ fun () ->
      let size = Cil.bitsSizeOf typ |> in_bits in
      match init with
      | SingleInit(e) ->
        let m =
          (Expression.expression e >>= fun value ->
           Expression.lvalue lv >>= fun address ->
           return (value,address))
        in
        begin match run state m with
          | None -> None
          | Some((value,address),state) ->
            assert(address.bitfield == None);
            let ctx = state.context.ctx in
            let mem = Domain.Memory_Forward.store ~size ctx state.mem address.address value in
            Some {state with mem }
        end
      | CompoundInit(ct,initl) ->
        let doinit off init typ state =
          match state with
          | None -> None
          | Some state -> doit (Cil.addOffsetLval off lv) init typ state
        in
        (* Note: [implicit:true] handles incomplete initializers for us. *)
        Cil.foldLeftCompound
          ~implicit:true
          ~doinit ~ct ~initl ~acc:(Some state)
    in
    (* let initialize_padding ~size = Binary.bunknown_explicit ~size ~level:0 in *)
    (* let mem = Lang.allocate_var mem var initialize_padding in *)
    doit (Var var, NoOffset) init var.vtype state
  ;;



  module Builtin = struct

    let show_each ret f args instr_loc state =
      if (ret <> None) then failwith "Frama_C_show_each does not return a value";
      (* let tuple = Immutable_dynamic_array.from_array (Array.of_list (mem.mem::args)) in *)
      let len = (String.length "Frama_C_show_each") in
      let suffix = String.sub f.Cil_types.vname len  ((String.length f.vname) - len) in


      let m = run state @@ List.fold_left (fun acc arg ->
          acc >>= fun acc ->
          let size = Cil.(bitsSizeOf (typeOf arg)) |> in_bits in
          Expression.expression arg >>= fun arg ->
          return ((size,arg)::acc)) (return []) args
      in
      match m with
      | None -> None
      | Some(args,state) ->
        let args = List.rev args in
        Log.notice (fun p ->
            let ctx = state.context.ctx in
            let pelt fmt (size,x) = (Domain.binary_pretty ~size ctx) fmt x in
            let plist = Fmt.(list ~sep:(const string ",") pelt) in
            p "show each%s:@[<hv>%a@]" suffix plist args);
        Some state
    ;;

    let bzero ret _f args instr_loc state =
      let arg1,arg2 = match ret,args with
        | None, [arg1;arg2] -> arg1,arg2
        | _ -> failwith "Wrong number of arguments/return value to bzero"
      in
      let size = match Cil.constFoldToInt ~machdep:true arg2 with
        | Some int -> int
        | None -> failwith "bzero called with non-constant size not handled" in
      match  run state @@ Expression.expression arg1 with
      | None -> None
      | Some(address,state) -> begin
          let size = Z.to_int size in
          let size = size * 8 |> in_bits in
          let zero = Domain.Binary_Forward.biconst ~size Z.zero state.context.ctx in
          (* Write a big chunk of zero. *)
          let mem = Domain.Memory_Forward.store ~size state.context.ctx state.mem address zero in
          Some {state with mem}
        end
    ;;

    let verifier_assume ret _f args instr_loc state =
      let arg = match args with | [arg] -> arg | _ -> assert false in
      match run state @@ Expression.cond_node arg with
      | None -> None
      | Some(arg,state) ->
        let ctx = Domain.assume state.context.ctx arg in
        match ctx with
        | None -> None
        | Some ctx ->
          (* Codex_log.feedback "In verifier assume: mem=%a" *)
          (*   (Domain.memory_pretty ctx) state.mem;                 *)
          let state = {state with context =  {state.context with ctx}} in
          (* Codex_log.feedback "After verifier assume: mem=%a" *)
          (*   (Domain.memory_pretty state.context.ctx) state.mem;         *)
          Some state

    (* Note: called assert, but it most like a check: the state is unmodified. *)
    let verifier_assert ret _f args instr_loc state =
      let arg = match args with | [arg] -> arg | _ -> assert false in
      match run state @@ Expression.cond_node arg with
      | None -> None
      | Some (arg,state) ->
        Assertion_Table.register_assertion instr_loc arg state ;
        let ctx = Domain.assume state.context.ctx arg in
        match ctx with
        | None -> None
        | Some ctx -> Some {state with context = { state.context with ctx }}

    let verifier_check ret _f args instr_loc state =
      let arg = match args with | [arg] -> arg | _ -> assert false in
      match run state @@ Expression.cond_node arg with
      | None -> None
      | Some (arg,state) ->
        Assertion_Table.register_assertion instr_loc arg state ;
        Some state


    let verifier_error ret _f args instr_loc state =
      assert(args = []);
      Assertion_Table.register_reachability_check instr_loc state;
      None


    let verifier_nondet_int ret _f args instr_loc state =
      assert(args = []);
      let ctx = state.context.ctx in
      let level = Domain.Context.level ctx in
      match ret with
      | None -> failwith "Nondet int called without a return value"
      | Some ret ->
        let size = Cil.bitsSizeOf @@ Cil.typeOfLval ret |> in_bits in
        let retval = Domain.binary_unknown ~size ctx in
        let state = store_lvalue ~instr_loc ret retval size state in
        state
    ;;


    let free ret f args instr_loc state =
      let arg = match args with
        | [x] -> x
        | _ -> failwith "Wrong number of arguments to free"
      in
      match run state @@ Expression.expression arg with
      | None -> None
      | Some (arg,state) ->
        let mem = Domain.Memory_Forward.free state.context.ctx state.mem arg in
        Some {state with mem}
    ;;


    let malloc ret _f args instr_loc state =
      let module TypedC = Codex.Types.TypedC in
      (* let open Lang.Memory in *)
      let sizeexp = match args with
        | [x] -> x
        | _ -> failwith "Wrong number arguments to malloc"
      in
      let malloc_size,state = match run state @@ Expression.expression sizeexp with
        | None -> failwith "Size of malloc gives <bottom>"
        | Some res -> res
      in
      (* let malloc_size = match Cil.constFoldToInt ~machdep:true sizeexp with
        | Some size -> Z.to_int size
        | None -> assert false
      in *)
      (* Does the lvalue has a type that we want to treat specially? (Here we
         only support variable lvalues.) *)
      (* Really do the malloc. *)
      let size = Codex_config.ptr_size () in
      let untyped () =
        Log.debug (fun p -> p "malloc: default case");
        let malloc_size =
          match Domain.Query.(Binary_Lattice.is_singleton ~size @@ binary ~size state.context.ctx malloc_size) with
          | None -> assert false
          | Some sz -> Z.to_int sz |> in_bytes
        in
        let sid = match state.context.kinstr with Kglobal -> assert false | Kstmt stmt -> stmt.sid in
        let id = Codex.Operator.Malloc_id.fresh ("malloc" ^ string_of_int sid) in
        let ptr,mem = Domain.Memory_Forward.malloc ~id ~malloc_size state.context.ctx state.mem in
        let state = match ret with
          | None -> None
          | Some lval ->
            let value = ptr in
            store_lvalue ~instr_loc lval value (Codex_config.ptr_size()) {state with mem}
        in
        state
      in
      let state = match ret with
        (* We replace malloc by a "pure malloc" + storing an uninitialized value. *)
        | Some lval when Codex_config.handle_malloc_as_unknown_typed_pointers() ->
          begin
            match Domain.Query.(Binary_Lattice.is_singleton ~size @@ binary ~size state.context.ctx malloc_size) with
            | Some sz ->
              let malloc_size = Z.to_int sz |> in_bytes in
              let malloc_size_in_bits = (In_bytes.in_bits malloc_size) in
              if not @@ Codex_options.UseWeakTypes.get() then
                let vtype = Cil.typeOfLval lval in
                let mytype = Compile_type.cil_type_to_ctype vtype in
                Log.debug (fun p -> p "malloc: typed case with type %a at loc %a" Cil_types_debug.pp_typ vtype Cil_types_debug.pp_location instr_loc);
                let mytype = match mytype.TypedC.descr, mytype.TypedC.pred with
                  | TypedC.(Ptr _, pred) -> {mytype with TypedC.pred = TypedC.Pred.conjunction pred @@ TypedC.Pred.(Cmp(NotEqual,Self,Const Z.zero))}
                  | _ -> assert false
                in
                let level = state.context.loop_nesting_level in
                let ptr_size = Codex_config.ptr_size () in
                let ptr = Domain.binary_unknown_typed ~size:ptr_size state.context.ctx mytype in
                let z = Domain.Binary_Forward.(beq ~size:ptr_size state.context.ctx ptr (biconst ~size:ptr_size Z.zero state.context.ctx)) in
                let nz = Domain.Boolean_Forward.not state.context.ctx z in
                local_addresses := AddrSet.add ptr !local_addresses ;
                match Domain.assume state.context.ctx nz with
                | None -> None
                | Some newctx ->
                  let state = {state with context = {state.context with ctx = newctx}} in
                  begin match store_lvalue ~instr_loc lval ptr (*XXX:change*) malloc_size_in_bits state with
                  | None -> None
                  | Some state ->
                    let init_val = Domain.Binary_Forward.buninit ~size:malloc_size_in_bits state.context.ctx in
                    begin
                      match Domain.Memory_Forward.store ~size:malloc_size_in_bits state.context.ctx state.mem ptr init_val with
                      | exception Domains.Sig.Memory_Empty -> None
                      | mem -> Some {state with mem}
                    end
                end
              else
                (* Codex_log.debug "size of malloc is %d" malloc_size ; *)
                let weak_type = TypedC.{descr = Weak (TypedC.word ~byte_size:malloc_size); pred = Pred.true_} in
                let mytype = TypedC.(Build.ptr weak_type Pred.(Cmp(NotEqual,Self,Const Z.zero))) in
                Log.debug (fun p -> p "Allocating type %a of size %d with malloc" TypedC.pp mytype (malloc_size:>int));
                let level = state.context.loop_nesting_level in
                let ptr_size = Codex_config.ptr_size () in
                let ptr = Domain.binary_unknown_typed ~size:ptr_size state.context.ctx mytype in
                let z = Domain.Binary_Forward.(beq ~size:ptr_size state.context.ctx ptr (biconst ~size:ptr_size Z.zero state.context.ctx)) in
                let nz = Domain.Boolean_Forward.not state.context.ctx z in
                local_addresses := AddrSet.add ptr !local_addresses ;
                begin
                  match Domain.assume state.context.ctx nz with
                  | None -> None
                  | Some newctx ->
                    let state = {state with context = {state.context with ctx = newctx}} in
                    begin match store_lvalue ~instr_loc lval ptr (* XXX: change.*) malloc_size_in_bits state with
                      | None -> None
                      | Some state ->
                        let init_val = Domain.Binary_Forward.buninit ~size:malloc_size_in_bits state.context.ctx in
                        begin
                          match Domain.Memory_Forward.store ~size:malloc_size_in_bits state.context.ctx state.mem ptr init_val with
                          | exception Domains.Sig.Memory_Empty -> None
                          | mem -> Some {state with mem}
                        end
                    end
                end

            | None ->
              let sz_symb = fresh_symbol () in
              Domain.add_global_symbol ~size state.context.ctx sz_symb malloc_size ;
              let array_type = TypedC.{descr = Array (word ~byte_size:In_bytes.one, Variable_length sz_symb) ; pred = Pred.true_} in
              let weak_type = TypedC.{descr = Weak array_type; pred = Pred.true_} in
              let mytype = TypedC.(Build.ptr weak_type Pred.(Cmp(NotEqual,Self,Const Z.zero))) in
              Log.debug (fun p -> p "Allocating array type %a of size %a with malloc" TypedC.pp mytype (Domain.binary_pretty ~size state.context.ctx) malloc_size);
              let level = state.context.loop_nesting_level in
              let ptr_size = Codex_config.ptr_size () in
              let ptr = Domain.binary_unknown_typed ~size:ptr_size state.context.ctx mytype in
              let z = Domain.Binary_Forward.(beq ~size:ptr_size state.context.ctx ptr (biconst ~size:ptr_size Z.zero state.context.ctx)) in
              let nz = Domain.Boolean_Forward.not state.context.ctx z in
              local_addresses := AddrSet.add ptr !local_addresses ;
              begin
                match Domain.assume state.context.ctx nz with
                | None -> None
                | Some newctx ->
                  let state = {state with context = {state.context with ctx = newctx}} in
                  store_lvalue ~instr_loc lval ptr size state
              end
          end
        | _ -> untyped ()
      in
      state
    ;;



  end



  (* TODO: Define an alist of builtins instead. *)
  let is_builtin name =
    List.mem name ["malloc";"free";"alloca";"__fc_vla_alloc";"__fc_vla_free";"exit";"abort";
                "Frama_C_bzero";"calloc";"realloc";"reach_error";
                "__VERIFIER_assert"; "__VERIFIER_check"; "__VERIFIER_error"; "__VERIFIER_assume";
                "__VERIFIER_nondet_int"]
  || String.starts_with ~prefix:"Frama_C_show_each" name
  ;;

  let call_builtin ret f args instr_loc state =
    if String.starts_with ~prefix:"Frama_C_show_each" f.Cil_types.vname
    then
      Builtin.show_each ret f args instr_loc state
    else match f.Cil_types.vname with
      | "exit" | "abort" -> None
      | "Frama_C_bzero" -> Builtin.bzero ret f args instr_loc state
      | "__VERIFIER_assume" -> Builtin.verifier_assume ret f args instr_loc state
      | "__VERIFIER_assert" -> Builtin.verifier_assert ret f args instr_loc state
      | "__VERIFIER_check" -> Builtin.verifier_check ret f args instr_loc state
      | "__VERIFIER_error" | "reach_error" -> Builtin.verifier_error ret f args instr_loc state
      | "__VERIFIER_nondet_int" -> Builtin.verifier_nondet_int ret f args instr_loc state
      | "malloc" -> Builtin.malloc ret f args instr_loc state
      | "free" -> Builtin.free ret f args instr_loc state
      | _ -> Codex_log.fatal "Need to handle builtin %s" f.Cil_types.vname
  ;;


  (* This warning has a tendency to spam. *)
  let no_definition_warning_hash = Cil_datatype.Varinfo.Hashtbl.create 17;;

  (* Call to unknown function *)
  let call_to_unknown state ret f args instr_loc =
    if not @@ Cil_datatype.Varinfo.Hashtbl.mem no_definition_warning_hash f
    then begin
      Log.warning (fun p -> p "No definition for function %a" Cil_datatype.Varinfo.pretty f);
      Cil_datatype.Varinfo.Hashtbl.add no_definition_warning_hash f ()
    end;

    (* Note: unsound: this considers that the function assigns
       nothing, and just returns a result. *)
    (match ret with
     | None -> Some state
     | Some lv ->
       let size = (Cil.bitsSizeOf (Cil.typeOfLval lv)) |> in_bits in
       let ctx = state.context.ctx in
       let value = Domain.binary_unknown ~size ctx in
       store_lvalue ~instr_loc lv value size state)
  ;;

  type funcall =
    Kernel_function.t -> (In_bits.t * Domain.binary) list -> state ->
    (In_bits.t * Domain.binary) option * state option

  (* Known definition. *)
  let call_known ~funcall state ret f args instr_loc =
    let kf = Globals.Functions.get f in
    let retval,state = funcall kf args state in
    match state,ret,retval with
    | None, _, _ -> None
    | Some state, None,_ -> Some state
    | Some _, Some _, None ->
      Codex_log.fatal "Expected a return value, but the function does not return one."
    | Some state, Some lvalue, Some (retsize,retval) ->
      let expected_size = (Cil.bitsSizeOf (Cil.getReturnType f.Cil_types.vtype)) |> in_bits in
      assert(retsize == expected_size);
      store_lvalue ~instr_loc lvalue retval retsize state
  ;;

  let return_called rtyp state ret retval instr_loc =
    match state, ret, retval with
    | None, _, _ -> None
    | Some state, None, _ -> Some state
    | Some _, Some _, None ->
      Codex_log.fatal "Expected a return value, but the function does not return one."
    | Some state, Some lvalue, Some (retsize,retval) ->
      let expected_size = Types.TypedC.sizeof rtyp |> In_bytes.in_bits in
      assert(retsize == expected_size);
      store_lvalue ~instr_loc lvalue retval retsize state

  (* Procedures to analyze functions interproceduraly *)

  let analyze_summary funtyp args state =
    let _, ret, mem = Domain.analyze_summary state.context.ctx funtyp args state.mem in
    ret, {state with mem}


  (* Handle the call instruction for non builtins. *)
  let call ~funcall ret lhost args instr_loc state =
    let m = run state @@
      (* Compile the arguments of the function (they may be subject to runtime errors) *)
      List.fold_left (fun acc arg ->
          acc >>= fun acc ->
          let size = Cil.(bitsSizeOf (typeOf arg)) |> in_bits in
          Expression.expression arg >>= fun arg ->
          return ((size,arg)::acc)) (return []) args
    in
    match m with
    | None -> None
    | Some (args,state) -> begin
        let args = List.rev args in
        match run state @@ Expression.lhost lhost with
        | None -> assert false
        | Some(loaded,state) ->
          let ptr_size = Codex_config.ptr_size() in
          let typ = Domain.type_of ~size:ptr_size state.context.ctx loaded in
          begin match typ with
            (* If there is a type: use it as a summary. *)
            | Some {descr = Ptr {pointed}} when Types.TypedC.(match (inlined pointed).descr with Function _ -> true | _ -> false) ->
              let funtyp = Types.TypedC.inlined pointed in
              let rtyp = match funtyp.descr with Function{ret} -> ret | _ -> assert false in
              (* = {descr = (Function {ret = rtyp})} as funtyp}} -> *)
              Log.debug (fun p -> p "Calling function summary from typed pointer %a" (Domain.binary_pretty ~size:ptr_size state.context.ctx) loaded);
              let retval, state = analyze_summary funtyp args state in
              let retval, state, rtyp =  retval, Some state, rtyp in
              return_called rtyp state ret retval instr_loc
            (* Otherwise: call the function (if there is no definition, make up something. *)
            | _ when Domain.is_function_pointer loaded ->
              let f = Domain.get_pointed_function loaded in
              let kf = Globals.Functions.get f in
              Log.debug (fun p -> p "Calling function %a from untyped pointer" Kernel_function.pretty kf);
              begin
                if not (Kernel_function.is_definition kf)
                then call_to_unknown state ret f args instr_loc
                else call_known ~funcall state ret f args instr_loc
              end
            (* We could not retrieve the function from the value (e.g. join between function pointers). *)
            | _ -> assert false
          end
      end
  ;;

  let instruction' ~funcall:funcall instr state =
    let open Cil_types in
    let funcall:funcall = funcall in
    match instr with
    | Skip _ -> Some state
    | Asm _ ->
      Log.warning (fun p -> p "Skipping assembly instruction");
      Some state
    (* Optional optimisation. *)
    | Set(lvdst,({enode=Lval(lvsrc);_} as exp),instr_loc) when false ->
      let f (state:state) : ((compiled_lvalue * compiled_lvalue) * state) option =
        let open Expression.State_Monad in
        run state @@
        (Expression.lvalue lvdst >>= fun ptrdst ->
        Expression.lvalue lvsrc >>= fun ptrsrc ->
        return (ptrdst,ptrsrc))
      in
      let _size = (Cil.bitsSizeOf @@ Cil.typeOf exp) in
      begin match f state with
        | None -> None
        | Some((ptrdst,ptrsrc),state) ->
          let _addrdst = ptrdst.address and _addrsrc = ptrsrc.address in
          (* Does not work yet for memcopies in bitfields. *)
          assert (ptrdst.bitfield == None);
          assert (ptrsrc.bitfield == None);
          assert false
      end

    (* Optimisation: when copying structures, copy each field.  Note
       that the analysis would be faster (and more precise, as it
       would delay the copies) if we would just call memcpy here, but
       I do not have it for now. *)
    (* Note: this is not an optimisation at all, and slows things down.
       I need to have memcpy instead. *)
    | Set(lvdst,exp,instr_loc)
      when false && Ast_types.is_struct_or_union @@ Ast_types.unroll @@ Cil.typeOf exp ->
      let open Cil_types in
      let lvsrc = match exp.enode with Lval lv -> lv | _ -> assert false in
      let rec f lvdst lvsrc (state : state) : state option =
        let typ = Ast_types.unroll @@ Cil.typeOfLval lvsrc in
        Codex_log.feedback "Type of %a is %a" Cil_datatype.Lval.pretty lvsrc Cil_datatype.Typ.pretty typ;
        if Ast_types.is_arithmetic typ || Ast_types.is_ptr typ then
          match run state @@ (Expression.expression (Cil.dummy_exp @@  Lval lvsrc)) with
          | None -> None
          | Some (value,state) ->
            store_lvalue ~instr_loc lvdst value (Cil.bitsSizeOf typ |> in_bits) state
        else if Ast_types.is_array typ then begin
          (* The type should be known statically. *)
          let len = match typ.tnode with TArray(_,i) -> i | _ -> assert false in
          let len = match len with None -> assert false | Some len -> len in
          let len = Cil.constFoldToInt ~machdep:true len in
          let len = match len with None -> assert false | Some len -> Z.to_int len in
          let rec loop state i =
            if i >= len then Some state
            else
              let offs = Index(Cil.integer ~loc:instr_loc i,NoOffset) in
              let lvsrc = Cil.addOffsetLval offs lvsrc in
              let lvdst = Cil.addOffsetLval offs lvdst in
              let state = f lvdst lvsrc state in
              match state with
              | None -> None
              | Some state -> loop state (i + 1)
          in loop state 0
        end
        else begin
          assert(Ast_types.is_struct_or_union typ);
          let compinfo = match typ.tnode with Cil_types.TComp ci -> ci | _ -> assert false in
          if compinfo.cstruct = false (* union *)
          then assert false           (* TODO. assign the largest field? *)
          else
            let open Cil_types in
            let state = List.fold_left (fun state fi ->
                match state with
                | None -> None
                | Some state ->
                  let lvsrc = Cil.addOffsetLval (Field(fi,NoOffset)) lvsrc in
                  let lvdst = Cil.addOffsetLval (Field(fi,NoOffset)) lvdst in
                  f lvdst lvsrc state
              ) (Some state) (match compinfo.cfields with None-> assert false | Some x -> x)  in
            state
        end
      in
      f lvdst lvsrc state
    | Set(lv,exp,instr_loc) ->
      if Ast_types.is_struct_or_union @@ Ast_types.unroll @@ Cil.typeOf exp
      then Log.warning (fun p -> p "Assignment of union or structure may\
                                  loose precision; we should use memcopy");
      begin match run state @@ Expression.expression exp with
        | None -> None
        | Some(value,state) ->
          store_lvalue ~instr_loc lv value (Cil.bitsSizeOf @@ Cil.typeOf exp |> in_bits) state
      end
    | Local_init(vi,AssignInit i,_) -> initialize_variable vi state i

    (* Note: we let the builtins compile the arguments as they wish
       (can be compiled as a boolean, to a constant...)  *)
    | Call(ret,{enode=Lval(Var f,NoOffset);_},args,instr_loc)
        when is_builtin f.vname ->
        call_builtin ret f args instr_loc state
    | Local_init(ret,ConsInit(f,args,Plain_func),instr_loc)
      when is_builtin f.vname ->
      let ret = (Some (Var ret,NoOffset)) in
        call_builtin ret f args instr_loc state

    | Call(ret,{enode=Lval(lhost,NoOffset);_},args,instr_loc) ->
      call ~funcall ret lhost args instr_loc state
    | Local_init(ret,ConsInit(f,args,Plain_func),instr_loc) ->
      let ret = (Some (Var ret,NoOffset)) in
      call ~funcall ret (Var f) args instr_loc state

    | i -> Kernel.fatal "instr %a not yet implemented" Cil_datatype.Instr.pretty i

  ;;

  let instruction ~funcall:funcall stmt instr state =
    let pp_ret fmt = function
      | None -> Format.fprintf fmt "<bottom>"
      | Some _ -> Format.fprintf fmt "some state"
    in
    Log.trace (* ~loc:(Codex_options.Location.Instruction instr) *)
      (fun p -> p "Instruction %a" Cil_datatype.Instr.pretty instr) ~pp_ret
      (fun () ->
         instruction' ~funcall:funcall instr state)
  ;;


  type return_result =
    | Return_fails
    | Return_result of {return_state:state;return_value:(In_bits.t * Domain.binary) option}

    (* Returns either a state option (None = Bottom), or the result of
       a return instruction (a state and possibly a value) *)
  type ret_transition =
    | State of state option
    | Return of return_result

  let transition ~funcall:funcall transition state =
    (* Codex_log.feedback "Doing transition: state=%a" pretty_state state; *)
    let open Interpreted_automata in
    match transition with
    | Skip -> State (Some state)
    | Return (None,_) ->
      Log.trace (fun p -> p "Instruction return;") (fun () ->
          (Return (Return_result{return_state=state;return_value=None}):ret_transition))
    | Return (Some exp,_) ->
      Log.trace
        (fun p -> p "Instruction return %a;" Cil_datatype.Exp.pretty exp)
        (fun () ->
           (* We evaluate in case the expression modifies the state. *)
           begin match run state @@ Expression.expression exp with
             | None -> (Return Return_fails:ret_transition)
             | Some (value, state) ->
               let return_value = Some(Cil.bitsSizeOf @@ Cil.typeOf exp |> in_bits, value) in
               (Return (Return_result{return_state=state;return_value}))
           end)
    (* Note: the evaluation of guards is done twice, but it is probably not a problem. *)
    | Guard (e,k,_) -> begin
        match run state @@ Expression.cond_node e with
        | None -> State None
        | Some (bool,state) ->
          let ctx = state.context.ctx in
          let bool = match k with Then -> bool | Else -> Domain.Boolean_Forward.not ctx bool in
            let ctx = Domain.assume ctx bool in
            match ctx with
            | None -> State None
            | Some ctx ->
              let state = { state with context = { state.context with ctx } } in
              State (Some state)
      end
    | Prop _ -> assert false
    | Instr (i,s) ->
      (* Codex_log.feedback "Doing instruction %a@." Cil_datatype.Instr.pretty i; *)
      let state' = instruction ~funcall s i state in
      begin match state' with
        | None -> State None
        | Some state' ->
          (* Codex_log.feedback "After %a: mem=%a level=%d %d" *)
          (*   Cil_datatype.Instr.pretty i (Domain.memory_pretty state'.context.ctx) state'.mem state'.context.loop_nesting_level (Domain.Context.level state'.context.ctx); *)
          State (Some(state'))
      end
    | Enter b -> State (Some (block_entry state b))
    | Leave b -> State (Some (block_close state b))
  ;;


  let cond_node exp state =
    Expression.State_Monad.run state @@ Expression.cond_node exp
  ;;

  let expression exp state =
    Expression.State_Monad.run state @@ Expression.expression exp
  ;;


  (* This module is for setuping the initial memory.  *)
  module Initial_State = struct

    let initial_ctx() = Domain.root_context();;

    let initial_mem ctx = Domain.Memory_Forward.unknown ctx ~level:0


    let initial_state context =
      let ctx = context.ctx in
      { mem = initial_mem ctx;
        var_addresses = VarMap.empty;
        string_addresses = StringMap.empty;
        context
      }

    ;;

    (* Compute the type of the argument to the function. *)
    let compute_initial_function_args kf ctx =
      let open Types.TypedC in
      match function_of_name @@ Kernel_function.get_name kf with
      | Some type_kf -> begin
          (*Codex_log.debug "%a" pp type_kf;*)
          match type_kf.descr with
          | Function {args} ->
            List.map (fun t ->
                let size = sizeof t |> In_bytes.in_bits in
                (size,Domain.binary_unknown_typed ctx ~size t)) args
          | _ -> assert false
        end
      | None ->
        let formals = Kernel_function.get_formals kf in
        let args = formals |> List.map (fun vi ->
            let size = Cil.bitsSizeOf vi.Cil_types.vtype |> in_bits in
            let ctyp = Compile_type.cil_type_to_ctype vi.vtype in
            (size,Domain.binary_unknown_typed ctx ~size ctyp))
        in args
    ;;

    let rec compute_initial_function_args_ret funtyp ctx =
      let open Types.TypedC in
      match (inlined funtyp).descr with
      | Function {ret; args} ->
        List.map (fun t ->
          let size = sizeof t |> In_bytes.in_bits in
          (size,Domain.binary_unknown_typed ctx ~size t)) args, ret
      | Existential {bound_typ;bound_var;body} ->
        let sz = sizeof bound_typ |> In_bytes.in_bits in
        let res = Domain.binary_unknown_typed ~size:sz ctx bound_typ in
        let symb = fresh_symbol () in
        Domain.add_global_symbol ~size:sz ctx symb res ;
        let newft = substitute_symbol body bound_var symb in
        compute_initial_function_args_ret newft ctx

      | _ -> assert false


    let compute_initial_function_args_ret kf ctx =
      let open Types.TypedC in
      match function_of_name @@ Kernel_function.get_name kf with
      | Some funtyp -> compute_initial_function_args_ret funtyp ctx
      | None ->
        let rtyp = Compile_type.cil_type_to_ctype @@ Kernel_function.get_return_type kf in
        let formals = Kernel_function.get_formals kf in
        let args = formals |> List.map (fun vi ->
          let size = Cil.bitsSizeOf vi.Cil_types.vtype |> in_bits in
          let ctyp = Compile_type.cil_type_to_ctype vi.vtype in
          (size,Domain.binary_unknown_typed ctx ~size ctyp))
        in args, rtyp
    ;;


    let initialize_strings strings_used state =
      Log.trace (fun p -> p "initialize_strings") @@ fun () ->
      Datatype.String.Set.fold (fun str state ->
          let malloc_size = (String.length str + 1) |> in_bytes in
          let size = malloc_size |> In_bytes.in_bits in
          (* Display allocated strings using their C representation *)
          let displayed_str = ("\"" ^ Frama_c_kernel.Escape.escape_string str ^ "\"") in
          let id = Codex.Operator.Malloc_id.fresh displayed_str in
          let addr,mem = Domain.Memory_Forward.malloc ~id ~malloc_size state.context.ctx state.mem  in
          let to_store =
            let bitvector =
              (* big-endian representation of integers. *)
              if false then
                let rec loop acc i =
                  if i < String.length str then
                    let char = String.get str i in
                    let value = Char.code char in
                    let acc = Z.(lor) (Z.shift_left acc 8) (Z.of_int value) in
                    loop acc (i + 1)
                  else Z.shift_left acc 8 (* trailing zero *)
                in loop Z.zero 0
              else
                (* little-endian one. *)
                let rec loop acc i =
                  if i < 0 then acc
                  else
                    let char = String.get str i in
                    let value = Char.code char in
                    let acc = Z.(lor) (Z.shift_left acc 8) (Z.of_int value) in
                    loop acc (i - 1)
                in loop Z.zero (String.length str - 1)
            in
            Domain.Binary_Forward.biconst ~size bitvector state.context.ctx
          in
          let mem = Domain.Memory_Forward.store ~size state.context.ctx mem addr to_store in
          let string_addresses = StringMap.add str addr state.string_addresses in
          let state = {state with mem; string_addresses}
          in state
        ) strings_used state
    ;;



    let initialize_function_ptrs functions_used state =
      Log.trace (fun p -> p "initialize_function_ptrs") @@ fun () ->
      (* TODO: This relies on the fact that size of functions is defined,
         which is not the case in MSVC. *)
      Cil_datatype.Varinfo.Set.fold (fun f state ->
          allocate_var state f @@ Some (fun ~size ->
              Domain.binary_unknown ~size state.context.ctx)
        ) functions_used state
    ;;


    (* Given a memory term, returns a memory term where the initial values
       of global variables have been written. If libentry, only
       const variables are written.  *)
    let initialize_global_variables ~libentry (state:state) globals_used =
      Log.trace (fun p -> p "initialize_global_variables") @@ fun () ->
      let module Memory = Domain.Memory_Forward in
      let module Binary = Domain.Binary_Forward in
      let open Cil_types in

      (* We need two passes: because they may be mutually referencing each
         other, we must first allocate the variables, and then write
         pointer to each others. But to avoid writing "unknown" that would
         be immediately overwritten, we try to immediately write the
         correct value when feasible. Thus we try to process the
         dependencies of a variable before this variable, and separate
         allocation only for recursively-defined global variables. *)

      let module VarSet = Cil_datatype.Varinfo.Set in

      (* Note: I could just allocate everything, and then initialize everything,
         it would be simpler. *)
      (* First pass: allocate all the variables, and initialize when
         feasible. *)
      let do_global var (state,allocated_vars,initialized_vars) =

        (* Const values are always known, except when volatile. Arrays of
           consts are also known. *)
        let rec is_const typ =
          (Ast_types.has_attribute "const" typ
           && not (Ast_types.has_qualifier "volatile" typ))
          || match typ.tnode with
          | TArray(typ,_) -> is_const typ
          | _ -> false
        in

        let unknown_initial_value =
          (* Extern declarations are always unknown *)
          (var.vstorage == Extern && not var.vdefined)
          (* We could be more precise here: if there is const somewhere in
             a subtype, then some part of the value may be known (e.g. in
             a array of structs, some fields could be const). *)
          || (libentry && not (is_const var.vtype)) in
        let {init} = Globals.Vars.find var in
        let init_unknown ~size = Domain.binary_unknown ~size state.context.ctx in
        let init_zero ~size = Binary.biconst ~size Z.zero state.context.ctx  in
        let allocated_vars = VarSet.add var allocated_vars in
        (* MAYBE: Be smart, and initialize variables on which we depend,
           when this avoids writing unknown. Probably not worth it. *)
        let initial_valuef,initialized_vars =
          if unknown_initial_value
          then Some init_unknown, VarSet.add var initialized_vars
          else match init with
            | None -> Some init_zero, VarSet.add var initialized_vars
            | Some(SingleInit e) -> begin
                match Cil.constFoldToInt ~machdep:true e with
                | Some i ->
                  Some (fun ~size -> Binary.biconst ~size (Expression.z_of_integer i) state.context.ctx),
                  VarSet.add var initialized_vars
                | None -> (* Some init_unknown *)None, initialized_vars (* Not initialized. *)
              end
            | Some(CompoundInit _) ->
              (* We do not initialize, as foldLeftCompound ~implicit:true
                 adds the missing elements. *)
              (* Some init_zero*) None, initialized_vars
        in
        let state = allocate_var state var initial_valuef in
        state,allocated_vars,initialized_vars
      in

      let rec loop1 ((state,allocated_vars,initialized_vars) as acc) =
        let todo = VarSet.diff globals_used allocated_vars in
        if VarSet.is_empty todo then acc
        else
          let acc = VarSet.fold do_global todo acc in
          loop1 acc
      in
      let (state,allocated_vars,initialized_vars) = loop1 (state,VarSet.empty,VarSet.empty) in

      (* Second pass: all the variables where allocated; initialize the
         remaining ones. *)
      let initialize_var var state =
        let {init} = Globals.Vars.find var in
        let init = match init with None -> assert false | Some x -> x in

        (* Use the memory as the accumulator. Also maintains lv, that
                 cannot go in the accumulator. *)
        let rec doit lv init typ state =
          let size = Cil.bitsSizeOf typ |> in_bits in
          match state,init with
          | None, _ -> None
          | Some state, SingleInit(e) ->
            (* Note: as these should be constant expressions and
               lvalues, state should not change. But everything should
               still work if we were analyzing C++. *)
            let m =
              Expression.expression e >>= fun value ->
              (* TODO: maybe we should compile lvalue only once. *)
              Expression.lvalue lv >>= fun address ->
              return (value,address)
            in
            begin match run state m with
              | None -> None
              | Some ((value,address),state) ->
            (* let (value,state) = run state @@  Expression.expression e in *)
            (* let (address,state) = run state @@ Expression.lvalue lv in *)
                assert(address.bitfield == None);
                let mem = Memory.store ~size (state.context.ctx) state.mem address.address value in
                Some {state with mem}
            end
          | Some state, CompoundInit(ct,initl) ->
            let doinit off init typ acc = doit (Cil.addOffsetLval off lv) init typ acc in
            (* Note: [implicit:true] handles incomplete initializers for us. *)
            Cil.foldLeftCompound
              ~implicit:true
              ~doinit ~ct ~initl ~acc:(Some state)
        in
        doit (Var var, NoOffset) init var.vtype state
      in

      VarSet.fold initialize_var (VarSet.diff allocated_vars initialized_vars) (Some state)
  end

  let initial_state_ret kf root =
    Log.trace (fun p -> p "initial_state_ret %a (computing initial state)" Kernel_function.pretty kf) @@ fun () ->
    let ctx = Initial_State.initial_ctx() in
    let context = {ctx;kinstr=Cil_types.Kglobal;loop_nesting_level=0} in
    let state = Initial_State.initial_state context in
    let args, rtyp = Initial_State.compute_initial_function_args_ret kf ctx in

    let module P = Globals_needed.Make(struct let main = kf end) in
    let state = Initial_State.initialize_function_ptrs P.functions_used state in
    let state = Initial_State.initialize_strings P.strings_used state in
    let state = Initial_State.initialize_global_variables ~libentry:(Kernel.LibEntry.get()) state P.globals_used in

    state,args,rtyp

end
