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

module Codex = Codex
module VarMap = Codex.Extstdlib.Map.Make(Cil_datatype.Varinfo);;
module StringMap = Map.Make(String);;

(* We use a kinstr because lval that do not have eids, and eids
   are sometimes incorrect... *)
module ExpInStmt = 
  Datatype.Pair_with_collections
    (Cil_datatype.Kinstr)
    (Cil_datatype.Exp)
    (struct let module_name = "ExpInStmt2" end)

module LvalInStmt = 
  Datatype.Pair_with_collections
    (Cil_datatype.Kinstr)
    (Cil_datatype.Lval)
    (struct let module_name = "LvalInStmt2" end)

module Log = Tracelog.Make(struct let category = "C2Codex" end);;

module Compile_type = struct

  module StringHash = Datatype_sig.StringHash;;
  let hash_comp_types = StringHash.create 17;;    
  (* Hashtbl.Make(struct include String let hash = Hashtbl.hash end) *)

  let rec cil_type_to_ctype typ =
    let open Cil_types in
    let module Ctypes = Codex.Types.Ctypes in
    match Cil.unrollType typ with
    | TInt (IInt,_) ->
      let size = Cil.bytesSizeOf typ in
      assert (size = 4);
      Ctypes.({descr = Base (size, "int"); pred=Pred.True})
    | TPtr (TComp({cname;cfields=None;_},_),_) ->
      (* Pointers to incomplete structs are seen as a special kind of word with no meaning. *)
      let size = Cil.bytesSizeOf typ in
      Ctypes.({descr = Base (size, "ptr_to_" ^ cname); pred=Pred.True})
    | TFloat(FFloat,_) ->
      let size = Cil.bytesSizeOf typ in
      Ctypes.({descr = Base (size, "float"); pred=Pred.True})
    | TFloat(FDouble,_) ->
      let size = Cil.bytesSizeOf typ in
      Ctypes.({descr = Base (size, "double"); pred=Pred.True})
    | TInt (_,_) ->
      (* Pointers to incomplete types are seen as a special kind of word with no meaning. *)
      let size = Cil.bytesSizeOf typ in
      Ctypes.({descr = Base (size, "anyIntegerType"); pred=Pred.True})
    | TEnum({ename; ekind=IUInt; eitems},_) ->
      let size = Cil.bytesSizeOf typ in
      assert (size = 4) ;
      let nb_items = List.length eitems in
      let pred = Ctypes.Pred.(conjunction (sgeq @@ Const Z.zero) (slt @@ Const (Z.of_int nb_items))) in 
      Ctypes.({descr = Base (size, "int"); pred})
      
    | TPtr (pointed,attr) ->
      let ptyp = cil_type_to_ctype pointed in
      let pred = if List.exists (function Attr("notnull",_) -> true | _ -> false) attr
        then Ctypes.Pred.(neq (Const Z.zero))
        else Ctypes.Pred.True
      in
      let descr = Ctypes.(Ptr{pointed=ptyp; index=Zero}) in
      Ctypes.({descr; pred})
    | TComp ({cname;cfields;cstruct=true;_},_) ->
      begin
        let struct_cname = "struct " ^ cname in
        try StringHash.find hash_comp_types struct_cname
        with Not_found ->
          (* In the C level, we attach a named type to each C struct. *)
          (* Then, in the type level, we attach a struct type to the named type. *)
          let named_typ = Ctypes.{descr=Name(struct_cname); pred=Pred.True;} in
          StringHash.replace hash_comp_types struct_cname named_typ;
          let st_name = Some cname in
          let st_byte_size =  (Cil.bytesSizeOf typ) in
          let st_members =        (* offset, name, type *)
            match cfields with
            | None -> assert false (* Not handled if not a pointer to it. *)
            | Some fields ->
              (* Put normal fields; add padding and replace bitfields by ints as needed. *)
              let (st_members,last_offset,_) = 
                List.fold_left (fun (acc,last_offset,num_padding) fi ->
                    if(fi.fbitfield <> None) then (acc,last_offset,num_padding)
                    (* we see bitfields like padding bytes. *)                                             
                    else
                      let (field_offset,field_size) = Cil.fieldBitsOffset fi in
                      (* Codex_log.feedback "Field %s offset %d size %d " fi.fname field_offset field_size; *)
                      assert(field_offset mod 8 == 0 && field_size mod 8 == 0);
                      let field_offset = field_offset / 8 and field_size = field_size / 8 in
                      let acc,num_padding =
                        if field_offset != last_offset
                        (* Need to add a padding or bitfield field. *)
                        then
                          let name = cname ^ "_padding_" ^ (string_of_int num_padding) in
                          let size = field_offset - last_offset in
                          let typ = Ctypes.({descr = Base (size, name); pred=Pred.True}) in
                          ((field_offset,name,typ)::acc,num_padding + 1)
                        else
                          acc,num_padding
                      in
                      let last_offset = field_offset + field_size in
                      ((field_offset,fi.fname,cil_type_to_ctype fi.ftype)::acc,last_offset,num_padding)
                  ) ([],0,0) fields
              in
              let st_members = 
                if(last_offset = st_byte_size)
                then st_members
                else 
                  let name = cname ^ "_last_padding" in
                  let size = st_byte_size - last_offset in
                  let typ = Ctypes.({descr = Base (size, name); pred=Pred.True}) in
                  (last_offset, name, typ)::st_members
              in
              List.rev st_members 
          in
          (* TODO: Check attributes for better translation of predicates from C types. *)
          let struct_type = Ctypes.{ descr = Structure { st_byte_size = Some st_byte_size;
                                                         st_members};
                                     pred = Pred.True }
          in
          Ctypes.add_type_name_definition struct_cname struct_type;
          named_typ
      end
    | TArray (typ,size,_) ->
      let size = match size with
        | None -> None
        | Some e -> try Some(Ctypes.Const(Cil.lenOfArray64 size)) with Cil.LenOfArray _ -> None in
      let descr = Ctypes.Array(cil_type_to_ctype typ,size) in
      Ctypes.{descr;pred=Pred.True}
    | t ->
      raise (Invalid_argument (Format.asprintf "cil_type_to_my_type: %a" Cil_types_debug.pp_typ t))



end


module type CallingContext = Datatype_sig.S

(* module Make(CallingContext:CallingContext)(Domain:Codex.Domains.Domain_sig.Base) = struct *)
module Make(CallingContext:CallingContext)(Domain:Codex.Domains.With_focusing.S_with_types) = struct

  (* The context in which an instruction is called. *)
  type context = {

    (* We follow a recursive iteration strategy. This is the number of loops in which we are iterating;
       0 means that we are not in a loop (and thus the results that we compute are definitive). *)
    loop_nesting_level: int;

    (* Calling context of a function ; e.g. the stack of callsites leading to calling the function. *)
    calling_context: CallingContext.t;
    (* Current statement (or global if called outside of a statement). *)
    kinstr : Cil_types.kinstr;
    (* Analysis context, representing the current basic block in the current calling context. *)
    ctx: Domain.Context.t;
  }
  ;;

  (* A state is either bottom, or a tuple with a memory, a mapping
     from variable names to their addresses, and a mapping from
     strings to their addresses. *)
  type state = { mem: Domain.memory;
                 var_addresses : Domain.binary VarMap.t;
                 string_addresses: Domain.binary StringMap.t;
                 context: context;
               }


  let pretty_state fmt state =
    Format.fprintf fmt "@[<v>loop nesting:%d@ kinstr:%a@ mem:%a ctx:%a@]"
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
              if not @@ Codex_config.try_hard_on_assertions()
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
    with Not_found -> false
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
          Transfer_functions.Malloc_id.fresh (Format.asprintf "%a" (* Varinfo_Enclosing_Function *)Cil_datatype.Varinfo.pretty vi) in
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
    let addr,mem = try
        let malloc_size =
          try Cil.bytesSizeOf vi.Cil_types.vtype
          with Cil.SizeOfError _ ->
            if Cil.isFunctionType vi.Cil_types.vtype
            then raise Function_Type
            else
              Kernel.warning "Could not compute the size of type %a for %a, defaulting  to 1"
                Cil_datatype.Typ.pretty vi.Cil_types.vtype
                Cil_datatype.Varinfo.pretty vi;
            1
        in
        let size = 8 * malloc_size in
        let addr,mem = Domain.Memory_Forward.malloc ~id ~malloc_size ctx state.mem in
        match initf with
        | None -> addr,mem       (* No initial write. *)
        | Some initf -> 
          let to_store = initf ~size in
          let mem = Domain.Memory_Forward.store ctx ~size mem addr to_store in
          addr,mem
      (* Functions are not deallocated, so we don't have to allocate anything. 
         MAYBE: do the same for every global variables of unknown size? *)
      with Function_Type -> Domain.binary_unknown ctx ~size:(Codex_config.ptr_size()), state.mem
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
    let state = List.fold_left2 (fun state vi (s,arg) ->
        let state = allocate_var state vi @@ Some (fun ~size ->
            assert (size == s);
            arg)
        in
        state) state formals args in
    state
  ;;

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
    bit_offset: int;
    bit_size: int;
  }

  (* Sometimes we need the address of a bitfield (e.g. when assigning
     bitfields), hence this type. *)
  type compiled_lvalue = {
    address: Domain.binary;
    size: int;
    bitfield: bitfield option;
  }

  
  module Expression = struct

    let ptr_bit_size = Cil.bitsSizeOf Cil.voidPtrType;;  

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
          Domain.serialize_binary ~size thenctx one elsectx zero (true, Domain.Context.empty_tuple)
        in
        let newctx,res_tup = Domain.typed_nondet2 thenctx elsectx tup in
        let res,_ = deserialize newctx res_tup in
        Some(res,newctx)
      ;;

      let bofbool_join ~size ctx cond =
        Log.trace (fun p -> p "bofbool_join size:%d" size) (fun fmt _ -> ()) (fun () -> 
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
      val tracem_bin: size:int -> 'a Tracelog.log -> Domain.binary m -> Domain.binary m
      val tracem_bool: 'a Tracelog.log -> Domain.boolean m -> Domain.boolean m                    
      val get_state: unit -> state m
      (* val get_context: context m           *)
      (* val get_ctx: Domain.Context.t m *)
      val load: size:int -> Domain.binary -> Domain.binary m
      val value_of_truth: size:int -> Domain.boolean -> Domain.binary m
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
      module Boolean_Forward:Transfer_functions.Boolean_Forward
        with module Arity := State_Monad_Arity and type boolean := Domain.boolean
      module Binary_Forward:Transfer_functions.Binary_Forward
        with module Arity := State_Monad_Arity
         and type boolean := Domain.boolean and type binary := Domain.binary
      val binary_unknown: size:int -> Domain.binary m
    end = struct
      type 'a m = state -> ('a * state) option;;
      let return x state = Some (x,state)
      let (>>=) m f = fun state ->
        match m state with
        | Some (v,state) -> f v state
        | None -> None
      let ( let* ) = (>>=);;
      let run state x = x state
      let tracem log pp_ret (f:'a m) = 
        (fun state -> 
           let res = Log.trace log pp_ret (fun () ->
               let x = run state f in
               x)
           in res
        )
      ;;
      let tracem_bin ~size log (f:Domain.binary m) = 
        let pp_ret_option fmt = function
          | None -> Format.fprintf fmt "None"
          | Some(ret,state) ->
            let ctx = state.context.ctx in 
            Domain.binary_pretty ~size ctx fmt ret 
        in
        tracem log pp_ret_option f;; 

      let tracem_bool log (f:Domain.boolean m) = 
        let pp_ret_option fmt = function
          | None -> Format.fprintf fmt "None"
          | Some(ret,state) ->
            let ctx = state.context.ctx in 
            Domain.boolean_pretty ctx fmt ret
        in
        tracem log pp_ret_option f;;
      ;;
      
        
      
      let get_state () = fun (state) -> Some (state,state);;
      let get_context = fun (state) -> Some(state.context,state);;            
      let get_ctx = fun (state) -> Some(state.context.ctx,state);;
      let load ~size address state =
        match Domain.Memory_Forward.load ~size state.context.ctx state.mem address with
        | exception Domains.Memory_sig.Memory_Empty -> None
        | (bin, mem) -> Some(bin,{state with mem})
      ;;
      let value_of_truth ~size cond state =
        let ctx = state.context.ctx in
        match bofbool_join ~size ctx cond with
        | None -> None
        | Some(res,ctx) -> Some (res, {state with context = {state.context with ctx}})
      ;;

      
      let register_alarm (alarm,loc) bool = fun state ->
        let ctx = state.context.ctx in
        (* Codex_log.feedback "register alarm boolean = %a" (Domain.boolean_pretty ctx) bool; *)
        let qbool = Domain.query_boolean ctx bool in
        Assertion_Table.register_alarm qbool (alarm,loc,state.context.kinstr) state;
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
      ;;
      
      let register_lvalue lval bin = return ();;    (* TODO *)
      let register_expression exp bin = fun state ->
        let level = state.context.loop_nesting_level in
        if level > 0 then ()
        else begin
          let open Register_Table in
          let h = ExpInStmtHash.find exptrie (state.context.kinstr,exp) in
          let size = Cil.(bitsSizeOf (typeOf exp)) in
          (* Codex_log.feedback "Registering %a (%a) with %s" Cil_datatype.Exp.pretty exp ExpInStmt.pretty (state.context.kinstr,exp)  (Format.asprintf "%a" (Domain.binary_pretty ~size state.context.ctx) bin); *)
          CallingContextHash.replace h state.context.calling_context
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
        CallingContextHash.replace h state.context.calling_context x;
          (* (Format.asprintf "%a" (Domain.boolean_pretty state.context.ctx) bool); *)
        Some((),state)
      ;;


      (* This is more similar to what we had before. The advantage is that it forces 
         propagation of the boolean expression, which is otherwise not done. 
         The performance cost is not important (the precision increase exists, but is also rare). *)
      let register_boolean_expression exp bool = fun state ->
        let size = 32 in
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
      
      module Transfer_functions = struct
      
        module Conversion(* :Transfer_functions.Conversions.Conversion *) = struct
          module From_Arity = Domains.Domain_sig.Context_Arity_Forward(Domain.Context);;
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

        module Boolean_Forward = Transfer_functions.Conversions.Convert_Boolean_Forward(Conversion)(struct include Types include Domain.Boolean_Forward end)
        module Binary_Forward = Transfer_functions.Conversions.Convert_Binary_Forward(Conversion)(struct include Types include Domain.Binary_Forward end)



        let binary_unknown ~size =
          let* state = get_state() in
          let level = state.context.loop_nesting_level in
          let ctx = state.context.ctx in
          return (Domain.binary_unknown ~size ctx)
      end

      include Transfer_functions
          

    end


    open State_Monad;;

    (* Change the size of a value. *)
    let cast_size ~sext ~from_size ~to_size v =
      if from_size > to_size 
      then Binary_Forward.bextract ~index:0 ~size:to_size ~oldsize:from_size v
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
        | CEnum {eival;_} ->
          (match Cil.isInteger eival with
           | None -> assert false   (* Should not happen *)
           | Some v -> Binary_Forward.biconst ~size (z_of_integer v))
        | CStr str -> let* state = get_state () in return @@ StringMap.find str state.string_addresses
        | CWStr _ws -> (* return (Conv.loc_of_wstring env ws) *) assert false
        | CReal _ -> binary_unknown ~size
    ;;


    let rec expression' exp =
      let exp_size = Cil.(bitsSizeOf (typeOf exp)) in
      match exp.enode with
      | Const c -> constant ~size:exp_size c
      | Lval(lv) ->
        let* loc = lvalue lv in
        let size = loc.size in
        let address = loc.address in
        let* () =
          if trivially_valid lv
          then return ()
          else let* valid = Binary_Forward.valid ~size Transfer_functions.Read address in
          register_alarm (Alarms.Memory_access(lv,Alarms.For_reading),exp.eloc) valid
        in
        let* v =
          (* Translate volatile using bunknown. *)
          (if Cil.typeHasQualifier "volatile" @@ Cil.typeOfLval lv
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
            else let op = match Cil.unrollType @@ Cil.typeOf exp with
                | TInt(ikind,_) -> if Cil.isSigned ikind then Binary_Forward.bsext ~oldsize:bit_size else Binary_Forward.buext ~oldsize:bit_size 
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
        let (TArray(t,_,_)) = Cil.unrollType @@ Cil.typeOfLval lv in
        Binary_Forward.bindex ~size:ptr_bit_size (Cil.bytesSizeOf t) loc.address zero
      | BinOp(bop, e1, e2, _) -> binop exp_size bop e1 e2
      | UnOp(uop, e1, _) -> unop exp_size uop e1
      | SizeOf(typ) -> Binary_Forward.biconst ~size:exp_size (Z.of_int (Cil.bytesSizeOf typ))
      | SizeOfE(exp) -> 
        Binary_Forward.biconst ~size:exp_size (Z.of_int (Cil.bytesSizeOf (Cil.typeOf exp))) 
      | CastE(to_typ,subexp) ->
        let from_typ = Cil.typeOf subexp in
        let* subexp = expression subexp in
        (match Cil.unrollType from_typ, Cil.unrollType to_typ with
         | (TInt (from_kind,_) | TEnum({ekind = from_kind;_},_)),
           (TEnum({ekind = to_kind;_},_) | TInt (to_kind,_)) ->
           (match Cil.bitsSizeOfInt from_kind, Cil.bitsSizeOfInt to_kind with
            | from,to_ when from == to_ -> return subexp
            | from,to_ when from < to_ ->
              let from_size = Cil.bitsSizeOf from_typ in
              assert(from == from_size);
              if Cil.isSigned from_kind
              then Binary_Forward.bsext ~size:to_ ~oldsize:from_size subexp
              else Binary_Forward.buext ~size:to_ ~oldsize:from_size subexp
            | from,to_ -> Binary_Forward.bextract ~index:0 ~size:to_ ~oldsize:from subexp)
         | TFloat _, (TInt _ | TEnum _)
         | (TInt _ | TEnum _), TFloat _
         | TFloat _, TFloat _ -> binary_unknown ~size:(Cil.bitsSizeOf to_typ)

         (* Conversion between pointers, and putting pointers in arrays *)
         | (TPtr _ | TArray _) , (TPtr _ | TArray _) -> return subexp
         | (TInt _ | TEnum _ | TPtr _), (TInt _ | TEnum _ | TPtr _)
           when Cil.bitsSizeOf from_typ == Cil.bitsSizeOf to_typ -> return subexp
         | _, _ -> Kernel.fatal "cast not handled: from %a to %a (sizes: %d and %d)" 
                     Cil_datatype.Typ.pretty from_typ Cil_datatype.Typ.pretty to_typ
                     (Cil.bitsSizeOf from_typ) (Cil.bitsSizeOf to_typ))
      | _ -> Kernel.fatal "Expression not implemented: %a" Cil_datatype.Exp.pretty exp

    and expression: Cil_types.exp -> Domain.binary m = fun exp ->
      State_Monad.tracem_bin ~size:Cil.(bitsSizeOf (typeOf exp)) 
        (fun p -> p "Expression %a" Cil_datatype.Exp.pretty exp)  @@
      let* result = expression' exp in
      let* () = register_expression exp result in
      return result

    and apply_binpred bop typ v1 v2 =
      let signed = match (Cil.unrollType typ) with
        | Cil_types.TInt(ikind,_) | Cil_types.TEnum({Cil_types.ekind = ikind;_},_) -> Cil.isSigned ikind
        | Cil_types.TPtr _ | Cil_types.TArray _ -> false (* Pointer is seen as unsigned integers. *)
        | Cil_types.TFloat _ -> false (* And also floats, currently. *)
        | TFun _ -> assert false
        | TNamed _ -> assert false
        | TVoid _ -> assert false
        | TComp _ -> assert false
        | TBuiltin_va_list _ -> assert false                  
      in
      let size = Cil.bitsSizeOf typ in
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


    and binop exp_size bop e1 e2 =
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
        | PlusA | MinusA | Mult | Div | Mod when Cil.isFloatingType typ_e1 ->
          assert(Cil.isFloatingType typ_e2);
          binary_unknown ~size:exp_size

        (* Arithmetical operations. *)
        | PlusA -> Binary_Forward.biadd ~size:exp_size ~nsw ~nuw ~nusw v1 v2
        | MinusA -> Binary_Forward.bisub ~size:exp_size ~nsw ~nuw ~nusw v1 v2
        | Mult -> Binary_Forward.bimul ~size:exp_size ~nsw ~nuw v1 v2
        | Div | Mod -> 
          let* zero = Binary_Forward.biconst ~size:exp_size Z.zero in
          let* bool = Binary_Forward.beq ~size:(exp_size) zero v2  in
          let* bool = Boolean_Forward.not bool in
          let* () = register_alarm (Alarms.Division_by_zero e2,e2.eloc) bool in
          let typ = Cil.typeOf e1 in
          (* Operations that depends on the sign. *)
          assert (Cil.isIntegralType typ); (* For now *)
          let size = Cil.bitsSizeOf typ in
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
          assert (Cil.isIntegralType typ);
          assert (Cil.bitsSizeOf typ == exp_size);
          (* C operators do not mandate that operands have the same
             size, but Codex operand does. Cast the right operand to
             the correct size. *)
          let from_size = (Cil.bitsSizeOf @@ Cil.typeOf e2) in
          let* v2 = cast_size ~sext:false ~from_size ~to_size:exp_size v2 in
          (match (Cil.isSignedInteger typ,op) with
           (* Note: the behaviour of shifting negative values is actually unspecified. *)
           | (true,Shiftlt) ->  Binary_Forward.bshl ~size:exp_size ~nsw ~nuw v1 v2
           | (true,Shiftrt) ->  Binary_Forward.bashr ~size:exp_size v1 v2
           | (false,Shiftlt) -> Binary_Forward.bshl ~size:exp_size ~nsw ~nuw v1 v2
           | (false,Shiftrt) -> Binary_Forward.blshr ~size:exp_size v1 v2
           | _ -> assert false
          )

        (* Predicates on integers and pointers. *)
        | Lt | Gt | Le | Ge | Ne | Eq ->
          let* b = apply_binpred bop Cil.(unrollType (typeOf e1)) v1 v2 in
          value_of_truth ~size:exp_size  b

        (* Pointer operations. *)
        | PlusPI ->
          let k = Cil.(bytesSizeOf (typeOf_pointed (typeOf e1))) in
          let index_size = Cil.bitsSizeOf typ_e2 in
          let sext = (Cil.isSignedInteger typ_e2) in
          let* off = cast_size ~sext ~from_size:index_size ~to_size:ptr_bit_size v2 in          
          Binary_Forward.bindex ~size:ptr_bit_size k v1 off
        | MinusPI ->
          let k = Cil.(bytesSizeOf (typeOf_pointed (typeOf e1))) in
          let index_size = Cil.bitsSizeOf typ_e2 in
          let* off = cast_size ~sext:(Cil.isSignedInteger typ_e2) ~from_size:index_size ~to_size:ptr_bit_size v2 in
          let* zero = Binary_Forward.biconst ~size:ptr_bit_size Z.zero in
          let* moff = Binary_Forward.bisub ~size:ptr_bit_size ~nsw:false ~nuw:false ~nusw:false zero off in
          Binary_Forward.bindex ~size:ptr_bit_size k v1 moff
        | MinusPP ->
          let k = Cil.(bytesSizeOf (typeOf_pointed (typeOf e1))) in
          let k = Z.of_int k in
          let* diff = Binary_Forward.bisub ~size:exp_size ~nsw:false ~nuw:false ~nusw:false v1 v2 in
          let* k = Binary_Forward.biconst ~size:exp_size k in
          Binary_Forward.biudiv ~size:exp_size diff k

        (* Boolean specials. *)
        | LAnd | LOr -> assert false

    and unop exp_size uop e1 =
      match uop with
      | Neg ->
        let* v1 = expression e1 in
        let* zero = Binary_Forward.biconst ~size:exp_size Z.zero in
        Binary_Forward.bisub ~size:exp_size ~nsw:true ~nuw:false ~nusw:false zero v1
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

    (* TODO: On renvoie (addresse,taille de la lvalue en bits,offset en bits,taille en bits) *)
    and loffset typ (v:Domain.binary) offs =
      let open Cil_types in
      let size =
        try Cil.bitsSizeOf typ
        with Cil.SizeOfError _ ->
        match Cil.unrollType typ with
        | TFun _ ->
          Codex_config.function_size()
        | TArray(_,None,_) -> assert false
        | _ -> assert false
      in
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
        return {address = v; size; bitfield = Some({bit_offset;bit_size})}

      (* Cannot be a bitfield. *)
      | Field(fi,offs) -> begin
          let (bit_offset,bit_size) = Cil.bitsOffset typ (Field(fi,NoOffset)) in
          assert(bit_offset mod 8 == 0);
          assert(bit_size mod 8 == 0);
          do_field fi ~byte_offset:(bit_offset / 8) ~byte_size:(bit_size / 8) offs
        end

      | Index(exp,offs) ->
        let pointed_typ = Cil.typeOf_array_elem typ in
        let* off = expression exp in
        let typ_exp = Cil.typeOf exp in
        let size = Cil.(bitsSizeOf typ_exp) in
        let k = Cil.bytesSizeOf pointed_typ in
        let* off = cast_size ~sext:(Cil.isSignedInteger typ_exp)
          ~from_size:size ~to_size:ptr_bit_size off in
        let next = fun () ->
          let* v = Binary_Forward.bindex ~size:ptr_bit_size k v off in
          loffset pointed_typ v offs
        in

        (* This is to help the analysis. We already have \valid, but
           these stricter conditions help reduce the abstract state. *)
        (match Cil.unrollType typ with
         | TArray(_elt_typ,length,_) -> begin
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
        let* res = apply_binpred op Cil.(unrollType (typeOf e1)) v1 v2 in
        let* () = register_boolean_expression e res in
        return res

      | UnOp(LNot,e1,_) ->
        let* v = cond_node e1 in
        let* res = Boolean_Forward.not v in
        let* () = register_boolean_expression e res in
        return res

      | CastE(to_typ,e) when Cil.isIntegralType @@ Cil.unrollType to_typ -> cond_node' e
    
      | _ -> 

        (* The conversion to boolean depends on the static type of e. In
           every case we compare against 0, but 0 means "null pointer" for
           pointer types, and "0.0" in float types. *)
        (* Note: we do not register the boolean expression here; we prefer to register
           the non-boolean one.*)
        match (Cil.unrollType (Cil.typeOf e)) with
        | TInt _ | TEnum _ | TPtr _ as typ ->
          let* cond = expression e in
          let size = (Cil.bitsSizeOf typ) in
          let* izero = Binary_Forward.biconst ~size Z.zero in
          let* eqzero = Binary_Forward.beq ~size cond izero in
          Boolean_Forward.not eqzero
        | _ -> Codex_options.fatal "Not yet implemented cond_node %a" Cil_datatype.Exp.pretty e
                 
    and cond_node e =
      State_Monad.tracem_bool (fun p -> p "Condition node %a" Cil_datatype.Exp.pretty e) (cond_node' e)
                 
  end

  open Expression.State_Monad


  (* Insert the contents of a bitfield in a previous value. *)
  let bitfield_replace ctx old oldsize {bit_offset;bit_size} newv newvsize =
    (* Newv may be too big to enter in the bitfield; take only the last bits. *)
    let newv =
      if bit_size == newvsize then newv
      else Domain.Binary_Forward.bextract ~oldsize:newvsize ~index:0 ~size:bit_size ctx newv
    in
    let previous = 
      (if bit_offset == 0 then newv
       else
         let prev = Domain.Binary_Forward.bextract ~oldsize ~index:0 ~size:bit_offset ctx old in
         Domain.Binary_Forward.bconcat ~size1:bit_size ~size2:bit_offset ctx newv prev
      )
    in
    let end_index = bit_offset + bit_size in
    if end_index == oldsize
    then previous
    else
      let next = 
        Domain.Binary_Forward.bextract ~oldsize ~index:end_index ~size:(oldsize - end_index) ctx old 
      in
      (* Kernel.feedback "concat2 term %a size2 %d" Term.pretty (Obj.magic previous) end_index; *)
      Domain.Binary_Forward.bconcat ~size1:(oldsize - end_index) ~size2:end_index ctx next previous
  ;;


  (* Note: could be in the monad, as a unit m. *)
  let store_lvalue ~instr_loc lv newv newvsize state =
    match run state @@
      (let* loc = Expression.lvalue lv in
       let* () =
         if trivially_valid lv
         then return ()
         else
           let* valid = Binary_Forward.valid ~size:loc.size Transfer_functions.Write loc.address in
           register_alarm (Alarms.Memory_access(lv,Alarms.For_writing),instr_loc) valid 
  in
       return loc)
    with
    | None -> None
    | Some(loc,state) -> begin
        let ctx = state.context.ctx in
        match loc.bitfield with
        | None -> begin
            match Domain.Memory_Forward.store ~size:loc.size ctx (state.mem) loc.address newv with
            | exception Domains.Memory_sig.Memory_Empty -> None
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


  let init var state init =
    let open Cil_types in

    let rec doit lv init typ state =
      let size = Cil.bitsSizeOf typ in
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
          let size = Cil.(bitsSizeOf (typeOf arg)) in
          Expression.expression arg >>= fun arg ->
          return ((size,arg)::acc)) (return []) args 
      in
      match m with
      | None -> None
      | Some(args,state) ->
        let args = List.rev args in
        (* Should this be a builtin, or just be done directly here? Probably be done directly. *)
        let mem = Domain.builtin_show_each_in suffix state.context.ctx args state.mem in
        let state = {state with mem} in
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
          let size = size * 8 in
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
        let size = Cil.bitsSizeOf @@ Cil.typeOfLval ret in
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
      let module Ctypes = Codex.Types.Ctypes in      
      (* let open Lang.Memory in *)
      let sizeexp = match args with
        | [x] -> x
        | _ -> failwith "Wrong number arguments to malloc"
      in
      let malloc_size = match Cil.constFoldToInt ~machdep:true sizeexp with
        | Some size -> Z.to_int size
        | None -> assert false
      in
      (* Does the lvalue has a type that we want to treat specially? (Here we
         only support variable lvalues.) *)
      (* Really do the malloc. *)
      let untyped () =
        Codex_options.feedback "malloc: default case";
        let sid = match state.context.kinstr with Kglobal -> assert false | Kstmt stmt -> stmt.sid in
        let id = Codex.Transfer_functions.Malloc_id.fresh ("malloc" ^ string_of_int sid) in
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
          let vtype = Cil.typeOfLval lval in
          let mytype = Compile_type.cil_type_to_ctype vtype in
          Codex_options.feedback "malloc: typed case with type %a at loc %a" Cil_types_debug.pp_typ vtype Cil_types_debug.pp_location instr_loc;
          let mytype = match mytype.Ctypes.descr, mytype.Ctypes.pred with
            | Ctypes.(Ptr _, pred) -> {mytype with Ctypes.pred = Ctypes.Pred.conjunction pred @@ Ctypes.Pred.(neq (Const Z.zero))}                                           
            | _ -> assert false
          in
          let level = state.context.loop_nesting_level in
          let ptr = Domain.binary_unknown_typed ~size:32 state.context.ctx mytype in
          let z = Domain.Binary_Forward.(beq ~size:32 state.context.ctx ptr (biconst ~size:32 Z.zero state.context.ctx)) in
          let nz = Domain.Boolean_Forward.not state.context.ctx z in
          match Domain.assume state.context.ctx nz with
          | None -> None
          | Some newctx ->
            let state = {state with context = {state.context with ctx = newctx}} in
          begin match store_lvalue ~instr_loc lval ptr malloc_size state with
            | None -> None
            | Some state -> 
              let init_val = Domain.Binary_Forward.buninit ~size:(8*malloc_size) state.context.ctx in
              let mem = 
                Domain.Memory_Forward.store ~size:(8*malloc_size) state.context.ctx state.mem ptr init_val in
              Some {state with mem}
          end
        | _ -> untyped ()
      in
      state
    ;;


    
  end
  


  (* TODO: Define an alist of builtins instead. *)
  let is_builtin name =
    List.mem name ["malloc";"free";"alloca";"__fc_vla_alloc";"__fc_vla_free";"exit";
                "Frama_C_bzero";"calloc";"realloc";
                "__VERIFIER_assert"; "__VERIFIER_check"; "__VERIFIER_error"; "__VERIFIER_assume";
                "__VERIFIER_nondet_int"]
  || String.starts_with ~prefix:"Frama_C_show_each" name
  ;;

  let call_builtin ret f args instr_loc state =
    if String.starts_with ~prefix:"Frama_C_show_each" f.Cil_types.vname
    then
      Builtin.show_each ret f args instr_loc state
    else match f.Cil_types.vname with
      | "exit" -> None
      | "Frama_C_bzero" -> Builtin.bzero ret f args instr_loc state
      | "__VERIFIER_assume" -> Builtin.verifier_assume ret f args instr_loc state
      | "__VERIFIER_assert" -> Builtin.verifier_assert ret f args instr_loc state
      | "__VERIFIER_check" -> Builtin.verifier_check ret f args instr_loc state
      | "__VERIFIER_error" -> Builtin.verifier_error ret f args instr_loc state
      | "__VERIFIER_nondet_int" -> Builtin.verifier_nondet_int ret f args instr_loc state
      | "malloc" -> Builtin.malloc ret f args instr_loc state
      | "free" -> Builtin.free ret f args instr_loc state                                                      
      | _ -> Codex_log.fatal "Need to handle builtin %s" f.Cil_types.vname
  ;;

  (* Call to unknown function *)
  let call_to_unknown state ret f args instr_loc =
    Codex_options.warning ~once:true "Warning: no definition for %a" Cil_datatype.Varinfo.pretty f;

    (* Note: unsound: this considers that the function assigns
       nothing, and just returns a result. *)
    (match ret with
     | None -> Some state
     | Some lv ->
       let size = (Cil.bitsSizeOf (Cil.typeOfLval lv)) in
       let ctx = state.context.ctx in
       let value = Domain.binary_unknown ~size ctx in
       store_lvalue ~instr_loc lv value size state)
  ;;

  type funcall = 
    Kernel_function.t -> (int * Domain.binary) list -> state ->
    (int * Domain.binary) option * state option  

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
      let expected_size = (Cil.bitsSizeOf (Cil.getReturnType f.Cil_types.vtype)) in
      assert(retsize == expected_size);
      store_lvalue ~instr_loc lvalue retval retsize state
  ;;

  (* Handle the call instruction for non builtins. *)
  let call ~funcall ret lhost args instr_loc state =
    let m = run state @@
      (* Compile the arguments of the function (they may be subject to runtime errors) *)
      List.fold_left (fun acc arg ->
          acc >>= fun acc ->
          let size = Cil.(bitsSizeOf (typeOf arg)) in
          Expression.expression arg >>= fun arg ->
          return ((size,arg)::acc)) (return []) args
    in
    match m with
    | None -> None
    | Some (args,state) -> begin
        let args = List.rev args in
        let f = match lhost with
          | Cil_types.Var f -> f
          | _ -> assert false        (* TODO: enumerate. *)
        in
        let kf = Globals.Functions.get f in
        if not (Kernel_function.is_definition (Globals.Functions.get f))
        then call_to_unknown state ret f args instr_loc
        else call_known ~funcall state ret f args instr_loc
      end
  ;;

  let instruction' ~funcall:funcall instr state =
    let open Cil_types in
    let funcall:funcall = funcall in
    match instr with
    | Skip _ -> Some state
    | Asm _ -> 
      Codex_options.warning "Skipping assembly instruction";
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
      when false && Cil.isStructOrUnionType @@ Cil.unrollType @@ Cil.typeOf exp ->
      let open Cil_types in
      let lvsrc = match exp.enode with Lval lv -> lv | _ -> assert false in
      let rec f lvdst lvsrc (state : state) : state option =
        let typ = Cil.unrollType @@ Cil.typeOfLval lvsrc in
        Codex_log.feedback "Type of %a is %a" Cil_datatype.Lval.pretty lvsrc Cil_datatype.Typ.pretty typ;        
        if Cil.isArithmeticOrPointerType typ then
          match run state @@ (Expression.expression (Cil.dummy_exp @@  Lval lvsrc)) with
          | None -> None
          | Some (value,state) ->
            store_lvalue ~instr_loc lvdst value (Cil.bitsSizeOf typ) state
        else if Cil.isArrayType typ then begin
          (* The type should be known statically. *)
          let len = match typ with TArray(_,i,_) -> i | _ -> assert false in
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
          assert(Cil.isStructOrUnionType typ);
          let compinfo = match typ with Cil_types.TComp(ci,_) -> ci | _ -> assert false in
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
      if Cil.isStructOrUnionType @@ Cil.unrollType @@ Cil.typeOf exp
      then Codex_options.warning "Assignment of union or structure may\
                                  loose precision; we should use memcopy";
      begin match run state @@ Expression.expression exp with
        | None -> None
        | Some(value,state) ->
          store_lvalue ~instr_loc lv value (Cil.bitsSizeOf @@ Cil.typeOf exp) state
      end
    | Local_init(vi,AssignInit i,_) -> init vi state i

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
  Log.trace (fun p -> p "Instruction %a" Cil_datatype.Instr.pretty instr) pp_ret (fun () -> 
      instruction' ~funcall:funcall instr state)
  ;;
  
  let transition ~funcall:funcall transition state =
    (* Codex_log.feedback "Doing transition: state=%a" pretty_state state; *)
    let open Interpreted_automata in
    match transition with
    | Skip -> Some state,None
    | Return (None,_) -> Some state,None
    | Return (Some exp,_) ->
      (* We evaluate in case the expression modifies the state. *)
      begin match run state @@ Expression.expression exp with
        | None -> None,None
        | Some (value, state) -> Some state, Some (Cil.bitsSizeOf @@ Cil.typeOf exp, value)
      end
    (* Note: the evaluation of guards is done twice, but it is probably not a problem. *)
    | Guard (e,k,_) -> begin
        match run state @@ Expression.cond_node e with
        | None -> None,None
        | Some (bool,state) ->
          let ctx = state.context.ctx in
          let bool = match k with Then -> bool | Else -> Domain.Boolean_Forward.not ctx bool in
            let ctx = Domain.assume ctx bool in
            match ctx with
            | None -> None, None
            | Some ctx -> 
              let state = { state with context = { state.context with ctx } } in
              Some state,None
      end
    | Prop _ -> assert false
    | Instr (i,s) ->
      (* Codex_log.feedback "Doing instruction %a@." Cil_datatype.Instr.pretty i; *)
      let state' = instruction ~funcall s i state in
      begin match state' with
        | None -> None,None
        | Some state' -> 
          (* Codex_log.feedback "After %a: mem=%a level=%d %d" *)
          (*   Cil_datatype.Instr.pretty i (Domain.memory_pretty state'.context.ctx) state'.mem state'.context.loop_nesting_level (Domain.Context.level state'.context.ctx); *)
          Some(state'),None
      end
    | Enter b -> Some (block_entry state b),None
    | Leave b -> Some (block_close state b),None
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
      let open Types.Ctypes in
      try
        let type_kf = function_of_name @@ Kernel_function.get_name kf in
        (*Codex_log.debug "%a" pp type_kf;*)
        let args = match type_kf.descr with
        | Function (r, a) ->
          List.map (fun t ->
          let size = sizeof t * 8 in
          (size,Domain.binary_unknown_typed ctx ~size t)) a
        | _ -> assert false
        in args
      
      with Undefined_type a ->
        let formals = Kernel_function.get_formals kf in
        let args = formals |> List.map (fun vi ->
          let size = Cil.bitsSizeOf vi.Cil_types.vtype in
          let ctyp = Compile_type.cil_type_to_ctype vi.vtype in
          (size,Domain.binary_unknown_typed ctx ~size ctyp))
        in args
    ;;



    let initialize_strings strings_used state =
      Datatype.String.Set.fold (fun str state ->
          let malloc_size = (String.length str + 1) in
          let size = 8 * malloc_size in
          let id = Codex.Transfer_functions.Malloc_id.fresh str in
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
          (Cil.typeHasAttribute "const" typ
           && not (Cil.typeHasQualifier "volatile" typ))
          || match typ with
          | TArray(typ,_,_) -> is_const typ
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
          let size = Cil.bitsSizeOf typ in
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


    ;;

    

  end

  let initial_state kf root =
    let ctx = Initial_State.initial_ctx() in
    let context = {ctx;calling_context=root;kinstr=Cil_types.Kglobal;loop_nesting_level=0} in
    let state = Initial_State.initial_state context in
    let args = Initial_State.compute_initial_function_args kf ctx in

    let module P = Globals_needed.Make(struct let main = kf end) in
    let state = Initial_State.initialize_function_ptrs P.functions_used state in
    let state = Initial_State.initialize_strings P.strings_used state in
    let state = Initial_State.initialize_global_variables ~libentry:(Kernel.LibEntry.get()) state P.globals_used in
    
    state,args


  
end
