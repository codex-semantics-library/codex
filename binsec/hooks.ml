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

module Log = Tracelog.Make(struct let category = "Hooks" end)
module Addr_tbl : Hashtbl.S with type key = Virtual_address.t = Hashtbl.Make(struct
  type t = Virtual_address.t
  let equal x y = Virtual_address.compare x y = 0
  let hash = Virtual_address.to_int
end)


module Make(State:Dba2Codex.StateS)(Record_cfg:Record_cfg.S) = struct
  module State = State
  module Record_cfg = Record_cfg
  module Ctypes = Types.Ctypes;;
  module Domain= State.Domain

  type skip_type = NotWhenInterpreting | Always

  (** Hooks can decide to eventually not hook by throwing this exception.   *)
  (* exception No_hook *)
  
  type hook =
    | SkipTo of skip_type * Virtual_address.t (** skip to address *)
    | Hook of (Record_cfg.t -> State.t ->
               Record_cfg.t * (Virtual_address.t * State.t) list)
    (** Manually change the transfer function of an instruction. *)
    | ChangeState of (State.t -> State.t)
    (** Modifies the state before executing the corresponding instruction. *)
    | Unroll of int
    (** Unroll every loop having this address as its head. Argument: number
        of iterations. *)
    | EndPath
    (** End this trace *)
    | Return of Types.Ctypes.typ option
    (** End this trace and check the return type if given *)
    | EntryCall of string * Types.Ctypes.typ
    (** Used during interprocedural analysis to enter the entry function, but should be replaced the first time it is encountered *)

  let unoption msg = function
    | Some x -> x
    | None -> raise (Failure msg)

  let get_sym name =
    Loader_utils.address_of_symbol_by_name ~name (Kernel_functions.get_img ())
    |> unoption ("Cannot find symbol " ^ name)
    |> Virtual_address.create


  let hook_table = Addr_tbl.create 1;;
  let find_hook addr = Addr_tbl.find hook_table addr

  let stop addr ~msg =
    Addr_tbl.add hook_table addr (EndPath, msg);;

  let entrycall addr ftyp ~name ~msg =
    Addr_tbl.add hook_table addr (EntryCall (name,ftyp), msg);;

  let return addr rtyp ~msg =
    Addr_tbl.add hook_table addr (Return rtyp, msg);;

  let hook addr ~msg f =
    Addr_tbl.add hook_table addr (Hook f, msg);;

  let skip_always addr ~dest ~msg =
    Addr_tbl.add hook_table addr (SkipTo (Always, dest), msg);;
  let _unroll i max_iter ~msg =
    Addr_tbl.add hook_table (Virtual_address.create i) (Unroll max_iter, msg);;
  let lift_int_sym f sym = f (get_sym sym);;
  let stop_at = lift_int_sym stop;;
  let hook_at = lift_int_sym hook;;
  let _skip_always_at sym ~destsym =
    lift_int_sym (fun dest -> lift_int_sym skip_always sym ~dest) destsym;;


  let bunknown ~size ctx =
    Domain.binary_unknown ~size ctx
  ;;

  (* TODO: push and pop. *)
  let decr ctx state reg num =
    let value = State.get ~size:32 state reg in
    let new_value = Domain.Binary_Forward.bisub ~size:32 ~nsw:false ~nuw:false ~nusw:false ctx value @@
      Domain.Binary_Forward.biconst ~size:32 (Z.of_int num) ctx in
    State.set state reg new_value
  ;;

  let pop_reg state reg =
    let ctx = state.State.ctx in
    let reg_value = State.get ~size:32 state reg in
    let loaded_value,memory = Domain.Memory_Forward.load ~size:32 ctx state.memory reg_value in
    let new_reg_value = Domain.Binary_Forward.biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false ctx reg_value @@
      Domain.Binary_Forward.biconst ~size:32 (Z.of_int 4) ctx in
    let state = {state with memory } in
    let state = State.set state reg new_reg_value in
    loaded_value, state
  ;;

  let ret state =
    (* 0: esp<32> := (esp<32> + 4<32>); *)
    (* 1: goto @[(esp<32> - 4<32>),4] #return *)
    let ret_address,state = pop_reg state "esp" in
    let bl = Domain.Query.binary ~size:32 state.ctx ret_address in
    let sup = Z.pred @@ Z.shift_left Z.one 32 in
    let result = Domain.Query.binary_fold_crop ~size:32 bl ~inf:Z.zero ~sup [] (fun target acc ->
        Codex_log.feedback "Returning to %s@\n" (Z.to_string target);
        Codex_log.feedback "Returning to state %a@\n"
          State.dump_state state              ;
        let target = Virtual_address.of_int64 @@ Z.to_int64 target in
        (target,state)::acc
      ) in
    result
  ;;

  let typed_malloc name ctx state =
    let not_null = Ctypes.(Pred.(neq (Const Z.zero))) in
    let typ = Ctypes.type_of_name name in
    let malloc_size = Ctypes.sizeof typ in
    let ptyp = Ctypes.({descr = Ptr{pointed=typ;index=Zero}; pred=not_null}) in
    let ptr = Domain.binary_unknown_typed ~size:32 ctx ptyp in
    Log.debug (fun p -> p "Filling malloc'ed region with unknown value of size %d bytes" malloc_size);

    (* let init_val = Domain.binary_unknown_typed ~size:(malloc_size*8) ~level:(Domain.Context.level ctx) ctx typ in *)
    let init_val = Domain.Binary_Forward.buninit ~size:(malloc_size*8) ctx in
    let mem = Domain.Memory_Forward.store ~size:(malloc_size*8) ctx state.State.memory ptr init_val in
    ptr, {state with State.memory = mem}
  ;;

  let hook_malloc ~sym typ_name =
    match Loader_utils.address_of_symbol_by_name ~name:sym
            (Kernel_functions.get_img ()) with
    | None -> ()
    | Some address ->
      hook (Virtual_address.create address) ~msg:("malloc " ^ typ_name) (fun trace state ->
          let ptr, state = typed_malloc typ_name state.ctx state in
          let state = State.set state "eax" ptr in
          trace, ret state
        )
  ;;

  let init_hooks_malloc () =
    let mapper = fun n -> if String.starts_with ~prefix:"struct" n then (List.nth (String.split_on_char ' ' n) 1), n else (n,n) in
    let types = List.map mapper @@ Ctypes.get_type_definitions () in
    List.iter (fun (m,n) -> hook_malloc ~sym:("malloc_" ^ m) n) types
  ;;

  init_hooks_malloc () ;;
  
  (*
  hook_malloc ~sym:"malloc_int" "int";;
  hook_malloc ~sym:"malloc_uf_node" "uf_node";;
  hook_malloc ~sym:"malloc_T" "T";;
  hook_malloc ~sym:"malloc_elist" "elist";;
  hook_malloc ~sym:"malloc__gdsl_node" "_gdsl_node";;
  hook_malloc ~sym:"malloc__gdsl_list" "_gdsl_list";;
  hook_malloc ~sym:"malloc__gdsl_list_cursor" "_gdsl_list_cursor";; (* Undefined type (but should not be needed) *)
  hook_malloc ~sym:"malloc_jsw_avlnode" "jsw_avlnode";;
  hook_malloc ~sym:"malloc_jsw_avltrav" "jsw_avltrav";;
  hook_malloc ~sym:"malloc_jsw_avltree" "jsw_avltree";;
  hook_malloc ~sym:"malloc_rbnode" "rbnode";;
  hook_malloc ~sym:"malloc_rbtree" "rbtree";;
  hook_malloc ~sym:"malloc_snode" "snode";;
  hook_malloc ~sym:"malloc_stree" "stree";;
  hook_malloc ~sym:"malloc_edge" "edge";;
  hook_malloc ~sym:"malloc_node" "node";;
  *)

  let get_arguments_from_stack (state : State.t) typs =
    let ctx = state.ctx in
    let esp = State.get ~size:32 state "esp" in
    
    let folder (lst,mem,offset) typ =
      let size = 8 * Ctypes.sizeof typ in 
      let arg, mem =
        Domain.(Memory_Forward.load ~size ctx mem
          Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false ctx esp 
            (biconst ~size:32 (Z.of_int offset) ctx))
        )
      in (lst @ [(size,arg)], mem, offset + 4)
    in

    let args, mem, _ = List.fold_left folder ([], state.memory, 4) typs in
    args, {state with memory = mem}


  let rec analyze_summary funtyp (state : State.t) =
    if state.is_bottom then ret state
    else begin
      match funtyp.Ctypes.descr with
      | Function {ret = rtyp; args = typs} ->
        let args, state = get_arguments_from_stack state typs in
        let res, ret_val, mem = Domain.analyze_summary state.ctx funtyp args state.memory in
        let state = {state with memory = mem} in
        begin
          match ret_val with
          | None -> ret state
          | Some (sz, value) ->
            let state = State.set state "eax" value in
            ret state
        end

      | Existential (typ,_,_) -> analyze_summary typ state
        
      | _ -> assert false
    end

  let hook_interprocedural_analysis ~name funtyp =
    match Loader_utils.address_of_symbol_by_name ~name (Kernel_functions.get_img ()) with
    | None -> ()
    | Some address ->
      hook (Virtual_address.create address) ~msg:name (fun trace state ->
        Log.debug (fun p -> p "Analyzing function summary for %s" name);
        trace, analyze_summary funtyp state
      )
  ;;

  let init_functions_hooks = 
    fun () ->
      let entry_name = (Kernel_options.Entry_point.get ()) in
      let mapper name = (name, Ctypes.function_definition_of_name name) in

      let funlist = List.map mapper @@ Ctypes.get_function_names () in
      List.iter (fun (name, Ctypes.{funtyp;inline}) ->
        if entry_name <> name &&
          not inline then hook_interprocedural_analysis ~name funtyp
      ) funlist
    ;;

  init_functions_hooks () ;; 

  (match Loader_utils.address_of_symbol_by_name
           ~name:"__VERIFIER_nondet_int" (Kernel_functions.get_img ()) with
  | None -> ()
  | Some address ->
    hook (Virtual_address.create address) ~msg:"__VERIFIER_nondet_int stub" (fun trace state ->
        let value = bunknown ~size:32 state.ctx in
        let state = State.set state "eax" value in
        trace, ret state (* [(Virtual_address.create (sym_VERIFIER_nondet_int+5), state)] *)
      ))
  ;;

  (** new hook for flushing the cache *)

  (match Loader_utils.address_of_symbol_by_name
          ~name:"__VERIFIER_flush_cache" (Kernel_functions.get_img ()) with
  | None -> ()
  | Some address ->
    hook (Virtual_address.create address) ~msg:"__VERIFIER_flush_cache stub" (fun trace state ->
        Log.debug (fun p -> p "flushing cache");
          let state = {state with memory = Domain.flush_cache state.ctx state.memory} in
        trace, ret state (* [(Virtual_address.create (sym_VERIFIER_flush_cache), state)] *)
      ))  
  ;;

  (** new hook for assertion *)

  (match Loader_utils.address_of_symbol_by_name
        ~name:"__VERIFIER_assert" (Kernel_functions.get_img ()) with
    | None -> ()
    | Some address ->
    hook (Virtual_address.create address) ~msg:"__VERIFIER_assert stub" (fun trace state ->
      let esp = State.get ~size:32 state "esp" in
      let ctx = state.ctx in
      let value, mem = Domain.(Memory_Forward.load ~size:32 ctx state.memory
        Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false ctx esp (biconst ~size:32 (Z.of_int 4) ctx))) in
      let zero = Domain.Binary_Forward.biconst ~size:32 Z.zero ctx in
      let notc = Domain.Binary_Forward.beq ~size:32 ctx value zero in
      let cond = Domain.Boolean_Forward.not ctx notc in
      let c = Domain.query_boolean ctx cond in
      let open Lattices.Quadrivalent in
      trace, 
        begin match c with
          | False | Top -> Codex_log.warning "assertion may be false" ; ret @@ State.assume cond {state with memory = mem}
          | Bottom -> []
          | True -> ret state
        end
    ));;

  (match Loader_utils.address_of_symbol_by_name
        ~name:"__VERIFIER_memcmp" (Kernel_functions.get_img ()) with
    | None -> ()
    | Some address ->
    hook (Virtual_address.create address) ~msg:"__VERIFIER_memcmp stub" (fun trace state ->
      let esp = State.get ~size:32 state "esp" in
      let arg1, mem1 = Domain.(Memory_Forward.load ~size:32 state.ctx state.memory
        Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false state.ctx esp (biconst ~size:32 (Z.of_int 4) state.ctx))) in
      let arg2, mem2 = Domain.(Memory_Forward.load ~size:32 state.ctx mem1
        Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false state.ctx esp (biconst ~size:32 (Z.of_int 8) state.ctx))) in
      let arg3, mem3 = Domain.(Memory_Forward.load ~size:32 state.ctx state.memory
        Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false state.ctx esp (biconst ~size:32 (Z.of_int 12) state.ctx))) in

      (* let arg3_sng = Domain.Query.(binary_is_singleton ~size:32 @@ binary ~size:32 state.ctx arg3) in *)
      let c = Domain.Binary_Forward.beq ~size:32 state.ctx arg1 arg2 in
      
      let c = Domain.query_boolean state.ctx c in
      let open Lattices.Quadrivalent in
      trace, 
        begin match c with
          | False | Top -> Codex_log.alarm "the memory blocks may not be equal" ; ret state
          | Bottom -> []
          | True -> Codex_log.alarm "the memory blocks are equal"  ; ret state
        end
    ));;

  (match Loader_utils.address_of_symbol_by_name
        ~name:"__VERIFIER_equal" (Kernel_functions.get_img ()) with
    | None -> ()
    | Some address ->
    hook (Virtual_address.create address) ~msg:"__VERIFIER_equal stub" (fun trace state ->
      let esp = State.get ~size:32 state "esp" in
      let arg1, mem1 = Domain.(Memory_Forward.load ~size:32 state.ctx state.memory
        Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false state.ctx esp (biconst ~size:32 (Z.of_int 4) state.ctx))) in
      let arg2, mem2 = Domain.(Memory_Forward.load ~size:32 state.ctx mem1
        Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false state.ctx esp (biconst ~size:32 (Z.of_int 8) state.ctx))) in

      let c = Domain.Binary_Forward.beq ~size:32 state.ctx arg1 arg2 in
      
      let c = Domain.query_boolean state.ctx c in
      let open Lattices.Quadrivalent in
      trace, 
        begin match c with
          | False | Top -> Codex_log.alarm "the two arguments may not be equal" ; ret state
          | Bottom -> []
          | True -> Codex_log.alarm "the two arguments are equal"  ; ret state
        end
    ));;

  (match Loader_utils.address_of_symbol_by_name
        ~name:"diffu" (Kernel_functions.get_img ()) with
    | None -> ()
    | Some address ->
    hook (Virtual_address.create address) ~msg:"diffu stub" (fun trace state -> trace, ret state));;

  (* Fresh symbolic symbols from existential functions *)
  let fresh_int =
    let fresh_counter = ref (0 : int) in
    fun () -> incr fresh_counter ; !fresh_counter

  let fresh_symbol () = Format.sprintf "&%d" (fresh_int ()) ;;

  (match Loader_utils.address_of_symbol_by_name
        ~name:"malloc" (Kernel_functions.get_img ()) with
    | None -> ()
    | Some address ->
    let addr = Virtual_address.create address in
    hook addr ~msg:"malloc stub" (fun trace state ->
      let size = Codex_config.ptr_size () in

      let esp = State.get ~size:32 state "esp" in
      let ctx = state.ctx in
      let sz_b, mem = Domain.(Memory_Forward.load ~size:32 ctx state.memory
        Binary_Forward.(biadd ~size:32 ~nsw:false ~nuw:false ~nusw:false ctx esp (biconst ~size:32 (Z.of_int 4) ctx))) in

      begin match Domain.Query.(binary_is_singleton ~size @@ binary ~size state.ctx sz_b) with
      | Some byte_size ->
        let malloc_size = Z.to_int byte_size in
        let weak_typ = Ctypes.{descr = Weak (Ctypes.word ~byte_size:malloc_size); pred = Pred.True} in
        let ptr_typ = Ctypes.{descr = Ptr {pointed = weak_typ; index = Zero}; pred = Pred.neq (Const Z.zero)} in
        let ptr = Domain.binary_unknown_typed ~size ctx ptr_typ in
        Log.debug (fun p -> p "Allocating type %a of size %d with malloc" Ctypes.pp ptr_typ malloc_size);
        let init_val = Domain.Binary_Forward.buninit  ~size:malloc_size ctx in
        let mem = Domain.Memory_Forward.store ~size state.ctx mem ptr init_val in
        let state = {state with memory = mem} in
        let state = State.set state "eax" ptr in
        trace, ret state

      | None -> 
        let sz_symb = fresh_symbol () in
        Domain.add_global_symbol ~size ctx sz_symb sz_b ;
        let array_type = Ctypes.{descr = Array (word ~byte_size:1, Some (Sym sz_symb)) ; pred = Pred.True} in
        let weak_typ = Ctypes.{descr = Weak array_type; pred = Pred.True} in
        let ptr_typ = Ctypes.{descr = Ptr {pointed = weak_typ; index=Zero}; pred = Ctypes.Pred.(neq (Const Z.zero))} in
        Log.debug (fun p -> p "Allocating array type %a of size %a with malloc" Ctypes.pp ptr_typ (Domain.binary_pretty ~size ctx) sz_b);
        let ptr_size = Codex_config.ptr_size () in
        let ptr = Domain.binary_unknown_typed ~size:ptr_size ctx ptr_typ in
        let state = State.set state "eax" ptr in
        trace, ret state
      end
    ));;


  (** List of caller saved registers, EXCLUDING eax*)
  let caller_saved_registers = ["ecx"; "edx"; "esi"; "edi"] (* ; "r8d"; "r9d"; "r10d"; "r11d"] *)

  (** Adds a new unknown of given type to eax,
      adds new unknown values to other caller saved register *)
  let add_return_unknown addr typ =
    hook addr ~msg:(Format.asprintf "return_unknown %a" Ctypes.pp typ) (fun trace state ->
        let ctx = state.State.ctx in
        (* Return value of known type *)
        let size = 8 * (Ctypes.sizeof typ) in
        let value = Domain.binary_unknown_typed ~size ctx typ in
        let state = State.set state "eax" value in
        (* Caller saved registers - new values of unknown type *)
        let value size = Domain.binary_unknown ~size  ctx in
        let folder state register = State.set state register (value 32) in
        let state = List.fold_left folder state caller_saved_registers in
        trace, ret state
      )
  ;;


  let kernel_exit_point = Virtual_address.create 0x0
  let exploration_only = ref false

  let add_stop addr =
    stop addr ~msg:"Stop hook";;

  let add_skip addr ~dest = skip_always addr ~dest ~msg:"Skip hook";;

  let add_entrycall ~name addr ftyp =
    Log.debug (fun p -> p"Adding entry call hook");
    entrycall ~msg:"Entry call hook" ~name addr ftyp;;


  let add_function_hook ~name addr ftyp =
    Log.debug (fun p -> p "Adding new function hook for %s" name);
    hook addr ~msg:name (fun trace state ->
      Log.debug (fun p -> p "Analyzing function summary for %s" name);
      trace, analyze_summary ftyp state
    )
      

  let add_return addr rtyp =
    return addr rtyp ~msg:"Return hook";;

end
