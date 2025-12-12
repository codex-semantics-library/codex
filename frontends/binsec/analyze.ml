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

module Log = Tracelog.Make(struct let category = "Analyze" end)
module LogInstruction = Tracelog.Make(struct let category = "Instruction" end)

let in_bits = Units.In_bits.of_int


open Codex
module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree

module Logger = Codex_logger

module Create () = struct

module Dba2CodexC = Dba2Codex.Create ()

let () =
  Log.trace (fun p -> p "Parsing types") @@ fun () ->
  let conf_file = Codex_options.TypeConfigurationFile.get() in
  if conf_file = "" then ()
  else
    Codex.Types.Parse_ctypes.parse_file ~infer_spec:false conf_file

let results_tbl = Hashtbl.create 10000;;

(*module Arch = Ks_arch.Make(RB)(Dba2Codex.Domain)*)

module Arch = X86_arch.Make (Dba2CodexC.Domain)

open Dba2CodexC

module Dba2CState = Dba2CodexC.Make(Arch.Registers)
open Dba2CState

module Record_cfg = Record_cfg.Make(State)

module Settings = Hooks.Make(Dba2CState.State)(Record_cfg)
open Settings
module Addr_tbl = Hooks.Addr_tbl

module Cfg = Cfg_analysis.Cfg

module Dhunk_regex = Dhunk_analysis.Dhunk_regex

let ret_special_address = (Z.of_string "0xfedcba98") ;;
let ret_addr = Virtual_address.of_bigint ret_special_address

let exploration_result = ref None

(** Utility functions. **)

let find_opt f =
  try Some (f ())
  with Not_found -> None

module Dhunk_regex_hash = struct
  type t = Dhunk_regex.t
  let equal = Dhunk_regex.equal
  let hash = Hashtbl.hash
end

module Dhunk_regex_tbl = Hashtbl.Make(Dhunk_regex_hash)

let rec do_regex ~locid dhunk state_table regex =
  let open Fixpoint.Regex in
  try Some (Dhunk_regex_tbl.find state_table regex)
  with Not_found ->
    match regex with
    | Empty | Epsilon -> assert false
    | Append (_,r, (src,dst)) ->
        let state = do_regex ~locid dhunk state_table r in
        begin match state with
        | None -> None
        | Some state ->
            (* Codex_log.feedback "Here regexp %a" (State.dump_state ctx) state; *)
            Logger.result "edge from %d to %d" src dst;
            let src_i = Dhunk.inst dhunk src |> Option.get in
            let locid = Syntax_tree.Location_identifier.(DbaInstruction,Inside{locid;idx=src}) in
            let next = instr state (src_i,locid) in
            if next = [] then
              Logger.result "No successor states.";
            next |> List.iter (function
              | (Dba2Codex.Jump_Inner next_id, next_state) ->
                  let r' = Dhunk_regex.append r (src, next_id) in
                  assert (not (Dhunk_regex_tbl.mem state_table r')) ;
                  Dhunk_regex_tbl.add state_table r' next_state
              | _ -> ()
              );
            (* if List.length next >= 1 then *)
            (*   next |> List.iter (function *)
            (*     | (Dba2Codex.Jump_Inner next_id, next_state) -> *)
            (*         Logger.result "state diff at entry of successor %d:@ %a" next_id *)
            (*           (fun fmt s -> State.dump_state_diff ctx fmt state s) next_state *)
            (*     | (Dba2Codex.Jump_Outer addr, next_state) -> *)
            (*         Logger.result "state diff at entry of successor %a:@ %a" *)
            (*           Virtual_address.pp addr *)
            (*           (fun fmt s -> State.dump_state_diff ctx fmt state s) next_state *)
            (*     | (Dba2Codex.Jump_Dynamic, next_state) -> *)
            (*         Logger.result "state diff at entry of dynamic successor:@ %a" *)
            (*           (fun fmt s -> State.dump_state_diff ctx fmt state s) next_state *)
            (*   ); *)
            begin try next
              |> List.find (function
                | (Dba2Codex.Jump_Inner id, _) -> id = dst
                | (Dba2Codex.Jump_Outer _, _) -> false
                | (Dba2Codex.Jump_Dynamic, _) -> false)
              |> (fun (_,state) -> Some state)
            with Not_found -> None
            end
        end
    | AppendStar (_,r, body) ->
        (* The code below only works for max. 32 iterations (including one
         * after the star). Since we are not sure this will work for any other
         * loop than in CLZ, we will crash anywhere else. *)
        assert (Virtual_address.to_int !Codex_logger.current_instr_addr = 0x120005a8);
        let init_state = do_regex ~locid dhunk state_table r in
        begin match init_state with
        | None -> None
        | Some init_state ->
          Logger.warning "Unrolling in-dhunk star with max. 32 iterations...";
          let max_iter = 31 in
          let rec loop entry_state i =
            if i = max_iter then entry_state
            else begin
              Logger.result "Unrolling iteration %i..." i;
              let inner_table = Dhunk_regex_tbl.create 17 in
              Dhunk_regex_tbl.add inner_table Dhunk_regex.epsilon entry_state;
              let exit_state = do_regex ~locid dhunk inner_table body in
              let f () =
                (* The back edge is no longer taken, meaning that the exit
                 * condition has been met. We take the last exit state as an {e
                 * approximation} of the state. *)
                Logger.result "Unrolling: back edge can no longer be taken. \
                  Returning entry state at this iteration.";
                entry_state
              in
              match exit_state with
              | None -> f ()
              | Some x when x.State.is_bottom ->
                  (* The back edge is no longer taken, meaning that the exit
                   * condition has been met. We take the last exit state as an {e
                   * approximation} of the state. *)
                  Logger.result "Unrolling: back edge can no longer be taken. \
                    Returning entry state at this iteration.";
                  entry_state
              | Some exit_state ->
                  loop exit_state (i + 1)
            end
          in
          let s = loop init_state 0 in
          Logger.result "Finished unrolling star.";
          Dhunk_regex_tbl.add state_table regex s;
          Some s
          (*
          Logger.result "Analyzing star...";
          let mu_ctx = Domain.mu_context_open ctx in
          let mu_ctx' = Domain.mu_context_upcast mu_ctx in
          let rec loop (entry_state : Dba2CState.State.t) i =
            Logger.result "In-dhunk mu iteration %d..." i;
            let inner_table = Dhunk_regex_tbl.create 17 in
            Dhunk_regex_tbl.add inner_table Dhunk_regex.epsilon entry_state;
            let exit_state =
              match do_regex mu_ctx' dhunk inner_table body with
              | None -> State.bottom ctx
              | Some x -> x in
            Logger.result "@[<v 2>fixpoint between:@,entry@ @[<hov 2>%a@]@,exit@ @[<hov 2>%a@]@]" (State.dump_state mu_ctx') entry_state (State.dump_state mu_ctx') exit_state;
            let Domain.Result(included,in_tuple,deserialize) =
              State.serialize ~widens:true mu_ctx' entry_state exit_state (true, Domain.empty_tuple) in
            Logger.result "After serialize: included = %b" included;
            let fp,out =
              Domain.typed_fixpoint_step mu_ctx (included,in_tuple) in
            Logger.result "After fixpoint_step: fp = %b" fp;
            if fp then begin
              let out_tuple = out ~close:true in
              let out_state,_ = deserialize out_tuple in
              Logger.result "fixpoint reached, result: %a" (State.dump_state ctx) out_state;
              Some out_state
            end
            else begin
              let out_tuple = out ~close:false in
              let out_state,_ = deserialize out_tuple in
              Logger.result "fixpoint not reached, result: %a" (State.dump_state mu_ctx') out_state;
              loop out_state (i+1)
            end
          in
          let state' = loop init_state 0 in
          Logger.result "Finished analyzing star.";
          begin match state' with
          | Some s -> Dhunk_regex_tbl.add state_table regex s;
          | None -> ()
          end;
          state'
          *)
        end
    | Join (_,r1,r2) ->
        Logger.result "In-dhunk join. Term 1...";
        let state1 = do_regex ~locid dhunk state_table r1 in
        Logger.result "[in-dhunk join] Term 2...";
        let state2 = do_regex ~locid dhunk state_table r2 in
        begin match state1,state2 with
        | None, other ->
            Logger.result "In-dhunk join with left branch (at least) never taken";
            other
        | other, None ->
            Logger.result "In-dhunk join with right branch never taken";
            other
        | Some state1, Some state2 ->
          Logger.result "[in-dhunk join] nondet...";
          let new_state = State.join state1 state2 in
          Dhunk_regex_tbl.add state_table regex new_state;
          Some new_state
        end

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
      begin match f x with
      | Some y -> y :: filter_map f xs
      | None -> filter_map f xs
      end

let transfer_dhunk ~locid dhunk state =
  let regexes = Dhunk_analysis.exit_regexes dhunk in
  let state_table = Dhunk_regex_tbl.create 17 in
  Dhunk_regex_tbl.add state_table Dhunk_regex.epsilon state;
  regexes
  |> List.map (fun (id,r,addr) ->
      if addr <> None && Virtual_address.equal (Option.get addr) (Virtual_address.create 0x120005ac) then
        Logger.result "dhunk %a" Dhunk_regex.pretty r;
      match do_regex ~locid dhunk state_table r with
      | None -> []
      | Some state ->
        let locid = Syntax_tree.Location_identifier.(DbaInstruction,Inside{locid;idx=id}) in
          Dhunk.inst dhunk id |> Option.get |> fun i -> instr state (i,locid)
    )
  |> List.map (filter_map
      (function Dba2Codex.Jump_Outer addr, state -> Some (addr,state) | _ -> None))
  |> List.concat

(* Cache to avoid the enormous cost of spawning a new process for the decoder
 * all the time. *)
let instr_cache = Addr_tbl.create 5000

let decode_instr addr =
  try Addr_tbl.find instr_cache addr
  with Not_found ->
    Logger.result "instruction cache miss";
    let instr,_ = Disasm_core.decode addr in
    Addr_tbl.add instr_cache addr instr;
    instr

module Addr_map : sig
  include (Map.S with type key = Virtual_address.t)
  val of_list : (key * 'a) list -> 'a t
end = struct
  include Virtual_address.Map

  let of_list l =
    let f acc (addr,v) = add addr v acc in
    List.fold_left f empty l
end

let check_return_type state rtyp =
  (* Checks the expected return type against the value of register eax *)
  let eax = State.get ~size:(32 |> in_bits) state "eax" in
  match rtyp with
  | None
  | Some TypedC.{descr=Void} -> ()
  | Some typ when not state.is_bottom ->
    Log.debug (fun p -> p "Checking that the return value in eax has the expected type.");
    let size = TypedC.sizeof typ |> Units.In_bytes.in_bits in
    Domain.check_type ~size state.ctx typ eax
    |> Type_check_tree.save
  | _ -> ()

let transfer_instruction_nostub address state =
  let instr = decode_instr address in
  let locid = Syntax_tree.Location_identifier.(Address, Int64 (Virtual_address.to_int64 address)) in
  let dhunk = instr.Instruction.dba_block in
  Log.debug (fun p -> p "exploration_only : %b" !exploration_only);
  Logger.result "address: %a@ instruction: %a@\nblock: %a"
    Virtual_address.pp address
    Mnemonic.pp instr.Instruction.mnemonic
    Dhunk.pp dhunk;
  LogInstruction.trace (fun p -> p "%a: %a"
                           Virtual_address.pp address Mnemonic.pp instr.mnemonic )
      ~loc:(Codex_options.Location.Instruction {locid;address=Virtual_address.to_int64 address})
      ~bintrace:(Syntax_tree.Location_identifier.(Address, Int64(Virtual_address.to_int64 address)))
      (fun _ ->
  transfer_dhunk ~locid dhunk state |> Addr_map.of_list)

let transfer_instruction address trace state =
  let warn_skip () =
    Logger.alarm Operator.Alarm.Manual_stub in
  match Settings.find_hook address with
  | (SkipTo (_skip_type, dest), msg) ->
      warn_skip ();
      Logger.result "Skipping to %a. Reason: %s" Virtual_address.pp dest msg;
      trace, Addr_map.singleton dest state
  | (EndPath, msg) ->
      warn_skip ();
      Logger.result "@[<hov 2>Ending path. Reason: %s@]" msg;
      trace, Addr_map.empty
  | (Hook f, msg) ->
      Logger.alarm Operator.Alarm.Manual_stub;
      warn_skip ();
      Logger.result "@[<hov 2>Hook function. Reason: %s.@]" msg;
      let trace, successors = f trace state in
      Logger.result "@[<hov 2>Hook function executed. Successors: %a@]"
        (Format.pp_print_list Virtual_address.pp) (List.map fst successors);
      trace, Addr_map.of_list successors
  | (Unroll nb_iter, _) ->
      warn_skip ();
      Logger.result "Ignoring unroll(%i) indication during exploration." nb_iter;
      trace, transfer_instruction_nostub address state
  | (ChangeState f, msg) ->
      Logger.alarm Operator.Alarm.Manual_stub;
      Logger.result "Changing state before instruction. Reason: @[<hov 2>%s@]" msg;
      trace, transfer_instruction_nostub address @@ f state

  | (Return typ, msg) ->
      warn_skip ();
      Logger.result "@[<hov 2>Reaching return instruction. Reason: %s@]" msg;
      check_return_type state typ ;
      trace, Addr_map.empty
  | (EntryCall (name,ftyp), msg) ->
      Logger.alarm Operator.Alarm.Manual_stub;
      Logger.result "Replacing by a hook for later function calls. Reason: @[<hov 2>%s@]" msg;
      Settings.add_function_hook ~name address ftyp ;
      trace, transfer_instruction_nostub address state


  | exception Not_found ->
      trace, transfer_instruction_nostub address state

let transfer_from_to_generic ~transfer_instruction ~self ~stop_pred start_address trace state =
  let trace, successors = transfer_instruction start_address trace state in
  (* Invariant: the keys of the mapping returned by [transfer_from_to] are in
   * [end_addresses], and the values are the merge-over-all-paths final state
   * for all paths from the start address. *)
  if Addr_map.cardinal successors = 1 then
    let addr, state = Addr_map.choose successors in
    (self ~stop_pred addr trace state : Record_cfg.t * State.t Addr_map.t)
  else begin
    Logger.result "forking paths";
    Record_cfg.start_fork trace;
    let trace, final_states = Addr_map.fold (fun addr state (trace, final) ->
      Logger.result "New fork starting with %a" Virtual_address.pp addr;
      Record_cfg.next_fork trace;
      let trace, other_finals = self ~stop_pred addr trace state in
      Logger.result "End of fork starting with %a" Virtual_address.pp addr;
      trace, Addr_map.union (fun _ s1 s2 -> Some (State.join s1 s2))
        final other_finals
    ) successors (trace, Addr_map.empty)
    in
    Record_cfg.end_fork trace;
    trace, final_states
  end

let rec transfer_from_to ~stop_pred start_address trace state =
  let _, ctx_change = Record_cfg.(call_stack_after_jump trace (current_position trace) start_address) in
  if stop_pred start_address ctx_change
  then trace, Addr_map.singleton start_address state
  else begin
    Record_cfg.next trace ~exploration_only:true ~record_cfg:false start_address;
    transfer_from_to_generic ~transfer_instruction ~self:transfer_from_to
      ~stop_pred start_address trace state
  end

(* The first time, we avoid checking the hook, so that we do not loop
 * indefinitely. We also do not call [Record_cfg.next] nor check the stop
 * predicate, we assume this has already been done for this address (since this
 * function is intended to be used as a hook). *)
let transfer_from_to start_address ~stop_pred trace state =
  let transf addr trace state = trace, transfer_instruction_nostub addr state in
  transfer_from_to_generic ~transfer_instruction:transf
    ~self:transfer_from_to ~stop_pred start_address trace state

(** Like [analyze_address] but does not call [next] on the first, and thus will
   not stop if [address] was already visited. *)
let rec analyze_address_nocheck state trace address =
  let warn_skip () =
    Logger.error "Manual skip at %a!" Virtual_address.pp address in
  let successors =
    match Settings.find_hook address with
      | (SkipTo (skip_type, dest), msg) ->
          if !exploration_only && skip_type = NotWhenInterpreting then begin
            (* Ignore skips when in "concrete interpreter" *)
            Logger.result "ignoring skip in concrete execution mode.";
            None
          end else begin
            Logger.alarm Operator.Alarm.Manual_stub;
            warn_skip ();
            Logger.result "Skipping to %a. Reason: %s" Virtual_address.pp dest msg;
            Some [(dest, state)]
          end
      | (EndPath, msg) ->
          warn_skip ();
          Logger.result "@[<hov 2>Ending path. Reason: %s@]" msg;
          Some []
      | (Hook f, msg) ->
          Logger.alarm Operator.Alarm.Manual_stub;
          warn_skip ();
          Logger.result "@[<hov 2>Hook function. Reason: %s.@]" msg;
          let _, successors = f trace state in
          Logger.result "@[<hov 2>Hook function executed. Successors: %a@]"
            (Format.pp_print_list Virtual_address.pp) (List.map fst successors);
          Some successors
      | (Unroll nb_iter, _) ->
          warn_skip ();
          Logger.result "Ignoring unroll(%i) indication during exploration." nb_iter;
          None
      | (ChangeState _, _) ->
          Logger.alarm Operator.Alarm.Manual_stub;
          (* Handled lower in this function *)
          None
      | (Return typ, msg) ->
          warn_skip ();
          check_return_type state typ ;
          Logger.result "@[<hov 2>Reaching return instruction. Reason: %s@]" msg;
          Some []

      | (EntryCall (name,ftyp), msg) ->
          Logger.alarm Operator.Alarm.Manual_stub;
          Settings.add_function_hook ~name address ftyp ;
          None

      | exception Not_found -> None
  in
  let f (address, new_state) =
    Logger.result "Changes after block: @[<v>%a@]" (fun fmt ->
      State.dump_state_diff fmt state new_state address) results_tbl;
    (analyze_address[@tailcall]) new_state trace address
  in
  (* Optimisation to use tail calls when possible. As forks as rare,
     this reduces the amount of stack space used. *)
  match successors with
  | Some [] ->
      Logger.result "Warning: ending path";
      if !exploration_only && Virtual_address.equal address kernel_exit_point then begin
        Logger.result "Setting exploration_result at address %a" Virtual_address.pp address;
        exploration_result := Some state
      end
  | Some [x] -> (f[@tailcall]) x
  (* Uncomment this to keep only the first trace. *)
  (* | l -> f (List.hd l) *)
  | Some succs -> (Logger.result "Warning: forking paths";
          Record_cfg.start_fork trace;
          List.iteri (fun i x ->
              Logger.result "New path %d" i;
              if i != 0 then Record_cfg.next_fork trace;
              f x;
              Logger.result "Done path %d" i;
            ) succs;
          Record_cfg.end_fork trace;
         )
  | None -> begin
      let new_state =
        match Settings.find_hook address with
        | (ChangeState f, msg) ->
            warn_skip ();
            Logger.result "@[<hov 2>Manual state modification. Reason: %s@]" msg;
            let new_state = f state in
            Logger.result "@[<hov 2>Modification function executed. Executing \
              instruction normally now.@]";
            new_state

        | _ -> state
        | exception Not_found -> state
      in (analyze_address' [@tailcall]) new_state trace address
  end

and analyze_address state trace address =
  let visited = begin try
    Record_cfg.next trace ~exploration_only:!exploration_only ~record_cfg:true address; false
  with
  | Record_cfg.Visited_vertex ->
    (* stop evaluating this trace *)
    Logger.result "Visited vertex, ending path.";
    Logger.result "number of forks on the stack: %d"
      (Record_cfg.nb_forks trace);
    true
  end in
  if not visited then (analyze_address_nocheck[@tailcall]) state trace address

and analyze_address' state trace address =
  let prevstate = state in
  let res = transfer_instruction_nostub address state |> Addr_map.bindings in
  let f (address, state) =
    Codex_log.feedback "Changes after dhunk: @[<v>%a@]" (fun fmt -> State.dump_state_diff fmt prevstate state address) results_tbl;
    (* Codex_log.feedback "Changes after dhunk: @[<v>%a@]" (fun fmt -> State.dump_state ctx fmt) state;     *)
    analyze_address state trace address
  in
  (* Optimisation to use tail calls when possible. As forks as rare,
     this reduces the amount of stack space used. *)
  match res with
  | [] -> (Logger.result "Warning: ending path")
  | [x] -> (f [@tailcall]) x
  (* Uncomment this to keep only the first trace. *)
  (* | l -> f (List.hd l) *)
  | _ -> (Logger.result "Warning: forking paths";
          Record_cfg.start_fork trace;
          List.iteri (fun i x ->
              Logger.result "New path %d" i;
              if i != 0 then Record_cfg.next_fork trace;
              f x;
              Logger.result "Done path %d" i;
            ) res;
          Record_cfg.end_fork trace;
         )

exception Recursive_call

let rec destination rx =
  let open Fixpoint.Regex in
  match rx with
  | Empty -> raise @@ Invalid_argument "destination"
  | Epsilon -> raise @@ Invalid_argument "destination" (* raise Recursive_call *)
  | Append (_,_,(_,dst)) -> dst
  | Join (_,r1,_) -> destination r1
  | AppendStar (_,_,body) -> destination body

module Regex_tbl_0 = Hashtbl.Make(Cfg_analysis.CfgRegex)
module Regex_tbl = struct
  include Regex_tbl_0
  let debug_log = ref false
  let find ctx tbl key =
    if !debug_log then
      Logger.result "Regex_tbl.find @[<hov 2>%a@]" Cfg_analysis.CfgRegex.pretty key;
    let res =
      try find tbl key
      with Not_found ->
        Logger.warning "Regex_tbl.find: not found, returning Bottom";
        State.bottom ctx
    in
    if !debug_log then
      Logger.result "Regex_tbl.find returns @[<hov 2>%a@]%!" State.dump_state (Obj.magic res);
    res

  let latest_state_table = Addr_tbl.create 1000

  let add tbl key x =
    if !debug_log then
      Logger.result "Regex_tbl.add @[<hov 2>%a@,%a@]%!" (Cfg_analysis.CfgRegex.pretty) key State.dump_state (Obj.magic x);
    begin try Addr_tbl.replace latest_state_table (fst @@ destination key) x
    with Invalid_argument _ (* | Recursive_call *) -> () end;
    add tbl key x

  let latest_state address =
    Addr_tbl.find latest_state_table address
end

let handle_successors successors state_table state trace r' src =
  ignore (successors |> List.fold_left (fun i (dst, state') ->
      Logger.result "@[<v>changes at entry of successor %d (%a)@,%a@]"
        i Virtual_address.pp dst (fun fmt -> State.dump_state_diff fmt state state' dst) results_tbl;
      (* Memoize the state at destination to save future computations. *)
      let dst_stack,_ = Record_cfg.call_stack_after_jump trace src dst in
      let new_r = Cfg_analysis.CfgRegex.append r' (src,(dst,dst_stack)) in
      if Virtual_address.to_int !Codex_logger.current_instr_addr = 0x12000588 then
        Regex_tbl.debug_log := true;
      Logger.result "debug_log = %b" !Regex_tbl.debug_log;
      Regex_tbl.add state_table new_r state';
      if Virtual_address.to_int !Codex_logger.current_instr_addr = 0x12000588 then
        Regex_tbl.debug_log := false;
      (* If this jump adds a new edge to the graph, then we explore it but
       * the exploration does not affect the returned state. *)
      if not (Cfg.mem_edge (Record_cfg.instruction_graph trace) src (dst,dst_stack))
      then begin
        Logger.result "unvisited edge to %a !!" Virtual_address.pp dst;
        Record_cfg.set_position trace ~keep_forks:false src;
        analyze_address state' trace dst;
        i+1
      end else
        i+1
      )
    0)

(** Analyze a set of paths in the CFG (described by a regex) to possibly
    discover new edges. When that happens, the new path set is explored
    immediately, enriching the instruction graph by a depth-first search
    without merge ([analyze_address]).  If that happens, it means that the
    fixpoint was not reached, and [analyze_regex] returns [false].  Otherwise,
    if no new instruction is discovered, a fixpoint was reached and
    [analyze_regex] returns [true].
    {e Please note:} The instruction at the end of the path is not analyzed by
    this function.
    @param state_table A table associating a path expression of an instruction
      to the {e entry state} of that expression. When analyzing a regex, all
      intermediary regexes (i.e. expressions of subpaths) are updated in this
      table.
*)
let rec analyze_regex state_table ctx trace r =
  let open Fixpoint.Regex in
  let open Cfg_analysis.CfgRegex in
  (* We propagate two things along the regex: a boolean that tells whether the
   * instruction graph has changed, and the state. Like with dhunk-level
   * regexes, the state is memoized in a hash table. *)
  if not (Regex_tbl.mem state_table r) then match r with
  | Empty ->
      (* We cannot return a normal result, because if the input is, e.g.,
         [Append (Empty, r)] we don't want [r] to be analysed. *)
      raise @@ Invalid_argument "analyze_regex: empty regex"
  | Epsilon ->
      (* Epsilon should always be in the state table before calling this
       * function, so this is an error *)
      raise @@ Invalid_argument "analyze_regex: epsilon not found in state table"
  | Append (_,r',(src,_)) ->
      analyze_regex state_table ctx trace r';
      Codex_logger.current_instr_addr := fst src;
      Logger.result "analyze_regex, case Append, src = %a" Cfg_analysis.V.pretty src;
      let state = Regex_tbl.find ctx state_table r' in
      (*
      if Virtual_address.to_int (fst src) = 0x106e70 || Virtual_address.to_int (fst src) == 0x106df0 then begin
        let size = 32 in
        let Some waiting_heap_s = Loader_utils.address_of_symbol_by_name ~name:"waiting_heap" (Kernel_functions.get_img ()) in
        let heap_addr = Domain.Binary_Forward.biconst ~size (Z.of_int waiting_heap_s) ctx in
        let heap_size = Domain.Memory_Forward.load ~size ctx state.State.memory heap_addr in
        Logger.result "heap size: %a" (Domain.binary_pretty ~size ctx) heap_size;
      end
      else if Virtual_address.to_int (fst src) = 0x106fd0 || Virtual_address.to_int (fst src) == 0x107050 then begin
        let size = 32 in
        let Some ready_heap_s = Loader_utils.address_of_symbol_by_name ~name:"ready_heap" (Kernel_functions.get_img ()) in
        let heap_addr = Domain.Binary_Forward.biconst ~size (Z.of_int ready_heap_s) ctx in
        let heap_size = Domain.Memory_Forward.load ~size ctx state.State.memory heap_addr in
        Logger.result "heap size: %a" (Domain.binary_pretty ~size ctx) heap_size;
      end;
      *)
      let warn_skip () =
        Logger.error "Manual skip during r-analysis at %a!"
          Virtual_address.pp (fst src)
      in
      let exception Does_not_apply in
      let instr = decode_instr (fst src) in
      let locid = Syntax_tree.Location_identifier.(Address,Int64 (Virtual_address.to_int64 (fst src))) in
      LogInstruction.trace (fun p -> p "%a: %a"
                               Virtual_address.pp (fst src) Mnemonic.pp instr.mnemonic )
        ~loc:(Codex_options.Location.Instruction {locid;address=Virtual_address.to_int64 (fst src)})
      ~bintrace:(Syntax_tree.Location_identifier.(Address, Int64(Virtual_address.to_int64 (fst src))))
      (* ~bintrace:(Binarytrace.Instruction {instr_str=Format.asprintf "%a" Mnemonic.pp instr.mnemonic; address=Virtual_address.to_int64 (fst src)}) *)
        (fun _ ->
      begin try match Settings.find_hook (fst src) with
      | (SkipTo (_,new_dst), msg) ->
          Logger.alarm Operator.Alarm.Manual_stub;
          warn_skip ();
          Logger.result "Skipping to %a. Reason: %s"
            Virtual_address.pp new_dst msg;
          (* Do not interpret the skipped instruction. Instead, set the entry
           * state of the skip destination to the entry state of the skipped
           * instruction. *)
          Regex_tbl.add state_table r state
      | (Hook f, msg) ->
          Logger.alarm Operator.Alarm.Manual_stub;
          warn_skip ();
          (* Execute the hook function and record all the successors in the
           * state table. We assume that the hook function is sound, and
           * therefore that all edges going from {src} in the current CFG will
           * appear in its result. If this assumption is false, a {Not_found}
           * exception is bound to be raised during some recursive call of
           * {analyze_regex}. *)
          Logger.result "@[<hov 2>Hook function. Reason: %s.@]" msg;
          Record_cfg.set_position trace ~keep_forks:false src;
          let _, successors = f trace state in
          Logger.result "Hook function executed.";
          handle_successors successors state_table state trace r' src
      | (EndPath, _) ->
          (* Don't interpret this instruction and don't add it to the regex ->
            * state mapping. *)
          ()
      | (Unroll _, _) ->
          Logger.alarm Operator.Alarm.Manual_stub;
          (* Do nothing special: unrolling is managed by the {AppendStar}
           * case. *)
          raise Does_not_apply
      | (ChangeState _, _) ->
          (* This is handled elsewhere *)
          raise Does_not_apply
      | (Return _, _) ->
          (* Don't interpret this instruction and don't add it to the regex ->
            * state mapping. *)
          ()
      | (EntryCall _, _) ->
          (* Don't interpret this instruction and don't add it to the regex ->
            * state mapping. *)
            ()

      | exception Not_found -> raise Does_not_apply
      with Does_not_apply ->
        let instr = decode_instr (fst src) in
        let dhunk = instr.Instruction.dba_block in
        (* Codex_log.debug "exploration_only : %b" !exploration_only ; *)
        Logger.result "@[<v>address: %a@,instruction: %a@,dhunk: @[<hov 2>%a@]@]"
          Virtual_address.pp (fst src) Mnemonic.pp instr.Instruction.mnemonic
          Dhunk.pp dhunk;
        let new_state = match Settings.find_hook (fst src) with
          | (ChangeState f, msg) ->
              Logger.alarm Operator.Alarm.Manual_stub;
              Logger.result "@[<hov 2>Manual state modification. Reason: %s@]" msg;
              let new_state = f state in
              Logger.result "@[<hov 2>Modification function executed. Executing \
                instruction normally now.@]";
              new_state
          | _ -> state
          | exception Not_found -> state
        in
        let exits =
          transfer_dhunk ~locid dhunk new_state in
        handle_successors exits state_table state trace r' src
      end)
  | AppendStar (_,r',x) ->
      analyze_regex state_table ctx trace r';
      let init_state = Regex_tbl.find ctx state_table r' in
      if init_state.State.is_bottom then Regex_tbl.add state_table r init_state
      else begin
        (* try *)
        let head = destination r' in
        begin match find_opt (fun () -> Settings.find_hook (fst head)) with
        | Some (Unroll max_iter, msg) ->
          Logger.warning "Unrolling star... (reason: %s)" msg;
          let rec loop entry_state i =
            if i = max_iter then entry_state
            else begin
              Logger.result "Unrolling iteration %i..." i;
              let inner_table = Regex_tbl.create 100 in
              Regex_tbl.add inner_table epsilon entry_state;
              analyze_regex inner_table entry_state.ctx trace x;
              let exit_state = Regex_tbl.find entry_state.ctx inner_table x in
              if exit_state.State.is_bottom then begin
                (* The back edge is no longer taken, meaning that the exit
                 * condition has been met. We take the last exit state as an {e
                 * approximation} of the state. *)
                Logger.result "Unrolling %a: back edge can no longer be taken. \
                  Returning entry state at this iteration." Cfg.V.pretty head;
                entry_state
              end else
                loop exit_state (i + 1)
            end
          in
          let state' = loop init_state 0 in
          Logger.result "Finished unrolling star.";
          Regex_tbl.add state_table r state'
        | _ when !exploration_only ->
            Logger.warning "Unrolling star... (concrete exec.)";
            let rec loop entry_state i =
              Logger.result "Unrolling iteration %i..." i;
              let inner_table = Regex_tbl.create 100 in
              Regex_tbl.add inner_table epsilon entry_state;
              analyze_regex inner_table entry_state.ctx trace x;
              let exit_state = Regex_tbl.find entry_state.ctx inner_table x in
              if exit_state.State.is_bottom then begin
                (* The back edge is no longer taken, meaning that the exit
                 * condition has been met. We take the last exit state as an {e
                 * approximation} of the state. *)
                Logger.result "Unrolling %a: back edge can no longer be taken. \
                  Returning entry state at this iteration." Cfg.V.pretty head;
                entry_state
              end else
                loop exit_state (i + 1)
            in
            let state' = loop init_state 0 in
            Logger.result "Finished unrolling star.";
            Regex_tbl.add state_table r state'
        | _ ->
          Logger.result "Analyzing star...";
          let mu_ctx' = Domain.mu_context_open init_state.ctx in
          (* Way to go to use mu:
             1. keep the initial state
             2. in a loop, do:
               1. keep the state at loop entry
               2. analyze the regex under star
               3. call [new_mu_context_fixpoint_step3] with the initial state, the
                  state before analysis and the state after analysis (actual, arg,
                  body)
               4. if fixpoint was reached, call [close], else call [restart].
           *)
          let rec loop (entry_state : Dba2CState.State.t) i =
            Logger.result "Mu iteration %d ..." i;
            let inner_table = Regex_tbl.create 100 in
            Regex_tbl.add inner_table epsilon entry_state;
            analyze_regex inner_table (Domain.Context.copy entry_state.ctx) trace x;
            let exit_state = Regex_tbl.find entry_state.ctx inner_table x in
            Logger.result "@[<v 2>fixpoint between:@,entry@ @[<hov 2>%a@]@,exit@ @[<hov 2>%a@]@]" State.dump_state entry_state State.dump_state exit_state;
            let Domain.Context.Result(included,in_tuple,deserialize) =
              State.serialize ~widens:true entry_state exit_state (true, Domain.Context.empty_tuple ()) in
            Logger.result "After serialize: included = %b%!" included;
            let fp,out =
              (*
              if !exploration_only then
                included, fun ~close:_ -> Domain.typed_nondet2 ctx in_tuple
              else
              *)
              Domain.typed_fixpoint_step ~iteration:i
                ~init:init_state.ctx
                ~arg:entry_state.ctx
                ~body:exit_state.ctx
                (included,in_tuple)
            in

            Logger.result "After fixpoint: fp = %b%!" fp;
            if fp then begin
              let out_tuple,outctx = out ~close:true in
              let out_state,_ = deserialize outctx out_tuple in
              let out_state = {out_state with ctx = outctx} in
              Logger.result "fixpoint reached, result: %a" State.dump_state out_state;
              out_state
            end
            else begin
              let out_tuple,outctx = out ~close:false in
              let out_state,_ = deserialize outctx out_tuple in
              let out_state = {out_state with ctx = outctx} in
              Logger.result "fixpoint not reached, result: %a%!" State.dump_state out_state;
              loop out_state (i+1)
            end
          in
          let state' = loop {init_state with ctx = mu_ctx'} 0 in
          Logger.result "Finished analyzing star.";
          Regex_tbl.add state_table r state'
        end
        (* with Recursive_call ->
          Codex_log.debug "Reaching recursive call" ;
          Regex_tbl.add state_table r init_state *)
      end
  | Join (_,r1,r2) ->
      Logger.result "JOIN";
      analyze_regex state_table ctx trace r1;
      let state1 = Regex_tbl.find ctx state_table r1 in
      analyze_regex state_table ctx trace r2;
      let state2 = Regex_tbl.find ctx state_table r2 in
      let new_state = State.join state1 state2 in
      Regex_tbl.add state_table r new_state

(*********************************)
(* [run] and auxiliary functions *)

let find_end_nodes cfg entry =
  let rec aux visited node =
    let succ = Cfg.succ cfg node in
    if List.length succ = 0
    then [node]
    else List.concat @@ List.map
      (fun n ->
        if List.mem n visited then []
        else aux (node :: visited) n) succ
  in
  aux [] entry

let catch_exc operation_name default f =
  try f ()
  with e ->
    Logger.error "%s%!" @@ Printexc.get_backtrace ();
    Logger.error "Raised during %s: %s" operation_name (Printexc.to_string e);
    default ();

(*
let catch_exc _ _ f = f ()
*)

module Wto_cfg = Cfg_analysis.Wto
module Reduce_cfg = Cfg_analysis.Reduce

module G' = struct
  include Cfg
  let vertex_name v =
    Format.asprintf "%a" Cfg.V.pretty v
  let graph_attributes _ = []
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let vertex_attributes _ = []
  let default_vertex_attributes _ = []
  let get_subgraph _ = None
end

module OutputCfg = Graph.Graphviz.Dot(G')

let setup_entrypoint img start init_state =
  (* start_from entry_point *)
  let start = Virtual_address.create start in
  let trace = Record_cfg.init img start in
  let state_table = Regex_tbl.create 10000 in
  Regex_tbl.add state_table Cfg_analysis.CfgRegex.epsilon init_state;
  (start, trace, state_table)

let analyze_path_expression state_table init_state trace instr_graph node regex =
  let open State in
  analyze_regex state_table init_state.ctx trace regex;
  (* If this instruction has no successors, then it was not analyzed by
   * [analyze_regex]. Let's analyze it and its successors in a
   * depth-first search. *)
  if Cfg.out_degree instr_graph node = 0
  then
    begin
      Logger.result "Analyze end node %a." Cfg.V.pretty node;
      Record_cfg.set_position trace ~keep_forks:false node;
      (* The state we just computed *)
      let state = Regex_tbl.find init_state.ctx state_table regex in
      analyze_address_nocheck state trace (fst node)
    end

let compute_fixpoint graph_filename html_filename results img init_state start trace state_table =
  let rec loop i trace =
    Logger.result "##### Iteration %i #####" i;
    let entry = start, [] in
    let instr_graph = Record_cfg.instruction_graph trace in
    let wto = Wto_cfg.partition ~pref:(fun _ _ -> 0) ~init:entry ~succs:(Cfg.succ instr_graph) in
    Record_cfg.wto := wto;
    Logger.result "%a\n" Wto_cfg.pretty_partition wto;
    let path_expressions = Reduce_cfg.compute_exprs instr_graph wto in
    Logger.result "%d expressions computed!\n" (Hashtbl.length path_expressions);
    (* Regex analysis *)
    let trace = Record_cfg.reuse trace in
    let analyze_path_expression_closure = analyze_path_expression state_table init_state trace instr_graph in
    let close_record_cfg = (fun () -> Record_cfg.close trace ~ret_addr start ~graph_filename ~html_filename results img) in
    let logged_close_record_cfg = (fun msg -> Logger.result msg; ignore @@ close_record_cfg ()) in 
    let error_callback = (fun () ->
      logged_close_record_cfg "exception raised, closing and dumping state..."; true) in
    let wrapped_path_expressions_analysis = (fun () ->
        Hashtbl.iter analyze_path_expression_closure path_expressions;
        false) in
    let except_thrown = catch_exc (Format.sprintf "r-analysis %i" i) error_callback wrapped_path_expressions_analysis
    in
    if !exploration_only then (logged_close_record_cfg "exploration only: first iteration done"; except_thrown)
    else if except_thrown then (Logger.result "Fixpoint not reached due to an exception."; true)
    else if Record_cfg.graph_changed trace then
      begin
        logged_close_record_cfg "Fixpoint not reached.";
        Record_cfg.set_graph_changed trace false;
        loop (i+1) trace
      end
    else
      (Logger.result "Fixpoint reached at iteration %i." i; false)
  in
  loop 0 trace

let report_graph_informations graph =
  (* Count nodes of the graph *)
  Logger.result "Nodes in the graph (with call stack): %d" (Cfg.nb_vertex graph)

let report_instruction_informations graph =
  let instr_tbl = Addr_tbl.create 10000 in
  Cfg.iter_vertex (fun (addr,_) -> Addr_tbl.replace instr_tbl addr ()) graph;
  Logger.result "Number of instructions (no call stack): %d" (Addr_tbl.length instr_tbl)

let report_results trace =
  let graph = Record_cfg.instruction_graph trace in
  report_graph_informations graph;
  report_instruction_informations graph;
  Logger.result "End of analyze log"

let return_final_state trace expected_last_instr =
  let open Region in
  let open Virtual_address.Set in
  let ret = !written_data_addrs, !read_data_addrs in
  written_data_addrs := empty; read_data_addrs := empty;
  (* Return state at {expected_last_instr} *)
  match expected_last_instr with
  | Some instr ->
      let final_state = Regex_tbl.latest_state instr in
      ret, Some final_state, Record_cfg.visited_instructions trace
  | None -> ret, None, Record_cfg.visited_instructions trace

(* Do an analysis and returns the set of written data addresses, the set of
 * read data addresses, and the state at {expected_last_instr} (or one of them,
 * if it is present several times in the CFG). If {expected_last_instr} is not
 * in the CFG, {Invalid_argument} is thrown. *)
let analyze img start init_state graph_filename expected_last_instr (html_filename:string option) results =
  let start, trace, state_table = setup_entrypoint img start init_state in
  let except_thrown = compute_fixpoint graph_filename html_filename results img init_state start trace state_table in
  if not except_thrown then
    begin
      Logger.result "Over ################################################################";
      report_results trace;
      return_final_state trace expected_last_instr
    end
  else raise @@ Failure "cannot return final state: exception occurred"

let interprete_concrete img start init_state graph_filename html_filename results =
  let start = Virtual_address.create start in
  let trace = Record_cfg.init img start in
  Logger.result "##### Concrete interpretation #####";
  let except_thrown = catch_exc "concrete analysis"
    (fun () ->
      Logger.result "exception raised, closing and dumping state...";
      ignore @@ Record_cfg.close trace ~ret_addr start ~graph_filename ~html_filename results img;
      true)
    (fun () ->
      analyze_address init_state trace start;
      false)
  in
  if not except_thrown then begin
    Logger.result "Over ################################################################";
    (* Count nodes of the graph *)
    let graph = Record_cfg.instruction_graph trace in
    Logger.result "Nodes in the graph (with call stack): %d" (Cfg.nb_vertex graph);
    let instr_tbl = Addr_tbl.create 10000 in
    Cfg.iter_vertex (fun (addr,_) -> Addr_tbl.replace instr_tbl addr ()) graph;
    Logger.result "Number of instructions (no call stack): %d"
      (Addr_tbl.length instr_tbl);
    Logger.result "End of concrete interpretation";
    ignore @@ Record_cfg.close trace ~ret_addr start ~graph_filename ~html_filename results img;
    (* Return state at 0x12000730 *)
    let final_state = Option.get !exploration_result in
    let open Region in
    let ret = !written_data_addrs, !read_data_addrs in
    written_data_addrs := Virtual_address.Set.empty; read_data_addrs := Virtual_address.Set.empty;
    ret, Some final_state, Record_cfg.visited_instructions trace
  end
  else raise @@ Failure "cannot return final state: exception occurred"

let list_init n f =
  let rec aux acc i =
    if i = n then acc else aux (f i :: acc) (succ i)
  in
  List.rev @@ aux [] 0

let cpu_sp =
  list_init 4 (fun i -> ref @@ Virtual_address.create @@ 0x1200e000 + 1024 * i)


module ReadMem : Heap_typing.MEMORY with type t = Domain.Context.t * Dba2CState.State.t = struct
  type t = Domain.Context.t * Dba2CState.State.t
  exception Invalid_read of string
  let read size (ctx,state) addr =
    let b, _mem =
      Region.set_untyped_load true;
      Domain.Memory_Forward.load ctx ~size state.Dba2CState.State.memory @@
      Domain.Binary_Forward.biconst ~size:(32 |> in_bits) (Z.of_int @@ Virtual_address.to_int addr) ctx
    in
    Region.set_untyped_load false;
    match Domain.Query.binary ctx b ~size |> Domain.Query.Binary_Lattice.is_singleton ~size with
    | Some x -> Z.to_int x
    | None ->
        raise @@ Invalid_read (Format.asprintf "addr@ %a@ size@ %d" Virtual_address.pp addr (size:>int))
  let read_u8 = read (8 |> in_bits)
  let read_u32 = read (32 |> in_bits)
  let read_u64 = read (64 |> in_bits)
end

module Heap_typechecker = Heap_typing.Make(ReadMem)

let add_stack_arg offset type_ state =
  let s32 = 32 |> in_bits in
  let esp = State.get ~size:s32 state "esp" in
  let value = Domain.binary_unknown_typed ~size:s32 state.ctx type_ in
  { state with memory = Domain.(Memory_Forward.store ~size:s32 state.ctx
      state.memory Binary_Forward.(biadd ~size:s32 ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) state.ctx esp (biconst ~size:s32 (Z.of_int offset) state.ctx))
      value)
  }

let add_stack_arg_value offset value state =
  let s32 = 32 |> in_bits in
  let esp = State.get ~size:s32 state "esp" in
  { state with State.memory = Domain.(Memory_Forward.store ~size:s32 state.State.ctx
      state.State.memory Binary_Forward.(biadd ~size:s32 ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) state.ctx esp (biconst ~size:s32 (Z.of_int offset) state.ctx))
      value)
  }

let add_stack_return state =
  let s32 = 32 |> in_bits in
  let esp = State.get ~size:s32 state "esp" in
  let const = Domain.Binary_Forward.biconst ~size:s32 ret_special_address state.ctx in
  { state with memory = Domain.Memory_Forward.store ~size:s32 state.ctx state.memory esp const
  }

let populate_stack_with_args types state =
  (* Populate stack, leftmost argument at lowest offset, starting from offset 4
   * (offset 0 is for the return address) *)
  let _, state = List.fold_left (fun (offset, state) typ ->
    (offset + 4, add_stack_arg offset typ state)
  ) (4, state) types
  in
  state

let populate_globals_with_types spec state =
  let s32 = 32 |> in_bits in
  List.fold_left (fun state (addr,typ) ->
      let size = TypedC.sizeof typ |> Units.In_bytes.in_bits in
      let value = Domain.binary_unknown_typed ~size state.State.ctx typ in
      let addr = (Domain.Binary_Forward.biconst ~size:s32 addr state.State.ctx) in
      {state with State.memory = Domain.Memory_Forward.store ~size state.ctx state.State.memory addr value }
    ) state spec

let populate_globals_with_symbols spec ctx =
  Log.debug (fun p -> p "initiazing %d symbols" (List.length spec));
  List.iter (fun (symb,typ) ->
      let size = TypedC.sizeof typ |> Units.In_bytes.in_bits in
      let value = Domain.binary_unknown_typed ~size ctx typ in
      Domain.add_global_symbol ~size ctx symb value

    ) spec
;;

let populate_hook hook_directives =
  hook_directives |> List.iter (fun (addr,directive) ->
      match directive with
      | `stop -> Settings.add_stop addr
      | `return_unknown typ -> Settings.add_return_unknown addr typ
      | `nop ->
        (* Like a skip, but we compute the next address. *)
        let _,next = Disasm_core.decode addr in
        begin
          match next with
          | None -> assert false
          | Some next -> Settings.add_skip addr ~dest:next
        end
      | `skip_to(dest) -> Settings.add_skip addr ~dest

      | _ -> assert false
    ) ;
    Settings.add_stop @@ Virtual_address.of_bigint ret_special_address
    ;;

let set_mmio mmio_ranges state =
  List.fold_left (fun state (addr,size) ->
      let size = size |> in_bits in
      let value = Domain.binary_unknown ~size state.State.ctx in
      { state with State.memory = Domain.Memory_Forward.store ~size state.ctx state.memory (Domain.Binary_Forward.biconst (Z.of_int addr) state.ctx ~size:size ) value }
 ) state mmio_ranges


let fresh_int =
  let fresh_counter = ref (0 : int) in
  fun () ->
    incr fresh_counter ;
    !fresh_counter

let fresh_symbol () = Format.sprintf "#f%d" (fresh_int ())

let rec initial_function_args_ret_types funtyp ctx =
  let open Types.TypedC in
  match (inlined funtyp).descr with
  | Function {ret; args} -> args, ret
  | Existential {bound_typ;bound_var;body}  ->
    let sz = sizeof bound_typ |> Units.In_bytes.in_bits in
    let res = Domain.binary_unknown_typed ~size:sz ctx bound_typ in
    let symb = fresh_symbol () in
    Domain.add_global_symbol ~size:sz ctx symb res ;
    let newft = substitute_symbol body bound_var symb in
    initial_function_args_ret_types newft ctx

  | _ -> assert false
  ;;

end

let analyze_non_kernel() =  begin
  Binarytrace.init ();
  if Codex_options.Output_Html.is_set()
  then Tracelog.set_log_binary_trace true;
  (* Log.trace (fun p -> p "Analyze") @@ fun () ->  *) (* Root of the analysis. *)
  let tinit = Sys.time() in
  let () = Codex.Domains.Term_domain.set_pretty_terms (Codex_options.VariableDisplay.get ()) in
  let module Analyze = Create () in
  Hook.run_hook Hook.after_domain_build ();
  let open Analyze in
  let open Analyze.Dba2CodexC in
  let module State = Dba2CState.State in
  let t_begin = Benchmark.make 0L in
  let img = Kernel_functions.get_img () in
  let ctx = Domain.root_context () in
  let () =

    (* Odvtk_Trace.log "Real deal";                     *)
    let entry_name = (Kernel_options.Entry_point.get ()) in
    let entry = match Loader_utils.address_of_symbol_by_name ~name:entry_name img with 
    | None -> Log.fatal (fun p -> p "Could not load entry symbol with name %s. Please ensure the -entrypoint option is correctly specified" entry_name)
    | Some entry -> entry in

    let tprep = Sys.time() in
    let init_state = Log.trace (fun p -> p "Initializing entry state") @@ fun () -> begin
        let init_state = State.initial_concrete img ctx in
        (* let entry = unoption @@ Loader_utils.address_of_symbol_by_name ~name:(Kernel_options.Entry_point.get ()) img in *)
        (* let global_spec = Codex_options.GlobalSymbols.get () in
           let () = populate_globals_with_symbols global_spec ctx in *)
        (* let arg_spec = Codex_options.FnArgs.get () in *)

        let init_state = add_stack_return init_state in

        let init_state = add_stack_return init_state in

        let fundef = Option.get @@ TypedC.function_definition_of_name entry_name in
        let arg_spec, rtyp = initial_function_args_ret_types fundef.funtyp ctx in

        let init_state = populate_stack_with_args arg_spec init_state in
        let globals_spec = Codex_options.GlobalsTypes.get () in
        let init_state = populate_globals_with_types globals_spec init_state in
        let mmio_list = Codex_options.MMIOs.get () in
        let init_state = set_mmio mmio_list init_state in
        let hook_directives = Codex_options.Hooks.get() in

        (* let () = Settings.add_entrycall ~name:entry_name (Virtual_address.create entry) funtyp in *)
        let () = populate_hook hook_directives in
        let () = Settings.add_return (Virtual_address.of_bigint ret_special_address) (if fundef.inline then None else Some rtyp) in
        init_state
      end in

    let _ =
      Log.trace(fun p -> p "Analysing") (fun () ->
      analyze img entry init_state "cfg.dot" None (Codex_options.Output_Html.get_opt()) results_tbl) in


    let tfinal = Sys.time() in

    Format.printf "Preprocessing time : %fs\n" (tprep -. tinit);
    Format.printf "Analysis time : %fs\n" (tfinal -. tprep);
    ()
  in
  let t_end = Benchmark.make 0L in
  let myprint fmt = Format.(eprintf (fmt ^^ "\n")) in
  myprint "### Alarms ###";
  (* Odvtk_Trace.log "Alarms";                     *)
  let alarms = Logger.alarm_record () in
  myprint "%a" Logger.Alarm_record.(pretty ~unique:true) alarms;
  myprint "Analysis time: %s" Benchmark.(to_string (sub t_end t_begin));
  let total_alarms = Logger.Alarm_record.total_alarms ~ignore:["manual_stub"] ~unique:true
      ~ignore_phase:[] alarms
  in
  myprint "@[<v>Total alarms: %d@.%!@]" total_alarms;

  Binarytrace.close ();
  (match !Record_cfg.dump_html_fun with
    | Some f -> f ()
    | None -> ());
  if total_alarms <> 0 then exit 1
end
;;

let initialize_codex () =
  Codex_config.set_ptr_size (32 |> in_bits);
  let () =
    (* Try to infer data model heuristically. *)
    let data_model =
      match Binsec.Kernel_options.Machine.bits() with
      | `x32 -> `ILP32
      | `x64 -> begin
          let img = Kernel_functions.get_img () in
          match img with
            | Loader.PE _ -> `LLP64
            | Loader.ELF img -> `LP64
            | _ -> failwith "Cannot infer data model for this machin"
        end
      | _ -> failwith "Cannot analyze code with this bitwith yet"
    in Types.Parse_ctypes.init ~data_model in

  (* Tracelog.set_verbosity_level `Debug ; *)
  Codex_log.register (module Codex_logger.Codex_logger);
  if not (Codex_options.UseShape.get ()) then begin
    Framac_ival.Ival.set_small_cardinal @@ max 8 @@ Codex_options.NbTasks.get ();
    (* Codex_config.set_widen false; *)
  end;
  Codex_config.set_valid_absolute_addresses @@ (Z.one,Z.of_int (1 lsl 30));

  (* Map binsec debug level verbosity to tracelog. We have two levels: 0, 1 and 2.*)
  (match (Codex_options.Debug_level.get(),Codex_options.Loglevel.get()) with
   | x, _ when x >= 2 -> Tracelog.set_verbosity_level `Debug
   | _, "debug" -> Tracelog.set_verbosity_level `Debug
   | 1, _ -> Tracelog.set_verbosity_level `Info
   | _ -> ());

  Codex.Hook.(run_hook startup ())
;;

let run () =
  initialize_codex();
  analyze_non_kernel()
;;

let run_codex () =
  if Codex_options.is_enabled () then run ()
