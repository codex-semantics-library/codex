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

(* This file should contain only the analysis strategy, while all details are in C2Codex *)

module Regexp = Interpreted_automata_regexp.Regexp
(* module Domain_sig = Codex.Domains.Domain_sig *)
module With_focusing = Codex.Domains.With_focusing

module Log = Tracelog.Make(struct let category = "Direct_analysis" end);;

(* An hash-consed callstack. *)
module Callstack = struct

  type t =
    | Root
    | Frame of { id: int; calling_function: Kernel_function.t; calling_instr: Cil_types.kinstr; parent: t}

  let pretty fmt _ = ();;
  let hash = function
    | Root -> 0
    | Frame {id} -> id
  let compare a b = Stdlib.compare (hash a) (hash b)
  let equal a b = (hash a) == (hash b)


  module FrameHash = Weak.Make(struct
      type nonrec t = t

      let equal
          (Frame {id = _; calling_function =cfa; calling_instr=cia; parent=parenta}) 
          (Frame {id = _; calling_function =cfb; calling_instr=cib; parent=parentb}) =
        Kernel_function.equal cfa cfb && Cil_datatype.Kinstr.equal cia cib && parenta == parentb
      ;;

      let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;

      let hash (Frame {id = _; calling_function =cfa; calling_instr=cia; parent=parenta}) =
        sdbm (Kernel_function.hash cfa) @@ sdbm (Cil_datatype.Kinstr.hash cia) (hash parenta)
      ;;
    end)
  ;;


  let framehash= FrameHash.create 17;;
  let count = ref 1;;
  
  let cons kf ci parent =
    let tentative = Frame { id = !count; calling_function = kf; calling_instr = ci; parent } in
    let ret = FrameHash.merge framehash tentative in
    (if ret == tentative then incr count);
    ret
;;

  

end


(* module Analyze(Domain:Domain_sig.Base) = struct *)
module Analyze(Domain:With_focusing.S_with_types) = struct

  module VarMap = C2Codex.VarMap;;
  module StringMap = C2Codex.StringMap;;    
  module CallingContextHash = Hashtbl.Make(Callstack);;
  
  module C2Codex= C2Codex.Make(Callstack)(Domain)
  
  (* This deals with the analysis of a single function. *)
  module Analyze_Function = struct

    module RegexpHashTbl = Hashtbl.Make(Regexp);;


    let equal_var_addresses a b = VarMap.equal (==) a b
    let equal_string_addresses a b = StringMap.equal (==) a b        

    (* Could be moved to C2Codex, but widening must be here,
       so eventually it is better to keep it here. *)
    let join_state statea stateb =
      (* We assume that the mapping from variable to their addresses
         will be the same in each state. *)
      let open C2Codex in
      assert(equal_string_addresses statea.string_addresses stateb.string_addresses);      
      assert(statea.context.loop_nesting_level == stateb.context.loop_nesting_level);
      (* assert(Cil_datatype.Kinstr.equal statea.context.kinstr stateb.context.kinstr); *)
      assert(Callstack.equal statea.context.calling_context stateb.context.calling_context);
      let make_context ctx =
        { loop_nesting_level = statea.context.loop_nesting_level;
          kinstr = Kglobal;     (* because not the same on both states. *)
          calling_context = statea.context.calling_context;
          ctx = ctx 
        }
      in
      if (equal_var_addresses statea.var_addresses stateb.var_addresses)
      then
        (* Fast, simple case only need to join the memory. *)
        let Domain.Context.Result(_,tup,deserialize2) = 
          Domain.serialize_memory statea.context.ctx statea.mem stateb.context.ctx stateb.mem
            (true,Domain.Context.empty_tuple) in
        let ctxa = statea.context.ctx and ctxb = stateb.context.ctx in
        let ctx,res_tup = Domain.typed_nondet2 ctxa ctxb tup in
        let mem = fst @@ deserialize2 ctx res_tup in
        {mem;
         var_addresses = statea.var_addresses;
         string_addresses = statea.string_addresses;
         context = make_context ctx }
      else
        (* We need to join the addresses of variables because there are multiple ways 
           to enter the block. To avoid this, we could allocate the variable not for the block, 
           but for the whole function.  *)
        let ctxa = statea.context.ctx and ctxb = stateb.context.ctx in 
        let Domain.Context.Result(inc,tup,deserialize_mem) = 
          Domain.serialize_memory ctxa statea.mem ctxb stateb.mem (true,Domain.Context.empty_tuple) in
        let init_d_map = fun _ctx tup -> statea.var_addresses,tup in
        let Domain.Context.Result(_,tup,deserialize_map) = 
          VarMap.fold_on_diff2 statea.var_addresses stateb.var_addresses 
            (Domain.Context.Result(inc,tup,init_d_map))
            (fun vi a b (Domain.Context.Result(inc,acc,d_map) as res) ->
               match a,b with
               | Some a, Some b when a == b (* || BR.Binary.equal a b *) -> res
               | None, None -> res
               | _ ->
                 Codex_log.warning "Merging addresses for variable %a can lead to imprecision" 
                   Cil_datatype.Varinfo.pretty vi;
                 let f = function
                   | None -> assert false (* The variable should be mapped in both cases. *)
                   | Some x -> x
                 in
                 let a = f a and b = f b in
                 let size = Codex_config.ptr_size() in
                 let Domain.Context.Result(inc,acc,d_addr) = Domain.serialize_binary ~size ctxa a ctxb b
                     (inc,acc) in
                 Domain.Context.Result(inc, acc, fun ctx tup ->
                     let addr,tup = d_addr ctx tup in
                     let map,tup = d_map ctx tup in
                     VarMap.add vi addr map,tup))
        in
        let ctx,res_tup = Domain.typed_nondet2 ctxa ctxb tup in
        let (map,res_tup) = deserialize_map ctx res_tup in
        let (mem,_res_tup) = deserialize_mem ctx res_tup in
        {mem;
         var_addresses = map;
         string_addresses = statea.string_addresses;
         context = make_context ctx }
    ;;

    let join_state statea stateb = match statea, stateb with
      | None, x | x, None -> x
      | Some a, Some b -> Some(join_state a b)
    ;;

    let check_argument_types ctx typs args =
      not @@ List.exists2 (fun typ (sz,arg) -> not @@ Domain.has_type ~size:sz ctx typ arg) typs args


    let rec funcall':C2Codex.funcall = fun kf args state ->
      let open C2Codex in
      let fundec = Kernel_function.get_definition kf in
      try 
        let function_typ =
          try Types.Ctypes.function_of_name @@ Kernel_function.get_name kf
          with Types.Ctypes.Undefined_type _ -> raise Not_found
        in
        
        (* Codex_log.debug "Printing state : %a" C2Codex.pretty_state state ; *)
        let typs, rtyp = match function_typ.descr with Function (rtyp, typs) -> typs, rtyp | _ -> assert false in
        (* checking argument types *)
        let ctx = state.context.ctx in
        let _ = check_argument_types ctx typs args in
        let state = {state with mem = Domain.flush_cache ctx state.mem} in
        let ret = match rtyp.descr with
          | Void -> None
          | _ ->
            let ret_size = 8 * Types.Ctypes.sizeof rtyp in 
            Some (ret_size, Domain.binary_unknown_typed ~size:ret_size ctx rtyp)
        in
        ret, Some state
      with Not_found ->
        let old_calling_context = state.context.calling_context in
        let old_kinstr= state.context.kinstr in
        let old_level= state.context.loop_nesting_level in      
        let callstack = Callstack.cons kf state.context.kinstr state.context.calling_context in
        let state = {state with context = { state.context with calling_context = callstack}} in
        let fundec = Kernel_function.get_definition kf in
        let state = C2Codex.init_function_args state kf args in
        let ret,state = analyze_function state kf in
        let state = match state with
          | None -> None
          | Some state ->
            let state = C2Codex.free_function_args state kf in
            (* Restore context. *)
            let context = 
              { ctx = state.context.ctx; loop_nesting_level = old_level;
                kinstr= old_kinstr; calling_context = old_calling_context }
            in
            let state = {state with context} in
            Some(state)
        in
        ret,state

    and funcall kf args stat =
      Log.trace
        (fun p -> p "Calling function %a" Kernel_function.pretty kf)
        (fun fmt _ -> ())
        (fun () -> funcall' kf args stat)
    
    and entryfuncall:C2Codex.funcall = fun kf args state ->
      let open C2Codex in
      Log.trace (fun p -> p "Analyzing function %a" Kernel_function.pretty kf) 
        (fun fmt _ -> ()) (fun () ->
      (* Codex_log.feedback "Calling function %a" Kernel_function.pretty kf; *)
      let old_calling_context = state.context.calling_context in
      let old_kinstr= state.context.kinstr in
      let old_level= state.context.loop_nesting_level in      
      let callstack = Callstack.cons kf state.context.kinstr state.context.calling_context in
      let state = {state with context = { state.context with calling_context = callstack}} in
      let fundec = Kernel_function.get_definition kf in
      let state = C2Codex.init_function_args state kf args in
      let ret,state = analyze_function state kf in
      (try 
        let function_typ =
          try Types.Ctypes.function_of_name @@ Kernel_function.get_name kf
          with Types.Ctypes.Undefined_type _ -> raise Not_found
        in
        let _, rtyp = match function_typ.descr with Function (rtyp, typs) -> typs, rtyp | _ -> assert false in
        match (ret,state), rtyp.descr with
        | (None,_), Void -> ()
        | (Some _, _), Void -> Codex_log.alarm "incompatible-return-type" ; ()
        | (Some (size,value), Some state), _ -> ignore(Domain.has_type ~size state.context.ctx rtyp value)
        | _ -> Codex_log.alarm "incompatible-return-type" ; ()

      with Not_found -> ()
      ) ;
      
      let state = match state with
        | None -> None
        | Some state ->
          let state = C2Codex.free_function_args state kf in
          (* Restore context. *)
          let context = 
            { ctx = state.context.ctx; loop_nesting_level = old_level;
              kinstr= old_kinstr; calling_context = old_calling_context }
          in
          let state = {state with context} in
          Some(state)
      in
      ret,state)
      
    (* Analyze a function, can be also use for function calls. *)
    and analyze_function (entry_state:C2Codex.state) kf =

      let return_value = ref None in
      
      (* Codex_log.feedback "in Analyze function %a" Kernel_function.pretty kf; *)
      
      (* TODO: Compile the regular expression into a stack machine to
         interpret efficiently. *)
      let rec analyze_regexp tbl (initial_state:C2Codex.state) regexp =

        
        let rec doit regexp =
          (* Codex_log.feedback "Analyzing %a" Regexp.pretty regexp; *)
          match RegexpHashTbl.find tbl regexp with
          | exception Not_found ->
            let res = doit' regexp in
            RegexpHashTbl.replace tbl regexp res;
            res
          | res -> res
        and doit' = function
          | Fixpoint.Regex.Empty -> None
          | Fixpoint.Regex.Epsilon -> Some initial_state
          | Fixpoint.Regex.Join(_,a,b) ->
            let statea = doit a in
            let stateb = doit b in
            (* Codex_log.feedback "Doing join %a" Regexp.pretty regexp;             *)
            let state = join_state statea stateb in
            (* Codex_log.feedback "After join: state=%a" C2Codex.pretty_state_option state; *)
            state
          | Fixpoint.Regex.Append(_,a,l) ->
            let state = doit a in
            (* Codex_log.feedback "Doing Append %a %a" Regexp.pretty regexp C2Codex.pretty_state_option state;             *)
            begin match state with
              | None -> None
              | Some state ->
                let (_,e,_)= l in                
                let kinstr = e.Interpreted_automata.edge_kinstr in
                (* Codex_log.feedback "Eval append [%a] %a (kinstr %a)" Regexp.pretty a Letter.pretty l *)
                (*   Cil_datatype.Kinstr.pretty kinstr; *)
                let context = C2Codex.{kinstr = e.Interpreted_automata.edge_kinstr;
                                       ctx = state.context.ctx;
                                       loop_nesting_level = state.context.loop_nesting_level;
                                       calling_context = state.context.calling_context } in
                let state = {state with context} in
                (* Codex_log.feedback "Before transition"; *)
                let state, retval = 
                  C2Codex.transition ~funcall e.Interpreted_automata.edge_transition state
                in
                (* Codex_log.feedback "After instr: state=%a" C2Codex.pretty_state_option state; *)
                (*   let Some state = state in  *)
                (*   Codex_log.feedback "After transition: state = %a" *)
                (*     (Domain.memory_pretty state.C2Codex.context.ctx) state.mem *)
                (* in *)
                (match retval,!return_value with
                | None,_ -> ()
                | Some x, None -> return_value := retval
                | Some _, Some _ -> failwith "Two return values in the function is not handled");
                state
            end
          | Fixpoint.Regex.AppendStar(_,entry,body) ->
            let res = begin match doit entry with
              | None -> None     (* Skip the fixpoint altogether. *)
              | Some loop_entry_state ->
                let do_fixpoint_iteration () =
                  let context = loop_entry_state.context in
                  let ctx' = Domain.mu_context_open context.ctx in
                  let context' =
                    { context with ctx = ctx'; loop_nesting_level = context.loop_nesting_level + 1 } in
                  let state = { loop_entry_state with context = context'} in
                  let rec loop i (state:C2Codex.state) =
                    let body_state = Log.trace (fun p -> p "Iteration %d: Analyzing body" i)
                        (fun fmt -> function None -> Format.fprintf fmt "Bottom" | Some _ -> Format.fprintf fmt "Some state")
                        (fun () ->
                           (* Use a temporary hash table to save the result of the body. *)                         
                           let tbl = RegexpHashTbl.create 17 in
                           analyze_regexp tbl state body )
                    in match body_state with
                    | None ->
                      (* The evaluation of the body fails: only returns the entry state. *)
                      loop_entry_state
                    | Some(state') ->
                      let open C2Codex in
                      let do_fixpoint_step () =
                        assert(equal_var_addresses state.var_addresses state'.var_addresses);
                        assert(equal_string_addresses state.string_addresses state'.string_addresses);
                        let Domain.Context.Result(included,tup,deserialize2) =
                          Domain.serialize_memory state.context.ctx state.mem state'.context.ctx state'.mem
                            (true,Domain.Context.empty_tuple) in
                        let close, f =
                          Domain.typed_fixpoint_step
                            ~init:loop_entry_state.context.ctx
                            ~arg:state.context.ctx 
                            ~body:state'.context.ctx (included,tup) in
                        let out_tuple,out_ctx = f ~close in
                        let result_ctx = (if close then loop_entry_state.context.ctx else state.context.ctx) in
                        let mem = fst @@ deserialize2 result_ctx out_tuple in
                        let state = 
                          {mem; var_addresses = state.var_addresses; 
                           string_addresses = state.string_addresses;
                           context=state'.context}
                        in
                        close,state,out_ctx
                      in
                      let close,state,out_ctx =
                        Log.trace (fun p -> p "Iteration %d: Fixpoint step" i)
                          (fun fmt (close,_,_) -> Format.pp_print_string fmt @@ 
                            if close then "Finished" else "Again")
                          do_fixpoint_step
                      in
                      if close
                      then {state with context = {loop_entry_state.context with ctx = out_ctx}}
                      else loop (i + 1) {state with context = { state.context with ctx = out_ctx}}
                  in
                  let res = Some(loop 0 state) in
                  res
                in
                Log.trace
                  (fun p -> p "Fixpoint iteration")
                  (fun fmt _ -> ())
                  do_fixpoint_iteration
            end in
            res
        in
        doit regexp
      in 
      let exit_regexp,instr_to_regexps = Interpreted_automata_regexp.regexp_of_kf kf in
      
      (* Use the same global tbl for the evaluation of the statements of the function. *)
      let function_tbl = RegexpHashTbl.create 17 in      
      if (Domain.Context.level entry_state.context.ctx) = 0 then
        instr_to_regexps |> Interpreted_automata.Vertex.Map.iter 
          (fun _key regexp -> ignore(analyze_regexp function_tbl entry_state regexp));
      
      (* Note: we cannot evaluate the return value in the exit state,
         as the local variables are already de-allocated. That's why
         we need to do it before. *)
      let exit_state = analyze_regexp function_tbl entry_state exit_regexp in
      !return_value, exit_state

  end


  let print_value_of_exp exp_to_term fmt (indent,ki,exp) =
  
    (try
       let size =
         let typ = Cil.typeOf exp in
         try Cil.bitsSizeOf typ
         with Cil.SizeOfError _ ->
           assert(Cil.isFunctionType typ);
           Codex_config.function_size()
       in
       let exptrie = exp_to_term (ki,exp) in

       (* We collect the strings in a set, because 1. different
          versions of Ocaml print the string in different orders, and
          2. to avoid seeing twice the same output. *)
       let module StringSet = Set.Make(String) in

       let stringset = CallingContextHash.fold (fun _key str acc -> StringSet.add str acc) exptrie StringSet.empty in 
       
       (* Now print the strings; manually add the indentation when we detect newlines. *)
       let count = ref 0 in
       stringset |> StringSet.iter (fun string ->
           (String.split_on_char '\n' string) |> List.iter (fun line ->
               if !count > 0 then begin
                   Printf.fprintf fmt "\n";
                   for _i = 1 to indent do Printf.fprintf fmt " " done;
                 end;
               Printf.fprintf fmt "%s%!" line;
               incr count))

     with Not_found -> Printf.fprintf fmt "<unknown>")
  ;;

  let dump_exp_table outc = 
    let f outc (i,ki,e) =
      try
        let exptrie = (C2Codex.exp_to_value (ki,e)) in

        CallingContextHash.iter  (fun _ str -> Stdlib.output_string outc str) exptrie;
        (* Stdlib.output_string outc "<found>" *)
      with Not_found -> Stdlib.output_string outc "<unknown>"
    in
    let should_print = C2Codex.exp_has_value in
    let f = print_value_of_exp C2Codex.exp_to_value in
    (* Exp_dump.exp_dump1 f stdout; *)
    (* Exp_dump.exp_dump1 f stdout;       *)
    Exp_dump.exp_dump ~should_print f outc
  ;;

  let dump_alarms outc = 
    let f alarm loc boolean =
      Printf.fprintf outc "%s: %s %s\n"
        (Format.asprintf "%a" Cil_datatype.Location.pretty loc)
        (Format.asprintf "@[<h>%a@]" (fun fmt x -> Format.pp_set_max_indent fmt 1000; Alarms.pretty fmt x) alarm)
        (Format.asprintf "%a" Codex.Lattices.Quadrivalent.pretty boolean)
    in
    let total_count = ref 0 in
    let proved_count = ref 0 in
    let f alarm loc boolean =
      incr total_count;
      let open Codex.Lattices.Quadrivalent in
      match boolean with
      | True | Bottom -> incr proved_count
      | False | Top ->
        f alarm loc boolean
    in
    Printf.fprintf outc "Unproved alarms:\n%!";
    C2Codex.iter_on_alarms f;
    Printf.fprintf outc "Proved %d/%d alarms\n%!" !proved_count !total_count;
    Printf.fprintf outc "Unproved %d regular alarms and %d additional alarms.\n%!" (!total_count - !proved_count) !Codex_log.alarm_count;
  ;;

  let dump_assertions outc =
    (* Dump the evaluation of assertions. *)
    let total_assertions_count = ref 0 in
    let proved_assertions_count = ref 0 in
    let solved_assertions_count = ref 0 in
    let f loc boolean how =
      incr total_assertions_count;
      let open Codex.Lattices.Quadrivalent in
      begin match boolean with
        | "TRUE (valid)" | "TRUE (dead)" | "TRUE (unreachable)" -> incr proved_assertions_count; incr solved_assertions_count
        | "UNKNOWN" -> ()
        | "FALSE (counter-example exists)" | "FALSE (invalid)" | "FALSE (reachable)" -> incr solved_assertions_count
        | _ -> assert false
      end;
      Printf.fprintf outc "%s: assertion is %s (proved with %s)\n"
        (Format.asprintf "%a" Cil_datatype.Location.pretty loc)
        boolean
        (* (Format.asprintf "%a" Codex.Lattices.Quadrivalent.pretty boolean) *)
        how
    in
    C2Codex.iter_on_assertions f;
    Printf.fprintf outc "Solved %d/%d user assertions, proved %d\n%!" !solved_assertions_count !total_assertions_count !proved_assertions_count;            
    ()
  ;;
  ;;

  let print_html file =
    let pp_exp_value = print_value_of_exp C2Codex.exp_to_value in
    let print_expinfo fmt (ki,exp) =
      Printf.fprintf fmt "<td>Expr</td> <td>%s</td> <td>%s</td> <td>%a</td>"
        (Format.asprintf "%a" Cil_datatype.Exp.pretty exp) 
        (Format.asprintf "%a" Cil_datatype.Typ.pretty (Cil.typeOf exp))
        pp_exp_value (0, ki, exp)
    in
    let print_lvalinfo fmt (ki,exp) =
      Printf.fprintf fmt "<td>Lval</td> <td>%s</td> <td>%s</td>"
        (Format.asprintf "%a" Cil_datatype.Lval.pretty exp)
        (Format.asprintf "%a" Cil_datatype.Typ.pretty (Cil.typeOfLval exp))
    in
    let print_function fmt name =
      Printf.fprintf fmt "<td>Func</td> <td>%s</td> <td>%a</td>" 
      name
      Types.Ctypes.pp_function_type name
    in
    Printhtml.print file print_expinfo print_lvalinfo print_function; ()
  ;;

  let dump_html() =
    let htmlfile = Codex_options.HtmlDump.get() in
    if htmlfile = "" then () else print_html htmlfile;
  ;;

let parse_ctypes () =
  let file = Codex_options.TypeConfigurationFile.get() in
  if file = "" then () else Types.Parse_ctypes.parse_file file
;;

  let run kf =
    parse_ctypes();
    begin match (C2Codex.initial_state kf Root) with
    | Some state,args ->
      let context = state.context in
      (* ignore(Analyze_Function.funcall kf args state); *)
      ignore(Analyze_Function.entryfuncall kf args state);
      begin
        (* Dump the alarms and assertions with exp_dump if it exists. *)
        match Codex_options.ExpDump.get() with
        | "" -> begin
           dump_alarms stdout;
           dump_assertions stdout;
          end
        | file -> begin
           let outc = open_out file in
           dump_exp_table outc;
           dump_alarms outc;
           dump_assertions outc;
           close_out outc
          end
      end;
      dump_html();
      exit 0
    | None, _ -> exit 1
    end
  ;;
    

end

