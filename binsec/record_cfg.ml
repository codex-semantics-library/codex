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

(** A module to record a set of traces. Note: imperative interface for now. *)
open Html

module type S = sig

  type t

  val nb_forks: t -> int
  val init: Loader.Img.t -> Virtual_address.t -> t
  val reuse: t -> t
    (** Reopens the file handles and empty the work list that keeps track of
        forks. The trace recorded is unchanged. *)

  type context_change =
  | NoChange (** We remained in the same function. *)
  | EnteredFunction of string (** We entered a new function. *)
  | LeftFunction of string (** We left the current function, possibly more. *)

  exception Visited_vertex

  (** [next trace addr] adds the code address [addr] to the current
     trace [trace]. It returns a value of type [context_change] to express how
     the call stack is affected (according to our heuristic). If [addr] has
     been visited before, [next trace addr] raises the exception
     [Visited_vertex], unless [exploration_only] is set. *)
  val next: t -> exploration_only:bool -> record_cfg:bool -> Virtual_address.t -> unit
  val set_position : t -> keep_forks:bool -> Cfg_analysis.Cfg.V.t -> unit
    (** Set the current position. Overwrites the work list that keeps track of
        forks. This should only be used before starting a depth-first analysis
        at a given address, never during analysis.  *)

  val current_position : t -> Cfg_analysis.Cfg.V.t

  val instruction_graph : t -> Cfg_analysis.Cfg.t

  val call_stack_after_jump : t -> Cfg_analysis.Cfg.V.t -> Virtual_address.t ->
    Cfg_analysis.V.call_stack * context_change
    (** Given the current state and a destination address, return the call
        stack after a jump to that destination. The call stack reconstruction
        assumes that the symbols in the code represent the actual functions of
        every instruction. *)

  val start_fork: t -> unit
  val next_fork: t -> unit
  val end_fork: t -> unit
  val close: t -> Virtual_address.t ->
             graph_filename:string ->
             html_filename:string option -> (string, string) Hashtbl.t -> Cfg_analysis.Cfg.t
    (** [close tr start] computes the CFG (i.e. the graph of basic blocks) from
        the instruction graph, starting at address [start] using the record
        trace [tr]. The CFG is written to a Graphviz file, as well as an UML
        sequence diagram. *)

  val graph_changed : t -> bool
  val set_graph_changed : t -> bool -> unit

  (** Back edges are a relative notion. This tells whether the edge is a back
     edge relatively to the depth-first search memorized by [trace]. *)
  val back_edge : trace:t -> Cfg_analysis.Cfg.V.t -> Cfg_analysis.Cfg.V.t -> bool

  val visited_instructions : t -> Virtual_address.Set.t
end

module Interval2symbol = struct
  include Interval2symbol
  let find addr table =
    if addr = 0 then "ZERO"
    else if addr = 0xefacefac || addr = 0xcafecaf1 || addr = 0xcafecaf2
    then "_start"
    else if addr = 0xcafecafe || addr = 0xcafecaf0 then "ABSTASK"
    else match find addr table with
    | "Finished" | "Loop1" | "Skip" | "cpus_startup"
    | "cpu_init" | "cpu_startup_error" -> "Reset_Handler"
    | other -> other
end

module Logger = Codex_logger

module Make
  (State : Dba2Codex.StateS)
: S = struct

  module Cfg = Cfg_analysis.Cfg

  module Vertex_set = Set.Make(Cfg.V)
  module Edge_set = Set.Make(struct
    type t = Cfg.V.t * Cfg.V.t
    let compare (src1,dst1) (src2,dst2) =
      let c = Cfg.V.compare src1 src2 in
      if c = 0 then Cfg.V.compare dst1 dst2
      else c
  end)

  module Tree = struct
    (* For each node, its ancestor and its successors. *)
    type t = {
      parents: (Cfg.V.t, Cfg.V.t) Hashtbl.t;
      children: (Cfg.V.t, Vertex_set.t) Hashtbl.t;
    }

    let empty () = {
      parents = Hashtbl.create 17;
      children = Hashtbl.create 17 }

    let add {parents;children} parent child =
      Logger.result "adding tree edge %a -> %a"
        Cfg.V.pretty parent Cfg.V.pretty child;
      let new_children =
        try Vertex_set.add child (Hashtbl.find children parent)
        with Not_found -> Vertex_set.singleton child in
      Hashtbl.replace children parent new_children;
      Hashtbl.replace parents child parent

    let rec is_parent ({parents;_} as t) x y =
      try
        let p = Hashtbl.find parents y in
        if x = p
        then true
        else is_parent t x p
      with Not_found -> false
  end

  type call_stack = Cfg_analysis.V.call_stack

  type state =
    {
      func: string;
      address: Virtual_address.t;
      stack: call_stack
        (* Approximate call stack generated using debug symbols. Invariant: the
         * stack is never empty: it always contains at least the initial
         * address. *)
    }


  type t = {
    graph : Cfg.t;
    mutable graph_changed : bool;
    tree : Tree.t; (* A spanning tree of the graph. *)
    mutable back_edges : Edge_set.t;
    symtable : string Interval2symbol.t;
    outc : out_channel;
    mutable worklist : state list;   (* stack of states to keep track of forks. *)
    mutable visited : Virtual_address.Set.t;
  }

  let instruction_graph {graph;_} = graph

  let nb_forks {worklist;_} = List.length worklist

  let back_edge ~trace v1 v2 =
    Edge_set.mem (v1,v2) trace.back_edges

  let basic_block_graph {graph;symtable;_} initial =
    Logger.result "basic_block_graph: %i nodes" @@ Cfg.nb_vertex graph;
    let cfg = Cfg.create ~size:100 () in
    let block_map = Hashtbl.create 100 in
    (* Walk the instruction graph and construct basic blocks. *)
    let rec walk_graph pred current_leader instruction_acc addr =
      let succ = Cfg.succ graph addr in
      (* Distinguish according to the number of successors of the assembly
       * instruction *)
      match List.length succ with
      | 0 ->
          (* Ending path *)
          let this_block = List.rev (fst addr :: instruction_acc) in
          Cfg.add_edge_e cfg (pred, current_leader);
          Hashtbl.add block_map (fst current_leader) this_block
      | 1 ->
          let succ = List.hd succ in
          (* [succ] is the leader of a new block if it is in a different
           * function or has more than one predecessor. Otherwise, it is
           * integrated in the current block. *)
          if List.length (Cfg.pred graph succ) <> 1 ||
              (Interval2symbol.find (Virtual_address.to_int (fst succ)) symtable
               <> Interval2symbol.find (Virtual_address.to_int (fst current_leader)) symtable)
          then (
            let this_block = List.rev (fst addr :: instruction_acc) in
            Cfg.add_edge_e cfg (pred, current_leader);
            Hashtbl.add block_map (fst current_leader) this_block;
            (* Only recurse on the successor if not already in the graph. *)
            if not (Cfg.mem_vertex cfg succ) then
              walk_graph current_leader succ [] succ
            else
              (* Add the edge to the graph and stop the recursion. *)
              Cfg.add_edge_e cfg (current_leader, succ)
          ) else
            walk_graph pred current_leader (fst addr :: instruction_acc) succ
      | _ ->
          (* End of current basic block, add successors to the CFG and call
           * [walk_graph] on them *)
          (* We assume that the instruction is a conditional jump and that the
           * cardinal of [succ] is 2. *)
          let this_block = List.rev @@ fst addr :: instruction_acc in
          Cfg.add_edge_e cfg (pred, current_leader);
          Hashtbl.add block_map (fst current_leader) this_block;
          succ |> List.iter (fun dst ->
            if not (Cfg.mem_vertex cfg dst) then
              walk_graph current_leader dst [] dst
            else
              Cfg.add_edge_e cfg (current_leader, dst))
    in
    (* Initiate recursion *)
    Hashtbl.add block_map (fst initial) [fst initial];
    Cfg.iter_succ (fun n -> walk_graph initial n [] n) graph initial;
    cfg, block_map

  (*** CFG nodes are drawn in a hierarchy of clusters, according to their call
       stack. ***)

  type cluster =
    { name : string;
      cl_stack : call_stack;
      leaves : Cfg.V.t list;
      clusters : cluster list; }

  let rec last = function
    | [] -> raise @@ Invalid_argument "Record_cfg.last"
    | [x] -> x
    | _ :: xs -> last xs

  let rec pp_cluster cfg block_map fmt cl =
    let open Format in
    fprintf fmt "@[<v 2>subgraph \"cluster_%s%a\" {@,\
      style=\"filled,solid\";@,color=black;@,fillcolor=lightgrey;@,label=\"%s\";@]" cl.name Cfg.V.pp_stack
      cl.cl_stack cl.name;
    let pp_node fmt = fun ((lead_addr,_) as leader) ->
        let last_addr = Hashtbl.find block_map lead_addr |> last in
        fprintf fmt "\"%a\" [label=<<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\"><TR><TD>%a</TD></TR><HR/><TR><TD>%a</TD></TR></TABLE>>%s];@\n"
          Cfg.V.pretty leader
          Virtual_address.pp lead_addr
          Virtual_address.pp last_addr
          (if Cfg.out_degree cfg leader = 0 then " fillcolor=lightblue" else "")
    in
    cl.leaves |> List.iter (pp_node fmt);
    cl.clusters |> List.iter (fun sub_cl -> fprintf fmt "@[<hov 2>%a@]"
      (pp_cluster cfg block_map) sub_cl);
    fprintf fmt "}\n"

  let rec add_to_cl cl cfg trace visited ((_,node_stack) as node) =
    let node_stack_l = List.length node_stack in
    let stack_l = List.length cl.cl_stack in
    if node_stack_l = stack_l then (
      (* The node should be in this cluster. Invariant: its successors should
       * be in this cluster or an immediate sub-cluster *)
      let cl = { cl with leaves = node :: cl.leaves } in
      (* Recursive call: add successors (and their descendants) *)
      let new_cl, rejected = Cfg.fold_succ (fun succ (cl, rejected) ->
          let n_cl, r = if Vertex_set.mem succ !visited
            then cl, [] (* We don't follow back edges *)
            else begin
              visited := Vertex_set.add succ !visited;
              add_to_cl cl cfg trace visited succ
            end in
          n_cl, r @ rejected
        ) cfg node (cl, []) in
      (* Try to add rejected nodes to this cluster, otherwise return them *)
      let res = List.fold_left (fun (cl, rejected) n ->
          let n_cl, r = add_to_cl cl cfg trace visited n in
          n_cl, r @ rejected
        ) (new_cl,[]) rejected
      in
      res
    ) else if node_stack_l = stack_l + 1 then (
      (* The node should be in a sub-cluster. Create it, and add the node to
       * it. *)
      let sub_cl = {
        name = Interval2symbol.find (Virtual_address.to_int (fst node))
          trace.symtable;
        cl_stack = node_stack;
        leaves = [];
        clusters = [] }
      in
      let sub_cl, rejected = add_to_cl sub_cl cfg trace visited node in
      let cl = { cl with clusters = sub_cl :: cl.clusters } in
      (* Try to add rejected nodes to this cluster, otherwise return them *)
      let res = List.fold_left (fun (cl, rejected) n ->
          let n_cl, r = add_to_cl cl cfg trace visited n in
          n_cl, r @ rejected
        ) (cl,[]) rejected
      in
      res
    ) else if node_stack_l < stack_l then (
      (* The node should be in an ancestor cluster. Simply return it. *)
      cl, [node]
    ) else
      assert false

  let dump_graph trace entry_node bbg block_map fmt =
    Format.fprintf fmt "digraph G {@\n";
    Format.fprintf fmt "node[fillcolor=white style=\"filled,solid\" shape=none margin=0];@\n";
    bbg |> Cfg.iter_edges (fun block1 block2 ->
        let open Format in
        fprintf fmt "\"%a\" -> \"%a\"" Cfg.V.pretty block1 Cfg.V.pretty block2;
        let src_last = fst block1 |> Hashtbl.find block_map |> last in
        if block1 = block2
        then fprintf fmt " [dir=back color=red]"
        else if Edge_set.mem ((src_last, snd block1), block2) trace.back_edges
        then fprintf fmt " [color=red constraint=false]";
        fprintf fmt ";@\n");
    (* Compute clusters. *)
    let init_cl =
      { name = Interval2symbol.find (Virtual_address.to_int (fst entry_node))
          trace.symtable;
        leaves = [];
        clusters = [];
        cl_stack = [] } in
    let cluster_tree,rejected = add_to_cl init_cl bbg trace (ref Vertex_set.empty) entry_node in
    assert (rejected = []);
    (* Display clusters. *)
    Format.fprintf fmt "@[<hov 2>%a@]\n" (pp_cluster bbg block_map) cluster_tree;
    Format.fprintf fmt "}";
    Format.pp_print_flush fmt ();;
  
  module VAList = Map.Make(Virtual_address)

  let dump_html trace entry_node bbg block_map fmtHtml hashtbl =
    Format.fprintf fmtHtml "%s" Html.header;
    
    let prev = ref "" in
    Hashtbl.iter (fun k v ->
      begin if !prev <> k || !prev == "" then
        Format.fprintf fmtHtml "results[\"%s\"] = [];\n" k;
      end;
      Format.fprintf fmtHtml "results[\"%s\"].push(`%s`);\n" k v;
      prev := k
    ) hashtbl;

    Format.fprintf fmtHtml "%s" Html.header1;
    dump_graph trace entry_node bbg block_map fmtHtml;
    Format.fprintf fmtHtml "%s" Html.header2;
    
    let valist = ref VAList.empty in
    let hashtbl = Hashtbl.create 200000 in
    let initlist = ref [] in
    bbg |> Cfg.iter_vertex (fun block1 ->
      let instr, nextva = Disasm_core.decode (fst block1) in
      initlist := (instr, nextva) :: !initlist
    );
    let sorted = List.sort (fun (a, _) (b, _) -> if a.Instruction.address > b.Instruction.address then 1 else -1) !initlist in
    
    List.iter (fun (instr, nextva) ->
      match (VAList.find_opt instr.Instruction.address !valist) with
      | Some v -> ()
      | None -> valist := Html.load_addresses_from instr nextva !valist hashtbl;
      )
      sorted;
    
    Html.logger_hashtable fmtHtml hashtbl;
    
    Format.fprintf fmtHtml "%s" Html.header3;
    Html.print_html fmtHtml !valist hashtbl;
    Format.fprintf fmtHtml "%s" Html.footer;

    let alarms = Logger.alarm_record () in
    Format.fprintf fmtHtml "%a" Logger.Alarm_record.(pretty_html ~unique:true) alarms;
    
    Format.fprintf fmtHtml "%s" Html.footer1;;

  let init img start =
    let outc = open_out "file.uml" in
    output_string outc "@startuml\n";
    let symtable =
      let symbols = Loader.Img.symbols img in
      Array.fold_left (fun acc symb ->
          Interval2symbol.insert ~merge:(fun ~old:_ x -> x)
            (Loader.Symbol.value symb) (Loader.Symbol.name symb) acc
        ) Interval2symbol.empty symbols
    in
    let graph = Cfg.create ~size:10000 () in
    Cfg.add_vertex graph (start, []);
    let tree = Tree.empty () in
    let back_edges = Edge_set.empty in
    let graph_changed = false in
    (*
    let start_func =
      try Interval2symbol.find (Virtual_address.to_int start_addr) symtable
      with Not_found -> raise @@ Failure
        (Format.asprintf "no symbol for address %i in debug symbols"
          (Virtual_address.to_int start_addr)) in
    *)
    { graph; graph_changed; tree; back_edges; symtable; outc; worklist = [];
      visited = Virtual_address.Set.empty }

  let reuse tr =
    (* Essentially just reopen the file handle and empty the worklist *)
    let outc = open_out "file.uml" in
    output_string outc "@startuml\n";
    (*
    let start_func =
      try Interval2symbol.find (Virtual_address.to_int start_addr) tr.symtable
      with Not_found -> raise @@ Failure
        (Format.asprintf "no symbol for address %i in debug symbols"
          (Virtual_address.to_int start_addr)) in
    *)
    { tr with outc = outc; worklist = [] }

  exception Visited_vertex

  type context_change =
  | NoChange (** We remained in the same function. *)
  | EnteredFunction of string (** We entered a new function. *)
  | LeftFunction of string (** We left the current function, possibly more. *)

  (* Given the current state and a destination address, return the call stack
   * after a jump to that destination. The call stack reconstruction assumes
   * that the symbols in the code represent the actual functions of every
   * instruction. *)
  let call_stack_after_jump tr (src_addr,src_stack) dst_addr =
    let src_func =
      Interval2symbol.find (Virtual_address.to_int src_addr) tr.symtable in
    let dst_func =
      Interval2symbol.find (Virtual_address.to_int dst_addr) tr.symtable in
    if dst_func <> src_func then begin
      (* Look up {dst_func} down the stack, and if found, return the stack with
       * all elements down to {dst_func} included unstacked. *)
      let rec lookup = function
      | i :: is ->
          let func = Interval2symbol.find (Virtual_address.to_int i) tr.symtable in
          if dst_func = func then Some is
          else lookup is
      | [] -> None
      in
      match lookup src_stack with
      | Some new_stack ->
            (* We *assume* this is a return to the function {dst_func}. We pop
             * all elements in-between (there can because of sibling call
             * optimisation). This works as long as there are no recursive
             * functions. *)
            new_stack, LeftFunction src_func
      | None ->
            (* This is a call to a new function. We push it on the stack. *)
            src_addr :: src_stack, EnteredFunction dst_func
    end
    else src_stack, NoChange

  let current_position tr =
    match tr.worklist with
    | [] -> raise Not_found
    | cur :: _ -> cur.address, cur.stack

  let next tr ~exploration_only ~record_cfg address =
    Codex_logger.current_instr_addr := address;
    tr.visited <- Virtual_address.Set.add address tr.visited;
    match tr.worklist with
    | [] ->
        (* [address] is the first instruction visited. *)
        let stack = [] in
        let func = Interval2symbol.find (Virtual_address.to_int address) tr.symtable in
        tr.worklist <- [{func;address;stack}];
    | previous :: _ ->
        let previous_node = previous.address, previous.stack in
        let stack, ctx_change = call_stack_after_jump tr previous_node address in
        let new_node = address, stack in
        let log_ctx_change = function
        | NoChange -> previous.func
        | EnteredFunction name ->
            Logger.result "Calling function %s from %s" name previous.func; name
        | LeftFunction name ->
            let new_func = Interval2symbol.find (Virtual_address.to_int address) tr.symtable in
            Logger.result "Returning from function %s into %s" name new_func;
            new_func
        in
        if Cfg.mem_vertex tr.graph new_node
        then begin
          Logger.result "next, case vertex existing";
          if record_cfg then begin
            (* still add the edge to the graph *)
            if not (Cfg.mem_edge tr.graph previous_node new_node) then
              tr.graph_changed <- true;
            Cfg.add_edge tr.graph previous_node new_node;
            (* If the edge is a back edge, record it *)
            if Tree.is_parent tr.tree new_node previous_node
            then tr.back_edges <- Edge_set.add (previous_node, new_node) tr.back_edges;
          end;
          if exploration_only then begin
            let func = log_ctx_change ctx_change in
            tr.worklist <- {func;address;stack}::(List.tl tr.worklist);
            Logger.result "end next, case vertex existing";
          end else begin Logger.result "end next, case vertex existing";raise Visited_vertex end
        end else begin
          Logger.result "next, case vertex new";
          if record_cfg then begin
            tr.graph_changed <- true;
            Cfg.add_edge tr.graph previous_node new_node;
            (* Add destination node to the spanning tree. *)
            Tree.add tr.tree previous_node new_node;
          end;
          (* Log function change (if any) *)
          let func = log_ctx_change ctx_change in
          tr.worklist <- {func;address;stack}::(List.tl tr.worklist);
          Logger.result "end next, case vertex new";
        end

  let set_position tr ~keep_forks (address,stack) =
    let func = Interval2symbol.find (Virtual_address.to_int address) tr.symtable in
    let new_state = {func;address;stack} in
    Codex_logger.current_instr_addr := address;
    match tr.worklist with
    | [] -> tr.worklist <- [new_state]
    | _ :: tl ->
        tr.worklist <- if keep_forks then new_state :: tl else [new_state]

  let start_fork tr =
    tr.worklist <- List.hd tr.worklist :: tr.worklist;
    output_string tr.outc "alt \n"
  let next_fork tr =
    let tl = List.tl tr.worklist in
    tr.worklist <- List.hd tl :: tl;
    output_string tr.outc "else \n"
  let end_fork tr =
    tr.worklist <-  List.tl tr.worklist;
    output_string tr.outc "end \n"

  let close tr initial_addr ~graph_filename ~html_filename results =
    Logger.result "RecordTrace.close called";
    output_string tr.outc "participant hal_cpu_id\n"; (* Hack to put it last. *)
    output_string tr.outc "@enduml\n";
    close_out tr.outc;
    let bbg, block_map = basic_block_graph tr (initial_addr, []) in
    
    let outcGraph = open_out graph_filename in
    let fmtGraph = Format.formatter_of_out_channel outcGraph in
    dump_graph tr (initial_addr, []) bbg block_map fmtGraph;

    begin match html_filename with
    | None -> ()
    | Some html_filename ->
       let outcHtml = open_out html_filename in
       let fmtHtml = Format.formatter_of_out_channel outcHtml in
       dump_html tr (initial_addr, []) bbg block_map fmtHtml results;
       close_out outcHtml
    end;

    close_out outcGraph;
    bbg

  let graph_changed tr =
    tr.graph_changed

  let set_graph_changed tr b =
    tr.graph_changed <- b

  let visited_instructions trace =
    trace.visited
end
