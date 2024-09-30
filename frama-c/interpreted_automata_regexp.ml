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

module Interpreted_automata = Frama_c_kernel.Interpreted_automata
module Cil_types = Frama_c_kernel.Cil_types
module Cil_datatype = Frama_c_kernel.Cil_datatype
module Kernel_function = Frama_c_kernel.Kernel_function

module Letter = struct
  include Codex.Utils.Datatype_sig.Prod3(Interpreted_automata.Vertex)(Interpreted_automata.Edge)(Interpreted_automata.Vertex)
  open Interpreted_automata

  let pretty_kinstr fmt = function
    | Cil_types.Kglobal -> Format.fprintf fmt "kglobal"
    | Cil_types.Kstmt s -> Format.fprintf fmt "kstmt.sid_%d" s.sid
  ;;

  let pretty fmt (src,e,sink) =    
    Format.fprintf fmt "%d --(%d : %a)--> %d " src.Interpreted_automata.vertex_key e.Interpreted_automata.edge_key pretty_kinstr e.Interpreted_automata.edge_kinstr sink.Interpreted_automata.vertex_key 
  ;;

  (* We redefine equality (and hash) : the edges in interpreted
     automata cannot be used globally otherwise, as they depend on a
     key which depends on the function. *)
  let equal = (==)  
  let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;
  let hash (_,e,_) = 
    sdbm (Cil_datatype.Kinstr.hash e.Interpreted_automata.edge_kinstr) e.edge_key
  ;;

end

module Regexp = Fixpoint.Regex.Make(Letter)


module Function_to_regexp = struct

  (* Interpreted automata create many spurious, unconnected nodes.
     We add to the graph a reachability predicate to eliminate them,
     that we use when folding on predecessors.  *)
  module Graph = struct
    module V = Interpreted_automata.Vertex
    module E = Interpreted_automata.G.E

    type t = Interpreted_automata.G.t * (V.t -> bool)
    let fold_pred_e f (g,reachable) v acc =
      let f e acc =
        if reachable @@ E.src e
        then f e acc
        else acc
      in
      Interpreted_automata.G.fold_pred_e f g v acc
  end

  module Wto = Fixpoint.Wto.Make(Graph.V)
  module M = Fixpoint.Reduce.Make(Graph)(Regexp)


  let to_regexp kf =
    let automaton = Interpreted_automata.get_automaton kf in
    (* Interpreted_automata.output_to_dot stdout automaton;     *)
    let graph = automaton.Interpreted_automata.graph in

    let init = automaton.Interpreted_automata.entry_point in
    (* exit is just after the return statement. *)
    let exit = automaton.Interpreted_automata.return_point in

    let (return,_exit) = 
      Cil_datatype.Stmt.Hashtbl.find
        automaton.Interpreted_automata.stmt_table (Kernel_function.find_return kf) 
    in
    assert(_exit == exit);
    let succs = Interpreted_automata.G.succ graph in

    (* Reachability. *)
    (* Note: probably we could compute reachability from the WTO. *)
    let reachable =
      let module H = Hashtbl.Make(Interpreted_automata.Vertex) in
      let hashtbl = H.create 17 in
      let rec loop x =
        if H.mem hashtbl x then ()
        else begin
          H.replace hashtbl x ();
          List.iter loop @@ succs x
        end
      in
      loop init;
      fun x ->
        (* Codex_log.feedback "is reachable %a? %b"
           Interpreted_automata.Vertex.pretty x (H.mem hashtbl x); *)
        H.mem hashtbl x
    in

    let pref = fun _ _ -> 0 in
    let wto = Wto.partition ~pref ~init ~succs in
    (* Interpreted_automata.output_to_dot stdout automaton; *)
    let instr_to_regexps = M.compute_exprs (graph,reachable) wto in
    (* Hashtbl.iter (fun node rex -> *)
        (* Codex_log.feedback "node %a regexp %a" Interpreted_automata.Vertex.pretty node Regexp.pretty rex *)
    (* ) instr_to_regexps; *)

    let _return_regexp =
      try Hashtbl.find instr_to_regexps return
      with Not_found -> Regexp.empty
    in
    let exit_regexp =
      try Hashtbl.find instr_to_regexps exit
      with Not_found -> Regexp.empty
    in
    (* Codex_log.feedback "final_regexp for %a is %a"  Kernel_function.pretty kf Regexp.pretty final_regexp; *)

    (* It is better to use a fixed order for iterating on the regexps. *)
    let instr_to_regexps =
      Hashtbl.fold
        Interpreted_automata.Vertex.Map.add instr_to_regexps Interpreted_automata.Vertex.Map.empty
    in
    
    (* return_regexp,  *)exit_regexp, instr_to_regexps
end

let regexp_of_kf = Function_to_regexp.to_regexp
                     
