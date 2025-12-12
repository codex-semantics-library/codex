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

(* Compute, for each variable, the set of variables on which it
   depends.

   When considering the graph of terms, this corresponds to computing
   the set of nodes that are reachable from the initial term. Most of
   this is easy to compute on the fly. But to compte the set of
   indices needed in the tuples, we need to perform an initial
   pass, which is what this module implements. *)

module Make(T: Sig.TERMS) = struct

  module CapturedSet = Set.Make(T.Any)
  module CHash = Hashtbl.Make(T.Any)
  module CFG_Node_Hash = Hashtbl.Make(T.CFG_Node)
  module IntSet = Set.Make(Int)

  let deps c =
    (* The algorithm is a basic depth-first search, using the
       following hashes to mark the visited terms. *)

    (* If in the hash, the constraint is required. *)
    let hash = CHash.create 117 in

    (* If in the hash, which indices are required for this constraint. *)
    let cfg_node_hash = CFG_Node_Hash.create 117 in

    (* Link from a variable to its closed term. Note that this cannot
       be done when we create the variable (since the closed term does
       not yet bound_typ). *)
    let var_to_mu_i = CHash.create 117 in

    let rec constr: 'a . 'a T.t -> unit = fun c ->
      let any = T.Any c in
      if CHash.mem hash any
      then ()
      else begin
        CHash.replace hash any ();
        let open T in
        match Utils.get_term c with
        | T0 _ | Empty | Unknown _ -> ()
        | Mu_formal _ ->
          (* A formal depends on its position in the body. *)
          let (mu,i) = CHash.find var_to_mu_i any in
          tuple mu i
        | T1 {a} -> constr a
        | T2 {a;b} -> constr a; constr b
        | Tuple_get(i,tup) -> tuple tup i
        | Inductive_var{definition} -> constr definition
      end

    and tuple tup i =
      match CFG_Node_Hash.find cfg_node_hash tup with
      | exception Not_found -> begin
          CFG_Node_Hash.replace cfg_node_hash tup IntSet.empty;
          (* Do things that are done only once per tuple. *)
          (match tup with
          | Inductive_vars _ -> assert false
          | Nondet{conda_bool;condb_bool} ->
            constr conda_bool; constr condb_bool;
          | Mu{var;body_cond} -> begin
              var |> Immutable_array.iteri (fun i var ->
                  CHash.replace var_to_mu_i var (tup,i)
                );
              constr body_cond;
            end);
          tuple tup i
        end
      | indices ->
        if IntSet.mem i indices
        then ()
        else begin
          CFG_Node_Hash.replace cfg_node_hash tup (IntSet.add i indices);
          match tup with
          | Inductive_vars _ -> assert false
          | Nondet{a;b} ->
            let T.Any a = Immutable_array.get a i in
            constr a;
            let T.Any b = Immutable_array.get b i in
            constr b
          | Mu{var;init;body} ->
            let T.Any init = Immutable_array.get init i in
            constr init;
            let T.Any body = Immutable_array.get body i in
            constr body;
            let var = Immutable_array.get var i in
            (* constr var; *) (* No need to fully call constr here. *)
            CHash.replace hash var ();
        end
    in
    constr c;
    ((* (fun x -> CHash.mem hash x), *)
     (fun tup ->
        try let res = CFG_Node_Hash.find cfg_node_hash tup in
          IntSet.elements res
        with Not_found -> []
     ))
end
