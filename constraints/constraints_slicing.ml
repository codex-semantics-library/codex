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

(* Compute, for each variable, the set of variables on which it
   depends.

   When considering the graph of terms, this corresponds to computing
   the set of nodes that are reachable from the initial term. Most of
   this is easy to compute on the fly. But to compte the set of
   indices needed in the tuples, we need to perform an initial
   pass, which is what this module implements. *)

module Make(C:Constraints_constraints.Constraints) = struct
  
  module CapturedSet = Set.Make(C.Any)
  module CHash = Hashtbl.Make(C.Any)
  module TupleHash = Hashtbl.Make(C.Tuple)
  module IntSet = Set.Make(struct
      type t = int
      let compare (a:int) (b:int) = compare a b 
    end
    )

  let deps c =
    (* The algorithm is a basic depth-first search, using the
       following hashes to mark the visited terms. *)
    
    (* If in the hash, the constraint is required. *)
    let hash = CHash.create 117 in

    (* If in the hash, which indices are required for this constraint. *)
    let tuplehash = TupleHash.create 117 in

    (* Link from a variable to its closed term. Note that this cannot
       be done when we create the variable (since the closed term does
       not yet exist). *)
    let var_to_mu_i = CHash.create 117 in

    let rec constr: 'a . 'a C.t -> unit = fun c ->
      let any = C.Any c in
      if CHash.mem hash any
      then ()
      else begin
        CHash.replace hash any ();
        let open C in
        match C.Utils.get_term c with
        | T0 _ | Empty | Unknown _ -> ()
        | Mu_formal _ ->
          (* A formal depends on its position in the body. *)
          let (mu,i) = CHash.find var_to_mu_i any in
          tuple mu i
        | T1 {a} -> constr a
        | T2 {a;b} -> constr a; constr b
        | Tuple_get(i,tup) -> tuple tup i
      end

    and tuple tup i =
      match TupleHash.find tuplehash tup with
      | exception Not_found -> begin
          TupleHash.replace tuplehash tup IntSet.empty;
          (* Do things that are done only once per tuple. *)
          (match tup with
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
          TupleHash.replace tuplehash tup (IntSet.add i indices);
          match tup with
          | Nondet{a;b} ->
            let C.Any a = Immutable_array.get a i in
            constr a;
            let C.Any b = Immutable_array.get b i in          
            constr b
          | Mu{var;init;body} ->
            let C.Any init = Immutable_array.get init i in
            constr init;
            let C.Any body = Immutable_array.get body i in
            constr body;
            let var = Immutable_array.get var i in
            (* constr var; *) (* No need to fully call constr here. *)
            CHash.replace hash var ();
        end
    in
    constr c;
    ((* (fun x -> CHash.mem hash x), *)
     (fun tup ->
        try let res = TupleHash.find tuplehash tup in
          IntSet.elements res
        with Not_found -> []
     ))
  ;;

end
