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

open Cfg_sig;;

module Make(CL:Control_location):Cfg with module Control_location = CL =
struct
  module Control_location = CL;;

  type node =
    { id: int;
      control_location: CL.t;
      (* Sorted list of succs and preds. *)
      mutable succs: node list;
      mutable preds: node list;
    };;

  module Node = struct
    module Control_location = CL;;
    type t = node;;
    module CL_hash = Hashtbl.Make(CL);;
    let cl_hash = CL_hash.create 17;;
    let count = ref 0;;
    let fresh() = let v = !count in incr count; v;;

    (* Returns get, and a boolean telling if the node is new. *)
    let get cl =
      match CL_hash.find cl_hash cl with
      | exception Not_found ->
        let count = fresh() in
        let n = { id=count;control_location=cl;succs=[];preds=[]} in
        CL_hash.replace cl_hash cl n;
        n
      | n -> n
    ;;

    let succs n = n.succs;;
    let preds n = n.preds;;

    let compare a b = a.id - b.id;;

    let pretty fmt x = Format.fprintf fmt "n%d (%a)" x.id CL.pretty x.control_location;;
    let equal a b = a == b       (* by hash-consing, nodes are physically equal. *)
    let hash a = a.id;;

    let control_location x = x.control_location;;

  end

  let register_edge source sink =
    (* Node that [sink] may be already known. *)
    let n = Node.get sink in
    source.succs <- List.merge Node.compare source.succs [n];
    n.preds <- List.merge Node.compare n.preds [source];
    n
  ;;

  let known = Node.CL_hash.find_opt Node.cl_hash;;



end
