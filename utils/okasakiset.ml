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

module Make(Elt:sig
    type t
    (* Each element must be uniquely identified by an integer. *)
    val unique_id: t -> int
  end) = struct

  module Map = Okasakimap.Make(struct
      include Elt
      let to_int x = unique_id x
    end
    );;
  type t = Elt.t Map.t
  type elt = Elt.t

  let empty = Map.empty
  let is_empty x = (x == empty);;
  let mem elt set = match Map.find elt set with exception Not_found -> false | _ -> true;;
  let add elt set = Map.add elt elt set
  let singleton elt = Map.singleton elt elt
  let remove = Map.remove
  let union sa sb = Map.union (fun a b -> assert(a == b); a) sa sb
  let inter sa sb = Map.inter (fun a b -> assert(a == b); a) sa sb
  let equal sa sb = Map.equal (fun _ _ -> true) sa sb
  let compare sa sb = Map.compare (fun _ _ -> 0) sa sb
  let disjoint sa sb =
    let exception Not_disjoint in
    try
      (* f may not be called if sa == sb... *)
      let m = Map.inter (fun _ _  -> raise Not_disjoint) sa sb in
      is_empty m
    with Not_disjoint -> false
  ;;
  let iter f set = Map.iter (fun _i s -> f s) set;;

  let fold f set acc =
    Map.fold (fun _ x a -> f x a) set acc

  let cardinal = Map.cardinal

  (* TODO *)
  let min_elt t = snd @@ Map.min_elt t
  let max_elt t = snd @@ Map.max_elt t
  let is_singleton = Map.is_singleton

  let to_list t = List.map snd @@ Map.to_list t
  let pp ?pp_sep pp_elt fmt x =
    Format.fprintf fmt "@[%a@]"
      (Format.pp_print_list ?pp_sep pp_elt) (to_list x)
end
