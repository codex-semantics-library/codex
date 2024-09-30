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

open Frama_c_kernel

module Location = struct

  type t =
    | Transition of Cil_types.kinstr
    | Join of Cil_types.kinstr * Cil_types.kinstr
    | Return

  let pretty fmt x =
    let open Format in
    match x with
    | Transition k ->
      let l = Cil_datatype.Kinstr.loc k in
      fprintf fmt "%a : { %a }" Cil_datatype.Location.pretty l Cil_datatype.Kinstr.pretty k
    | Join (k1,k2) ->
      let l1 = Cil_datatype.Kinstr.loc k1 in
      let l2 = Cil_datatype.Kinstr.loc k2 in
      fprintf fmt "Join of { %a : %a } and { %a : %a }"
        Cil_datatype.Location.pretty l1 Cil_datatype.Kinstr.pretty k1
        Cil_datatype.Location.pretty l2 Cil_datatype.Kinstr.pretty k2

    | Return -> fprintf fmt "Return"

  let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;

  let hash x =
    match x with
    | Transition k -> Hashtbl.hash (1, Cil_datatype.(Location.hash @@ Kinstr.loc k))
    | Join (k1,k2) -> Hashtbl.hash (2, Cil_datatype.(Location.hash @@ Kinstr.loc k1), Cil_datatype.(Location.hash @@ Kinstr.loc k2))
    | Return -> Hashtbl.hash (3, x)

  let compare x y =
    match x,y with
    | Transition k1, Transition k2 -> Cil_datatype.(Location.compare (Kinstr.loc k1) (Kinstr.loc k2))
    | Transition _, _ -> -1
    | _, Transition _ -> 1
    | Join (k1x,k2x), Join (k1y,k2y) ->
      let c = Cil_datatype.(Location.compare (Kinstr.loc k1x) (Kinstr.loc k1y)) in
      if c <> 0 then c
      else Cil_datatype.(Location.compare (Kinstr.loc k1y) (Kinstr.loc k2y))
    | Join _, _ -> -1
    | _, Join _ -> 1
    | Return, Return -> 0

  let equal x y = compare x y = 0
end


let current_kinstr : Location.t option ref = ref None ;;

module StringSet = Set.Make(String) ;;
module LocMap = Map.Make(Location) ;;

let alarm_map : StringSet.t LocMap.t ref = ref LocMap.empty ;;

let alarm category =
  match !current_kinstr with
  | Some stmt ->
    begin match LocMap.find stmt !alarm_map with
      | exception Not_found ->
        alarm_map := LocMap.add stmt (StringSet.singleton category) !alarm_map
      | x ->
        alarm_map := LocMap.add stmt (StringSet.add category x) !alarm_map
    end

  | None -> assert false


let count_alarms () =
  LocMap.fold (fun _ x num -> num + StringSet.cardinal x) !alarm_map 0

let dump_alarms fmt =
  let f loc alarms =
    Printf.fprintf fmt "+ %s -> %s\n"
      (Format.asprintf "%a" Location.pretty loc)
      (Format.asprintf "%a"
        (Format.pp_print_list
          (fun fmt alarm -> Format.fprintf fmt "@[<hov 2>%s@] ;" alarm)) @@ StringSet.elements alarms)
  in
  LocMap.iter f !alarm_map
