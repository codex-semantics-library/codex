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

open Frama_c_kernel

module Location = struct

  (* TODO: Replace kinstr with instruction. *)
  type t =
    | Transition of Cil_types.kinstr
    | Join of Cil_types.kinstr * Cil_types.kinstr
    | Return

  (* Cil_datatype.Kinstr.loc was removed in recent Frama-C. *)
  let kinstr_loc = let open Cil_types in function
    | Kstmt st -> Cil_datatype.Stmt.loc st
    | Kglobal -> assert false

  
  let pretty fmt x =
    let open Format in
    match x with
    | Transition k ->
      let l = kinstr_loc k in
      fprintf fmt "%a : { %a }" Cil_datatype.Location.pretty l Cil_datatype.Kinstr.pretty k
    | Join (k1,k2) ->
      let l1 = kinstr_loc k1 in
      let l2 = kinstr_loc k2 in
      fprintf fmt "Join of { %a : %a } and { %a : %a }"
        Cil_datatype.Location.pretty l1 Cil_datatype.Kinstr.pretty k1
        Cil_datatype.Location.pretty l2 Cil_datatype.Kinstr.pretty k2

    | Return -> fprintf fmt "Return"

  let hash x =
    match x with
    | Transition k -> Hashtbl.hash (1, Cil_datatype.(Location.hash @@ kinstr_loc k))
    | Join (k1,k2) -> Hashtbl.hash (2, Cil_datatype.(Location.hash @@ kinstr_loc k1), Cil_datatype.(Location.hash @@ kinstr_loc k2))
    | Return -> Hashtbl.hash (3, x)

  let compare x y =
    match x,y with
    | Transition k1, Transition k2 -> Cil_datatype.(Location.compare (kinstr_loc k1) (kinstr_loc k2))
    | Transition _, _ -> -1
    | _, Transition _ -> 1
    | Join (k1x,k2x), Join (k1y,k2y) ->
      let c = Cil_datatype.(Location.compare (kinstr_loc k1x) (kinstr_loc k1y)) in
      if c <> 0 then c
      else Cil_datatype.(Location.compare (kinstr_loc k1y) (kinstr_loc k2y))
    | Join _, _ -> -1
    | _, Join _ -> 1
    | Return, Return -> 0

  let equal x y = compare x y = 0
end

module StringSet = Set.Make(String) ;;
module LocMap = Map.Make(Location) ;;

(** An alarm is located in a function, and possibly an instruction, and if possible an expression. *)
(* Todo: refine this notion of location to include an expression if possible *)
type alarm_location =
  { expression: Cil_types.exp option;
    instruction: Location.t;
    func: Kernel_function.t
  }

(** We traverse the location stack to find the current expression (if
    it exists), instruction and function.  Normally we should always
    have a function, then the instruction, and the a list of
    expression. On top of that we have the call stack. *)
let alarm_location_of_loc_stack l =
  let open Codex_options.Location in
  let find_function (expression,instruction)= function
    | (Function func)::l ->  {expression;instruction;func}
    | [] -> assert false
    | _ -> assert false
  in
  let rec find_instruction expression = function
    | (Kinstr i)::l -> find_function (expression,Transition i) l
    | FunctionReturn::l -> find_function (expression,Return) l
    | (Expression _)::l -> find_instruction expression l
    | _ -> assert false
  in
  let find_expression = function
    | (Expression e)::l -> find_instruction (Some e) l
    | _ -> find_instruction None l
  in
  find_expression l
;;

let alarm_map : StringSet.t LocMap.t ref = ref LocMap.empty ;;

let myalarm category loc_stack =
  let category = Operator.Alarm.show category in
  let current_loc = alarm_location_of_loc_stack loc_stack in
  let stmt = current_loc.instruction in
  match LocMap.find stmt !alarm_map with
  | exception Not_found ->
    alarm_map := LocMap.add stmt (StringSet.singleton category) !alarm_map
  | x ->
    alarm_map := LocMap.add stmt (StringSet.add category x) !alarm_map

let () = Emit_alarm.register_alarm_hook {hook = myalarm}

let count_alarms () =
  LocMap.fold (fun _ x num -> num + StringSet.cardinal x) !alarm_map 0

let reset_alarms () = 
  alarm_map := LocMap.empty

let dump_alarms fmt =
  let f loc alarms =
    Printf.fprintf fmt "+ %s -> %s\n"
      (Format.asprintf "%a" Location.pretty loc)
      (Format.asprintf "%a"
        (Format.pp_print_list
          (fun fmt alarm -> Format.fprintf fmt "@[<hov 2>%s@] ;" alarm)) @@ StringSet.elements alarms)
  in
  LocMap.iter f !alarm_map
