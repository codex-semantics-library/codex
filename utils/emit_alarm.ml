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

module Log = Tracelog.Make (struct
  let category = "Emit_alarm"
end)

type hook = {
  hook : 'a. 'a Operator.Alarm.t -> Tracelog.location list -> unit;
}

(*we use explicit types to avoid conflict with Stdlib.List when opening Emit_alarm*)

let register_alarm_hooks = ref []

let register_alarm_hook =
 fun f -> register_alarm_hooks := f :: !register_alarm_hooks

let rec rev_iter alarm loc = function
  | [] -> ()
  | hook :: q ->
      rev_iter alarm loc q;
      hook.hook alarm loc

let emit_alarm alarm =
  Log.error (fun p -> p "Alarm: %s" (Operator.Alarm.show alarm));
  let loc_stack = Tracelog.current_location_stack () in
  rev_iter alarm loc_stack !register_alarm_hooks

let reset_alarms () = register_alarm_hooks := []
