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

type hook = {
  hook : 'a. 'a Operator.Alarm.t -> Tracelog.location list -> unit;
}

val register_alarm_hook :  hook -> unit
(** Register a hook to be called when a new alarm has been emitted. The hook
    will be called with an alarm and the stack of tracelog locations. *)

val emit_alarm : 'a Operator.Alarm.t -> unit
(** Call all the registered hooks, in order, on the emitted alarm. *)

val reset_alarms : unit -> unit
(**Reset the alarm logs, this should be used only for post analysis*)
