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

val current_instr_addr : Virtual_address.t ref

module Alarm_record : sig
  type t
  val empty : t
  val check : string -> t -> t
  val alarm : string -> t -> t
  val pretty : unique:bool -> Format.formatter -> t -> unit
  val pretty_html : unique:bool -> Format.formatter -> t -> unit
  val new_phase : string -> t -> t
  val total_alarms : ignore:(string list) -> ?ignore_phase:(string list) ->
    unique:bool -> t -> int
end

module type BINSEC_LOGGER_S = sig
  include Logger.S
  val check : string -> unit
    (** Emit a check notification, i.e. inform the log reader that an alarm may
       follow. *)

  val alarm : _ Operator.Alarm.t -> unit
    (** Emit an alarm, i.e. a notification that the code could not be proved
       conform to the specification. *)

  val alarm_record : unit -> Alarm_record.t
  val switch_to_phase : string -> unit
end

include BINSEC_LOGGER_S

(** Logger to be used specifically by the libase library. *)
module Codex_logger : Codex.Codex_log.S
