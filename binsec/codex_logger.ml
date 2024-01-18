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

let current_instr_addr : Virtual_address.t ref = ref (Virtual_address.create 0)

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
end = struct
  module AddrSet = Virtual_address.Set
  module SMap = Basic_types.String.Map
  module STbl = Basic_types.String.Htbl

  (* Per-alarm address set *)
  type counter = (AddrSet.t * AddrSet.t) SMap.t

  (* Per-phase counter. *)
  type t = { counts : counter SMap.t; cur_phase : string }

  let empty =
    { counts = SMap.empty; cur_phase = "_none_" }

  let check alarm_name x =
    let counts = x.counts |> SMap.update x.cur_phase (function
    | Some c ->
        Some (c |> SMap.update alarm_name (function
        | None -> Some AddrSet.(singleton !current_instr_addr, empty)
        | Some (checks,alarms) ->
            Some Virtual_address.Set.(add !current_instr_addr checks, alarms)
        ))
    | None ->
        Some (SMap.singleton alarm_name AddrSet.(singleton !current_instr_addr, empty)
        )
    )
    in
    {x with counts}

  let alarm alarm_name x =
    let counts = x.counts |> SMap.update x.cur_phase (function
    | Some c ->
        Some (c |> SMap.update alarm_name (function
        | None -> Some AddrSet.(empty, singleton !current_instr_addr)
        | Some (checks,alarms) ->
            Some Virtual_address.Set.(checks, add !current_instr_addr alarms)
        ))
    | None ->
        Some (SMap.singleton alarm_name AddrSet.(singleton !current_instr_addr, empty)
        )
    )
    in
    {x with counts}

  let new_phase p x = {x with cur_phase = p}

  let merge_counters c1 c2 =
    SMap.merge (fun alarm_name v1 v2 -> match v1,v2 with
    | None,None -> assert false
    | Some (c,a), None | None, Some (c,a) -> Some (c,a)
    | Some (c1,a1), Some (c2,a2) -> Some AddrSet.(union c1 c2, union a1 a2)
    ) c1 c2

  let total_alarms ~ignore ?(ignore_phase = []) ~unique x =
    if unique then
      let merged = SMap.fold (fun phase counter acc ->
        if List.mem phase ignore_phase then acc else merge_counters acc counter
      ) x.counts SMap.empty
      in
      SMap.fold (fun alarm_name (_checks,alarms) acc ->
        if List.mem alarm_name ignore then acc else acc + AddrSet.cardinal alarms
      ) merged 0
    else
      SMap.fold (fun phase counter acc ->
        if List.mem phase ignore_phase then acc else
            SMap.fold (fun alarm_name (_checks,alarms) acc ->
              if List.mem alarm_name ignore then acc else acc + AddrSet.cardinal alarms
            ) counter acc
      ) x.counts 0

  let pretty ~unique fmt x =
    let open Format in
    x.counts |> SMap.iter (fun phase counter ->
      fprintf fmt "@[<v>@.== %s ==@.@]" phase;
      counter |> SMap.iter (fun alarm_name (checks,alarms) ->
        if Virtual_address.Set.cardinal alarms > 0 then
          fprintf fmt "@[<v>-alarm count-,%s,%d,%d,[@[<hov 2>%a@]]@.@]" alarm_name
            (AddrSet.cardinal checks) (AddrSet.cardinal alarms)
            Format.(pp_print_list Virtual_address.pp ~pp_sep:pp_print_space)
            (AddrSet.elements alarms);
      )
    );
    fprintf fmt "\n\n-total alarm count-,%d\n" @@ total_alarms ~ignore:[] ~unique x

    let pretty_html ~unique fmt x =
      let open Format in
      x.counts |> SMap.iter (fun phase counter ->
        fprintf fmt "Phase: %s" phase;
        counter |> SMap.iter (fun alarm_name (checks,alarms) ->
          if Virtual_address.Set.cardinal alarms > 0 then
            fprintf fmt "<div style=\"padding: 3px; padding-left: 10px\">%s, %d, %d, %a</div>\n" alarm_name
              (AddrSet.cardinal checks) (AddrSet.cardinal alarms)
              Format.(pp_print_list Virtual_address.pp ~pp_sep:pp_print_space)
              (AddrSet.elements alarms);
        )
      );
      fprintf fmt "</br>-total alarm count-,%d" @@ total_alarms ~ignore:[] ~unique x
end

module type Binsec_logger_S = sig
  include Logger.S
  val check : string -> unit
    (** Emit a check notification, i.e. inform the log reader that an alarm may
       follow. *)

  val alarm : string -> unit
    (** Emit an alarm, i.e. a notification that the code could not be proved
       conform to the specification. *)

  val alarm_record : unit -> Alarm_record.t
  val switch_to_phase : string -> unit
end

module Binsec_logger : Binsec_logger_S = struct
  include Codex_options.Logger

  let record = ref Alarm_record.empty

  let warning ?level fmt =
    warning ?level ("[%a]@ " ^^ fmt) Virtual_address.pp !current_instr_addr
  let error fmt =
    error ("[%a]@ " ^^ fmt) Virtual_address.pp !current_instr_addr
  let fatal ?e fmt =
    fatal ?e ("[%a]@ " ^^ fmt) Virtual_address.pp !current_instr_addr
  let result fmt =
    result ("[%a]@ " ^^ fmt) Virtual_address.pp !current_instr_addr

  let check alarm_name =
    result "@[<hov 2>-check- %s@]" alarm_name;
    record := Alarm_record.check alarm_name !record

  let alarm alarm_name =
    error "@[<hov 2>-alarm- %s@]" alarm_name;
    record := Alarm_record.alarm alarm_name !record

  let alarm_record () = !record

  let switch_to_phase phase =
    record := Alarm_record.new_phase phase !record
end

module Quiet : Binsec_logger_S = struct
  let record = ref Alarm_record.empty

  include Codex_options.Logger
  let logwith x = Format.ikfprintf  (fun _fmt -> ()) Format.err_formatter x
  let warning ?level:_ =
    logwith
  let error = Binsec_logger.error
  let result = logwith

  let check alarm_name =
    result "@[<hov 2>-check- %s@]" alarm_name;
    record := Alarm_record.check alarm_name !record

  let alarm alarm_name =
    error "@[<hov 2>-alarm- %s@]" alarm_name;
    record := Alarm_record.alarm alarm_name !record

  let alarm_record () = !record

  let switch_to_phase phase =
    record := Alarm_record.new_phase phase !record
end

module Codex_logger : Codex.Codex_log.S = struct
  include Binsec_logger
  (* include Quiet *)
  let warning fmt = warning fmt
  let performance_warning fmt = result fmt
  let imprecision_warning fmt = result fmt
  let debug ?level ?category:_ fmt = debug fmt
  let fatal fmt =
    Format.kasprintf (fun x -> failwith x) fmt
  let feedback fmt = result fmt
end

(* include Quiet *)
include Binsec_logger
