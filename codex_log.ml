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

type category = string
let register_category x = x

module type S = sig
  val result:  ('a, Format.formatter, unit) format -> 'a  
  val warning:  ('a, Format.formatter, unit) format -> 'a
  val error:  ('a, Format.formatter, unit) format -> 'a
  val feedback:  ('a, Format.formatter, unit) format -> 'a  
  val performance_warning:  ('a, Format.formatter, unit) format -> 'a
  val imprecision_warning:  ('a, Format.formatter, unit) format -> 'a    
  (* val debug: ?level:int -> ?category:string -> ('a, Format.formatter, unit) format -> 'a *)
  val fatal: ('a, Format.formatter, unit, 'b) format4 -> 'a

  (* Used to count the number of checks that are done, used before an
     alarm call. Deprecated, try not to use. *)
  val check: string -> unit
  
  (* Emit an alarm directly from a domain. Deprecated, try not to use. *)
  val alarm: string -> unit
end


module Default:S = struct
  let logwith x = Format.kfprintf (fun fmt ->
      Format.pp_print_newline fmt ();
      Format.pp_print_flush fmt ()
    ) Format.err_formatter x
  ;;

  let warning = logwith
  let error = logwith
  let result = logwith    
  let alarm str = logwith "%s" str
  let check str = logwith "%s" str   
  let feedback = logwith
  let performance_warning = logwith
  let imprecision_warning = logwith
  let debug ?level ?category = let _ = level and _ = category in logwith
  let fatal str = Format.kfprintf (fun fmt ->
      Format.pp_print_newline fmt ();
      Format.pp_print_flush fmt ();
      assert false)
      Format.err_formatter str ;;
end

let r = ref (module Default:S);;
let register (module X:S) = r:= (module X);;

(* Does not print anything (except fatal errors). *)
module Null:S = struct
  let logwith x = Format.ikfprintf  (fun fmt ->
      Format.pp_print_newline fmt ();
      Format.pp_print_flush fmt ()
    ) Format.err_formatter x
  ;;

  let warning = logwith
  let error = logwith
  let result = logwith    
  let alarm str = logwith "%s" str
  let check str = logwith "%s" str
  let feedback = logwith
  let performance_warning = logwith
  let imprecision_warning = logwith
  let debug ?level ?category = let _ = level and _ = category in logwith    
  let fatal str = Format.kfprintf (fun fmt ->
      Format.pp_print_newline fmt ();
      Format.pp_print_flush fmt ();
      assert false)
      Format.err_formatter str ;;
end


module Dynamic:S = struct
  (* let register_category = let module X:S = (val !r) in X.register_category;; *)
  let warning fmt = let module X:S = (val !r) in X.warning fmt;;
  let error fmt = let module X:S = (val !r) in X.error fmt;;
  let result fmt = let module X:S = (val !r) in X.result fmt;;  
  let alarm fmt = let module X:S = (val !r) in X.alarm fmt;;
  let check fmt = let module X:S = (val !r) in X.check fmt;;  
  let performance_warning fmt = let module X:S = (val !r) in X.performance_warning fmt;;
  let imprecision_warning fmt = let module X:S = (val !r) in X.imprecision_warning fmt;;
  (* let debug ?level ?category fmt = *)
  (*   let _ = level and _ = category in *)
  (*   let module X:S = (val !r) in X.debug ?category fmt;; *)
  let fatal fmt = let module X:S = (val !r) in X.fatal fmt;;
  let feedback fmt = let module X:S = (val !r) in X.feedback fmt;;  
end


module Tracelog_Instance = Tracelog.Make(struct let category = "codex" end);;


let alarm_count = ref 0;;

module Tracelog_Log:S = struct
  let result fmt = Format.kasprintf (fun str -> Tracelog_Instance.notice (fun m -> m "%s" str)) fmt;;  
  let feedback fmt = Format.kasprintf (fun str -> Tracelog_Instance.info (fun m -> m "%s" str)) fmt;;
  let error fmt = Format.kasprintf (fun str -> Tracelog_Instance.error (fun m -> m "%s" str)) fmt;;
  let warning fmt = Format.kasprintf (fun str -> Tracelog_Instance.warning (fun m -> m "%s" str)) fmt;;
  let performance_warning = warning
  let imprecision_warning = warning
  let debug ?level ?category fmt =
    Format.kasprintf (fun str -> Tracelog_Instance.debug (fun m -> m "%s" str)) fmt;;
  let fatal fmt = Format.kasprintf (fun str -> Tracelog_Instance.fatal (fun m -> m "%s" str)) fmt;;
  let check category = feedback "Check %s" category
  let alarm category = incr alarm_count; error "Alarm %s" category
end


module Used = Dynamic
(* module Used = Default *)
(* module Used = Tracelog_Log *)
(* module Used = Null *)

include Used

(* Make cleaner printers, tied to a category. *)
module Make(P:sig
    val category:string         (* Default category. *)
    val debug:bool              (* If true, print debug information for this category. *)
  end) = struct
  include Used
  (* let debug ?category str = *)
  (*   let category = match category with *)
  (*     | Some category -> category *)
  (*     | None -> P.category *)
  (*   in *)
  (*   if P.debug then debug ~category str else Format.ifprintf Format.err_formatter str;; *)
end
