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

(* -*- compile-command: "OCAMLRUNPARAM=\"b\" ocamlfind ocamlc common.ml terminal.mli terminal.ml tracelog.mli tracelog.ml -package unix -linkpkg && ./a.out" -*- *)

include Common;;

type 'a printf =  ('a, Format.formatter, unit) format -> 'a;;
type 'a log = 'a printf -> unit;;
type location = ..

module type S  = sig
  val error : 'a log -> unit
  val warning : 'a log -> unit
  val notice : 'a log -> unit
  val info : 'a log -> unit    
  val debug : 'a log -> unit
  val fatal : 'a log -> 'b
  val not_yet_implemented : 'a log -> unit
  val trace: 'a log -> ?loc:location -> ?pp_ret:(Format.formatter -> 'b -> unit) -> (unit -> 'b) -> 'b    
  val fatal_handler: ('a -> 'b) -> ('a -> 'b)    
end;;


(* TODO: Use standard log levels, liike syslog
   debug; debug-level messages.
   info;  informational. Those are normally not displayed.
   notice; significant
   warning; 
   error;
   critical;

   ;; traces occur on the info and debug levels.

 *)
module Level = struct
  let critical = 2            (* Always printed: the reason why we use this program. *)
  let error = 3               (* The program does not run normally. *)
  let warning = 4             (* There is possibly a problem.  *)
  let notice = 5              (* All messages above this level are displayed. *)
  let info = 6                (* Normally not displayed. *)
  let trace = info            (* Tracing information is normally not displayed. *)
  let debug = 7               (* Normally not displayed; extra verbose. *)
end;;


let current_level = ref Level.warning;; (* Print warnings but not feedback messages by default. *)

let set_verbosity_level x =
  let open Level in 
  let vb = match x with
    | `Error -> error 
    | `Warning -> warning
    | `Info -> info
    | `Notice -> notice
    | `Debug -> debug
  in current_level := vb
;;

let location_stack = ref [];;
let current_location() = match !location_stack with
  | [] -> None
  | hd::_ -> Some hd

let r_pp_location_stack = ref (fun fmt locstack ->
    match locstack with
    | [] -> ()
    | _ -> Format.fprintf fmt "<cannot print location>"
  )
;;

let set_pp_location_stack f = r_pp_location_stack := f;;

let pp_location_stack fmt loc =
  (* If we already print trace information, do not reprint the position. *)
  if !current_level >= Level.trace
  then ()
  else !r_pp_location_stack fmt loc
;;

module MakeGeneric
    (V:sig val get_verbosity_level: unit -> int end)
    (Category:sig val category:string end) = struct

  let print ?(last=false) f = f (Terminal.printf Category.category ~last)

  let string_of_log: type a. a log -> string = fun log ->
    let buf = Buffer.create 300 in
    let ppf = Format.formatter_of_buffer buf in
    log (Format.fprintf ppf);
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  ;;

  (* Not sure if this is useful. *)
  let log_of_string str : 'a log = fun m -> m (format_of_string "%s") str

  let log_concat (log1:'a log) (log2:'b log) : 'c log =
    fun (m:'c printf) -> m "%t%t" (fun fmt -> log1 (Format.fprintf fmt)) (fun fmt -> log2 (Format.fprintf fmt))
  ;;

  let print_at_level level f = 
    if V.get_verbosity_level() >= level
    then print f
  ;;

  let notice f = print_at_level Level.notice f
  let info f = print_at_level Level.info f

  let generic_error underlined_title log =
    if V.get_verbosity_level() >= Level.error
    then begin
      let colored_log fmt =
        Format.pp_open_stag fmt (Weight Bold);
        Format.pp_open_stag fmt (Color Red);
        pp_location_stack fmt !location_stack;
        Format.pp_open_stag fmt (Underline true);        
        Format.fprintf fmt underlined_title ;
        Format.pp_close_stag fmt ();
        Format.fprintf fmt ": %t" (fun fmt -> log (Format.fprintf fmt));        
        Format.pp_close_stag fmt ();
        Format.pp_close_stag fmt ()
      in
      print (fun p -> p "%t" colored_log)
    end
  ;;
  let error log = generic_error "Error" log;;

  let warning log =
    if V.get_verbosity_level() >= Level.error
    then begin
      let colored_log fmt =
        Format.pp_open_stag fmt (Color Red);
        pp_location_stack fmt !location_stack;
        Format.pp_open_stag fmt (Underline true);        
        Format.fprintf fmt "Warning";
        Format.pp_close_stag fmt ();
        Format.fprintf fmt ": %t" (fun fmt -> log (Format.fprintf fmt));        
        Format.pp_close_stag fmt ();
      in
      print (fun p -> p "%t" colored_log)
    end
  ;;

  let debug log =
    if V.get_verbosity_level() >= Level.debug
    then begin
      let colored_log fmt =
        Format.pp_open_stag fmt (Weight Faint);
        Format.pp_open_stag fmt (Color Cyan);
        Format.pp_open_stag fmt (Italic true);        
        log (Format.fprintf fmt);        
        Format.pp_close_stag fmt ();
        Format.pp_close_stag fmt ();        
        Format.pp_close_stag fmt ()
      in
      print (fun p -> p "%t" colored_log)
    end
  ;;

  (** Fail, but reuse the message in the failwith argument.  *)
  let fatal: type a. a log -> 'b =  fun l ->
    generic_error "Fatal Error" l;
    raise Fatal
  ;;

  let fatal_handler f x =
    try f x
    with
    | Fatal ->
      (* The error message is already printed by fatal. *)
      let backtrace = (Printexc.get_backtrace()) in
      print (fun p -> p "%s" backtrace);
      exit 1
    | e ->
      let exc_string = Printexc.to_string e in
      let backtrace = Printexc.get_backtrace() in
      generic_error "Uncaught exception" (fun p -> p "%s" exc_string);
      print (fun p -> p "The full backtrace is:\n%s" backtrace);
      exit 2
  ;;
  

  let not_yet_implemented log =
    fatal (log_concat (log_of_string "Not yet implemented: ") log)
  ;;

  (** - If [loc] is not None, push the loc on the stack of locations.
      - If printed: increase the nesting level.
      -

      - TODO: Add a log level; default would be info (or trace).

  *)
  let trace log ?loc ?(pp_ret=(fun fmt _ -> Format.fprintf fmt "<not printed>"))  f =
    let pop_loc = 
      match loc with
        | None -> fun () -> ()
        | Some loc ->
          location_stack := loc::!location_stack;
          fun () -> location_stack := List.tl !location_stack
    in
    (* Note: we do the try with only with if for performance purposes. *)
    if V.get_verbosity_level() >= Level.trace
    then begin try
        let with_color log = fun fmt ->
          Format.pp_open_stag fmt (Color Green);          
          log (Format.fprintf fmt);
          Format.pp_close_stag fmt ()
        in
        print ~last:false (fun p -> p "%t" @@ with_color log);
        Terminal.open_nesting ();
        let res = f() in
        let colored_log fmt =
          log (Format.fprintf fmt);
          Format.pp_close_stag fmt ()
        in
        print ~last:true (fun p -> p "%t" @@ with_color (fun p -> p "Result: %a" pp_ret res));
        pop_loc();
        Terminal.close_nesting ();    
        res
      with e ->     (* close nesting without returning, and raise with the same backtrace. *)
        (*
        print ~last:true (fun p -> p "No result (exception %s)" (Printexc.to_string e));
        pop_loc();
        Terminal.close_nesting ();
        Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
        *)
        raise e
    end
    else begin
      try
        let res = f() in
        pop_loc(); res
      with e ->
        pop_loc();
        Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
    end
  ;;

  
end;;


module Make = MakeGeneric[@inlined hint](struct let get_verbosity_level() = !current_level end) 
module MakeDebug = MakeGeneric(struct let get_verbosity_level() = Level.debug end)    


module Test() = struct
  module Logs=Make(struct let category = "test" end);;

  set_verbosity_level `Debug;;
  
  Logs.notice (fun m -> m "Test 0 message %s %a ads" "Hu" Format.pp_print_string "Ha");;
  Logs.notice (fun m -> m "Test simple message\n");;
  Logs.warning (fun m -> m "Test 1 warning with newline in middle %s %a@\nads" "Hu" Format.pp_print_string "Ha");;
  Logs.error (fun m -> m "Test 2 error with newline at the end %s %a@\n" "Hu" Format.pp_print_string "Ha");;
  (* Logs.output := false;; *)
  Logs.error (fun m -> m "Test 3 warning with newline in middle %s %a@\naaend" "Hu" Format.pp_print_string "Ha");;
  Logs.debug (fun m -> m "Test 4 debug output  %s %a@\naaend" "Hu" Format.pp_print_string "Ha");;
  try Logs.not_yet_implemented (fun p -> p "FEATURE") with _ -> ();;
  try Logs.fatal (fun m -> m "Test 5 fatal %s %a@\nad" "Hu" Format.pp_print_string "Ha") with _ -> ();;


  let my_add a b = 
    Logs.trace (fun p -> p "Evaluating %d + %d" a b) ~pp_ret:Format.pp_print_int (fun () -> a + b)
  ;;

  my_add (my_add 2 3) (my_add 4 5);; (* Flat. *)

  type expr =
    | Add of expr * expr
    | Cst of int
  let rec pp_expr fmt = function
    | Cst i -> Format.fprintf fmt "%d" i
    | Add(e1,e2) -> Format.fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2

  let rec eval' = function
    | Cst i -> i
    | Add(e1,e2) ->
      Logs.debug (fun p -> p "Before\n e1");      
      let r1 = eval e1 in 
      Logs.debug (fun p -> p "Middle");
      let r2 = eval e2 in
      Logs.debug (fun p -> p "End");
      r1 + r2
  and eval e =
    Logs.trace (fun p -> p "Evaluating @\nexpression %a" pp_expr e) ~pp_ret:Format.pp_print_int (fun () -> eval' e);;

  eval (Add (Add(Cst 2,Cst 3),Add(Cst 4,Cst 5)));;

  exception Custom;;
  let rec eval' = function
    | Cst i when i == 5 -> raise Custom
    | Cst i -> i
    | Add(e1,e2) ->
      Logs.debug (fun p -> p "Before\n e1");      
      let r1 = eval e1 in 
      Logs.debug (fun p -> p "Middle");
      let r2 = eval e2 in
      Logs.debug (fun p -> p "End");
      r1 + r2
  and eval e =
    Logs.trace (fun p -> p "Evaluating @\nexpression %a" pp_expr e) ~pp_ret:Format.pp_print_int (fun () -> eval' e);;

  try eval (Add (Add(Cst 2,Cst 3),Add(Cst 4,Cst 5)))
  with Custom -> 3;;


  
end;;

(* module M = Test();; *)
