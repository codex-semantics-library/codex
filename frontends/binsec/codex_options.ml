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

(* Helper functions to parse the command line.
   Note: must be called after the types and image are loaded. *)

(** Parses a type from a string *)
let type_of_string = Types.Parse_ctypes.type_of_string

(* Parse an address or a symbol. *)
let address_of_string s =
  match s.[0] with
  | '0' -> Z.of_string s
  | _ -> begin
      match Loader_utils.address_of_symbol_by_name ~name:s @@ Kernel_functions.get_img () with
      | None -> failwith ("Unknown address `" ^ s ^ "'")
      | Some x -> Z.of_int x
    end
;;

let directive_of_string s =
  match s with
  | "stop" -> `stop
  | "nop" -> `nop
  | "explore" -> `explore
  | _ ->
    try
      (* Codex_log.feedback "Scanning |%s|" s; *)
      let typ =  Scanf.sscanf s "return_unknown(%s@)" type_of_string in
      `return_unknown(typ)
    with Scanf.Scan_failure _ ->
    try
      (* Codex_log.feedback "Scanning |%s|" s; *)
      let address =  Scanf.sscanf s "skip_to(%s@)" address_of_string in
      `skip_to(Virtual_address.create (Z.to_int address))
    with Scanf.Scan_failure _ -> failwith ("Unknown directive: " ^ s)

;;

include Cli.Make (struct
  let name = "codex"
  let shortname = "codex"
end)

module ApplicationFile = struct
  include Builder.String(struct
    let name = "app-file"
    let default = ""
    let doc = "Path to an application ELF"
  end)
end

module TypeConfigurationFile = struct
  include Builder.String(struct
    let name = "type-file"
    let default = ""
    let doc = "Path to the file containing the type definitions"
  end)

end

module X86Types = struct
  include Builder.String (struct
    let name = "x86-types"
    let default = "rr"
    let doc = "Version of the C types for x86 OS"
  end)
end

module UseShape = struct
  include Builder.No (struct
    let name = "use-shape"
    let doc = "Set to not use shape"
  end)
end

module NbTasks = struct
  include Builder.Integer (struct
    let name = "nb-tasks"
    let doc = "Number of tasks (required)"
    let default = 0
  end)
end

module AnalyzeKernel = struct
  include Builder.False (struct
    let name = "analyze-kernel"
    let doc = "Whether to analyze a kernel (tailored, more complex analysis)"
  end)
end

module DynThreads = struct
  include Builder.No (struct
    let name = "dyn-threads"
    let doc = "Analyzed kernel features dynamic thread creation"
  end)
end

(*
module FnArgs = struct
  include Builder.String_list (struct
    let name = "fn-args"
    let doc = "Argument types to put in the stack at the initial state"
  end)
  let get() = get() |> List.map type_of_string
end
*)

module GlobalsTypes = struct
  include Builder.String_list (struct
      let name = "globals-types"
      let doc = "Assignment of globals to types. It is a coma-separated list of addr=typ, where address can be symbols or hexa."
    end)
  let get() = get() |> List.map (fun s ->
      let addr,typ = match String.split_on_char '=' s with
        | [a;b] -> String.trim a,String.trim b
        | _ -> failwith ("Global spec should be of the form addr=typ; got " ^ s)
      in
      let addr = address_of_string addr in
      let typ = type_of_string typ in
      (addr,typ)
    )
end

module Hooks = struct
  include Builder.String_list (struct
      let name = "hooks"
      let doc = "Hooks at certain addresses (which can be symbols or hexa) to directives, of the form addr=directive, where directive is stop|explore|skip. stop stops here, explore means do meet over all paths for the function from this point, and skip means immediately return (must be at function entry)"
    end)
  let get() = get() |> List.map (fun s ->
      let addr,directive = match String.split_on_char '=' s with
        | [a;b] -> String.trim a,String.trim b
        | _ -> failwith ("Global spec should be of the form addr=directive; got " ^ s)
      in
      let addr = Virtual_address.create @@ Z.to_int @@ address_of_string addr in
      let directive = directive_of_string directive in
      (addr,directive)
    )
end


module Focusing = struct
  include Builder.No (struct
    let name = "focusing"
    let doc = "Activate focusing"
  end)
end

module Output_Html = struct
  include Builder.String_option(struct
    let name = "output-html"
    let doc = "Output the results in an html file"
  end)
end

module UseLoopDomain = struct
  include Builder.False (struct
    let name = "use-loop-domain"
    let doc = "Activate inductive loop domain"
  end)
end

module VariableDisplay = Builder.Variant_choice(struct
  type t = Domains.Term_domain.pretty_terms
  let name = "variable-display"
  let doc = "How codex should display variable information. \
          \ One of 'value' (only print inferred value, default)\
          \ 'symbolic' (print associated symbolic term)\
          \ 'both' (print both value and symbolic term)\
          \ 'relation' (print value, symbolic term, and union-find relation associated with this term)."


  open Domains.Term_domain
  let to_string = function
    | Value -> "value"
    | Both -> "both"
    | Symbolic -> "symbolic"
    | Relation -> "relation"
  let of_string = function
    | "value" -> Value
    | "both" -> Both
    | "symbolic" -> Symbolic
    | "relation" -> Relation
    | _ -> assert false

  let choices = [ "value"; "both"; "symbolic"; "relation" ]
  let default = Value
end)

module MMIOs = struct
  include Builder.String_list (struct
      let name = "mmios"
      let doc = "sets memory blocks of the form addr+size as readable but unknown content"
    end)
  let get() = get() |> List.map (fun s ->
      let addr,size = match String.split_on_char '+' s with
        | [a;b] -> String.trim a,String.trim b
        | _ -> failwith ("Global spec should be of the form addr+size; got " ^ s)
      in
      let size = int_of_string size in
      let addr = int_of_string addr in
      (addr,size)
    )
end
(*
module GlobalSymbols = struct
  include Builder.String_list (struct
      let name = "global-symbols"
      let doc = "Declaration of global symbols and their types. It is a coma-separated list of sym=typ, where symbols are identifiers"
    end)
  let get() = get() |> List.map (fun s ->
      let symb,typ = match String.split_on_char '=' s with
        | [a;b] -> String.trim a,String.trim b
        | _ -> failwith ("Global spec should be of the form symb=typ; got " ^ s)
      in
      let typ = type_of_string typ in
      (symb,typ)
    )
end
*)
module Location = struct

  (* Absolute location representing an address. *)
  type address_location = int64 Syntax_tree.Location_identifier.t

  module L = struct
    include (Tracelog:sig type location = Tracelog.location = .. end)
    type location +=
      | Function of string
      | Instruction of { locid: address_location; address:int64 }
      | Dba_instr of { locid: Dba.Instr.t Syntax_tree.Location_identifier.t; dba_instr: Dba.Instr.t } (* Instr and position in the dhunk. *)
      | Dba_expr of { locid: Dba.Expr.t Syntax_tree.Location_identifier.t; dba_expr: Dba.Expr.t }     (* Expr and position in the expr. *)
    type t = location

  end

  include L

  let pp_loc fmt location = match location with
    | Function f -> Format.fprintf fmt "In function `%s':@ " f
    | Instruction {address} -> Format.fprintf fmt "In function `%08Lx':@ " address
    | Dba_instr{dba_instr} ->
      Format.fprintf fmt "In Dba_instr `%s':@ "
        (dba_instr |>
         Binsec2syntax_tree.instr_of_binsec_instr |>
         Syntax_tree.string_of_expr)
    | Dba_expr {dba_expr} ->
      Format.fprintf fmt "In Dba_expr `%s':@ "
        (dba_expr |>
         Binsec2syntax_tree.expr_of_binsec_expr |>
         Syntax_tree.string_of_expr)
    | _ -> assert false

  let pp_loc_stack fmt stack = match stack with
    | [] -> ()
    | _ -> Format.fprintf fmt "@[<hv>"; List.iter (pp_loc fmt) (List.rev stack); Format.fprintf fmt "@]"

  let () = Tracelog.set_pp_location_stack pp_loc_stack
end
