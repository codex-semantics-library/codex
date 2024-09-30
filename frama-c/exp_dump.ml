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

module Cil_types = Frama_c_kernel.Cil_types
module Filepath = Frama_c_kernel.Filepath
module Visitor = Frama_c_kernel.Visitor
module Printer = Frama_c_kernel.Printer
module Cil = Frama_c_kernel.Cil
module Ast = Frama_c_kernel.Ast

let exp_dump ~should_print pp_exp_value outc =
  let module Local = struct

    (* The GNU format of locations, used by GCC and Clang.
       See https://www.gnu.org/prep/standards/standards.html#Errors.

       The format is the following for a span:
       - If an error is spread over several files (rare):
         file1:line1.column1-file2:line2.column2: message
       - If an error is spread over several lines (uncommon):
         sourcefile:line1.column1-line2.column2: message
       - Else (common): sourcefile:line1.column1-column2: message

       For a single location, it is just:
       sourcefile:lineno:column: (or sourcefile:lineno.column:). *)
    let pp_location (begin_,end_) =
      if begin_.Filepath.pos_path == end_.Filepath.pos_path
      then
        if begin_.Filepath.pos_lnum == end_.Filepath.pos_lnum
        then
          Printf.sprintf "%s:%d.%d-%d"
            (Filepath.Normalized.to_pretty_string begin_.Filepath.pos_path)
            begin_.Filepath.pos_lnum
            (begin_.Filepath.pos_cnum - begin_.Filepath.pos_bol)
            (end_.Filepath.pos_cnum - begin_.Filepath.pos_bol)        
        else          
          Printf.sprintf "%s:%d.%d-%d.%d"
            (Filepath.Normalized.to_pretty_string begin_.Filepath.pos_path)
            begin_.Filepath.pos_lnum
            (begin_.Filepath.pos_cnum - begin_.Filepath.pos_bol)
            end_.Filepath.pos_lnum
            (end_.Filepath.pos_cnum - begin_.Filepath.pos_bol)        
      else
        (* So rare that we just print the beginning location. *)
        Printf.sprintf "%s:%d:%d"
        (Filepath.Normalized.to_pretty_string begin_.Filepath.pos_path)
        begin_.Filepath.pos_lnum
        (begin_.Filepath.pos_cnum - begin_.Filepath.pos_bol)
    ;;

    class my_visitor pp_exp_value fmt =
      object(self)
        inherit Visitor.frama_c_inplace

        val print_exp = fun ki -> fun outc exp ->
          if should_print (ki,exp) then begin

            let string = Printf.sprintf "%s: `%s' -> "
                (pp_location exp.Cil_types.eloc) (Format.asprintf "%a@?" Printer.pp_exp exp)
            in

            (* let posa = pos_out outc in *)
            (* Printf.fprintf outc "%a: `%s' -> " *)
            (*   pp_location exp.Cil_types.eloc *)
            (*   (Format.asprintf "%a@?" Printer.pp_exp exp); *)
            (* let posb = pos_out outc in *)
            (* let len = posb - posa in *)
            let len = String.length string in
            Printf.fprintf outc "%s" string;

            (* The normal way to avoid line reaks should be to use
               pretty format with a large margin, but for some reason
               this does not work. However, this is a good
               work-around. *)

            let fmt = Format.formatter_of_out_channel outc in

            let open Format in
            let funs = Format.pp_get_formatter_out_functions fmt () in
            Format.pp_set_formatter_out_functions fmt
              {funs with 
               out_newline = (fun () ->
                   funs.out_string "\n" 0 1; funs.out_string (String.make len ' ') 0 len)
              };
            Printf.fprintf outc "%a\n%!" pp_exp_value (len,ki,exp)
          end

        method! vexpr exp =
          let open Cil_types in
          let ki = match self#current_stmt with None -> Kglobal | Some s -> Kstmt s in
          if not (Cil.isConstant exp ||
                  match exp.enode with (* Do not print adresses of stack variables either. *)
                  | AddrOf (Var vi, off) | StartOf (Var vi, off)
                    when Cil.isConstantOffset off -> true
                  | _ -> false
                 ) then
            (print_exp ki fmt exp;
             Cil.DoChildren)
          else
            Cil.SkipChildren
      end
  end in

  (* We use Frama-C file so that locations are ordered. *)    
  (* Callgraph.Uses.iter_in_rev_order (fun kf -> *)
  (*     ignore(Visitor.visitFramacKf (new my_visitor pp_exp_value) kf) *)
  (* Format.fprintf fmt "-*- mode:compilation -*-\n@[<v>"; *)
  Visitor.visitFramacFile (new Local.my_visitor pp_exp_value outc) (Ast.get());
  (* Format.fprintf fmt "@]" *)
;;

(* let compare_to_value exp_to_term ctx = *)
(*   let pp_exp_value = print_value_of_exp_compared_to_value exp_to_term ctx in *)
(*   match Codex_options.CompareWithValue.get() with *)
(*   | "" -> () *)
(*   | file -> *)
(*     let outc = open_out file in *)
(*     let fmt = Format.formatter_of_out_channel outc in *)
(*     exp_dump pp_exp_value fmt; *)
(*     Format.pp_print_flush fmt (); *)
(*     close_out outc *)
(* ;; *)

