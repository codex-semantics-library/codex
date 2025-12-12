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

module PrinterClassHtml (X: Printer.PrinterClass) = struct
  
  let insert_marker fmt marker =
    Format.pp_print_as fmt 0 @@ Gui.Interface.marker_to_string marker


  class printer : Printer.extensible_printer = object(self)
    inherit X.printer as super
  

  method! pp_keyword fmt keyword =
    insert_marker fmt (Color_marker Keyword_color);
    super#pp_keyword fmt keyword;
    insert_marker fmt End_marker

  (*  the foo in struct foo. *)
  method! compname fmt compinfo =
    insert_marker fmt (Color_marker Type_color);
    super#compname fmt compinfo;
    insert_marker fmt End_marker

  (* int, long, etc. *)
  method! ikind fmt ikind =
    insert_marker fmt (Color_marker Type_color);
    super#ikind fmt ikind;
    insert_marker fmt End_marker

  method! fkind fmt fkind =
    insert_marker fmt (Color_marker Type_color);
    super#fkind fmt fkind;
    insert_marker fmt End_marker

 
 
method! vdecl fmt vi =
  let marker = if Ast_types.is_fun vi.vtype
      then Gui.Interface.Fun_color
      else Gui.Interface.Var_color in
    insert_marker fmt (Color_marker marker);
    super#vdecl fmt vi;
    insert_marker fmt End_marker

  
    
 method! annotated_stmt x fmt stmt =
    match stmt.skind with 
    | Instr instr -> 
        insert_marker fmt (Instruction stmt.sid);
        super#annotated_stmt x fmt stmt;
        insert_marker fmt End_marker
    | _ -> super#annotated_stmt x fmt stmt;
  
  method! global fmt g =
    match g with
    | GFun (fundec, loc) ->
        (* Open a fresh line and a clean block for the marker *)
        insert_marker fmt (Function_Definition fundec.svar.vname);
        super#global fmt g;
        insert_marker fmt End_marker;
        Format.pp_force_newline fmt ();

  | _ ->
      super#global fmt g;
  
end

  
end


let pp_file fmt file =

  let printer = Printer.current_printer() in
  let () = Printer.update_printer
      (module PrinterClassHtml: Printer.PrinterExtension) in
  Printer.pp_file fmt (Ast.get());
  Printer.set_printer printer
