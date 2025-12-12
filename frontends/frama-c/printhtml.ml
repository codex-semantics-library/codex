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
(* As the printer reconstructs some expressions (e.g. ++), the
   displayed expressions may not match the true expressions of the
   program (there are expressions that are created, and the nest level
   do not mach). Moreover the eid are not always reliable.

   So, we first hook the printer to construct a reliable hash of all
   the printed expressions of the program (and their level of nest);
   we fill this hash afterwards. *)

(* Note that comparison is done by ids; as ids follow the level of
   nest, this allows expinfos to be displayed in the correct order
   (which is why we use a map here) *)
type exp_or_lval =
  | Exp of Cil_types.kinstr * Cil_types.exp
  | Lval of Cil_types.kinstr * Cil_types.lval
  | Function of string


module ExpOrLvalWithId = struct

  module Type = struct
    type t = (exp_or_lval * int)
    let compare (_,a) (_,b) = compare (a:int) b
    let hash (_,a) = a
    let equal (_,a) (_,b) = a == b
    let pretty (a,_) = assert false
  end

  module Map = Map.Make(Type)

end

module PrintHtmlCode = struct


  (* These references are really ugly; however this module should only
     be used through pp_file below, which makes sure that they are
     correctly used. *)
  let exp_unique_id = ref 0;;
  let current_level = ref 0;;
  let cur_stmt = ref Cil.invalidStmt;;
  
  let printed_exp_hash = ref (Obj.magic 0)

  let add_exp exp id nesting_level =
    let expmap =
      try Cil_datatype.Stmt.Hashtbl.find !printed_exp_hash !cur_stmt
      with Not_found -> ExpOrLvalWithId.Map.empty
    in
    let expmap = ExpOrLvalWithId.Map.add (exp,id) nesting_level expmap in
    Cil_datatype.Stmt.Hashtbl.replace !printed_exp_hash !cur_stmt expmap;
  ;;

 let add_lval exp id nesting_level =
    let expmap =
      try Cil_datatype.Stmt.Hashtbl.find !printed_exp_hash !cur_stmt
      with Not_found -> ExpOrLvalWithId.Map.empty
    in
    let expmap = ExpOrLvalWithId.Map.add (exp,id) nesting_level expmap in
    Cil_datatype.Stmt.Hashtbl.replace !printed_exp_hash !cur_stmt expmap;
  ;;

  let pp_anchor fmt vi =
    let open Cil_types in
    Format.pp_print_as fmt 0 (Printf.sprintf "<a name=\"vi-global-%s\"></a>" vi.vname)   
  
  (* HTML code printer; escape problematic character codes <,>,and &;
     insert HTML tags invisible to the pretty printer. *)
  module PrinterClassHtml (X: Printer.PrinterClass) = struct

    open Cil_types;;

    class printer : Printer.extensible_printer = object(self)
      inherit X.printer as super
      
      (* Note: We use pp_print_as so that the markup text is not seen by
         the pretty printer. *)

      method! exp fmt exp =
        let kinstr = match self#current_stmt with None -> Kglobal | Some s -> Kstmt s in
        Format.pp_print_as fmt 0 
          (Format.sprintf "<span data-eid=\"%d\" class=\"expnest expnest%d\">"
             !exp_unique_id !current_level);
        add_exp (Exp (kinstr,exp)) !exp_unique_id !current_level;
        incr exp_unique_id;
        (match exp.enode with
         | AddrOf lv | StartOf lv -> Format.pp_print_as fmt 1 "&amp;"; self#lval fmt lv
         | Lval lval -> super#exp fmt exp;
         | _ -> incr current_level; super#exp fmt exp; decr current_level);
        Format.pp_print_as fmt 0 "</span>";
        
        (* MAYBE: do not increment level for Lval nodes, as this is already done by lval. *)

      method! lval fmt lval =
        let kinstr = match self#current_stmt with None -> Kglobal | Some s -> Kstmt s in
        Format.pp_print_as fmt 0 (Printf.sprintf "<span data-eid=\"%d\" class=\"expnest expnest%d\">" !exp_unique_id !current_level);
        add_exp (Lval(kinstr,lval)) !exp_unique_id !current_level;
        incr exp_unique_id;        
        incr current_level;
        super#lval fmt lval;
        decr current_level;
        Format.pp_print_as fmt 0 "</span>"

      (* Note: stmt does not work, we have to use annotated_stmt instead. *)
      method! annotated_stmt x fmt stmt =
        cur_stmt := stmt;
        super#annotated_stmt x fmt stmt
      
      method! instr fmt instr =
        Format.pp_print_as fmt 0 "<span class=\"statement\">";
        super#instr fmt instr;
        Format.pp_print_as fmt 0 "</span>";

      method! pp_keyword fmt keyword =
        Format.pp_print_as fmt 0 "<span class=\"keyword\">";
        super#pp_keyword fmt keyword;
        Format.pp_print_as fmt 0 "</span>";

      (*  the foo in struct foo. *)
      method! compname fmt compinfo =
        Format.pp_print_as fmt 0 "<span class=\"type\">";
        super#compname fmt compinfo;
        Format.pp_print_as fmt 0 "</span>";

        
        
      method! typ ?fundecl f fmt typ =
        let f = match f with
          | None -> None
          | Some f -> Some(fun fmt ->
            Format.pp_print_as fmt 0 "<span class=\"vardecl\">";
            f fmt;
            Format.pp_print_as fmt 0 "</span>")
        in
        Format.pp_print_as fmt 0 "<span class=\"type\">";
        super#typ ?fundecl f fmt typ;
        Format.pp_print_as fmt 0 "</span>";

        
      method! binop fmt = function
        | Shiftlt -> Format.pp_print_as fmt 2 "&lt;&lt;"
        | Shiftrt -> Format.pp_print_as fmt 2 "&gt;&gt;"
        | Lt -> Format.pp_print_as fmt 1 "&lt;"                    
        | Gt -> Format.pp_print_as fmt 1 "&gt;"                         
        | Le -> Format.pp_print_as fmt 2 "&lt;="                    
        | Ge -> Format.pp_print_as fmt 2 "&gt;="
        | LAnd -> Format.pp_print_as fmt 2 "&amp;&amp;"
        | x -> super#binop fmt x

      method! ikind fmt = function
        | IBool -> Format.pp_print_as fmt 1 "<span class=\"type\">_Bool</span>"
        | IChar -> Format.pp_print_as fmt 1 "<span class=\"type\">char</span>"
        | ISChar -> Format.pp_print_as fmt 1 "<span class=\"type\">signed char</span>"
        | IUChar -> Format.pp_print_as fmt 1 "<span class=\"type\">unsigned char</span>"
        | IInt -> Format.pp_print_as fmt 1 "<span class=\"type\">int</span>"
        | IUInt -> Format.pp_print_as fmt 1 "<span class=\"type\">unsigned int</span>"
        | IShort -> Format.pp_print_as fmt 1 "<span class=\"type\">short</span>"
        | IUShort -> Format.pp_print_as fmt 1 "<span class=\"type\">unsigned short</span>"
        | ILong -> Format.pp_print_as fmt 1 "<span class=\"type\">long</span>"
        | IULong -> Format.pp_print_as fmt 1 "<span class=\"type\">unsigned long</span>"
        | ILongLong -> Format.pp_print_as fmt 1 "<span class=\"type\">long long</span>"
        | IULongLong -> Format.pp_print_as fmt 1 "<span class=\"type\">unsigned long long</span>"

        method! fkind fmt = function
        | FFloat -> Format.pp_print_as fmt 1 "<span class=\"type\">float</span>"
        | FDouble -> Format.pp_print_as fmt 1 "<span class=\"type\">double</span>"
        | FLongDouble -> Format.pp_print_as fmt 1 "<span class=\"type\">long double</span>"

      (* Just add a link to jump to. *)
      method! varinfo fmt vi =
        if vi.vglob then begin
          let classname = if Ast_types.is_fun vi.vtype then "function-name" else "vardecl" in
          Format.pp_print_as fmt 0 (Printf.sprintf "<a data-eid=\"%d\" href=\"#vi-global-%s\" class=\"expnest expnest%d %s\">" !exp_unique_id vi.vname !current_level classname);
          let name = (Format.asprintf "%s" vi.vname) in
          add_exp (Function name) !exp_unique_id !current_level;
          incr exp_unique_id;
          incr current_level;
          super#varinfo fmt vi;
          decr current_level;
          Format.pp_print_as fmt 0 "</a>"
        end
        else
          super#varinfo fmt vi

      (* We link to the declaration, unless there is a definition
         (with more content), in which case we link there. *)
      method! vdecl fmt vi =
        if vi.vglob then begin
          (match Globals.Functions.get vi with
           | exception Not_found -> 
             (* No kernel_function: link to declaration. *)
             pp_anchor fmt vi
           | kf when Kernel_function.has_definition kf -> ()
             (* There is a definition: no anchor, as we link to the definition. *)
           | kf ->              (* No definition: put an anchor here. *)
             pp_anchor fmt vi
          );
          super#vdecl fmt vi
        end
        else super#vdecl fmt vi

      method! global fmt global =
        (match global with
         (* We link to the definition if a definition exists.  Note:
            there is a fundec method, but it does not seem to be
            called. Maybe it is a bug in Frama-C. *)
         | GFun(fundec,loc) -> pp_anchor fmt fundec.svar
         | _ -> ());
        super#global fmt global

    end
  end

  let pp_file fmt file =
    
    printed_exp_hash := Cil_datatype.Stmt.Hashtbl.create 117;
    
    let printer = Printer.current_printer() in
    let () = Printer.update_printer
        (module PrinterClassHtml: Printer.PrinterExtension) in
    Printer.pp_file fmt (Ast.get());
    Printer.set_printer printer;
    !printed_exp_hash
    
end

(* https://material.google.com/style/color.html#color-color-palette *)
(* The jquery code is used only to display the corresponding eidinfo node. *)
let prefixe = {prefixe|
<!DOCTYPE HTML>
<html lang="en">

<head>
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
  <script type='text/javascript'>
    clicked = [];
    
    $(document).ready(function () {
 
      $(".expnest").mouseup(function () {
        $(this).css({"background-color": $(this).css("background-color"), "color": "black"});
        $("#eidinfo" + $(this).attr("data-eid")).show();
        clicked.push($(this));
      });
      
      $(".expnest").hover(
          function () {
            if (clicked.length == 0) {
                $(this).css({"background-color": $(this).css("background-color"), "color": "black"});
                $("#eidinfo" + $(this).attr("data-eid")).show();
            }
          }, 
          function () {
            if (clicked.length == 0) {
                $(this).removeAttr('style');
                $("#eidinfo" + $(this).attr("data-eid")).hide();
            }
      });
      
    $(".code-container").mousedown(function (e) {
        if ((e.offsetX <= e.target.clientWidth && e.offsetY <= e.target.clientHeight) || 
            (e.target.clientWidth == 0 && e.target.clientHeight == 0)) {
          clicked.forEach(c => {c.removeAttr('style'); $("#eidinfo" + c.attr("data-eid")).hide();});
          clicked = [];
        }
    });
    
    });
  </script>
  <style>

    .def {
      display: none;
    }

    .expnest:hover+.expinfo>.def {
      display: inline-block;
    }

    .expinfo {
      border: 1px solid black;
      border-radius: 12px;
    }

    .expinfo>.eidinfo {
      display: none;
    }

    .expnest0:hover {
      background-color: #DDFFD6;
      color: #000000;
    }

    .expnest1:hover {
      background-color: #FFC2C2;
      color: #000000;
    }

    .expnest2:hover {
      background-color: #ADF8FF;
      color: #000000;
    }

    .expnest3:hover {
      background-color: #FEFFD6;
      color: #000000;
    }

    .expnest4:hover {
      background-color: #DCD6FF;
      color: #000000;
    }

    .expnest5:hover {
      background-color: #FFE3C2;
      color: #000000;
    }

    .expnest6:hover {
      background-color: #C2D9FF;
      color: #000000;
    }

    .expnest7:hover {
      background-color: #F9C8E0;
      color: #000000;
    }

    .expnest8:hover {
      background-color: #DCD6FF;
      color: #000000;
    }

    .expnest9:hover {
      background-color: #BDB2FF;
      color: #000000;
    }

    .expnest10:hover {
      background-color: #FFECD6;
      color: #000000;
    }

    .expnest11:hover {
      background-color: #FFD6A5;
      color: #000000;
    }

    .expnest12:hover {
      background-color: #D6E6FF;
      color: #000000;
    }

    .expnest13:hover {
      background-color: #A0C4FF;
      color: #000000;
    }

    .expnest14:hover {
      background-color: #FBDAEB;
      color: #000000;
    }

    .expnest15:hover {
      background-color: #F7B6D7;
      color: #000000;
    }

    .expnest0 {
      background-color: #f0f0f0;
    }

    .font-expnest0 {
      background-color: #DDFFD6;
    }

    .font-expnest1 {
      background-color: #FFC2C2;
    }

    .font-expnest2 {
      background-color: #ADF8FF;
    }

    .font-expnest3 {
      background-color: #FEFFD6;
    }

    .font-expnest4 {
      background-color: #DCD6FF;
    }

    .font-expnest5 {
      background-color: #FFE3C2;
    }

    .font-expnest6 {
      background-color: #C2D9FF;
    }

    .font-expnest7 {
      background-color: #F9C8E0;
    }
  
    .font-expnest8 {
      background-color: #DCD6FF;
    }

    .font-expnest9 {
      background-color: #BDB2FF;
    }

    .font-expnest10 {
      background-color: #FFECD6;
    }

    .font-expnest11 {
      background-color: #FFD6A5;
    }

    .font-expnest12 {
      background-color: #D6E6FF;
    }

    .font-expnest13 {
      background-color: #A0C4FF;
    }

    .font-expnest14 {
      background-color: #FBDAEB;
    }

    .font-expnest15 {
      background-color: #F7B6D7;
    }

    body {
      /* background-color: #001B2E; */
    }

    .code-container {
      font-family: monospace;
      line-height: 1.5;
      color: /* #FFF8EB; */ #000000;
      overflow: auto;
      height: 92vh;
    }

    .expinfo {
      /* background-color: #ADB6C4; */
      background-color: #eeeeee;
      padding: 10px;
      font-family: monospace;
      line-height: 1.5;
      color: #000;
    }

    .column {
      float: left;
      overflow: auto;
    }

    .row:after {
      content: "";
      display: table;
      clear: both;
    }

    /* Syntax highlighting for C keywords */
    .code-container .keyword {
      color: #800080 /* 579dd6; 
      font-weight: bold; */
    }

    .code-container .type {
      color: #228b22;
    }

    .code-container .vardecl {
      color: #a0522d;
    }

    /* Syntax highlighting for C comments */
    .code-container .comment {
      color: #6a9955;
      /* green */
      font-style: italic;
    }

    /* Syntax highlighting for C strings */
    .code-container .string {
      color: #ce9079;
      /* red */
    }

    /* Syntax highlighting for C numbers */
    .code-container .number {
      color: #b5cea8;
      /* orange */
    }

    /* Syntax highlighting for C preprocessor directives */
    .code-container .preprocessor {
      color: #c586c0;
      /* gray */
    }

    /* Syntax highlighting for C functions */
    .code-container .function-name {
      color: #0000ff;
    }

    .scrollbar::-webkit-scrollbar-track
    {
      border: 1px solid black;
      background-color: #FFF8EB;
    }

    .scrollbar::-webkit-scrollbar
    {
      width: 10px;
    }

    .scrollbar::-webkit-scrollbar-thumb
    {
      background-color: #ADB6C4;	
    }
  </style>
  <meta http-equiv="content-type" content="text/html; charset=utf-8">
  <title>Frama-C/Codex Analysis results</title>
</head>

<body>
  <div class="row">
    <pre class="scrollbar column code-container" style="width: 45%; margin: 0px;">
|prefixe};;

let suffixe1 = "</pre><div class=\"column expinfo\" style=\"height: 92vh; width: 54%; padding: 0px;\">";;
let suffixe2 = "</div></body></html>";;

(* Note: si on veut  rajouter des liens, attention on ne peut pas les nester. *)
(*    <a href="tutu">nesting</a><a href="pimpon">not</a><a href="pompom">permitted</a> *)

let print filename print_expinfo print_lvalinfo print_function print_alarms =

  let print_exp_or_lval_info fmt = function
    | Exp (ki,e) -> print_expinfo fmt (ki,e)
    | Lval (ki,e) -> print_lvalinfo fmt (ki,e)
    | Function s -> print_function fmt s
  in

  (* Open file and print prefixe. *)
  let outfile = open_out filename in
  let outfmt = Format.formatter_of_out_channel outfile in
  let file = Ast.get() in
  output_string outfile prefixe;
  flush outfile;

  (* Print the main file. *)
  let printed_exp_hash = PrintHtmlCode.pp_file outfmt file in
  Format.pp_print_flush outfmt ();
  output_string outfile suffixe1;
  flush outfile;

  print_alarms outfile;

  (* Print the "informations" frame. *)
  Printf.fprintf outfile "<table style=\"width: 100%%\"> <tr> <th style=\"font-style: italic\">/</th> <th>Name</th> <th>Type</th> <th>Value</th> </tr>";
  printed_exp_hash |> Cil_datatype.Stmt.Hashtbl.iter (fun stmt expmap ->
    expmap |> ExpOrLvalWithId.Map.iter (fun (exp,id) nestlevel ->
                  Printf.fprintf outfile "<tr class=\"font-expnest%d eidinfo\" id=\"eidinfo%d\" style=\"display: none\">"
                    nestlevel id;
                  print_exp_or_lval_info outfile exp;
                  Printf.fprintf outfile "</tr>\n"));
  Printf.fprintf outfile "</table>";

  (* Print suffixe. *)
  output_string outfile suffixe2;
  flush outfile;
  close_out outfile;
;;
