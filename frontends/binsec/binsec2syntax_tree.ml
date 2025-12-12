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

open Syntax_tree


type 'a decomp =
  | Ar0: {s:string} -> 'a decomp
  | Ar1: {format:Syntax_tree.ar1_format; arg:'a} -> 'a decomp
  | Ar2: {format:Syntax_tree.ar2_format; arg1:'a; arg2: 'a} -> 'a decomp
  | Ar3: {format:Syntax_tree.ar3_format; arg1:'a; arg2: 'a; arg3: 'a} -> 'a decomp
  | Ite: {cond:'a;then_expr:'a;else_expr:'a} ->'a decomp (* XXX: temp *)

let decomp_arity = function
  | Ar0 _ -> 0 | Ar1 _ -> 1 | Ar2 _ -> 2 | Ar3 _ -> 3 | Ite _ -> 3


let var_to_string var = var.Dba.Var.name
  (* Format.asprintf "%s<%d>" *)
  (*   var.Dba.Var.name var.Dba.Var.size *)
let bitvector_to_string bitvector =
    Format.asprintf "%s"
    (Int64.to_string (Bitvector.to_int64 bitvector)) 
  (* Format.asprintf "%s<%d>" *)
  (*   (Int64.to_string (Bitvector.to_int64 bitvector)) (Bitvector.size_of bitvector) *)

(* Make sure to physically share the format strings. We don't need
   hashconsing with this. *)
let binop_fmt =
  let infix_fmt string  = {prefix="";middle=(" " ^ string ^ " ");suffix=""} in
  let plus = infix_fmt  "+" in
  let minus = infix_fmt  "-" in
  let mult = infix_fmt  "*" in
  let divu = infix_fmt  "/" in
  let divs = infix_fmt  "/" in
  let modu = infix_fmt  "mod" in
  let mods = infix_fmt  "mod"in
  let or_ = infix_fmt  "|" in
  let and_ = infix_fmt  "&" in
  let xor = infix_fmt  "^"in
  let concat = infix_fmt  "++"in
  let lshift = infix_fmt  "<<" in
  let rshiftu = infix_fmt  ">>u" in
  let rshifts = infix_fmt  ">>s"in
  let leftrotate = infix_fmt  "<<<" in
  let rightrotate = infix_fmt  ">>>"in
  let eq = infix_fmt  "=" in
  let diff = infix_fmt  "<>"in
  let lequ = infix_fmt  "<=u" in
  let ltu = infix_fmt  "<u" in
  let gequ = infix_fmt  ">=u" in
  let gtu = infix_fmt  ">u"in
  let leqs = infix_fmt  "<=s" in
  let lts = infix_fmt  "<s" in
  let geqs = infix_fmt  ">=s" in
  let gts = infix_fmt ">s" in
  let open Dba.Binary_op in
  function
  | Plus -> plus | Minus -> minus | Mult -> mult
  | DivU -> divu | DivS -> divs | ModU -> modu | ModS -> mods
  | Or -> or_ | And -> and_ | Xor -> xor
  | Concat -> concat
  | LShift -> lshift | RShiftU -> rshiftu | RShiftS -> rshifts
  | LeftRotate -> leftrotate | RightRotate -> rightrotate
  | Eq -> eq | Diff -> diff
  | LeqU -> lequ | LtU -> ltu | GeqU -> gequ | GtU -> gtu
  | LeqS -> leqs | LtS -> lts | GeqS -> geqs | GtS -> gts

let binop op arg1 arg2 : Dba.Expr.t decomp = Ar2{format=(binop_fmt op);arg1;arg2}      

let rec decomp_expr (expr:Dba.Expr.t) : Dba.Expr.t decomp = 
  match expr with
  | Var var -> Ar0 {s=(var_to_string var)}
  | Cst bv -> Ar0 {s=(bitvector_to_string bv)}
  | Unary (op, expr) ->
    (* TODO: We should hashcons to share the format. *)
    (match op with
     | UMinus -> Ar1{format={prefix="-";suffix=""}; arg=expr}
     | Not -> Ar1{format={prefix="!";suffix=""}; arg= expr}                   
     | Uext size -> Ar1{format={prefix="extu "; suffix=" " ^ string_of_int size}; arg= expr}
     | Sext size -> Ar1{format={prefix="exts "; suffix=" " ^ string_of_int size}; arg= expr}
     | Restrict {lo; hi}->
       let suffix = 
         if (lo=hi) then "{" ^ string_of_int lo ^ "}"
         else
           "{" ^ string_of_int lo ^ "," ^ string_of_int hi ^ "}"
       in
       Ar1 {format={prefix="";suffix};arg= expr}
    )
  | Binary (op, arg1, arg2) ->
    binop op arg1 arg2
  | Load (size, endian, expr, str) ->
    let str = match str with None -> "@" | Some x -> x in
    let prefix = str ^ "[" in
    let suffix = ", " ^ (string_of_int size) ^ "]" in
    Ar1{format={prefix;suffix};arg=expr}
| Ite (cond, then_expr, else_expr) -> Ite{cond;then_expr;else_expr}

and decomp_to_syntax_tree : _ decomp -> Syntax_tree.expr = function
  | Ar0{s} -> Ar0{s}
  | Ar1{format;arg} -> Ar1{format;arg=expr_of_binsec_expr arg}
  | Ar2{format;arg1;arg2} -> Ar2{format;arg1=expr_of_binsec_expr arg1;arg2=expr_of_binsec_expr arg2}
  | Ar3{format;arg1;arg2;arg3} -> Ar3{format;arg1=expr_of_binsec_expr arg1;
                                      arg2=expr_of_binsec_expr arg2;arg3=expr_of_binsec_expr arg3}
  | Ite{cond;then_expr;else_expr} -> Ite{cond=expr_of_binsec_expr cond; then_expr=expr_of_binsec_expr then_expr;
                                         else_expr=expr_of_binsec_expr else_expr}


and expr_of_binsec_expr (expr:Dba.Expr.t) =
  decomp_to_syntax_tree (decomp_expr expr)


let pp_array =
  Format.pp_print_option
    ~none:(fun ppf () -> Format.pp_print_char ppf '@')
    Format.pp_print_string


let pp_code_address ppf addr =
  if addr.Dba.id = 0 then       (* The normal case. *)
    Virtual_address.pp ppf addr.Dba.base
  else
    Format.fprintf ppf "(%a, %d)" Virtual_address.pp addr.Dba.base addr.Dba.id

let pp_jump_target ppf = let open Format in function
    | Dba.JInner id -> fprintf ppf "%d" id
    | Dba.JOuter caddr -> fprintf ppf "%a" pp_code_address caddr


let instr_of_binsec_instr (instr : Dba.Instr.t) : expr = 
  match instr with
  | Dba.Instr.Assign (lhs, rhs, id) ->
    (* If the LHS is a store instruction, also display it as an hoverable expr. *)
    begin
      match lhs with
      | Var _ | Restrict _ -> 
        Ar1{format={prefix=(string_of_expr (expr_of_binsec_expr (Dba.LValue.to_expr lhs)) ^ " := ")
                   ;suffix=""};
            arg=expr_of_binsec_expr rhs}
      | Store(size,endian,expr,str) ->
        let prefix = Format.asprintf "%a[" pp_array str in
        let middle = Format.asprintf ",%d] := " size in
        Ar2{format={prefix;middle;suffix=""};arg1=(expr_of_binsec_expr expr);arg2=(expr_of_binsec_expr rhs)}
  end
  | Dba.Instr.SJump (addr, tag) ->
    Ar0{s=Printf.sprintf "goto %s %s"
            (Format.asprintf "%a" pp_jump_target addr)
            (Format.asprintf "%a" Dba_printer.Ascii.pp_tag tag)}
  | Dba.Instr.DJump (e_addr, tag) ->
    Ar1{format={prefix="goto "; suffix=(Format.asprintf " %a" Dba_printer.Ascii.pp_tag tag)};
        arg= expr_of_binsec_expr e_addr}
  | Dba.Instr.If (e, addr, int_addr) ->
    let suffix = Format.asprintf " goto %a else goto %d" pp_jump_target addr int_addr in
    Ar1{format={prefix="if ";suffix};
        arg = expr_of_binsec_expr e}
  | Dba.Instr.Stop state_opt ->
    Ar0{s="stop"}
    (* Stop (match state_opt with *)
    (* | Some state -> Some((match state with *)
    (*   | Dba.KO -> Syntax_tree.KO *)
    (*   | Dba.OK -> Syntax_tree.OK *)
    (*   | Dba.Undecoded str -> Syntax_tree.Undecoded str *)
    (*   | Dba.Unsupported str -> Syntax_tree.Unsupported str)) *)
    (* | None -> None) *)
  | Dba.Instr.Assume (cond, id) ->
    Ar1{format={prefix="assume ";suffix=""};arg=expr_of_binsec_expr cond}
  | Dba.Instr.Assert (cond, id) ->
    Ar1{format={prefix="assert ";suffix=""};arg=expr_of_binsec_expr cond}
  | Dba.Instr.Undef (lhs, id) ->
    Ar0{s="XXX undef"}
      (* Undef (expr_of_binsec_expr (Dba.LValue.to_expr lhs), id) *)
  | Dba.Instr.Nondet (lhs, id) ->
    Ar0{s="XXX nondet"}    
      (* Nondet (expr_of_binsec_expr (Dba.LValue.to_expr lhs), id) *)



let decomp_instr (instr : Dba.Instr.t) : Dba.Expr.t decomp = 
  match instr with
  | Dba.Instr.Assign (lhs, rhs, id) ->
    (* If the LHS is a store instruction, also display it as an hoverable expr. *)
    begin
      match lhs with
      | Var _ | Restrict _ -> 
        Ar1{format={prefix=(string_of_expr (expr_of_binsec_expr (Dba.LValue.to_expr lhs)) ^ " := ")
                   ;suffix=""};
            arg=rhs}
      | Store(size,endian,expr,str) ->
        let prefix = Format.asprintf "%a[" pp_array str in
        let middle = Format.asprintf ",%d] := " size in
        Ar2{format={prefix;middle;suffix=""};arg1=expr;arg2=rhs}
  end
  | Dba.Instr.SJump (addr, tag) ->
    Ar0{s=Printf.sprintf "goto %s %s"
            (Format.asprintf "%a" pp_jump_target addr)
            (Format.asprintf "%a" Dba_printer.Ascii.pp_tag tag)}
  | Dba.Instr.DJump (e_addr, tag) ->
    Ar1{format={prefix="goto "; suffix=(Format.asprintf " %a" Dba_printer.Ascii.pp_tag tag)};
        arg= e_addr}
  | Dba.Instr.If (e, addr, int_addr) ->
    let suffix = Format.asprintf " goto %a else goto %d" pp_jump_target addr int_addr in
    Ar1{format={prefix="if ";suffix};
        arg = e}
  | Dba.Instr.Stop state_opt ->
    Ar0{s="stop"}
  | Dba.Instr.Assume (cond, id) ->
    Ar1{format={prefix="assume ";suffix=""};arg=cond}
  | Dba.Instr.Assert (cond, id) ->
    Ar1{format={prefix="assert ";suffix=""};arg=cond}
  | Dba.Instr.Undef (lhs, id) ->
    Ar0{s="XXX undef"}
  | Dba.Instr.Nondet (lhs, id) ->
    Ar0{s="XXX nondet"}    



let instr_of_binsec_instr i : Syntax_tree.expr =
  i |> decomp_instr |> decomp_to_syntax_tree


let check_idx expr_of_src decomp_src ((instr,locid): _ Syntax_tree.located) ~idx target_expr : _ Syntax_tree.located =
  let locid = Syntax_tree.Location_identifier.Inside{locid;idx} in
  let kind = Syntax_tree.Location_identifier.Expression in
  let f e =
    if not (e == target_expr) then
      failwith @@
      Format.asprintf "In `%s', index %d does not correspond to `%s'"
        (Syntax_tree.string_of_expr @@ expr_of_src instr) idx
        (Syntax_tree.string_of_expr @@ expr_of_binsec_expr e)
    else (e,(kind,locid)) in
  match decomp_src instr, idx with
  | Ar1{arg}, 0 -> f arg
  | Ar2{arg1}, 0 -> f arg1
  | Ar2{arg2}, 1 -> f arg2
  | Ar3{arg1}, 0 -> f arg1
  | Ar3{arg2}, 1 -> f arg2
  | Ar3{arg3}, 2 -> f arg3
  | Ite{cond}, 0 -> f cond
  | Ite{then_expr}, 1 -> f then_expr
  | Ite{else_expr}, 2 -> f else_expr
  | d, _ ->
    failwith @@
    Format.asprintf 
      "In instr `%s' of arity %d, index %d does not correspond."
      (Syntax_tree.string_of_expr @@ expr_of_src instr)
      (decomp_arity d) idx

let check_instr_idx = check_idx instr_of_binsec_instr decomp_instr
let check_expr_idx = check_idx expr_of_binsec_expr decomp_expr
