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
module Make(Main:sig val main: Kernel_function.t end) = struct

  module VISet = Cil_datatype.Varinfo.Set;;
  module StringSet = Datatype.String.Set;;

  type acc = {
    functions_used: VISet.t;
    functions_visited:VISet.t;
    globals_used: VISet.t;
    globals_visited: VISet.t;
    strings_used: StringSet.t
  }

  open Cil_types;;

  let visit_constant acc = function
    | CStr str -> {acc with strings_used = StringSet.add str acc.strings_used }
    | CWStr _ -> Codex_options.fatal "Wide strings not yet handled"
    | CInt64 _ | CChr _ | CReal _ | CEnum _ -> acc
  
  let rec visit_expr acc exp = match exp.enode with
    | Const c -> visit_constant acc c
    | Lval lv -> visit_lval acc lv
    | SizeOf _ | SizeOfE _ | SizeOfStr _
    | AlignOf _ | AlignOfE _ -> acc
    | UnOp(_,e,_) | CastE(_,e) (* | Info(e,_) *) -> visit_expr acc e
    | BinOp(_,e1,e2,_) ->
      let acc = visit_expr acc e1 in
      visit_expr acc e2
    | AddrOf lv | StartOf lv -> visit_lval acc lv

  and visit_lval acc (lhost,offset) =
    let acc = visit_lhost acc lhost in
    visit_offset acc offset

  and visit_lhost acc = function
    | Var vi ->
      if vi.vglob
      then match vi.vtype.tnode with
        | (TFun _) -> {acc with functions_used = VISet.add vi acc.functions_used}
        | _ -> {acc with globals_used = VISet.add vi acc.globals_used }
      else acc
    | Mem e -> visit_expr acc e

  and visit_offset acc = function
    | NoOffset -> acc
    | Field(_,o) -> visit_offset acc o
    | Index(e,o) ->
      let acc = visit_expr acc e in
      visit_offset acc o

  let rec visit_init acc = function
    | SingleInit e -> visit_expr acc e
    | CompoundInit(_,l) -> List.fold_left (fun acc (o,i) ->
        let acc = visit_offset acc o in
        visit_init acc i
      ) acc l
  ;;
  
  let visit_local_init acc = function
    | AssignInit i -> visit_init acc i
    | ConsInit(f,args,_) ->
      let acc = visit_lhost acc (Var f) in
      List.fold_left visit_expr acc args  

  
  let visit_instr acc = function
    | Set(lv,e,_) ->
      let acc = visit_lval acc lv in
      visit_expr acc e
    | Call(lv,e,args,_) ->
      let acc = match lv with
        | None -> acc
        | Some lv -> visit_lval acc lv in
      let acc = visit_expr acc e in
      List.fold_left visit_expr acc args
    | Asm _ ->  acc
    | Skip _ -> acc
    | Code_annot _ -> acc
    | Local_init(vi,li,_)  ->
      let acc = visit_lhost acc (Var vi) in
      visit_local_init acc li
  ;;
  
  let rec visit_block acc block =
    List.fold_left visit_stmt acc block.bstmts

  and visit_stmt acc stmt =
    match stmt.skind with
    | Instr i -> visit_instr acc i
    | Return(Some e,_) -> visit_expr acc e
    | Return(None,_) -> acc
    | Goto _ | Break _ | Continue _ -> acc
    | If(e,b1,b2,_) ->
      let acc = visit_expr acc e in
      let acc = visit_block acc b1 in
      visit_block acc b2
    | Switch(e1,b1,_,_) ->
      let acc = visit_expr acc e1 in
      visit_block acc b1
    | Loop(_,b,_,_,_)
    | Block b -> visit_block acc b
    | UnspecifiedSequence l ->
      visit_block acc @@ Cil.block_from_unspecified_sequence l
    | Throw _ | TryCatch _ | TryFinally _ | TryExcept _ -> assert false


  let visit_fun acc vi =
    (* let vi = Kernel_function.get_vi kf in *)
    let acc = {acc with functions_visited = VISet.add vi acc.functions_visited} in
    try
      let fd = Kernel_function.get_definition @@ Globals.Functions.get vi in
      visit_block acc fd.sbody 
    with Kernel_function.No_Definition -> acc
  ;;
  
  (* Globals might be referencing other globals, including functions. *)
  let visit_global acc vi =
    let acc = {acc with globals_visited = VISet.add vi acc.globals_visited } in
    let initinfo = Globals.Vars.find vi in
    match initinfo.init with
    | None -> acc
    | Some init -> visit_init acc init
  ;;
  
  let functions_used, globals_used, strings_used =
    let main_vi = Kernel_function.get_vi Main.main in
    let acc = {
      functions_used = VISet.singleton main_vi;
      globals_used = VISet.empty;
      globals_visited = VISet.empty;
      functions_visited =  VISet.empty;
      strings_used = StringSet.empty
    } in
    let rec loop acc = 
      let fun_to_visit = VISet.diff acc.functions_used acc.functions_visited in
      if not @@ VISet.is_empty fun_to_visit
      then loop @@ VISet.fold (fun vi acc -> visit_fun acc vi) fun_to_visit acc
      else
        let globals_to_visit = VISet.diff acc.globals_used acc.globals_visited in
        if not @@ VISet.is_empty globals_to_visit
        then loop @@ VISet.fold (fun vi acc -> visit_global acc vi) globals_to_visit acc
        else acc
    in
    let acc = loop acc in
    (acc.functions_used, acc.globals_used, acc.strings_used)
  ;;


  
end
