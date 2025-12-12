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

(** Disassembly: equivalent of objdump -d. But because the DOM is
    slow, we only show a small set of lines, that we update
    dynamically using next/prev/goto (moreover this will be kept in
    sync with the trace when we have it, and we will also show some
    structural information like calls, if, while etc.)  *)

open Vdom
open Vdom_ext
open Interface
open Static_data

module StringSet = Set.Make(String)

type selected = {
  line: int64;                   (** The current line. *)
  group: (int64 * int64) option; (** If the line is in a group, the start and end of this group. *)
  dba_instr: Hoverable_expr.model array;
}

(* A path in the tree of menus. *)
type menu_state =
  | Root
  | In_goto


type model = {
  static: Static_data.disassembly;
  menu_state: menu_state;
  first_visible_address: Int64.t;
  last_symbols: string list;    (** Invisible symbols that are before the first_visible_address. *)
  hover_ranges: (int64 * int64, StringSet.t) Hashtbl.t;
  (*hover_ranges: (int64(head) * int64(tail), string(node_id)) Hashtable. Every (key, value) corresponds to a node;*)
  selected: selected;
  hovered_group: (int64 * int64) option;
  (*results_map: value_node list Int64Map.t;  corresponds to trace_model's related_nodes: same structure, used to find expr values*)
}

type internal_message =
  | Nop
  (* The mouse hovers on this line, or on nothing. *)
  | Set_hovered_group of int64 option  (* None = unselect *)
  (* We have selected the line (e.g., by keyboard or clicking on it)   *)
  | Set_selected_line of int64
  | Hoverable_expr_internal of {index:int;message:Hoverable_expr.internal_message}
  | Keyboard_action of (model -> model * outgoing_message Vdom.Cmd.t)
  | Goto of int64
  | Change_menu_state of menu_state
  | Batch of outgoing_message list


and incoming_message =
  | Internal_in of internal_message
  | Direct_goto of int64

and outgoing_message =
  | Internal_out of internal_message
  | Display_modal: 'a Modal.t * ('a option ->  internal_message) -> outgoing_message
  | Trace_update_filter of (int64 * Syntax_tree.expr_path * Syntax_tree.expr) option  (* line clicked inside Hoverable_expr    *)

let hoverable_inject addr index = function
  | Hoverable_expr.ClickInstr(path, expr) ->
    Trace_update_filter (Some(addr,path, expr))
  | Hoverable_expr.HoverInstr path -> Internal_out Nop
  | Hoverable_expr.Outgoing_internal message -> Internal_out(Hoverable_expr_internal{index;message})

(* Or wrap on a Hoverable message that we don't interpret but pass*)

let update_last_symbols map (split_addr:int64) (last_symbols:string list) =
  match
    map
    |> Int64Map.split (Int64.add split_addr (Int64.of_int(1)))
    |> (fun (l, data, r) -> l)
    |> Int64Map.filter (fun addr disassembly_line -> disassembly_line.symbols_names <> []) (* empty string list means no symbol *)
    |> Int64Map.max_binding_opt

  with
  | Some s ->  (snd s).symbols_names
  | None -> last_symbols
  
let select_line addr = Internal_in (Set_selected_line addr)


(* let find_substring_index (s: string) (sub: string) : int option = *)
(*   let len_s = String.length s in *)
(*   let len_sub = String.length sub in *)
(*   if len_sub > len_s then None else *)
(* let rec search_index i = *)
(*   if i + len_sub > len_s then None *)
(*   else if String.sub s i len_sub = sub then Some i *)
(*   else search_index (i + 1) *)
(* in *)
(* search_index 0 *)

(* let extract_ranges dot_str = *)
(*   let lines = String.split_on_char ';' dot_str in *)
(*   let extract_range line = *)
(*     try *)
(*       match find_substring_index line "[label=<<TAB" with *)
(*         | Some i -> ( *)
(*           let start_addr = Int64.of_string (String.sub line (i+65) 10) in *)
(*           let end_addr = Int64.of_string (String.sub line (i+98) 10) in *)
(*         Some (start_addr, end_addr)) *)
(*         | None -> None *)

(*     with *)
(*     | Not_found -> *)
(*         print_endline "Error: Not_found exception"; *)
(*         None *)
(*     | Failure msg -> *)
(*         print_endline ("Error: Failure exception - " ^ msg); *)
(*         None *)
(*     | Invalid_argument msg -> *)
(*         print_endline ("Error: Invalid_argument exception - " ^ msg); *)
(*         None *)
(*   in *)
(*   List.filter_map extract_range lines *)
(*   |> List.sort (fun (start1, end1) (start2, end2) -> Int64.to_int (Int64.sub start1 start2)) *)
(*   (\* Sort the list so search_ranges is faster*\) *)

let search_range (element: int64) (ranges: (int64 * int64, StringSet.t) Hashtbl.t)=
  let group = ref None in
  let node_ids = ref StringSet.empty in
  Hashtbl.iter (fun (start, end_) ids ->
      if start <= element && element <= end_ then( (*PARENTHESE*)
        group := Some (start,end_);
        node_ids := ids)
    ) ranges;
  match !group with
  | None ->
    (* TODO: This should not fail, but seems safe for now. *)
    (* NB: Actually, it can fail, if the line is not in a basic block. *)
    (* Js_browser.Console.log Js_browser.console (Obj.magic "Search range failed"); *)
    (* Js_browser.Console.log Js_browser.console (Obj.magic (Int64.to_string element)); *)
    (None) (* Every address should be in a range. *)
  | Some group -> (Some group)

(* Function to get the digraph string*)
(* let get_dot_str display_graph ranges_map = (Format.asprintf "%a" (Display_graph.pp_dot_graph ?ranges_map) display_graph) *)

type initial_data = Static_data.disassembly * (int64 * int64, Set.Make(String).t) Hashtbl.t

let init (static,ranges_map)  =
  let map = static.decompiled in
  let first_visible_address = fst (Int64Map.min_binding map) in
  Vdom.return
    { static;
      first_visible_address = first_visible_address;
      last_symbols = update_last_symbols map first_visible_address ["No function"];
      (*ranges = []extract_ranges (get_dot_str display_graph);;*)
      hover_ranges = ranges_map;
      selected = {line=first_visible_address; group = None; dba_instr=[||]}; (* TODO: we should have a group, but hover_ranges is not set yet. *)
      hovered_group = None;
      menu_state = Root;
    }

(* Note on the table layout:
   - Because we use table-auto, the layout sometimes jump a bit when we scroll.
      table-fixed works too however, but the computation of the sizes may not be right.
   - The hidden xl:table-cell classes allows the opcode element to be display:hidden on small screens,
      and to show up on large screens. I do this because this information is less useful.
      MAYBE: Show the opcode on hover or inside a tooltip.
   - I use pl-6 to add some distance between the columns.
   - text-left is to left-align the text.
*)

(* let find_next_trace_node (parent_trace:trace_node) next_expr =
   let res = ref None in
   List.iter (fun child ->
    match child.value with
      | Dba_expr {expr} -> if expr = next_expr then res := Some child;
      | _ -> ()
    ) parent_trace.children;
   match !res with
    | Some res -> res
    | _ -> print_endline "Problem"; assert false *)





let view model =
  let disassembly = model.static in
  let set_hovered_group addr _mouse_event (* node_ids *) (* message _ *) =
    Internal_out (Set_hovered_group addr)
  in

  let is_on_selected_line x  =
    match model.selected with
    | y -> x = y.line in

  let set_selected_line line_addr _ =
    Internal_out (Set_selected_line line_addr)
  in
  let vdom_node =
    let first_visible_address = model.first_visible_address in
    let rows =
      Int64Map.to_seq_from first_visible_address disassembly.decompiled
      |> Seq.map (fun (key,Interface.{binary_code;mnemonic;symbols_names;instructions; results}) ->
          let results = String.concat "" results in
          let bg = "odd:bg-white even:bg-gray-100" in
          let row =
            let instruction_string = mnemonic in
            let row_classes=
              (match model.selected with

               (* On the selected line. *)
               | s when s.line = key -> (* selected line. *)
                 "bg-emerald-300"

               (* The selected line is in a group, and the key belong to it. *)
               | s when
                   (match s.group with
                    | None -> false
                    | Some(group_head,group_last) ->
                      group_head <= key && key <= group_last) ->
                 "bg-emerald-100"

               (* The selected line is in the hovered group. *)
               | _ when
                   (match model.hovered_group with
                    | None -> false
                    | Some(group_head,group_last) ->
                      group_head <= key && key <= group_last) ->
                 "bg-yellow-200"
               | _ -> bg)
              (* ^ (Printf.sprintf " line 0x%08Lx" @@ (match group with g -> g | _ -> Int64.zero (\* XXX*\))) *)

            in
            tr ~a:[attr "id" ("line-" ^ Printf.sprintf "0x%08Lx" key);
                   class_ row_classes;
                   onmouseenter (set_hovered_group (Some key) (* node_ids "mouse_enter" *));
                   onmouseleave (set_hovered_group None (* node_ids "mouse_out" *));
                   onclick (set_selected_line key)]
              [ td ~a:[class_ "w-1/4"] [pre [Vdom.text (Printf.sprintf "%08Lx:" key)]]
              ; td ~a:[class_ "w-1/4   hidden xl:pl-6 xl:table-cell"] [ pre [Vdom.text binary_code ]]
              ; td ~a:[class_ "pl-6 w-1/16 text-left"] [ pre [ Vdom.text (instruction_string) ]]
              ] in
          let row = if (is_on_selected_line key) then
              (let dba_results_row = Vdom.fragment [
                   tr ~a:[class_ "bg-blue-100 border border-stone-900";]
                     [
                       td ~a:[colspan "3"; class_ "w-1/4 text-left"; style "width" "100%";]
                         [
                           div ~a:[style "max-height" "100%"; style "height" "100%"; style "width" "100%"; style "max-width" "100%"; style "overflow-y" "auto";]
                             [ pre ~a:[style "margin" "0"; style "white-space" "pre-wrap"; style "overflow-wrap" "break-word";]
                                 [Vdom.fragment (model.selected.dba_instr |> Array.to_list |> List.mapi (fun i instr_model ->
                                      div [Vdom.map (hoverable_inject key i) (Hoverable_expr.view instr_model)]))]
                             ]
                         ]
                     ]
                 (* Not 100% sure that this one is useful.  *)
                 ; tr ~a:[class_ "bg-green-100 border border-stone-900";]
                     [
                       td ~a:[colspan "3"; class_ "w-1/4 text-left"; style "width" "100%"; style "height" "fit-content"; style "max-height" "6rem";]
                         [
                           div ~a:[style "max-height" "100%"; style "height" "100%"; style "width" "100%"; style "max-width" "100%"; style "overflow-y" "auto";]
                             [ pre ~a:[style "margin" "0"; style "white-space" "pre-wrap"; style "overflow-wrap" "break-word";]
                                 [ Vdom.text results]
                             ]
                         ]
                     ]
                 ]

               in Vdom.fragment
                 [ tr ~a:[style "border-bottom" "2px solid black"] [td  ~a:[colspan "3"] []]
                 ; row
                 ; dba_results_row
                 ; tr ~a:[style "border-bottom" "2px solid black"] [td  ~a:[colspan "3"] []]
                 ]) else row
          in
          let row =
            let show_symbol_row = symbols_names <> [] in
            if show_symbol_row then
              let symbol_row =
                tr ~a:[class_ "bg-blue-200 line"]
                  [ td ~a:[class_ "w-1/4 text-left"]
                      [pre [Vdom.text (Printf.sprintf "%08Lx:" key)]]
                  ; td ~a:[class_ " w-1/4 text-left"; colspan "2"]
                      [ pre [ Vdom.text ("Symbol(s): " ^ (String.concat "," symbols_names))]]] in
              Vdom.fragment
                [ symbol_row
                ; row ]
            else row
          in
          row



        )
      |> Seq.take 60
      |> List.of_seq
    in
    let rows =
      (* TODO: no parent_symbol_row if first line already has a parent. *)
      let parent_symbol_row =
        tr ~a:[class_ "bg-blue-200"]
          [ td ~a:[class_ " w-1/4 text-left"; colspan "3"]
              [ pre [ Vdom.text ("Parent symbol(s): " ^ (String.concat ", " model.last_symbols))]] ] in
      parent_symbol_row::rows
    in

    table ~a:[class_ "text-sm table-auto w-full min-w-full"]
      [
        thead [ tr [ th ~a:[class_ "w-1/4 text-left"; style "width" "10px"] [ text "Address" ]
                   ; th ~a:[class_ "w-1/4 text-left  hidden xl:pl-6 xl:table-cell"] [ text "Binary code" ]
                   ; th ~a:[class_ "pl-6 w-full text-left"] [ text "Instruction" ]]];

        tbody ~key:"contentArea" ~a:[attr "id" "contentArea";style "width" "100px"; style "max-width" "100px"] rows
      ]
  in

  (* Vdom.div ~a:[class_ "flex-1 flex flex-col"] *)
  Vdom.div ~a:[class_ "overflow-y-hidden overflow-x-auto";
               (* style "height" "48rem" *)
              ]
    [vdom_node]


let info_about address idx  : (module Hoverable_expr.Tooltip_information) =
  let locid = Syntax_tree.Location_identifier.(Address,Int64 address) in
  (* assert (Static_data.(Loc_hash.mem loc_hash) (Any locid)); *)
  let module X = struct
    type t = Syntax_tree.Location_identifier.any
    let root =
      let r = Syntax_tree.Location_identifier.(Any (DbaInstruction,Inside{locid;idx})) in
      (* assert (Static_data.(Loc_hash.mem loc_hash) r); *)
      r
      
    let get x =
      try
        (* TODO: Return all the nodes, so that we can select the values of interest. *)
        let Any_trace_node
            trace_node = Static_data.(Loc_hash.find loc_hash) x in
        match trace_node.result with
        | No_result | Single _ -> None
        | Result s -> Some s
      with Not_found ->
        None
    let find_child (Syntax_tree.Location_identifier.Any locid) idx = Some(Syntax_tree.Location_identifier.(Any (Expression,Inside{locid;idx})))
  end
  in (module X)

let selected disassembly line =
  let group = search_range line disassembly.hover_ranges in
  let desassembly_line = Int64Map.find line disassembly.static.decompiled in
  let instructions = desassembly_line.instructions in
  let dba_instr = instructions |> Array.of_list |> Array.mapi (fun idx instr ->
      let x = info_about line idx in
      match Hoverable_expr.init (x,instr) 
      with model,Vdom.Cmd.Batch [] -> model 
      |_-> assert false
      ) in
  {line;group;dba_instr}

let handle_goto address disassembly (cur_addr:int64) =
  let map = disassembly.static.decompiled in
  let last_symbols = disassembly.last_symbols in
  match Int64Map.find_opt address disassembly.static.decompiled with
  | Some _ ->
    {disassembly with first_visible_address = address;
                      last_symbols=update_last_symbols map address last_symbols;
                      selected = selected disassembly address }
  | None -> disassembly

(** Goto and unselect*)
let handle_direct_goto address model cur_addr =
  handle_goto address model cur_addr;;

module Update = struct

  (* For now: increment first line and current line in sync, because I
     don't know how to test when I reach the end. I should just check
     whether next( next element) is visible; if not, the previous one
     may be only partially visible; in which case I move the
     view. nano and vim do it like this. There are checkVisibility
     functions, so it may not be too complicated. TODO: CHeck if it
     works using Console.log *)
  let next_instr model cur =
    let map = model.static.decompiled in
    if (cur <> fst (Int64Map.max_binding map)) then
      map
      |> Int64Map.split cur
      |> (fun (l, data, r) -> r)
      |> Int64Map.min_binding
      |> fst
    else cur (*if cur is already the last element the next line must be the same*)
  ;;

  let prev_instr model cur =
    let map = model.static.decompiled in
    if (cur <> fst (Int64Map.min_binding map)) then
      map
      |> Int64Map.split cur
      |> (fun (l, data, r) -> l)
      |> Int64Map.max_binding
      |> fst
    else cur (*if cur is already the first element the previous line must be the same*)
  ;;

  (* TODO: factorize, in particular the fact that we change selected
     line. *)
  let next_line model =
    let map = model.static.decompiled in
    let curfirst = model.first_visible_address in
    let cur = model.selected.line in
    let nextfirst = next_instr model curfirst in
    let nextselected = next_instr model cur in
    let selected = selected model nextselected in
    let last_symbols=update_last_symbols map nextfirst model.last_symbols in
    Vdom.return @@ {model with first_visible_address = nextfirst; last_symbols; selected}

  let previous_line model =
    let map = model.static.decompiled in
    let curfirst = model.first_visible_address in
    let cur = model.selected.line in
    let prevfirst = prev_instr model curfirst in
    let prevselected = prev_instr model cur in
    let selected = selected model prevselected in
    let last_symbols=update_last_symbols map prevfirst model.last_symbols in
    Vdom.return @@ {model with first_visible_address = prevfirst; last_symbols; selected}

end

let update disassembly message =
  match message with
  | Direct_goto addr | Internal_in (Goto addr) ->
    Vdom.return @@ handle_direct_goto addr disassembly disassembly.first_visible_address
  | Internal_in Nop ->
    Vdom.return disassembly
  | Internal_in(Set_hovered_group None) ->
    Vdom.return @@ {disassembly with hovered_group=None}
  | Internal_in(Set_hovered_group (Some addr)) ->
    let group = search_range addr disassembly.hover_ranges in
    Vdom.return @@ {disassembly with hovered_group=group}
  | Internal_in(Set_selected_line line) ->
    let selected = selected disassembly line in
    Vdom.return @@ {disassembly with selected}
  | Internal_in(Hoverable_expr_internal {index;message}) ->
    let dba_instr = disassembly.selected.dba_instr in
    let exprm = dba_instr.(index) in
    (match Hoverable_expr.update exprm (Hoverable_expr.Incoming_internal message) with 
    |exprm,Vdom.Cmd.Batch [] -> 
    let dba_instr =
      let dba_instr = Array.copy dba_instr in
      dba_instr.(index) <- exprm;
      dba_instr
    in
    let selected = {disassembly.selected with dba_instr} in
    Vdom.return @@ {disassembly with selected}
    |_-> assert false)
  | Internal_in (Keyboard_action act) -> act disassembly
  | Internal_in (Change_menu_state menu_state) ->
    Vdom.return {disassembly with menu_state}
  | Internal_in (Batch l) ->
    Vdom.return ~c:(l |> List.map Vdom.Cmd.echo) disassembly

(* MAYBE we want to display what is inside an expression too. Maybe
   with left/right/up/down, or f/b/RET/ESC. This will be for later. *)

let kbd_action x = Internal_out (Keyboard_action x)

let go_to_modal ~title ~suggestions parse_res : outgoing_message =
  let modal = Modal_input_string.input_string ~title ~suggestions in
  let f = function
    | None -> Nop
    | Some result -> begin
        match parse_res result with
        | Some addr -> Goto (addr)
        | None -> Nop           (* Could not find the address. *)
    end
  in
  Internal_out (Batch [Internal_out (Change_menu_state Root);
                       Display_modal(modal,f)])

let go_to_address_modal model =
  let map = model.static.decompiled in
  let parse_addr addr_str =
    let addr = Int64.of_string("0x" ^ addr_str) in
    if Int64Map.mem addr map then Some addr else None in
  let suggestions str ~max = "", [] in
  go_to_modal ~title:"Go to address:" ~suggestions parse_addr


let go_to_symbol_modal model =
  let map = model.static.decompiled in
  (* let suggestions = fun string ~max -> ["Suggestion 11";"Suggestion 22";"Suggestion 33"] in *)
  let suggestions = Prefix_tree.suggestions model.static.symbols_trie in
  let parse_symbol symbol_str =
    map
    |> Int64Map.filter (fun addr disassembly_line -> (List.mem symbol_str disassembly_line.symbols_names))
    |> Int64Map.choose_opt |> Option.map fst in
  go_to_modal ~title:"Go to symbol:" ~suggestions parse_symbol

let get_menu (model:model) =
  let open Transient_menu in
  let stack = match model.menu_state with
    | Root -> []
    | In_goto ->
      [{ title = "Goto ...";
        entries =
          [| {highlight=false; keys=[Char "a"]; desc="address"; action=(go_to_address_modal model)};
             {highlight=false; keys=[Char "s"]; desc="symbol"; action=(go_to_symbol_modal model)};
             {highlight=false; keys=[Escape];   desc="cancel"; action=(Internal_out (Change_menu_state Root))};
          |]}]
  in
  let menu =
    { title = "Disassembly";
      entries =
        [| {highlight=false; keys=[Char "n"; ArrowDown]; desc="next-line";     action=kbd_action Update.next_line};
           {highlight=false; keys=[Char "p"; ArrowUp]  ; desc="previous-line"; action=kbd_action Update.previous_line};
           {highlight=(model.menu_state = In_goto); keys=[Char "g"]  ; desc="goto ..."; action=Internal_out(Change_menu_state In_goto)};
        |]}
  in
  menu::stack


  (* TODO: u/left = previous symbol *)
  (* TODO: f/b on a headline = next headline. *)
  (* TODO: RET = expand. *)
