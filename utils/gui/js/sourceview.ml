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

module Lines = struct

  type t =
    { text: string;
      line_starts: int array }  (* Starting index of each line. *)

  let num_lines t = Array.length t.line_starts

  (* Return an array with the starting index of each line. *)
  let line_starts (s : string) : int array =
    let starts = ref [0] in
    let rec loop i =
      let i', tok = Interface.get_token s i in
      match tok with
      | EOF -> ()
      | Marker _ -> loop i'  (* Skip markers entirely *)
      | String (_, a, b) ->
          (* count only '\n' in the plain text region *)
          for k = a to b - 1 do
            if s.[k] = '\n' then
              starts := (k + 1) :: !starts
          done;
          loop i'
    in
    loop 0;
    Array.of_list (List.rev !starts)

  let init text =
    { text; line_starts = line_starts text }

  (* The the text for line number i (starting from 0). *)
  let line ls i : string =
    let starts = ls.line_starts in
    let start = starts.(i) in
    let stop =
      if i + 1 < Array.length starts then
        starts.(i + 1) - 1
      else
        String.length ls.text
    in
    String.sub ls.text start (stop - start)

end

module Render = struct


  let style_of_marker = function
    (* Keyword. *)
    | Interface.Keyword_color -> [Vdom.style "font-weight" "600"; Vdom.style "color" "#800080"] (* 600 is semibold. *)
    (* Types. *)
    | Type_color    -> [Vdom.style "color" "#228b22"]
    (* Variable declarations. *)
    | Var_color     -> [Vdom.style "color" "#a0522d"]
     (* Functions declarations. *)
    | Fun_color     -> [Vdom.style "color" "#0000ff"]


let render (s : string) : 'a Vdom.vdom =
  let n = String.length s in

  let rec parse_nodes i =
    if i >= n then ([], i)
    else if s.[i] = '\x00' then (
      let m, _, j = Interface.string_to_marker s i in
      match m with
      | End_marker -> ([], j)
      | Color_marker sm ->
          let style = style_of_marker sm in
          let (children, next) = parse_nodes j in
          let (rest, k) = parse_nodes next in
          (Vdom_ext.span ~a:style children :: rest, k)
      | Function_Definition _name ->
          let (children, next) = parse_nodes j in
          let (rest, k) = parse_nodes next in
          (Vdom.fragment children :: rest, k)
      | Instruction id ->
          let (children, next) = parse_nodes j in
          let (rest, k) = parse_nodes next in
          (Vdom.fragment children :: rest, k)
    )
    else (
      (* Plain text block until the next marker *)
      let j =
        try String.index_from s i '\x00'
        with Not_found -> n
      in
      let txt = Vdom.text (String.sub s i (j - i)) in
      let (rest, k) = parse_nodes j in
      (txt :: rest, k)
    )
  in

  let (children, _) = parse_nodes 0 in
  Vdom_ext.pre children

end

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


type fun_info = {
  start_line : int; (*Line number where the function definition begins*)
  end_line   : int; (*Line number where the function definition ends*)
  collapsed : bool; (*Flag for toggle button to collapse/expand function definitions*)
}


module IntSet = Set.Make (Int)

module Row = struct
  type visibility = {
    lines: Lines.t; (*Formatted text in the form of lines*)
    fun_index : fun_info StringMap.t; (*Map of function names to their [fun_info]*)
    breakpoints : IntSet.t (*Set of line numbers with breakpoints activated*)
  }

  type t = int

  type message =
    | Change_open_status of bool * string
    | Toggle_breakpoint of int



 let is_visible v line =
  StringMap.for_all
    (fun _ info ->
       let inside_function =
         line > info.start_line && line <= info.end_line
       in
       not (info.collapsed && inside_function && line <> info.start_line))
    v.fun_index

  let next_visible (v : visibility) (x : t) =
    let n = Lines.num_lines v.lines in
    let rec step i =
      if i >= n then None
      else if is_visible v i then Some i
      else step (i + 1)
    in
    step (x + 1)

  let previous_visible (v : visibility) (x : t) =
    let rec step i =
      if i < 0 then None
      else if is_visible v i then Some i
      else step (i - 1)
    in
    step (x - 1)

  let equal = Int.equal
  let compare = Int.compare

  let preferred_visible_span _ = None
  let row_height = 20

  (* let first_line_of_function lines funinfo : string =
    Lines.line lines funinfo.start_line |> String.trim *)

  let margin_breakpoint (vis : visibility) (row : t) =
    let open Vdom in
    let has_bp = IntSet.mem row vis.breakpoints in
    let bp_color =
      if has_bp then "bg-red-600 opacity-100"
      else "bg-transparent opacity-0 group-hover:opacity-100 group-hover:bg-red-500"
    in
    div
      ~a:[
        class_ "w-3 flex justify-center items-center select-none group";
        onclick (fun _ -> Toggle_breakpoint row);
      ]
      [
        div
          ~a:[
            class_
              ("w-2 h-2 rounded-full cursor-pointer transition-all duration-150 " ^ bp_color);
          ]
          [];
      ]

  let margin_linenumber (_vis : visibility) (row : t) =
    let open Vdom in
    div
      ~a:[class_ "w-6 text-right pr-1 text-[11px] text-gray-500 font-mono select-none leading-none"]
      [text (string_of_int (row))]

  let margin_foldbutton (vis : visibility) (row : t) =
    let open Vdom in
    let text = Lines.line vis.lines row in
    let rec find_function_marker i =
      let j, tok = Interface.get_token text i in
      match tok with
      | Marker (Interface.Function_Definition name) -> Some name
      | EOF -> None
      | _ -> if j >= String.length text then None else find_function_marker j
    in
    match find_function_marker 0 with
    | Some name when StringMap.mem name vis.fun_index ->
        let { start_line; end_line; collapsed } = StringMap.find name vis.fun_index in
        let is_open = not collapsed in
        let icon = if is_open then "▾" else "▸" in
        div
          ~a:[
            onclick (fun _ -> Change_open_status (not is_open, name));
            class_ "w-4 text-center text-gray-600 text-[11px] cursor-pointer hover:text-black select-none";
          ]
          [Vdom.text icon]
    | _ ->
        div ~a:[class_ "w-4"] []

let view (vis : visibility) (row : t) ~cur:_ =
  (*Precede each row by three columns: one for breakpoints, one for line numbers, and one for fold.*)
  let text = Lines.line vis.lines row in
  (None,
  Vdom.div ~a:[Vdom.class_ "flex flex-row items-center text-sm leading-tight"]
    [
      Vdom.div
        ~a:[Vdom.class_ "flex flex-row items-center group hover:bg-gray-50 transition-colors"]
        [
          margin_breakpoint vis row;
          margin_linenumber vis row;
          margin_foldbutton vis row;
        ];
      Vdom.div ~a:[Vdom.class_ "flex-1 pl-1 font-mono"] [Render.render text];
    ])
end


module LV = Line_viewer.Make(Row)

type menu_state = Root | In_goto_fun


type model = {
  lv: LV.model;
  menu_state: menu_state;
  visibility: Row.visibility;
  fun_index : fun_info StringMap.t; (*Function names mapped to their corresponding integer line numbers*)
  fun_trie  : Prefix_tree.t;
}

type initial_data = string

type internal_message =
  | LV_internal of LV.internal_message
  | Keyboard_action of (model -> model)
  | Change_menu_state of menu_state
  | Batch of outgoing_message list
  | Row_message of Row.message


and incoming_message =
  | Internal_in of internal_message

and outgoing_message =
  | Internal_out of internal_message
  | Display_modal : 'a Modal.t * ('a option -> internal_message) -> outgoing_message

let lift_line_viewer_messages = function
  | LV.Internal_out msg ->
      Internal_out (LV_internal msg)
  | LV.Custom_out row_msg ->
      Internal_out (Row_message row_msg)

let build_fun_index text =
  let fun_index = ref StringMap.empty in
  let trie = ref Prefix_tree.empty_set in
  let stack = Stack.create () in
  let open Interface in
  let rec scan i current_line =
    let j, tok = get_token text i in
    match tok with
    | EOF -> ()
    | Marker (Function_Definition name) ->
        Stack.push (Function_Definition name, current_line) stack;
        trie := Prefix_tree.add !trie name;
        scan j current_line
    | Marker ((Instruction _ | Color_marker _) as m) ->
        Stack.push (m, current_line) stack;
        scan j current_line
    | Marker End_marker ->
        begin match Stack.pop_opt stack with
        | Some (Function_Definition name, start_line) ->
            let end_line = current_line in
            let info = {start_line; end_line; collapsed = false} in
            fun_index := StringMap.add name info !fun_index
        | Some _ | None -> ()
        end;
        scan j current_line
    | String (s, a, b) ->
        let lines = ref 0 in
        for k = a to b - 1 do
          if s.[k] = '\n' then incr lines
        done;
        scan j (current_line + !lines)
  in
  scan 0 0;
  !fun_index, !trie




let init text =
  let first_line = 0 in
  let lines = Lines.init text in
  let fun_index, fun_trie = build_fun_index text in
   fun_index |> StringMap.iter (fun name {start_line; end_line} ->
  Js_browser.Console.log Js_browser.console
    (Obj.magic (Printf.sprintf "function %s: %d %d\n" name start_line end_line)));
  let breakpoints = IntSet.empty in
  let visibility = Row.{ lines; fun_index; breakpoints} in
  let lv, cmd = LV.init (visibility, first_line) in
  let cmd = Vdom.Cmd.map lift_line_viewer_messages cmd in
  Vdom.return ~c:[cmd]
    { lv; menu_state = Root; visibility; fun_index; fun_trie }



let update model msg =
  Js_browser.Console.log Js_browser.console (Obj.magic "Sourceview.update");
  match msg with
  | Internal_in (LV_internal msg) ->
    let lv,cmd = LV.update model.lv (LV.Internal_in msg) in
    {model with lv}, Vdom.Cmd.map lift_line_viewer_messages cmd
  | Internal_in (Keyboard_action f) -> Vdom.return @@ f model
      (* let new_model, cmd = f model in
      Vdom.return ~c:[cmd] new_model *)
  (* | Internal_in (Toggle_function name) ->
      let current = try Hashtbl.find model.collapsed name with Not_found -> false in
      Hashtbl.replace model.collapsed name (not current);
      Vdom.return model *)
  | Internal_in (Change_menu_state state) -> Vdom.return {model with menu_state = state}
  | Internal_in (Batch l) ->
    Vdom.return ~c:(l |> List.map Vdom.Cmd.echo) model
  | Internal_in (Row_message (Row.Change_open_status (open_it, name))) ->
      (match StringMap.find_opt name model.fun_index with
      | Some info ->
          let f_info = {info with collapsed = not open_it} in
          let fun_map = StringMap.add name f_info model.visibility.fun_index in
          let new_visibility = {model.visibility with fun_index = fun_map} in
          let lv = LV.update_visibility model.lv new_visibility in
          Vdom.return { model with lv; visibility = new_visibility }
      | None -> Vdom.return model)
  | Internal_in (Row_message (Row.Toggle_breakpoint line)) ->
      let bp =
        if IntSet.mem line model.visibility.breakpoints
        then IntSet.remove line model.visibility.breakpoints
        else IntSet.add line model.visibility.breakpoints
      in
      let visibility = Row.{ model.visibility with breakpoints = bp } in
      let lv = LV.update_visibility model.lv visibility in
      Vdom.return { model with lv; visibility }





let view {lv} =
  Js_browser.Console.log Js_browser.console (Obj.magic "Sourceview.view");
  let open Vdom in
  Vdom.div ~a:[class_ (* overflow-y-auto overflow-x-auto  *)"flex flex-1 flex-col"]  [
    Vdom.map lift_line_viewer_messages @@ LV.view lv;
  ]


module Movement = struct

  let move model target = match target with
    | None -> model
    | Some (target) ->
      {model with lv = LV.go_to_line model.lv target}

  let next_line model =
    move model @@ Row.next_visible model.visibility (LV.current model.lv)

  let previous_line model =
    move model @@ Row.previous_visible model.visibility (LV.current model.lv)


end

let go_to_modal ~title ~suggestions parse_res : outgoing_message =
  let modal = Modal_input_string.input_string ~title ~suggestions in

  let f = function
    | None -> Change_menu_state Root
    | Some result -> begin
        match parse_res result with
        | Some msg -> msg
        | None -> Change_menu_state Root
    end
  in
  Internal_out (Batch [
    Internal_out (Change_menu_state Root);
    Display_modal (modal, f);
  ])

let go_to_function_def (model : model) : outgoing_message =
  let suggestions = Prefix_tree.suggestions model.fun_trie in
  let parse name =
    match StringMap.find_opt name model.fun_index with
    | None -> None
    | Some line ->
        Some (Keyboard_action (fun m ->
          { m with lv = LV.go_to_line m.lv line.start_line }))
  in
  go_to_modal
    ~title:"Go to function:"
    ~suggestions
    parse

let get_menu model =
  let open Transient_menu in
  let stack = match model.menu_state with
    | Root -> []
    | In_goto_fun ->
      [{ title = "Goto ...";
        entries =
          [| {highlight=false; keys=[Char "f"]; desc="function"; action=(go_to_function_def model)};
             {highlight=false; keys=[Escape];   desc="cancel"; action=(Internal_out (Change_menu_state Root))};
          |]}]
  in
  let entries = [|
    {highlight=false; keys=[Char "p"; ArrowUp];    desc="previous-line"; action=Internal_out(Keyboard_action(Movement.previous_line))};
    {highlight=false; keys=[Char "n"; ArrowDown];  desc="next-line";     action=Internal_out(Keyboard_action(Movement.next_line))};
    {highlight=(model.menu_state = In_goto_fun); keys=[Char "g"]  ; desc="goto ..."; action=Internal_out(Change_menu_state In_goto_fun)};

  |]
  in
  Transient_menu.{title = "Source"; entries}::stack
