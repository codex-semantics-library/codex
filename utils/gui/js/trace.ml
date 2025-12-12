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

open Vdom
open Vdom_ext
open Static_data

let row_height = 20;;

module Severity = struct
  (* These colors work for font, but not for normal color
     debug = light blue
     info = black
     notice = black or green bold
     Warning = orange bold
     Error = red bold
     Critical = bordeaux bold

  *)
  let color ~severity = match severity with
    | 7 -> "#888888"            (* Debug *)
    | 6 -> "#000000"            (* Info *)
    (* | 3 -> "#008858"            (\* Notice *\) *)
    (* | 4 -> "#b6532f"            (\* Warning *\) *)
    | 5 -> "lightgreen"         (* Notice *)
    | 4 -> "orange"             (* Warning *)
    | 3 | 2 | 1 -> "#e00033"     (* Error, Critical, beyond critical *)
    | _ -> assert false

  let to_string = function
    | 7 -> "Debug"
    | 6 -> "Info"
    | 5 -> "Notice"
    | 4 -> "Warning"
    | 3 -> "Error"
    | 2 -> "Critical"
    | _ -> assert false         (* Note: 1  = always displayed *)

  let notice = 5
end

module Trace_row = 
  Unique_prefix.Make(struct let name = "trace_row" end)
module Trace_container = 
  Unique_prefix.Make(struct let name = "tracecontainer" end)


module Interval_node =
struct

  (* t is a node with the path leading to this node. Note that this
     avoids having to keep a pointer to the parent for every node (we
     keep it only for the displayed ones). *)
  type 'a t = {n:'a trace_node; kind: parent}
  and parent =
    | Root
    | Inside of {idx:int; parent:[`interval_node] t} (* We add idx to the path. *)

  type any = Any: 'a t -> any (* [@@unboxed]   *)
  let equal a b = a.n.id = b.n.id
  let compare a b = Int.compare a.n.id b.n.id
  let hash a = a.n.id

  let indentation: type a. a t -> int = fun x ->  
    let rec aux: type a. a t -> int -> int = fun x acc ->
      match x.kind with
      | Root -> acc
      | Inside{parent} -> aux parent (acc + 1)
    in aux x 0

  let is_ancestor: type a b. a t -> b t -> bool =
    fun a b ->
    let ia = indentation a in
    let ib = indentation b in
    if ia > ib then false
    else begin
      let rec take: type a. a t -> int -> bool =
        fun x n ->
          if n = 0 then
            a.n.id = x.n.id 
          else match x.kind with
            | Root -> assert false
            | Inside{parent} -> take parent (n - 1)
      in take b (ib - ia)
    end

  let root = {n=static_data.trace_data.trace_root_node;kind=Root}
  let is_root n = match n.kind with Root -> true | _ -> false
  let is_leaf tn = (tn.n.children = [||])

  exception Cannot_go of string

  let parent t = match t.kind with
    | Root -> raise (Cannot_go "No parent for root")
    | Inside {parent} -> parent

  let child t idx =
    let n = t.n in
    if idx < 0 || idx >= Array.length n.children
    then raise (Cannot_go "No such children")
    else
      let (Any_trace_node n') = n.children.(idx) in
      Any{n=n';kind=Inside {idx;parent=t}}

  let first_child x = child x 0
  let last_child x = child  x ((Array.length x.n.children) - 1)

  type 'a kind =
    | Interval: interval_node kind
    | Single: single_node kind

  let kind_of: type a. a t -> a kind = fun x -> match x.n.result with
    | No_result -> Interval | Result _ -> Interval | Single _ -> Single

  let next_sibling x =
    match x.kind with
    | Root -> raise (Cannot_go "Cannot go after the root")
    | Inside {idx;parent} -> child parent (idx + 1)

  let previous_sibling x =
    match x.kind with
    | Root -> raise (Cannot_go "Cannot go before the root")
    | Inside {idx;parent} -> child parent (idx - 1)



  let view_start {n} =
    Vdom.text @@ "[" ^ n.category ^ "] " ^ n.content

  let view_end {n} =
    let result = match n.result with
      | Result str -> str | No_result -> "<unknown>"
    in 
    (* Note: there is an invisible unicode variant selector 15
       (U+FE0E) to request text style instead of emojy style here. *)
    Vdom.text @@ "↪︎ " ^ result

  let severity (node:[`single_node] t) = 
    let Single {severity}  = node.n.result in
    severity
    

  let view_single (node:[`single_node] t) =
    Vdom.text @@ "[" ^ node.n.category ^ "] " ^ node.n.content

  let exists_child (Any node) f =
    let exception Exit in
    match kind_of node with
    | Single -> false
    | Interval ->
      try 
        node.n.children |> Array.iteri (fun i _ ->  if f (child node i) then raise Exit);
        false
      with Exit -> true

end

(** {2 Some filters} *)

module Filter = struct

  type 'a tmp = 'a Interval_line_viewer.Filter.t = { short_name: string; f: 'a -> bool; unique_name: string;}
  type t = Interval_node.any Interval_line_viewer.Filter.t

  let all_locations =
    { short_name = "all loc"; unique_name = "all locations";
      f = function Interval_node.Any{n={loc = Some _}} -> true | _ -> false }

  let all_instructions =
    let open Syntax_tree.Location_identifier in
    { short_name = "all inst"; unique_name = "all instructions";
      f = function Interval_node.Any{n={loc = Some (Any(Address,_))}} -> true | _ -> false }

  let category string =
    { short_name = "category: " ^string; unique_name = "category: " ^ string;
      f = function Interval_node.Any{n={category}} -> category = string }

  let severity_above target =
    { short_name = "severity >= " ^ (Severity.to_string target); unique_name = "severity >= " ^ (Severity.to_string target);
      f = function Interval_node.Any{n={result=Single{severity}}} -> severity <= target | _ -> false }
      
  let location_at address expr_path expr =
    let join_with_slash lst =
      String.concat "/" (List.map string_of_int lst) 
    in 
    let addr_str = Printf.sprintf "%#Lx" address in
    let name = String.cat addr_str @@ "-" in 
    let name = String.cat name @@ join_with_slash expr_path in 
    let name = String.cat ": " name  in 
    let short_name = Syntax_tree.string_of_expr expr in 

    { short_name; unique_name =  String.cat (short_name) name ;
      f = function
        | Interval_node.Any {n={loc = Some (Syntax_tree.Location_identifier.Any (Address, Int64 addr'))}} ->
            addr' = address
        | Interval_node.Any {n={loc = Some (Syntax_tree.Location_identifier.Any (DbaInstruction, Inside {locid; _}))}} ->
            Syntax_tree.Location_identifier.(match locid with
              | Address, Int64 addr' -> addr' = address
              | _ -> false)
        | _ -> false }
end

module Interval_line_viewer =
  Interval_line_viewer.Make
    (struct let row_height = row_height end)
    (Severity)
    (Interval_node)


(* We represent the menu_state using a simple automaton. *)
type open_location_instruction_menu_state =
  | Root

type open_location_menu_state =
  | Root
  | In_open_location_instruction of open_location_instruction_menu_state
  | In_open_location_microinstruction of unit
  | In_open_location_expression of unit

type open_menu_state =
   | Root
   | In_open_location of open_location_menu_state

type menu_state =
  | Root
  | In_open of open_menu_state

type model = {
  ilv_model: Interval_line_viewer.model;
  menu_state:menu_state;
}

type internal_message =
  | Nop
  | Batch of outgoing_message list
  | Line_viewer of Interval_line_viewer.internal_message
  | Set_filter_activity of Filter.t * bool
  | Remove_filter of Filter.t
  | Add_filter of Filter.t
  | Keyboard_action of (model -> model)
  | Menu_state of menu_state

and incoming_message =
  | Internal_in of internal_message
  | Update_filter of (int64 * Syntax_tree.expr_path * Syntax_tree.expr) option

and outgoing_message =
  | Internal_out of internal_message
  | Display_modal: 'a Modal.t * ('a option ->  internal_message) -> outgoing_message      
  | Disassembly_goto of int64
  | Sync_selection of int64

let lift_line_viewer_messages = function
  | Interval_line_viewer.Internal_out msg ->
      Internal_out (Line_viewer msg)
  | Interval_line_viewer.Goto_disasm row_node -> 
      let add = row_node.n.loc in
      match add with 
      | None -> Internal_out Nop
      | Some (Syntax_tree.Location_identifier.Any (Address, Int64 addr)) ->
          Disassembly_goto addr
      | Some _ ->
          Internal_out Nop
       
type initial_data = Interface.marshalled
let init (marshalled:Interface.marshalled) =
  let initial_filters =
    [(Filter.all_locations, false);
     (Filter.severity_above Severity.notice, true);
    ]
  in
  let root = Interval_node.(Any root) in
  let ilv_model,cmd = Interval_line_viewer.init (initial_filters,root) in
  let cmd = Vdom.Cmd.map lift_line_viewer_messages cmd in
  Vdom.return ~c:[cmd]
  {ilv_model; menu_state = Root }

(* MAYBE: A color by filter-type: blue for locations, orange for
   severity, etc.  And maybe highlight elements that match a filter,
   maybe using the background.*)
let filter_button (Filter.{short_name;unique_name} as f) checked =
  Vdom_ext.button ~a:[ class_ "inline-flex items-center gap-1 text-white bg-blue-700 hover:bg-blue-800
                    focus:outline-none focus:ring-4 focus:ring-blue-300 font-medium rounded-full
                    text-sm px-2 py-1 text-center me-2 mb-2 dark:bg-blue-600
                    dark:hover:bg-blue-700 dark:focus:ring-blue-800";
         Vdom.attr "title" unique_name ]
    [ Vdom.input ~a:[type_ "checkbox"; Vdom.bool_prop "checked" checked; class_ "w-3 h-3 align-middle";
                     Vdom.onchange_checked (fun newstatus -> Internal_out(Set_filter_activity(f,newstatus)))] [];
      Vdom.text short_name;
      Vdom_ext.span ~a:[class_ "cursor-pointer m-0 text-gray-300 hover:text-white";
                        Vdom.onclick (fun _ -> Internal_out(Remove_filter f))
                       ]
        [Vdom.text "×"]
    ]


let view_filters model =
  div ~a:[]
    (Interval_line_viewer.get_filters model.ilv_model |> List.map @@ fun (f,active) ->
     filter_button f active)

let view model =
  let open Vdom in
  div ~a:[class_ (* overflow-y-auto overflow-x-auto  *)"flex flex-1 flex-col"]
    [ view_filters model;
      Vdom.map lift_line_viewer_messages @@ Interval_line_viewer.view model.ilv_model ]
      


let update (model:model) message =
  let trace = model in
  match message with
  | Internal_in (Line_viewer m) ->
    let ilv_model, cmd = Interval_line_viewer.update model.ilv_model (Interval_line_viewer.Internal_in m) in
    let cmd = Vdom.Cmd.map lift_line_viewer_messages cmd in
    {model with ilv_model}, cmd
  | Internal_in Nop -> Vdom.return model
  | Internal_in(Batch l) ->
    Vdom.return ~c:(l |> List.map Vdom.Cmd.echo) model

  | Internal_in(Set_filter_activity(f,status)) ->
    Vdom.return
      { model with ilv_model = Interval_line_viewer.add_filter model.ilv_model (f,status) }

  | Internal_in(Remove_filter f) ->
    Vdom.return { model with ilv_model = Interval_line_viewer.remove_filter model.ilv_model f }

  | Update_filter None -> assert false
  | Update_filter (Some (address, expr_path, expr))  ->
      let f = Filter.location_at address expr_path expr in
      Vdom.return { model with
        ilv_model = Interval_line_viewer.add_filter model.ilv_model (f, true) }
  
  | Internal_in(Keyboard_action f) -> Vdom.return @@ f trace
  | Internal_in (Menu_state m) -> Vdom.return @@ {model with menu_state = m }
  | Internal_in (Add_filter f) ->
    (* Do not add filters that are already there (but maybe enable them). *)
    Vdom.return
      { model with ilv_model = Interval_line_viewer.add_filter model.ilv_model (f,true) }

let ask_category_modal =
  let suggestions = Prefix_tree.suggestions static_data.trace_data.categories in
  let title = "Select category" in
  let modal = Modal_input_string.input_string ~title ~suggestions in
  let f = function
    | None -> Nop
    | Some cat -> Add_filter(Filter.category cat)
  in
  Internal_out (Batch [Internal_out (Menu_state Root); Display_modal(modal,f)])

let get_menu model =
  let open Transient_menu in

  let act m = Internal_out(Menu_state m) in
  let act_open m = act @@ In_open m in
  let act_open_location m = act_open @@ In_open_location m in
  let act_open_location_instruction m = act_open_location @@ In_open_location_instruction m in
  let act_open_location_expression m = act_open_location @@ In_open_location_expression m in  
  let act_open_location_microinstruction m = act_open_location @@ In_open_location_microinstruction m in

  let add_filter f = Internal_out (Batch [Internal_out (Menu_state Root);
                                          Internal_out (Add_filter f)]) in


  let open_location_instruction_menu (menu_state:open_location_instruction_menu_state) =
    let stack = [] in
    { title = "Instruction";
      entries =
        [|
          {highlight=false; keys=[Char "i"];   desc="all"; action=add_filter Filter.all_instructions};
          {highlight=false; keys=[Escape];   desc="cancel"; action=act_open_location Root};
        |]}::stack
  in
  
  let open_location_menu (menu_state:open_location_menu_state) =
    let stack = match menu_state with
      | Root -> []
      | In_open_location_instruction oli -> open_location_instruction_menu oli
      | _ -> []
    in
    { title = "Location";
      entries =
        [|
          {highlight=false; keys=[Char "l"];   desc="all"; action=add_filter Filter.all_locations};
          {highlight=(match menu_state with In_open_location_instruction _ -> true | _ -> false);
           keys=[Char "i"]; desc = "instruction"; action = act_open_location_instruction Root};
          {highlight=(match menu_state with In_open_location_microinstruction _ -> true | _ -> false);
           keys=[Char "m"]; desc = "microinstruction"; action = act_open_location_microinstruction ()};
          {highlight=(match menu_state with In_open_location_expression _ -> true | _ -> false);
           keys=[Char "e"]; desc = "expression"; action = act_open_location_expression ()};          
          {highlight=false; keys=[Escape];   desc="cancel"; action=act_open Root};
        |]}::stack
  in

  let open_menu (menu_state:open_menu_state) =
    let stack = match menu_state with
      | Root -> []
      | In_open_location ol -> open_location_menu ol
    in
    { title = "Open";
      entries =
        [| {highlight=(match menu_state with In_open_location _ -> true | _ -> false);
              keys=[Char "l"]; desc = "location"; action = act_open_location Root};
           {highlight=false;
              keys=[Char "c"]; desc = "category"; action = ask_category_modal};
           {highlight=false; keys=[Escape];   desc="cancel"; action=act Root};
        |]}::stack
  in

  let main_menu menu_state =
    let entries = Interval_line_viewer.menu_entries model.ilv_model in
    let entries =
      entries |> List.map (fun entry -> {entry with action = lift_line_viewer_messages entry.action }) in
    let entries = entries @
                  [
                    {highlight=(menu_state <> Root); keys=[Char "o"]; desc="open filters"; action=Internal_out(Menu_state (In_open Root))};
                    {highlight=false; keys=[Char "r"];             desc="remove filter";        action=Internal_out(Keyboard_action(fun _ -> assert false))};
                  ]
    in
    { title = "Trace";
      entries = Array.of_list entries }
  in

  let stack menu_state = match menu_state with
    | Root -> [main_menu menu_state]
    | In_open m -> (main_menu menu_state)::(open_menu m)
  in
  stack model.menu_state

(* TODO: Display, in the disassembly view, all the possible values for
   an expression; and, when clicking on a valuation, filter on the
   traces where this expression got this value. *)
