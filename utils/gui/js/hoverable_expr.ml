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
open Syntax_tree

module type Tooltip_information = sig
  type t
  val root: t
  val get: t -> string option      (* Evaluation result for the current node. *)
  (* val child: t -> int -> t      (\* Evaluation result for child i. *\) *)
  val find_child: t -> (* Syntax_tree.expr *) int -> t option
end

let find_child (module E : Tooltip_information) idx =
  match E.find_child E.root idx with
  | None -> None
  | Some child ->
    let module E' = struct include E let root = child end in
    Some(module E':Tooltip_information)

type expr_path = Syntax_tree.expr_path

let id_count = ref 0
let top_expr_id id = "hoverable_expr_" ^ (string_of_int id)

module Expr = struct

  type internal_message =
    | Hovered of {path:expr_path; (* Hovered position in the expr_tree.  *)
                  depth:int;      (* List.length expr_path. *)
                  rect: Js_browser.Rect.t;    (* Bounding box for the containing expression. *)
                 }
    | Unhovered of {depth:int} (* Unhover up to (including) depth. *)

  type incoming_message =
    | Incoming_internal of internal_message

  type outgoing_message =
    (* Note: we should only be providing an expr_path. *)
    | ClickInstr of expr_path * Syntax_tree.expr
    | Outgoing_internal of internal_message



  (* Information if we are hovered. *)
  type hovered = {
    path: expr_path;              (* [] if the element is highlighted, or path to its children otherwise. *)
    rect: Js_browser.Rect.t
  }

  type model = {expr:expr;
                (* For each position in the "format string" which is the
                   top-level expr, is it hovered. *)
                hovered: hovered option;
                unique_id: int;
                ti:(module Tooltip_information) option; (* Type information, or None if no info. *)
               }


  let init (ti,expr) =
    incr id_count;
    Vdom.return {expr; hovered = None;unique_id=(!id_count);ti}
  let update m = function
    | Incoming_internal Hovered{path;depth;rect} ->
      assert(depth = List.length path);
      (* Js_browser.Console.log Js_browser.console (Obj.magic "In updated hovered: new depth"); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic  (string_of_int depth)); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic "current len"); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic  (string_of_int (List.length path))); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic "print done"); *)

      (* Try to reuse rect to avoid flickeing? *)
      let rect =
        match m.hovered with
        | None -> rect
        | Some{rect} -> rect in
      let hovered = Some {path;rect} in
      (* assert(depth > List.length m.hovered_path); (\* We should be extending the current path. *\) *)
      (* let outgoing_message = HoverInstr(Some(m.address,global_path)) in *)
      { m with hovered}
    | Incoming_internal Unhovered{depth} ->
      (* We are no longer at depth x (so we will be at depth - 1).
         Because events can be received in different orders, either we
         already know that and do nothing, or limit the depth of the
         current expr_path we are in to depth. *)
      match m.hovered with
      | None -> m (* Already unhovered. *)
      | Some hovered -> begin     (* Was hovered. *)
          let len = List.length hovered.path in
          (* Js_browser.Console.log Js_browser.console (Obj.magic "In updated unhovered: depth"); *)
          (* Js_browser.Console.log Js_browser.console (Obj.magic  (string_of_int depth)); *)
          (* Js_browser.Console.log Js_browser.console (Obj.magic "current len"); *)
          (* Js_browser.Console.log Js_browser.console (Obj.magic  (string_of_int len)); *)
          (* Js_browser.Console.log Js_browser.console (Obj.magic "print done"); *)

          (* If events arrive in order, we should be removing items for the
             current path. But the code is robust if this does not hold. *)
          assert(depth <= len);

          assert(depth >= 0);
          (* Take element until the size is depth. *)
          let rec take depth l = match depth, l with
            | _, [] -> []
            | 0, _ -> []
            | n, hd::tl -> hd::(take (depth - 1) tl) in
          (* If we are no longer at depth 0, the expression is not
             hovered anymore. *)
          if depth = 0
          then
            (* let outgoing_message = HoverInstr None in *)
            {m with hovered = None}
          else
            (* We remove everything above (and including) level depth. *)
            let hovered_path = take (depth - 1) hovered.path in
            (* let outgoing_message = *)
            (*   HoverInstr (Some(m.address,global_path)) in *)
            { m with hovered = Some{path=hovered_path;rect=hovered.rect}}
        end

  let update model msg = Vdom.return @@ update model msg

  module Make(Param:sig
      type message
      val inject: outgoing_message -> message
    end) = struct

    open Param
    let tooltip_counter = ref 0


    let show_tooltip model depth path  =
      (* Js_browser.Console.log Js_browser.console (Obj.magic "In show_tooltip: depth"); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic  (string_of_int depth)); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic "In show_tooltip: len"); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic  (string_of_int (List.length path))); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic  path); *)

      let rect =
        let open Js_browser in
        (* We are hovering on it, so it exists, so we can use Option.get. *)
        let elt = Option.get @@ Document.get_element_by_id document (top_expr_id model.unique_id) in
        Element.get_bounding_client_rect elt
      in

      assert(depth = List.length path);
      (inject (Outgoing_internal(Hovered{path;depth;rect})))

    let hide_tooltip depth =
      (inject (Outgoing_internal(Unhovered{depth})))


    (* let update_filter path = inject (HoverInstr (Some path)) *)
    (* Messages.Trace (Messages.Trace.Update_filter (Some path)) *)

    (* Function to create hoverable expr (with nesting)*)
(*
   We accumulate the path in [expr_path], but in reverse order (the last visited element is on top).
   This is used in callback function to identify the subexpresson where we are.
   This is built for all nodes.

   Some of the paths in the tree may be hovered, they are identified by [hovered_path]:
   hovered_path = None = this expression and its subexpressions are not hovered;
   hovered_path = Some [] = this expression is hovered, but not its sub-expressions;
   hovered_path = Some l = A subexpression of this expression is hovered (and thus, this one too).

   hovered is in the correct order, this is the path that we already
   know.

   depth = the depth in the tree. The root is 1 (0 means no expression).


   We run this twice:
   -with_tooltip: redisplay that show the tooltips
   -without tooltip: set the hover and the underlying messages.

*)
    let hoverable_expr model with_tooltip rest =
      let rec aux expr ei (expr_path,depth,hovered_path) =
        let (module TI:Tooltip_information) = ei in
        assert( depth >= 0);
        assert( depth = List.length expr_path);

        let is_hovered =
          match hovered_path with
          | None -> false
          | Some _ -> true
        in

        (* This prepares the triple (expr_path,depth,hovered_path) for the
           pos-th children. *)
        let sub pos =
          let hovered_path' =
            match hovered_path with
            | Some(x::tl) when pos = x -> Some tl (* Good element. *)
            | None               (* Parent already not hovered. *)
            | Some _ -> None                 (* Other children. *)
          in
          (pos::expr_path,depth+1,hovered_path')
        in

        let value = match TI.get TI.root with
          | Some r -> r
          | None -> "None" in

        (* Create uid for the tooltip :*)
        let tooltip_id = Printf.sprintf "tooltip_%d" !tooltip_counter in
        incr tooltip_counter;
        (* match highlight color and tooltip color *)
        let color =
          match depth mod 5 with
          | 0 -> "#ffbe0b"
          | 1 -> "#fb5607"
          | 2 -> "#ff006e"
          | 3 -> "#8338ec"
          | 4 -> "#3a86ff"
          | _ -> "#06d6a0"
        in

        let hover_attrs =
          (* If hovered, highlight based on color. *)
          let classes =
            let classes = "cursor-pointer relative inline-block" in
            if is_hovered
            then  "bg-[" ^ color ^ "] " ^ classes
            else classes
          in
          [ class_ (classes);
            onmouseenter ~stop_propagation:() (fun _ -> show_tooltip model depth @@ List.rev expr_path);
            onmouseleave (fun _ -> hide_tooltip depth);
            onclick ~stop_propagation:() (fun _ ->
                inject (ClickInstr (List.rev expr_path, expr)));
          ] in

        (* If is hovered, we show the tooltip with the information. *)
        let tooltip =
          (* Calculate tooltip position based on depth *)
          let tooltip_position = -((depth + 1) * 40) in
          if is_hovered && with_tooltip then
            div ~a:[
              class_ "absolute bg-white border border-gray-300 p-2 rounded shadow-lg text-center";
              attr "id" tooltip_id;
              style "min-width" "100%";
              style "left" "50%";
              style "bottom" (string_of_int tooltip_position ^ "px");
              style "left" "0";
              style "white-space" "nowrap";
              style "z-index" "10";
              style "border-left" ("4px solid " ^ color);
            ] [
              Vdom.text value
            ]
          else Vdom.fragment []
        in

        (* Display the element, and recurse on sub-expressions. *)
        match expr with
        | Ar0 {s} -> span ~a:hover_attrs [Vdom.text s; tooltip]
        | Ar1 {format; arg} ->
          let arg_element = match find_child ei 0 with
            | Some next_trace -> aux arg next_trace (sub 0)
            | None -> Vdom.text (string_of_expr arg) in
          span ~a:hover_attrs [ Vdom.text format.prefix; arg_element; Vdom.text format.suffix; tooltip ]
        | Ar2 {format={prefix;middle;suffix}; arg1; arg2} ->
          let arg1_element = match find_child ei 0 with
            | Some next_trace -> aux arg1 next_trace (sub 0)
            | None -> Vdom.text (string_of_expr arg1) in
          let arg2_element = match find_child ei 1 with
            | Some next_trace -> aux arg2 next_trace (sub 1)
            | None -> Vdom.text (string_of_expr arg2) in
          span ~a:hover_attrs [ Vdom.text prefix; arg1_element;
                                Vdom.text middle; arg2_element;
                                Vdom.text suffix; tooltip]
        | Ar3 _ -> assert false       (* TODO *)
        | Ite {cond; then_expr; else_expr} ->
          (* if the analyzer knows wether cond=0 or not, it will only evaluate one expr among then and else*)
          (* so can only show the value of one of them *)
          let cond_element = match find_child ei 0 with
            | Some next_trace -> aux cond next_trace (sub 0)
            | None -> Vdom.text (string_of_expr cond) in
          let then_element = match find_child ei 1 with
            | Some trace_node -> aux then_expr trace_node (sub 1)
            | None -> Vdom.text (string_of_expr then_expr)
          in
          let else_element = match find_child ei 2 with
            | Some trace_node -> aux else_expr trace_node (sub 1)
            | None -> Vdom.text (string_of_expr else_expr)
          in

          span ~a:hover_attrs [
            span [
              cond_element;
              Vdom.text " ? ";
              then_element;
              Vdom.text " : ";
              else_element
            ];
            tooltip
          ]
      in aux rest


    let view model =
      let expr = model.expr in
      let hovered_path = match model.hovered with
        | None -> None
        | Some({path}) -> Some path
      in
      let inside put_tooltip =
        let hovered_path = if put_tooltip then hovered_path else None in
        match model.ti with
        (* match find_child model.ti expr with *)
        | Some next_trace -> hoverable_expr model put_tooltip expr next_trace ([],0,hovered_path)
        | None -> Vdom.text (string_of_expr expr)
      in

      (* To reuse the existing code and having to compute left/right, we
         change the whole expression to a fixed position.

         It would be best to just compute the positions of the tooltip in
         an absolute position, this would probably be more robust. To do
         that we have to get the bounding box for every sub
         expression.  *)
      let attrs = [attr "id" (top_expr_id model.unique_id)(* ;class_ "border border-red-500" *)] in
      match model.hovered with
      | None ->
        span ~a:attrs [
          inside false
        ]
      | Some {rect} ->
        let top,left,width,height = Js_browser.Rect.(top rect, left rect, width rect, height rect) in
        (* This avoids parsing errors. *)
        let string_of_float f = Printf.sprintf "%f" f in
        let new_attrs =
          [(* style "left" ((string_of_int @@ int_of_float left) ^ "px"); *)
            (* style "top" ((string_of_int @@ int_of_float top) ^ "px"); *)
            (* style "width" ((string_of_int @@ 1 + int_of_float width) ^ "px"); *)
            (* style "height" ((string_of_int @@ 1 + int_of_float height) ^ "px"); *)
            style "left" ((string_of_float left) ^ "px");
            style "top" ((string_of_float top) ^ "px");
            style "width" ((string_of_float width) ^ "px");
            style "height" ((string_of_float height) ^ "px");

            style "position" "fixed";
            style "z-index" "1000"] (* @ attrs *)
        in


        (* We create a first span with a unique identifier. This is where
           the element is supposed to be.

           If the element is hovered, we redo it on top find where it is using getBoundingBoxRec.
           Then, we use a fixed positioning for this element.

           We do this so that the tooltips can escape, wherever they are.

           XXX: In the second run, do not set any callbacks.
        *)
        Vdom.fragment [
          span ~a:attrs [
            inside false
          ];
          span ~a:new_attrs [
            inside true
          ]
        ]

  end

  let view (type a) model inject =
    let module M = Make(struct type message = a let inject = inject end) in
    let open M in
    view model
end

(* A "instr" is just a text that contains hoverable expressions, but is not
   itself hoverable. We reuise Syntax_tree.expr as "instr". *)
module Instr = struct

  (* We keep one state per expression that appear in the state. *)
  type model = {instr:expr;
                expr_models: Expr.model array;
                ti:(module Tooltip_information);
               }

  (* Internal message + position in the instruction. *)
  type internal_message = int * Expr.internal_message

  type incoming_message =
  | Incoming_internal of internal_message

  type outgoing_message =
    | ClickInstr of Syntax_tree.expr_path * Syntax_tree.expr
    | HoverInstr of Syntax_tree.expr_path option
    | Outgoing_internal of internal_message

  type initial_data = (module Tooltip_information)*Syntax_tree.expr

  let init (ti, instr) =
    let instr,expr_models, cmd =
      match instr with
      | Ar0 _ -> instr, [||], Vdom.Cmd.batch []
      | Ar1 {arg} ->
        let model, cmd = Expr.init (find_child ti 0,arg) in
          instr, [| model |], cmd
      | Ar2{arg1;arg2} ->
        let model1, cmd1 = Expr.init (find_child ti 0,arg1) in
        let model2, cmd2 = Expr.init (find_child ti 1,arg2) in
        assert(cmd1 = Vdom.Cmd.Batch []);
        instr, [|model1;model2|], cmd2
      | Ar3 _ -> assert false
      | Ite _ -> assert false
    in {ti;expr_models;instr}, cmd

  (** Update the right model according to the arity. *)
  let update model (msg:incoming_message) = match msg with
    | Incoming_internal (pos,msg) ->(
      match 
       Expr.update (model.expr_models.(pos)) (Expr.Incoming_internal msg)
  with emodel, Vdom.Cmd.Batch [] -> 
      let expr_models = begin
        let array = Array.copy model.expr_models in
        array.(pos) <- emodel;
        array
      end in
      Vdom.return {model with expr_models}
        |_ -> assert false)

    (* Function to display a dba instruction (type instr) *)
    let view {instr;ti;expr_models} =

      let inject (pos:int) (msg:Expr.outgoing_message) =
        (* Change or wrap the outgoing message *)
        let msg = match msg with
          | ClickInstr (path, expr) -> ClickInstr (pos::path, expr)
          | Outgoing_internal msg -> Outgoing_internal (pos,msg)
        in msg
      in

    let attrs = [] in
    let elt = match instr with
      | Ar0 {s} -> span ~a:attrs [Vdom.text s]
      | Ar1 {format; arg} ->
        let arg_element = Expr.view expr_models.(0) (inject 0) in
        span ~a:attrs [ Vdom.text format.prefix; arg_element; Vdom.text format.suffix ]
      | Ar2 {format={prefix;middle;suffix}; arg1; arg2} ->
        let arg1_element = Expr.view expr_models.(0) (inject 0) in
        let arg2_element = Expr.view expr_models.(1) (inject 1) in
        span ~a:attrs [ Vdom.text prefix; arg1_element;
                        Vdom.text middle; arg2_element;
                        Vdom.text suffix]
      | _ -> assert false in
    elt
end

include Instr
