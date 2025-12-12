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

(* Custom graphviz component. We also handle color changes directly,
   without a full recomputation of the graph. *)
type initial_data = string

type model = {dot:string}

type elt_kind =
  | Cluster
  | Node
  | Edge
  | Nothing

type event_type =
  | Mouseenter
  | Mouseleave
  | Mousemove
  | Click

type outgoing_message =
  | Event of { kind:elt_kind; elt:Js_browser.Element.t;event_type:event_type}

type Vdom.Custom.t += Graphviz of model
type Vdom.Custom.event += Outgoing_message of outgoing_message

let set_background_color id color =
  match Js_browser.(Document.get_element_by_id document id) with
  | None -> assert false
  | Some node ->
    (* We have to find the shape inside of the node; this is where is the color. *)
    let shape = Js_browser.Element.query_selector node "polygon, ellipse, rect" in
    Js_browser.Element.set_attribute shape "fill" color

(* Note: you can chose classes and ids for graphviz nodes.  MAYBE:
   This could simplify setup for listeners etc. *)



(* Create a custom event handler, following the documentation in:

   https://discuss.ocaml.org/t/how-do-you-handle-imperative-web-apis-in-lexifis-ocaml-vdom/2491.

   We need to handle both the display of the initial state (where the
   state is defined in Vdom.Custom.t), and updates to this state.

   For this, we need to define a custom handler, that initially
   creates the DOM element that will be displayed, and handles updates
   to the state by a sync function. The sync function need to compute
   a diff between the old and new state, so we save the old state in a
   reference. *)
let graphviz_handler ctx =
  let open Js_of_ocaml in
  let open Vdom_blit in
  function
  | Graphviz({dot=initial_dot}) ->
    (* Create the DOM element in which we draw the graph, and setup
       event handlers on it that will send the [outgoing_message]s. *)
    let div =
      let open Js_browser in
      let div = Js_browser.Document.create_element Js_browser.document "div" in
      let style = Js_browser.Element.style div in

      (* We use the bounding box to know the size of the SVG, so the div should take all the space. *)
      (* TODO: I use vh to that the height corresponds to the height of the view port,
         but a better solution would be to have a vertical flexbox here. *)
      Style.set_width style "100%";
      (* Style.set_height style "100%"; *)
      (* Style.set_height style "100vh"; *)
      Style.set style "flex" "1";
      (* Style.set style "display" "flex"; *)
      (* Style.set style "flex-direction" "column";       *)
      Style.set style "max-width" "100%";
      Style.set style "max-height" "100%";
      Style.set style "overflow" "auto";

      let handle_event event_type (event:Js_browser.Event.t) =
        Js_browser.Event.stop_propagation event;
        let event_target = Js_browser.Event.target event in
        (* Graphviz displays the elements it a flat hierarchy. This
           guaranteed that only one of these elements will be
           selected. *)
        let get_elt selector : Js_browser.Element.t option =
          (event_target |> Obj.magic)##closest selector |> Js.Opt.to_option
        in
        let maybe_node = get_elt ".node" in
        let maybe_cluster = get_elt ".cluster" in
        let maybe_edge = get_elt ".edge" in

        let send_message msg =
          Vdom_blit.Custom.send_event ctx (Custom (Outgoing_message msg));
        in

        match (maybe_node, maybe_edge, maybe_cluster) with
        | Some node, None, None ->
          send_message (Event {kind=Node;elt=node;event_type})
        | None, Some edge, None ->
          send_message (Event {kind=Edge;elt=edge;event_type})
        | None, None, Some cluster ->
          send_message (Event {kind=Cluster;elt=cluster;event_type})
        | None, None, None ->
          send_message (Event {kind=Nothing;elt=(Obj.magic event_target);event_type})
        | _ -> failwith "Too many/too few event type"
      in
      Js_browser.Element.add_event_listener div Js_browser.Event.Click (handle_event Click) true;

      (* We send events when we move the mouse, or when we leave the div (to unhighlight things) *)
      Js_browser.Element.add_event_listener div Js_browser.Event.Mouseleave (handle_event Mouseleave) true;
      Js_browser.Element.add_event_listener div Js_browser.Event.Mousemove (handle_event Mousemove) true;
      div
    in

    let gv = Js.Unsafe.(fun_call (js_expr "d3_graphviz") [|inject div|]) in
    (* We have an error on transitions if we do not disable tweenShapes. *)
    let gv = Js.Unsafe.meth_call gv "tweenShapes" [| Js.Unsafe.inject Js._false |] in

    (* There wre display performance issues on large graphs with
       tweenPaths, that disappear without it.  *)
    let gv = Js.Unsafe.meth_call gv "tweenPaths" [| Js.Unsafe.inject Js._false |] in
    let gv = gv ## fit true in

    let render gv dot =
      (gv##renderDot (Js.string dot));
    in

    let r_dot = ref initial_dot in         (* Save the last printed dot. *)

    (* Set up a resize observer, to notify graphviz to make the SVG
       size correspond to the enclosing div size. Will be also
       triggered on the first insertion of the div, and will thus do
       the first rendering too. *)
    let () = begin
      let callback = Js.wrap_callback (fun entries ->
          let entry = Js.array_get entries 0 in
          let rect = Js.Unsafe.get entry "contentRect" in
          let width = Js.Unsafe.get rect "width" in
          let height = Js.Unsafe.get rect "height" in
          (* Js_browser.Console.log Js_browser.console (Obj.magic "re-render");           *)
          let gv = gv ## width width in
          let gv = gv ## height height in

          (* Re-render using the new height and width *)
          render gv !r_dot) in
      let resize_observer =
        Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "ResizeObserver") [| Js.Unsafe.inject callback |]
      in
      ignore(resize_observer ## observe div)
    end in

    let sync x  =
      (* Read latest version. *)
      let old_dot = !r_dot in
      match x with
      | Graphviz({dot=new_dot}) ->
        if old_dot == new_dot || String.equal new_dot old_dot then true
        else begin
          (* Save the update. *)
          r_dot := new_dot;
          (* We create a "transition factory", a function that
             creates a new transition. I have errors otherwise. *)
          let transition() =
            let d3 = Js.Unsafe.js_expr "d3" in
            d3##(transition ())##(delay 0)##duration 800
          in
          let gv = gv##transition transition in
          ignore(render gv new_dot);
          true
        end
      | _ -> assert false
    in
    Some (Custom.make ~sync div)
  | _ -> None;;
Vdom_blit.(register @@ custom graphviz_handler)


(****************  ****************)

type incoming_message =
  | Set_background_color of {id:string; color:string }
  | Redraw of {dot:string}      (* Change the graph definition and redraw. *)

let init initial = Vdom.return {dot=initial}
let update model = function
  | Set_background_color{id;color} -> set_background_color id color; Vdom.return model
  | Redraw {dot} -> Vdom.return {dot}

let view model =
  (* Vdom.div ~a:[class_ "flex flex-grow flex-1"] [ *)
  Vdom.custom ~a:[Vdom.oncustomevent (fun (evt:Vdom.Custom.event) ->
      match evt with
      (* Note: we could be handling internal messages directly, and
         return None here. *)
      | Outgoing_message m -> Some m
      | _ -> assert false)]
    (Graphviz model)
