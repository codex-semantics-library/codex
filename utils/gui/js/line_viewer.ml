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

module Make(Row:sig

    type visibility
    
    type t
    val is_visible: visibility -> t -> bool
    val next_visible: visibility -> t -> t option
    val previous_visible: visibility ->  t -> t option
    val equal: t -> t -> bool
    
    type message

    val compare: t -> t -> int

    val view: visibility -> t -> cur:t -> string option * message Vdom.vdom

    val preferred_visible_span: t -> (t * t) option
    
    val row_height:int (* The height, in pixel, of each row. *)
    
  end):sig
  type internal_message
  type incoming_message = Internal_in of internal_message
  type outgoing_message =   
  | Internal_out of internal_message
  | Custom_out of Row.message

  include Component.S
    with type initial_data = Row.visibility * Row.t
     and type incoming_message := incoming_message
     and type outgoing_message := outgoing_message

  val update_visibility: model -> Row.visibility -> model  
  val go_to_line: model -> Row.t -> model
  val current: model -> Row.t
end
= struct

  let row_height_px = (string_of_int Row.row_height) ^ "px"

  type scroll_kind =
    | Down of int     (* Scroll from first to last of int lines. *)
    | DownLarge       (* Scroll from first to last to a large amount. *)
    | Up of int       (* Scroll from last to first of int lines. *)
    | UpLarge         (* Scroll from last to first by a large amount. *)

  (* Is the element in both versions, only in the old one,  only in the new one? *)
  type existence =
    | InBoth
    | InOld
    | InNew
  
  type animation =
    | Stable                    (* No animation *)
    | Scroll_from of
        {old_first_visible:Row.t; (* We were previously displaying from there. *)
         scroll_kind: scroll_kind; }
    | Change_visibility of {diff: (Row.t * existence) list }

  type model = {
    (* We display height pixels starting from first_visible. *)
    height: int;
    first_visible: Row.t;       (* TODO: Rename to first_of_viewport. *)
    current: Row.t;
    animate: animation;
    visibility: Row.visibility
  }

  type internal_message =
    | Resize of Js_browser.Rect.t
    | Transition_end 
    | Scroll_lines of int
  type incoming_message = Internal_in of internal_message
  type outgoing_message = 
  | Internal_out of internal_message
  | Custom_out of Row.message
    (* TODO: Click on row, hover on row, etc. *)

  type initial_data = Row.visibility * Row.t
  
  let step_first_visible visibility (first: Row.t) k : Row.t =
    let rec fwd r n =
      if n <= 0 then r
      else match Row.next_visible visibility r with
        | None -> r
        | Some r' -> fwd r' (n - 1)
    in
    let rec bwd r n =
      if n >= 0 then r
      else match Row.previous_visible visibility r with
        | None -> r
        | Some r' -> bwd r' (n + 1)
    in
    if k >= 0 then fwd first k else bwd first k
    

  module Line_Viewer_Id = 
    Unique_prefix.Make(struct let name = "lineviewer" end)()
  
  let init (visibility,first) =
    let cmd =
      (* Setup a resize observer that sends message on init and when
         the height has changed. *)
      let old_height = ref None in
      Command.Setup_resize_observer
        (Line_Viewer_Id.prefix,
         (fun rect ->
            let height = Js_browser.Rect.height rect in
            let height = int_of_float height in
            match !old_height with
            | Some h when height = h -> None
            | _ -> begin
                (* Js_browser.Console.log Js_browser.console @@ Obj.magic @@ Format.asprintf "old height: %a new height: %d" *)
                (*   (Format.pp_print_option Format.pp_print_int) !old_height height; *)
                old_height := Some height;
                Some(Internal_out (Resize rect))
              end)) in
    { visibility;
      height = 0; current = first; first_visible = first; animate = Stable }, cmd

  
  (* Number of rows displayed in full. *)
  let floor_num_rows model = model.height / Row.row_height

  (* Number of rows that appear on screen. *)
  let ceil_num_rows model = (model.height + Row.row_height - 1) / Row.row_height

  (* We scroll, unless there is a too large distance between the lines.
     In this case, we insert ellipses.

     When we insert ellipses, when scrolling down, we display: (where
     n is the number of lines per screen):
     - the n lines of the old position;
     - n additional lines of context after the old position;
     - (We don't use any visual indicator that there were a cut)
     - n additional lines starting before the new position;
     - n lines of the new position, but the new position is at the start of those lines.

     Thus, too far appart is 3*n lines. *)
  let max_distance_between_screens model =
    let n = ceil_num_rows model in
    3*n

  
  (* Move previous k time if we can. *)
  let rec row_previous model row k =
    if k = 0 then row
    else
      match Row.previous_visible model.visibility row with
      | None -> row
      | Some row ->
        row_previous model row (k - 1)


  (* Compute the distance from from to to, or clip if distance is
     higher than max. Returns the answer as a scroll_kind. *)
  let distance_from_to visibility ~max from_ to_ down =
    let rec loop ptr acc =
      let default() =
        if down then DownLarge
        else UpLarge
      in
      if Row.equal ptr to_
      then
        if down then Down acc
        else Up acc
      else begin
        if acc = max
        then default()
        else
          match Row.next_visible visibility ptr with
          | None ->
            (* We cannot compute, for instance because one is no
               longer visible => approximate. *)
            default()
          | Some ptr' -> loop ptr' (acc + 1)
      end
    in loop from_ 0

  (* Compute what is needed for the transition. *)
  let scroll_from model old_first_visible new_first_visible =

    match Row.compare old_first_visible new_first_visible with
    | 0 -> Stable
    | -1 -> (* Old before new: scroll down. *)
      let scroll_kind =
        distance_from_to model.visibility
          ~max:(max_distance_between_screens model)
          old_first_visible new_first_visible true in
      Scroll_from { old_first_visible; scroll_kind }
    | 1 -> (* New before old: scroll up. *)
      let scroll_kind =
        distance_from_to model.visibility
          ~max:(max_distance_between_screens model)
          new_first_visible old_first_visible false in
      Scroll_from { old_first_visible; scroll_kind }
    | _ -> failwith "Impossible"

  let update model msg =
      (* Js_browser.Console.log Js_browser.console (Obj.magic "Line_viewer.update"); *)
      match msg with
      | Internal_in (Scroll_lines k) ->
        (* keep current selection; just move viewport *)
        let new_first = step_first_visible model.visibility model.first_visible k in
        let animate = scroll_from model model.first_visible new_first in
        Vdom.return { model with first_visible = new_first; animate }
      | Internal_in(Resize rect) ->
        (* Js_browser.Console.log Js_browser.console (Obj.magic "Resize_rect"); *)
        let height = Js_browser.Rect.height rect in
        let height = int_of_float height in
        (* If height unchanged: returns physically equal model, so that
          memo works, so that the dom is not changed and we do not have
          transition cancel events. *)      
        if height = model.height then Vdom.return model
        else Vdom.return @@ {model with height; animate = Stable}
      | Internal_in(Transition_end) ->
        (* Js_browser.Console.log Js_browser.console (Obj.magic "Transition_end"); *)
        Vdom.return @@ {model with animate = Stable}


  (* Can we make the span of lines [a,b] with a <= b visible as a
     whole in the viewport? Returns None if false, and Some n with
     n the distance between a and b, if it can. *)
  let span_fits_in_viewport model a b = 
    let num_rows = ceil_num_rows model in
    assert(Row.compare a b <= 0);
    match distance_from_to model.visibility ~max:num_rows a b true with
    | DownLarge -> None
    | Down n -> 
      assert(n <= num_rows);
      Some n
    | UpLarge | Up _ -> assert false

  (* We recenter/detect visibility not based on the current line
     alone, but on a span in which is the current line, when possible.
     For the traces, we try to see for each call/return node the
     corresponding one. For the DBA, we should try to see the DBA
     instructions, in addition to the mnemonic. *)
  let recenter model =
    let num_rows = ceil_num_rows model in
    let first_visible =
      (* Center on the current line. *)
      let default() = row_previous model model.current (num_rows/2) in
      match Row.preferred_visible_span model.current with
      | None -> default()
      | Some(a,b) -> begin
          assert(Row.compare a model.current <= 0);
          assert(Row.compare model.current b <= 0);
          match  span_fits_in_viewport model a b with
          | None -> default()
          | Some n ->
            (* We center around the space, so we have [space_before_a]
               before and after the span. *)
            let space_before_a = (num_rows - n) / 2 in
            row_previous model a space_before_a
        end
    in
    (* let first_visible = row_previous model.current (num_rows/2) in *)
    let animate = scroll_from model model.first_visible first_visible in
    {model with first_visible; animate}

  (* current visible = in one of the lines. *)
  let is_in_viewport (model:model) cur =
    (* Round down. *)
    let num_rows = floor_num_rows model in
    
    let rec aux row upto =
      if upto = 0 then false
      else if Row.equal row cur then true
      else match Row.next_visible model.visibility row with
        | None -> false
        | Some row' -> aux row' (upto - 1)
    in aux model.first_visible (num_rows - 1)

  (* if current is not visible, recenter around it.  if current is in
     a span that we could see it by recentering, recenter too. *)
  let recenter_if_current_not_in_viewport model =
    (* By default, we look at the current line only. *)
    let default() =
      not @@ is_in_viewport model model.current
    in
    let need_recenter = 
      match Row.preferred_visible_span model.current with
      | None ->  default()
      (* We would like a span to be visible. *)
      | Some(a,b) ->
        match is_in_viewport model a, is_in_viewport model b with
        | false, false -> true  (* Nothing is seen: recenter. *)
        | true, true -> false   (* Everyting can be seen: do not recenter *)
        | false, true | true, false -> begin
            match span_fits_in_viewport model a b with
            | None -> default() (* Doesn't fit -> still try to see the current line. *)
            | Some _ -> true    (* it fits -> recenter. *)
          end
    in
    if need_recenter then recenter model else model

  let n_visible_rows_starting_from n starting_from visibility =
    let rec aux n row acc =
      (* Js_browser.Console.log Js_browser.console (Obj.magic "aux"); *)
      (* Js_browser.Console.log Js_browser.console (Obj.magic (string_of_int n)); *)
      let acc = row::acc in
      let n = n - 1 in
      if n <= 0 then acc
      else
        match Row.next_visible visibility row with
        | None -> acc
        | Some row' -> aux n row' acc
    in List.rev @@ aux n starting_from []

  module RowMap = Map.Make(Row)


  
  (* We present the diff as a list of rows mapped to {InBoth, InOld,
     InNew}) *)
  let compute_diff model first_visible old_visibility new_visibility =
    let displayed_rows = ceil_num_rows model in    
    let linemap visibility =
      let rows = n_visible_rows_starting_from  displayed_rows first_visible visibility in
      List.fold_left (fun map row -> RowMap.add row () map) RowMap.empty rows
    in
    let old_map = linemap old_visibility in
    let new_map = linemap new_visibility in
    let bothmap = RowMap.merge (fun _key a b -> match a,b with
        | Some _, Some _ -> Some InBoth
        | Some _, None -> Some InOld
        | None, Some _ -> Some InNew
        | None, None -> assert false) old_map new_map in
    RowMap.bindings bothmap

  let update_visibility model visibility =
    let old_visibility = model.visibility in
    assert(Row.is_visible old_visibility model.first_visible);

    (*  If the first visible line becomes invisible, we first scroll
        upward (until we find a visible line), with the assumption
        that we will eventually find one. *)
    let model =
      if Row.is_visible visibility model.first_visible
      then model
      else
        let new_first_visible =
          Option.get @@ Row.previous_visible visibility model.first_visible in
        {model with first_visible = new_first_visible }
    in

    (* The current line may disappear too. *)
    let model =
      if Row.is_visible visibility model.current
      then model
      else
        let current =
          Option.get @@ Row.previous_visible visibility model.current in
        {model with current }
    in

    let diff = compute_diff model model.first_visible old_visibility visibility in
    let model = {model with visibility; animate = Change_visibility {diff}} in

    (* MAYBE: sequence transitions. But this is barely noticeable *)
    (* let model = recenter_if_current_not_in_viewport model in *)
    model

  let go_to_line model row = recenter_if_current_not_in_viewport  {model with current = row }

  let view_row model row =
    (* Each row is filld from left to right. *)
    let cur = model.current in
    let bg,content = Row.view model.visibility ~cur:model.current row in
    let content = Vdom.map (fun (m : Row.message) -> Custom_out m) content in
    
    (* Background depends on the current line, but may be customized. *)
    let bg = match bg with
      | None ->
        if Row.equal row cur
        then "bg-emerald-300"
        else "odd:bg-white even:bg-gray-100"
      | Some bg -> bg
    in
    let open Vdom in
    Vdom_ext.div (* ?key *) ~a:[(* class_ "outline-1 outline-red-800";  *)
      class_ bg;
      style "min-height" row_height_px;
      style "height" row_height_px;
      style "display" "flex";
      style "white-space" "nowrap"] [content]

  let view model =
    (* (match model.animate with *)
    (*  | Stable -> Js_browser.Console.log Js_browser.console (Obj.magic "Stable") *)
    (*  | Scroll_from _ -> Js_browser.Console.log Js_browser.console (Obj.magic "Scroll_from") *)
    (*  | Change_visibility _ -> Js_browser.Console.log Js_browser.console (Obj.magic "Change_visibility")); *)
    let displayed_rows = ceil_num_rows model in

    let n_rows_starting_from n starting_from =
      n_visible_rows_starting_from n starting_from model.visibility
      |> List.map (view_row model)
    in

    (* Notification when the transition has finished, to return to a
       stable state. *)
    let transition_end_events =
      [Vdom_ext.ontransitionend (fun () ->
           (* Js_browser.Console.log Js_browser.console (Obj.magic "Transition end"); *)
           Internal_out Transition_end);
       Vdom_ext.ontransitioncancel (fun () ->
           (* Js_browser.Console.log Js_browser.console (Obj.magic "Transition cancel"); *)
           Internal_out Transition_end)]
    in
    let put_transition_end_event = ref false in

    let div_changing_size ~old_height ~new_height content =
      (* To animate rows that appear/disappear, we group them and
         place them under this div whose max-height changes with a
         transition. *)


      (* We want a notification when the transition has stopped, but
         we need only one, even if we have multiple blocks of rows
         simultaneously appearing. *)
      let head =
        if not !put_transition_end_event then
          (put_transition_end_event := true;
           transition_end_events)
        else []
      in

      Vdom_ext.div
        ~a:(head@
            [Vdom.style "max-height" ((string_of_int old_height) ^ "px");
             Vdom.style "transition" "max-height 0.3s";
             (* We use overflow so that the content of the div
                is not shown when clipped by the height. *)
             Vdom.style "overflow-y" "hidden";
             Vdom.style "overflow-x" "hidden";             
             (* Dummy animation to detect when the element has been put
                in the DOM and painted. *)
             Vdom.style "animation" "mymount 0.001s";
             Vdom.on_js "animationstart" (fun js ->
                 (* We want for the element to be painted to change the
                    height and trigger the animation. *)
                 let x = match js with Vdom_blit.Ojs x -> x | _ -> assert false in
                 let elt = Js_browser.Event.(target @@ t_of_js x) in
                 let style = Js_browser.Element.(style @@ t_of_js elt) in
                 (* Js_browser.Style.set style "transition" "max-height 0.5s"; *)
                 Js_browser.Style.set style "max-height" ((string_of_int new_height) ^ "px");
                 None);
            ]) content
    in

    let rows_to_scroll, rows = match model.animate with
      | Stable -> 0, n_rows_starting_from displayed_rows model.first_visible

      | Change_visibility {diff} -> begin
          let num_rows = List.length diff in
          let rec next_block block_typ l acc  = match l with
            | (hd,typ)::tl when block_typ = typ -> next_block block_typ tl (hd::acc)
            | _ -> List.rev acc, l
          in

          let rec loop = function
            | [] -> []
            | (row,InBoth)::tl -> (view_row model row)::(loop tl)
            | ((_,InOld):: _) as l ->
              let block,rest = next_block InOld l [] in
              let content = List.map (view_row model) block in
              let old_height = Row.row_height * List.length block in
              (div_changing_size ~old_height ~new_height:0 content )::loop rest
            | ((_,InNew):: _) as l ->
              let block,rest = next_block InNew l [] in
              let content = List.map (view_row model) block in
              let new_height = Row.row_height * List.length block in
              (div_changing_size ~old_height:0 ~new_height content )::loop rest              
          in
          num_rows, loop diff
        end

      | Scroll_from { old_first_visible; scroll_kind = Down rows_from_old_to_new } ->
        let num_rows = displayed_rows + rows_from_old_to_new in
        (* Js_browser.Console.log Js_browser.console (Obj.magic "Line_viewer: Num rows"); *)
        (* Js_browser.Console.log Js_browser.console (Obj.magic (string_of_int num_rows)); *)
        (* let num_rows = min num_rows 300 in (\* Safeguard if there is a problem. *\) *)
        -rows_from_old_to_new, n_rows_starting_from num_rows old_first_visible
      | Scroll_from { old_first_visible; scroll_kind = DownLarge } ->
        let rows_per_screen = ceil_num_rows model in
        let two_screens = n_rows_starting_from (2*rows_per_screen) old_first_visible in
        let start_again = row_previous model model.first_visible rows_per_screen in
        let two_screens' = n_rows_starting_from (2*rows_per_screen) start_again in
        -3 * rows_per_screen,  two_screens @ two_screens'
      | Scroll_from { old_first_visible; scroll_kind = Up rows_from_old_to_new } ->
        let num_rows = displayed_rows + rows_from_old_to_new in
        rows_from_old_to_new, n_rows_starting_from num_rows model.first_visible
      | Scroll_from { old_first_visible; scroll_kind = UpLarge } ->
        let rows_per_screen = ceil_num_rows model in
        let two_screens = n_rows_starting_from (2*rows_per_screen) model.first_visible in
        let start_again = row_previous model old_first_visible rows_per_screen in
        let two_screens' = n_rows_starting_from (2*rows_per_screen) start_again in
        3 * rows_per_screen, two_screens @ two_screens'
    in
    let open Vdom in

    (* min-h-0/min-w-0 here says that the height/width of the pane is not based on the content,
       but on the layout (the fact that it is a flex). *)  
    Vdom_ext.div ~a:[class_ "min-h-0 min-w-0 text-sm flex-1 flex-grow flex-shrink overflow-x-auto";
                     Line_Viewer_Id.id ""; 
                     Vdom.on_js "wheel" (fun js ->
                        let lines =
                          try
                            let o = match js with Vdom_blit.Ojs o -> o | _ -> failwith "no ojs" in
                            let ev = Js_browser.Event.t_of_js o in
                            let dy = Js_browser.Event.delta_y ev in
                            let approx = int_of_float (dy /. float_of_int Row.row_height) in
                            if approx = 0 then (if dy > 0. then 1 else -1) else approx
                          with _ -> 1
                        in
                        Some (Internal_out (Scroll_lines lines)))
                     ]
      [ let pixels_to_scrolls = rows_to_scroll * Row.row_height in        
        (* Intermediate "viewport" div to fix the height and avoid resize
           observer events during the transition. *)
        Vdom_ext.div ~a:[style "height" (Format.asprintf "%dpx" model.height);
                         style "overflow-y" "hidden"]


          (* This div has full width based on the content, and does not
             shrink. It is this div which is scrolled. Without it, there
             were display problems: svg elements were removed, and the lines
             had no background when scrolled.

             NB: An alternative to scrolling would be to force the rows that
             are too long to wrap; but this means that rows may span several
             lines. *)
          [ let base_classes = "w-full flex-shrink-0" in
            match model.animate with
            | Stable | Change_visibility _ -> Vdom_ext.div ~a:[class_ base_classes] rows
            | Scroll_from _ ->
              Vdom_ext.div
                ~a:(transition_end_events@
                    [ class_ base_classes;
                      (let transformation = "translateY(" ^ (string_of_int pixels_to_scrolls) ^ "px)" in
                       style "transform" transformation);
                      style "transition" "transform 0.4s"])
                (* Positioner div: If scrolling up, we need an initial
                   transformation that will be undone in the transition. *)  
                (let positioner x =
                   if pixels_to_scrolls <= 0  then x
                   else
                     [Vdom_ext.div ~a:[style "transform" ("translateY(-" ^ (string_of_int pixels_to_scrolls) ^ "px)")] x]
                 in positioner rows)]]

  let view model = Vdom.memo ~key:Line_Viewer_Id.prefix view model

  let current model = model.current
end
