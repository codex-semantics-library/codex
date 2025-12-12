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

module Filter = struct
  type 'a t = { short_name: string; f: 'a -> bool; unique_name: string;}
  (* Assumption: different filters must have different names. *)
  let equal a b = a.unique_name = b.unique_name
end

module Make
    (Row_height: sig val row_height: int end)
    (Severity:sig val color: severity:int -> string end)
    (Interval_node:sig
       type 'a t

       (* TODO: Replace Cannot_go with an option type, maybe reusing
          any for this?  *)
       exception Cannot_go of string
       val is_root: 'a t -> bool
       val indentation: 'a t -> int
       val equal: 'a t -> 'b t -> bool
       val compare: 'a t -> 'b t -> int
       val is_ancestor: 'a t -> 'b t -> bool
       val parent: 'a t -> [`interval_node] t
       type 'a kind =
         | Interval: [`interval_node] kind
         | Single: [`single_node] kind
       val kind_of: 'a. 'a t -> 'a kind
       type any = Any: 'a t -> any
       val next_sibling: 'a t -> any
       val previous_sibling: 'a t -> any    
       val first_child: [`interval_node] t -> any
       val last_child: [`interval_node] t -> any    
       val hash: 'a t -> int
       val exists_child: any -> (any -> bool) -> bool
       val is_leaf: 'a t -> bool
       val severity: [ `single_node ] t -> int         
       val view_single: [`single_node] t -> 'a Vdom.vdom
       val view_start: [`interval_node] t -> 'a Vdom.vdom
       val view_end: [`interval_node] t -> 'a Vdom.vdom
     end) =
struct
  let row_height = Row_height.row_height


  (* {2 Graphical characters. }
     
     To have nice controls on the left, and especially control over the
     alignment, the easiest way is to have fixed row and a "grid" of svg
     characters. Here the grid is [level_width] px for the horizontal
     space (we need it narrow because we have a large amount of
     nesting), and [row_height] for the vertical space (enough to fit
     text). *)  
  module Svg = struct
    open Vdom

    (* Number of pixels for each level of indentation. *)
    let level_width = 12

    let stroke_color = "#666"

    (* Each svg character is a square. We put it as inline-block so that
       we can display them side by side like characters; otherwise the
       display can be weird. *)
    let svg_attributes =
      [int_attr "width" level_width;
       int_attr "height" row_height;
       style "display" "inline-block";
       (* If the row is too small, the svg image is still displayed. *)
       style "flex-shrink" "0";
      ]

    let triangle a b c d e f =
      Format.sprintf "%d,%d %d,%d %d,%d" a b c d e f

    let line ~highlight x1 y1 x2 y2 ~dashed =
      let stroke_color =
        if highlight then "black" else stroke_color in
      let stroke_width =
        if highlight then "1.6" else "1.5" in
      svg_elt "line"
        ~a:[int_attr "x1" x1;
            int_attr "y1" y1;
            int_attr "x2" x2;
            int_attr "y2" y2;
            attr "stroke" stroke_color;
            attr "stroke-dasharray" (if dashed then "2,1" else "none");
            attr "stroke-width" stroke_width] [];;

    let polygon ~highlight points = 
      let stroke_color =
        if highlight then "black" else stroke_color in
      let stroke_width =
        if highlight then "1.5" else "1.5" in
      let fill =
        if highlight then "#2a6fbb" else "none" in
      svg_elt "polygon"
        ~a:[attr "points" points;
            attr "stroke" stroke_color;
            attr "fill" fill;
            attr "stroke-width" stroke_width
            (* attr "fill" "#2a6fbb" *)
           ] []

    (* Radius, cx, cy *)
    let circle ~highlight ~r ~cx ~cy ~fill =
      let stroke_color =
        if highlight then "black" else stroke_color in
      let stroke_width =
        if highlight then "1.5" else "1.5" in
      svg_elt "circle"
        ~a:[int_attr "cx" cx;
            int_attr "cy" cy;
            int_attr "r" r;
            attr "stroke" stroke_color;
            attr "fill" fill;
            attr "stroke-width" stroke_width          
           ] []

    (* A vertical line in the middle of the level width. *)
    let svg_vertical_line ~highlight =
      svg_elt "svg" ~a:svg_attributes
        [
          line ~highlight
            (level_width/2) 0
            (level_width/2) row_height
            ~dashed:false
        ]

    (* Note: maybe we could use icons for errors and warning. *)
    let svg_circle ~highlight ~severity =
      let cx = level_width / 2 in
      let cy = (row_height + 3) / 2 in
      let r = 4 in
      let fill = Severity.color ~severity in
      svg_elt "svg" ~a:svg_attributes
        [ circle ~fill ~highlight ~r ~cx ~cy ]


    (* Note: vertical centering is not perfect, because the text
       baseline, is generally at 70-80% of the height. So we add some
       offset. *)
    let svg_opened_triangle_start ~highlight =
      let midx = level_width / 2 in
      svg_elt "svg" ~a:svg_attributes
        [
          (* A vertical line up to 1/3 of the square. *)
          line ~highlight midx (row_height) midx (2 * row_height /3) ~dashed:false;

          (* The triangle is vertically in the middle third, and horizontally centered. *)
          polygon ~highlight  (triangle midx (2 * row_height / 3) (midx + 5) (row_height / 3) (midx - 5) (row_height / 3));
        ]

    let svg_closed_triangle_start ~highlight =
      let midx = level_width / 2 in
      svg_elt "svg" ~a:svg_attributes
        [
          (* A vertical line up to 1/3 of the square. *)
          line ~highlight midx (row_height) midx (2 * row_height /3) ~dashed:true;

          (* The triangle is vertically in the middle third, and horizontally centered. *)
          polygon ~highlight  (triangle midx (row_height / 3) (midx + 5) (2 * row_height / 3) (midx - 5) (2 * row_height / 3));
        ]

    let svg_closed_triangle_end ~highlight =
      let midx = level_width / 2 in
      let offsety = 5 in  (* The baseline is not in the middle, so we shift. *)
      svg_elt "svg" ~a:svg_attributes
        [
          (* A vertical line up to 1/3 of the square. *)
          line ~highlight midx 0 midx ((row_height /3) + offsety) ~dashed:true;

          (* The triangle is vertically in the middle third, and horizontally centered. *)
          polygon ~highlight  (triangle midx ((2 * row_height / 3) + offsety) (midx + 5) ((row_height / 3) + offsety) (midx - 5) ((row_height / 3) + offsety));
        ]

    let svg_opened_triangle_end ~highlight =
      let midx = level_width / 2 in
      let offsety = 5 in            (* The baseline is not in the middle, so we shift. *)
      svg_elt "svg" ~a:[int_attr "width" level_width;
                        int_attr "height" row_height;
                        style "display" "inline-block"
                       ]
        [
          (* A vertical line up to 1/3 of the square. *)
          line ~highlight midx 0 midx ((row_height /3) + offsety) ~dashed:false;      

          (* The triangle is vertically in the middle third, and horizontally centered. *)
          polygon ~highlight  (triangle midx ((row_height / 3) + offsety) (midx + 5) ((2 * row_height / 3) + offsety) (midx - 5) ((2 * row_height / 3) + offsety));
        ]
  end

  (* {2 Identifier for a row to be displayed.}

     A row is either the start of an interval, the end of an interval,
     or a single node. *)
  module Row = struct

    (* Position on an interval node; the kind of position depends on the kind of node. *)
    type 'a position =
      | Start : [`interval_node] position
      | Single: [`single_node] position
      | End: [`interval_node] position

    type 'a t = {node:'a Interval_node.t; where:'a position}

    type any = Any: 'a t -> any 

    let equal_where: type a b. a position -> b position -> bool =
      fun a b -> match a,b with 
        | Start, Start -> true
        | End, End -> true
        | Single, Single -> true
        | _ -> false

    (* Same row = same node and position in the node. *)
    let equal (Any a) (Any b) =
      Interval_node.equal a.node b.node && equal_where a.where b.where


    let compare (Any a) (Any b) =
      (* Js_browser.Console.log Js_browser.console @@ Obj.magic @@ *)
      (* Format.asprintf "compare rows %s %d %b %s %d %b" *)
      (*   (snd @@ get_content a.n.value) a.n.id (a.where = End) *)
      (*   (snd @@ get_content b.n.value) b.n.id (a.where = End); *)
      (* If same node: the call is before the return. *)
      if Interval_node.equal a.node b.node then begin
        match a.where, b.where with
        | Start, Start | Single, Single | End, End -> 0
        (* If the same id and single, should be compared only to itself. *)
        | Single, _ | _, Single -> assert false 
        | Start, End -> -1
        | End, Start -> 1
      end
      (* If intervals overlap, one is the ancestor of the other; the
         start of the ancestor is before its children, and the end is
         after. Otherwise, intervals do not overlap and we can compare
         the intervals by their ids. *)
      else if Interval_node.is_ancestor a.node b.node &&
              (match a.where with End -> true | Start -> false | Single -> assert false)
      then 1
      else if Interval_node.is_ancestor b.node a.node &&
              (match b.where with End -> true | Start -> false | Single -> assert false)
      then -1
      else Interval_node.compare a.node b.node

    let upward (Any x) = {node = Interval_node.parent x.node;where=Start}

    (* Set the position of a node; this depends on the kind of node. *)
    let start_of (Interval_node.Any interval_node) =
      match Interval_node.kind_of interval_node with
      | Interval ->  Any{node = interval_node; where=Start} 
      | Single -> Any{node=interval_node;where=Single}

    let end_of (Interval_node.Any interval_node) =
      match Interval_node.kind_of interval_node with
      | Interval -> Any{node = interval_node; where=End}
      | Single -> Any{node=interval_node;where=Single}

    (* Try to go to the next sibling; if it fails, goes to the parent's return node. *)
    let next_sibling_or_parent t =
      let open Interval_node in
      if Interval_node.is_root t.node
      then raise (Cannot_go "Cannot go after the root")
      (* Try to go the next sibling *)        
      else
        try start_of @@ Interval_node.next_sibling t.node
        with Cannot_go _ -> end_of (Interval_node.(Any (parent t.node)))
    (* No more children -> Go to the return node of the parent.
       We know that it is an interval node. *)


    let previous_sibling_or_parent t =
      (* Try to go to the previous sibling. *)
      let open Interval_node in
      if Interval_node.is_root t.node
      then raise (Cannot_go "Cannot go before the root")
      else 
        try end_of @@ Interval_node.previous_sibling t.node
        with Cannot_go _ -> start_of (Interval_node.(Any (parent t.node)))

    (* Goes to the next line when everything is expanded.
       Try to find, in order:
       - A children;
       - the next sibling;
       - the parent's return node. *)
    let next (type a) (t: a t) =
      match t.where with
      | Start ->
        (* Try to go down in a child.
           If no child: go to to the return node. *)
        begin try start_of @@ Interval_node.first_child t.node
          with Interval_node.Cannot_go _ -> Any {t with where = End }
        end
      | (End|Single) -> next_sibling_or_parent t

    (* Contrary of next. *)
    let previous (type a) (t: a t) =
      match t.where with
      | End ->
        begin try end_of @@ Interval_node.last_child t.node
          with Interval_node.Cannot_go _ -> Any {t with where = Start }
        end
      | (Start|Single) -> previous_sibling_or_parent t

  end

  (* {2 Collections of interval nodes.} *)
  module Interval_node_map = struct
    open Interval_node
    include Map.Make(struct
        type t = Interval_node.any
        let compare (Any a) (Any b) = Interval_node.compare a b
      end)
    let find x m = find (Any x) m
  end

  module Interval_node_hash = struct
    open Interval_node
    include Hashtbl.Make(struct
        type t = Interval_node.any
        let hash (Any a) = Interval_node.hash a
        let equal (Any a) (Any b) = Interval_node.equal a b
      end)
    let find = find
    let replace = replace
    let memo hash f k =
      match find hash k with
      | exception Not_found ->
        (let res = f k in
         replace hash k res;
         res)
      | x -> x
  end  

  (* {2 Handling which row is being displayed.} *)  
  module Visibility:sig
    (* We encapsulate this interface to make sure that all operations
       that change the visibility properly update the cache. *)
    type cache
    type visibility = private {
      filters : (Interval_node.any Filter.t * bool) list;    
      explicitly_opened : bool Interval_node_map.t;
      cache : cache;
    }

    (* Accessing the visibility. *)
    val initial : (Interval_node.any Filter.t * bool) list -> visibility
    val is_opened : visibility -> 'a Interval_node.t -> bool
    val is_single : visibility -> 'a Row.t -> bool
    val is_visible : visibility -> 'a Row.t -> bool
    val next_visible : visibility -> Row.any -> Row.any option
    val previous_visible : visibility -> Row.any -> Row.any option

    (* Changing the visibility *)
    val open_path : visibility -> Row.any -> visibility
    val close_path : visibility -> Row.any -> visibility
    val add_filter: visibility -> (Interval_node.any Filter.t * bool) -> visibility
    val remove_filter: visibility -> Interval_node.any Filter.t -> visibility
  end = struct

    (* Here are some definitions:
       - A row is visible if it can appear in the viewport.
       - A row is selected if the used explicitly asked it to be visible.
       - An interior node is opened when all its immediate children are visible.
         If it is closed, then none of its children are visible.
       - All the ancestors of a selected node are opened. 

       Note that:
       - The siblings of a selected row are also visible
       - A selected row may be closed. *)

    (* Note on the interaction modes. Two modes are possible for filters:

       - Declarative: we declare what should be opened; it is closed
         when the filter is removed.  However, we can no longer close
         items that are manually selected.

       - Stateful: we open all the nodes on a collection. The problem is
         we that we no longer know how to undo an opened node.

       To solve this problem, we do it like this:

       - We have an explicitly_opened map with three status: forcibly
         opened, forcibly closed, default (= absent from the map).
       - We obey the forced status which corresponds to explicit user interaction.
       - Forcibly opened nodes will stay opened (otherwise some "lost" nodes are difficult to find)
       - Adding view filters can reopen nodes that have been closed explicitly (if we add the filter,
         we want to view it). *)

    (* We use a cache for the long-running recursive functions, and to
       avoid supra-linear complexity (we handle each node at most
       once, ideally 0 times). Node that we may use several caches at
       once (one per visibility), as multiple visibility may coexist
       during animations: do not recycle the caches! *)
    type cache = {
      contains_selected: bool Interval_node_hash.t;
      is_visible: bool Interval_node_hash.t
    }

    type visibility = {
      (* By default, the open/close status depends on what is selected
         by the filters. To each filter that is here, we associated a
         checkbox to say if it is active. Filters that are not selected
         are still useful as some kinds of bookmarks. *)
      filters: (Interval_node.any Filter.t * bool) list;

      (* The user can change the selection by opening (true) or closing
         (false) nodes. The nodes that do not appear in this map have
         the default status. *)
      explicitly_opened: bool Interval_node_map.t;

      cache:cache
    }

    let new_cache() = 
      { contains_selected = Interval_node_hash.create 17;
        is_visible = Interval_node_hash.create 17;}

    let initial filters =
      let cache = new_cache()  in
      { explicitly_opened = Interval_node_map.empty; filters; cache }

    let is_selected visibility n =
      let n = Interval_node.Any n in
      List.exists (fun (filter,active) -> active && filter.Filter.f n) visibility.filters

    (* Exploring the list of children may possibly be costly if done
       several times, so we cache the computation. We may still have to
       traverse every node (in particular when there are no filters and
       we view all the trace), but it seems OK for now. *)
    let rec selected_or_contains_selected visibility (Interval_node.Any n) =
      (is_selected visibility n) || contains_selected visibility (Interval_node.Any n)
    and contains_selected' visibility n =
      Interval_node.exists_child n (selected_or_contains_selected visibility)
    and contains_selected visibility n =
      Interval_node_hash.memo visibility.cache.contains_selected (contains_selected' visibility) n

    let implicitly_opened = contains_selected

    let is_opened visibility x =
      match Interval_node_map.find x visibility.explicitly_opened with
      | exception Not_found -> implicitly_opened visibility (Interval_node.Any x)
      | answer -> answer

    let is_closed visibility x = not @@ is_opened visibility x

    let is_single visibility x =
      let x = x.Row.node in
      Interval_node.is_leaf x
      || not @@ is_opened visibility x

    let rec is_visible' visibility (Interval_node.Any x) : bool =
      (* We cannot hide the root node. *)
      Interval_node.is_root x
      ||
      let parent = Interval_node.parent x in
      (* Parent hidden => all children are hidden. *)
      is_visible visibility parent &&
      is_opened visibility parent

    and is_visible: type a. visibility -> a Interval_node.t -> bool = fun visibility x ->
      let x = Interval_node.Any x in
      match Interval_node_hash.find visibility.cache.is_visible x with
      | exception Not_found -> begin
          let res = is_visible' visibility x in
          Interval_node_hash.replace visibility.cache.is_visible x res;
          res
        end
      | res -> res

    let rec next_visible:  type a. visibility -> a Row.t -> Row.any option =
      fun visibility path ->
      (* Skip closed subtrees for faster movement. This is important for precision. *)
      match path.where with
      | Start when is_closed visibility path.node -> Some (Row.Any{path with where = End})
      | _ ->(match Row.next path with
          | exception Interval_node.Cannot_go _ -> None
          | Row.Any p ->
            if is_visible visibility p.node then Some (Row.Any p)
            else next_visible visibility p)
    let next_visible visibility (Row.Any path) = next_visible visibility path

    let rec previous_visible: type a. visibility -> a Row.t -> Row.any option =
      fun visibility path ->
      (* Skip closed subtrees for faster movement. *)
      match path.where with
      | End when is_closed visibility path.node -> Some (Row.Any{path with where = Start})
      | _ -> (match Row.previous path with
          | exception Interval_node.Cannot_go _ -> None
          | Row.Any p ->
            if is_visible visibility p.node then Some (Row.Any p)
            else previous_visible visibility p)
    let previous_visible visibility (Row.Any path) = previous_visible visibility path

    let is_visible visibility x = is_visible visibility x.Row.node
    
    (* We create another cache so that the structure stays
       functional. Note that due to animations, we sometimes have
       multiple calls to visibility at the same time. *)
    let drop_cache v =
      { v with cache = new_cache()}

    (* These functions change the visibility, so must drop the cache. *)

    let open_path visibility (Row.Any path) =
      (* Once opened, the nodes stay open. Closing nodes automatically
         would make them harder to find. *)
      let explicitly_opened = Interval_node_map.add (Interval_node.Any path.Row.node) true visibility.explicitly_opened in
      drop_cache { visibility with explicitly_opened }

    let close_path visibility (Row.Any path) =
      let node = Interval_node.Any path.Row.node in
      (* If the result of close corresponds to the default status, we do
         not remember it, as the next addition of a filter can reopen
         the node.  *)
      let explicitly_opened = 
        if not @@ implicitly_opened visibility node
        then Interval_node_map.remove node visibility.explicitly_opened
        else Interval_node_map.add node false visibility.explicitly_opened
      in
      drop_cache { visibility with explicitly_opened }

    let add_filter visibility (f,active) =
      let filters =
        (* Replace activie status if needed, or at on top. *)
        let rec aux = function
          | [] -> raise Not_found
          | (f',_)::tl when Filter.equal f' f -> (f,active)::tl
          | hd::tl -> hd::aux tl
        in
        match aux visibility.filters with
        | exception Not_found -> (f,active)::visibility.filters
        | res -> res
      in
      let visibility = drop_cache {visibility with filters } in
      (* Explicitly closed nodes may be reopened by filters. *)
      let explicitly_opened = 
        visibility.explicitly_opened |> Interval_node_map.filter (fun n status ->
            if status then true
            else implicitly_opened visibility n)
      in { visibility with explicitly_opened }

    let remove_filter visibility f =
      let filters = List.remove_assoc f visibility.filters in
      drop_cache { visibility with filters }
  end

  (* {2 How to display each row.}

     Note that the animation, recentering etc. logic is handled by the
     line viewer. *)
  module Line_viewer_row = struct

    type visibility = Visibility.visibility
    
    type message =  
      | Change_open_status of bool* [`interval_node] Row.t
      | Goto_disasm of [`interval_node ] Row.t

    type t = Row.any
    let is_visible visibility (Row.Any x) = Visibility.is_visible visibility x
    let next_visible = Visibility.next_visible
    let previous_visible = Visibility.previous_visible
    let equal = Row.equal
    let compare = Row.compare

    let view visibility (Row.Any row) ~cur =
      let Row.Any cur = cur in
      let highlight = Interval_node.equal row.node cur.node in
      let is_child_of_cur = Interval_node.is_ancestor cur.node row.node in

      (* We display the vertical lines corresponding to the parent
         intervals (and it also adds indentation). If one of the
         parent is selected, we need to highlight this vertical
         line. *)
      let indent_chars =
        (* We highlight the vertical line of the descendants of the current node. *)
        let indentation = Interval_node.indentation row.node in
        (* Highlight the vertical line of the current row in the children. *)
        let highlight_level =
          if is_child_of_cur
          then Interval_node.indentation cur.node
          else -1
        in
        (* A list of vertical lines at the start of the line. *)
        let rec aux n acc =
          if n = 0 then acc
          else
            let highlight = highlight_level = (indentation - n) in
            aux (n - 1) ((Svg.svg_vertical_line ~highlight)::acc)
        in
        (aux indentation [])
      in

      let content =
        (* There is more = the node is openable. *)
        let is_open = Visibility.is_opened visibility row.node in 
        let there_is_more =
          not (Interval_node.is_leaf row.node) &&
          not is_open in
        let open Row in
        match row.where with
        | Single ->
          let severity = Interval_node.severity row.node in
          let vdom = Interval_node.view_single row.node in
          [Svg.svg_circle ~highlight ~severity; vdom ]
        | Start ->
          let open_triangle =
            if there_is_more
            then Svg.svg_closed_triangle_start
            else Svg.svg_opened_triangle_start
          in
          let clickable_triangle =
            Vdom.div
              ~a:[ Vdom.onclick (fun _ -> Change_open_status (not is_open, row)) ]
              [open_triangle ~highlight]
          in
          let clickable_content =
            Vdom.div
              ~a:[ Vdom.onclick (fun _ -> Goto_disasm row) ]
              [Interval_node.view_start row.node]
          in
          [clickable_triangle; clickable_content]
        | End ->
          let close_triangle = if there_is_more
            then Svg.svg_closed_triangle_end
            else Svg.svg_opened_triangle_end
          in
          [close_triangle ~highlight; Interval_node.view_end row.node ]
      in

      let bg =
        if Interval_node.equal row.node cur.node then
          (* The current line, or both this line and its matching line
             when viewed as a single line, are selected. *)
          if Row.equal_where row.where cur.where || Visibility.is_single visibility cur
          then Some "bg-emerald-300"
          (* The matching Start/return line. *)
          else Some "bg-emerald-100"
        else if is_child_of_cur
        then Some "bg-amber-100"
        else None
      in
      bg, Vdom.fragment @@ List.rev_append indent_chars content


    (* When moving to a line, we try to have its whole span to be
       visible, when possible.

       MAYBE: We could maybe take the parents into account, so as to
       have the maximum context to be visible. But it seems to work
       fine as it is. *)
    let preferred_visible_span (Row.Any cur) =
      match cur.where with
      | Single -> None
      | Start -> Some(Row.Any cur,Row.Any{cur with where = End})
      | End -> Some(Row.Any{cur with where = Start}, Row.Any cur)


    let row_height = row_height

  end

  module Line_viewer = Line_viewer.Make(Line_viewer_row)


  (* {2 Component definition: model and messages.} *)
  type model = {
    (* Note that visibility must be copied to the line_viewer model. *)
    visibility: Visibility.visibility;
    line_viewer: Line_viewer.model;
  }
  

    
  type internal_message =
    | Line_viewer of Line_viewer.internal_message
    | Keyboard_action of (model -> model * outgoing_message Vdom.Cmd.t)
    | Line_viewer_out of Line_viewer_row.message
          
  and incoming_message =
    | Internal_in of internal_message  

  and outgoing_message =
    | Internal_out of internal_message
    | Goto_disasm of [ `interval_node ] Interval_node.t

    


let lift_line_viewer_messages a = 
  match a with
  | Line_viewer.Internal_out msg ->
      Internal_out (Line_viewer msg)
  | Line_viewer.Custom_out row_msg ->
      match row_msg with 
      | Change_open_status _ -> Internal_out (Line_viewer_out row_msg)
      | Goto_disasm r -> Goto_disasm r.node
      
      
  type initial_data  = (Interval_node.any Filter.t * bool) list * Interval_node.any  

  let init (initial_filters,root_node) =
    let visibility = Visibility.initial initial_filters in
    let init_path = Row.start_of root_node in
    let line_viewer,cmd = Line_viewer.init (visibility,init_path) in
    let cmd = Vdom.Cmd.map lift_line_viewer_messages cmd in    
    Vdom.return ~c:[cmd] { visibility; line_viewer }



  module Update = struct

    let update_visibility model visibility =
      let line_viewer = Line_viewer.update_visibility model.line_viewer visibility in
      { line_viewer; visibility }

    let open_cur model =
      update_visibility model @@ Visibility.open_path model.visibility (Line_viewer.current model.line_viewer)

    let close_cur model =
      update_visibility model @@ Visibility.close_path model.visibility (Line_viewer.current model.line_viewer)

  end

  let update (model:model) = function
    | Internal_in(Line_viewer msg) ->
      let line_viewer,cmd = Line_viewer.update model.line_viewer (Line_viewer.Internal_in msg) in
      let cmd = Vdom.Cmd.map lift_line_viewer_messages cmd in
      {model with line_viewer}, cmd
    | Internal_in(Keyboard_action f) -> 
        let model, cmd = f model in
        model, cmd
    | Internal_in (Line_viewer_out (Change_open_status (bool, row))) ->
      if bool then 
        Vdom.return @@ Update.update_visibility model
        (Visibility.open_path model.visibility (Row.Any row))
      else
        Vdom.return @@ Update.update_visibility model
        (Visibility.close_path model.visibility (Row.Any row))
    | Internal_in (Line_viewer_out (Goto_disasm r)) -> Vdom.return model
        (* Vdom.return ~o:(Jump_to_disasm row) model *)

  let view model =
      Vdom.map lift_line_viewer_messages @@ Line_viewer.view model.line_viewer  


  (* {2 Moving in the hierarchical list.}

     We try to be DWIM (do-what-I-mean). In particular, when the call
     and return nodes of an interval are next to the other, we display
     them as a single element, and we handle that in the movement too. *)
  module Movement = struct

    let start_of_current_node model  =
      (* If on a call/return group, we should already be be on Start. *)
      let cur = Line_viewer.current model.line_viewer in
      (* Note: the following may be false when visibility changes. *)    
      (* assert(not (Visibility.is_single model.visibility cur) || cur.where = Start);*)
      cur

    (* Note: on single nodes, stay where we are. *)
    let end_of_current_node model  =
      let Row.Any cur = Line_viewer.current model.line_viewer in
      (* Note: the following may be false when visibility changes. *)
      (* assert(not (Visibility.is_single model.visibility cur) || cur.where = Start); *)
      if Visibility.is_single model.visibility cur
      then
        let f: type a. a Row.t -> a Row.t = fun cur ->
          match cur.Row.where with
          | Start when Visibility.is_single model.visibility cur -> {  cur with where = End}
          | _ -> cur
        in Row.Any (f cur)
      else Row.Any cur

  let move model target =
    match target with
    | None -> model, Vdom.Cmd.Batch []
    | Some (Row.Any t) ->
        let f : type a. a Row.t -> a Row.t = fun t ->
          match t.where with
          | End when Visibility.is_single model.visibility t -> { t with where = Start }
          | _ -> t
        in
        let t = f t in
        let model =
          { model with line_viewer = Line_viewer.go_to_line model.line_viewer (Row.Any t) }
        in
        begin match Interval_node.kind_of t.node with
        | Interval_node.Interval ->
            model, Vdom.Cmd.echo (Goto_disasm t.node)
        | Interval_node.Single ->
            model, Vdom.Cmd.Batch []
        end


    let next_line model =
      let cur = end_of_current_node model in
      move model @@ Visibility.next_visible model.visibility cur

    let previous_line model =
      let cur = start_of_current_node model in
      move model @@ Visibility.previous_visible model.visibility cur

    (* Forward goes forward in a DWIM way, but guarantees that it will
       not go down a level.
       - If on a call,goes to the corresponding return;
       - (if the call and return is grouped, do as if we were already on this return)
       - If on a return or a single node, goes to the next sibling
       - (Note that if the current node is visible, so is the next sibling)
       - If we are on the last sibling, go to the return node of the parent.
       - If on the return on the root node, stop there.  *)
    let forward model =
      let Row.Any cur = end_of_current_node model in 
      move model @@
      match cur.where with
      | Start -> Some (Row.Any { cur with where = End })
      | End | Single -> Some (Row.next_sibling_or_parent  cur)

    let backward model =
      let Row.Any cur = start_of_current_node model in
      move model @@
      match cur.where with
      | End -> Some (Row.Any { cur with where = Start })
      | Start | Single -> Some (Row.previous_sibling_or_parent cur)

    let upward model =
      let current = Line_viewer.current model.line_viewer in
      move model @@ Some (Row.Any{ (Row.upward current) with where = Start })

  end

  let get_filters model = model.visibility.filters

  let add_filter model (f,status) =
    let visibility = Visibility.add_filter model.visibility (f,status) in
    Update.update_visibility model visibility

  let remove_filter model f =  
    let visibility = Visibility.remove_filter model.visibility f in
    Update.update_visibility model visibility

  (* {2. Menu entries, commands for the movement keys. } *)
  let menu_entries model =
    let desc_cur, action_cur =
      let Row.Any cur = Line_viewer.current model.line_viewer in
      if Interval_node.is_leaf cur.node
      then "", (fun m -> m, Vdom.Cmd.Batch [])
      else if Visibility.is_opened model.visibility cur.node
      then "close", (fun m -> Update.close_cur m, Vdom.Cmd.Batch [])
      else "open",  (fun m -> Update.open_cur m,  Vdom.Cmd.Batch [])
    in
    let open Transient_menu in
    [{highlight=false; keys=[Char "p"; ArrowUp];    desc="previous-line"; action=Internal_out(Keyboard_action(Movement.previous_line))};
      {highlight=false; keys=[Char "n"; ArrowDown];  desc="next-line";     action=Internal_out(Keyboard_action(Movement.next_line))};
      {highlight=false; keys=[Char "b"; ArrowLeft];  desc="backward/begin"; action=Internal_out(Keyboard_action(Movement.backward))};
      {highlight=false; keys=[Char "f"; ArrowRight]; desc="forward/finish"; action=Internal_out(Keyboard_action(Movement.forward))};
      {highlight=false; keys=[Char "u"];             desc="upward";        action=Internal_out(Keyboard_action(Movement.upward))};
      {highlight=false; keys=[Enter];                desc=desc_cur;      action=Internal_out(Keyboard_action(action_cur))};
    ]
    
  (* TODO: x (execute) -> go to next selected; r (reverse execution) -> go to previous  *)
  
end
