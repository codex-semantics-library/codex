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

(** This component displays interactively a tree of nested
    (non-overlapping) intervals. Leaves can be intervals with no
    children, or single nodes. Interior nodes are intervals with a
    list of children. The component allows intervals to be
    opened/closed manually, or programmatically using filters, that
    specifies the nodes that must be visible. *)


module Filter : sig

  (** Filters that match elements of type 'a. 
        Each filter must define:
        - [unique_name]: a unique identifier used to distinguish 
            this filter from all others.
        - [short_name]: a shortened version of unique_name suitable for display. 
        - The [f] function returns true when the filter matches. *)
  type 'a t = { short_name : string; f : 'a -> bool; unique_name: string;}
  val equal : 'a t -> 'b t -> bool
end

module Make
    (Row_height : sig

       (** The element is displayed as sequence of lines of height [row_height].  *)
       val row_height : int end)
    (Severity : sig

       (** [color] provides an interpretation of the severity of a
           message as a color. *)
       val color : severity:int -> string end)
    (Interval_node : sig

       (** A single node, which can be an interval or a single node. *)
       type 'a t

       (** Descriminates between nodes. *)
       type 'a kind =
           Interval : [ `interval_node ] kind
         | Single : [ `single_node ] kind
       val kind_of : 'a t -> 'a kind

       (** The root is the top-level interval node.  *)
       val is_root : 'a t -> bool

       (** A leaf is a single node or an interval node with no
           children. *)
       val is_leaf : 'a t -> bool

       (** This exception is raised for impossible movements: taking
           the parent of a root, the next children of the last child,
           etc. *)
       exception Cannot_go of string


       (** Indentation is the depth of the node, where the root is 0. *)
       val indentation : 'a t -> int

       val equal : 'a t -> 'b t -> bool

       (** If two intervals do not overlap, then comparison should
           correspond to their display order (e.g., a pre-order
           depth-first numbering of the tree works). *)
       val compare : 'a t -> 'b t -> int
       val hash : 'a t -> int

       (** [is_ancestor a b] returns true if [a] is an interval node
           containing [b], transitively. *)
       val is_ancestor : 'a t -> 'b t -> bool

       (** Returns the parent of a node, which has to be an interval
           node. *)
       val parent : 'a t -> [ `interval_node ] t

       (** Existential wrapper, which allows returning an element with an unknown kind. *)
       type any = Any : 'a t -> any

       (** [next_sibling x] is the next child of the parent of [x].  *)
       val next_sibling : 'a t -> any

       (** [previous_sibling x] is the previous child of the parent of [x].  *)       
       val previous_sibling : 'a t -> any

       (** Returns the first child of an interval node, or raise
           [Cannot_go] if there are no children.  *)
       val first_child : [ `interval_node ] t -> any

       (** Returns the last child of an interval node, or raise
           [Cannot_go] if there are no children.  *)
       val last_child : [ `interval_node ] t -> any

       (** [exists_child] returns false for single nodes, and tests
           every direct child of the interval node otherwise. *)
       val exists_child : any -> (any -> bool) -> bool

       (** Single nodes have a severity level (lowest = more severe) *)
       val severity: [ `single_node ] t -> int

       (** Display content of a single node. *)
       val view_single : [ `single_node ] t ->  'a Vdom.vdom

       (** Display content corresponding to the start of an interval node. *)       
       val view_start : [ `interval_node ] t -> 'a Vdom.vdom

       (** Display content corresponding to the end of an interval node. *)              
       val view_end : [ `interval_node ] t -> 'a Vdom.vdom
     end):
sig
  type internal_message
  
  
  type incoming_message = Internal_in of internal_message
    and outgoing_message =
    | Internal_out of internal_message
    | Goto_disasm of [ `interval_node ] Interval_node.t


  (** Initial argument =
      1. map of pairs (filter, whether the filter is activated);
      2. root node. *)
  type initial_data = (Interval_node.any Filter.t * bool) list * Interval_node.any
  
  include Component.S
    with type initial_data := initial_data
     and type incoming_message := incoming_message
     and type outgoing_message := outgoing_message 


  (** Retrieve the active filters (and whether they are currently
      activated) from the model. *)
  val get_filters: model -> (Interval_node.any Filter.t * bool) list

  (** Add a filter, or change its activation status if already here. *)
  val add_filter: model -> (Interval_node.any Filter.t * bool) -> model

  (** Remove the filter if present. *)  
  val remove_filter: model -> Interval_node.any Filter.t -> model    

  (** Compute menu entries for movement keys and open/close nodes in
      the view. *)
  val menu_entries: model -> outgoing_message Transient_menu.entry list
end
