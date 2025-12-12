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

type typ = TypedC.typ

type 'a t
(** Type checking tree *)

val pp : Format.formatter -> 'a t -> unit

val is_valid : 'a t -> bool
(** Check if the type checking is valid*)

type 'a typing_node
(** Corresponds to the node of a 'a:typ type tree checking*)

val create_typing_node :
  'a -> printer:(Format.formatter -> 'a -> unit) -> typ -> 'a typing_node
(** Create a [typing_node] from a block, a pretty printer and a type*)

val sub_check : 'a typing_node -> 'sub t -> 'a t
(** [sub_check node sub] returns a type tree with node [node] that is
    valid iff [sub] is valid
    {v
       'sub t
      -------- : 'a t
      'a node
    v}
    *)

val no_error : 'a typing_node -> 'a t
(** The type checking went smoothly and raised no errors*)

val type_error :
  'a typing_node -> [ `Typing ] Operator.Alarm.t -> 'a t
(** Emit an error and logs it onto the datatype*)

type tlist =
  | [] : tlist
  | ( :: ) : ('a t * tlist) -> tlist
      (** A custom list type for polymorphic and*)

val tlist_map : ('a -> _ t) -> 'a list -> tlist
(** Map a function returning a [type_check_tree] onto a [List.t], used in
    combination with [and_]*)

val and_ : 'a typing_node -> tlist -> 'a t
(** Construct a conjuction of [type_check_tree], eagerly evaluated
    {v
      'sub1 t ... 'subn t
      ------------------- : 'a t
            'a node
    v}
    *)

val or_ : 'a typing_node -> ('b -> _ t) -> 'b list -> 'a t
(** Lazily construct a disjunction of type tree TODO: improve this documentation*)

val try_with :
      'a 'b. 'a t -> 'b -> (typ -> [`Typing] Operator.Alarm.t -> 'b) -> 'b
(** [try_with t default catcher] tries to evaluate the typing_tree [t] to a
    default value, and catch the first potential error encoutered with [catcher]*)

val find_first_error :
      _ t -> (typ -> [`Typing] Operator.Alarm.t -> bool) -> bool
  (** [find_first_error t catcher] finds the first error in a type tree and applies the catcher, returns true if no error is found. Uses [try_with]*)

val save : 'a t -> unit
(** Imperativaly save the type checking logs and returns the error tree if the
    type checking failed*)

val number_of_errors : unit -> int
(** number of type errors logged*)

(** We need this type for higher kinded polymorphism on the different blocks typing tree*)
type poly_pred = { f : 'a. 'a t -> bool }

val for_all_logs : poly_pred -> bool
(** iterates on all the logged typing trees*)

val reset : unit -> unit
(** reset the typing tree logs*)

val dump_results : unit -> unit
(** Dump the saved results, used for debug purposes*)
