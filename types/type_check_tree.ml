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

module Log = Tracelog.Make (struct
  let category = "Type_check_tree"
end)

type typ = TypedC.typ
type 'a formatter = Format.formatter -> 'a -> unit
type 'a typing_node = { block : 'a; printer : 'a formatter; typ : typ }

type 'a t = {
  typing_node : 'a typing_node;
  type_tree : type_tree;
  sub_errors : [ `Typing ] Operator.Alarm.t list;
}

and type_tree =
  | Error of [ `Typing ] Operator.Alarm.t
  | Correct
  | Sub : 'a t -> type_tree
  | And of tlist
  | Or of tlist

and tlist = [] : tlist | ( :: ) : ('a t * tlist) -> tlist

type poly_pred = { f : 'a. 'a t -> bool }

let rec tlist_map f = function
  | List.[] -> []
  | List.( :: ) (x, q) -> f x :: tlist_map f q

let rec tlist_for_all poly_pred = function
  | [] -> true
  | x :: q -> poly_pred.f x && tlist_for_all poly_pred q

let rec try_k :
    'a.
    'a t ->
    (unit -> 'b) ->
    (typ -> [ `Typing ] Operator.Alarm.t -> 'b) ->
    'b =
 fun t k kerr -> try_type_tree t.typing_node.typ t.type_tree k kerr

and try_type_tree typ type_tree k kerr =
  match type_tree with
  | Error err -> kerr typ err
  | Correct -> k ()
  | Sub t -> try_k t k kerr
  | And [] | Or [] -> k ()
  | And (t :: q) -> try_k t (fun () -> try_type_tree typ (And q) k kerr) kerr
  | Or (t :: q) -> try_k t k (fun _ _ -> try_type_tree typ (Or q) k kerr)

let try_with t default catcher = try_k t (fun _ -> default) catcher
let find_first_error t catcher  = try_with t true catcher

let rec is_valid : 'a. 'a t -> bool =
 fun { type_tree } -> is_valid_type_tree type_tree

and is_valid_type_tree = function
  | Error _ -> false
  | Correct -> true
  | Sub t -> is_valid t
  | And [] -> true
  | Or [] -> false
  | And (x :: q) -> is_valid x && (is_valid_type_tree @@ And q)
  | Or (x :: q) -> is_valid x || (is_valid_type_tree @@ Or q)

let rec pp : 'a. 'a t formatter =
 fun fmt { typing_node; type_tree } ->
  Format.fprintf fmt "@[%a@;%a@]" pp_typing_node typing_node pp_type_tree
    type_tree

and pp_typing_node fmt { block; printer; typ } =
  Format.fprintf fmt "%a: %a" printer block TypedC.pp typ

and pp_type_tree fmt type_tree =
  let open Format in
  match type_tree with
  | Error err -> fprintf fmt "ERROR %s" (Operator.Alarm.show err)
  | Correct -> fprintf fmt "OK"
  | Sub t -> pp fmt t
  | And [] | Or [] -> fprintf fmt ""
  | And l -> pp_list ~sep:"AND" fmt l
  | Or l -> pp_list ~sep:"OR" fmt l

and pp_list ~sep fmt l =
  let open Format in
  match l with
  | [] -> fprintf fmt ""
  | [ x ] -> fprintf fmt "%a" pp x
  | x :: q -> fprintf fmt "%a@;%s %a" pp x sep (pp_list ~sep) q

let create_typing_node block ~printer typ = { block; printer; typ }

let sub_check typing_node sub =
  { typing_node; type_tree = Sub sub; sub_errors = sub.sub_errors }

let no_error typing_node = { typing_node; type_tree = Correct; sub_errors = [] }

let type_error typing_node err =
  Emit_alarm.emit_alarm err;
  { typing_node; type_tree = Error err; sub_errors = [ err ] }

let rec get_errors = function
  | [] -> List.[]
  | { sub_errors } :: q -> sub_errors @ get_errors q

let and_ typing_node l =
  { typing_node; type_tree = And l; sub_errors = get_errors l }

let or_ typing_node f l =
  let rec or_type_tree f l =
    match l with
    | List.[] -> []
    | List.( :: ) (x, q) ->
        let t = f x in
        if is_valid t then [ t ] else t :: or_type_tree f q
  in
  { typing_node; type_tree = Or (or_type_tree f l); sub_errors = [] }

module TypeLogs () = struct
  type poly_iter = { f : 'a. 'a t -> unit }

  let rec iter (poly : poly_iter) = function
    | [] -> ()
    | x :: q ->
        poly.f x;
        iter poly q

  let rec length = function [] -> 0 | _ :: q -> 1 + length q
  let logs = ref []
  let number_of_errors () = length !logs
  let reset () = logs := []
  let for_all_logs poly_pred = tlist_for_all poly_pred !logs

  let save t =
    Log.debug (fun p -> p "SAVING RESULTS");
    if not @@ is_valid t then logs := t :: !logs

  let dump_results () =
    Log.debug (fun p -> p "DUMPING RESULTS");
    iter { f = (fun t -> Log.debug (fun p -> p "%a" pp t)) } !logs
end

module TL = TypeLogs ()

let save = TL.save
let number_of_errors = TL.number_of_errors
let reset = TL.reset
let for_all_logs = TL.for_all_logs
let dump_results = TL.dump_results
