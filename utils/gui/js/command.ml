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

(* We generally have a lot of empty commands, that we can eagerly
   remove. *)
let empty = Vdom.Cmd.Batch []

let rec is_empty: type msg. msg Vdom.Cmd.t -> bool = function
  | Vdom.Cmd.Batch [] -> true
  | Vdom.Cmd.Map(_,x) when is_empty x -> true
  | _ -> false

let batch l =
  match List.filter (fun x -> not @@ is_empty x) l with
  | [cmd] -> cmd
  | l -> Vdom.Cmd.Batch l


(* Specialised version of batch *)
let concat cmd1 cmd2 =
    Js_browser.Console.log Js_browser.console (Obj.magic "In concat");
    if is_empty cmd1 then cmd2
    else if is_empty cmd2 then cmd1
    else Vdom.Cmd.Batch [cmd1;cmd2]

(* Delay a message after a redraw. Note that it seems that it is not
   guaranee that elements will be inserted in the dom, you have to
   test and reschedule an after_redraw if needed. *)
type 'msg Vdom.Cmd.t +=
  | After_redraw of 'msg

let after_redraw_handler : Vdom_blit.Cmd.handler =
  let f: type msg. msg Vdom_blit.Cmd.ctx -> msg Vdom.Cmd.t -> bool =
    fun ctx cmd ->
    (* Js_browser.Console.log Js_browser.console (Obj.magic "In after_redraw_handler"); *)
      match cmd with
      | After_redraw m ->
        Vdom_blit.Cmd.after_redraw ctx (fun () ->
          (* Js_browser.Console.log Js_browser.console (Obj.magic "After the redraw"); *)
          Vdom_blit.Cmd.send_msg ctx m); true
    | _ -> false
  in Vdom_blit.Cmd.{f}

(* Setup a resize observer that will send the coordinates of the
   element identified by string after each resize (including the first
   time the element is displayed). *)
type 'msg Vdom.Cmd.t +=
  | Setup_resize_observer of string * (Js_browser.Rect.t -> 'msg option)

let rec setup_resize_observer ctx id f =
  Js_browser.Console.log Js_browser.console (Obj.magic ("In setup_resize_observer for " ^ id));
  let document = Js_browser.document in
  match Js_browser.Document.get_element_by_id document id with
  (* Element is not yet ready. Try again after 100ms. *)
  | None -> 
    Js_browser.Console.log Js_browser.console (Obj.magic ("Object " ^ id ^ " is absent"));
    ignore @@ Js_browser.(Window.set_timeout window)
      (fun () -> setup_resize_observer ctx id f) 100
  | Some elt ->
    Js_browser.Console.log Js_browser.console (Obj.magic ("Object " ^ id ^ " is present"));
    let open Js_of_ocaml in
    let callback = Js.wrap_callback (fun entries ->
        let entry = Js.array_get entries 0 in
        let rect = Js.Unsafe.get entry "contentRect" in
        match f rect with
        | None -> ()
        | Some msg -> Vdom_blit.Cmd.send_msg ctx msg)
    in
    let resize_observer =
      Js.Unsafe.new_obj
        (Js.Unsafe.pure_js_expr "ResizeObserver")
        [| Js.Unsafe.inject callback |]
    in
    ignore(resize_observer ## observe elt)

let resize_observer_handler : Vdom_blit.Cmd.handler =
  let f: type msg. msg Vdom_blit.Cmd.ctx -> msg Vdom.Cmd.t -> bool =
    fun ctx cmd ->
      match cmd with
      | Setup_resize_observer(id,f) ->
        setup_resize_observer ctx id f; true
      | _ -> false
  in Vdom_blit.Cmd.{f}

(* Note: an alternative would be to collect commands using effects,
   but compilation with effects to javascript is slower. *)
module Writer_monad = struct

  type ('a,'b) m = 'a * 'b Vdom.Cmd.t


  
  (* Most of what we do returns no Cmd, so we could simplify batch in
     particular, when there are no cmd. *)
  let return = Vdom.return
  let (let*) vm f =
    let v,cmd1 = vm in
    let v,cmd2 = f v in
    v,concat cmd1 cmd2

  let (let+) (v,cmds) f = f v,cmds
end
