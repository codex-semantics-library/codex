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

type ('result,'internal) outgoing =
    | Internal_out of 'internal
    | Result of 'result

module type S = sig
  type result
  type internal_message
  type incoming_message = Internal_in of internal_message
  type outgoing_message = (result option,internal_message) outgoing

  include Component.S with type incoming_message := incoming_message
                       and type outgoing_message := outgoing_message
                       and type initial_data = unit

  val title: string

end

type 'a t = (module S with type result = 'a)

(* This packs the module into the model so that the user does not have
   to think about it. We also need a tag for type equality proof in
   the messages. The user only need to provide the return type (in the
   case of a modal, it will be an internal message of the composant
   making use of the view). *)
module Dynamic(Exit_message:sig type t end)(* :Component.S *) = struct

  (* Pack a modal module, a model for this modal, and a unique tag
     used to identify the type of the important types in the component. *)
  type model =
    | Model : { tag: ('model * 'result * 'internal_message) Tag.t;
                module_: (module S with type model = 'model
                                    and type result = 'result
                                    and type internal_message = 'internal_message);
                f: 'result option -> Exit_message.t;
                model:  'model
              } -> model
    | No_model


  (* We reuse the tag in the internal message, which proves that it
     corresponds to the types of the model. *)
  type internal_message =
    (* Message from the modal component to itself. *)
    | Internal: (_ * _ * 'internal_message) Tag.t * 'internal_message -> internal_message

    (* When the modal component has finished, it sends this message,
       that we catch, notably to close the modal. We should not
       directly send Result, as otherwise the message won't get
       closed. *)
    | Internal_result of Exit_message.t

    | Nop

  type incoming_message =
    | Internal_in of internal_message
    | Display_modal: 'a t * ('a option -> Exit_message.t) -> incoming_message

  type outgoing_message = (Exit_message.t, internal_message) outgoing

  let lift_message
      (type internal_message result)
      (tag: (_ * result * internal_message) Tag.t)
      (f: result option -> Exit_message.t)
      (msg: (result option,internal_message) outgoing) : outgoing_message =
    match msg with
    | Internal_out msg -> Internal_out (Internal(tag,msg))
    | Result x -> Internal_out (Internal_result (f x))


  type initial_data = unit

  let init () = Vdom.return No_model

  let new_modal (type result) (component:result t) f =
    let (module C) = component in
    let model,cmds = C.init () in
    let tag: (C.model * C.result * C.internal_message) Tag.t = Tag.create () in
    (Model{tag;module_=(module C);model;f}), Vdom.Cmd.map (lift_message tag f) cmds

  (* The invariant for update is that the tag of the model and msg
     correspond. We can't enforce it statically, hence the use of
     dynamic tags. *)
  let update model msg =
    match model, msg with
    | _, Internal_in Nop -> Vdom.return model
    | _, Internal_in(Internal_result r) -> begin
        Vdom.return ~c:[Vdom.Cmd.echo (Result r)] No_model
      end
    | _, Display_modal(m,f) -> new_modal m f
    | (Model{tag;module_;model;f}), Internal_in (Internal(tag2,msg)) ->
      let (module C) = module_ in
      let Refl = Tag.equal tag tag2 in
      let model,cmds = C.update model (C.Internal_in msg) in
      (Model{tag;module_;model;f}), Vdom.Cmd.map (lift_message tag f) cmds
    | No_model, Internal_in _ ->
      (* This, or the case where the message and model have a tag that
         do not match, should normally not happen; but it could if the
         modal performs an asynchronous command that arrives after it
         has been dismissed. If this happened, we should ignore the
         message. *)
      assert false


  (* let view = function
    | No_model -> Vdom.fragment []
    | (Model{tag;module_=(module C);model;f}) ->
      Vdom.map (lift_message tag f) @@ C.view model *)

end

(* Here, we extend on dynamic to handle the views, and the problem of
   handling click to return an absence of result. *)
module Display_modal(Exit_message:sig type t end) = struct
  include Dynamic(Exit_message)

  let is_modal = function
    | No_model -> false
    | Model _ -> true

  let close_message f = Internal_out (Internal_result (f None))

  let view = function
    | No_model -> Vdom.fragment []
    | (Model{tag;module_=(module M);model;f}) -> begin
        let open Vdom in
        let open Vdom_ext in
        (* The external div is a semi-translucid overlay area. If you click on it,
           the modal is dismissed. *)
        div ~a:([autofocus;
                 onclick (fun _ -> close_message f);
                 class_ "fixed inset-0 bg-black/50 flex items-center justify-center z-50"])
          [ div ~a:[class_ "relative bg-white rounded-lg shadow-lg max-w-md w-full p-6"]
              [ button ~a:[ class_ "absolute top-2 right-2 cursor-pointer text-3xl text-gray-500 hover:text-black"
                          ; onclick (fun _mouse_event -> close_message f)
                          ; onkeydown (fun ev ->
                                if ev.key = "Escape"
                                then close_message f
                                else Internal_out Nop)
                          ]
                  [text "×"]
              ; h2 ~a:[class_ "text-xl font-semibold mb-4"] [text M.title]
              ; p [Vdom.map (lift_message tag f) @@ M.view model]
              ]
          ]
      end

end
