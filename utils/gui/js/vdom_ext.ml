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

type 'msg elt = ?key:string -> ?a:'msg Vdom.attribute list -> 'msg Vdom.vdom list -> 'msg Vdom.vdom
    
let colspan = Vdom.attr "colspan"
let placeholder x = Vdom.Property ("placeholder", String x)

(* MAYBE: make the code compatible with mlx: https://github.com/ocaml-mlx/mlx. *)
(* All tags in alphabetical order *)
let body   ?key ?a sub = Vdom.elt "body"   ?key ?a sub
let br     ?key ?a sub = Vdom.elt "br"     ?key ?a sub
let button ?key ?a sub = Vdom.elt "button" ?key ?a sub
let details ?key ?a sub = Vdom.elt "details" ?key ?a sub    
let div    ?key ?a sub = Vdom.elt "div"    ?key ?a sub
let footer ?key ?a sub = Vdom.elt "footer" ?key ?a sub
let h1     ?key ?a sub = Vdom.elt "h1"     ?key ?a sub    
let h2     ?key ?a sub = Vdom.elt "h2"     ?key ?a sub
let header ?key ?a sub = Vdom.elt "header" ?key ?a sub
let input  ?key ?a sub = Vdom.elt "input"  ?key ?a sub
let label  ?key ?a sub = Vdom.elt "label"  ?key ?a sub
let li     ?key ?a sub = Vdom.elt "li"     ?key ?a sub    
let main   ?key ?a sub = Vdom.elt "main"   ?key ?a sub    
let p      ?key ?a sub = Vdom.elt "p"      ?key ?a sub
let pre    ?key ?a sub = Vdom.elt "pre"    ?key ?a sub    
let span   ?key ?a sub = Vdom.elt "span"   ?key ?a sub    
let summary ?key ?a sub = Vdom.elt "summary" ?key ?a sub    
let table  ?key ?a sub = Vdom.elt "table"  ?key ?a sub    
let tbody  ?key ?a sub = Vdom.elt "tbody"  ?key ?a sub        
let td     ?key ?a sub = Vdom.elt "td"     ?key ?a sub
let thead  ?key ?a sub = Vdom.elt "thead"  ?key ?a sub
let th     ?key ?a sub = Vdom.elt "th"     ?key ?a sub    
let tr     ?key ?a sub = Vdom.elt "tr"     ?key ?a sub
let ul     ?key ?a sub = Vdom.elt "ul"     ?key ?a sub    

type key_event =
  {key:string; ctrl_key:bool;shift_key:bool;alt_key:bool}

(* We redefine onkeydown to give use the 'key' field, instead of
   'which'. *)
let onkeydown ?prevent_default ?stop_propagation f =
  let open Vdom.Decoder in
  let key_event =
    let+ key = field "key" string
    and+ alt_key = field "altKey" bool
    and+ ctrl_key = field "ctrlKey" bool
    and+ shift_key = field "shiftKey" bool in
    Some (f {key; alt_key; ctrl_key; shift_key})
  in
  Vdom.on ?prevent_default ?stop_propagation "keydown" key_event

(* Note on on_with_options:
   - If the returned msg is None, we don't send any message;
   - we can choose whether to stop propagation or prevent the default
   action depending on the key. *)
let onkeydown_with_options f =
  let open Vdom.Decoder in
  let key_event =
    let+ key = field "key" string
    and+ alt_key = field "altKey" bool
    and+ ctrl_key = field "ctrlKey" bool
    and+ shift_key = field "shiftKey" bool in
    f {key; alt_key; ctrl_key; shift_key}
  in
  Vdom.on_with_options "keydown" key_event

let ontransitionend ?prevent_default ?stop_propagation f =
  let open Vdom.Decoder in
  let event =
    let+ () = Vdom.Decoder.unit in
    Some (f ())
  in
  Vdom.on ?prevent_default ?stop_propagation "transitionend" event

let ontransitioncancel ?prevent_default ?stop_propagation f =
  let open Vdom.Decoder in
  let event =
    let+ () = Vdom.Decoder.unit in
    Some (f ())
  in
  Vdom.on ?prevent_default ?stop_propagation "transitioncancel" event
