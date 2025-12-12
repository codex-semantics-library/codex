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

open Modal

module Input_String(Arg:sig
    val title: string
    val suggestions: string -> max:int -> string * string list
    (** Given the current input string, return a list of suggestion
        (whose length should be smaller than max), and a common prefix
        for all these suggestions (that can be used for auto-completion)
        
        Do not display a suggestion when the list is empty. *)
    
  end):S with type result = string = struct

  type result = string
  type internal_message =
    | Set_text of string

  type incoming_message = Internal_in of internal_message
  type outgoing_message = (result option,internal_message) outgoing

  type initial_data = unit  
  type model = { content:string}

  open Vdom
  
  let init () =
    let model = {content = ""} in
    Vdom.return model
  
  let update m =  function
    | Internal_in (Set_text s) -> Vdom.return {content = s} 

  let title = Arg.title


  let view_suggestions model : string * outgoing_message Vdom.vdom =
    let max = 30 in             (* Custom limit to number of completions. *)
    let completion,suggestions = Arg.suggestions model.content ~max in
    let return x = (completion,x) in
    if suggestions = []
    then return @@ Vdom.fragment [] 
    else return begin

      (* We format the typed part in bold blue.
         If we can tab-complete, then the completable part is in bold black.
         If we cannot, then the next differentiating letter is in bold red. *)
      let format_suggestion =
        let typed = model.content in
        let open Vdom in let open Vdom_ext in        
        if completion <> "" then
          let typed_completion = typed ^ completion in
          let len_typed_completion = String.length typed_completion in
          fun x ->
            assert(String.starts_with ~prefix:typed_completion x);
            let rest = String.sub x len_typed_completion (String.length x - len_typed_completion) in
            fragment
              [ span ~a:[class_ "text-blue-600 font-semibold"] [text typed]
              ; span ~a:[class_ "font-semibold"] [text completion]
              ; text rest   
              ]
        else
          let len_typed = String.length typed in
          fun x ->
            assert(String.starts_with ~prefix:typed x);
            let len_x = String.length x in
            let typed_format =
              span ~a:[class_ "text-blue-600 font-semibold"] [text typed]
            in
            if len_x > len_typed
            then
              let char = String.get x len_typed in
              let char = String.make 1 char in
              let rest = String.sub x (len_typed + 1) (len_x - len_typed - 1) in
              fragment
                [ typed_format
                ; span ~a:[class_ "text-red-600 font-semibold"] [text char]
                ; text rest ]
            else begin
              assert (String.length x = len_typed);
              typed_format
            end
      in
      
      let lis =
        suggestions |> List.map
          (fun txt ->
             (* XXX: Just to see the completion.
                XXX: Maybe I can just style the prefix part of the suggestions, and leave the box as is.
                Three colors: in bold dark blue what already matches, in bold what can be completed, and in normal the rest.

                Je vais faire ainsi:
                - blue pour ce qui est matche;
                - si j'ai une completion: bold pour ce que tab peut completer;
                - si je n'ai pas de completion: la premiere lettre en rouge pour montrer comment se distinguent les candidats;
                - le reste en noir normal.
                
             *)
             Vdom_ext.li ~a:[class_ "px-3 py-2 cursor-pointer hover:bg-gray-100"]
               [format_suggestion txt
                  (* Vdom.text txt; Vdom.text "|"; Vdom.text completion *)]) in
      Vdom_ext.ul ~a:[class_ ("absolute z-10 w-full mt-1 bg-white border border-gray-300 "
                              ^ "rounded-md shadow-lg max-h-[40vh] overflow-y-auto")] lis
    end
  
  let view model : outgoing_message Vdom.vdom =

    let open Vdom in
    let open Vdom_ext in
    let completion,suggestions = view_suggestions model in
    let input = 
      Vdom.input
        ~a:[ type_ "text"
           ;class_ "w-full border border-gray-300 rounded px-3 py-2 focus:outline-none focus:ring"
           ; value model.content
           ; autofocus
           ; oninput (fun s -> Internal_out(Set_text s))
           ; onkeydown_with_options
               (fun  ev ->
                  let stop_propagation = true in
                  let capture_and_send_message msg =
                    {msg=Some msg;stop_propagation;prevent_default=true} in
                  if ev.key = "Enter"
                  then capture_and_send_message (Result (Some model.content))
                  else if ev.key = "Escape"
                  then capture_and_send_message (Result None)
                  (* TODO: Ctrl-Backspace = remove, but beware of camlcase and snake case. *)
                  else if ev.key = "Tab"
                  then capture_and_send_message (Internal_out(Set_text (model.content ^ completion)))
                  (* Normal key: use normal behaviour, which is to write in the form. *)
                  else {msg=None;stop_propagation;prevent_default=false})
           ] [] in
    Vdom.fragment [input;suggestions]

end

let input_string ~title ~suggestions : string t =
  (module Input_String(struct
       let title = title
       let suggestions = suggestions
     end))
