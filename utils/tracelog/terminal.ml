(**************************************************************************)
(*  This file is part of the Codex semantics library.                     *)
(*                                                                        *)
(*  Copyright (C) 2013-2024                                               *)
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

open Common;;

type terminal_code_status = {
  weight: weight;
  color: color option; (* None when we don't know: the "normal" color. *)
  underline: bool;
  italic: bool;
}
let initial_state = { weight = Normal; color = None; underline = false; italic = false }

let nesting_level = ref 0;;
let open_nesting  () = incr nesting_level;;
let close_nesting  () = decr nesting_level;;

(* Sgr stands for Select Graphic Rendition *)
module AnsiSgrCodes = struct

  (* See the list here https://en.wikipedia.org/wiki/ANSI_escape_code *)
  let normal = "\027[0m";;      (* Reset to normal. *)
  let bold = "\027[1m";;


  let output_code outc str =
    if outc == stdout && Unix.isatty Unix.stdout then
      output_string outc str
    else ()
  ;;

  let color_to_code = function
    | Black -> "30"                
    | Red -> "31"
    | Green -> "32"
    | Yellow -> "33"
    | Blue -> "34"
    | Magenta -> "35"
    | Cyan -> "36"
    | White -> "37"

  let reset_code = "\027[0m";;
  let middle_gray = "\027[0;38;5;244m";;  

  (* Note that I track the status of colors myself instead of using
     the terminal.  Thus, we prepend every change with 0; to reset
     the terminal before furthre printing. We could instead remember
     the previous status, and output only the changes that we need,
     but why bother. *)

  let to_code ({weight;color;italic;underline} as _x) =
    let code = "\027[0" in
    let code = code ^ (match weight with
        | Normal -> ""
        | Bold -> ";1"
        | Faint -> ";2") in
    let code = code ^ (match italic with
        | false -> ""
        | true -> ";3") in            
    let code = code ^ (match underline with
        | false -> ""
        | true -> ";4") in
    let code = code ^ (match color with
        | None -> ""
        | Some color -> ";" ^ (color_to_code color)) in
    (* (to_string _x) ^  *)code ^ "m"

end;;

(* Note: a part of this code could be shared with the the HTML
   one. On the other hand, the "add more entities" logic is handled
   directly in HTML, so there is no need for it there. *)

(* The terminal only remembers the last code, so we need to remember
   how weight and color adds up. Hmm, not sure about that. *)
let terminal_stag_functions prev =
  let stack = Stack.create () in
  Stack.push initial_state stack;
  let mark_open_stag m =
    let exception Not_handeld in try 
      let cur = Stack.top stack in
      let newer = match m with
        | Weight weight ->{cur with weight}
        | Color color -> {cur with color = Some color}
        | Underline underline -> {cur with underline}
        | Italic italic -> {cur with italic}
        | _ -> raise Not_handeld
      in
      Stack.push newer stack;
      AnsiSgrCodes.to_code newer
    with Not_handeld ->
      (Stack.push (Stack.top stack) stack;
       prev.Format.mark_open_stag m)
  in
  let mark_close_stag _m =
    let _ = Stack.pop stack in
    let prev = Stack.top stack in
    (AnsiSgrCodes.to_code prev)
  in
  let print_open_stag _ = assert false in
  let print_close_stag _ = assert false in
  Format.{ mark_close_stag; mark_open_stag; print_open_stag; print_close_stag }
;;

let n_times_string n str =
  let len = String.length str in
  let result = Bytes.create (len * n) in
  for i = 0 to n - 1 do
    Bytes.blit_string str 0 result (i * len) len
  done;
  Bytes.unsafe_to_string result
;;

type line_characters = {
  prefix: string;               (* Prefix to change mode. *)
  vertical_bar: string;         (* │ / | *)
  right_tee: string;            (* ├ / +  *)
  horizontal_bar: string;       (* ─ / -  *)
  up_right_corner: string;      (* └ / `  *)
  postfix: string;              (* Postfix to return to normal mode. *)
}

let unicode_line_characters = {
  prefix="";
  vertical_bar = "│";           (* "\u{2502}" *)
  right_tee = "├";              (* "\u{251c}" *)
  horizontal_bar = "─";         (* "\u{2500}" *)
  up_right_corner = "└";        (* "\u{2514}" *)
  postfix = "";
};;

let vt100_line_characters = {
  prefix = "\027(0";    (* setspecg0: Switch to the graphic character set. *)
  vertical_bar = "\120";
  right_tee = "\116";
  horizontal_bar = "\113";
  up_right_corner = "\109";
  postfix = "\027(B"; (* setusg0: Return to the normal character set. *)
}
;;

let ascii_line_characters = {
  prefix = "";
  vertical_bar = "|";
  right_tee = "+";
  horizontal_bar = "-";
  up_right_corner = "`";
  postfix = ""
};;

type choice = [`Yes | `No | `Terminal | `Autodetect];;

let use_unicode: choice ref = ref `Autodetect;;

let set_use_unicode x = use_unicode := x;;

(* All the methods for line display do not work everywhere. In Emacs,
   use Unicode; in terminals, use VT100; when in doubt, display less
   pretty ASCII. *)
let rec get_line_characters () =
  (* We cache the method, which should be called only once. *)
  match !use_unicode with
  | `Yes -> unicode_line_characters
  | `No -> ascii_line_characters
  | `Terminal -> vt100_line_characters
  | `Autodetect -> begin
      let res = match Sys.getenv "TERM" with
      | exception Not_found ->
         if Unix.isatty Unix.stdout then `No
         else `Yes
      | "dumb" | "emacs" -> `Yes
      | _  when not @@ Unix.isatty Unix.stdout -> `Yes
      | "linux" | "vt100" | "konsole" | "gnome-terminal" | "xterm"
        | "xterm-256color" | "screen" | "tmux" | "cygwin" | "putty"
        | "alacritty" | "kitty" | "fbterm" ->
         `Terminal
      | s when String.starts_with ~prefix:"xterm" s -> `Terminal 
      | s when String.starts_with ~prefix:"rxvt"  s -> `Terminal 
      | s when String.starts_with ~prefix:"linux" s -> `Terminal         
      | s when String.starts_with ~prefix:"vt100" s -> `Terminal 
      | s when String.starts_with ~prefix:"vte" s ->   `Terminal
      | s when String.starts_with ~prefix:"st" s ->    `Terminal
      | s when String.starts_with ~prefix:"eterm" s -> `Terminal
      | _ -> `No            (* We don't know; be safe. *)
      in use_unicode := res;
         get_line_characters ()
    end
;;

let print_initial_indentation ~last outc level =
  (* output_string outc (string_of_int level); *)
  match level with
  | 0 -> ()
  | _ ->
    AnsiSgrCodes.(output_code outc middle_gray);      
    let lc = get_line_characters() in
    output_string outc lc.prefix;
    for _i = 0 to (level - 2) do
      (* String.init (2 * (level - 1)) (fun i -> *)
      output_string outc lc.vertical_bar;
      output_char outc ' '
    done;
    output_string outc (if last then lc.up_right_corner else lc.right_tee);
    output_string outc lc.horizontal_bar;
    output_string outc lc.postfix;
    AnsiSgrCodes.(output_code outc reset_code);      
;;

let print_normal_indentation ~last tc outc level =
  (* AnsiSgrCodes.(output_code outc reset_code); *)
  AnsiSgrCodes.(output_code outc middle_gray);  
  let lc = get_line_characters() in
  output_string outc lc.prefix;        
  if last then begin
    assert (level >= 1);
    for _i = 0 to level - 2 do
      output_string outc lc.vertical_bar;
      output_char outc ' '
    done;
    output_string outc "    ";
  end
  else begin
    for _i = 0 to level do
      output_string outc lc.vertical_bar;
      output_char outc ' '
    done
  end;
  output_string outc lc.postfix;          
  AnsiSgrCodes.(output_code outc tc);  
;;

(* Print the buffer message, taking care of properly indenting
   everything, and of outputing a last newline if there was none. *)
(* Note: I could have used a symbolc output buffer instead. *)
let printf category ~last  formatstring =
  (* let givenk_:(Format.formatter -> unit) option = k in *)
  let buf = Format.make_symbolic_output_buffer () in (* Buffer.create 300 in *)
  let nesting_level = !nesting_level in
  let tag = ("[" ^ category ^ "] ") in
  let indentation = String.length tag in
  let ppf = Format.formatter_of_symbolic_output_buffer buf in
  (* Do not print terminal codes when redirecting to a file. *)
  if Unix.isatty Unix.stdout then begin
    let formatter_tag_functions = terminal_stag_functions (Format.pp_get_formatter_stag_functions ppf ()) in
    Format.pp_set_formatter_stag_functions ppf formatter_tag_functions;
    Format.pp_set_mark_tags ppf true
  end;

  let k ppf =
    (* Get the list of things that we have to print. *)
    Format.pp_print_flush ppf ();
    let symbolic_item_list = Format.get_symbolic_output_buffer buf in

    print_initial_indentation ~last stdout nesting_level;
    (* Begin doing the real output. *)
    AnsiSgrCodes.(output_code stdout bold);
    output_string stdout tag;
    AnsiSgrCodes.(output_code stdout normal);        


    (* We are folding on the list of symbolic items, and the
       accumulator is here to "cache" things that we do not want to
       print right away. There are two things that we cache:

       - The terminal commands: we have to parse them anyway, and the
         way we produce them ensures that each command replaces the
         previous one, so we make advantage of that to only output the
         last one (we also need to remember what it was when we display
         the tree lines after a line break, to restore it when we print
         the rest of the line). It is represented as a (bool * string),
         where the string is the "current" terminal command, and bool is
         true if it was outputed.

       - The newline and following indentation: we need to track
         that to ensure that each log outputs end with a single
         newline (not 0, not two), no matter if the message was
         designed to end with a newline or not (as Frama-C does). It
         is represented as an int option, where the int is the number
         of spaces. *)
    
    let output_terminal_command = function
      | (true,_) as tc -> tc
      | (false,tc) -> output_string stdout tc; (true,tc)
    in

    let output_newline_and_indent ((_,terminal_command),newline_and_indent) =
      match newline_and_indent with
      | None -> ()
      | Some n ->
        output_string stdout "\n";
        print_normal_indentation ~last terminal_command stdout nesting_level;
        output_string stdout (String.make (n + indentation - 2) ' ')
    in

    (* The function used to fold on symbolic items. *)
    let rec do_symbolic_output_item ((terminal_command,newline_and_indent) as acc) = function
      | Format.Output_flush -> flush stdout; acc
      | Format.Output_newline -> begin
          match newline_and_indent with
          | None -> ()
          (* Two new lines in a row; we print the first, without its indentation. *)                          
          | Some _ -> output_string stdout "\n"; 
        end;
        (terminal_command,Some 0)
      | Format.(Output_spaces n | Output_indent n) -> begin
          let newline_and_indent = match newline_and_indent with
            | Some m -> Some (m + n)
            | None -> output_string stdout (String.make n ' '); None
          in terminal_command,newline_and_indent
        end
      | Format.Output_string s ->
        (* The string might be empty and only contain newline,
           spaces, and terminal_commands, in which case we do not
           want to print our newline and terminal command.

            Also we want to handle "hard" newlines and "hard" spaces
            that were not given to the format function. For all
            these reasons, we parse the string that we output.*)

        let ((terminal_command,newline_and_indent) as acc),rest = parse_for_empty_beginning acc s 0 in
        match rest with
        | "" -> acc
        | rest ->
          let terminal_command = output_terminal_command terminal_command in
          output_newline_and_indent acc;
          begin match String.index rest '\n' with
            | exception Not_found -> output_string stdout rest; (terminal_command,None)
            | i ->
              (* Print the first sentence, then continue processing for the second. *)
              let fst = String.sub rest 0 i in
              let snd = String.sub rest (i + 1) ((String.length rest) - i - 1) in
              assert(fst <> "");
              output_string stdout fst;
              let acc = (terminal_command,None) in
              let acc = do_symbolic_output_item acc Format.Output_newline in
              let acc = if snd = "" then acc else do_symbolic_output_item acc (Format.Output_string snd) in
              acc
          end

    (* We parse the string, but try to reuse the previous function on symbolic output items when feasible. *)
    and parse_for_empty_beginning acc string idx =
      (* Printf.eprintf "parsing <%s>%!\n" string; *)
      match String.get string idx with
      | exception Invalid_argument _ -> acc, ""
      | ' ' -> parse_for_empty_beginning (do_symbolic_output_item acc (Format.Output_spaces 1)) string (idx + 1)
      | '\n' -> parse_for_empty_beginning (do_symbolic_output_item acc (Format.Output_newline)) string (idx + 1)
      | '\027' ->
        parse_terminal acc string idx
      | _ -> 
        acc, String.sub string idx (String.length string - idx)

    (* We are in a terminal escape sequence, just parse it until the end. *)
    and parse_terminal (oldtc,newline_and_indent) string start_idx =
      assert (String.get string (start_idx + 1) == '[');
      let rec loop : int -> _ * _ = fun idx -> match String.get string idx with
        | ';' | '0' | '1' | '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9' -> loop @@ idx + 1
        | 'm' ->  (String.sub string start_idx (1 + idx - start_idx)), idx + 1
        | c ->
          Printf.printf "Char is %c" c;
          assert false (* Invalid terminal code, or one that we do not use. *)
      in
      let new_terminal_command,idx = loop (start_idx + 2) in
      let newtc = match oldtc with
        | (true, x) when x = new_terminal_command -> oldtc
        | _ -> (false,new_terminal_command)          
      in
      parse_for_empty_beginning (newtc,newline_and_indent) string idx
    in
    let (terminal_command,_newline_and_indent) = List.fold_left do_symbolic_output_item ((true,AnsiSgrCodes.reset_code),None) symbolic_item_list in
    (* Note that we do not consider the final newline_and_indent. *)
    output_string stdout "\n";
    (* Normally the final terminal command should be a reset command. *)
    (* Note: sometimes it is not, when there is a @. in the middle of the printing command. *)
    (* assert (snd terminal_command = AnsiSgrCodes.reset_code); *)
    let _ = output_terminal_command terminal_command in
    ()
  in Format.kfprintf k ppf formatstring
;;

