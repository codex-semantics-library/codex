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

open Printf


(** The goal of this file is to inline binary files in a HTML file,
    where the binary file will be transformed into a Javascript
    Uint8Array. This simplify UI design as no web server, request,
    protocol etc. is needed to access the data, and it works with
    relatively large files (up to 2GB on Chromium).

    The general strategy is the following:
    - We generate the binary data as chunks of base64 text inside script tags, e.g.
    <script id="binary-data0" type="application/octet-stream">aXhHSU0vNVp4Uno5d3...V2aFJKSUpkT</script>
    <script id="binary-data1" type="application/octet-stream">QCBAgYBEQAmEQEAwAA...AQQoIIIAASk</script>    

    The fact that we put it inside script tag means that it won't be displayed.
    
    - We generate some code that takes all these chunks and fill a
    variable containing javascript array from it, in another script
    tag, so that the array is available from the browser. 

    <script>const dataArray = ... load array ...</script>

    The performance of doing so is OK; e.g., we need 30s to load 2GB of binary data into Firefox.
    Note that Chrome has a limitation that forbids it to allocate arrays of size > 2GB.

    Ideally, we could use wasm to do the same thing more efficiently. *)

(* Note: we could have a more efficient encoding for text files, where
   it suffices to escape the text instead of doing a base64
   encoing. But we can just consider everything a binary file. *)

let chunk_size = 1024 * 1024
(* 1 megabyte. This chunk size was chosen arbitrarily. *)


(** This preamble contains the function that turn a binary chunk into
    the array. FILE2HTML__setBase64 copy the binary chunk to the array at
    offset offset.  FILE2HTML__CreateArray allocates an array of size
    [totalSize] filled with data labeled with [id].

    Note: Unfortunately, we need an intermediary conversion pass to
    javascript strings, where each character is represented as a
    short. *)
let preamble = (sprintf {|
<script>
      function FILE2HTML__setBase64(targetArray, offset = 0, id) {
        const element = document.getElementById(id);
        const base64String = element.textContent;
        const binaryString = atob(base64String);
        const length = binaryString.length;
        for (let i = 0; i < length; i++) {
          targetArray[offset + i] = binaryString.charCodeAt(i);
        }
        element.remove();
      }
function FILE2HTML__CreateArray(totalSize, id){
    let max_offset = totalSize / %d;
    const array = new Uint8Array(totalSize);
    for(let i = 0; i < max_offset; i++) FILE2HTML__setBase64(array,i*%d,id.concat(i));
    return array;
}
</script>
|} chunk_size chunk_size);;

(* Write in [outc] a sequence of <script id=%idnum>base64</script>
   corresponding to the content of filename.  Also return the length
   of the file/string as it will be useful for the javascript part. *)
let generate_data outc filename id =
  let inc = open_in_bin filename in
  let len = in_channel_length inc in
  let buffer = Bytes.create chunk_size in

  let rec loop i offset =
    if offset >= len then ()
    else
      let bytes_to_read = min chunk_size (len - offset) in
      (* Use a buffer of appropriate size: we don't want to print the
         full buffer if we don't use it. *)
      let buffer =
        if bytes_to_read = chunk_size then buffer
        else Bytes.create bytes_to_read
      in
      Printf.printf "bytes_to_read %d chunk_size %d len-offset %d\n"
        bytes_to_read chunk_size (len - offset);
      really_input inc buffer 0 bytes_to_read;
      Printf.fprintf outc
        "<script id=\"%s%d\" type=\"application/octet-stream\">%s</script>\n" id
        i (Base64.encode_string (Bytes.unsafe_to_string buffer));
      loop (i + 1) (offset + chunk_size)
  in
  loop 0 0;
  len

(** Generate Javascript that loads (and delete) the data in chunks
    named [id], and put it in variable [varid], which is a Uint8Array
    of length [len]. *)
let generate_js_loading_data outc ~varid ~id len =
  Printf.fprintf outc
    "<script>const %s = FILE2HTML__CreateArray(%d,\"%s\");</script>"
    varid len id

(** Generate HTML script in [outc] such that the content of [filename]
    will be placed in the variable [varid], which is a Uint8Array.  *)
let file_to_html outc ~filename ~varid =
  let id = "__file2html__" ^ varid in 
  let len = generate_data outc filename id in
  generate_js_loading_data outc ~varid ~id len
;;


(* Sample code that shows how to use file_to_html *)
let%test_module "File_to_HTML" = (module struct

(* Generate the full HTML *)
let generate_html outc filename =

  Printf.fprintf outc {|
<!DOCTYPE html>
<html lang="en">
<head>
    <title>File_to_html test</title>
</head>
<body>
|};
  Printf.fprintf outc "%s" preamble;
  let len = generate_data outc filename "binary-data" in
  generate_js_loading_data outc ~varid:"dataArray" ~id:"binary-data" len;
  Printf.fprintf outc {|
    <h1>File_to_html test</h1>
    <p id="result"></p>	<!-- Make room for the display of the result. -->
    <script>
        // Traverse the ArrayBuffer to count bytes with the value 11 (0x0B)
        let count = 0;
        for (let i = 0; i < dataArray.length; i++) {
            if (dataArray[i] === 11) {
                count++;
            }
        }
        // Display the result
        document.getElementById("result").textContent =
            `The concatenated ArrayBuffer contains ${count} bytes with the value 11 (0x0B).`;
    </script>
</body>
</html>
|} ;;

(* Main program *)
let () =
  let input_file = "file_to_html.mli" in
  let output_file = "file_to_html.mli.html" in

  (* Generate HTML *)
  let outc = open_out output_file in  
  let () = generate_html outc input_file in
  close_out outc;

  printf "HTML file '%s' generated successfully.\n" output_file

  
end)
