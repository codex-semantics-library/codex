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

(* TODO: Bundle also depenendencies. *)

(* A string with the binsec contents.  *)
let binsec_webapp = Binsec_webapp_string.binsec_webapp_dot_bc_dot_js
let tailwindcss = Tailwindcss_string.tailwind4_dot_1_dot_5_dot_css
let graphvizjs = Graphviz_string.graphviz_dot_umd_dot_js
(* let d3js = D3_string.d3_dot_v7_dot_min_dot_js *)
(* let d3graphvizjs = D3_graphviz_string.d3_graphviz_dot_min_dot_js *)
let bundlejs = Bundle_string.bundle_output_dot_js

let with_generate_files marshalled f =
  let filename, outc = Filename.open_temp_file ~mode:[Open_binary] "dump" "bin" in
  Marshal.to_channel outc marshalled [];
  close_out outc;
  f filename;
  Unix.unlink filename

(* Inline dependencies, so that the file can be viewed without a network connection. *)
let inline_deps = true;;

(* If true and we inline the deps, we also include worker code. This
   is faster but make the javascript a bit larger (as especially, we
   already include this in the bundle). *)
let included_inlined_worker = true;;

let print_html_webapp outc (marshalled:Interface.marshalled) =
  
  with_generate_files marshalled (fun filename -> 
      Printf.fprintf outc
        {|
<!DOCTYPE html>    
<html>
<head>

<style>
@keyframes mymount { from { transform: translateX(0) } to { transform: translateX(0) } }
.tree-controls {
  display: flex;
  flex-direction: column;
  font-size: 0.8em;
}
</style>|};


(* For some unknown reason, only d3.graphviz works when we inline
   dependencies, and only d3.select().graphviz work when we don't.
   So, we generate a d3_graphviz function to use instead of d3.graphviz. *)
Printf.fprintf outc {|
<script>function d3_graphviz(selector){ return %s; }</script>
|} (if not inline_deps then "d3.select(selector).graphviz()"
    else "d3.graphviz(selector)");

if not inline_deps then
  Printf.fprintf outc {|
<script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4.1.5"></script>
<script src="https://d3js.org/d3.v7.min.js"></script>
<script src="https://cdn.jsdelivr.net/npm/d3-graphviz@5.6.0/build/d3-graphviz.js"></script>
<script src="https://cdn.jsdelivr.net/npm/@hpcc-js/wasm@2.22.4/dist/graphviz.umd.js"></script>
|}
else begin
  Printf.fprintf outc "<script>%s</script>" tailwindcss;

  (* We pre-bundle everyting; directly loading the js does not work. *)
  Printf.fprintf outc "<script>%s</script>" bundlejs;

  (* If we want to have a worker (i.e., having graphviz perform
     computation on a separate thread), d3-graphviz requires
     graphiz.umd.js to be loaded in a script with special type
     "javascript/worker". This normally requires a URL; to circumvant
     that, we use base64 as the URL. If this fails, d3-graphviz displays
     this messages in the console:

     No script tag of type "javascript/worker" was found and "useWorker"
     is true. Not using web worker.

     Otherwise, it succeeded. *)
  if included_inlined_worker then
    Printf.fprintf outc
      (* NB: The simple version seems to suffice. *)
      (if true then {|
<script type = "javascript/worker" src="data:application/javascript;base64,%s"></script> |}
       else {|
<script>
      const base64Worker = '%s';
      const dataUrl = `data:application/javascript;base64,${base64Worker}`;
      const workerScript = document.createElement("script");
      workerScript.type = "javascript/worker";
      workerScript.src = dataUrl;
      document.head.appendChild(workerScript);
    </script>|}) (Base64.encode_string graphvizjs)
  else ();
end;
      Printf.fprintf outc "%s" File_to_html.preamble;
      File_to_html.file_to_html outc ~filename ~varid:"dataArray";  
      Printf.fprintf outc "<script>%s</script>" binsec_webapp;
      (* Better handling of errors thanks to source map when
         debugging; also we need only to recompile the ocaml, not to
         re-run the main program.. *)
      (* Printf.fprintf outc "<script src=\"/home/matthieu/git/codex-webapp/_build/default/utils/gui/js/binsec_webapp.bc.js\"></script>"; *)
      Printf.fprintf outc
        {|
  </head><body></body>
</html>
|})
;;

(* SMall test to check dependencies: *)
(*   <!-- <body><div id="graph" style="width: 100%%; height: 500px;"></div> *)
(* <script> *)
(* window.onload = function() { *)
(*   console.log("ICI");d3.graphviz("#graph").renderDot('digraph {a -> b}');console.log("ICI2");}</script> *)
(*   </body>--> *)
