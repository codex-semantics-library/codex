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

open Format
module Cfg = Instr_cfg
module VAList = Map.Make(Virtual_address)
module Html = struct
  let header = {header|
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <script src="https://unpkg.com/htmx.org@1.7.0/dist/htmx.min.js"></script>
    <script src="https://unpkg.com/hyperscript.org@0.9.5/dist/_hyperscript_web.min.js"></script>
    <script src="https://d3js.org/d3.v5.min.js"></script>
    <script src="https://unpkg.com/@hpcc-js/wasm@0.3.11/dist/index.min.js"></script>
    <script src="https://unpkg.com/d3-graphviz@3.0.5/build/d3-graphviz.js"></script>
    <script>
      results = {};
  |header};;

  let header1 = {header|
      var dotSrc = `
  |header};;

  let header2 = {header|
      `;
      
      var ranges = [];
      var repeat;

          
      function linkGraph() {
        var graphviz = d3.select("#graph").graphviz();
        graphviz.renderDot(dotSrc).on("end", function () {
          var nodes = document.getElementsByClassName("node");
          for (let i = 0; i < nodes.length; i++) {
            repeat = undefined;
            ranges.forEach(range => {
              if (range[0] == nodes[i].children[2].innerHTML && range[1] == nodes[i].children[3].innerHTML) {
                nodes[i].children[1].classList.add(range[2]);
                repeat = range[2];
              }
            });

            if (repeat == undefined) {
              ranges.push([nodes[i].children[2].innerHTML, nodes[i].children[3].innerHTML, nodes[i].id]);
              repeat = nodes[i].id;
            }

            nodes[i].setAttribute("group", repeat);

            nodes[i].addEventListener('mouseover', () => {
              if (selected == undefined || getNodeClass(selected) != nodes[i].getAttribute("group")) {
                highlightGroup(nodes[i].getAttribute("group"), '#FEF08F');
              }
            });
            nodes[i].addEventListener('mouseout', () => {
              if (getNodeClass(selected) != nodes[i].getAttribute("group")) {
                highlightGroup(nodes[i].getAttribute("group"), 'white');
              }
            });
            nodes[i].addEventListener('click', () => {
              if(document.getElementsByClassName(nodes[i].getAttribute("group")).length != 0) {
                document.getElementsByClassName(nodes[i].getAttribute("group"))[0].scrollIntoView({ behavior: 'smooth', block: 'center' })
              }
            });

          }
          addTagsToLines();
        });
      }

      function addTagsToLines() {
        const lines = document.querySelectorAll('.line');
        lines.forEach(line => {
          range = searchRange(line.children[0].innerHTML);
          if (range != undefined) {
            line.classList.add(range);
            line.style.color = "#FFF8EB";
          }
        });
      }

      function searchRange(element) {
        var res = undefined;
        element = element.trim();
        ranges.forEach(function (range, i) {
            if (element >= range[0] && element <= range[1]) {
                res = range[2];
            }
        })
        return res;
      };

      function paintNode(node, color) {
        if (node != undefined) {
          document.getElementById(node).children[1].style.fill = color;
        }
      }

      function highlightGroup(name, color) {
        var elements = document.getElementsByClassName(name);
        Array.from(elements).forEach(element => {
          if (element.tagName == "polygon") {
            element.style.fill = color;
          } else {
            element.style.color = color;
          }
        });
        paintNode(name, color);
      }

      function getNodeClass(node) {
      if (node != undefined) {
        return node.classList[2]
      }
    }

    function loadDhunk(instructions) {
      let res = "";
      JSON.parse(instructions).forEach((instr, i) => {
        res += i + ": " + dhunk[instr] + "\n";
      })
      return res;
    }

    let selected = undefined;
    function loadExplanations() {
      const lines = document.querySelectorAll('.line');
      const explanationBox = document.querySelector('.middle-column .explanation');
      const resultsBox = document.querySelector('.middle-column .results');

      let maintain = false;
      
      lines.forEach(line => {

        line.addEventListener('mouseover', () => {
          if (getNodeClass(line) != getNodeClass(selected)) {
            highlightGroup(getNodeClass(line), '#FEF08F');
          }
          if (!maintain){
            explanationBox.textContent = loadDhunk(line.getAttribute('data-explanation'));
            explanationBox.style.display = 'block';
            var res = results[line.children[0].innerHTML.trim()];
            if (res != undefined){
              resultsBox.textContent = results[line.children[0].innerHTML.trim()].join("");
              resultsBox.style.display = 'block';
            }
          }
        });

        line.addEventListener('mouseout', () => {
          if (!maintain){
            explanationBox.style.display = 'none';
            resultsBox.style.display = 'none';
          }
          if (getNodeClass(line) != getNodeClass(selected)) {
            highlightGroup(getNodeClass(line), 'white');
          }
        });

        line.addEventListener('click', () => {
          if (!maintain) {
            highlightGroup(getNodeClass(line), '#FB7170');
            selected = line;
            line.style.color = '#bf7947';
            explanationBox.textContent = loadDhunk(line.getAttribute('data-explanation'));
            explanationBox.style.display = 'block';
            var res = results[line.children[0].innerHTML.trim()];
            if (res != undefined){
              resultsBox.textContent = results[line.children[0].innerHTML.trim()].join("");
              resultsBox.style.display = 'block';
            }
            maintain = true;
          } else {
            if (selected == line) {
              if (getNodeClass(line) != undefined) {
                highlightGroup(getNodeClass(line), '#FEF08F');
              } else {
                line.style.removeProperty('color');
              }
              maintain = false;
              selected = undefined;
            } else {
              if (getNodeClass(selected) != getNodeClass(line)) {
                if (getNodeClass(selected) == undefined) {
                  selected.style.removeProperty('color');
                  highlightGroup(getNodeClass(line), '#FB7170');
                } else {
                  highlightGroup(getNodeClass(selected), 'white');
                  highlightGroup(getNodeClass(line), '#FB7170');
                  selected.style.color = 'white';
                }
              } else {
                if (getNodeClass(selected) == undefined) {
                  selected.style.removeProperty('color');
                } else {
                  selected.style.color = '#FB7170'
                }
              }
              selected = line;
              line.style.color = '#bf7947';
              explanationBox.textContent = loadDhunk(line.getAttribute('data-explanation'));
              explanationBox.style.display = 'block';
              var res = results[line.children[0].innerHTML.trim()];
              if (res != undefined){
                resultsBox.textContent = results[line.children[0].innerHTML.trim()].join("");
                resultsBox.style.display = 'block';
              } else {
                resultsBox.textContent = "";
                resultsBox.style.display = 'none';
              }
            }
          }
        });

      });

    }
    const dhunk = {
  |header};;

  let header3 = {header|
    }
    </script>
    <style>
    body {
      font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
      background-color: #001B2E;
      color: #FFF8EB;
      font-size: 12px;
    }

    tr {
      color: gray;
    }

    * {
      box-sizing: border-box;
    }

    /* Create two equal columns that floats next to each other */
    .column {
      float: left;
      width: 33%;
      padding: 10px;
      box-shadow: 0 3px 5px rgba(0, 0, 0, 0.2);
    }

    .scrollbar::-webkit-scrollbar-track
    {
      border: 1px solid black;
      background-color: #FFF8EB;
    }

    .scrollbar::-webkit-scrollbar
    {
      width: 10px;
    }

    .scrollbar::-webkit-scrollbar-thumb
    {
      background-color: #ADB6C4;	
    }

    .hoverable:hover {
      color: #FEF08F;
    }

    .row:after {
      content: "";
      display: table;
      clear: both;
    }

    th {
      padding-right: 20px;
      text-align: left;
    }

    .explanation, .results {
      font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
      display: none;
      padding: 10px;
      border-radius: 5px;
      background-color: #294C60;
      color: #FFF8EB;
      box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      margin-top: 10px;
      overflow: auto;
      line-height: 1.5em;
    }

    .alarms {
      font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
      padding: 10px;
      border-radius: 5px;
      background-color: #294C60;
      color: #FFF8EB;
      box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      margin-top: 10px;
      overflow: auto;
    }

    .left-column:hover + .middle-column .explanation {
      display: block;
    }

    .left-column:hover + .middle-column .results {
      display: block;
    }

    svg {
      width: 100%;
      height: 80%;
    }

    .right-column {
      background-color: #294C60;
      max-height: 90vh;
      overflow: hidden;
    }
    </style>
    <title>Binsec/Codex Analysis</title>
  </head>

  <body onload="loadExplanations(); linkGraph();">
  <h2>Binsec/Codex Analysis</h2>
  <div class="row">
    <div class="column scrollbar" style="background-color:#294C60; overflow-y: scroll; height: 90vh;">
      <h2>Disassembly Results</h2>
      <table id="disasmTable">
  |header};;

  let footer = {footer|
  </table>
  </div>
  <div class="column middle-column" style="background-color:#001B2E;">
  <div style="height: 30vh;">
    <h2>DBA blocks</h2>
    <pre style="overflow: auto; max-height: 80%;" class="explanation scrollbar">Explanation will appear here.</pre>
  </div>
  <div style="height: 30vh;">
    <h2>Analysis Result</h2>
    <pre style="overflow: auto; max-height: 80%;" class="results scrollbar"></pre>
  </div>
  <div style="height: 21vh;">
  <h2>Alarms</h2>
  <div class="alarms scrollbar" style="max-height: 21vh; overflow: auto;">
  |footer};;

  let footer1 = {footer|
  </div>
  </div>
  </div>
  <div id="graph" class="column right-column" style="text-align: center;">
  </div>
  </div>
  </body>
  </html>
  |footer};;

  let logger_hashtable fmt hashtable =
    Hashtbl.iter (fun str id -> fprintf fmt "\"%d\" : \"%s\",\n" id
      (Str.global_replace (Str.regexp "\n") "\\n" 
      (Str.global_replace (Str.regexp "\\undef") "\\undef" str))
      ) hashtable
  ;;

  let add_unique_to_table hashtable instructionlist =
    List.iter (fun (addr, instr) ->
      let instrstring = Format.asprintf "%a" (Dba_printer.Ascii.pp_instruction_maybe_goto ~current_id:addr) instr in
      if not (Hashtbl.mem hashtable instrstring) 
        then Hashtbl.add hashtable instrstring (Hashtbl.length hashtable)) instructionlist
  ;;

  let rec load_addresses_from instr nextva valist hashtbl =
    let dhunk = instr.Instruction.dba_block in
    let dhunklist = Dhunk.flatten dhunk in
    add_unique_to_table hashtbl dhunklist;
    let binstr = Instruction.to_generic_instruction instr in
    let opcode_str = asprintf "%a" Instruction.Generic.pp_opcode binstr |> String.trim in
    let valist = VAList.add instr.Instruction.address (dhunk, opcode_str, binstr) valist in
      try match nextva with
      | Some validNext ->  let ni, nnva = Disasm_core.decode validNext in load_addresses_from ni nnva valist hashtbl;
      | _ -> valist
    with e -> valist
    ;;

  let rec print_dhunk hashtable fmt  = function 
  [] -> ()
  | [(addr, instr)] -> 
    let instrstring = Format.asprintf "%a" (Dba_printer.Ascii.pp_instruction_maybe_goto ~current_id:addr) instr in
    Format.fprintf fmt "%d" (Hashtbl.find hashtable instrstring);
  | (addr, instr)::xs -> 
    let instrstring = Format.asprintf "%a" (Dba_printer.Ascii.pp_instruction_maybe_goto ~current_id:addr) instr in
    Format.fprintf fmt "%d," (Hashtbl.find hashtable instrstring);
    print_dhunk hashtable fmt xs
  ;;
  
  let print_html fmt valist hashtable =
    VAList.iter (fun va (dhunk, opcode_str, binstr) ->
      let dhunklist = Dhunk.flatten dhunk in
      fprintf fmt "<tr class=\"line hoverable\" data-explanation=\"[%a]\"> <th> %a </th> <th> %s </th> <th> %a </th> </tr>\n"
        (print_dhunk hashtable) dhunklist
        Virtual_address.pp va
        opcode_str
        Instruction.Generic.pp_mnemonic binstr
    ) valist
  ;;

end
