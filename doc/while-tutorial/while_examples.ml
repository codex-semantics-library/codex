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

(** Example while programs *)

open While_ast

(* Examples from simple to more complicated. *)
let examples = [
  (* Skip. *)
  ("skip", [%while_lang skip ]);

  (* Constant assignment. *)
  ("assign", [%while_lang x := 3 ]);

  (* Sequence. *)
  ("sequence", [%while_lang x := 3; y := 5 ]);

  (* Variable. *)
  ("variable", [%while_lang y := 1; x := y ]);

  (* Arithmetic operations. *)
  ("add", [%while_lang x := 2; x := 2 * x + 1; ]);

  (* If. *)
  ("if", [%while_lang
    x := 11;
    if (x <= 10)
    then skip
    else x := 12 ]);

  (* While. *)
  ("chapter3",
(* $MDX part-begin=example_ch3 *)
[%while_lang
    x := 13;
    while(x > 3) do
       x := x - 2
    done]
(* $MDX part-end *)
);

  ("bis",
(* $MDX part-begin=example_ch3 *)
[%while_lang
    x := 13;
    if (x > 3)
    then x := x + 1
    else x := x - 1;
    y := x - 2
    ]
(* $MDX part-end *)
);



  ("if_while", [%while_lang
    x := 5 + 3;
    if x <= 10 then skip else x := x - 1;
    while x > 0 do
      x := x - 1
    done
  ]);

  ("chapter1", [%while_lang
    x := 5;
    x := x + 1;
    if x <= 10 then skip else x := x - 1;
  ]);

  ("chapter3", [%while_lang
    x := 1;
    i := 3;
    while i > 0 do
      x := x + 1;
      i := i - 1;
    done;
  ]);
]
(* Format.printf "Result is %a@." pp_com _example;; *)
