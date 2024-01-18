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

(* ML version of the builtins, which are useful for using with the REPL. *)

(* -v land v gives a number with a single 1 whose position is the one of the last non-0 bit.
   A modulo with the prime number 37 assigns each of them a unique number.  *)
let count_trailing_zeroes v =
  let idx = ((-v) land v) mod 37 in
  [|  32; 0; 1; 26; 2; 23; 27; 0; 3; 16; 24; 30; 28; 11; 0; 13; 4;
      7; 17; 0; 25; 22; 31; 15; 29; 10; 12; 6; 0; 21; 14; 9; 5;
      20; 8; 19; 18   |].(idx)
;;

let find_last_set v = count_trailing_zeroes v + 1;;
