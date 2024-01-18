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

(** printf [category] fmt prints a text in a terminal in a pretty way
    (using terminal colors, indentation, and guidelines), or using
    plain text if the output is redirected to a file. Also ensures
    that each logs ends with one newline. *)
val printf : string -> last:bool -> ('a, Format.formatter, unit) format -> 'a

val open_nesting: unit -> unit
val close_nesting: unit -> unit


(** Whether to use unicode.
    - Yes: directly display unicode character.
    - No: use ascii replacements.
    - Terminal: use vt100 graphic character (that are translated to unicode by the terminal).
    - Autodetect: try to find the most suitable option. *)
val set_use_unicode: [`Yes | `No | `Terminal | `Autodetect] -> unit
