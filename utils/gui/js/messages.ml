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

(** Messages are used update to the state, that will also change the
    view. They are also used for communication between different
    components.

    More precisely: messages are update to the state that come from a
    user interaction in the view. Normally, there is no need to create
    a message if used only internal communication between components;
    state-updating functions should suffice for this.

    There are several kind of messages:

    - Internal messages: originates from one component, and handled by
    that component alone. Ideally, we would not see them here and
    define them in each component. This is hard due to the fact that
    we cannot have recursive dependencies. However, we use extensible
    records for this purpose: constructors for internal messages are
    define inside each component, and thus their scope is local.

    - Global "semantic" messages, that will be handled globally
    (e.g. go to location, go to trace event, highlight location, etc.,
    for an abstract notion of location), and are defined here. If the
    message target is a single component, this can be a message to
    that component. Otherwise, you should find the appropriate parent
    in the component tree that should handle this message, up to the
    global "App".

    Note that due to intermal messages, we group messages by
    component; and each component handle messages of the type
    corresponding to that component. For this reason, messages are
    grouped by their destination, not by their origin. *)


(* Note: As use panes in messages (and other places), i.e. it is an
   important mean of communication between components, so we define it
   here). *)
