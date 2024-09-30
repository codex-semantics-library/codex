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

module Additive = Constraints.Relations.Additive

module Const_eval = Constraints.Const_eval

module MakeAdditive
    (C: Constraint_domains_sig.Domain_S
        with type ('a, 'b) Constraints.Relation.t = ('a, 'b) Additive.t) =
struct
  include C

  (** Maybe: export this so it doesn't have to be recreated here ? *)
  module Const_eval = Const_eval.Make(Constraints)

  module Binary_Forward = struct
    include C.Binary_Forward

    let union child parent relation =
      match C.Constraints.UnionFind.union child parent relation with
      | Ok () -> ()
        (* Format.printf "Union %a --(%a)--> %a"
          Constraints.pretty child
          Additive.pretty relation
          Constraints.pretty parent *)
      | Error old_relation ->
        (* Codex_log.feedback "Union %a --(%a)--> %a"
          Constraints.pretty child
          Additive.pretty relation
          Constraints.pretty parent; *)
        Codex_log.fatal "Union between two related terms (new relation: %a, old relation: %a)@."
          Additive.pretty relation
          Additive.pretty old_relation

    let biadd ~size ~nsw ~nuw ~nusw dom a b res =
      (* Codex_log.feedback "BIADD %a = %a + %a"
      Constraints.pretty res
      Constraints.pretty a
      Constraints.pretty b; *)
      let a_const = Constraints.level a == -1 in
      let b_const = Constraints.level b == -1 in
      try
        (* Perform union if one and only one of the arguments is a constant *)
        if a_const && not b_const then
          (* let val_res = query_binary res dom in *)
          let () = union res b (Additive.additive_binary
            ~size Additive.PlusOne (Const_eval.binary a)) in
          dom
        else if b_const && not a_const then
          let () = union res a (Additive.additive_binary
            ~size Additive.PlusOne (Const_eval.binary b)) in
          dom
        else raise Const_eval.Not_a_constant
      with
      | Const_eval.Empty
      | Const_eval.Not_a_constant ->
          Binary_Forward.biadd ~size ~nsw ~nuw ~nusw dom a b res

    let bisub  ~size ~nsw ~nuw ~nusw dom a b res =
      (* Codex_log.feedback "BISUB %a = %a - %a"
      Constraints.pretty res
      Constraints.pretty a
      Constraints.pretty b; *)
      let a_const = Constraints.level a == -1 in
      let b_const = Constraints.level b == -1 in
      try
        if a_const && not b_const then
          let () = union res b (Additive.additive_binary
            ~size Additive.MinusOne (Const_eval.binary a)) in
          dom
        else if b_const && not a_const then
          let () = union res a (Additive.additive_binary
            ~size Additive.PlusOne (Z.neg (Const_eval.binary b))) in
          dom
        else raise Const_eval.Not_a_constant
      with
      | Const_eval.Empty
      | Const_eval.Not_a_constant ->
          Binary_Forward.bisub ~size ~nsw ~nuw ~nusw dom a b res
  end
end
