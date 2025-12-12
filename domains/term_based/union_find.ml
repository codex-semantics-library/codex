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

module Additive = Terms.Relations.Additive
module Linear = Terms.Relations.LinearTwoVarEquality
module Const_eval = Terms.Const_eval
module In_bits = Units.In_bits

module MakeAdditive
    (C: Term_based_sig.DOMAIN_WITH_UNION
        with type ('a, 'b) Terms.Relation.t = ('a, 'b) Additive.t) =
struct
  include C

  (** Maybe: export this so it doesn't have to be recreated here ? *)
  module Const_eval = Const_eval.Make(Terms)


  module Binary_Forward = struct
    include C.Binary_Forward

    let biadd ~size ~flags dom a b res =
      let a_const = Terms.level a == -1 in
      let b_const = Terms.level b == -1 in
      try
        (* Perform union if one and only one of the arguments is a constant *)
        if a_const && not b_const then
          (* let val_res = query_binary res dom in *)
          C.union dom res b (Additive.additive_bitvector ~size Additive.PlusOne (Const_eval.bitvector a))
        else if b_const && not a_const then
          C.union dom res a (Additive.additive_bitvector ~size Additive.PlusOne (Const_eval.bitvector b))
        else raise Const_eval.Not_a_constant
      with
      | Const_eval.Empty
      | Const_eval.Not_a_constant ->
          Binary_Forward.biadd ~size ~flags dom a b res

    let bisub  ~size ~flags dom a b res =
      let a_const = Terms.level a == -1 in
      let b_const = Terms.level b == -1 in
      try
        if a_const && not b_const then
          C.union dom res b (Additive.additive_bitvector ~size Additive.MinusOne (Const_eval.bitvector a))
        else if b_const && not a_const then
          C.union dom res a (Additive.additive_bitvector ~size Additive.PlusOne (Z.neg (Const_eval.bitvector b)))
        else raise Const_eval.Not_a_constant
      with
      | Const_eval.Empty
      | Const_eval.Not_a_constant ->
          Binary_Forward.bisub ~size ~flags dom a b res
  end

  (* module Boolean_Forward = struct
    include C.Boolean_Forward

    let not dom a res =
      (* Format.printf "LET %a = NOT %a@." Terms.pretty res Terms.pretty a; *)
      union res a Additive.boolean_not;
      dom
  end *)
end

module MakeLinearTwoVarEquality
    (C: Term_based_sig.DOMAIN_WITH_UNION
        with type ('a, 'b) Terms.Relation.t = ('a, 'b) Linear.t) =
struct
  include C

  module Const_eval = Const_eval.Make(Terms)

  let is_constant ?(syntaxic_only=true) ~size dom term =
    if Terms.level term == -1
    then
      try Some (Const_eval.binary term) (* Syntaxic constants *)
      with Const_eval.Empty -> None
    else
      if syntaxic_only then None
      else Query.Binary_Lattice.is_singleton ~size (Query.binary ~size dom term) (* Semantic constants *)

  module Binary_Forward = struct include Binary_Forward
    let biadd ~size ~flags dom a b res =
      match is_constant ~size dom a, is_constant ~size dom b with
      | Some a, None ->
          (* res = b + cst so res - b = cst *)
          C.union dom res b (Linear.make ~size ~f1:Z.one ~f2:Z.minus_one a)
      | None, Some b ->
          C.union dom res a (Linear.make ~size ~f1:Z.one ~f2:Z.minus_one b)
      | _, _ -> biadd ~size ~flags dom a b res

    let bisub ~size ~flags dom a b res =
      match is_constant ~size dom a, is_constant ~size dom b with
      | Some a, None ->
          (* res = cst - b so res + b = cst *)
          C.union dom res b (Linear.make ~size ~f1:Z.one ~f2:Z.one a)
      | None, Some b ->
          (* res = a - cst so res - a = - cst*)
          C.union dom res a (Linear.make ~size ~f1:Z.one ~f2:Z.minus_one (Z.neg b))
      | _, _ -> bisub ~size ~flags dom a b res

    let bimul ~size ~flags dom a b res =
      match is_constant ~size dom a, is_constant ~size dom b with
      | Some a, None when not (Z.equal a Z.zero) ->
          C.union dom res b (Linear.make ~size ~f1:Z.one ~f2:Z.(- a) Z.zero)
      | None, Some b when not (Z.equal b Z.zero) ->
          (* res = a * cst so res - cst * a = 0 *)
          C.union dom res a (Linear.make ~size ~f1:Z.one ~f2:Z.(- b) Z.zero)
      | _, _ -> bimul ~size ~flags dom a b res
  end

  (** [add_constant ~size dom xa xb x_var opt]
      Adds the new join variable [x_var = phi(xa, xb)] where [xa] and [xb] are constants
      If there are other [phi(const_a, const_b)], then [x_var] can be related to them
      via a new linear relation.
      Doing so amounts to finding a line between two points.

      The final argument [opt] is a triplet [ya,yb,yvar] of the previously encountered
      constant variable.

      This function returns
      - the updated [opt] (either [xa,xb,xvar] or [ya,yb,yvar], depending on which
        was chosen as reprensentative)
      - the updated domain after union was performed.

      This function assumes [xa-xb != 0] and [ya-yb != 0]. *)
  let add_constant ~size dom xa xb x_var ((ya, yb, y_var) as opt) =
      (* We want to find [f1], [f2] and [offset] such that:
        [f1*x + f2*y = offset]
        We know the values of [x] and [y] at points [a] and [b], so
        we have two equations:
        (1) [f1*xa + f2*ya = offset]
        (2) [f1*xb + f2*yb = offset]

        Using the following:
        - [f1 = yb - ya]
        - [f2 = xa - xb]
        - [offset = f1*xa + f2*ya]
        It obviously satisfies (1) by reflexivity,
        (2) requires a bit of unfolding but also works out
      *)
      let f1 = Z.(yb - ya) in
      let f2 = Z.(xa - xb) in
      if Z.(leq (abs f1) (abs f2))
      (* Choose smaller factor as a parent, as it may divide the other one *)
      then opt, C.union dom x_var y_var (Linear.make ~size ~f1 ~f2 Z.(f1*xa + f2*ya))
      else (xa, xb, x_var), C.union dom y_var x_var (Linear.make ~size ~f1:f2 ~f2:f1 Z.(f1*xa + f2*ya))

  (** Adds the new join variable [x_var = phi(xa, xb)]
      If there is another variable such that [y_var = phi(ya,yb)] and there
      exists a relation [r] with [ya --(r)--> xa], [yb --(r)--> xb], then we
      add [y_var --(r)--> x_var].

      This is compared pairwise to all previously encountered phi variables,
      hoping to find a match *)
  let rec add_non_constant ~size dom xa xb x_var list = match list with
    | [] -> [xa, xb, x_var], dom
    | ((ya, yb, y_var) as y)::next ->
        match Terms.UnionFind.check_related xa ya with
        | None ->
              let ys, dom = add_non_constant ~size dom xa xb x_var next in
              y::ys, dom
        | Some rel_a -> match Terms.UnionFind.check_related xb yb with
            | Some rel_b when Linear.equal rel_a rel_b ->
                (* Choose smaller factor as a parent, as it may divide the other one *)
                if Z.(leq (abs (Linear.f1 rel_a)) (abs (Linear.f2 rel_a)))
                then list, C.union dom x_var y_var rel_a
                else (xa,xb,x_var)::next, C.union dom y_var x_var (Linear.inverse rel_a)
            | _ -> let ys, dom = add_non_constant ~size dom xa xb x_var next in
                   y::ys, dom

  type bin_term = Operator.Function_symbol.binary Terms.t

  let hashtabl_update key f map =
    let opt = Hashtbl.find_opt map key in
    let new_value, new_result = f opt in
    Hashtbl.replace map key new_value;
    new_result

  (** This function should be called whenever phi variables are created.
      It maintain union-find across the phi function:
      - For pairs of constants [(x,y) = phi((1,2), (3,5))], it adds the relation [y=2x+1]
      - For variables that are already related [(x,y) = phi((xl,yl), (xr,yr))] when
        [xl --(r)--> yl] and [xr --(r)--> yr], it adds [x --(r)--> y].

      [args = phi(args_left, args_right)]. *)
  let add_phi_unions ~dom_left ~dom_right ~dom ~args_left ~args_right ~args =
    (* We use Hashtables to avoid comparing binary terms of different sizes.
      - One table if for constant terms (both join predecessors are constants)
      - The other is for non constant terms, which may still be related. *)
    let constants : (In_bits.t, (Z.t * Z.t * bin_term)) Hashtbl.t = Hashtbl.create 3 in
    let non_constants : (In_bits.t, (bin_term*bin_term*bin_term) list) Hashtbl.t = Hashtbl.create 3 in
    Immutable_array.fold_left3 (fun dom (Terms.Any a) (Terms.Any b) (Terms.Any res) ->
      match a, b, res with
        | Terms.Binary {size}, Terms.Binary {size=sz}, Terms.Binary {size=sz1} ->
            assert (size=sz && size=sz1);
            begin match
              is_constant ~syntaxic_only:false ~size dom_left a,
              is_constant ~syntaxic_only:false ~size dom_right b
            with
              | Some a', Some b' when not Z.(equal (b' - a') zero) ->
                  hashtabl_update size (function
                    | None -> (a',b',res), dom
                    | Some l -> let l, dom = add_constant ~size dom a' b' res l in l, dom
                  ) constants
              | _, _ ->
                hashtabl_update size (function
                    | None -> [a,b,res], dom
                    | Some l -> let l, dom = add_non_constant ~size dom a b res l in l, dom
                  ) non_constants
              end
          | _ -> dom
        ) dom args_left args_right args

  let nondet ~doma ~tupa ~domb ~tupb ~tupres =
    let dom = nondet ~doma ~tupa ~domb ~tupb ~tupres in
    add_phi_unions
      ~dom_left:doma ~dom_right:domb ~dom
      ~args_left:tupa ~args_right:tupb ~args:tupres

  let widened_fixpoint_step ~previous ~previous_tup ~next ~next_tup b ~res_tup =
    let dom, b = widened_fixpoint_step ~previous ~previous_tup ~next ~next_tup b ~res_tup in
    add_phi_unions
      ~dom_left:previous ~dom_right:next ~dom
      ~args_left:previous_tup ~args_right:next_tup ~args:res_tup, b

  let fixpoint_step ~lvl ~iteration dom_actual ~actuals dom_args ~args dom_finals ~finals =
    let b, f = fixpoint_step ~lvl ~iteration dom_actual ~actuals dom_args ~args dom_finals ~finals in
    b, (fun ~close res ->
      let dom = f ~close res in
      add_phi_unions
        ~dom_left:dom_args ~dom_right:dom_finals ~dom
        ~args_left:args ~args_right:finals ~args:res)
end
