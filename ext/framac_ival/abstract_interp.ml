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

exception Error_Top
exception Error_Bottom
exception Not_less_than
exception Can_not_subdiv

(* let msg_emitter = Lattice_messages.register "Abstract_interp" *)

(* open Lattice_type *)
type truth = True | False | Unknown

let inv_truth = function
  | Unknown -> Unknown
  | True -> False
  | False -> True

module Comp = struct
  type t = Lt | Gt | Le | Ge | Eq | Ne

  type result = truth = True | False | Unknown
  
  let inv = function
    | Gt -> Le
    | Lt -> Ge
    | Le -> Gt
    | Ge -> Lt
    | Eq -> Ne
    | Ne -> Eq

  let sym = function
    | Gt -> Lt
    | Lt -> Gt
    | Le -> Ge
    | Ge -> Le
    | Eq -> Eq
    | Ne -> Ne

  let pretty_comp fmt = function
    | Gt -> Format.pp_print_string fmt ">"
    | Lt -> Format.pp_print_string fmt "<"
    | Le -> Format.pp_print_string fmt "<="
    | Ge -> Format.pp_print_string fmt ">="
    | Eq -> Format.pp_print_string fmt "=="
    | Ne -> Format.pp_print_string fmt "!="

  let inv_result = function
    | Unknown -> Unknown
    | True -> False
    | False -> True

end

module Int = struct
  include (Integer: module type of Integer with type t = Integer.t)

  module Set = Set.Make(Integer)
  (* include (Datatype.Integer: Datatype.S_with_collections with type t:=Integer.t) *)

  let ten_thousands = Integer.of_int 10000;;
  let minus_ten_thousands = Integer.of_int @@ -10000;;  
  let pretty fmt v =
    (* If we can print a 4-digit decimal number. *)    
    if Integer.ge v ten_thousands 
     || Integer.le v minus_ten_thousands
    then Format.fprintf fmt "%a" (Integer.pretty ~hexa:true) v
    else Format.fprintf fmt "%s" @@ Z.to_string v
  (* let pretty fmt v =
   *   if not (Kernel.BigIntsHex.is_default ()) then
   *     let max = of_int (Kernel.BigIntsHex.get ()) in
   *     if gt (abs v) max then Integer.pretty ~hexa:true fmt v
   *     else Integer.pretty ~hexa:false fmt v
   *   else
   *     Integer.pretty ~hexa:false fmt v *)

  (** execute [f] on [inf], [inf + step], ... *)
  let fold f ~inf ~sup ~step acc =
(*    Format.printf "Int.fold: inf:%a sup:%a step:%a@\n"
       pretty inf pretty sup pretty step; *)
    let nb_loop = e_div (sub sup inf) step in
    let rec fold_incr ~counter ~inf acc =
      if equal counter onethousand then
        Codex_log.performance_warning
          "enumerating %a integers" pretty nb_loop;
      if le inf sup then begin
          (*          Format.printf "Int.fold: %a@\n" pretty inf; *)
        fold_incr ~counter:(succ counter) ~inf:(add step inf) (f inf acc)
      end else acc
    in
    let rec fold_decr ~counter ~sup acc =
      if equal counter onethousand then
        Codex_log.performance_warning
          "enumerating %a integers" pretty nb_loop;
      if le inf sup then begin
        (*          Format.printf "Int.fold: %a@\n" pretty inf; *)
        fold_decr ~counter:(succ counter) ~sup:(add step sup) (f sup acc)
      end else acc
    in
    if le zero step
    then fold_incr ~counter:zero ~inf acc
    else fold_decr ~counter:zero ~sup acc

end

(* Typing constraints are enforced directly in the .mli *)
module Rel = struct
  include Int

  let check ~rem ~modu =
    zero <= rem && rem < modu

  let add_abs = add
  let sub_abs = sub
end


module Bool = struct
  type t = Top | True | False | Bottom
  let hash (b : t) = Hashtbl.hash b
  let equal (b1 : t) (b2 : t) = b1 = b2
  let compare (b1 : t) (b2 : t) = Stdlib.compare b1 b2
  let pretty fmt = function
    | Top -> Format.fprintf fmt "Top"
    | True -> Format.fprintf fmt "True"
    | False -> Format.fprintf fmt "False"
    | Bottom -> Format.fprintf fmt "Bottom"
  let is_included t1 t2 = match t1, t2 with
    | Bottom, _
    | _, Top
    | True, True
    | False, False -> true
    | _ -> false
  let bottom = Bottom
  let top = Top
  let join b1 b2 = match b1, b2 with
    | Top, _
    | _, Top
    | True, False -> Top
    | True, _
    | _, True -> True
    | False, _
    | _, False -> False
    | Bottom, Bottom -> Bottom
  let narrow b1 b2 = match b1, b2 with
    | Bottom, _
    | _, Bottom
    | True, False -> Bottom
    | True, _
    | _, True -> True
    | False, _
    | _, False -> False
    | Top, Top -> Top
  let link = join
  let meet = narrow
  type widen_hint = unit
  let widen () = join
  let cardinal_zero_or_one b = not (equal b top)
  let intersects b1 b2 = match b1, b2 with
    | Bottom, _ | _, Bottom -> false
    | _, Top | Top, _ -> true
    | False, False | True, True -> true
    | False, True | True, False -> false
  let diff b1 b2 = match b1, b2 with
    | b1, Bottom -> b1
    | _, Top -> Bottom
    | Bottom, _ -> Bottom
    | Top, True -> False
    | Top, False -> True
    | True, True -> Bottom
    | True, False -> True
    | False, True -> False
    | False, False -> Bottom
  let diff_if_one b1 b2 = match b1, b2 with
    | b1, Top -> b1
    | _, _ -> diff b1 b2
  let fold_enum f b init =
    let elements = match b with
      | Top -> [True; False]
      | True -> [True]
      | False -> [False]
      | Bottom -> []
    in
    List.fold_right (fun b acc -> f b acc) elements init
  let cardinal = function
    | Top -> 2
    | True | False -> 1
    | Bottom -> 0
  let cardinal_less_than b i =
    let c = cardinal b in
    if c > i then raise Not_less_than
    else c

  type blt = t
  (* include (Datatype.Make_with_collections *)
  (*            (struct *)
  (*              type t = blt *)
  (*              let name = "Bool_lattice" *)
  (*              let structural_descr = Structural_descr.t_abstract *)
  (*              let reprs = [Top; True; False; Bottom] *)
  (*              let equal = equal *)
  (*              let compare = compare *)
  (*              let hash = hash *)
  (*              let rehash = Datatype.identity *)
  (*              let copy = Datatype.identity *)
  (*              let pretty = pretty *)
  (*              let internal_pretty_code = Datatype.undefined *)
  (*              let varname = Datatype.undefined *)
  (*              let mem_project = Datatype.never_any_project *)
  (*            end) : *)
  (*            Datatype.S with type t := t) *)
end
