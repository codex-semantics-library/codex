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

module Tbl = Virtual_address.Htbl
module StrMap = Codex.Datatype_sig.StringMap
module Log = Codex_logger
let () =
  Log.channel_set_color true Log.error_channel

module Ctypes = Codex.Types.Ctypes

(* This module is already defined in Stdlib for versions >= 4.08.0. *)
module Result = struct
  let (>>) r f =
    match r with
    | Ok _ -> f ()
    | Error _ as e -> e

  let iter_error f = function
  | Ok _ -> ()
  | Error e -> f e
end

module Path : sig
  type t
  val atom : string -> t
  val field : t -> string -> t
  val array_elem : t -> int -> t
  val deref : t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Atom of string | PField of t * string | PElem of t * int | PDeref of t
  let atom s = Atom s
  let field p f = PField (p, f)
  let array_elem p i = PElem (p, i)
  let deref p = PDeref p
  let rec pp_aux prec fmt =
    let open Format in function
    | Atom s -> pp_print_string fmt s
    | PField (PDeref p, f) ->
        if prec > 2 then fprintf fmt "(%a->@,%s)" (pp_aux 2) p f
        else fprintf fmt "%a->@,%s" (pp_aux 2) p f
    | PField (p, f) ->
        if prec > 2 then fprintf fmt "(%a.@,%s)" (pp_aux 2) p f
        else fprintf fmt "%a.@,%s" (pp_aux 2) p f
    | PElem (p, i) ->
        if prec > 3 then fprintf fmt "(%a[%d])" (pp_aux 3) p i
        else fprintf fmt "%a[%d]" (pp_aux 3) p i
    | PDeref p ->
        if prec > 1 then fprintf fmt "(*%a)" (pp_aux 1) p
        else fprintf fmt "*%a" (pp_aux 1) p
  let pp fmt = pp_aux 0 fmt
end

type cell_typ = Ctypes.typ * int

let ptr_size = 4

let pp_cell_typ fmt (t,i) =
  Format.fprintf fmt "(%a,@,%d)" Ctypes.pp t i

exception Incomparable_types of string
exception Falsified_predicate of string
exception Type_error of string

let print_exc (f : ('a, Format.formatter, unit) format -> 'a) = function
  | Incomparable_types msg ->
      f "Incomparable_types:@ %s" msg
  | Falsified_predicate msg ->
      f "Falsified_predicate:@ %s" msg
  | Type_error msg ->
      f "Type_error:@ %s" msg
  | _ -> assert false

let print_exc e =
  print_exc (fun pp -> Log.alarm "heap_typing"; Log.error pp) e

let iter_error res =
  Log.check "heap_typing";
  Result.iter_error print_exc res

let eval_value syms =
  let open Ctypes in function
  | Const x -> x
  | Sym s ->
      (try StrMap.find s syms with Not_found -> raise @@ Pred.Undefined_symbol s)

(** Returns the type at a given offset of a non-scalar type.
   @param binary zero a [BR.binary] of value zero and of size
   [Type.offset_size].
   @return a triple (subtyp, idx, ofs) such that [offset] is in [subtyp] at
   index [idx] (if [subtyp] is an array) and offset [ofs].
*)
(* TODO: This is a duplicate of the function in type_domain, which should actually go in ctypes.  *)
let type_at_offset symbols typ offset : cell_typ =
  (*
  Log.result "type_at_offset.@ typ =@ %a.@ offset =@ %d"
    Ctypes.pp_descr typ
    offset;
  *)
  let open Ctypes in
  match (inlined typ).descr with
  | Structure st ->
    begin
      let members =
        List.sort_uniq (fun (x,_,_) (y,_,_) -> Stdlib.compare x y) st.st_members in
      let struct_size =
        match st.st_byte_size with Some s -> s | None -> assert false in
      let rec loop cur_field = function
        | (ofs,_,_) as field :: fields when offset >= ofs -> loop field fields
        | _ ->
            let (prec_ofs,_fname,ftyp) = cur_field in
            if offset >= struct_size then begin
              let msg = Format.asprintf "Offset %d not found in structure %a."
                offset Ctypes.pp typ in
              raise (Type_error msg)
            end;
            (*
            Logger.result "Access to field %s : %a (offset %d) of struct %a"
              fname
              Ctypes.pp ftyp
              prec_ofs
              Ctypes.pp_descr typ;
            *)
            ftyp, offset - prec_ofs
      in
      let fst_field = try List.hd members with Failure _ ->
        raise (Invalid_argument "type_at_offset: empty structure") in
      loop fst_field (List.tl members)
    end
  | Array (elem_typ, Some l) ->
      let nb_elem = eval_value symbols l in
      let elem_sz = Ctypes.sizeof elem_typ in
      let array_index = offset / elem_sz in
      (* Already emitted in Dba2Codex *)
      (*Logger.check "array_offset_access";*)
      if not (array_index >= 0 && Z.lt (Z.of_int array_index) nb_elem) then begin
        let msg = Format.asprintf "@[<hov 2>Out-of-bounds access at index %d in array of \
          size %a.@]" array_index Z.pp_print nb_elem in
        raise (Type_error msg)
      end;
      let offset_in_elem = offset mod elem_sz in
      elem_typ,
      offset_in_elem
  | _ when offset = 0 ->
      (* This is a scalar type and does not have any fields. *)
      raise Exit
  | _ ->
      let msg = Format.asprintf "@[<hov 2>Access in type %a \
        with non-zero offset %d is not allowed@]" Ctypes.pp typ offset in
      raise (Type_error msg)

let subtype symbols ((t,i) : cell_typ) ((u,j) : cell_typ) =
  (*Log.result "subtype@ %a@ %a" pp_cell_typ (t,i) pp_cell_typ (u,j);*)
  let open Ctypes in
  if Ctypes.equiv ~only_descr:false t u && i = j then
    u.pred = Pred.True || t.pred = u.pred
  else
    if not (u.pred = Pred.True || t.pred = u.pred) then false
    else
      let rec parents x ofs =
        try
          let parent_t, parent_ofs = type_at_offset symbols x ofs in
          (x,ofs) :: parents parent_t parent_ofs
        with
        | Exit -> [(x,ofs)]
        | Type_error _ ->
            (* If the access is ill-typed, then the next parent is "top", and
             * therefore our ascending chain stops here. *)
            [(x,ofs)]
      in
      let parents_t, parents_u = parents t i, parents u j in
      (* Tells whether [l2] is a prefix of [l1]. *)
      let rec walk l1 l2 = match l1,l2 with
      | [],[] ->
          (* This should only happen for equal cell types, handled above in
           * this function *)
          assert false
      | [],_ -> false
      | _,[] -> true
      | (x,ox) :: r1, (y,oy) :: r2 ->
          if Ctypes.equiv ~only_descr:false x y && ((=):int->int->bool) ox oy
          then walk r1 r2
          else false
      in
      walk (List.rev parents_t) (List.rev parents_u)

module type MEMORY = sig
  type t
  val read_u8 : t -> Virtual_address.t -> Loader_types.u8
  val read_u32 : t -> Virtual_address.t -> Loader_types.u32
  val read_u64 : t -> Virtual_address.t -> Loader_types.u64
end

module Make (Memory : MEMORY) = struct

  let array_size symbols elem_t length =
    (* Will raise [Overflow] if result does not fit on 32 bits. *)
    Int32.to_int @@ Z.to_int32 @@
    Z.mul (eval_value symbols length) (Z.of_int @@ Ctypes.sizeof elem_t)

  (* Assign a typ or not to a one-byte cell, depending on the possible types
   * already assigned to it. *)
  let type_cell a (typ : cell_typ) (typing, symbols, to_check) =
    begin match Tbl.find typing a with
    | typ' when subtype symbols typ' typ ->
        (* The registered type for this address is compatible and more precise.
         * This should have been detected earlier. *)
        assert false
    | typ' when subtype symbols typ typ' ->
        (* The registered type for this address is strictly less precise:
          * erase the previous type. *)
        (*Log.result "Assigning %a the type %a" Virtual_address.pp a pp_cell_typ typ;*)
        Tbl.replace typing a typ;
    | u ->
        (* Address would have two incomparable types. This is an error. *)
        let msg = Format.asprintf "incomparable types@ at address@ %a:@ %a and %a"
          Virtual_address.pp a pp_cell_typ typ pp_cell_typ u in
        raise @@ Incomparable_types msg
    | exception Not_found ->
        (* Address not visited yet. Register [t] as its type. *)
        (*Log.result "Assigning %a the type %a" Virtual_address.pp a pp_cell_typ typ;*)
        Tbl.add typing a typ;
    end;
    match Tbl.find to_check a, typ with
    | t, (u,_) when Ctypes.equiv ~only_descr:false t u ->
        Tbl.remove to_check a
    | _ -> ()
    | exception Not_found -> ()

  let type_region start_addr typ size state =
    try
      for i = 0 to size-1 do
        type_cell (Virtual_address.add_int i start_addr) (typ,i) state
      done
    with Incomparable_types msg ->
      Log.error "%s" msg

  (* Check that a 4 KiB memory page may does not intersect the kernel address space. *)
  let check_4k_mem_page start_addr =
    Log.result "check_4k_mem_page@ %a" Virtual_address.pp start_addr;
    let c1 =
      Virtual_address.compare (Virtual_address.create 0x12000000) start_addr < 0 in
    let c2 =
      Virtual_address.compare (Virtual_address.add_int (4096*4) start_addr)
      (Virtual_address.create 0x12031000) > 0 in
    if c1 || c2 then
      Log.error "Memory page of size 4 KiB at %a intersects with forbidden space"
        Virtual_address.pp start_addr

  let type_heap ~symbols (mem : Memory.t)
      (init_addr : Virtual_address.t) (init_type : Ctypes.typ) =
    let open Ctypes in
    let typing : cell_typ Tbl.t = Tbl.create 4000 in
    let to_check : Ctypes.typ Tbl.t = Tbl.create 10 in
    let state = typing, symbols, to_check in
    (* Checking and propagating a type for a memory region:
       1. assigning the appropriate types to bytes in the region, if possible,
          otherwise it's an error
       2. check that values in the region satisfy all the predicates of the type
       3. recurse on pointers.
       The order of steps 1 and 2 could be changed, but this one seems more
       efficient in case of error.
    *)
    let rec check_region path start_addr typ =
      if typ.pred = Pred.True then Ok () else
      match typ.descr with
      | Void | Function _ -> assert false
      | Base (size,_) ->
          let value =
            if size = 1 then Memory.read_u8 mem start_addr
            else if size = 4 then Memory.read_u32 mem start_addr
            (* FIXME: this is wrong because {Loader_types.u64} = {int} *)
            else if size = 8 then Memory.read_u64 mem start_addr
            else assert false
          in
          if not @@ Pred.eval ~symbols ~self:(Z.of_int value) typ.pred then
            let msg = Format.asprintf "Path@ %a:@ value@ %x@ at@ %a@ (size %d)@ falsifies@ %a"
              Path.pp path value Virtual_address.pp start_addr size Pred.pp typ.pred in
            Error (Falsified_predicate msg)
          else Ok ()
      | Enum {en_byte_size=size; en_values; _} ->
          let value =
            if size = 1 then Memory.read_u8 mem start_addr
            else if size = 4 then Memory.read_u32 mem start_addr
            else assert false
          in
          if not @@ Pred.eval ~symbols typ.pred ~self:(Z.of_int value) then
            let msg = Format.asprintf "Path@ %a:@ value@ %x@ at %a@ (size@ %d)@ falsifies@ %a"
              Path.pp path value Virtual_address.pp start_addr size Pred.pp typ.pred in
            Error (Falsified_predicate msg)
          else if not @@ List.exists (fun (_,v) -> value = v) en_values then
            let msg = Format.asprintf "Path@ %a:@ value@ %x@ at %a@ (size@ %d)@ not in enum@ %a"
              Path.pp path value Virtual_address.pp start_addr size Ctypes.pp_descr typ.descr in
            Error (Falsified_predicate msg)
          else
            Ok ()
      | Ptr _ ->
          let value = Memory.read_u32 mem start_addr in
          if not @@ Pred.eval ~symbols typ.pred ~self:(Z.of_int value) then
            let msg = Format.asprintf "Path@ %a:@ pointer@ %x@ at@ %a@ falsifies@ %a"
              Path.pp path value Virtual_address.pp start_addr Pred.pp typ.pred in
            Error (Falsified_predicate msg)
          else Ok ()
      | Array (_,None) ->
          let msg = Format.asprintf "Path@ %a:@ Cannot@ check@ predicates@ on array@ of unknown \
            size@ %a@ at address@ %a" Path.pp path pp typ Virtual_address.pp start_addr in
          Error (Falsified_predicate msg)
      | Array (elem_t, Some length) ->
          let nb_elem = Int32.to_int @@ Z.to_int32 @@ eval_value symbols length in
          let elem_sz = Ctypes.sizeof elem_t in
          let rec loop = function
          | i when i >= nb_elem -> Ok ()
          | i ->
              let open Result in
              check_region (Path.array_elem path i)
                (Virtual_address.add_int (elem_sz*i) start_addr) elem_t >> fun () ->
              loop (i+1)
          in
          loop 0
      | Structure {st_members; _} ->
          let rec loop = function
          | [] -> Ok ()
          | (offset,fname,ftyp) :: tl ->
              let open Result in
              check_region (Path.field path fname) (Virtual_address.add_int offset start_addr)
                  ftyp >> fun () ->
              loop tl
          in
          loop st_members

      | Name name -> failwith ("Use of a non-inlined named type " ^ name)
      | Application _ -> failwith ("Use of a non-applied constructor type")
      | Existential _ -> failwith ("Use of a non-instantiated existential type")
      | Union _ -> failwith ("Use of an ambiguous union type")
      | Weak _ -> failwith ("Use of an ambiguous weak type")

    and propagate_type path ~mutate (typ : Ctypes.typ) a =
      match typ.descr with
      | Void | Function _ ->
          (* Seems safer to mark this as an unreachable case and treat "pointers
           * to void" and "pointer to function" as a special case. *)
          assert false
      | Base (size,_) | Enum {en_byte_size = size; _} ->
          if mutate then type_region a typ size state;
          check_region path a typ |> iter_error
      | Ptr{pointed = pointed_t; index = Zero} ->
          if mutate then type_region a typ ptr_size state;
          check_region path a typ |> iter_error;
          begin match pointed_t.descr with
          | Void ->
              (* We need only check that the pointer is valid. *)
              let v = Memory.read_u32 mem a in
              if v <> 0 then begin
                try ignore @@ Memory.read_u8 mem @@ Virtual_address.create v
                with Invalid_argument _ ->
                  Log.error "Path@ %a:@ Pointer of type void* is invalid." Path.pp path
              end
          | Function _ ->
              if Memory.read_u32 mem a <> 0 then
                Log.error "Path@ %a:@ Non-null function pointer: not checking address." Path.pp path
          | Base _ | Enum _ | Structure _ | Array _ | Ptr _ ->
              let v = Memory.read_u32 mem a in
              if v <> 0 then
                propagate_guarded (Path.deref path) ~mutate:true pointed_t (Virtual_address.create v)

          | Name name -> failwith ("Use of a non-inlined named type " ^ name)
          | Application _ -> failwith ("Use of a non-applied constructor type")
          | Existential _ -> failwith ("Use of a non-instantiated existential type")
          | Union _ -> failwith ("Use of an ambiguous union type")
          | Weak _ -> failwith ("Use of an ambiguous weak type")

          end
      | Ptr{pointed = {descr=Array (elem_t, Some ar_sz);_} as pointed_t; index = PastEnd} ->
          if mutate then type_region a typ ptr_size state;
          let size = array_size symbols elem_t ar_sz in
          check_region path a typ |> iter_error;
          let v = Memory.read_u32 mem a in
          if v <> 0 then
            propagate_guarded (Path.deref path) ~mutate:true pointed_t @@ Virtual_address.create @@ v - size
      | Ptr{pointed = {descr=Array (_, Some _);_} as pointed; index = ValidUnknown} ->
          if mutate then type_region a typ ptr_size state;
          check_region path a typ |> iter_error;
          (* Cannot do propagation of the array, but we shouldn't have to (if
            * the heap is well-typed, there is a pointer to the beginning of
            * the array somewhere. So we will just check make sure that the
            * pointed cell is typed as a [(pointed, n)], either now, or later. *)
          let v = Virtual_address.create @@ Memory.read_u32 mem a in
          begin match Tbl.find typing v with
          | (t,_) when Ctypes.equiv ~only_descr:false t pointed ->
              ()
          | _ ->
              Tbl.add to_check v pointed
          end
      | Ptr{pointed = {descr=Array (_,None);_}; index = (PastEnd | ValidUnknown)} ->
          assert false
      | Ptr{pointed = {descr=_;_}; index = (PastEnd | ValidUnknown)} ->
          assert false

      | Ptr{pointed = _; index = Idx _} -> assert false 
        
      | Array (_, None) ->
          Log.error "Path@ %a:@ Array@ %a@ of unknown size@ starts at@ %a.@ Not verifying or@ \
            propagating anything." Path.pp path pp typ Virtual_address.pp a
      | Array (elem_t, Some sz) ->
          let nb_elem = Int32.to_int @@ Z.to_int32 @@ eval_value symbols sz in
          let elem_sz = Ctypes.sizeof elem_t in
          let ar_size = array_size symbols elem_t sz in
          if mutate then type_region a typ ar_size state;
          check_region path a typ |> iter_error;
          for i = 0 to nb_elem-1 do
            propagate_type (Path.array_elem path i) ~mutate:false elem_t (Virtual_address.add_int (i*elem_sz) a)
          done
      | Structure {st_byte_size=Some st_sz; st_members; _} ->
          if mutate then type_region a typ st_sz state;
          check_region path a typ |> iter_error;
          st_members |> List.iter (fun (offset,fname,ftyp) ->
              propagate_type (Path.field path fname) ~mutate:false ftyp (Virtual_address.add_int offset a)
            )
      | Structure {st_byte_size=None;_} -> assert false

      | Name name -> failwith ("Use of a non-inlined named type " ^ name)
      | Application _ -> failwith ("Use of a non-applied constructor type")
      | Existential _ -> failwith ("Use of a non-instantiated existential type")
      | Union _ -> failwith ("Use of an ambiguous union type")
      | Weak _ -> failwith ("Use of an ambiguous weak type")

    and propagate_guarded path ~mutate typ a =
      Log.result "propagate_guarded@ %a@ ~mutate:%B@ %a@ %a" Path.pp path
        mutate Ctypes.pp typ Virtual_address.pp a;
      match Tbl.find typing a with
      | c_typ ->
          if subtype symbols c_typ (typ,0) then
            Log.result "Path@ %a:@ Typing@ %a is@ more@ precise,@ not@ propagating."
              Path.pp path pp_cell_typ c_typ
          else propagate_type path ~mutate typ a
      | exception Not_found -> propagate_type path ~mutate typ a
    in
    propagate_type (Path.atom "runtime") ~mutate:true init_type init_addr;
    to_check |> Tbl.iter (fun addr typ ->
      Log.error "unchecked@ address@ %a@ remained@ with@ type@ %a and unknown offset"
        Virtual_address.pp addr Ctypes.pp typ);
    if Tbl.length to_check = 0 then
      Log.result "No address left to check.";
    typing

end
