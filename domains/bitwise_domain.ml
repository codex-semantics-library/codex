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


(* One issue with my tests is that whenever I get through the memory,
   it does not work.  So, the bitwise domain should be beneath
   memory_domain, but above the pointers.

   This raises the questions: why these operable values are not true
   domains?

   Answer: they should. Region separation should just be 2 functor
   domains, one for the binary, one for the memory.

   Or at least, memory_domain should be split into two domains, two
   allow intermediate domains like this one. *)
let pretty_detailed = true

module Quadrivalent = Lattices.Quadrivalent

module type Subdomain = Domain_sig.Base

module Make (Sub : Subdomain) = struct

  let name() = "Bitwise(" ^ (Sub.name()) ^ ")";;
  let unique_id() = Domain_sig.Fresh_id.fresh @@ name();;

  (* To represent values, we could use an array where each bit would
     be a bit in another variable, but this would be slow for most
     operation. So, we use a list of "parts" that group these bits,
     and the information is complemented with the known 0 bits and
     known 1 bits informat that we query from Sub. *)
  type binary_part =
    (* We call this extract even when there is no extraction, in which case index == 0 and size == oldsize. *)
    | Extract of { size:int;
                   index:int;
                   oldsize:int;
                   value:Sub.binary }
  ;;

  module Part = struct
    let pretty ctx fmt (Extract{size;index;oldsize;value}) =
      if index == 0 && size == oldsize
      then Format.fprintf fmt "(%a)<%d>" (Sub.binary_pretty ~size ctx) value size
      else Format.fprintf fmt "(%a)[%d..+%d]" (Sub.binary_pretty ~size ctx) value index size
    ;;

    let zero ctx ~size = Extract {size;index=0;oldsize=size;
                                  value=Sub.Binary_Forward.biconst ~size Z.zero ctx}

    let ones ctx ~size =
      Extract {size;index=0;oldsize=size;
               value = Sub.Binary_Forward.biconst ~size (Z.extract Z.minus_one 0 size) ctx }

    let do_extract ctx = function
      | Extract{size;index;oldsize;value} ->
        if size = oldsize then value
        else Sub.Binary_Forward.bextract ~size ~index ~oldsize ctx value
    ;;

    let to_singleton ctx = function
      | Extract{size;index;oldsize;value} ->
        let bin = Sub.Binary_Forward.bextract ~size ~index ~oldsize ctx value in
        Sub.Query.binary ~size ctx bin |> Sub.Query.binary_is_singleton ~size
    ;;

    let from_value ~size value = Extract{size;index=0;oldsize=size;value};;

  end

  module Parts = struct

    (* A binary is represented as a list where head is the least significant binary_part
       (this allows for efficient additions, etc). *)
    type t = binary_part list;;

    (* Remove the first ~index bits from the start. *)
    let cut_start ~index p =
      (* Invariant: i <= index. *)
      let rec loop i = function
        | [] -> assert false     (* Index is larger than the size. *)
        | Extract{size}::rest when i + size <= index -> loop (i + size) rest
        | (Extract{size=sizee;index=indexe;oldsize;value}::rest) as l ->
          if i == index then l
          else
            let removed_size = index - i in
            Extract{size=sizee-removed_size;index=indexe+removed_size;oldsize;value}::rest
      in loop 0 p
    ;;

    (* Cut the part so that its size becomes size, which must be smaller than the original size. *)
    let cut_end ~size p =
      let rec loop i = function
        (* | [] when i == size -> [] *)
        | [] -> assert false     (* The parts is too small, cannot cut. *)
        | (Extract{size=sizee} as hd)::tl when i + sizee < size -> hd::(loop (i + sizee) tl)
        | (Extract{size=sizee} as hd)::_ when i + sizee = size -> [hd]
        | Extract{size=sizee;index;oldsize;value}::_ ->
          let removed_size = i + sizee - size in
          [Extract{size=sizee-removed_size;index;oldsize;value}]
      in loop 0 p
    ;;

    let extract ~index ~size ~oldsize p =
      (* cut the first bits up to index. *)
      let p =
        if index == 0 then p
        else cut_start ~index p
      in
      (* cut the size firts bits of p. *)
      let p =
        if index + size == oldsize then p
        else cut_end ~size p
      in
      p
    ;;

    let append a b = List.append a b;;

    let append ctx a b =
      match b with
      | [] -> assert false
      | hdb::tlb -> begin
          let rec loop = function
            | [] -> b
            | [lasta] -> begin
                (*Codex_log.feedback "lasta = %a" (Part.pretty ctx) lasta;
                  Codex_log.feedback "hdb = %a" (Part.pretty ctx) hdb;*)
                let Extract lasta' = lasta in
                let Extract hdb' = hdb in
                (*let b1 = lasta'.index + lasta'.size == hdb'.index in
                  let b2 = Sub.Binary.equal lasta'.value hdb'.value in
                  Codex_log.feedback "b1 = %B" b1;
                  Codex_log.feedback "b2 = %B" b2;*)
                match lasta,hdb with
                | Extract{size=sizea;index=indexa;oldsize=oldsizea;value=valuea},
                  Extract{size=sizeb;index=indexb;oldsize=oldsizeb;value=valueb}
                  when indexa + sizea == indexb && Sub.Binary.equal valuea valueb ->
                  (* Stitch together contiguous extractions. *)
                  assert(oldsizea == oldsizeb);
                  Extract{size=sizea+sizeb;index=indexa;oldsize=oldsizea;value=valuea}::tlb
                | Extract{size=sizea},Extract{size=sizeb} -> begin
                    match Part.to_singleton ctx lasta,Part.to_singleton ctx hdb with
                    | Some(csta),Some(cstb) ->
                      let size = sizea + sizeb in
                      let zvalue = Transfer_functions.Concrete.Bitvector_Interp.bconcat ~size1:sizeb ~size2:sizea cstb csta in
                      let value = Sub.Binary_Forward.biconst ~size zvalue ctx in
                      (Part.from_value ~size value)::tlb
                    | _ -> lasta::hdb::tlb
                  end
              end
            | hd::tl -> hd::(loop tl)
          in loop a
        end
    ;;

    (* Preprend (i.e. put in least significant positions) size bits of value 0.  *)
    let prepend_zero ~size ctx p = append ctx [(Part.zero ~size ctx)] p;;

    (* Append (i.e. put in most significant positions) size bits of value 0.  *)
    let append_zero ~size ctx p = append ctx p [(Part.zero ~size ctx)];;

    let chop_last l =
      let rec loop acc = function
        | [] -> assert false
        | [x] -> List.rev acc, x
        | hd :: tl -> loop (hd :: acc) tl
      in loop [] l

    (* Append (i.e. put in most significant positions) size times the most
     * significant bit. If the most significant bit is unknown, use the
     * subdomain to extend the last part. *)
    let signed_extend ~size ~oldsize ctx p =
      let beginning, Extract{size=s_last;value;_} = chop_last p in
      let zeroes, ones = Sub.Query.(binary ~size:s_last ctx value |> binary_to_known_bits ~size:s_last) in
      if not (Z.testbit zeroes (s_last-1)) then
        append_zero ~size:(size - oldsize) ctx p
      else if Z.testbit ones (s_last-1) then
        append ctx p [Part.ones ~size:(size - oldsize) ctx]
      else
        let s = size - oldsize + s_last in
        append ctx beginning [Part.from_value ~size:s (Sub.Binary_Forward.bsext ~size:s ~oldsize:s_last ctx value)]

    let fold2 ctx f acc pa pb =
      let rec loop acc pa pb = match pa,pb with
        | [],[] -> acc
        | [], _ | _, [] -> acc (* assert false *) (* Different lengths. *)
        | (Extract{size=sizea;index=indexa;oldsize=oldsizea;value=valuea} as pa)::tla,
          (Extract{size=sizeb;index=indexb;oldsize=oldsizeb;value=valueb} as pb)::tlb ->
          (*Codex_log.feedback "fold2 sizea:%d sizeb:%d oldsizea:%d oldsizeb:%d" sizea sizeb oldsizea oldsizeb;*)
          if(sizea == sizeb) then
            let acc = f ~size:sizea acc pa pb in
            loop acc tla tlb
          else if(sizea < sizeb) then
            let bb = Extract{size=sizea; index=indexb; oldsize=oldsizeb; value=valueb} in
            let acc = f ~size:sizea acc pa bb in
            loop acc tla (Extract{size=sizeb-sizea;index=indexb+sizea;oldsize=oldsizeb;value=valueb}::tlb)
          else
            let ba = Extract{size=sizeb; index=indexa; oldsize=oldsizea; value=valuea} in
            let acc = f ~size:sizeb acc ba pb in
            loop acc (Extract{size=sizea-sizeb;index=indexa+sizeb;oldsize=oldsizea;value=valuea}::tla) tlb
      in loop acc pa pb

    type bits =
      | Zeroes of int (** size. *)
      | Ones of int (** size. *)
      | Unknown of binary_part

    let pretty_bits ctx fmt =
      let open Format in function
        | Zeroes s -> fprintf fmt "00..0<%d>" s
        | Ones s -> fprintf fmt "11..1<%d>" s
        | Unknown p -> Part.pretty ctx fmt p

    let decompose_using_known_bits ~size ctx part =
      let Extract{size;index;oldsize;value} = part in
      let sub = Part.do_extract ctx part in
      (* Identify groups of known bits *)
      let zeroes, ones = Sub.Query.binary_to_known_bits ~size @@ Sub.Query.binary ~size ctx sub in
      let rec loop acc cur i =
        if i >= size then List.rev (cur :: acc)
        else match Z.testbit zeroes i, Z.testbit ones i, cur with
          | false, true, _ -> raise Domain_sig.Bottom
          | false, _, Zeroes s -> loop acc (Zeroes (s+1)) (i+1)
          | false, _, _ -> loop (cur :: acc) (Zeroes 1) (i+1)
          | _, true, Ones s -> loop acc (Ones (s+1)) (i+1)
          | _, true, _ -> loop (cur :: acc) (Ones 1) (i+1)
          | true, false, Unknown(Extract({size=s;_} as e)) -> loop acc (Unknown(Extract{e with size=s+1})) (i+1)
          | true, false, _ -> loop (cur :: acc) (Unknown(Extract{size=1;index=index+i;oldsize;value})) (i+1)
      in
      loop []
        (if not (Z.testbit zeroes 0)
         then Zeroes 1
         else if Z.testbit ones 0
         then Ones 1
         else Unknown (Extract{size=1;index;oldsize;value}))
        1

    let fold2_bits ctx f acc pa pb =
      let split size_fst = function
        | Zeroes size -> Zeroes size_fst, Zeroes (size - size_fst)
        | Ones size -> Ones size_fst, Ones (size - size_fst)
        | Unknown (Extract {size;index;oldsize;value}) ->
          Unknown (Extract{size=size_fst; index; oldsize; value}),
          Unknown (Extract{size=size-size_fst; index=index+size_fst; oldsize; value})
      in
      let rec loop acc pa pb = match pa,pb with
        | [],[] -> acc
        | [],_ | _,[] -> assert false (* Should not happen (invariant of this function) *)
        | (( Unknown(Extract{size=sizea;_})
           | Zeroes sizea
           | Ones sizea) as pa) :: tla,
          (( Unknown(Extract{size=sizeb;_})
           | Zeroes sizeb
           | Ones sizeb) as pb) :: tlb ->
          if sizea = sizeb then
            let acc = f ctx ~size:sizea acc pa pb in
            loop acc tla tlb
          else if sizea < sizeb then
            let fst_pb, rest_pb = split sizea pb in
            let acc = f ctx ~size:sizea acc pa fst_pb in
            loop acc tla (rest_pb :: tlb)
          else
            let fst_pa, rest_pa = split sizeb pa in
            let acc = f ctx ~size:sizeb acc fst_pa pb in
            loop acc (rest_pa :: tla) tlb
      in
      loop acc pa pb

    (* Note: can raise Bottom if it discovers bottom. *)
    let fold2_known_bits ctx f acc pa pb =
      fold2 ctx (fun ~size acc pa pb ->
          let bitsa = decompose_using_known_bits ~size ctx pa in
          let bitsb = decompose_using_known_bits ~size ctx pb in
          fold2_bits ctx f acc bitsa bitsb
        ) acc pa pb

  end

  type binary_ = { complete: Sub.binary;
                   parts: Parts.t
                 }

  module Binary = struct
    type t = binary_;;

    (*  We could check for equality of the parts too. *)
    let equal x y = Sub.Binary.equal x.complete y.complete

    let compare x y = Sub.Binary.compare x.complete y.complete
    let hash _ = assert false

    let pretty pp x =
      Sub.Binary.pretty pp x.complete

    (* Lift a constant. *)
    let lift0 ~size x = { complete = x; parts = [Extract{size;oldsize=size;index=0;value=x}]}
    (* Lifts a (bitwise) function over binaries. *)
    let lift1 ~size f x =
      { complete = f ~size x.complete;
        parts = List.map (function
            | Extract{index;oldsize;size;value} -> Extract{index;oldsize;size;value=f ~size:oldsize value}
            (* | Repeat{repeat;value} -> Repeat{repeat;value=f ~size:1 value} *)
          ) x.parts }

    (* Lift a (non-necessarily bitwise) binary operator. *)
    let lift2 ~size ctx f x y =
      (* If an operand consists in a single part, use it, as it should usually
       * (always?) be more precise than x.complete. *)
      let x = match x.parts with
        | [p] -> Part.do_extract ctx p
        | _ -> x.complete
      in
      let y = match y.parts with
        | [p] -> Part.do_extract ctx p
        | _ -> y.complete
      in
      f ~size ctx x y

    (* TODO: improve this. *)
    let lift2_pred ~size ctx f x y =
      let with_parts = lift2 ~size ctx f x y in
      match Sub.query_boolean ctx with_parts with
      | Quadrivalent.True | Quadrivalent.False | Quadrivalent.Bottom -> with_parts
      | _ -> f ~size ctx x.complete y.complete
    ;;


  end

  module Types = struct
    type binary = Binary.t
    type boolean = Sub.boolean
  end
  include Types

  module Boolean = Sub.Boolean
  module Context = Sub.Context
  open Context

  let root_context = Sub.root_context
  let context_pretty = Sub.context_pretty

  (* include Transfer_functions.Builtin.Make(Types)(Context) *)

  let mu_context_open = Sub.mu_context_open

  let assume = Sub.assume
  let imperative_assume = Sub.imperative_assume
  let imperative_assign_context ctx1 ctx2 = Sub.imperative_assign_context ctx1 ctx2

  let binary_empty ~size ctx = { parts = []; complete = Sub.binary_empty ~size ctx }
  let boolean_empty = Sub.boolean_empty


  module Boolean_Forward = Sub.Boolean_Forward
  (* let [@inline always] ar0 ~size f = fun ctx ->
   *   Binary.of_sub ~size @@ f ctx
   * let [@inline always] ar1 ~size f = fun ctx a ->
   *   Binary.of_sub ~size @@ f ctx @@ Binary.to_sub ~size ctx a
   * let [@inline always] ar2 ~size f = fun ctx a b ->
   *   Binary.of_sub ~size @@ f ctx
   *     (Binary.to_sub ~size ctx a) (Binary.to_sub ~size ctx b)
   * let [@inline always] pred2 ~size f = fun ctx a b ->
   *   f ctx (Binary.to_sub ~size ctx a) (Binary.to_sub ~size ctx b)
   * let [@inline always] ar3 f = fun ctx a b c -> match a,b,c with
   *   | Some a, Some b,  Some c -> Some (f ctx a b c)
   *   | _ -> None
   *
   * let to_ival ~signed ~size ctx x =
   *   Binary.to_sub ~size ctx x |> Sub.Query.binary ~size ctx
   *   |> Sub.Query.binary_to_ival ~signed ~size
   *
   * let to_integer ~signed ~size ctx x =
   *   to_ival ~signed ~size ctx x |> Framac_ival.Ival.project_int *)

  let binary_pretty ~size ctx fmt (x : binary) =
    (* match List.rev x.parts with
    | [] -> assert false
    | hd::tl ->
      Sub.binary_pretty ~size ctx fmt x.complete;
      (*
      Format.pp_print_string fmt "(= [";
      Part.pretty ctx fmt hd;
      List.iter (fun p -> Format.fprintf fmt "::%a" (Part.pretty ctx) p) tl;
      Format.pp_print_string fmt "])";
      *)
    *)
    Sub.binary_pretty ~size ctx fmt x.complete

  module Binary_Forward = struct

    module SBF = Sub.Binary_Forward;;

    let bofbool ~size ctx b =
      SBF.bofbool ~size ctx b |> Binary.lift0 ~size

    let bchoose ~size _ _ = assert false
    let valid ~size _ = assert false

    (* let valid_ptr_arith ~size _ = assert false
     * let bunknown ~size _ = assert false
     * let baddr ~size _  = assert false
     * let biconst ~size _ = assert false
     * let buninit ~size _ = assert false *)

    let bindex ~size _ = assert false

    let valid_ptr_arith ~size arith ctx a b =
      Sub.Binary_Forward.valid_ptr_arith ~size arith ctx a.complete b.complete

    let biconst ~size ctx k = Binary.lift0 ~size @@ SBF.biconst ~size ctx k;;
    let buninit ~size ctx = Binary.lift0 ~size @@ SBF.buninit ~size ctx;;
    let bisle ~size ctx a b = Binary.lift2_pred ~size ctx SBF.bisle a b
    let biule ~size ctx a b = Binary.lift2_pred ~size ctx SBF.biule a b

    let bisub ~size ~nsw ~nuw ~nusw ctx a b = Binary.lift0 ~size @@ SBF.bisub ~size ~nsw ~nuw ~nusw ctx a.complete b.complete
    let bimul ~size ~nsw ~nuw ctx a b = Binary.lift0 ~size @@ SBF.bimul ~size ~nsw ~nuw ctx a.complete b.complete
    let bisdiv ~size ctx a b = Binary.lift0 ~size @@ SBF.bisdiv ~size ctx a.complete b.complete
    let biudiv ~size ctx a b = Binary.lift0 ~size @@ SBF.biudiv ~size ctx a.complete b.complete
    let bismod ~size ctx a b = Binary.lift0 ~size @@ SBF.bismod ~size ctx a.complete b.complete
    let biumod ~size ctx a b = Binary.lift0 ~size @@ SBF.biumod ~size ctx a.complete b.complete

    let bconcat ~size1 ~size2 ctx b1 b2 =
      (*Codex_log.feedback "@[<v 2>BW.bconcat ~size1:%d ~size2:%d@.%a@.%a@]" size1 size2 (binary_pretty ~size:size1 ctx) b1 (binary_pretty ~size:size2 ctx) b2;*)
      let complete = SBF.bconcat ~size1 ~size2 ctx b1.complete b2.complete in
      (* Order of arguments reversed, since b1 should become the most
       * significant part. *)
      let parts = Parts.append ctx b2.parts b1.parts in
      let res = { complete; parts; } in
      (*Codex_log.feedback "result = %a" (binary_pretty ~size:(size1+size2) ctx) res;*)
      res

    let buext ~size ~oldsize ctx x =
      { complete = SBF.buext ~size ~oldsize ctx x.complete;
        (* parts = Parts.append x.parts [Part.zero ~size:(size-oldsize) ctx]; *)
        parts = Parts.append_zero ~size:(size-oldsize) ctx x.parts;
      }
    ;;

    let bsext ~size ~oldsize ctx x =
      { complete = SBF.bsext ~size ~oldsize ctx x.complete;
        parts = Parts.signed_extend ~size ~oldsize ctx x.parts }

    let bextract ~size ~index ~oldsize ctx x =
      (*Codex_log.feedback "bextract ~size:%d ~index:%d ~oldsize:%d %a" size index oldsize (binary_pretty ~size:oldsize ctx) x;*)
      let res =
        { parts = Parts.extract ~index ~size ~oldsize x.parts;
          complete = SBF.bextract ~size ~index ~oldsize ctx x.complete
        }
      in
      (*Codex_log.feedback "result = %a" (binary_pretty ~size:oldsize ctx) res;*)
      res

    let bshl ~size ~nsw ~nuw ctx x y =
      let complete = SBF.bshl ~size ~nsw ~nuw ctx x.complete y.complete in
      match Sub.Query.binary_is_singleton ~size @@ Sub.Query.binary ~size ctx y.complete with
      | None -> Binary.lift0 ~size complete
      | Some y ->
        let parts = Parts.prepend_zero ~size:(Z.to_int y) ctx x.parts in
        let parts = Parts.cut_end ~size parts in
        { complete; parts }
    ;;

    let blshr ~size ctx x y =
      let complete = SBF.blshr ~size ctx x.complete y.complete in
      match Sub.Query.binary_is_singleton ~size @@ Sub.Query.binary ~size ctx y.complete with
      | None -> Binary.lift0 ~size complete
      | Some y ->
        let yi = Z.to_int y in
        let parts = Parts.append_zero ~size:yi ctx x.parts in
        let parts = Parts.cut_start ~index:yi parts in
        { complete; parts }
    ;;

    let bashr ~size ctx x y =
      let complete = SBF.bashr ~size ctx x.complete y.complete in
      match Sub.Query.(binary_is_singleton ~size @@ binary ~size ctx y.complete) with
      | None -> Binary.lift0 ~size complete
      | Some y ->
        let yi = Z.to_int y in
        let beginning, Extract{size=s_last;value;_} = Parts.chop_last x.parts in
        let zeroes,ones = Sub.Query.(binary_to_known_bits ~size:s_last @@
                                     binary ~size:s_last ctx value) in
        let parts =
          if not (Z.testbit zeroes (s_last-1)) then
            let parts = Parts.append_zero ~size:yi ctx x.parts in
            Parts.cut_start ~index:yi parts
          else if Z.testbit ones (s_last-1) then
            let parts = Parts.append ctx x.parts [Part.ones ctx ~size:yi] in
            Parts.cut_start ~index:yi parts
          else
            let new_sz_last = s_last+yi in
            let to_append = SBF.bsext ~size:new_sz_last ~oldsize:s_last ctx value in
            let parts = Parts.append ctx beginning [Part.from_value ~size:new_sz_last to_append] in
            Parts.cut_start ~index:yi parts
        in
        { complete; parts }

    (* TODO: ici, on voudrait faire l'intersection des deux modes de calcul. *)
    let beq ~size ctx a b =
      (*Codex_log.feedback "@[<v 2>beq ~size:%d@.a = %a@.b = %a@]"
        size (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b;*)
      Parts.fold2 ctx (fun ~size acc a b ->
          (*Codex_log.feedback "beq folding: acc = %a, a = %a, b = %a"
            Sub.Boolean.pretty acc (Part.pretty ctx) a (Part.pretty ctx) b;*)
          Sub.Boolean_Forward.(&&) ctx acc @@
          Sub.Binary_Forward.beq ~size ctx (Part.do_extract ctx a) (Part.do_extract ctx b)
        ) (Sub.Boolean_Forward.true_ ctx) a.parts b.parts
    ;;

    (* TODO: Use the other beq only if better. *)
    let beq ~size ctx a b =
      let with_parts = beq ~size ctx a b in
      match Sub.query_boolean ctx with_parts with
      | Quadrivalent.True | Quadrivalent.False | Quadrivalent.Bottom -> with_parts
      | _ -> Sub.Binary_Forward.beq ~size ctx a.complete b.complete;;


    let of_bits ctx = function
      | Parts.Zeroes size -> Part.from_value ~size @@ SBF.biconst ~size Z.zero ctx
      | Parts.Ones size -> Part.from_value ~size @@ SBF.biconst ~size (Z.extract Z.minus_one 0 size) ctx
      | Parts.Unknown part -> part

    let or_bits ~size ctx a b = match a,b with
      | Parts.Zeroes _, _ -> of_bits ctx b
      | _, Parts.Zeroes _ -> of_bits ctx a
      | Parts.Ones _, _ -> of_bits ctx a
      | _, Parts.Ones _ -> of_bits ctx b
      | Parts.(Unknown pa, Unknown pb) ->
        Part.from_value ~size @@
        SBF.bor ~size ctx (Part.do_extract ctx pa) (Part.do_extract ctx pb)

    (* The parts is also used by addition in some cases. *)
    let bor' complete ~size ctx a b =
      (*Codex_log.feedback "@[<v 2>BW.bor ~size:%d@.%a@.%a@]" size (binary_pretty ~size:size ctx) a (binary_pretty ~size:size ctx) b;*)
      let res = try
          let parts = Parts.fold2_known_bits ctx (fun ctx ~size acc ba bb ->
              (*Codex_log.feedback "ba = %a, bb = %a" (Parts.pretty_bits ctx) ba (Parts.pretty_bits ctx) bb;*)
              let slice = or_bits ~size ctx ba bb in
              Parts.append ctx acc [slice]
            ) [] a.parts b.parts in
          { parts; complete }
        with Domain_sig.Bottom -> binary_empty ~size ctx in
      (*Codex_log.feedback "result = %a" (binary_pretty ~size ctx) res;*)
      res

    let bor ~size ctx a b =
      let complete = SBF.bor ~size ctx a.complete b.complete in
      bor' complete ~size ctx a b
    ;;

    let and_bits ~size ctx a b = match a,b with
      | Parts.Zeroes _, _ -> of_bits ctx a
      | _, Parts.Zeroes _ -> of_bits ctx b
      | Parts.Ones _, _ -> of_bits ctx b
      | _, Parts.Ones _ -> of_bits ctx a
      | Parts.(Unknown pa, Unknown pb) ->
        Part.from_value ~size @@
        SBF.band ~size ctx (Part.do_extract ctx pa) (Part.do_extract ctx pb)

    let band ~size ctx a b =
      try let parts = Parts.fold2_known_bits ctx (fun ctx ~size acc ba bb ->
          let slice = and_bits ~size ctx ba bb in
          Parts.append ctx acc [slice]
        ) [] a.parts b.parts in
        { parts; complete = SBF.band ~size ctx a.complete b.complete }
      with Domain_sig.Bottom -> binary_empty ~size ctx

    let xor_bits ~size ctx a b = match a,b with
      | Parts.Zeroes _, _ -> of_bits ctx b
      | _, Parts.Zeroes _ -> of_bits ctx a
      | Parts.Ones _, Parts.Ones _ ->
        Part.zero ctx ~size
      | Parts.(Unknown p, Ones _) | Parts.(Ones _, Unknown p) ->
        let pb = Part.ones ctx ~size in
        Part.from_value ~size @@
        SBF.bxor ~size ctx (Part.do_extract ctx p) (Part.do_extract ctx pb)
      | Parts.(Unknown pa, Unknown pb) ->
        Part.from_value ~size @@
        SBF.bxor ~size ctx (Part.do_extract ctx pa) (Part.do_extract ctx pb)

    let bxor ~size ctx a b =
      try let parts = Parts.fold2_known_bits ctx (fun ctx ~size acc ba bb ->
          let slice = xor_bits ~size ctx ba bb in
          Parts.append ctx acc [slice]
        ) [] a.parts b.parts in
        { parts; complete = SBF.bxor ~size ctx a.complete b.complete }
      with Domain_sig.Bottom -> binary_empty ~size ctx

    let biadd ~size ~nsw ~nuw ~nusw ctx a b =
      (* Codex_log.feedback "@[<v 2>biadd ~size:%d %a %a@]" size (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b; *)
      (* If no face-to-face bits are simultaneously 1, then addition is an OR. *)
      let exception Exit in
      let orable = try Parts.fold2 ctx (fun ~size acc pa pb ->
          let za,oa = Sub.Query.(binary ~size ctx (Part.do_extract ctx pa) |> binary_to_known_bits ~size) in
          let zb,ob = Sub.Query.(binary ~size ctx (Part.do_extract ctx pb) |> binary_to_known_bits ~size) in
          if (Z.equal Z.zero @@ Z.(land) za zb) (* If simultaneously at 0 everywhere *)
          then true
          else raise Exit) true a.parts b.parts
        with Exit -> false
      in
      if orable then
        let complete = SBF.biadd ~size ~nsw ~nuw ~nusw ctx a.complete b.complete in
        bor' complete ~size ctx a b
      else
        Binary.lift0 ~size @@ SBF.biadd ~size ~nsw ~nuw ~nusw ctx a.complete b.complete

    let bshift ~size ~offset ~max ctx x =
      let offset = biconst ~size (Z.of_int offset) ctx in
      biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx x offset
  end

  let binary_unknown ~size ctx = Binary.lift0 ~size @@ Sub.binary_unknown ~size ctx;;
  let binary_unknown_typed ~size ctx typ = Binary.lift0 ~size @@ Sub.binary_unknown_typed ~size ctx typ;;
  let boolean_unknown = Sub.boolean_unknown

  let union _ = assert false

  (**************** Serialization, fixpoind and nondet. ****************)

  (* The resulting computation: have we computed something, or should
     we juste take one of the arguments (or none). *)

  (* Higher-ranked polymorphism is required here, and we need a record for that. *)
  type 'elt higher = {subf: 'tl. Sub.Context.t -> 'elt -> Sub.Context.t -> 'elt -> 'tl
                          Sub.Context.in_acc -> ('elt,'tl) Sub.Context.result  } [@@unboxed]

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let serialize (type elt) (type c)
      {subf} ctxa a ctxb b (included, (acc : c in_tuple)) : (elt,c) result =
    let Result (included, in_tup, deserialize) = subf ctxa a ctxb b (included, acc) in
    Result (included, in_tup, deserialize)

  let serialize_boolean ctxa a ctxb b acc = serialize {subf=Sub.serialize_boolean} ctxa a ctxb b acc

  (* For now we loose everything upon serialization. *)
  let serialize_binary ~size ctxa a ctxb b (included, acc) =
    let Result (included, in_tup, deserialize) = Sub.serialize_binary ~size ctxa a.complete ctxb b.complete (included, acc) in
    Result (included, in_tup, (fun ctx out_tup ->
        let res, out_tup = deserialize ctx out_tup in
        Binary.lift0 ~size res,out_tup))
  ;;


  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let typed_nondet2 (type c) ctxa ctxb (acc : c in_tuple) =
    Sub.typed_nondet2 ctxa ctxb acc

  let nondet_same_context = Sub.nondet_same_context

  (* Note: OCaml infers the wrong type (no principal type), we have to help it here. *)
  let typed_fixpoint_step (type c) ~iteration ~init ~arg ~body (included, (acc : c in_tuple)) : bool * (close:bool -> (c out_tuple * Context.t)) =
    let bool,continuef = Sub.typed_fixpoint_step ~iteration ~init ~arg ~body (included, acc) in
    bool,fun ~close -> continuef ~close

  let widened_fixpoint_step = Sub.widened_fixpoint_step

  (**************** Queries ****************)

  module Query = struct
    include Sub.Query

    let reachable _ = assert false (* Memory-related *)

    let binary ~size ctx x =
      binary ~size ctx x.complete

  end
  let query_boolean = Sub.query_boolean

   let may_alias ~size _ = assert false
   let should_focus ~size _ = assert false


 (**************** Pretty printing ****************)


  (* let basis = Query.binary ~size ctx x in
   * Query.Binary_Lattice.pretty ~size pp basis;
   * if pretty_detailed then begin
   *   Format.pp_print_string pp "(= [";
   *   match Imap.to_list ~size ctx x with
   *   | [] -> ()
   *   | (size_fst, fst) :: tl ->
   *       pretty_bits ~size:size_fst ctx pp fst;
   *       List.iter (fun (size,bits) ->
   *         Format.fprintf pp ":%a" (pretty_bits ~size ctx) bits
   *       ) tl;
   *       Format.pp_print_string pp "]"; *)

  let boolean_pretty = Sub.boolean_pretty

  let binary_is_empty ~size ctx x = x.parts = []
  let integer_is_empty _ = assert false
  let boolean_is_empty _ = assert false

  let builtin_show_each_in string ctx args memory =
    assert false

  let satisfiable = Sub.satisfiable

end
