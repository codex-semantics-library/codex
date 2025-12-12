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

module type S = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  val pretty: Format.formatter -> t -> unit
end

module Undefined(Name:sig val name:string end) = struct
  let equal _ = failwith ("equal not implemented for " ^ Name.name)
  let compare _ = failwith ("compare not implemented for " ^ Name.name)
  let hash _ = failwith ("hash not implemented for " ^ Name.name)
  let pretty _ = failwith ("pretty not implemented for " ^ Name.name)
end

module Conv(B1:S)(C:sig type t val conv: t -> B1.t end) = struct
  type t = C.t
  let equal a b = B1.equal (C.conv a) (C.conv b)
  let compare a b = B1.compare (C.conv a) (C.conv b)
  let hash a = B1.hash (C.conv a)
  let pretty fmt a = B1.pretty fmt (C.conv a)
end

module Prod2(B1:S)(B2:S) = struct
  type t = B1.t * B2.t
  let equal (a1,b1) (a2,b2) = B1.equal a1 a2 && B2.equal b1 b2
  let compare (a1,b1) (a2,b2) =
    let ra = B1.compare a1 a2 in
    if ra != 0 then ra
    else B2.compare b1 b2
  let hash (a,b) = Hashing.hash2 (B1.hash a) (B2.hash b)
  let pretty fmt (a,b) = Format.fprintf fmt "(%a,%a)" B1.pretty a B2.pretty b
end

module Prod3(B1:S)(B2:S)(B3:S) = struct
  type t = B1.t * B2.t * B3.t
  let equal (a1,b1,c1) (a2,b2,c2) = B1.equal a1 a2 && B2.equal b1 b2 && B3.equal c1 c2
  let compare (a1,b1,c1) (a2,b2,c2) =
    let ra = B1.compare a1 a2 in
    if ra != 0 then ra
    else
      let rb = B2.compare b1 b2 in
      if rb != 0 then rb
      else B3.compare c1 c2
  let hash (a,b,c) = Hashing.hash3 (B1.hash a) (B2.hash b) (B3.hash c)
  let pretty fmt (a,b,c) = Format.fprintf fmt "(%a,%a,%a)" B1.pretty a B2.pretty b B3.pretty c
end

(****************************************************************)
(* Generic sum types. *)

module Sum2(BA:S)(BB:S):S with type t = (BA.t,BB.t) Either.t = struct
  open Either
  type t = (BA.t,BB.t) Either.t

  let equal: t -> t -> bool = fun x y ->
    match x,y with
    | Left x, Left y ->  BA.equal x y
    | Right x, Right y ->  BB.equal x y
    | _ -> false

  let compare: t -> t -> int = fun x y ->
    match x,y with
    | Left _, Right _ -> -1
    | Left x, Left y ->  BA.compare x y
    | Right x, Right y ->  BB.compare x y
    | Right _, Left _ -> 1
  ;;

  let hash: t -> int = fun x ->
    match x with
    | Left x -> 2 * BA.hash x
    | Right x -> (2 * BB.hash x) + 1
  ;;

  let pretty: Format.formatter -> t -> unit = fun fmt x ->
    match x with
    | Left x -> BA.pretty fmt x
    | Right x -> BB.pretty fmt x
  ;;
end

type ('a,'b,'c) sum3 = Sum3A of 'a | Sum3B of 'b | Sum3C of 'c

module Sum3(BA:S)(BB:S)(BC:S):S with type t = (BA.t,BB.t,BC.t) sum3 = struct
  type t = (BA.t,BB.t,BC.t) sum3

  let equal: t -> t -> bool = fun x y ->
    match x,y with
    | Sum3A x, Sum3A y ->  BA.equal x y
    | Sum3B x, Sum3B y ->  BB.equal x y
    | Sum3C x, Sum3C y ->  BC.equal x y
    | _ -> false

  let compare: t -> t -> int = fun x y ->
    match x,y with
    | Sum3A x, Sum3A y ->  BA.compare x y
    | Sum3A _, _ -> -1
    | Sum3B _, Sum3A _ -> 1
    | Sum3B x, Sum3B y ->  BB.compare x y
    | Sum3B _, _ -> -1
    | Sum3C x, Sum3C y -> BC.compare x y
    | Sum3C x, _ -> 1
  ;;

  let hash: t -> int = fun x ->
    match x with
    | Sum3A x -> 3 * BA.hash x
    | Sum3B x -> (3 * BB.hash x) + 1
    | Sum3C x -> (3 * BC.hash x) + 2
  ;;

  let pretty: Format.formatter -> t -> unit = fun fmt x ->
    match x with
    | Sum3A x -> BA.pretty fmt x
    | Sum3B x -> BB.pretty fmt x
    | Sum3C x -> BC.pretty fmt x
  ;;
end

(****************************************************************)
(* Usual type operators. *)

module Int = struct
  type t = int
  let equal = (==)
  let compare (a:int) (b:int) = Int.compare a b
  let hash x = x
  let pretty = Format.pp_print_int
end

module String = struct
  include Stdlib.String
  let hash s = Stdlib.Hashtbl.hash s
  let pretty = Format.pp_print_string
end

module Option(B:S)= struct
  type t = B.t option
  let equal a b = match (a,b) with
    | None, None -> true
    | Some a, Some b -> B.equal a b
    | _ -> false
  ;;

  let compare a b = match (a,b) with
    | None, None -> 0
    | None, Some a -> -1
    | Some a, None -> 1
    | Some a, Some b -> B.compare a b
  ;;

  let hash = function
    | None -> 0
    | Some x -> 1 + B.hash x
  ;;

  let pretty fmt = function
    | None -> Format.fprintf fmt "None"
    | Some x -> Format.fprintf fmt "Some(%a)" B.pretty x
  ;;

  let the = function
    | None -> failwith "the on None"
    | Some x -> x

end

module List(B:S)= struct
  type t = B.t list

  let hash l = Hashing.hash_list B.hash l

  let pretty fmt = function
    | [] -> Format.fprintf fmt "[]"
    | [x] -> Format.fprintf fmt "[%a]" B.pretty x
    | a::b -> Format.fprintf fmt "[@[<hv>%a"  B.pretty a;
      List.iter (fun x -> Format.fprintf fmt ";@,%a" B.pretty x) b;
      Format.fprintf fmt "@]]"

  module L = struct
    include List
    type 'a t = 'a list
  end

  include (L: module type of L with type 'a t := 'a list)

  let equal a b =
    try List.for_all2 B.equal a b
    with Invalid_argument _ -> false

  let compare a b = compare B.compare a b
end

module Set(B:S) = struct
  open Extstdlib
  include Set.Make(B);;

  let hash l = fold (fun x acc -> Hashing.hash2 acc (B.hash x)) l 0

  let pretty fmt x =
    if is_empty x then Format.fprintf fmt "{}"
    else let i = ref 0 in
         iter (fun x ->
           if !i == 0 then Format.fprintf fmt "@[<hv>%a@]" B.pretty x
           else Format.fprintf fmt ";@,%a" B.pretty x;
           incr i) x
end

module Map(B:S) = struct
  include Extstdlib.Map.Make(B)

  module With_Value(V:S) = struct

    type 'a map = 'a t
    type t = V.t map

    let compare = compare V.compare
    let equal = equal V.equal
    let hash m = fold (fun key value acc ->
      Hashing.hash3 acc (B.hash key) (V.hash value)) m 0
    ;;
    module Alist = List(Prod2(B)(V));;
    let pretty fmt m = Alist.pretty fmt (bindings m);;
  end
end

module Hashtbl(B:S) = struct
  include Hashtbl.Make(B)

  module With_Value(V:S) = struct
    type 'a tbl = 'a t
    type t = V.t tbl

    (* Comparing a mutable structure does not really make sense. *)
    let compare _ _ = assert false
    let equal = (==)

    (* We could hash by hashing on the first element found by iter (or
       all of them).  *)
    let hash m = assert false
    ;;
    let pretty fmt h =
      Format.fprintf fmt "{@[<hv>";
      iter (fun key value -> Format.fprintf fmt "@[<h>%a@ ->@ %a;@]@ "
        B.pretty key V.pretty value
      ) h;
      Format.fprintf fmt "@]}"
    ;;

  end
end

module Unit:S with type t = unit = struct
  type t = unit
  let compare () () = 0
  let equal () () = true
  let hash () = 0
  let pretty fmt () = ()
end

module StringMap = Map(String)
module StringHash = Hashtbl(String)
