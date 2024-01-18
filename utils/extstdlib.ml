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

module Array = struct

  include Array

  exception Fast_ret
  let for_all p a =
    try 
      for i = 0 to (Array.length a) - 1 do
        if not @@ p a.(i) then raise Fast_ret
      done;
      true
    with Fast_ret -> false
    
  (* A special kind of fold: computes f(a.(0),f(a.(1),...)). *)
  let reduce f x =
    let rec loop acc i =
      if i == 0 then acc
      else let i = i - 1 in
           loop (f acc x.(i)) i
    in let n = (Array.length x) - 1 in
       loop x.(n) n
  ;;

  (* Map and reduce at the same time; no intermediary array is
     necessary. *)
  let map_reduce m f ar = 
    let rec loop acc i =
      if i == 0 then acc
      else let i = i - 1 in
           loop (f acc (m ar.(i))) i
    in let n = (Array.length ar) - 1 in
       loop (m ar.(n)) n
  ;;

    
  (* TODO: Could be made more efficient than that; at least avoid the
     first conversion to list, and just copy the array if nothing is
     filtered. *)
  let filter p ar = Array.of_list (List.filter p (Array.to_list ar));;

  (* Tests that all elements in the array are equivalent using the
     equivalence relation [pred]. *)
  let equivalent pred array =
    assert (Array.length array > 0);
    (* As this is an equivalence relation, we only need to compare
       with the first element. *)
    let first = array.(0) in
    let rec loop i =
      if i >= Array.length array then true
      else
        let elt = array.(i) in
        if pred first elt then loop (i+1)
        else false
    in loop 1
  ;;

    
end

module List = struct
  include List
  let reduce f l =
    match l with
    | [] -> assert false
    | a::b -> List.fold_left f a b

  (* Only available in recent OCamls. *)
  let rec equal eq l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | [], _::_ | _::_, [] -> false
    | a1::l1, a2::l2 -> eq a1 a2 && equal eq l1 l2

end

(* TODO: Faire un module ExtStream *)
module Stream = struct
  include Stream

  let map f stream =
    let next _i =
      try Some (f (Stream.next stream))
      with Stream.Failure -> None in
    Stream.from next
  ;;

  let fold f init stream = 
    let rec loop acc =
      match Stream.peek stream with
      | None -> acc
      | Some x -> Stream.junk stream; loop @@ f acc x
    in loop init

  let reduce f stream =
    let value =
      try Stream.next stream
      with Stream.Failure -> assert false in
    fold f value stream
  ;;
  
end

module Map = struct

  module type S = sig
    include Map.S
    val is_singleton: 'a t -> (key * 'a) option
    val is_inter_empty: 'a t -> 'b t -> bool
    val mk_pretty: (Format.formatter -> key -> unit) -> (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a t -> unit
    
    val for_all2: (key -> 'a option -> 'a option -> bool) -> 'a t -> 'a t -> bool

    val fold2:'a t -> 'b t -> 'c -> (key -> 'a option -> 'b option -> 'c -> 'c) -> 'c
    val fold_on_diff2:'a t -> 'a t -> 'c -> (key -> 'a option -> 'a option -> 'c -> 'c) -> 'c
    val fold3:'a t -> 'b t -> 'c t -> 'd -> (key -> 'a option -> 'b option -> 'c option -> 'd -> 'd) -> 'd
    val fold_on_diff3:'a t -> 'a t -> 'a t -> 'd -> (key -> 'a option -> 'a option -> 'a option -> 'd -> 'd) -> 'd      
  end
  
  module Make(Ord:Map.OrderedType) = struct

    include Map.Make(Ord);;

    let is_singleton m =
      let exception Exit in
      try fold (fun key value acc -> match acc with
          | None -> Some(key,value)
          | Some _ -> raise Exit
        ) m None
      with Exit -> None

    let is_inter_empty a b =
      let exception Not_empty in
      try 
        let _ = merge (fun address a b ->
            match a,b with
            | Some _, Some _ -> raise Not_empty
            | _, _ -> None) a b in
        true
      with Not_empty -> false

    
    exception MyExit;;
    (* We use merge, but try to build nothing. *)
    let for_all2 pred m1 m2 =
      try
        ignore (merge (fun key a b ->
            let result = pred key a b in
            match result with
            | false -> raise MyExit
            | true -> None) m1 m2);
        true
      with MyExit -> false
    ;;

    let fold2 a b acc f = 
      let commonab = merge (fun addr a b -> Some(a,b)) a b in
      fold (fun key (a,b) acc -> f key a b acc) commonab acc
    ;;

    (* TODO: Should be skipping subtrees. *)
    let fold_on_diff2 a b acc f =
      fold2 a b acc (fun key a b acc ->
          match a,b with
          | None,None -> assert false
          | Some a, Some b when a == b -> acc
          | _ -> f key a b  acc
        ) 
    ;;

    let fold3 a b c acc f = 
      let commonab = merge (fun addr a b -> Some(a,b)) a b in
      let commonabc = merge (fun addr ab c -> match ab,c with
          | Some(a,b), c -> Some(a,b,c)
          | None, c -> Some(None,None,c)
        ) commonab c
      in fold (fun key (a,b,c) acc -> f key a b c acc) commonabc acc
    ;;

    (* TODO: Should be skipping subtrees. *)
    let fold_on_diff3 a b c acc f =
      fold3  a b c acc (fun key a b c acc ->
          match a,b,c with
          | None,None,None -> assert false
          | Some a, Some b, Some c when a == b && b == c -> acc
          | _ -> f key a b c acc
        ) 
    ;;

    let mk_pretty key value fmt map =
      Format.fprintf fmt "@[<hv>";
      map |> iter (fun addr v -> Format.fprintf fmt "%a -> %a@ " key addr value v);
      Format.fprintf fmt "@]"
    ;;

    
  end


end

module Set = struct

  module type S = sig
    include Set.S
    val reduce: (elt -> elt -> elt) -> t -> elt
    val map_reduce: (elt -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a
    val is_singleton: t -> bool
    val mk_pretty: (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
  end

  module Make(Ord:Set.OrderedType) = struct

    include Set.Make(Ord)

    let reduce f s =
      assert(not @@ is_empty s);
      let first = choose s in
      let others = remove first s in
      fold f others first
    ;;

    let map_reduce map reduce s =
      assert(not @@ is_empty s);
      let first = choose s in
      let others = remove first s in
      fold (fun x acc -> reduce acc @@ map x) others (map first)

    let is_singleton set =
      let i = ref 0 in
      try
        iter (fun _ -> incr i;
               if !i > 1 then raise Exit) set;
        !i == 1
      with Exit -> false
    ;;

    let mk_pretty elt fmt set =
      Format.fprintf fmt "@[<hv>";
      set |> iter (fun x -> Format.fprintf fmt "%a@ " elt x);
      Format.fprintf fmt "@]"
    ;;

      
  end
  
end

module Option = struct

  let the = function
    | Some x -> x
    | _ -> assert false

  let map f = function
    | None -> None
    | Some x -> Some(f x)

end
