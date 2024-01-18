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

(* Memory-efficient replacement for maps, useful when we have a lot of
   small maps. *)
module Make(Key:Map.OrderedType) = struct
  module M = Map.Make(Key)

  type key = Key.t
           
  (* Small maps, with keys in increasing order.*)
  type 'a t =
    | Empty
    | One of {key1:Key.t;value1:'a}
    | Two of {key1:Key.t;value1:'a;key2:Key.t;value2:'a}
    | Three of {key1:Key.t;value1:'a;
                key2:Key.t;value2:'a;
                key3:Key.t;value3:'a;                
               }
    | Large of 'a M.t
  ;;

  let find key map = match map with
    | Empty -> raise Not_found
    | One{key1;value1} ->
       if Key.compare key key1 = 0 then value1
       else raise Not_found
    | Two{key1;value1;key2;value2} ->
       begin match Key.compare key key1 with
       | 0 -> value1
       | x when (* x > 0 && *) Key.compare key key2 = 0 -> value2
       | _ -> raise Not_found
       end
    | Three{key1;value1;key2;value2;key3;value3} ->
       begin match Key.compare key key2 with
       | 0 -> value2
       | x when x < 0 ->
          if Key.compare key key1 = 0 then value1 else raise Not_found
       | _ ->
          if Key.compare key key3 = 0 then value3 else raise Not_found
       end
       (* begin
        * if Key.compare key key1 = 0 then value1
        * else if Key.compare key key2 = 0 then value2
        * else if Key.compare key key3 = 0 then value3
        * else raise Not_found *)
    | Large m -> M.find key m
  ;;

  let bindings = function
    | Empty -> []
    | One{key1;value1} -> [(key1,value1)]
    | Two{key1;value1;key2;value2} -> [(key1,value1);(key2,value2)]
    | Three{key1;value1;key2;value2;key3;value3} -> [(key1,value1);(key2,value2);(key3,value3)]
    | Large m -> M.bindings m

  let fold f m init = match m with
    | Empty -> init
    | One{key1;value1} -> f key1 value1 init
    | Two{key1;value1;key2;value2} -> f key2 value2 @@ f key1 value1 init
    | Three{key1;value1;key2;value2;key3;value3} ->
       f key3 value3 @@ f key2 value2 @@ f key1 value1 init
    | Large m -> M.fold f m init

  let add key value map = match map with
    | Empty -> One{key1=key;value1=value}
    | One{key1;value1} -> begin
       match Key.compare key key1 with
       | 0 -> One{key1;value1=value}
       | x when x < 0 -> Two{key1=key;value1=value;key2=key1;value2=value1}
       | _ -> Two{key1;value1;key2=key;value2=value}
      end
    | Two{key1;value1;key2;value2} -> begin
        match Key.compare key key1 with
        | 0 -> Two{key1;value1=value;key2;value2}
        | x when x < 0 -> Three{key1=key;value1=value;key2=key1;value2=value1;key3=key2;value3=value2}
        | _ -> begin match Key.compare key key2 with
               | 0 -> Two{key1;value1;key2;value2=value}
               | x when x < 0 -> Three{key1;value1;key2=key;value2=value;key3=key2;value3=value2}
               | _ -> Three{key1;value1;key2;value2;key3=key;value3=value}
               end
      end
    | Three{key1;value1;key2;value2;key3;value3} -> begin
        match Key.compare key key2 with
        | 0 -> Three{key1;value1;key2;value2=value;key3;value3}
        | x when x < 0 && Key.compare key key1 = 0 -> Three{key1;value1=value;key2;value2;key3;value3}
        | x when x > 0 && Key.compare key key3 = 0 -> Three{key1;value1;key2;value2;key3;value3=value}
        | _ ->
           Large (M.add key value @@ M.add key3 value3 @@ M.add key2 value2 @@ M.singleton key1 value1)
      end
    | Large m -> Large (M.add key value m)
  ;;

  let empty = Empty
               
end
