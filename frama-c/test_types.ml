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

(* List of types used in tests. *)

module Ctypes = Codex.Types.Ctypes;;
open Ctypes;;

let int = { descr = Base (4, "int"); pred = Pred.True }

let contiki_list =
  { descr = Structure
    { st_byte_size = Some 4;
      st_members =
      [ (0, "next", {descr = Ptr {pointed={descr=Name "list";pred=Pred.True};index=Zero}; pred=Pred.True});
      ]
    };
    pred = Pred.True;
  }

let kennedy_union_find =
  { descr = Structure
    { st_byte_size = Some 4;
      st_members =
      [ (0, "parent", {descr = Ptr {pointed={descr=Name "uf";pred=Pred.True};index=Zero}; pred=Pred.True});
      ]
    };
    pred = Pred.True;
  }

let pred_nz = Pred.neq (Const Z.zero)

let kennedy_doubly_linked =
  { descr = Structure
    { st_byte_size = Some 8;
      st_members =
      [ (0, "next", {descr = Ptr {pointed={descr=Name "dll";pred=Pred.True};index=Zero}; pred=pred_nz});
        (4, "prev", {descr = Ptr {pointed={descr=Name "dll";pred=Pred.True};index=Zero}; pred=pred_nz});
      ]
    };
    pred = Pred.True;
  }

let kennedy_node =
  { descr = Structure
    { st_byte_size = Some 16;
      st_members =
      [ (0, "id", int);
        (4, "dll", {descr=Name "dll";pred=Pred.True});
        (12, "uf", {descr=Name "uf";pred=Pred.True});
      ]
    };
    pred = Pred.True;
  }

let kennedy_node2 =
  { descr = Structure
    { st_byte_size = Some 20;
      st_members =
      [ (0, "id1", int);
        (4, "id2", int);
        (8, "uf", {descr=Name "uf";pred=Pred.True});
        (12, "dll", {descr=Name "dll";pred=Pred.True});
      ]
    };
    pred = Pred.True;
  }

(* sll-delmin *)
let popl2017_elist =
  { descr = Structure
    { st_byte_size = Some 8;
      st_members =
      [ (0, "next", {descr = Ptr {pointed={descr=Name "elist";pred=Pred.True};index=Zero}; pred=Pred.True});
        (4, "data", int);
      ]
    };
    pred = Pred.True;
  }

(* bstree-find *)
let popl2017_etree =
  { descr = Structure
    { st_byte_size = Some 12;
      st_members =
      [ (0, "l", {descr = Ptr {pointed={descr=Name "etree";pred=Pred.True};index=Zero}; pred=Pred.True});
        (4, "r", {descr = Ptr {pointed={descr=Name "etree";pred=Pred.True};index=Zero}; pred=Pred.True});
        (8, "data", int);
      ]
    };
    pred = Pred.True;
  }

let popl2017_jsw_avlnode =
  let open Ctypes in
  { descr = Structure
    { st_byte_size = Some 16;
      st_members =
      [ (0, "balance", int);
        (4, "data", {descr = Ptr {pointed=int;index=Zero}; pred=pred_nz});
        (8, "link", {descr = Array (
            {descr = Ptr {pointed={descr=Name "jsw_avlnode";pred=Pred.True};index=Zero}; pred=Pred.True},
            Some (Const (Z.of_int 2)));
          pred=Pred.True});
      ]
    };
    pred = Pred.True;
  }

let popl2017_jsw_avltree =
  let open Ctypes in
  { descr = Structure
    { st_byte_size = Some 8;
      st_members =
      [ (0, "root", {descr = Ptr {pointed={descr=Name "jsw_avlnode";pred=Pred.True};index=Zero}; pred=Pred.True});
        (4, "size", int);
      ]
    };
    pred = Pred.True;
  }

let popl2017_jsw_avltrav =
  let open Ctypes in
  { descr = Structure
    { st_byte_size = Some 268;
      st_members =
      [ (0, "tree", {descr = Ptr {pointed={descr=Name "jsw_avltree";pred=Pred.True};index=Zero}; pred=Pred.True});
        (4, "it", {descr = Ptr {pointed={descr=Name "jsw_avlnode";pred=Pred.True};index=Zero}; pred=Pred.True});
        (8, "path", {descr = Array (
            {descr = Ptr {pointed={descr=Name "jsw_avlnode";pred=Pred.True};index=Zero}; pred=Pred.True},
            Some (Const (Z.of_int 64)));
          pred=Pred.True});
        (264, "top", int);
      ]
    };
    pred = Pred.True;
  }

let popl2017_gdsl_element_t =
  { descr = Ptr{pointed=int; index=Zero}; pred = Pred.True}

let popl2017_gdsl_element_t_nz =
  { descr = Name "gdsl_element_t" ; pred = pred_nz}

let popl2017__gdsl_node =
  { descr = Structure
    { st_byte_size = Some 12;
      st_members =
      [
        (0, "succ", {descr = Ptr{pointed={descr=Name "_gdsl_node";pred=Pred.True};index=Zero}; pred=Pred.True});
        (4, "pred", {descr = Ptr{pointed={descr=Name "_gdsl_node";pred=Pred.True};index=Zero}; pred=Pred.True});
        (8, "content", {descr=Name "gdsl_element_t";pred=Pred.True});
      ];
    };
    pred = Pred.True;
  }

let popl2017__gdsl_list =
  { descr = Structure
    { st_byte_size = Some 12;
      st_members =
      [ (0, "d", {descr = Ptr{pointed={descr=Name "_gdsl_node";pred=Pred.True};index=Zero}; pred=Pred.True;});
        (4, "z", {descr = Ptr{pointed={descr=Name "_gdsl_node";pred=Pred.True};index=Zero}; pred=Pred.True;});
        (8, "card", int);
      ];
    };
    pred = Pred.True
  }

let popl2017__gdsl_bintree =
  { descr = Structure
    { st_byte_size = Some 16;
      st_members =
      [ (0, "left", {descr = Ptr{pointed={descr=Name "_gdsl_bintree";pred=Pred.True};index=Zero}; pred=pred_nz});
        (4, "right", {descr = Ptr{pointed={descr=Name "_gdsl_bintree";pred=Pred.True};index=Zero}; pred=pred_nz});
        (8, "parent", {descr = Ptr{pointed={descr=Name "_gdsl_bintree";pred=Pred.True};index=Zero}; pred=pred_nz});
        (12, "content", {descr=Name "gdsl_element_t_nz";pred=Pred.True});
      ];
    };
    pred = Pred.True;
  }

let popl2017_gdsl_bstree =
  { descr = Structure
    { st_byte_size = Some 4;
      st_members =
      [ (0, "sent", {descr = Ptr{pointed={descr=Name "_gdsl_bintree";pred=Pred.True};index=Zero}; pred = pred_nz});
      ];
    };
    pred = Pred.True;
  }

module M = Datatype_sig.StringMap


let types =
  let t = M.empty in
  let t = M.add "list" contiki_list t in
  let t = M.add "T" contiki_list t in
  let t = M.add "uf" kennedy_union_find t in
  let t = M.add "dll" kennedy_doubly_linked t in
  let t = M.add "node" kennedy_node t in
  let t = M.add "node2" kennedy_node2 t in
  let t = M.add "elist" popl2017_elist t in
  let t = M.add "etree" popl2017_etree t in
  let t = M.add "jsw_avlnode" popl2017_jsw_avlnode t in
  let t = M.add "jsw_avltree" popl2017_jsw_avltree t in
  let t = M.add "jsw_avltrav" popl2017_jsw_avltrav t in
  let t = M.add "gdsl_element_t" popl2017_gdsl_element_t t in
  let t = M.add "_gdsl_node" popl2017__gdsl_node t in
  let t = M.add "_gdsl_list" popl2017__gdsl_list t in
  let t = M.add "_gdsl_bintree" popl2017__gdsl_bintree t in

  (* Weird name, necessary because this type is actually a typedef of an
   * anonymous struct *)
  let t = M.add "__anonstruct_gdsl_bstree_4" popl2017_gdsl_bstree t in
  t
