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

module Make
    (Domain : Codex.Domains.Memory_domains.With_focusing.S_with_types)
= struct

  module Registers = struct
    module Domain = Domain
    let registers = [("eax",32); ("ebx",32); ("ecx",32); ("edx",32); ("edi",32);
                     ("esp",32); ("ebp",32);
                     ("esi",32);
                     (* ("div",64); ("quo",64); ("rem",64); *)
                     ("div64",64); ("quo64",64); ("rem64",64);
                     (* Flags? *)
                     ("CF",1); ("PF",1); ("AF",1); ("ZF",1); ("SF",1); ("TF",1);
                     ("DF",1); ("OF",1); ("NT",1); ("RF",1); ("AC",1); ("ID",1);
                     ("IF",1); ("IOPL",2); ("VIF",1); ("VIP",1); ("VM",1); ("cpl",2);
                     (* Segment registers *)
                     ("cs",16); ("ds",16); ("ss",16); ("es",16); ("fs",16); ("gs",16);
                     (* Task register *)
                     ("tr",16);
                     (***** "Virtual" registers needed for the DBA translation *****)
                     ("res8",8); ("res16",16); ("res32",32); ("temp32",32);
                     ("temp32_0",32); ("temp32_1",32); ("temp32_2",32); ("temp32_3",32);
                     ("temp64",64);
                     ("res64",64);
                     (* GDTR (global descriptor table) base *)
                     ("gdt",32);
                     (* Segment bases (hidden registers) *)
                     ("ds_base",32); ("cs_base",32); ("ss_base",32); ("es_base",32);
                     ("fs_base",32); ("gs_base",32);
                     ("tr_base",32);
                     (* Segment descriptors (hidden registers) *)
                     ("ds_desc",64); ("cs_desc",64); ("ss_desc",64); ("es_desc",64);
                     ("fs_desc",64); ("gs_desc",64);
                     ("tr_desc",64);
                     (* IDTR (interrupt descriptor table) base *)
                     ("idt",32);
                     (* MM0 *)
                     ("mm0",64);
                    ] |> List.map (fun (name,size) -> (name, Units.In_bits.of_int size))
    let initial_value ctx (name,size) =
      match name with
      | "esp" ->
          if Codex_options.AnalyzeKernel.get () then
            Domain.binary_unknown ~size ctx
          else
            (* XXX: specific to one example *)
            Domain.Binary_Forward.biconst ~size (Z.of_int 0x10000) ctx
      | "ds_base" | "cs_base" | "ss_base" | "ts_base" ->
          Domain.Binary_Forward.biconst ~size Z.zero ctx
      | "IF" | "TF" | "VM" | "RF" | "NT" ->
          Domain.Binary_Forward.biconst ~size Z.zero ctx
      | _ -> Domain.binary_unknown ~size ctx
  end
end
