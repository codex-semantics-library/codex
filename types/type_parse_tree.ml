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

(** This module contains the abstract syntax tree for the types in our input [.typedc]
  files, along with their reference concrete syntax. A [.typedc] file is simply
  a list of {!definition}s.

  The parser itself is defined in {!Type_parser}.

  For each type in the AST, we will describe in mathematical block the corresponding
  grammar as it should appear in the text file:
  {ul
  {- terminals (i.e. symbols and names that appear verbatim in the [.typedc] file),
    will use {m \texttt{\color{green}green typewriter font}}}
  {- all other fonts are non-terminals. They are usually defined somewhere else in this file.
     - We use {m \text{``}\mathrm{ident}\text{''}} as a non-terminal that matches C-like identifiers (combination of
       upper case and lowercase letter, underscores [_] and digits, that does not start with a digit.)
     - Similarly, {m \text{``}\mathrm{fun\_ident}\text{''}} is a non-terminal for function identifiers.
       It is an identifier that may also contain [@] characters.}
  {- Terminals or non-terminals that end in {m ?} (not to be confused with the terminal {m \texttt{\color{green}?}})
     are optional and may be omitted.}
  }

  The parser also support C style inline {m \texttt{\color{green}//} ... \texttt{\color{green}\textbackslash{}n} }
  and block {m \texttt{\color{green}/*} ... \texttt{\color{green}*/}}
  comments, and accepts any number of whitespace
  characters between terminals.

  This page is meant as a technical reference, for a gentler introduction to
  the typing language, see {!page-"Types tutorial"}. *)

module Symbol = struct
  type t = {name:string; id:int}
  let intern (x:string) =
    let count = ref 0 in
    let h = Hashtbl.create 100 in
    match Hashtbl.find h x with
    | exception Not_found ->
      let res = {name=x;id = !count} in
      incr count;
      Hashtbl.replace h x res;
      res
    | x -> x
  let compare a b = Int.compare a.id b.id
  let equal = (==)
  let pp fmt x = Format.pp_print_string fmt x.name
end

(** Unary operators:
  {math
    \begin{array}{rcll}
      \sim & \triangleq & \texttt{\color{green}!} & \text{TODO logical or bitwise negation?} \\
           & |          & \texttt{\color{green}-} & \text{unary minus}
    \end{array}
  }
  TODO: it seems unop is currently not part of the parser
*)
type unop =
  | Not
  | UMinus

(** Binary operators:
  {math
    \begin{array}{rcll}
      \diamond & \triangleq & \texttt{\color{green}+} \;\;|\;\;\texttt{\color{green}-} \;\;|\;\;\texttt{\color{green}*} \;\;|\;\;\texttt{\color{green}/} \;\;|\;\;\texttt{\color{green}\%} & \text{arithmetic operators} \\
               & | & \texttt{\color{green}\&} \;\;|\;\; \texttt{\color{green}|} \;\;|\;\; \texttt{\color{green}\textasciicircum} \;\;|\;\; \texttt{\color{green}<<}  \;\;|\;\; \texttt{\color{green}>>}  & \text{bitwise operators} \\
               & | & \texttt{\color{green}==} \;\;|\;\; \texttt{\color{green}!=} \;\;|\;\; \texttt{\color{green}>=} \;\;|\;\; \texttt{\color{green}>} \;\;|\;\; \texttt{\color{green}<=} \;\;|\;\; \texttt{\color{green}<} & \text{comparison operators} \\
               & | & \texttt{\color{green}\&\&} \;\;|\;\; \texttt{\color{green}|} \texttt{\color{green}|} & \text{logical and, logical or}
    \end{array}
  }
*)
type binop =
  | Plus                        (* + *)
  | Minus                       (* - *)
  | Mult                        (* * *)
  | Div                         (* / *)
  | Mod                         (* % *)
  | Bitwise_and                 (* & *)
  | Bitwise_or                  (* | *)
  | Bitwise_xor                 (* ^ *)
  | LShift                      (* << *)
  | RShift                      (* >> *)
  | Eq                          (* == *)
  | Diff                        (* != *)
  | Ge                          (* >= *)
  | Gt                          (* > *)
  | Le                          (* <= *)
  | Lt                          (* < *)
  | Logical_and                 (* && *)
  | Logical_or                  (* || *)



(** Type expressions {m \mathrm{exp}} are given by:
  {math
    \begin{array}{rcll}
      \mathrm{exp} & \triangleq & \texttt{\color{green}self}    & \text{self keyword} \\
               & |          & z                & \text{numeric constant }z \in \mathbb{Z} \\
               & |          & \mathrm{ident}            & \text{symbolic variable} \\
               & |          & \sim \mathrm{exp}           & \text{unary operation: TODO (this is not implemented)} \\
               & |          & \mathrm{exp} \diamond \mathrm{exp} & \text{binary operation} \\
               & |          & \texttt{\color{green}(} \mathrm{exp} \texttt{\color{green})} & \text{parenthesized expression}
    \end{array}
  }

  To avoid any ambiguity, the parser only allows only obvious precedences:

   - multiplicative operators ([*], [/] and [%]) bind more strongly than
     additive operators ([+] and [-]);
   - both bind less strongly than binary predicates ([==], [<], ...);
   - binary predicates bind less strongly than [&&] and [||];
   - multiplicative and additive operators are left-associative.
   - we report an error otherwise.

    We don't allow chaining binary predicates, e.g.
    [a < b == c], [a < b > c], or [a || b && c] is forbidden.

    This technique is inspired by Per Vognsen, who pointed it out in
    {: https://www.scattered-thoughts.net/writing/better-operator-precedence/}
  *)
type expr =
  | Self
  | Cst    of Z.t
  | Var    of Symbol.t
  | Unary  of unop * expr
  | Binary of binop * expr * expr

(** Pointer annotations:
    {math
    \begin{array}{rcll}
      \mathrm{ptr\_annot} & \triangleq & \texttt{\color{green}*}    & \text{Unknown: may or may not be null} \\
                      & |          & \texttt{\color{green}+}    & \text{Non-null} \\
                      & |          & \texttt{\color{green}?}    & \text{null or non null}
    \end{array}
    }
  Note that, while in C pointers may implicitly represent pointer to arrays, in Codex
  that is not the case. A pointer points to a single value, to have a pointer to an
  array, you must explicitly use {m \tau[n]* } or its variants.

  TODO clarify semantics of the star, especially relative to [?]
*)
type ptr_annot =
  | Maybe_null (* p* *)
  | Non_null (* p+ *)
  | Null_or_non_null (* ? *)


(** Think of this like a string, e.g. [TypeNameStruct "hello"] correspons to "struct hello".
    We do this because of the different namespaces that exist in C.

    {math
    \begin{array}{rcll}
      \mathrm{type\_name} & \triangleq & \texttt{\color{green}struct}\ \mathrm{ident}                    & \text{struct name} \\
                      & |          & \texttt{\color{green}union}\ \mathrm{ident}                    & \text{union name} \\
                      & |          & \texttt{\color{green}enum}\ \mathrm{ident}                    & \text{enum name, TODO this is not implemented in the parser} \\
                      & |          & \mathrm{ident} & \text{custom name (introduced by }\texttt{\color{green}type}\text{ or }\texttt{\color{green}region}\text{)}
    \end{array}
    }
    Basic type names [int], [char], [long], [float], [int32_t], [uint8_t]... are predefined and can be used
    as in C. Their size depends on the selected architecture (see {!Parse_ctypes.init}).
    We also have an [integer] type and explicitly sized [word1], [word2], [word4] and [word8] types.
    *)
type type_name =
  | TypeNameStruct of string
  | TypeNameUnion of string
  | TypeNameEnum of string
  | TypeName of string

(** Codex types:
    {math
    \begin{array}{rcll}
      \tau & \triangleq & \mathrm{type\_name}                    & \text{named type} \\
           & |          & \mathrm{type\_name}\ \texttt{\color{green}(}\ \mathrm{exp}_1\texttt{\color{green},}\ \cdots\texttt{\color{green},}\ \mathrm{exp}_n\ \texttt{\color{green})} & \text{applied type (parameter instantiation)} \\
           & |          & \tau\; \texttt{\color{green}?} \;\;|\;\; \tau\; \texttt{\color{green}+} \;\;|\;\; \tau\; \texttt{\color{green}*}       & \text{pointer types, see }\mathrm{ptr\_annot} \\
           & |          & \tau{\color{green}\texttt [ }\mathrm{expr}{\color{green}\texttt ] }  & \text{array type} \\
           & |          & \texttt{\color{green}struct \{} \tau_1\ \mathrm{ident}_1\texttt{\color{green};}\; \cdots\texttt{\color{green};}\; \tau_n\ \mathrm{ident}_n\texttt{\color{green};}\ \texttt{\color{green}\}} & \text{struct type, trailing semi-colon required} \\
           & |          & \texttt{\color{green}union \{} \tau_1\ \mathrm{ident}_1\texttt{\color{green};}\; \cdots\texttt{\color{green};}\; \tau_n\ \mathrm{ident}_n\texttt{\color{green};}\ \texttt{\color{green}\}} & \text{union type, trailing semi-colon required} \\
           & |          & \tau \ \texttt{\color{green}with}\ \mathrm{exp} & \text{refinement type: type with a constraint predicate} \\
           & |          & \texttt{\color{green}∃}\;\mathrm{ident} \ \texttt{\color{green}:}\ \tau\ \texttt{\color{green}.}\ \tau \;\;|\;\; \texttt{\color{green}\textbackslash exists}\;\mathrm{ident}\ \texttt{\color{green}:}\ \tau\ \texttt{\color{green}.}\ \tau & \text{existential type, using unicode ∃ (U+2203) or latex command}\\
           & |          & {\color{green}\texttt [ } \tau_1\texttt{\color{green},}\ \cdots \texttt{\color{green},}\ \tau_n {\color{green}\texttt ] }\ \texttt{\color{green}->}\ \tau_{\text r} \;\;|\;\;
           \tau_{\text r}\ \texttt{\color{green}<-}\ \texttt{\color{green}(} \tau_1\texttt{\color{green},}\ \cdots \texttt{\color{green},}\ \tau_n \texttt{\color{green})}  & \text{function type} \\
           & |          & \tau_{\text r}\ \texttt{\color{green}(}\mathrm{ptr\_annot} \texttt{\color{green})}\ \texttt{\color{green}(} \tau_1\texttt{\color{green},}\ \cdots \texttt{\color{green},}\ \tau_n \texttt{\color{green})} & \text{C-like function pointer type} \\
           & |          & {\color{green}\texttt ( }\tau {\color{green}\texttt ) } & \text{parenthesized type}
    \end{array}
    }
*)
type typ =
  | Name       of type_name
  | Applied    of constr * (expr list)
  | Pointer    of typ * ptr_annot
  | Array      of typ * expr
  | Struct     of (string * typ) list
  | Union      of (string * typ) list
  | Constraint of typ * expr
  | Exists     of Symbol.t * typ * typ
  | Function   of typ * typ list

and constr =
  | LambdaAlias  of type_name

and constr_def =
  | Lambda       of (string list) * typ

type t =
  | Type      of typ
  | Constr    of constr_def
  | FunDef    of {inline:bool; pure:bool; funtyp:typ} (** funtyp must be a function type: {!Function}, {{!Exists}[Exists(_,_,Function _)]} or deeper existentials *)
  | Global    of typ

(** Type definitions, these can appear as statements in [.typedc] files.
    {math
    \begin{array}{rcll}
      \mathrm{fun\_param} & \triangleq & \texttt{\color{green}void}    & \text{void parameter} \\
                      & |          & \tau \; \mathrm{ident} & \text{named parameter}
    \end{array}
    }

    {math
    \begin{array}{rcll}
      \mathrm{fun} & \triangleq & \tau \; \mathrm{fun\_ident}\texttt{\color{green}(}\mathrm{fun\_param}_1\texttt{\color{green},}\ \cdots\texttt{\color{green},}\ \mathrm{fun\_param}_n \texttt{\color{green})}    & \text{Function declaration} \\
               & |          & \texttt{\color{green}∃}\;\mathrm{ident} \ \texttt{\color{green}:}\ \tau\ \texttt{\color{green}.}\ \mathrm{fun} \;\;|\;\; \texttt{\color{green}\textbackslash exists}\;\mathrm{ident}\ \texttt{\color{green}:}\ \tau\ \texttt{\color{green}.}\ \mathrm{fun} & \text{Existential function declaration}
    \end{array}
    }

    {math
    \begin{array}{rcll}
      \mathrm{def} & \triangleq & \texttt{\color{green}type}\ \mathrm{type\_name}\ \texttt{\color{green}=}\ \tau\texttt{\color{green};} & \text{type definition} \\
               & | & \texttt{\color{green}type}\ \mathrm{type\_name}\ \texttt{\color{green}(}\ \mathrm{ident}_1\texttt{\color{green},}\ \cdots\texttt{\color{green},}\ \mathrm{ident}_n\ \texttt{\color{green})}\ \texttt{\color{green}=}\ \tau\texttt{\color{green};} & \text{parameterized type definition} \\
               & | & \texttt{\color{green}region}\ \mathrm{type\_name}\ \texttt{\color{green}=}\ \tau\texttt{\color{green};} & \text{region definition} \\
               & | & \texttt{\color{green}region}\ \mathrm{type\_name}\ \texttt{\color{green}(}\ \mathrm{ident}_1\texttt{\color{green},}\ \cdots\texttt{\color{green},}\ \mathrm{ident}_n\ \texttt{\color{green})}\ \texttt{\color{green}=}\ \tau\texttt{\color{green};} & \text{parameterized region definition} \\
               & | & \texttt{\color{green}inline}?\ \texttt{\color{green}pure}?\ \mathrm{fun}\texttt{\color{green};} & \text{function declaration, inline and pure are optional} \\
               & | & \texttt{\color{green}struct}\ \mathrm{ident}\ \texttt{\color{green}\{}\ \tau_1\ \mathrm{ident}_1\texttt{\color{green};}\; \cdots\texttt{\color{green};}\; \tau_n\ \mathrm{ident}_n\texttt{\color{green};}\ \texttt{\color{green}\};} & \text{C struct declaration, trailing semi-colon required} \\
               & | & \texttt{\color{green}union}\ \mathrm{ident}\ \texttt{\color{green}\{}\ \tau_1\ \mathrm{ident}_1\texttt{\color{green};}\; \cdots\texttt{\color{green};}\; \tau_n\ \mathrm{ident}_n\texttt{\color{green};}\ \texttt{\color{green}\};} & \text{C union declaration, trailing semi-colon required} \\
               & | & \tau\ \mathrm{ident} \texttt{\color{green};} & \text{global type declaration} \\
    \end{array}
    }
*)
type definition =
  | TypeDefinition of (type_name * t)
  | RegionDefinition of (type_name * t)
  | GlobalDefinition of (string * t)


module Pretty = struct


  let pp_sep str = fun fmt () -> Format.fprintf fmt str;;
  let pp_list_with_sep str x =
    Format.pp_print_list ~pp_sep:(pp_sep str) x


  let rec typ fmt = function
    | Name s -> Format.fprintf fmt "%a" type_name s
    | Pointer(t,a) -> Format.fprintf fmt "(%a)%a" typ t ptr_annot a
    | Array(t,e) -> Format.fprintf fmt "(%a)[%a]" typ t expr e
    | Struct l -> Format.fprintf fmt "struct { @[<hv>%a@] }" (pp_list_with_sep "@ " cfield) l
    | Union l -> Format.fprintf fmt "union { @[<hv>%a@] }" (pp_list_with_sep "@ " cfield) l
    | Constraint(t,p) -> Format.fprintf fmt "(%a) with %a" typ t expr p
    | Exists(v,t1,t2) -> Format.fprintf fmt "∃ %a:(%a). (%a)" Symbol.pp v typ t1 typ t2
    | Applied((LambdaAlias x),args) ->
      Format.fprintf fmt "%a(%a)" type_name x (pp_list_with_sep ", " expr) args
    | Function(ret,args) ->
      Format.fprintf fmt "%a <- (%a)" typ ret (pp_list_with_sep ", " typ) args

  and type_name fmt = function
    | TypeNameEnum str -> Format.fprintf fmt "enum %s" str
    | TypeNameStruct str -> Format.fprintf fmt "struct %s" str
    | TypeNameUnion str -> Format.fprintf fmt "union %s" str
    | TypeName str -> Format.fprintf fmt "%s" str

  and binop = function
    | Eq -> "=="
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Bitwise_or -> "|"
    | Bitwise_and -> "&"
    | Bitwise_xor -> "^"
    | LShift -> "<<"
    | RShift -> ">>"
    | Diff -> "!="
    | Ge -> ">="
    | Gt -> ">"
    | Le -> "<="
    | Lt -> "<"
    | Logical_and -> "&&"
    | Logical_or -> "||"


  and expr fmt = function
    | Binary(bop,a,b) -> Format.fprintf fmt "(%a) %s (%a)" expr a (binop bop) expr b
    | Cst a -> Format.fprintf fmt "%s" (Z.format "%d" a)
    | Self -> Format.fprintf fmt "self"
    | Var v -> Format.fprintf fmt "%a" Symbol.pp v
    | Unary _ -> assert false


  and cfield fmt (name,t) = Format.fprintf fmt "%a %s;" typ t name

  and ptr_annot fmt = function
    | Maybe_null -> Format.fprintf fmt "*"
    | Non_null -> Format.fprintf fmt "+"
    | Null_or_non_null -> Format.fprintf fmt "?"

  and def fmt = function
    | TypeDefinition(name,Type t) -> Format.fprintf fmt "type %a = %a" type_name name typ t
    | RegionDefinition(name,Type t) -> Format.fprintf fmt "region %a = %a" type_name name typ t
    | GlobalDefinition(name, FunDef{inline;funtyp=Function(ret,args)}) ->
       Format.fprintf fmt "%s %a %s(%a);" (if inline then "inline " else "")
         typ ret name (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") typ) args
    | GlobalDefinition(name, FunDef{inline;pure;funtyp}) ->
       Format.fprintf fmt "%s %s %a" (if inline then "inline " else "") (if pure then "pure " else "") typ funtyp
    | TypeDefinition(name, Constr(Lambda(v,t))) ->
      Format.fprintf fmt "type %a(%a) = %a" type_name name vars v typ t
    | RegionDefinition(name, Constr(Lambda(v,t))) ->
      Format.fprintf fmt "region %a(%a) = %a" type_name name vars v typ t
    | GlobalDefinition(name, Global t) -> Format.fprintf fmt "%a %s;" typ t name
    | _ -> assert false

  and vars fmt vars = (pp_list_with_sep ",@ " var) fmt vars

  and var = Format.pp_print_string


  and annotations fmt x =
    x |> List.iter (fun d -> Format.fprintf fmt "%a@." def d)

end
