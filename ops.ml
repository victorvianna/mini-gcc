
(** {2 Opérations x86-64} *)

(** opérations unaires *)
type munop =
  | Maddi   of int32
  | Msetei  of int32
  | Msetnei of int32

(** opérations binaires *)
type mbinop =
  | Mmov
  | Madd
  | Msub
  | Mmul
  | Mdiv
  | Msete
  | Msetne
  | Msetl
  | Msetle
  | Msetg
  | Msetge

(** opérations de branchement unaires *)
type mubranch =
  | Mjz
  | Mjnz
  | Mjlei of int32
  | Mjgi  of int32

(** opérations de branchement binaires *)
type mbbranch =
  | Mjl
  | Mjle

(** {2 Fonctions d'impression} *)

open Format

let print_munop fmt = function
  | Maddi i -> fprintf fmt "add $%ld" i
  | Msetei i -> fprintf fmt "sete $%ld" i
  | Msetnei i -> fprintf fmt "setne $%ld" i

let print_mbinop fmt = function
  | Mmov -> fprintf fmt "mov"
  | Madd -> fprintf fmt "add"
  | Msub -> fprintf fmt "sub"
  | Mmul -> fprintf fmt "imul"
  | Mdiv -> fprintf fmt "idiv"
  | Msete -> fprintf fmt "sete"
  | Msetne -> fprintf fmt "setne"
  | Msetl -> fprintf fmt "setl"
  | Msetle -> fprintf fmt "setle"
  | Msetg -> fprintf fmt "setg"
  | Msetge -> fprintf fmt "setge"

let print_mubranch fmt = function
  | Mjz -> fprintf fmt "jz"
  | Mjnz -> fprintf fmt "jnz"
  | Mjlei n -> fprintf fmt "jle $%ld" n
  | Mjgi n -> fprintf fmt "jg $%ld" n

let print_mbbranch fmt = function
  | Mjl -> fprintf fmt "jl"
  | Mjle -> fprintf fmt "jle"


