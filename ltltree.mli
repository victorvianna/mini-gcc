
(** {2 Location Transfer Language (LTL)} *)

open Ops

type ident = string
  (** uniquement pour les fonctions *)

type register = Register.t

type label = Label.t

(** une opérande est un registre physique (Reg) ou un emplacement
    de pile (Spilled) *)
type operand =
  | Reg of Register.t
  | Spilled of int

(** Les différentes instructions LTL *)
type instr =
  (** les mêmes que dans ERTL *)
  | Eload of register * int * register * label
  | Estore of register * register * int * label
  | Egoto of label
  | Ereturn
  (** les mêmes que dans ERTL, mais avec operand à la place de register *)
  | Econst of int32 * operand * label
  | Emunop of munop * operand * label
  | Embinop of mbinop * operand * operand * label
  | Emubranch of mubranch * operand * label * label
  | Embbranch of mbbranch * operand * operand * label * label
  | Epush of operand * label
  (** légèrement modifiée *)
  | Ecall of ident * label
  (** nouveau *)
  | Epop of register * label

type cfg = instr Label.map
  (** Un graphe de flot de contrôle est un dictionnaire associant à des
      étiquettes des instructions LTL. *)

(** une fonction LTL *)
type deffun = {
  fun_name : ident;
  fun_entry: label;
  fun_body : cfg;
}

(** un programme LTL *)
type file = {
  funs : deffun list;
}

(** {2 Utilitaires} *)

val succ: instr -> label list

(** {2 Fonctions d'impression, pour debugger} *)

val print_operand: Format.formatter -> operand -> unit

val print_instr: Format.formatter -> instr -> unit

val print_graph: Format.formatter -> cfg -> label -> unit

val print_deffun: Format.formatter -> deffun -> unit

val print_file: Format.formatter -> file -> unit

