
(** {2 Register Transfer Language} *)

open Ops

type register = Register.t

type label = Label.t

(** Les différents instructions RTL.
    Chaque instruction contient la ou les étiquettes
    suivantes dans le graphe de flot de contrôle. *)
type instr =
  | Econst of int32 * register * label
  | Eload of register * int * register * label
  | Estore of register * register * int * label
  | Emunop of munop * register * label
  | Embinop of mbinop * register * register * label
    (** attention au sens : [op r1 r2] représente [r2 <- r2 op r1] *)
  | Emubranch of mubranch * register * label * label
  | Embbranch of mbbranch * register * register * label * label
    (** attention au sens : [br r1 r2] représente [r2 br r1] *)
  | Ecall of register * string * register list * label
  | Egoto of label

type cfg = instr Label.map
  (** Un graphe de flot de contrôle est un dictionnaire associant à des
      étiquettes des instructions RTL. *)

(** Une fonction RTL. *)
type deffun = {
  fun_name   : string;
  fun_formals: register list;
  fun_result : register;
  fun_locals : Register.set;
  (** toutes les variables locales de la fonction maintenant regroupées ici *)
  fun_entry  : label;
  fun_exit   : label;
  fun_body   : cfg;
}

(** Un programme RTL. *)
type file = {
  funs : deffun list;
}

(** {2 Fonctions d'impression, pour debugger} *)

val print_instr: Format.formatter -> instr -> unit

val print_graph: Format.formatter ->
  cfg -> (*entry*)Label.t -> (*exit*)Label.t -> unit

val print_deffun: Format.formatter -> deffun -> unit

val print_file: Format.formatter -> file -> unit
