
(** {2 Registres (pseudo-registres et registres physiques)} *)

type t = private string

val fresh : unit -> t
  (** un pseudo-registre frais (de la forme ["#nnn"]) *)

val is_hw: t -> bool
  (** s'agit-il d'un registre physique? *)
val is_pseudo: t -> bool
  (** s'agit-il d'un pseudo-registre? *)

val print: Format.formatter -> t -> unit
  (** fonction d'impression *)

module M : Map.S with type key = t
type 'a map = 'a M.t
  (** dictionnaires dont les clés sont des registres *)

module S : Set.S with type elt = t
type set = S.t
  (** ensembles dont les éléments sont des registres *)

val set_of_list: t list -> set

val print_set: Format.formatter -> set -> unit
  (** fonction d'impression *)

(** {2 Registres x86-64} *)

val rax: t
val rdx: t
val rdi: t
val rbp: t
val rsp: t
  (** quelques registres particuliers *)

val parameters  : t list
val result: t
  (** registres utilisés pour passer les paramètres et renvoyer le résultat *)

val caller_saved: t list
val callee_saved: t list

val allocatable : set
  (** ensemble des registres participant à l'allocation de registres *)

val tmp1: t
val tmp2: t
  (** deux registres ne faisant pas partie de [allocatable],
      que l'on peut donc utiliser pour compiler des instructions non atomiques
      (par exemple un double accès à la mémoire) *)
