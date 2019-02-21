
(** {2 Un modèle mémoire bas niveau pour les interprètes ERTL et LTL} *)

type t
(** le type abstrait d'une mémoire *)

type value = int64
type ptr = int64
(** ici toutes les valeurs sont des entiers 64 bits *)

val word_size: int
(** vaut ici 8, i.e., 64 bits *)

val zero: value
val one: value

exception Error of string
(** cette exception est levée en cas d'accès illégal à la mémoire *)

val create: ?words:int -> unit -> t
(** renvoie une mémoire fraîche
    (par défaut, sa taille est de 65536 mots, partagés entre pile et tas) *)

val malloc: t -> int -> ptr
(** [malloc t n] alloue [n] octets *)

val get: t -> Register.t -> value
val set: t -> Register.t -> value -> unit

val load: t -> ptr -> ofs:int -> value
(** lit en mémoire ; [ptr+ofs] doit être un multiple de [word_size] *)

val store: t -> ptr -> ofs:int -> value -> unit
(** écrit en mémoire ; [ptr+ofs] doit être un multiple de [word_size] *)

val push: t -> value -> unit
val push_register: t -> Register.t -> unit
val pop: t -> value
val pop_in_register: t -> Register.t -> unit

val fresh_registers: t -> t
(** duplique la mémoire, avec de nouveaux pseudo-registres (pour un appel) ;
    tout le reste est partagé (registres physiques, sbrk, contenu mémoire) *)
