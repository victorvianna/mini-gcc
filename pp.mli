
(** {2 Utilitaires pour l'impression} *)

open Format

val print_list:
  (formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
(** [print_list sep print l] affiche la liste [l].
    Chaque élément est affiché avec [print].
    Les éléments sont séparés par un appel à [sep]. *)

val comma: formatter -> unit -> unit
(** Une virgule. *)

val semi: formatter -> unit -> unit
(** Un point-virgule. *)

val space: formatter -> unit -> unit
(** Un espace sécable. *)

val newline: formatter -> unit -> unit
(** Un retour-chariot. *)

