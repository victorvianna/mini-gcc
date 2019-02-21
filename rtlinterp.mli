
(** {2 Interprète de code RTL} *)

val program: Rtltree.file -> int64
  (** interprète un programme RTL, à partir de la fonction [main];
      renvoie la valeur renvoyée par [main] *)
