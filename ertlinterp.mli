
(** {2 Interprète de code ERTL} *)

val program: Ertltree.file -> int64
  (** interprète un programme ERTL, à partir de la fonction [main];
      renvoie la valeur renvoyée par [main] *)
