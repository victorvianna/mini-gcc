
(** {2 Interprète de code LTL} *)

val program: Ltltree.file -> int64
  (** interprète un programme LTL, à partir de la fonction [main];
      renvoie la valeur renvoyée par [main] *)
