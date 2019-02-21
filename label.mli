
(** {2 Étiquettes} *)

type t = private string
  (** le type des étiquettes *)

val fresh : unit -> t
  (** une étiquette fraîche (de la forme ["Lnnn"]) *)

module M : Map.S with type key = t
type 'a map = 'a M.t
  (** dictionnaires dont les clés sont des étiquettes *)

module S : Set.S with type elt = t
type set = S.t
  (** ensembles dont les éléments sont des étiquettes *)

val print : Format.formatter -> t -> unit
  (** fonction d'impression *)


