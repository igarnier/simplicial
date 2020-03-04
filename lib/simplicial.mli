(** The type of abstract simplices. *)
type simplex

(** Handling set of simplices (which are not necessarily simplicial sets!) *)
module Set : Set.S with type elt = simplex

(** Pretty-printing. *)
val pp : Format.formatter -> simplex -> unit

val pp_set : Format.formatter -> Set.t -> unit

(** Module type of abstract simplicial complexes *)
module type S = sig
  type t

  type data

  val of_vertices : int list -> simplex

  val empty : t

  val mem : simplex -> t -> bool

  val faces : simplex -> t -> Set.t

  val incidence : simplex -> t -> Set.t

  val data : simplex -> t -> data option

  val attach : simplex -> data -> t -> t

  val insert : simplex -> t -> t

  val set : simplex -> t -> Set.t

  val lower : simplex -> t -> t

  val closure : Set.t -> t -> t

  val star : Set.t -> t -> Set.t

  val pp : Format.formatter -> t -> unit
end

module Make (X : sig
  type data

  val table_size : int
end) : S with type data = X.data
