(** Computing with simplicial complexes. *)
module Make (S : Intf.Simplex) : sig
  (** [t] is the type of an abstract simplicial complex. *)
  type t

  module Simplex : Intf.Simplex with type t = S.t

  module Set : sig
    include Set.S with type elt = Simplex.t

    val pp : Format.formatter -> t -> unit
  end

  (** The empty simplicial complex. *)
  val empty : t

  val mem : Simplex.t -> t -> bool

  val faces : Simplex.t -> t -> Set.t

  (** [incidence s c] returns the set of simplices to which
      [s] is incident in the compelx [c]. *)
  val incidence : Simplex.t -> t -> Set.t

  (** [insert s c] adds the Simplex.t [s] and recursively all
      sub-simplices into the complex [c]. *)
  val insert : Simplex.t -> t -> t

  val set : Simplex.t -> t -> Set.t

  val lower : Simplex.t -> t -> t

  val closure : Set.t -> t -> t

  val star : Set.t -> t -> Set.t

  (** [slice k c] returns the set of k-dimensional simplices in the complex [c]. *)
  val slice : int -> t -> Set.t

  (** [dim c] returns the dimension of the complex, ie maximum dimension of the simplices included in [c].
      If [c] is empty, returns [-1]. *)
  val dim : t -> int

  val pp : Format.formatter -> t -> unit
end
