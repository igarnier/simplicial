(** Totally ordered types *)
module type Ordered = sig
  type t

  val compare : t -> t -> int
end

(** Pretty-printable types *)
module type Pp = sig
  type t

  val pp : Format.formatter -> t -> unit
end

(** We require some structures that we manipulate
    to be comparable, printable and hashable. *)
module type Base = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val hash : t -> int
end

module type Simplex = sig
  type t

  include Base with type t := t

  val dim : t -> int

  val of_list : int list -> t

  val faces : t -> t list

  val fold_faces : (t -> 'a -> 'a) -> t -> 'a -> 'a
end

module type Complex = sig
  type t

  type simplex

  module Set : sig
    include Set.S with type elt = simplex

    val pp : Format.formatter -> t -> unit
  end

  val empty : t

  val mem : simplex -> t -> bool

  val faces : simplex -> t -> Set.t

  val incidence : simplex -> t -> Set.t

  val insert : simplex -> t -> t

  val set : simplex -> t -> Set.t

  val lower : simplex -> t -> t

  val closure : Set.t -> t -> t

  val star : Set.t -> t -> Set.t

  val slice : int -> t -> Set.t

  val pp : Format.formatter -> t -> unit
end

module type Abelian_group = sig
  type t

  val zero : t

  val add : t -> t -> t

  val neg : t -> t
end

module type Monoid = sig
  type t

  val one : t

  val mul : t -> t -> t
end

module type Ring = sig
  include Abelian_group

  include Monoid with type t := t
end

module type Unique_factorization_domain = sig
  (* Ring must be commutative *)
  include Ring

  val prime_factorization : t -> t list
end

module type Module = sig
  module R : Ring

  include Abelian_group

  val smul : R.t -> t -> t
end

module type Free_module = sig
  include Module

  type underlying

  (** "Dirac" delta *)
  val delta : underlying -> t

  (** project the coefficient corresponding to a basis vector *)
  val eval : t -> underlying -> R.t

  (** [bind] = canonical, "multilinear" extension *)
  val bind : t -> (underlying -> t) -> t
end

module type Finitely_generated_module = sig
  include Free_module

  val generators : underlying list

  (** List coefficients in the order of [generators] *)
  val coefficients : t -> R.t list
end
