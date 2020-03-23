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

  type basis

  (** "Dirac" delta *)
  val delta : basis -> t

  (** project the coefficient corresponding to a basis vector *)
  val eval : t -> basis -> R.t

  (** [bind] = canonical, "multilinear" extension *)
  val bind : t -> (basis -> t) -> t
end

module type Finitely_generated_module = sig
  include Free_module

  (** TODO: relations? *)

  val generators : basis list

  (** List coefficients in the order of [generators] *)
  val coefficients : t -> R.t list
end

(** Sparse vectors. *)
module type Vec = sig
  include Module

  type basis

  (** Tests whether a vector is all-zero. *)
  val is_empty : t -> bool

  (** Fold over the elements of a vector in the canonical basis. *)
  val fold : (basis -> R.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** Iterate over the elements of a vector in the canonical basis. *)
  val iter : (basis -> R.t -> unit) -> t -> unit

  (** [find_map f v] iterates over the elements of a vector in the
      canonical basis. Returns the first non-[None] value of [f basis r]
      where [(basis, r)] [r] is the value of [v] at component [basis].
      This function is mostly useful when elements of the vector are enumerated
      in some well-specified order. *)
  val find_map : (basis -> R.t -> 'res option) -> t -> 'res option

  (** [set v i r] sets the component [i] of [v] to [r]. *)
  val set : t -> basis -> R.t -> t

  (** [get_exn v i] returns the element at component [i] of [v] if it
      is nonzero and raises [Not_found] otherwise. *)
  val get_exn : t -> basis -> R.t

  (** [get_opt v i] is similar to [get_exn] but returns [None] if the
      element is zero. *)
  val get_opt : t -> basis -> R.t option

  (** [get v i] returns the element at component [i] of [v]. *)
  val get : t -> basis -> R.t

  (** [swap v i j] swaps the element at components [i] and [j] of [v]. *)
  val swap : t -> basis -> basis -> t

  (** [eval v i] returns the element at component [i] of [v]. *)
  val eval : t -> basis -> R.t

  (** [of_list] builds a vector from a list of elements. *)
  val of_list : (basis * R.t) list -> t

  (** Pretty-printing *)
  val pp :
    pp_basis:(Format.formatter -> basis -> unit) ->
    pp_element:(Format.formatter -> R.t -> unit) ->
    Format.formatter ->
    t ->
    unit

  (** Infix operators. *)
  module Op : sig
    (** Alias to [get] *)
    val ( .%[] ) : t -> basis -> R.t

    (** Alias to [set] *)
    val ( .%[]<- ) : t -> basis -> R.t -> t

    (** Alias to [add] (from [Module]) *)
    val ( + ) : t -> t -> t

    (** Alias to [smul] (from [Module]) *)
    val ( * ) : R.t -> t -> t
  end
end

(** Sparse matrices. *)
module type Mat = sig
  module R : Ring

  module Col : Vec with module R = R and type basis = int

  module Row : Vec with module R = R and type basis = int

  type t

  (** Create empty (all-zero) sparse matrix. *)
  val create : rows:int -> cols:int -> t

  (** Return number of columns of matrix. *)
  val cols : t -> int

  (** Return number of rows of matrix. *)
  val rows : t -> int

  (** [get_col c m] returns column [c] of [m]. *)
  val get_col : int -> t -> Col.t

  (** [set_col c col m] sets column [col] to column index [c] in [m]. *)
  val set_col : int -> Col.t -> t -> t

  (** [get_col r m] returns row [r] of [m]. *)
  val get_row : int -> t -> Row.t

  (** [set_row r row m] sets row [row] to row index [r] in [m]. *)
  val set_row : int -> Row.t -> t -> t

  (** [get ~row ~col m] returns the element at the specified position. *)
  val get : row:int -> col:int -> t -> R.t

  (** [get_opt ~row ~col m] returns [Some x] if [x] is at the specified position
      and nonzero, [None] otherwise. *)
  val get_opt : row:int -> col:int -> t -> R.t option

  (** [set ~row ~col m v] sets the element at the specified position to [v]. *)
  val set : row:int -> col:int -> R.t -> t -> t

  (** [is_empty] returns true if the matrix is all-zero *)
  val is_empty : t -> bool

  (** Matrix equality *)
  val equal : t -> t -> bool

  (** [swap_cols i j m] swap columns [i] and [j] in [m] *)
  val swap_cols : int -> int -> t -> t

  (** [swap_rows i j m] swap rows [i] and [j] in [m] *)
  val swap_rows : int -> int -> t -> t

  (** [fold_cols f m acc] folds over the columns of [m] *)
  val fold_cols : (int -> Col.t -> 'b -> 'b) -> t -> 'b -> 'b

  (** [iter_cols_break f m] iterates a function over the colums of [m]
      and returns as soon as [f c col] is non-None. *)
  val find_map_cols : (int -> Col.t -> 'res option) -> t -> 'res option

  (** [identity n] is the identity matrix of size [n] *)
  val identity : int -> t

  (** [mul m1 m2] is the matrix multiplication of [m1] and [m2]. *)
  val mul : t -> t -> t

  (** Pretty-printer. *)
  val pp : Format.formatter -> t -> unit

  (** Infix operators. *)
  module Op : sig
    (** Alias to [get] *)
    val ( .%{} ) : t -> int * int -> R.t

    (** Alias to [set] *)
    val ( .%{}<- ) : t -> int * int -> R.t -> t

    (** Alias to [get_col] *)
    val ( .%|{} ) : t -> int -> Col.t

    (** Alias to [set_col] *)
    val ( .%|{}<- ) : t -> int -> Col.t -> t

    (** Alias to [get_row] *)
    val ( .%-{} ) : t -> int -> Row.t

    (** Alias to [set_row] *)
    val ( .%-{}<- ) : t -> int -> Row.t -> t
  end
end
