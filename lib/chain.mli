(** Handling simplicial chains (free [C]-modules over [S]) *)
module Make (C : sig
  include Intf.Ring

  include Intf.Base with type t := t
end)
(S : Intf.Simplex) : sig
  module Complex : Intf.Complex with type simplex = S.t

  module Module : Intf.Free_module with type basis = S.t and type R.t = C.t

  type vector = Module.t

  type chain = (S.t, vector, C.t) Free_module.t

  val k_chain : complex:Complex.t -> k:int -> chain

  val boundary_simplex : S.t -> vector

  (** The boundary is a linear map from k-chains to k-1-chains *)
  val boundary :
    complex:Complex.t -> k:int -> (vector, vector, C.t) Free_module.linear_map
end
