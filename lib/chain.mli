module Make (C : Intf.Unique_factorization_domain) (S : Intf.Simplex) : sig
  include Intf.Free_module with module R = C and type underlying = S.t

  val boundary_simplex : S.t -> t

  val boundary : t -> t
end
