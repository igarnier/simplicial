module Make (R : Intf.Ring) (U : Intf.Ordered) :
  Intf.Free_module
    with type t = R.t Map.Make(U).t
     and type underlying = U.t
     and module R = R

module Finitely_generated
    (M : Intf.Free_module) (G : sig
      val generators : M.underlying list
    end) :
  Intf.Finitely_generated_module
    with type t = M.t
     and type underlying = M.underlying
     and module R = M.R

(** Packed free modules. Type parameters:
    'u = index of the basis vectors
    'e = elements of the free module (finite liner combinations indexed by 'u)
    'c = coefficients of the linear combinations
*)
type ('u, 'e, 'c) t =
  (module Intf.Finitely_generated_module
     with type underlying = 'u
      and type t = 'e
      and type R.t = 'c)

(** Type of linear maps between free modules. A linear map can always
    be decomposed into a Kleisli arrow [map] from the basis vectors
    of the [domain] to the vectors of [range]. *)
type (_, _, _) linear_map =
  | Ex : {
      domain : ('a, 'e, 'd) t;
      range : ('b, 'f, 'c) t;
      map : 'a -> 'f;
    }
      -> ('e, 'f, 'c) linear_map

val to_matrix : ('e, 'f, 'c) linear_map -> 'c Sparse_matrix.t
