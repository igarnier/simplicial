module Make (R : sig
  include Intf.Ring

  include Intf.Ordered with type t := t
end)
(U : Intf.Ordered) :
  Intf.Free_module
    with type t = R.t Map.Make(U).t
     and type basis = U.t
     and module R = R

module Finitely_generated
    (M : Intf.Free_module) (G : sig
      val generators : M.basis list
    end) :
  Intf.Finitely_generated_module
    with type t = M.t
     and type basis = M.basis
     and module R = M.R

(** Packed free modules. Type parameters:
    'u = index of the basis vectors
    'e = elements of the free module (finite liner combinations indexed by 'u)
    'c = coefficients of the linear combinations
*)
type ('u, 'e, 'c) t =
  (module Intf.Finitely_generated_module
     with type basis = 'u
      and type t = 'e
      and type R.t = 'c)

(** Type of linear maps between free modules. A linear map can always
    be decomposed into a Kleisli arrow [map] from the basis vectors
    of the [domain] to the vectors of [range]. *)
type (_, _, _) linear_map =
  | Ex :
      { domain : ('a, 'e, 'd) t; range : ('b, 'f, 'c) t; map : 'a -> 'f }
      -> ('e, 'f, 'c) linear_map

type stats =
  { input_rank : int;
    output_rank : int;
    kernel_rank : int;
    image_rank : int;
    torsion_coefficients : Z.t list
  }

val to_matrix : ('e, 'f, int) linear_map -> Sparse_matrix.Z.t

val rank : ('u, 'e, 'c) t -> int

val ranks_of_kernel_and_image : ('u, 'e, int) linear_map -> stats

val pp_stats : Format.formatter -> stats -> unit
