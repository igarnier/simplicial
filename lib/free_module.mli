module Finitely_generated
    (M : Basic_intf.Free_module_std) (G : sig
      val generators : M.basis list
    end) :
  Intf_simplicial.Finitely_generated_module
    with module Basis = M.Basis
     and module R = M.R
     and type t = M.t

type ('coeff, 'basis, 'vector) t =
  (module Intf_simplicial.Finitely_generated_module
     with type R.t = 'coeff
      and type Basis.t = 'basis
      and type t = 'vector)

(** Type of linear maps between free modules. A linear map can always
    be decomposed into a Kleisli arrow [map] from the basis vectors
    of the [domain] to the vectors of [range]. *)
type 'coeff linear_map =
  | Ex :
      { domain : (_, 'b1, _) t; range : ('c2, _, 'v2) t; map : 'b1 -> 'v2 }
      -> 'c2 linear_map

type map_info =
  { input_rank : int;
    output_rank : int;
    kernel_rank : int;
    image_rank : int;
    torsion_coefficients : Z.t list
  }

val to_matrix :
  (module Intf_simplicial.Mat with type t = 'm and type R.t = 'c) ->
  'c linear_map ->
  'm

(* val to_matrix : Z.t linear_map -> Sparse_matrix.Z.t *)

val rank : (_, _, _) t -> int

val map_info : 'c Coefficient.t -> 'c linear_map -> map_info

val pp_info : Format.formatter -> map_info -> unit
