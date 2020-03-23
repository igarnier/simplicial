module Make (R : sig
  include Intf.Ring

  include Intf.Ordered with type t := t
end)
(U : Intf.Ordered) :
  Intf.Free_module
    with type t = R.t Map.Make(U).t
     and type basis = U.t
     and module R = R = struct
  module M = Map.Make (U)
  include Sparse_vec.Make (R) (M)

  let delta x = M.singleton x R.one

  let bind : t -> (U.t -> t) -> t =
   fun chain f ->
    fold
      (fun simplex coeff acc ->
        let chain = f simplex in
        add acc (smul coeff chain))
      zero
      chain
end

module Finitely_generated
    (M : Intf.Free_module) (G : sig
      val generators : M.basis list
    end) =
struct
  include M

  let generators = G.generators

  let coefficients vec = List.map (eval vec) generators
end

type ('u, 'e, 'c) t =
  (module Intf.Finitely_generated_module
     with type basis = 'u
      and type t = 'e
      and type R.t = 'c)

(* A linear map between free modules *)
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

let to_matrix : type e f. (e, f, int) linear_map -> Sparse_matrix.Z.t =
 fun map ->
  match map with
  | Ex { domain = (module Domain); range = (module Range); map } ->
      let input_rank = List.length Domain.generators in
      let output_rank = List.length Range.generators in
      let matrix = Sparse_matrix.Z.create ~rows:output_rank ~cols:input_rank in
      let (matrix, _) =
        List.fold_left
          (fun (matrix, col) u ->
            let coeffs = Range.coefficients (map u) in
            let (matrix, _) =
              List.fold_left
                (fun (matrix, row) coeff ->
                  let matrix =
                    Sparse_matrix.Z.set ~row ~col (Z.of_int coeff) matrix
                  in
                  (matrix, row + 1))
                (matrix, 0)
                coeffs
            in
            (matrix, col + 1))
          (matrix, 0)
          Domain.generators
      in
      matrix

let rank : type u e c. (u, e, c) t -> int =
 fun m ->
  let (module M) = m in
  List.length M.generators

let nonzero_diagonal : Sparse_matrix.Z.t -> Z.t list =
 fun matrix ->
  Sparse_matrix.Z.fold_cols
    (fun i c acc ->
      let e = Sparse_matrix.Z.Col.get c i in
      if Z.equal e Z.zero then acc else e :: acc)
    matrix
    []

let ranks_of_kernel_and_image : type u e. (u, e, int) linear_map -> stats =
 fun map ->
  match map with
  | Ex { domain; range; _ } ->
      let input_rank = rank domain in
      let output_rank = rank range in
      let matrix = to_matrix map in
      let matrix = Snf.smith_normal_form matrix in
      let diag = nonzero_diagonal matrix in
      let kernel_rank = input_rank - List.length diag in
      let image_rank = List.length diag in
      let torsion_coefficients = diag in
      { input_rank; output_rank; kernel_rank; image_rank; torsion_coefficients }

let pp_stats : Format.formatter -> stats -> unit =
 fun fmtr stats ->
  Format.fprintf fmtr "@[" ;
  Format.fprintf fmtr "input_rank = %d;" stats.input_rank ;
  Format.fprintf fmtr "output_rank = %d;" stats.output_rank ;
  Format.fprintf fmtr "kernel_rank = %d;" stats.kernel_rank ;
  Format.fprintf fmtr "image_rank = %d;" stats.image_rank ;
  Format.fprintf
    fmtr
    "torsion_coefficients = [%a];"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";")
       Z.pp_print)
    stats.torsion_coefficients ;
  Format.fprintf fmtr "@]"
