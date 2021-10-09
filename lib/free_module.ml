module Finitely_generated
    (M : Basic_intf.Free_module_std) (G : sig
      val generators : M.basis list
    end) =
struct
  include M

  let generators = G.generators

  let coefficients vec = List.map (eval vec) generators
end

type ('coeff, 'basis, 'vector) t =
  (module Intf_simplicial.Finitely_generated_module
     with type R.t = 'coeff
      and type Basis.t = 'basis
      and type t = 'vector)

(* A linear map between free modules *)
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

let to_matrix :
    type c m.
    (module Intf_simplicial.Mat with type t = m and type R.t = c) ->
    c linear_map ->
    m =
 fun (module Mat) map ->
  match map with
  | Ex { domain = (module Domain); range = (module Range); map } ->
      let input_rank = List.length Domain.generators in
      let output_rank = List.length Range.generators in
      let matrix = Mat.create ~rows:output_rank ~cols:input_rank in
      let (matrix, _) =
        List.fold_left
          (fun (matrix, col) u ->
            let coeffs = Range.coefficients (map u) in
            let (matrix, _) =
              List.fold_left
                (fun (matrix, row) coeff ->
                  let matrix = Mat.set ~row ~col coeff matrix in
                  (matrix, row + 1))
                (matrix, 0)
                coeffs
            in
            (matrix, col + 1))
          (matrix, 0)
          Domain.generators
      in
      matrix

(* let to_matrix : type c. c Coefficient.t -> c linear_map -> c Sparse_matrix.t =
 *   fun (type c) (coeff : c Coefficient.t) (map : c linear_map) ->
 *    let (module Mat : Intf_simplicial.Mat with type R.t = c) =
 *      match coeff with
 *      | Coefficient.Z_coeff -> (module Sparse_matrix.Z)
 *      | Coefficient.Int_coeff -> (module Sparse_matrix.Int)
 *      | Coefficient.Z2_coeff -> (module Sparse_matrix.Z2)
 *      | Coefficient.Q_coeff -> (module Sparse_matrix.Q)
 *      | Coefficient.Float_coeff -> (module Sparse_matrix.Float)
 *    in
 *    match map with
 *    | Ex { domain = (module Domain); range = (module Range); map } ->
 *        let input_rank = List.length Domain.generators in
 *        let output_rank = List.length Range.generators in
 *        let matrix = Mat.create ~rows:output_rank ~cols:input_rank in
 *        let (matrix, _) =
 *          List.fold_left
 *            (fun (matrix, col) u ->
 *              let coeffs = Range.coefficients (map u) in
 *              let (matrix, _) =
 *                List.fold_left
 *                  (fun (matrix, row) coeff ->
 *                    let matrix = Mat.set ~row ~col coeff matrix in
 *                    (matrix, row + 1))
 *                  (matrix, 0)
 *                  coeffs
 *              in
 *              (matrix, col + 1))
 *            (matrix, 0)
 *            Domain.generators
 *        in
 *        matrix
 *
 * let to_matrix : Z.t linear_map -> Sparse_matrix.Z.t =
 *  fun map ->
 *   match map with
 *   | Ex { domain = (module Domain); range = (module Range); map } ->
 *       let input_rank = List.length Domain.generators in
 *       let output_rank = List.length Range.generators in
 *       let matrix = Sparse_matrix.Z.create ~rows:output_rank ~cols:input_rank in
 *       let (matrix, _) =
 *         List.fold_left
 *           (fun (matrix, col) u ->
 *             let coeffs = Range.coefficients (map u) in
 *             let (matrix, _) =
 *               List.fold_left
 *                 (fun (matrix, row) coeff ->
 *                   let matrix = Sparse_matrix.Z.set ~row ~col coeff matrix in
 *                   (matrix, row + 1))
 *                 (matrix, 0)
 *                 coeffs
 *             in
 *             (matrix, col + 1))
 *           (matrix, 0)
 *           Domain.generators
 *       in
 *       matrix *)

let rank : type u e c. (u, e, c) t -> int =
 fun m ->
  let (module M) = m in
  List.length M.generators

let nonzero_diagonal :
    type c m.
    (module Intf_simplicial.Mat with type t = m and type R.t = c) -> m -> c list
    =
 fun (module Mat) matrix ->
  Mat.fold_cols
    (fun i c acc ->
      let e = Mat.Col.get c i in
      if Mat.R.equal e Mat.R.zero then acc else e :: acc)
    matrix
    []

let map_info : type c. c Coefficient.t -> c linear_map -> map_info =
 fun coeff map ->
  match map with
  | Ex { domain; range; _ } ->
      let input_rank = rank domain in
      let output_rank = rank range in
      let diag =
        match coeff with
        | Coefficient.Z_coeff ->
            let matrix = to_matrix (module Sparse_matrix.Z) map in
            let matrix = Snf.smith_normal_form matrix in
            nonzero_diagonal (module Sparse_matrix.Z) matrix
        | Coefficient.Z2_coeff | Coefficient.Q_coeff | Coefficient.Float_coeff
          ->
            (* TODO: gaussian elimination over arbitrary field *)
            Stdlib.failwith "non-Z coefficients not implemented yet"
      in
      let kernel_rank = input_rank - List.length diag in
      let image_rank = List.length diag in
      let torsion_coefficients = diag in
      { input_rank; output_rank; kernel_rank; image_rank; torsion_coefficients }

let pp_info : Format.formatter -> map_info -> unit =
 fun fmtr map_info ->
  Format.fprintf fmtr "@[" ;
  Format.fprintf fmtr "input_rank = %d;" map_info.input_rank ;
  Format.fprintf fmtr "output_rank = %d;" map_info.output_rank ;
  Format.fprintf fmtr "kernel_rank = %d;" map_info.kernel_rank ;
  Format.fprintf fmtr "image_rank = %d;" map_info.image_rank ;
  Format.fprintf
    fmtr
    "torsion_coefficients = [%a];"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";")
       Z.pp_print)
    map_info.torsion_coefficients ;
  Format.fprintf fmtr "@]"
