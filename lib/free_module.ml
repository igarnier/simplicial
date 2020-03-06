module Make (R : Intf.Ring) (U : Intf.Ordered) :
  Intf.Free_module
    with type t = R.t Map.Make(U).t
     and type underlying = U.t
     and module R = R = struct
  module R = R
  module Map = Map.Make (U)

  type t = R.t Map.t

  type underlying = U.t

  let zero = Map.empty

  let delta x = Map.singleton x R.one

  let add (chain1 : t) (chain2 : t) : t =
    Map.union (fun _elt i1 i2 -> Some (R.add i1 i2)) chain1 chain2

  let smul coeff chain = Map.map (fun x -> R.mul coeff x) chain

  let fold f acc (chain : t) = Map.fold f chain acc

  let eval v u = match Map.find_opt u v with None -> R.zero | Some c -> c

  let bind : t -> (U.t -> t) -> t =
   fun chain f ->
    fold
      (fun simplex coeff acc ->
        let chain = f simplex in
        add acc (smul coeff chain))
      zero
      chain

  let neg vec = Map.map R.neg vec
end

module Finitely_generated
    (M : Intf.Free_module) (G : sig
      val generators : M.underlying list
    end) =
struct
  include M

  let generators = G.generators

  let coefficients vec = List.map (eval vec) generators
end

type ('u, 'e, 'c) t =
  (module Intf.Finitely_generated_module
     with type underlying = 'u
      and type t = 'e
      and type R.t = 'c)

(* A linear map between free modules *)
type (_, _, _) linear_map =
  | Ex : {
      domain : ('a, 'e, 'd) t;
      range : ('b, 'f, 'c) t;
      map : 'a -> 'f;
    }
      -> ('e, 'f, 'c) linear_map

let to_matrix : type e f c. (e, f, c) linear_map -> c Sparse_matrix.t =
 fun map ->
  match map with
  | Ex {domain = (module Domain); range = (module Range); map} ->
      let input_dimension = List.length Domain.generators in
      let output_dimension = List.length Range.generators in
      let matrix =
        Sparse_matrix.create ~rows:output_dimension ~cols:input_dimension
      in
      let (matrix, _) =
        List.fold_left
          (fun (matrix, col) u ->
            let coeffs = Range.coefficients (map u) in
            let (matrix, _) =
              List.fold_left
                (fun (matrix, row) coeff ->
                  let matrix = Sparse_matrix.set ~row ~col matrix coeff in
                  (matrix, row + 1))
                (matrix, 0)
                coeffs
            in
            (matrix, col + 1))
          (matrix, 0)
          Domain.generators
      in
      matrix
