module Make (Coeff : sig
  type c

  val coeff : c Coefficient.t
end)
(S : Intf.Simplex) =
struct
  module R = (val Coefficient.ring Coeff.coeff : Intf.Ring with type t = Coeff.c)

  module Simplicial_complex = Complex.Make (S)
  module M = Free_module.Make (R) (S)

  type chain_complex =
    | Empty
    | Succ of
        { dim : int;
          domain : (R.t, S.t, M.t) Free_module.t;
          map : R.t Free_module.linear_map;
          rest : chain_complex
        }

  let k_dimensional :
      Simplicial_complex.t -> int -> (R.t, S.t, M.t) Free_module.t =
   fun complex k ->
    let k_simplices =
      Simplicial_complex.Set.elements (Simplicial_complex.slice k complex)
    in
    let module K_chain =
      Free_module.Finitely_generated
        (M)
        (struct
          let generators = k_simplices
        end)
    in
    (module K_chain)

  let boundary_simplex simplex =
    let faces = S.faces simplex in
    let (chain, _) =
      let open M in
      List.fold_left
        (fun (acc, sign) face ->
          let acc = add acc (smul sign (delta face)) in
          let sign = R.mul sign (R.neg R.one) in
          (acc, sign))
        (zero, R.one)
        faces
    in
    chain

  let boundary ~complex ~k =
    let domain = k_dimensional complex k in
    let range = k_dimensional complex (k - 1) in
    Free_module.Ex { domain; range; map = boundary_simplex }

  let chain_complex ~complex =
    let dim = Simplicial_complex.dim complex in
    let rec loop dim =
      if dim = -1 then Empty
      else
        let rest = loop (dim - 1) in
        let m = k_dimensional complex dim in
        let range =
          match rest with
          | Empty -> k_dimensional complex ~-1
          | Succ { domain = range; _ } -> range
        in
        let map =
          Free_module.Ex { domain = m; range; map = boundary_simplex }
        in
        Succ { dim; domain = m; map; rest }
    in
    loop dim

  let rec homology_aux : int -> chain_complex -> (int * int) list =
   fun rank_of_image complex ->
    match complex with
    | Empty -> []
    | Succ { dim; map; rest; _ } ->
        let info = Free_module.map_info Coeff.coeff map in
        let betti = info.kernel_rank - rank_of_image in
        (dim, betti) :: homology_aux info.image_rank rest

  let homology ~complex =
    let complex = chain_complex ~complex in
    homology_aux 0 complex
end
