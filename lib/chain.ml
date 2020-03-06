module Make (C : sig
  include Intf.Ring

  include Intf.Base with type t := t
end)
(S : Intf.Simplex) =
struct
  module Module = Free_module.Make (C) (S)
  module Complex = Complex.Make (S)

  type vector = Module.t

  type chain = (S.t, Module.t, C.t) Free_module.t

  let k_chain : complex:Complex.t -> k:int -> chain =
   fun ~complex ~k ->
    let k_simplices = Complex.Set.elements (Complex.slice k complex) in
    let module K_chain =
      Free_module.Finitely_generated
        (Module)
        (struct
          let generators = k_simplices
        end)
    in
    (module K_chain)

  let boundary_simplex : S.t -> vector =
   fun simplex ->
    let faces = S.faces simplex in
    let (chain, _) =
      let open Module in
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
    let domain = k_chain ~complex ~k in
    let range = k_chain ~complex ~k:(k - 1) in
    Free_module.Ex {domain; range; map = boundary_simplex}
end
