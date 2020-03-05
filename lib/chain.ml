module Z : Intf.Unique_factorization_domain with type t = int = struct
  type t = int

  let add = ( + )

  let mul = ( * )

  let neg = ( ~- )

  let one = 1

  let zero = 0

  let big_is_prime p =
    (* Taken from OCaml-Primes library by "Kit Freddura <kitfreddura@gmail.com>" *)
    let open Z in
    let interval x y by =
      match x = y with
      | true ->
          Gen.empty
      | false ->
          Gen.unfold
            (fun z ->
              if compare x y = compare z y then Some (z, add by z) else None)
            x
    in
    let binom a b =
      let frac =
        Gen.fold mul one (interval a (a - b) minus_one)
        / Gen.fold mul one (interval b zero minus_one)
      in
      let res = if b < a && is_even b then minus_one else one in
      frac |> mul @@ res
    in
    let expansion n = Gen.map (binom n) (interval n (add n one) one) in
    Gen.drop 1 (expansion p)
    |> Gen.peek
    |> Gen.filter_map (function (_, None) -> None | (x, _) -> Some x)
    |> Gen.for_all (fun n -> rem n p = zero)

  let is_prime p = big_is_prime (Z.of_int p)

  let prime_factorization p =
    (* Taken from OCaml-Primes library by "Kit Freddura <kitfreddura@gmail.com>" *)
    let rec divide_out p n = if p mod n = 0 then divide_out (p / n) n else p in
    let rec prime_factors' p n acc =
      if p = 1 then List.rev acc
      else
        match p mod n = 0 with
        | true ->
            if is_prime n then
              prime_factors' (divide_out p n) (n + 1) (n :: acc)
            else prime_factors' (p / n) n acc
        | false ->
            prime_factors' p (n + 1) acc
    in
    prime_factors' p 2 []
end

module Make (C : Intf.Unique_factorization_domain) (S : Intf.Simplex) = struct
  module R = C
  module M = Map.Make (S)

  type t = C.t M.t

  type underlying = S.t

  let zero = M.empty

  let delta x = M.singleton x C.one

  let add (chain1 : t) (chain2 : t) : t =
    M.union (fun _elt i1 i2 -> Some (C.add i1 i2)) chain1 chain2

  let smul coeff chain = M.map (fun x -> R.mul coeff x) chain

  let fold f acc (chain : t) = M.fold f chain acc

  let bind : t -> (S.t -> t) -> t =
   fun chain f ->
    fold
      (fun simplex coeff acc ->
        let chain = f simplex in
        add acc (smul coeff chain))
      zero
      chain

  (* let iter f mset = M.iter (fun key elt -> f key elt) mset *)

  (* let map f mset =
   *   fold (fun mset elt mult -> M.add (f elt) mult mset) M.empty mset *)

  let neg mset = M.map C.neg mset

  let boundary_simplex : S.t -> t =
   fun simplex ->
    let faces = S.faces simplex in
    let (chain, _) =
      List.fold_left
        (fun (acc, sign) face ->
          let acc = add acc (smul sign (delta face)) in
          let sign = R.mul sign (R.neg R.one) in
          (acc, sign))
        (zero, R.one)
        faces
    in
    chain

  let boundary chain = bind chain boundary_simplex
end
