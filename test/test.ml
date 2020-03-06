open Simplicial

module Test_1 = struct
  module S = Simplex.Hash_consed (struct
    let initial_table_size = 41
  end)

  module M = Complex.Make (S)

  let simplex1 = S.of_list [1; 2; 3; 4]

  let simplex2 = S.of_list [3; 4; 5; 6]

  let state =
    let asc = M.insert simplex1 M.empty in
    M.insert simplex2 asc

  let print_simplex face = Format.printf "%a@." S.pp face

  let () =
    let splx = S.of_list [3; 4; 5; 6] in
    let set = M.faces splx state in
    Format.printf "printing faces of %a@." S.pp splx ;
    Format.printf "%a@." M.Set.pp set

  let () =
    let splx = S.of_list [3; 4] in
    let set = M.incidence splx state in
    Format.printf "printing direct incident simplicices to %a@." S.pp splx ;
    Format.printf "%a@." M.Set.pp set

  let () =
    let splx = S.of_list [3] in
    let set = M.incidence splx state in
    Format.printf "printing direct incident simplicices to %a@." S.pp splx ;
    Format.printf "%a@." M.Set.pp set
end

(* Testing the boundary and star operation. *)

module Test_2 = struct
  module S = Simplex.Hash_consed (struct
    let initial_table_size = 41
  end)

  module M = Complex.Make (S)

  let () = Format.printf "Testing other operations@."

  let print_set (s : M.Set.t) = Format.printf "%a@." M.Set.pp s

  let set_of_list = M.Set.of_list

  let () =
    let tri1 = S.of_list [0; 1; 5] in
    let tri2 = S.of_list [1; 2; 5] in
    let tri3 = S.of_list [2; 3; 5] in
    let tri4 = S.of_list [3; 4; 5] in
    let tri5 = S.of_list [0; 4; 5] in
    let tris = [tri1; tri2; tri3; tri4; tri5] in
    let complex = List.fold_right M.insert tris M.empty in
    let set = set_of_list [S.of_list [1; 5]; S.of_list [2; 3; 5]] in
    let closure = M.closure set complex in
    Format.printf "before:@.%a@." M.Set.pp set ;
    Format.printf "closure:@.%a@." M.pp closure ;
    let set = set_of_list [S.of_list [5]] in
    let star = M.star set complex in
    Format.printf "star:@.%a@." M.Set.pp star ;
    let set = M.slice 2 closure in
    Format.printf "dim 2:@.%a@." M.Set.pp set ;
    let set = M.slice 1 closure in
    Format.printf "dim 1:@.%a@." M.Set.pp set ;
    let set = M.slice 0 closure in
    Format.printf "dim 0:@.%a@." M.Set.pp set
end

module Z = struct
  type t = int

  let pp = Format.pp_print_int

  let hash = Hashtbl.hash

  let compare = Int.compare

  let equal = Int.equal

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

(* Test chains *)
module Test_3 = struct
  module S = Simplex.Hash_consed (struct
    let initial_table_size = 41
  end)

  module M = Complex.Make (S)
  module Chain = Chain.Make (Z) (S)

  let simplex = S.of_list [0; 1; 2; 3]

  let complex = Chain.Complex.insert simplex Chain.Complex.empty

  let () =
    let dims = [0; 1; 2; 3; 4] in
    List.iter
      (fun i ->
        let subcomplexes = Chain.Complex.slice i complex in
        Format.printf
          "dimension %d simplices: %d@."
          i
          (Chain.Complex.Set.cardinal subcomplexes))
      dims

  let boundary = Chain.boundary ~complex ~k:2

  let () =
    match boundary with
    | Free_module.Ex {domain; range; map = _} ->
        let (module D) = domain in
        let (module R) = range in
        Format.printf "input dim: %d@." (List.length D.generators) ;
        Format.printf "output dim: %d@." (List.length R.generators)

  let matrix = Free_module.to_matrix boundary

  let () =
    (* Format.printf "boundary of %a:@." S.pp simplex ;
     * Format.printf "%a@." Chain.pp boundary ; *)
    Format.printf "boundary matrix of %a:@." S.pp simplex ;
    Format.printf "%a@." (Sparse_matrix.pp Format.pp_print_int) matrix
end
