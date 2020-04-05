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
    Format.printf "Dimension of complex: %d@." (M.dim state) ;
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

(* Test chains *)
module Test_3 = struct
  module S = Simplex.Hash_consed (struct
    let initial_table_size = 41
  end)

  module Homology =
    Homology.Make
      (struct
        type c = Z.t

        let coeff = Coefficient.Z_coeff
      end)
      (S)

  module SC = Homology.Simplicial_complex

  let simplex = S.of_list [0; 1; 2; 3]

  let complex = SC.insert simplex SC.empty

  let () =
    let dims = [0; 1; 2; 3; 4] in
    List.iter
      (fun i ->
        let subcomplexes = SC.slice i complex in
        Format.printf
          "dimension %d simplices: %d@."
          i
          (SC.Set.cardinal subcomplexes))
      dims

  let boundary = Homology.boundary ~complex ~k:2

  let () =
    match boundary with
    | Free_module.Ex { domain; range; map = _ } ->
        let (module D) = domain in
        let (module R) = range in
        Format.printf "input dim: %d@." (List.length D.generators) ;
        Format.printf "output dim: %d@." (List.length R.generators)

  let matrix = Free_module.to_matrix (module Sparse_matrix.Z) boundary

  let snf = Snf.smith_normal_form matrix

  let () =
    (* Format.printf "boundary of %a:@." S.pp simplex ;
     * Format.printf "%a@." Chain.pp boundary ; *)
    Format.printf "boundary matrix of %a:@." S.pp simplex ;
    Format.printf "%a@." Sparse_matrix.Z.pp matrix ;
    Format.printf "%a@." Sparse_matrix.Z.pp snf

  let () =
    let stats = Free_module.map_info Coefficient.Z_coeff boundary in
    Format.printf "stats:@.%a@." Free_module.pp_info stats
end

(* Test smith normal form *)
module Test_4 = struct
  module M = Sparse_matrix.Z

  let m =
    let open M.Op in
    let m = M.create ~rows:3 ~cols:3 in
    let m = m.%{(0, 0)} <- Z.of_int 2 in
    let m = m.%{(0, 1)} <- Z.of_int 4 in
    let m = m.%{(0, 2)} <- Z.of_int 4 in
    let m = m.%{(1, 0)} <- Z.of_int (-6) in
    let m = m.%{(1, 1)} <- Z.of_int 6 in
    let m = m.%{(1, 2)} <- Z.of_int 12 in
    let m = m.%{(2, 0)} <- Z.of_int 10 in
    let m = m.%{(2, 1)} <- Z.of_int (-4) in
    m.%{(2, 2)} <- Z.of_int (-16)

  let () = Format.printf "%a@." M.pp m

  let m = Snf.smith_normal_form m

  let () = Format.printf "%a@." Sparse_matrix.Z.pp m

  let m =
    let open M.Op in
    let m = M.create ~rows:4 ~cols:4 in
    let m = m.%{(0, 0)} <- Z.of_int (-6) in
    let m = m.%{(0, 1)} <- Z.of_int 111 in
    let m = m.%{(0, 2)} <- Z.of_int (-36) in
    let m = m.%{(0, 3)} <- Z.of_int 6 in
    let m = m.%{(1, 0)} <- Z.of_int 5 in
    let m = m.%{(1, 1)} <- Z.of_int (-672) in
    let m = m.%{(1, 2)} <- Z.of_int 210 in
    let m = m.%{(1, 3)} <- Z.of_int 74 in
    let m = m.%{(2, 0)} <- Z.of_int 0 in
    let m = m.%{(2, 1)} <- Z.of_int (-255) in
    let m = m.%{(2, 2)} <- Z.of_int 81 in
    let m = m.%{(2, 3)} <- Z.of_int 24 in
    let m = m.%{(3, 0)} <- Z.of_int (-7) in
    let m = m.%{(3, 1)} <- Z.of_int 255 in
    let m = m.%{(3, 2)} <- Z.of_int (-81) in
    m.%{(3, 3)} <- Z.of_int (-10)

  let () = Format.printf "%a@." M.pp m

  let m = Snf.smith_normal_form m

  let () = Format.printf "%a@." Sparse_matrix.Z.pp m
end

module Test_5 = struct
  module S = Simplex.Hash_consed (struct
    let initial_table_size = 41
  end)

  module Homology =
    Homology.Make
      (struct
        type c = Z.t

        let coeff = Coefficient.Z_coeff
      end)
      (S)

  module SC = Homology.Simplicial_complex

  let complex =
    let full = S.of_list [0; 1; 2; 3] in
    S.fold_faces SC.insert full SC.empty

  let () =
    let homology = Homology.homology ~complex in
    List.iter
      (fun (dim, betti) -> Format.eprintf "dim %d: betti = %d@." dim betti)
      homology
end
