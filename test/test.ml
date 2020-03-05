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
