module M = Simplicial.Make (struct
  type data = unit

  let table_size = 30
end)

let simplex1 = M.of_vertices [1; 2; 3; 4]

let simplex2 = M.of_vertices [3; 4; 5; 6]

let state =
  let asc = M.insert simplex1 M.empty in
  M.insert simplex2 asc

let pp_simplex fmtr face =
  Format.fprintf
    fmtr
    "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";")
       Format.pp_print_int)
    face

let print_simplex face = Format.printf "%a@." pp_simplex face

let () =
  let splx = M.of_vertices [3; 4; 5; 6] in
  let set = M.faces splx state in
  Format.printf "printing faces of %a@." Simplicial.pp splx ;
  Simplicial.Set.iter (fun face -> Format.printf "%a" Simplicial.pp face) set

let () =
  let splx = M.of_vertices [3; 4] in
  let set = M.incidence splx state in
  Format.printf
    "printing direct incident simplicices to %a@."
    Simplicial.pp
    splx ;
  Simplicial.Set.iter (fun face -> Format.printf "%a" Simplicial.pp face) set

let () =
  let splx = M.of_vertices [3] in
  let set = M.incidence splx state in
  Format.printf
    "printing direct incident simplicices to %a@."
    Simplicial.pp
    splx ;
  Simplicial.Set.iter (fun face -> Format.printf "%a" Simplicial.pp face) set

(* Testing the boundary and star operation. *)

let () = Format.printf "Testing other operations@."

module N = Simplicial.Make (struct
  type data = unit

  let table_size = 30
end)

let print_set (s : Simplicial.Set.t) = Format.printf "%a@." Simplicial.pp_set s

let set_of_list l =
  List.fold_left (fun acc x -> Simplicial.Set.add x acc) Simplicial.Set.empty l

let () =
  let tri1 = N.of_vertices [0; 1; 5] in
  let tri2 = N.of_vertices [1; 2; 5] in
  let tri3 = N.of_vertices [2; 3; 5] in
  let tri4 = N.of_vertices [3; 4; 5] in
  let tri5 = N.of_vertices [0; 4; 5] in
  let tris = [tri1; tri2; tri3; tri4; tri5] in
  let complex = List.fold_right N.insert tris N.empty in
  let set = set_of_list [N.of_vertices [1; 5]; N.of_vertices [2; 3; 5]] in
  let closure = N.closure set complex in
  Format.printf "before:@.%a@." Simplicial.pp_set set ;
  Format.printf "closure:@.%a@." N.pp closure ;
  let set = set_of_list [N.of_vertices [5]] in
  let star = N.star set complex in
  Format.printf "star:@.%a@." Simplicial.pp_set star
