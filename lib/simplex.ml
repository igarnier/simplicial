module Not_hash_consed = struct
  type t = int list

  let rec dim = function [] -> -1 | _ :: tl -> dim tl + 1

  let int_compare (x : int) (y : int) =
    if x < y then -1 else if x > y then 1 else 0

  let of_list x = List.sort int_compare x

  let rec compare (x : t) (y : t) =
    let x = of_list x in
    let y = of_list y in
    list_compare x y

  and list_compare (x : t) (y : t) =
    match (x, y) with
    | ([], []) ->
        0
    | (x :: a, y :: b) ->
        let c = int_compare x y in
        if c = 0 then list_compare a b else c
    | ([], _) ->
        -1
    | (_, []) ->
        1

  let fold_faces : (t -> 'a -> 'a) -> t -> 'a -> 'a =
   fun f l acc ->
    let rec loop l prev acc =
      match l with
      | [] ->
          acc
      | x :: tl ->
          let face = List.rev_append prev tl in
          loop tl (x :: prev) (f face acc)
    in
    loop l [] acc

  let faces x =
    let faces = fold_faces (fun f l -> f :: l) x [] in
    List.rev faces

  let equal x y = compare x y = 0

  let rec hash (x : t) = hash_aux x 0

  and hash_aux (x : t) (acc : int) =
    match x with
    | [] ->
        Hashtbl.hash []
    | x :: tl ->
        hash_aux tl (Hashtbl.hash (x, acc))

  let pp fmtr x =
    match x with
    | [] ->
        Format.fprintf fmtr "∅"
    | _ ->
        Format.fprintf
          fmtr
          "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";")
             Format.pp_print_int)
          x
end

module Hash_consed (X : sig
  val initial_table_size : int
end) =
struct
  module Table = Hashcons.Make (Not_hash_consed)

  type hconsed = Not_hash_consed.t Hashcons.hash_consed

  type t = {dim : int; simplex : hconsed; faces : t list}

  let table = Table.create X.initial_table_size

  let dim {dim; _} = dim

  let rec of_list l =
    let simplex = Not_hash_consed.of_list l in
    let hashed = Table.hashcons table simplex in
    let dim = List.length simplex in
    let faces =
      Not_hash_consed.fold_faces
        (fun face acc ->
          let face = of_list face in
          face :: acc)
        simplex
        []
    in
    {dim; simplex = hashed; faces = List.rev faces}

  let faces {faces; _} = faces

  let compare x y =
    let open Hashcons in
    Int.compare x.simplex.tag y.simplex.tag

  let equal x y =
    let open Hashcons in
    Int.equal x.simplex.tag y.simplex.tag

  let hash x = x.simplex.hkey

  let fold_faces f {faces; _} acc = List.fold_right f faces acc

  let pp fmtr {simplex; _} = Not_hash_consed.pp fmtr simplex.node
end
