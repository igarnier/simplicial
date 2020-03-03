type t = int list

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

let equal x y = compare x y = 0

let rec hash (x : t) = hash_aux x 0

and hash_aux (x : t) (acc : int) =
  match x with
  | [] ->
      Hashtbl.hash []
  | x :: tl ->
      hash_aux tl (Hashtbl.hash (x, acc))
