module Make (R : Intf_simplicial.Ring) (M : Map.S) :
  Intf_simplicial.Vec
    with module R = R
     and type t = R.t M.t
     and type basis = M.key = struct
  module R = R

  type t = R.t M.t

  type basis = M.key

  let is_empty = M.is_empty

  let zero = M.empty

  let add (vec1 : t) (vec2 : t) : t =
    M.union
      (fun _elt i1 i2 ->
        let res = R.add i1 i2 in
        if R.compare res R.zero = 0 then None else Some res)
      vec1
      vec2

  let smul coeff vec =
    if R.compare coeff R.zero = 0 then zero
    else M.map (fun x -> R.mul coeff x) vec

  let neg vec = M.map R.neg vec

  let fold = M.fold

  let iter = M.iter

  let find_map : (basis -> R.t -> 'res option) -> t -> 'res option =
    fun (type res) f vec ->
     let exception Return of res in
     try
       M.iter
         (fun basis elt ->
           match f basis elt with None -> () | Some res -> raise (Return res))
         vec ;
       None
     with Return res -> Some res

  let set vec i e =
    if R.compare e R.zero = 0 then M.remove i vec else M.add i e vec

  let get_exn vec i = M.find i vec

  let get_opt vec i = M.find_opt i vec

  let get vec i = try M.find i vec with Not_found -> R.zero

  let swap vec i j =
    match (M.find_opt i vec, M.find_opt j vec) with
    | (None, None) -> vec
    | (Some elt, None) ->
        let vec = set vec i R.zero in
        set vec j elt
    | (None, Some elt) ->
        let vec = set vec j R.zero in
        set vec i elt
    | (Some e1, Some e2) ->
        let vec = set vec i e2 in
        set vec j e1

  let eval v u = match M.find_opt u v with None -> R.zero | Some c -> c

  let of_list : (basis * R.t) list -> t =
   fun l -> List.fold_left (fun vec (i, elt) -> set vec i elt) zero l

  let pp :
      pp_basis:(Format.formatter -> basis -> unit) ->
      pp_element:(Format.formatter -> R.t -> unit) ->
      Format.formatter ->
      t ->
      unit =
   fun ~pp_basis ~pp_element fmtr vec ->
    if M.is_empty vec then Format.fprintf fmtr "∅"
    else
      let bindings = M.bindings vec in
      Format.pp_print_list
        ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";@,")
        (fun fmtr (k, v) ->
          Format.fprintf fmtr "%a ↦ %a" pp_basis k pp_element v)
        fmtr
        bindings

  module Op = struct
    let ( .%[] ) vec x = get vec x

    let ( .%[]<- ) vec x v = set vec x v

    let ( + ) = add

    let ( * ) = smul
  end
end

let%test "sparse_vec" =
  let module M = Map.Make (Coefficient.Z) in
  let module V = Make (Coefficient.Z) (M) in
  let of_list l =
    V.of_list (List.map (fun (x, y) -> (Z.of_int x, Z.of_int y)) l)
  in
  let v1 = of_list [(0, 10); (1, 3); (2, 0)] in
  let v2 = of_list [(0, -10); (1, -3); (2, 0)] in
  let v3 = of_list [(0, -10); (1, -3); (2, 1)] in
  M.is_empty (V.add v1 v2) && not (M.is_empty (V.add v1 v3))
