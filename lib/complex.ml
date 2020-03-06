module Int_map = Map.Make (Int)

module Make (S : Intf.Simplex) = struct
  module Set = struct
    include Set.Make (S)

    let pp fmtr (set : t) =
      let elements = elements set in
      Format.fprintf
        fmtr
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " ;;@.")
           S.pp)
        elements
  end

  module Map = Map.Make (S)

  type simplex = S.t

  type cell = {simplex : S.t; incident_to : Set.t}

  type t = {hasse : cell Map.t; slices : Set.t Int_map.t}

  let empty = {hasse = Map.empty; slices = Int_map.empty}

  let add_cell cell complex =
    let hasse = Map.add cell.simplex cell complex.hasse in
    let slices =
      let set =
        match Int_map.find_opt (S.dim cell.simplex) complex.slices with
        | None ->
            Set.singleton cell.simplex
        | Some set ->
            Set.add cell.simplex set
      in
      Int_map.add (S.dim cell.simplex) set complex.slices
    in
    {hasse; slices}

  let mem simplex {hasse; _} = Map.mem simplex hasse

  let faces s {hasse; _} =
    let cell = Map.find s hasse in
    Set.of_list (S.faces cell.simplex)

  let incidence s {hasse; _} =
    let cell = Map.find s hasse in
    cell.incident_to

  let rec insert_aux : t -> upper_opt:S.t option -> simplex:S.t -> t =
   fun complex ~upper_opt ~simplex ->
    match Map.find simplex complex.hasse with
    | cell -> (
      (* update incidence relation *)
      match upper_opt with
      | Some upper ->
          let incident_to = Set.add upper cell.incident_to in
          let cell = {cell with incident_to} in
          {hasse = Map.add simplex cell complex.hasse; slices = complex.slices}
      | None ->
          complex )
    | exception Not_found ->
        (* Recursively insert faces into abstract simplicial complex *)
        let complex =
          S.fold_faces
            (fun face complex ->
              insert_aux complex ~upper_opt:(Some simplex) ~simplex:face)
            simplex
            complex
        in
        let incident_to =
          match upper_opt with
          | None ->
              Set.empty
          | Some upper ->
              Set.singleton upper
        in
        let cell = {simplex; incident_to} in
        add_cell cell complex

  let insert : S.t -> t -> t =
   fun simplex complex -> insert_aux complex ~upper_opt:None ~simplex

  (* return the set of all subsimplexes of a simplex (including itself) *)
  let set : S.t -> t -> Set.t =
   fun simplex {hasse; _} ->
    let rec loop simplex acc =
      let acc = Set.add simplex acc in
      let cell = Map.find simplex hasse in
      List.fold_right loop (S.faces cell.simplex) acc
    in
    loop simplex Set.empty

  (* return the subcomplex under a simplex (for the natural inclusion order) *)
  let add_lower : t -> S.t -> t -> t =
   fun complex simplex complex' ->
    let rec loop simplex acc =
      let cell = Map.find simplex complex.hasse in
      let acc = add_cell cell acc in
      List.fold_right loop (S.faces cell.simplex) acc
    in
    loop simplex complex'

  let lower simplex complex = add_lower complex simplex empty

  let closure : Set.t -> t -> t =
   fun simplices complex ->
    (* /!\ We share the hash-consing table. *)
    Set.fold (fun simplex acc -> add_lower complex simplex acc) simplices empty

  let star_simplex : t -> S.t -> Set.t =
   fun complex simplex ->
    let rec loop simplex acc =
      let incident_set = incidence simplex complex in
      Set.fold
        (fun incident acc ->
          let acc = Set.add incident acc in
          loop incident acc)
        incident_set
        acc
    in
    loop simplex Set.empty

  let star : Set.t -> t -> Set.t =
   fun simplices map ->
    Set.fold
      (fun simplex acc -> Set.union acc (star_simplex map simplex))
      simplices
      Set.empty

  let slice dim {slices; _} =
    try Int_map.find dim slices with Not_found -> Set.empty

  (* let link : t -> simplex_set -> simplex_set =
   *  fun map simplices ->
   *   Hset.diff
   *     (set_of_simplices map (closure (star simplices)))
   *     (star map (set_of_simplices (closure simplices))) *)

  (* let index l = Array.of_list l |> Array.mapi (fun i elt -> (elt, i)) *)

  let pp fmtr (complex : t) =
    let keys = Map.fold (fun k _cell acc -> k :: acc) complex.hasse [] in
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " ;;@.")
      S.pp
      fmtr
      keys
end
