open Hashcons
module Hashed_abstract_simplex = Make (Abstract_simplex)

type simplex = Abstract_simplex.t hash_consed

module Set = Set.Make (struct
  type t = simplex

  let compare (x : t) (y : t) = Int.compare x.tag y.tag
end)

let pp fmtr (simplex : simplex) =
  match simplex.node with
  | [] ->
      Format.fprintf fmtr "∅"
  | _ ->
      Format.fprintf
        fmtr
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";")
           Format.pp_print_int)
        simplex.node

let pp_set fmtr (set : Set.t) =
  let elements = Set.elements set in
  Format.fprintf
    fmtr
    "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " ;;@.")
       pp)
    elements

type 'a cell = {
  simplex : int list;
  incident_to : Set.t;
  faces : Set.t;
  data : 'a option;
}

module type S = sig
  type t

  type data

  val of_vertices : int list -> simplex

  val empty : t

  val mem : simplex -> t -> bool

  val faces : simplex -> t -> Set.t

  val incidence : simplex -> t -> Set.t

  val data : simplex -> t -> data option

  val attach : simplex -> data -> t -> t

  val insert : simplex -> t -> t

  val set : simplex -> t -> Set.t

  val lower : simplex -> t -> t

  val closure : Set.t -> t -> t

  val star : Set.t -> t -> Set.t

  val pp : Format.formatter -> t -> unit
end

module Make (X : sig
  type data

  val table_size : int
end) =
struct
  type table = Hashed_abstract_simplex.t

  let table = Hashed_abstract_simplex.create X.table_size

  let of_vertices (l : int list) : simplex =
    Hashed_abstract_simplex.hashcons table (Abstract_simplex.of_list l)

  type t = (Abstract_simplex.t, X.data cell) Hmap.t

  type data = X.data

  let empty = Hmap.empty

  let mem = Hmap.mem

  let faces s map =
    let cell = Hmap.find s map in
    cell.faces

  let incidence s map =
    let cell = Hmap.find s map in
    cell.incident_to

  let data s map =
    let cell = Hmap.find s map in
    cell.data

  let attach simplex data map =
    let cell = Hmap.find simplex map in
    let cell = {cell with data = Some data} in
    Hmap.add simplex cell map

  let rec insert_aux :
      table -> t -> upper_opt:simplex option -> simplex:simplex -> t =
   fun table map ~upper_opt ~simplex ->
    match Hmap.find simplex map with
    | cell -> (
      (* update incidence relation *)
      match upper_opt with
      | Some upper ->
          let incident_to = Set.add upper cell.incident_to in
          let cell = {cell with incident_to} in
          Hmap.add simplex cell map
      | None ->
          map )
    | exception Not_found ->
        (* Recursively insert faces into abstract simplicial complex *)
        let (faces, map) =
          Abstract_simplex.fold_faces
            (fun face (faces, map) ->
              let hashed_face = Hashed_abstract_simplex.hashcons table face in
              let map =
                insert_aux
                  table
                  map
                  ~upper_opt:(Some simplex)
                  ~simplex:hashed_face
              in
              (Set.add hashed_face faces, map))
            simplex.node
            (Set.empty, map)
        in
        let incident_to =
          match upper_opt with
          | None ->
              Set.empty
          | Some upper ->
              Set.singleton upper
        in
        let cell = {simplex = simplex.node; incident_to; faces; data = None} in
        Hmap.add simplex cell map

  let insert : simplex -> t -> t =
   fun simplex map -> insert_aux table map ~upper_opt:None ~simplex

  (* return the set of all subsimplexes of a simplex (including itself) *)
  let set : simplex -> t -> Set.t =
   fun simplex map ->
    let rec loop simplex acc =
      let acc = Set.add simplex acc in
      let cell = Hmap.find simplex map in
      Set.fold loop cell.faces acc
    in
    loop simplex Set.empty

  (* return the subcomplex under a simplex (for the natural inclusion order) *)
  let add_lower : t -> simplex -> t -> t =
   fun map simplex map' ->
    let rec loop simplex acc =
      let cell = Hmap.find simplex map in
      let acc = Hmap.add simplex cell acc in
      Set.fold loop cell.faces acc
    in
    loop simplex map'

  let lower simplex map = add_lower map simplex Hmap.empty

  let closure : Set.t -> t -> t =
   fun simplices map ->
    Set.fold
      (fun simplex acc -> add_lower map simplex acc)
      simplices
      Hmap.empty

  let star_simplex : t -> simplex -> Set.t =
   fun map simplex ->
    let rec loop simplex acc =
      let incident_set = incidence simplex map in
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

  (* let link : t -> simplex_set -> simplex_set =
   *  fun map simplices ->
   *   Hset.diff
   *     (set_of_simplices map (closure (star simplices)))
   *     (star map (set_of_simplices (closure simplices))) *)

  let _iter f =
    Hashed_abstract_simplex.iter (fun hash_consed -> f hash_consed.node) table

  let pp fmtr (map : t) =
    let keys = Hmap.fold (fun k _cell acc -> k :: acc) map [] in
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " ;;@.")
      pp
      fmtr
      keys
end
