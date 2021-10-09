module Make (R : Basic_intf.Ring_std) : Intf_simplicial.Mat with module R = R =
struct
  module Int_map = Map.Make (struct
    type t = int

    (* Overflows should not occur as these integers correspond
       to column/row indices. *)
    let compare (x : int) (y : int) = x - y
  end)

  module R = R
  module Col = Sparse_vec.Make (R) (Int_map)
  module Row = Sparse_vec.Make (R) (Int_map)

  type t = { cols : int; rows : int; data : Col.t Int_map.t }

  exception Out_of_bounds

  exception Dimensions_mismatch

  let create ~rows ~cols = { cols; rows; data = Int_map.empty }

  let cols { cols; _ } = cols

  let rows { rows; _ } = rows

  (* get_col_*/set_col_* *)
  (* Note that dimensions are not checked for these low-level functions *)

  let get_col_exn column (m : t) : Col.t = Int_map.find column m.data

  let get_col_opt column (m : t) : Col.t option = Int_map.find_opt column m.data

  let get_col column (m : t) : Col.t =
    if not (column >= 0 && column < m.cols) then raise Out_of_bounds ;
    match Int_map.find_opt column m.data with Some c -> c | None -> Col.zero

  let set_col column (c : Col.t) (m : t) : t =
    if not (column >= 0 && column < m.cols) then raise Out_of_bounds ;
    (* /!\ Dimensions of column are not checked! *)
    if Col.is_empty c then { m with data = Int_map.remove column m.data }
    else { m with data = Int_map.add column c m.data }

  (* get_col_elt/set_col_elt*)

  (* let get_col_elt_exn row (c : Col.t) : R.t = Int_map.find row c
   *
   * let set_col_elt col (c : Col.t) elt : Col.t = Col.set c col elt *)

  (* get_row/set_row*)

  let get_row : int -> t -> Row.t =
   fun row m ->
    if not (row >= 0 && row < m.rows) then raise Out_of_bounds ;
    Int_map.fold
      (fun col c acc ->
        match Col.get_opt c row with
        | None -> acc
        | Some elt -> Row.set acc col elt)
      m.data
      Int_map.empty

  let set_row row (r : Row.t) (m : t) : t =
    if not (row >= 0 && row < m.rows) then raise Out_of_bounds ;
    let data =
      Int_map.merge
        (fun c v_opt col_opt ->
          match (v_opt, col_opt) with
          | (None, None) -> None
          | (Some v, None) -> Some (Int_map.singleton c v)
          | (None, Some col) -> Some (Col.set col row R.zero)
          | (Some v, Some col) -> Some (Col.set col row v))
        r
        m.data
    in
    { m with data }

  (* let m' = Int_map.mapi (fun c v -> Int_map.singleton c v) r in
   * { m with
   *   data =
   *     Int_map.union (fun _c col1 col2 -> Some (Col.add col1 col2)) m' m.data
   * } *)

  (* Row.fold
   *   (fun column elt mat ->
   *     let c = get_col column mat in
   *     let c = Col.set c row elt in
   *     set_col column c mat)
   *   r
   *   m *)

  let%test "get_row/set_row" =
    let m = create ~rows:3 ~cols:3 in
    let r = Row.set Row.zero 1 R.one in
    let m = set_row 1 r m in
    let c0 = get_col_opt 0 m in
    let c1 = get_col_exn 1 m in
    let c2 = get_col_opt 2 m in
    Option.is_none c0 && Option.is_none c2
    && R.compare (Col.get c1 0) R.zero = 0
    && R.compare (Col.get c1 1) R.one = 0
    && R.compare (Col.get c1 2) R.zero = 0

  (* get_elt/set_elt*)

  let get_exn : row:int -> col:int -> t -> 'a =
   fun ~row ~col m ->
    let c = get_col_exn col m in
    Col.get_exn c row

  let get_opt : row:int -> col:int -> t -> R.t option =
   fun ~row ~col m -> try Some (get_exn ~row ~col m) with Not_found -> None

  let get : row:int -> col:int -> t -> R.t =
   fun ~row ~col m ->
    if row < 0 || row >= m.rows then raise Out_of_bounds ;
    if col < 0 || col >= m.cols then raise Out_of_bounds ;
    try get_exn ~row ~col m with Not_found -> R.zero

  let set : row:int -> col:int -> R.t -> t -> t =
   fun ~row ~col elt m ->
    if row < 0 || row >= m.rows then raise Out_of_bounds ;
    if col < 0 || col >= m.cols then raise Out_of_bounds ;
    let c = get_col col m in
    let c = Col.set c row elt in
    set_col col c m

  let column_is_empty column (m : t) : bool = not (Int_map.mem column m.data)

  let row_is_empty row (m : t) : bool =
    Int_map.for_all (fun _ c -> not (Int_map.mem row c)) m.data

  let is_empty (m : t) : bool = Int_map.is_empty m.data

  let submatrix : row:int -> col:int -> t -> t =
   fun ~row ~col m ->
    if row < 0 || row >= m.rows then raise Out_of_bounds ;
    if col < 0 || col >= m.cols then raise Out_of_bounds ;
    let cols = Int_map.remove col m.data in
    let data = Int_map.map (fun col -> Int_map.remove row col) cols in
    { data; cols = m.cols - 1; rows = m.rows - 1 }

  let equal : t -> t -> bool =
   fun m1 m2 ->
    if (not (Int.equal m1.cols m2.cols)) || not (Int.equal m1.rows m2.rows) then
      false
    else
      Int_map.fold
        (fun coli col acc ->
          match get_col_opt coli m2 with
          | None -> false
          | Some col' ->
              Col.fold
                (fun rowi elt acc ->
                  match Col.get_opt col' rowi with
                  | None -> false
                  | Some elt' -> acc && R.compare elt elt' = 0)
                col
                acc)
        m1.data
        true

  let%test "submatrix" =
    let m = create ~rows:3 ~cols:3 in
    let m = set ~row:1 ~col:1 R.one m in
    let m' = submatrix ~row:1 ~col:1 m in
    let n = create ~rows:2 ~cols:2 in
    equal m' n

  let%test "test_get_set" =
    let matrix = create ~rows:3 ~cols:3 in
    let rec loop mat n =
      if n = 0 then true
      else
        let r = Random.int 3 in
        let c = Random.int 3 in
        let v = R.one in
        let m = set ~row:r ~col:c v mat in
        let v' = get_exn ~row:r ~col:c m in
        R.compare v v' = 0 && loop m (n - 1)
    in
    loop matrix 30

  let swap_rows row1 row2 (m : t) : t =
    { m with data = Int_map.map (fun col -> Col.swap col row1 row2) m.data }

  let%test "test_swap_rows" =
    let m = create ~rows:3 ~cols:3 in
    let m = set ~row:1 ~col:0 R.one m in
    let m = swap_rows 0 1 m in
    R.compare (get ~row:0 ~col:0 m) R.one = 0
    && R.compare (get ~row:1 ~col:0 m) R.one <> 0

  let swap_cols col1 col2 (m : t) : t =
    match (get_col_opt col1 m, get_col_opt col2 m) with
    | (None, None) -> m
    | (None, Some c2) ->
        let data = m.data in
        let data = Int_map.remove col2 data in
        let data = Int_map.add col1 c2 data in
        { m with data }
    | (Some c1, None) ->
        let data = m.data in
        let data = Int_map.remove col1 data in
        let data = Int_map.add col2 c1 data in
        { m with data }
    | (Some c1, Some c2) ->
        let data = m.data in
        let data = Int_map.add col1 c2 data in
        let data = Int_map.add col2 c1 data in
        { m with data }

  let%test "test_swap_cols" =
    let m = create ~rows:3 ~cols:3 in
    let m = set ~row:0 ~col:1 R.one m in
    let m = swap_cols 0 1 m in
    R.compare (get ~row:0 ~col:0 m) R.one = 0
    && R.compare (get ~row:0 ~col:1 m) R.one <> 0

  let fold_cols : (int -> Col.t -> 'b -> 'b) -> t -> 'b -> 'b =
   fun f { data; _ } acc -> Int_map.fold f data acc

  let find_map_cols : (int -> Col.t -> 'res option) -> t -> 'res option =
    fun (type res) f { data; _ } ->
     let exception Return of res in
     try
       Int_map.iter
         (fun c col ->
           match f c col with None -> () | Some res -> raise (Return res))
         data ;
       None
     with Return res -> Some res

  let integers n = List.init n (fun i -> i)

  let identity n =
    let empty = { cols = n; rows = n; data = Int_map.empty } in
    List.fold_left (fun acc i -> set ~row:i ~col:i R.one acc) empty (integers n)

  let mul (m1 : t) (m2 : t) : t =
    if m1.cols <> m2.rows then raise Dimensions_mismatch ;
    let rows = m1.rows in
    let cols = m2.cols in
    let empty = { cols; rows; data = Int_map.empty } in
    Int_map.fold
      (fun col2 c2 acc ->
        if column_is_empty col2 m2 then acc
        else
          let rows = integers rows in
          List.fold_left
            (fun acc row1 ->
              if row_is_empty row1 m1 then acc
              else
                let res =
                  Int_map.fold
                    (fun row2 elt2 acc ->
                      match get_opt ~row:row1 ~col:row2 m1 with
                      | None -> acc
                      | Some elt1 -> R.add (R.mul elt1 elt2) acc)
                    c2
                    R.zero
                in
                if R.compare res R.zero = 0 then acc
                else set ~row:row1 ~col:col2 res acc)
            acc
            rows)
      m2.data
      empty

  let%test "mul_identity" =
    let m = identity 5 in
    let p = mul m m in
    equal m p

  let to_string_matrix : t -> string array array =
   fun m ->
    Array.init m.cols (fun c ->
        let col = get_col c m in
        Array.init m.rows (fun r -> Format.asprintf "%a" R.pp (Col.get col r)))

  let pp : Format.formatter -> t -> unit =
   fun fmtr m ->
    let dense = to_string_matrix m in
    let lengths = Array.map (Array.map String.length) dense in
    let max_lengths = Array.map (Array.fold_left max min_int) lengths in
    Format.pp_open_tbox fmtr () ;
    (* print header *)
    Format.pp_set_tab fmtr () ;
    Format.fprintf fmtr "   " ;
    for c = 0 to m.cols - 1 do
      Format.pp_set_tab fmtr () ;
      let spaces = String.make max_lengths.(c) ' ' in
      Format.fprintf fmtr "%d%s" c spaces
    done ;
    Format.pp_print_tab fmtr () ;
    Format.fprintf fmtr "   " ;
    for c = 0 to m.cols - 1 do
      Format.pp_print_tab fmtr () ;
      let spaces = String.make (max_lengths.(c) + 1) '-' in
      Format.fprintf fmtr "%s" spaces
    done ;
    for r = 0 to m.rows - 1 do
      Format.pp_print_tab fmtr () ;
      Format.fprintf fmtr "%d" r ;
      for c = 0 to m.cols - 1 do
        match get_opt ~row:r ~col:c m with
        | None ->
            Format.pp_print_tab fmtr () ;
            let spaces = String.make max_lengths.(c) ' ' in
            Format.fprintf fmtr "%s" spaces
        | Some elt ->
            Format.pp_print_tab fmtr () ;
            let selt = Format.asprintf "%a" R.pp elt in
            let spc = max_lengths.(c) - String.length selt in
            let spaces = String.make spc ' ' in
            Format.fprintf fmtr "%s%a" spaces R.pp elt
      done
    done ;
    Format.pp_close_tbox fmtr ()

  module Op = struct
    let ( .%{} ) m (r, c) = get ~row:r ~col:c m

    let ( .%{}<- ) m (r, c) v = set ~row:r ~col:c v m

    let ( .%|{} ) m c = get_col c m

    let ( .%|{}<- ) m c col = set_col c col m

    let ( .%-{} ) m r = get_row r m

    let ( .%-{}<- ) m r row = set_row r row m

    (* ocamlformat doesn't like multi-index operators yet *)
    (* let (.%[;..]) m r c = get ~row:r ~col:c m
     * let (.%[;..]<-) m r c v = set ~row:r ~col:c m v *)
  end
end

module Z = Make (Basic_impl.Integer)
module Z2 = Make (Basic_impl.Integer_mod2)
module Q = Make (Basic_impl.Reals.Rational)
module Float = Make (Basic_impl.Reals.Float)

type 'a t =
  | Z_mat : Z.t -> Basic_impl.Integer.t t
  | Z2_mat : Z2.t -> Basic_impl.Integer_mod2.t t
  | Q_mat : Q.t -> Basic_impl.Reals.Rational.t t
  | Float_mat : Float.t -> Basic_impl.Reals.Float.t t

let pp : type c. Format.formatter -> c t -> unit =
 fun fmtr mat ->
  match mat with
  | Z_mat m -> Z.pp fmtr m
  | Z2_mat m -> Z2.pp fmtr m
  | Q_mat m -> Q.pp fmtr m
  | Float_mat m -> Float.pp fmtr m
