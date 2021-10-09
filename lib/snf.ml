type matrix_elt = { elt : Z.t; row : int; col : int }

type mat_loc = Row of int | Col of int

type where = Starting_from of int | At of mat_loc list

(* ------------------------------------------------------------------------- *)
(* Finding elements not divided by [divider] *)

module Mat = Sparse_matrix.Z

let find_elt_not_divided_in_col :
    m:Mat.t -> div:Z.t -> c:int -> (matrix_elt * Z.t) option =
 fun ~m ~div ~c ->
  let col = Mat.get_col c m in
  Mat.Col.find_map
    (fun rowi elt ->
      let (q, rem) = Z.div_rem elt div in
      if Z.equal rem Z.zero then None else Some ({ elt; row = rowi; col = c }, q))
    col

let find_elt_not_divided_in_row :
    m:Mat.t -> div:Z.t -> r:int -> (matrix_elt * Z.t) option =
 fun ~m ~div ~r ->
  let rowv = Mat.get_row r m in
  Mat.Row.find_map
    (fun coli elt ->
      let (q, rem) = Z.div_rem elt div in
      if Z.equal rem Z.zero then None else Some ({ elt; row = r; col = coli }, q))
    rowv

let find_elt_not_divided_loop ~m ~div ~loc_list =
  List.find_map
    (function
      | Row i -> find_elt_not_divided_in_row ~m ~div ~r:i
      | Col i -> find_elt_not_divided_in_col ~m ~div ~c:i)
    loc_list

let find_elt_not_divided :
    m:Mat.t -> div:Z.t -> where:where -> (matrix_elt * Z.t) option =
 fun ~m ~div ~where ->
  match where with
  | Starting_from start ->
      Mat.find_map_cols
        (fun c _ ->
          if c < start then None else find_elt_not_divided_in_col ~m ~div ~c)
        m
  | At loc_list -> find_elt_not_divided_loop ~m ~div ~loc_list

(* ------------------------------------------------------------------------- *)
(* Finding minimal elements *)

let find_min_elt_in_col : m:Mat.t -> c:int -> min:matrix_elt -> matrix_elt =
 fun ~m ~c ~min ->
  let col = Mat.get_col c m in
  Mat.Col.fold
    (fun rowi elt min ->
      if Z.gt (Z.abs elt) (Z.abs min.elt) then min
      else { elt; row = rowi; col = c })
    col
    min

let find_min_elt_in_row : m:Mat.t -> r:int -> min:matrix_elt -> matrix_elt =
 fun ~m ~r ~min ->
  let rowv = Mat.get_row r m in
  Mat.Row.fold
    (fun coli elt min ->
      if Z.gt (Z.abs elt) (Z.abs min.elt) then min
      else { elt; row = r; col = coli })
    rowv
    min

let find_min_elt_all : m:Mat.t -> start:int -> matrix_elt option =
 fun ~m ~start ->
  Mat.fold_cols
    (fun coli col acc ->
      if coli < start then acc
      else
        Mat.Col.fold
          (fun rowi elt acc ->
            if rowi < start then acc
            else
              match acc with
              | None -> Some { elt; row = rowi; col = coli }
              | Some { elt = min; _ } ->
                  if Z.Compare.(Z.abs elt >= Z.abs min) then acc
                  else Some { elt; row = rowi; col = coli })
          col
          acc)
    m
    None

let find_min_elt : m:Mat.t -> min:matrix_elt -> where:where -> matrix_elt =
 fun ~m ~min ~where ->
  match where with
  | Starting_from start ->
      Option.value ~default:min (find_min_elt_all ~m ~start)
  | At loc_list ->
      List.fold_left
        (fun min loc ->
          match loc with
          | Row i -> find_min_elt_in_row ~m ~r:i ~min
          | Col i -> find_min_elt_in_col ~m ~c:i ~min)
        min
        loc_list

let pp_min fmtr { elt; row; col } =
  Format.fprintf fmtr "m[%d,%d] = %a" row col Z.pp_print elt

let rec smith_loop m min index =
  (* Search element not divided by min_elt in its column *)
  let () =
    Log.log "search elt not divided by %a in col %d@." pp_min min min.col
  in
  let res = find_elt_not_divided_in_col ~m ~div:min.elt ~c:min.col in
  match res with
  | Some (target, ratio) ->
      let () =
        Log.log
          "found elt not divided by %a in col %d: %a (ratio = %a)@."
          pp_min
          min
          min.col
          pp_min
          target
          Z.pp_print
          ratio
      in
      let open Mat.Op in
      let open Mat.Row.Op in
      let () =
        Log.log
          "m.-{%d} <- m.-{%d} - (%a * m.-{%d}) = %a@."
          target.row
          target.row
          Z.pp_print
          ratio
          min.row
          (Mat.Row.pp ~pp_basis:Format.pp_print_int ~pp_element:Z.pp_print)
          (m.%-{target.row} + (Z.neg ratio * m.%-{min.row}))
      in
      let m =
        m.%-{target.row} <- m.%-{target.row} + (Z.neg ratio * m.%-{min.row})
      in
      let () = Log.log "%a@." Mat.pp m in
      let min = find_min_elt ~m ~min ~where:(At [Row target.row]) in
      smith_loop m min index
  | None -> (
      let () =
        Log.log "found no element divided by %a in col %d@." pp_min min min.col
      in
      let () =
        Log.log "search elt not divided by %a in row %d@." pp_min min min.row
      in
      let res = find_elt_not_divided_in_row ~m ~div:min.elt ~r:min.row in
      match res with
      | Some (target, ratio) ->
          let () =
            Log.log
              "found elt not divided by %a in row %d: %a@."
              pp_min
              min
              min.row
              pp_min
              target
          in
          let open Mat.Op in
          let open Mat.Col.Op in
          let m =
            m.%|{target.col} <- m.%|{target.col} + (Z.neg ratio * m.%|{min.col})
          in
          let min = find_min_elt ~m ~min ~where:(At [Col target.col]) in
          smith_loop m min index
      | None -> (
          let () =
            Log.log
              "found no element divided by %a in row %d@."
              pp_min
              min
              min.row
          in
          let () =
            Log.log
              "searching element divided not by %a in submatrix >= %d@."
              pp_min
              min
              index
          in
          let res =
            find_elt_not_divided ~m ~div:min.elt ~where:(Starting_from index)
          in
          match res with
          | Some (target, _ratio) ->
              let () =
                Log.log
                  "found element not divided by %a in submatrix >= %d: %a@."
                  pp_min
                  min
                  index
                  pp_min
                  target
              in
              let open Mat.Op in
              let open Mat.Col.Op in
              let cmin = m.%|{min.col} in
              let ctgt = m.%|{target.col} in
              let x = ctgt.%[min.row] in
              assert (Z.equal (Z.rem x min.elt) Z.zero) ;
              let r = Z.divexact x min.elt in
              let m = m.%|{target.col} <- ctgt + (Z.neg r * cmin) in
              Log.log "first simplification:@.%a@." Mat.pp m ;
              assert (Z.equal m.%{(target.col, min.row)} Z.zero) ;
              let m = m.%|{min.col} <- m.%|{min.col} + m.%|{target.col} in
              Log.log "second simplification:@.%a@." Mat.pp m ;
              let min =
                find_min_elt ~m ~min ~where:(At [Col min.col; Col target.col])
              in
              let () =
                Log.log "result:@.%a@." Mat.pp m ;
                Log.log "min: %a@." pp_min min
              in
              smith_loop m min index
          | None ->
              let () =
                Log.log
                  "Found no element dividing %a in submatrix >= %d@."
                  pp_min
                  min
                  index
              in
              (m, min)))

let simplify m min =
  let open Mat.Op in
  let col = m.%|{min.col} in
  let row = m.%-{min.row} in
  let m =
    Mat.Col.fold
      (fun i v m ->
        if i = min.row then m
        else
          let open Mat.Row.Op in
          let q = Z.divexact v min.elt in
          m.%-{i} <- m.%-{i} + (Z.neg q * row))
      col
      m
  in
  let col = m.%|{min.col} in
  Mat.Row.fold
    (fun i v m ->
      if i = min.col then m
      else
        let open Mat.Col.Op in
        let q = Z.divexact v min.elt in
        m.%|{i} <- m.%|{i} + (Z.neg q * col))
    row
    m

let smith_normal_form : Sparse_matrix.Z.t -> Sparse_matrix.Z.t =
 fun m ->
  let rows = Mat.rows m in
  let cols = Mat.cols m in
  let dim = Stdlib.min rows cols in
  let min = find_min_elt_all ~m ~start:0 in
  match min with
  | None -> (* matrix is zero *) m
  | Some min ->
      let rec iter i min (m : Mat.t) =
        if i >= dim then m
        else
          let open Mat.Op in
          Log.log "ITERATION %d@." i ;
          Log.log "%a@." Mat.pp m ;
          (* invariant: submatrix nonzero *)
          let (m, min) = smith_loop m min i in
          Log.log "After smith (min = %a)@." pp_min min ;
          Log.log "%a@." Mat.pp m ;
          let m = simplify m min in
          Log.log "After simpl:@." ;
          Log.log "%a@." Mat.pp m ;
          let m = Mat.swap_cols i min.col m in
          let m = Mat.swap_rows i min.row m in
          let m = m.%{(i, i)} <- Z.abs m.%{(i, i)} in
          Log.log "After swap:@." ;
          Log.log "%a@." Mat.pp m ;
          let i = i + 1 in
          let min = find_min_elt_all ~m ~start:i in
          match min with None -> m | Some min -> iter i min m
      in
      iter 0 min m
