let log = ref false

let log x =
  let fmtr =
    if !log then Format.err_formatter
    else Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
  in
  Format.fprintf fmtr x
