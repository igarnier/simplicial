module Z : Intf.Ring with type t = Z.t = struct
  include Z

  let pp = pp_print
end

module Z2 : Intf.Field = struct
  type t = Zero | One

  let zero = Zero

  let one = One

  let add (x : t) (y : t) =
    match (x, y) with
    | (Zero, Zero) -> Zero
    | (Zero, One) | (One, Zero) -> One
    | (One, One) -> Zero

  let neg (x : t) = x

  let mul (x : t) (y : t) =
    match (x, y) with (Zero, _) | (_, Zero) -> Zero | (One, One) -> One

  let div (x : t) (y : t) =
    match y with Zero -> Stdlib.failwith "Z2: division by zero" | _ -> x

  let hash (x : t) = match x with Zero -> 0 | One -> 1

  let pp fmtr (x : t) =
    match x with
    | Zero -> Format.pp_print_string fmtr "zero"
    | One -> Format.pp_print_string fmtr "one"

  let equal (x : t) (y : t) =
    match (x, y) with (Zero, Zero) -> true | (One, One) -> true | _ -> false

  let compare (x : t) (y : t) =
    match (x, y) with
    | (Zero, Zero) | (One, One) -> 0
    | (Zero, One) -> -1
    | (One, Zero) -> 1
end

module Q : Intf.Field with type t = Q.t = struct
  include Q

  let hash ({ num; den } : Q.t) = Hashtbl.hash (Z.hash num, Z.hash den)

  let pp = Q.pp_print
end

module Float : Intf.Field with type t = float = struct
  include Float

  let hash (x : float) = Hashtbl.hash x

  let pp = Format.pp_print_float
end

type 'a t =
  | Z_coeff : Z.t t
  | Z2_coeff : Z2.t t
  | Q_coeff : Q.t t
  | Float_coeff : float t

let ring : type c. c t -> (module Intf.Ring with type t = c) =
 fun coeff ->
  match coeff with
  | Z_coeff -> (module Z)
  | Z2_coeff -> (module Z2)
  | Q_coeff -> (module Q)
  | Float_coeff -> (module Float)
