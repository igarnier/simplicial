type 'a t =
  | Z_coeff : Z.t t
  | Z2_coeff : Basic_impl.Integer_mod2.t t
  | Q_coeff : Q.t t
  | Float_coeff : float t

let ring : type c. c t -> (module Basic_intf.Ring_std with type t = c) =
 fun coeff ->
  match coeff with
  | Z_coeff -> (module Basic_impl.Integer)
  | Z2_coeff -> (module Basic_impl.Integer_mod2)
  | Q_coeff -> (module Basic_impl.Reals.Rational)
  | Float_coeff -> (module Basic_impl.Reals.Float)
