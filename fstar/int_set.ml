module Int_set = Set.Make (Int)
include Int_set

let eq (x : t) (y : t) = compare x y = 0
