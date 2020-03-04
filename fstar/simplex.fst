module Simplex

open Int_set

type t : int -> Type = | Set : #n:int -> Int_set.t -> t n

val empty : t (-1)
let empty = Set Int_set.empty

val cone : #n:int -> FStar.Int32.t -> t n -> option (t (n + 1))
let cone #n i (Set points) =
  if Int_set.mem i points then
    None
  else
    Some (Set (Int_set.add i points))

val fold_faces : #a:Type -> #n:int -> (t n -> a -> a) -> t (n + 1) -> a -> a
let fold_faces #a #n f (Set points) (acc : a) =
  Int_set.fold (fun elt acc ->
    let face : t n = Set (Int_set.remove elt points) in
    f face acc
  ) points acc

val hash : #n:int -> t n -> FStar.Int32.t
let hash #n (Set points) =
  Int_set.hash points
