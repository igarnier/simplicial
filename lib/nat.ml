type z = Z

type 'a s = S

type _ nat = Zero : z nat | Succ : 'a nat -> 'a s nat

type ex_nat = Ex_nat : 'a nat -> ex_nat

let succ x = Succ x

type ('a, _) vec =
  | Empty : ('a, z) vec
  | Cons : 'a * ('a, 'n) vec -> ('a, 'n s) vec

let tail : type a n. (a, n s) vec -> (a, n) vec =
 fun vec -> match vec with Cons (_, tail) -> tail

let rec get : type a n. n nat -> (a, n s) vec -> a =
 fun n vec ->
  match n with
  | Zero -> (
    match vec with Cons (elt, Empty) -> elt )
  | Succ n -> (
    match vec with Cons (_, tail) -> get n tail )
