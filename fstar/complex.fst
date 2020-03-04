module Complex

open FStar.Int32
open Simplex

type cell : int -> Type =
 | Cell :
   #dim: int ->
   uid: FStar.Int32.t ->
   simplex: Simplex.t dim  ->
   incident: list (hash_consed (dim + 1)) ->
   faces: list (hash_consed (dim - 1)) ->
   cell dim

and hash_consed : int -> Type =
  { #dim: int ;
    node: cell dim ;
    uid: FStar.Int32.t ;
    hash: FStar.Int32.t }

type dim_indexed : int -> Type =
  | Zero_dim : table (cell 0) -> 0 dim_indexed
  | Succ_dim :
    #dim:int ->
    table (cell (dim + 1)) ->
    dim_indexed dim ->
    dim_indexed (dim + 1)
