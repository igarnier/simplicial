(* Original copyright notice: *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a hashable = {
  eq : 'a -> 'a -> bool ;
  hash : 'a -> int
}

type ('a, 'b) t = {
  t_eq : 'a -> 'a -> bool ;
  t_hash : 'a -> int ;
  mutable size: int;                        (* number of entries *)
  mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
  mutable seed: int;                        (* for randomization *)
  mutable initial_size: int;                (* initial array size *)
}

and ('a, 'b) bucketlist =
    Empty
  | Cons of { mutable key: 'a;
              mutable data: 'b;
              mutable next: ('a, 'b) bucketlist }

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create initial_size (hashable : 'a hashable) =
  let s = power_2_above 16 initial_size in
  let seed = 0 in
  { t_eq = hashable.eq ;
    t_hash = hashable.hash ;
    initial_size = s;
    size = 0;
    seed = seed;
    data = Array.make s Empty }

let ongoing_traversal h =
  Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
  || h.initial_size < 0

let key_index h key =
  (h.t_hash key) land (Array.length h.data - 1)

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let ndata_tail = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
      | Empty -> ()
      | Cons {key; data; next} as cell ->
          let cell =
            if inplace then cell
            else Cons {key; data; next = Empty}
          in
          let nidx = indexfun h key in
          begin match ndata_tail.(nidx) with
          | Empty -> ndata.(nidx) <- cell;
          | Cons tail -> tail.next <- cell;
          end;
          ndata_tail.(nidx) <- cell;
          insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    if inplace then
      for i = 0 to nsize - 1 do
        match ndata_tail.(i) with
        | Empty -> ()
        | Cons tail -> tail.next <- Empty
      done;
  end

let add h key data =
  let i = key_index h key in
  let bucket = Cons{key; data; next=h.data.(i)} in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize key_index h


let rec find_rec_opt h key = function
  | Empty ->
    None
  | Cons{key=k; data; next} ->
    if h.t_eq key k then Some data else find_rec_opt h key next

let find_opt h key =
  match h.data.(key_index h key) with
  | Empty -> None
  | Cons{key=k1; data=d1; next=next1} ->
    if h.t_eq key k1 then Some d1 else
      match next1 with
      | Empty -> None
      | Cons{key=k2; data=d2; next=next2} ->
        if h.t_eq key k2 then Some d2 else
          match next2 with
          | Empty -> None
          | Cons{key=k3; data=d3; next=next3} ->
            if h.t_eq key k3 then Some d3 else find_rec_opt h key next3
