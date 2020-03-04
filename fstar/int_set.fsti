module Int_set

open FStar.Int32

assume new type t : eqtype

val empty : t

val add : FStar.Int32.t -> t -> t

val remove : FStar.Int32.t -> t -> t

val fold : (#a: Type) -> (FStar.Int32.t -> a -> a) -> t -> a -> a

val mem : FStar.Int32.t -> t -> bool

val compare : t -> t -> FStar.Int32.t

val eq : t -> t -> bool

val cardinal : t -> FStar.Int32.t

val hash : t -> FStar.Int32.t
