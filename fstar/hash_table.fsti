module Hash_table

  open FStar.Int32
  open FStar.Pervasives
  open FStar.All

  assume new type t 'k 'a : Type

  noeq type hashable 'a = { eq : 'a -> 'a -> bool ; hash : 'a -> FStar.Int32.t }

  val create : #a:Type -> FStar.Int32.t -> hashable 'a -> t 'k 'a

  val add : #k:Type -> #a:Type -> t k a -> k -> a -> unit

  val find_opt : #k:Type -> #a:Type -> t k a -> k -> option a
