module Not_hash_consed : Intf.Simplex

module Hash_consed (X : sig
  val initial_table_size : int
end) : Intf.Simplex
