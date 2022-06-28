type t [@@deriving compare, equal, hash, sexp]

val init : int -> f:(int -> int) -> t
val get : t -> int -> int
val set : t -> int -> int -> t
val set_many : t -> (int * int) Iter.t -> t
