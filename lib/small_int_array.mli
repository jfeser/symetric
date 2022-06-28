type t [@@deriving compare, equal, hash, sexp]

val get : t -> int -> int
val set : t -> int -> int -> t
