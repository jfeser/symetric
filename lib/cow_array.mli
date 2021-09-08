type 'a t [@@deriving compare, equal, hash, sexp]

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> 'a t

val of_array : 'a array -> 'a t
