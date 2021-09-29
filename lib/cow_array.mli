type 'a t [@@deriving compare, equal, hash, sexp]

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> 'a t

val of_array : 'a array -> 'a t

val length : 'a t -> int

val count : 'a t -> f:('a -> bool) -> int

val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t