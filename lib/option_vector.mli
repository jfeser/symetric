type 'a t

val create : int -> 'a t
val reserve : 'a t -> int -> unit
val get_some_exn : 'a t -> int -> 'a
val set_some : 'a t -> int -> 'a -> unit
val get : 'a t -> int -> 'a option
