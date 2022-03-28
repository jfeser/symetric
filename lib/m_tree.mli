type 'a t

val empty : ?capacity:int -> ('a -> 'a -> int) -> ('a -> 'a -> float) -> 'a t
val insert : 'a t -> 'a -> unit
val range : 'a t -> 'a -> float -> 'a Iter.t
