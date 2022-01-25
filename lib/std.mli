module Option : sig
  include module type of Option

  val value_lazy : default:'a Lazy.t -> 'a t -> 'a
end

module Iter : sig
  include module type of Iter
  include Sexpable.S1 with type 'a t := 'a t

  val of_set : ('a, _) Base.Set.t -> 'a t
  val to_set : ('a, 'w) Base.Set.comparator -> 'a t -> ('a, 'w) Base.Set.t
  val of_queue : 'a Queue.t -> 'a t
  val of_hashtbl : ('a, 'b) Base.Hashtbl.t -> ('a * 'b) t
  val list_product : 'a t list -> 'a list t
  val top_k : (module Binary_heap.Ordered with type t = 'a) -> int -> 'a t -> 'a t
  val group_by : 'a Base.Hashtbl.Key.t -> ('a * 'b) t -> ('a * 'b list) t
  val min_floor : to_float:('a -> float) -> float -> 'a t -> 'a option
end

module Array : sig
  include module type of Array

  val stddev : float array -> float
  val median : ('a -> 'a -> int) -> 'a t -> 'a
end

module Non_empty_list : sig
  type 'a t

  val of_list : 'a list -> 'a t option
  val of_list_exn : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val of_tuple : 'a * 'a list -> 'a t
  val to_tuple : 'a t -> 'a * 'a list
  val ( @ ) : 'a t -> 'a t -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a list
end

module List : sig
  include module type of List

  val product : int t -> int
  val group_by : 'a Base.Hashtbl.Key.t -> ('a * 'b) t -> ('a * 'b Non_empty_list.t) t
  val take : n:int -> 'a t -> 'a t
  val stats : float list -> float * float * float
end

val ( <. ) : float -> float -> bool
val ( >. ) : float -> float -> bool
val ( <=. ) : float -> float -> bool
val ( >=. ) : float -> float -> bool
