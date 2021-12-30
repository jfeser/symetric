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
  val top_k : cmp:('a -> 'a -> int) -> int -> 'a t -> 'a t
  val group_by : 'a Base.Hashtbl.Key.t -> ('a * 'b) t -> ('a * 'b list) t
  val min_floor : to_float:('a -> float) -> float -> 'a t -> 'a option
end

module Array : sig
  include module type of Array

  val stddev : float array -> float
  val median : ('a -> 'a -> int) -> 'a t -> 'a
end

module List : sig
  include module type of List

  val product : int t -> int
  val group_by : 'a Base.Hashtbl.Key.t -> ('a * 'b) t -> ('a * 'b t) t
end
