type 'a t = { groups : ('a, 'a list) Hashtbl.t; n_queries : int; n_samples : int }

val create_vp :
  'a Base.Hashtbl.Key.t -> float -> ('a -> 'a -> float) -> int -> 'a Iter.t -> 'a t

val create_m :
  'a Base.Hashtbl.Key.t -> float -> ('a -> 'a -> float) -> int -> 'a Iter.t -> 'a t
