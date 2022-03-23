type 'a t = { groups : ('a, 'a list) Hashtbl.t; n_queries : int }

val create_vp : 'a Base.Hashtbl.Key.t -> float -> ('a -> 'a -> float) -> 'a Iter.t -> 'a t
