type 'a t = {
  groups : ('a, 'a list) Hashtbl.t;
  n_queries : int;
  n_samples : int;
  runtime : Time.Span.t;
}

val create_vp :
  'a Base.Hashtbl.Key.t -> float -> ('a -> 'a -> float) -> int -> 'a Iter.t -> 'a t

val create_m :
  'a Base.Hashtbl.Key.t -> float -> ('a -> 'a -> float) -> int -> 'a Iter.t -> 'a t
