val weighted_random :
  ?state:Random.State.t -> weight:('a -> float) -> int -> 'a list -> 'a list

module Incremental : sig
  type ('e, 's, 'b) t = { add : 'e -> 'b; get_sample : unit -> 's }

  val weighted :
    ?state:Random.State.t -> unit -> ('a, (float * 'a) option, float -> unit) t

  val weighted_reservoir : ?state:Random.State.t -> int -> ('a, 'a list, float -> unit) t
  val reservoir : ?state:Random.State.t -> int -> ('a, 'a list, unit) t

  val reservoir_unique :
    ?state:Random.State.t -> ('a, 'b) Core.Set.comparator -> int -> ('a, 'a list, unit) t
end

val stochastic :
  ?n:int ->
  score:('a -> float) ->
  propose:('a -> 'a) ->
  'a ->
  ('a * float -> unit) ->
  unit
