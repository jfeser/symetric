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

(** Estimate quantiles. Returned quantiles will be within epsilon * N of the
    true quantiles with probability 1 - delta. *)
module Quantile_estimator : sig
  type 'a t [@@deriving yojson_of]

  val create :
    ?epsilon:float ->
    ?delta:float ->
    ?quantiles:float list ->
    default:'a ->
    (module Comparable.S with type t = 'a) ->
    'a t

  val add : 'a t -> 'a -> unit
  val quantiles : 'a t -> 'a list
end
