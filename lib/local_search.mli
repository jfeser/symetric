module type Value_intf = sig
  type t [@@deriving compare, hash, sexp]
end

val of_unnormalize_tabu :
  ?max_tabu:int ->
  ?random:bool ->
  target_distance:('value -> float) ->
  (module Value_intf with type t = 'op) ->
  (module Value_intf with type t = 'value) ->
  ('op Program.t -> 'op Program.t list) ->
  ('op Program.t -> 'value) ->
  'op Program.t ->
  'op Program.t Std.Iter.t
