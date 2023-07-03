module type DSL = sig
  module Type : sig
    type t
  end

  module Op : sig
    type t

    val arity : t -> int
    val args_type : t -> Type.t list
    val is_commutative : t -> bool
  end

  module Value : sig
    type t

    val eval : Op.t -> t list -> t
    val is_error : t -> bool
  end
end

val generate :
  (module DSL with type Type.t = 'type_ and type Value.t = 'value and type Op.t = 'op) ->
  (cost:int -> type_:'type_ -> 'v Iter.t) ->
  ('v -> 'value) ->
  'op list ->
  int ->
  ('value * 'op * 'v list) Iter.t
