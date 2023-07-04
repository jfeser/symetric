val timed : [< `Add of Time.Span.t ref | `Set of Time.Span.t ref ] -> (unit -> 'a) -> 'a

module type DSL = sig
  module Op : sig
    type t [@@deriving compare, hash, sexp]
  end

  module Value : sig
    type t [@@deriving compare, hash, sexp]

    val eval : Op.t -> t list -> t
  end
end

val memoized_eval :
  ?cache_size_bound:int ->
  (module DSL with type Op.t = 'op and type Value.t = 'value) ->
  'op ->
  'value list ->
  'value
