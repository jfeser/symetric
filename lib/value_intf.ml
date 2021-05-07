module type S = sig
  type params

  type op

  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t

  val eval : params -> op -> t list -> t
end
