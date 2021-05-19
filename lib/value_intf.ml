module type S = sig
  type op

  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t

  val eval : Params.t -> op -> t list -> t
end
