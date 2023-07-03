module type S = sig
  type op
  type t [@@deriving compare, equal, hash, sexp]

  module Ctx : sig
    type t

    val of_params : Params.t -> t
  end

  include Comparator.S with type t := t

  val eval : Ctx.t -> op -> t list -> t
  val dist : Ctx.t -> t -> t -> float
  val is_error : t -> bool
  val default : t
end
