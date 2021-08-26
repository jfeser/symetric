module type S = sig
  type op

  type t [@@deriving compare, equal, hash, sexp_of]

  module Ctx : sig
    type t

    val of_params : Params.t -> t
  end

  include Comparator.S with type t := t

  val eval : Ctx.t -> op -> t list -> t

  val dist : Ctx.t -> t -> t -> float

  val embed : Ctx.t -> t list -> Torch.Tensor.t
end
