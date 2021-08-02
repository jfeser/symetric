module type S = sig
  type op

  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val eval : Params.t -> op -> t list -> t

  val dist : Params.t -> t -> t -> float

  val embed : Params.t -> t list -> Torch.Tensor.t
end
