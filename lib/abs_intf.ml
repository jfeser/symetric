module type S = sig
  type t [@@deriving compare, hash, sexp]

  type conc [@@deriving sexp]

  val top : t

  val bot : t

  val lub : t -> t -> t

  val glb : t -> t -> t

  val is_subset : t -> of_:t -> bool

  val contains : t -> conc -> bool
end
