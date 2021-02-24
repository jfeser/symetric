module type S = sig
  type t [@@deriving compare, hash, sexp]

  type conc

  val top : t

  val bot : t

  val lub : t -> t -> t

  val glb : t -> t -> t

  val contains : t -> conc -> bool
end
