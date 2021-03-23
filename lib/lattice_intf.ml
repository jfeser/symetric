open Base_quickcheck

module type S = sig
  type t [@@deriving compare, hash, sexp, quickcheck]

  val quickcheck_generator_leq : (t -> t Generator.t) option

  val top : t

  val bot : t

  val leq : t -> t -> bool

  val lub : t -> t -> t

  val glb : t -> t -> t
end
