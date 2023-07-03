module type S = sig
  type type_
  type t [@@deriving compare, equal, hash, sexp, yojson]

  val cost : t -> int
  val arity : t -> int
  val args_type : t -> type_ list
  val ret_type : t -> type_
  val is_commutative : t -> bool
  val pp : t Fmt.t
  val default : t
end
