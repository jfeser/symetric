module Make_sexp_of (T : sig
  type t [@@deriving compare, hash, sexp_of]
end) : sig
  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t

  val create : T.t -> t

  val value : t -> T.t

  val equal : t -> t -> bool
end

module Make (T : sig
  type t [@@deriving compare, hash, sexp]
end) : sig
  type t [@@deriving compare, hash, sexp]

  include Comparator.S with type t := t

  val create : T.t -> t

  val value : t -> T.t

  val equal : t -> t -> bool
end
