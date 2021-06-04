module Make (T : sig
  type t [@@deriving compare, hash, sexp]
end) : sig
  type t [@@deriving compare, hash, sexp]

  val create : T.t -> t

  val value : t -> T.t

  val equal : t -> t -> bool
end
