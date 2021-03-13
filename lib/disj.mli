module Make (A : Abs_intf.S) : sig
  type t [@@deriving compare, hash, sexp, quickcheck]

  include Comparator.S with type t := t

  include Lattice_intf.S with type t := t

  val to_list : t -> A.t list

  val of_list : A.t list -> t

  val contains : t -> A.conc -> bool

  val lift : A.t -> t
end
