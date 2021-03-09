module Make (A : Abs_intf.S) : sig
  type t [@@deriving compare, hash, sexp]

  include Comparator.S with type t := t

  val to_list : t -> A.t list

  val of_list : A.t list -> t

  val top : t

  val bot : t

  val is_subset : t -> of_:t -> bool

  val lub : t -> t -> t

  val glb : t -> t -> t

  val contains : t -> A.conc -> bool

  val lift : A.t -> t
end
