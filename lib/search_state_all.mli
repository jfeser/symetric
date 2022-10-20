module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val output : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val pp : t Fmt.t
    val is_commutative : t -> bool
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
  end
end

module Make (Lang : Lang_intf) :
  Search_state_intf.S
    with type type_ := Lang.Type.t
     and type value := Lang.Value.t
     and type op := Lang.Op.t
