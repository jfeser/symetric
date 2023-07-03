module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val pp : t Fmt.t
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]
  end
end

module Make (Dsl : DSL) :
  Search_state_intf.S
    with type type_ := Dsl.Type.t
     and type value := Dsl.Value.t
     and type op := Dsl.Op.t
