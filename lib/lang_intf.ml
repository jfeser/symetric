module type S = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val output : t
    val default : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp, yojson]

    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
    val pp : t Fmt.t
    val default : t
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    val eval : Op.t -> t list -> t
    val distance : t -> t -> float
    val is_error : t -> bool
    val default : t
    val pp : t Fmt.t
  end

  val parse : Sexp.t -> Op.t Program.t
  val serialize : Op.t Program.t -> Sexp.t
end

module type S_with_gen = sig
  include S

  module Gen : sig
    val random_program : Params.t -> int -> (Op.t Program.t * Op.t list) option

    (* val to_bench : Dumb_params.t -> Op.t list -> Op.t Program.t -> Value.t -> Bench.t *)
    val spec : Dumb_params.Spec.t
  end
end
