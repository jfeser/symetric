module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val output : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp, yojson]

    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
    val pp : t Fmt.t
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    module Ctx : sig
      type t

      val default : t
    end

    include Comparator.S with type t := t

    val mk_eval_memoized : unit -> Ctx.t -> Op.t -> t list -> t
    val eval : Ctx.t -> Op.t -> t list -> t
    val distance : t -> t -> float
    val is_error : t -> bool
    val pp : t Fmt.t
  end

  val operators : Op.t list
  val parse : Sexp.t -> Op.t Program.t
  val serialize : Op.t Program.t -> Sexp.t
  val rewrite : Op.t Program.t -> Op.t Program.t list
end

val cmd : (module DSL) -> Command.t
