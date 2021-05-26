module type S = sig
  type bench

  module Type : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t
  end

  module Op : Op_intf.S with type type_ := Type.t

  module Value : Value_intf.S with type op := Op.t

  module Bench : sig
    type t = bench [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Value.t

    val solution_exn : t -> Op.t Program.t
  end

  val bench : (Bench.t, Dumb_params.Param.bound) Dumb_params.Param.t

  val spec : Dumb_params.Spec.t
end
