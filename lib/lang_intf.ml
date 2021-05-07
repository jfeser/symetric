module type S = sig
  type bench

  type lparams

  type params = (bench, lparams) Params.t

  module Type : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t
  end

  module Op : Op_intf.S with type type_ := Type.t

  module Value : Value_intf.S with type params := params and type op := Op.t

  module Bench : sig
    type t = bench [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Value.t

    val solution_exn : t -> Op.t Program.t
  end
end
