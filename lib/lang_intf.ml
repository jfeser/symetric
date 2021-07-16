module type S = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val output : t
  end

  module Op : Op_intf.S with type type_ = Type.t

  module Value : Value_intf.S with type op := Op.t

  module Bench : sig
    type t [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Value.t

    val solution_exn : t -> Op.t Program.t

    val load : string -> t

    val save : string -> t -> unit
  end

  val name : string

  val bench : (Bench.t, Dumb_params.Param.bound) Dumb_params.Param.t

  val spec : Dumb_params.Spec.t
end

module type S_with_features = sig
  include S

  val features : Value.t -> Owl.Arr.arr
  (** Returns an array of features of shape (nfeatures,). *)

  val dist : Params.t -> Owl.Arr.arr -> Value.t -> Value.t -> float
  (** Takes a distance matrix M of shape (nfeatures, nfeatures) and produces a distance. *)

  module Gen : sig
    val random_program : Params.t -> int -> (Op.t Program.t * Op.t list) option

    val to_bench : Dumb_params.t -> Op.t list -> Op.t Program.t -> Value.t -> Bench.t

    val spec : Dumb_params.Spec.t
  end
end

module type S_with_gen = sig
  include S

  module Gen : sig
    val random_program : Params.t -> int -> (Op.t Program.t * Op.t list) option

    val to_bench : Dumb_params.t -> Op.t list -> Op.t Program.t -> Value.t -> Bench.t

    val spec : Dumb_params.Spec.t
  end
end
