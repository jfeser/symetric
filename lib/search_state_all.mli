module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]
  end

  module Op : sig
    type t [@@deriving compare, hash, sexp]

    val cost : t -> int

    val arity : t -> int

    val args_type : t -> Type.t list

    val ret_type : t -> Type.t
  end

  module Value : sig
    type t [@@deriving compare, hash, sexp]
  end
end

module Make (Lang : Lang_intf) : sig
  open Lang

  module Attr : sig
    type t = { cost : int; type_ : Type.t } [@@deriving compare, hash, sexp]

    val create : int -> Type.t -> t
  end

  module TValue : sig
    type t = { type_ : Type.t; value : Value.t } [@@deriving compare, hash, sexp]
  end

  type t = {
    values : Value.t Base.Queue.t Core.Hashtbl.M(Attr).t;
    paths : (int * Op.t * Value.t list) Base.Queue.t Core.Hashtbl.M(TValue).t;
  }
  [@@deriving sexp]

  val create : unit -> t

  val search : t -> cost:int -> type_:Type.t -> Value.t list

  val find_term : t -> Op.t Program.t -> (Op.t * Value.t list) Program.t

  val mem : t -> TValue.t -> bool

  val insert : t -> int -> Value.t -> Op.t -> Value.t list -> unit

  val insert_groups : t -> int -> (Value.t * Op.t * Value.t list) list list -> unit

  val states : ?cost:int -> ?type_:Type.t -> t -> Value.t Iter.t

  val length : t -> int

  val print_stats : t -> unit

  val print_contents : t -> unit

  val program_exn : t -> Type.t -> Value.t -> Op.t Program.t

  val program_of_op_args_exn : t -> Op.t -> Value.t list -> Op.t Program.t

  val random_program_of_op_args_exn : t -> Op.t -> Value.t list -> Op.t Program.t

  val random_program_exn : ?max_cost:int -> t -> Type.t -> Value.t -> Op.t Program.t

  val clear : t -> unit

  val cost_of : t -> Value.t -> int option

  val n_states : t -> int

  val n_transitions : t -> int
end
