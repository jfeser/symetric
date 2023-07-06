module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val output : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val pp : t Fmt.t
    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val eval : Op.t -> t list -> t
    val is_error : t -> bool
  end

  val operators : Op.t list
end

module Params : sig
  type t = { max_cost : int; verbose : bool } [@@deriving yojson]

  val default_verbose : bool
  val create : ?verbose:bool -> max_cost:int -> unit -> t
  val param : t Command.Param.t
end

val synthesize :
  (module DSL with type Op.t = 'op and type Value.t = 'value) ->
  ?log:(Yojson.Safe.t -> unit) ->
  Params.t ->
  [ `Pred of 'op -> 'value -> bool | `Value of 'value ] ->
  'op Program.t option
