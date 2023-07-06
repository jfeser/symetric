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

    val eval : Op.t -> t list -> t
    val distance : t -> t -> float
    val target_distance : t -> float
    val is_error : t -> bool
    val pp : t Fmt.t
  end

  val operators : Op.t list
  val serialize : Op.t Program.t -> Sexp.t
  val rewrite : Op.t Program.t -> Op.t Program.t list
end

module Params : sig
  type t = {
    local_search_steps : int;
    group_threshold : float;
    max_cost : int;
    max_height : int;
    backward_pass_repeats : int;
    verbosity : int;
    target_groups : int;
    use_ranking : bool;
    extract : [ `Centroid | `Exhaustive | `Greedy | `Random ];
    repair : [ `Guided | `Random ];
    exhaustive_width : int;
  }
  [@@deriving yojson]

  val create :
    ?backward_pass_repeats:int ->
    ?verbosity:int ->
    ?use_ranking:bool ->
    ?extract:[ `Centroid | `Exhaustive | `Greedy | `Random ] ->
    ?repair:[ `Guided | `Random ] ->
    ?exhaustive_width:int ->
    ?max_height:int ->
    local_search_steps:int ->
    group_threshold:float ->
    max_cost:int ->
    n_groups:int ->
    unit ->
    t

  val param : t Command.Param.t
end

val synthesize :
  ?log:(Yojson.Safe.t -> unit) ->
  Params.t ->
  (module DSL with type Op.t = 'op) ->
  'op Program.t option
