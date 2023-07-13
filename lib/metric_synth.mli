module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]
    (** Operator argument and return types. *)

    val output : t
    (** The type of the output of a program. In the case of CAD, this is a 2D scene. *)
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp, yojson]
    (** Operators in the DSL. *)

    val cost : t -> int
    (** [cost op] is the cost of applying [op]. *)

    val arity : t -> int
    (** [arity op] is the number of arguments of [op]. *)

    val args_type : t -> Type.t list
    (** [args_type op] is the types of the arguments of [op]. *)

    val ret_type : t -> Type.t
    (** [ret_type op] is the type of the return value of [op]. *)

    val is_commutative : t -> bool
    (** [is_commutative op] is true if [op] is commutative. *)

    val pp : t Fmt.t
    (** [pp] is a pretty-printer for [t]. *)
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]
    (** Values in the DSL. *)

    val eval : Op.t -> t list -> t
    (** [eval op args] is the result of applying [op] to [args]. *)

    val distance : t -> t -> float
    (** [distance v1 v2] is the distance between [v1] and [v2]. *)

    val target_distance : t -> float
    (** [target_distance v] is the distance between [v] and the target (e.g. for CAD this would be the distance to the goal scene). *)

    val is_error : t -> bool
    (** [is_error v] is true if [v] is an error value. Programs that evaluate to an error value are not retained in the search space. *)

    val pp : t Fmt.t
    (** [pp] is a pretty-printer for [t]. *)
  end

  val operators : Op.t list
  (** [operators] is the list of operators in the DSL. *)

  val serialize : Op.t Program.t -> Sexp.t
  (** [serialize p] is a S-expression representation of [p]. *)

  val rewrite : Op.t Program.t -> Op.t Program.t list
  (** [rewrite p] is the list of programs that can be obtained by applying a single rewrite rule to [p]. *)
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
