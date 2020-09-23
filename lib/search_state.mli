module Op : sig
  type t = Input of Conc.t | Union | Inter | Sub
  [@@deriving compare, equal, hash, sexp]

  val pp : t Fmt.t

  val arity : t -> int
end

module Args : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val create : Op.t -> t

  val id : t -> int

  val op : t -> Op.t

  val graphviz_pp : t Fmt.t
end

module State : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val create : Abs.t -> int -> t

  val id : t -> int

  val state : t -> Abs.t

  val cost : t -> int

  val graphviz_pp : t Fmt.t
end

module Node : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val match_ : args:(Args.t -> 'a) -> state:(State.t -> 'a) -> t -> 'a

  val pp : t Fmt.t

  val id : t -> int

  val is_state : t -> bool

  val is_args : t -> bool

  val of_args : Args.t -> t

  val of_state : State.t -> t

  val to_args : t -> Args.t option

  val to_state : t -> State.t option

  val to_args_exn : t -> Args.t

  val to_state_exn : t -> State.t
end

module G :
  Graph.Sig.I with type V.t = Node.t and type E.t = Node.t * int * Node.t

module Args_table_key : sig
  type t = Op.t * State.t list [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t
end

module State_table_key : sig
  type t = Abs.t * int [@@deriving compare, hash, sexp]

  include Comparator.S with type t := t
end

type t = {
  graph : G.t;
  args_table : Args.t Hashtbl.M(Args_table_key).t;
  state_table : State.t Hashtbl.M(State_table_key).t;
  cost_table : State.t list array;
}

module E : sig
  include Comparator.S with type t := G.E.t

  include Container.S0 with type t := t and type elt := G.E.t

  type t = G.E.t
end

module V : sig
  include Comparator.S with type t := G.V.t

  include Container.S0 with type t := t and type elt := G.V.t

  val filter_map : t -> f:(G.V.t -> 'a option) -> 'a list

  val filter : t -> f:(G.V.t -> bool) -> G.V.t list

  type t = G.V.t
end

val create : int -> t

val pred : t -> G.V.t -> G.V.t list

val pred_e : t -> G.V.t -> G.E.t list

val succ : t -> G.V.t -> G.V.t list

val succ_e : t -> G.V.t -> G.E.t list

val add_edge_e : t -> G.E.t -> unit

val states_of_cost : t -> int -> State.t list

val set_states_of_cost : t -> int -> State.t list -> unit

val filter : t -> f:(G.V.t -> bool) -> unit

val remove_vertexes : t -> G.V.t list -> unit

val nb_vertex : t -> int

val check : t -> unit

val inputs : t -> Args.t -> State.t list
