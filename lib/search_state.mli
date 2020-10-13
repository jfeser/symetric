module Is_fresh : sig
  type 'a t = Fresh of 'a | Stale of 'a

  val is_fresh : 'a t -> bool

  val unwrap : 'a t -> 'a
end

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

  val create : Abs.t -> int -> t Is_fresh.t

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

module G : sig
  include
    Graph.Sig.I with type V.t = Node.t and type E.t = Node.t * int * Node.t

  val has_changed : unit -> bool

  val reset_changed : unit -> unit
end

type t = G.t

module E : sig
  include Comparator.S with type t := G.E.t

  include Container.S0 with type t := G.t and type elt := G.E.t

  type t = G.E.t
end

module V : sig
  include Comparator.S with type t := G.V.t

  include Container.S0 with type t := G.t and type elt := G.V.t

  val filter_map : G.t -> f:(G.V.t -> 'a option) -> 'a list

  val filter : G.t -> f:(G.V.t -> bool) -> G.V.t list

  type t = G.V.t
end

module Pred : Container.S0 with type t := G.t * G.V.t and type elt := G.V.t

module Succ : Container.S0 with type t := G.t * G.V.t and type elt := G.V.t

val create : unit -> t

val states_of_cost : t -> int -> State.t list

val filter : t -> f:(G.V.t -> bool) -> unit

val nb_vertex : t -> int

val inputs : t -> Args.t -> State.t list

val fix_up : t -> unit

val insert_hyper_edge_if_not_exists :
  t -> State.t list -> Op.t -> State.t -> unit
