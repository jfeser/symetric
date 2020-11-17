open Ast

module Is_fresh : sig
  type 'a t = Fresh of 'a | Stale of 'a

  val is_fresh : 'a t -> bool

  val unwrap : 'a t -> 'a
end

module Args : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val create : Op.t -> t

  val id : t -> int

  val op : t -> Op.t

  val graphviz_pp : t Fmt.t

  val output_type : t -> Type.t
end

module State : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val create : Abs.t -> int -> Type.t -> t Is_fresh.t

  val id : t -> int

  val state : t -> Abs.t

  val cost : t -> int

  val type_ : t -> Type.t

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

  val type_ : t -> Type.t
end

module G : sig
  include
    Graph.Sig.I with type V.t = Node.t and type E.t = Node.t * int * Node.t

  include
    Graph_ext.CHANGED
      with type graph = t
       and type vertex = V.t
       and type edge = E.t

  include
    Graph_ext.FOLDS
      with type graph = t
       and type vertex = V.t
       and type edge = E.t
end

type t = G.t

val create : unit -> t

val states_of_cost : t -> int -> State.t list

val filter : t -> f:(G.V.t -> bool) -> unit

val nb_vertex : t -> int

val inputs : t -> Args.t -> State.t list

val fix_up : t -> unit

val insert_hyper_edge_if_not_exists :
  t -> State.t list -> Op.t -> State.t -> unit

val pp : t Fmt.t

module Unshare (G_condensed : sig
  module V : sig
    type t [@@deriving sexp_of]

    val hash : t -> int

    val compare : t -> t -> int

    val kind : t -> [ `Args | `State ]
  end

  module E : sig
    type t = V.t * int * V.t
  end

  type t

  val succ_e : t -> V.t -> E.t list

  val in_degree : t -> V.t -> int

  val iter_vertex : (V.t -> unit) -> t -> unit
end) : sig
  module V_ref : sig
    type t

    val pp : G_condensed.V.t Fmt.t -> t Fmt.t

    val vertex : t -> G_condensed.V.t
  end

  module G_replicated :
    Graph.Sig.I
      with type V.t = V_ref.t
       and type V.label = V_ref.t
       and type E.t = V_ref.t * int * V_ref.t
       and type E.label = int

  val unshare : G_condensed.t -> G_replicated.t
end
