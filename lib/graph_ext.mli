module type GRAPH = sig
  include Graph.Sig.I

  module V : sig
    type t [@@deriving compare, hash, sexp_of]

    include Graph.Sig.VERTEX with type t := t
  end
  with type t = vertex

  module E : sig
    type t [@@deriving compare, hash, sexp_of]

    include Graph.Sig.EDGE with type t := t and type vertex = V.t
  end
  with type t = edge
end

module type FOLDS = sig
  type graph

  type edge

  type vertex

  module E : sig
    type t = edge [@@deriving compare, hash, sexp_of]

    include Graph.Sig.EDGE with type t := t and type vertex = vertex

    include Comparator.S with type t := t

    include Container.S0 with type t := graph and type elt := t
  end

  module V : sig
    type t = vertex [@@deriving compare, hash, sexp_of]

    include Graph.Sig.VERTEX with type t := t

    include Comparator.S with type t := t

    include Container.S0 with type t := graph and type elt := t

    val filter_map : graph -> f:(vertex -> 'a option) -> 'a list

    val filter : graph -> f:(vertex -> bool) -> vertex list
  end

  module Pred :
    Container.S0 with type t := graph * vertex and type elt := vertex

  module Succ :
    Container.S0 with type t := graph * vertex and type elt := vertex
end

module Folds (G : GRAPH) :
  FOLDS with type graph = G.t and type vertex = G.V.t and type edge = G.E.t

module type CHANGED = sig
  type graph

  type vertex

  type edge

  val add_vertex : graph -> vertex -> unit

  val remove_vertex : graph -> vertex -> unit

  val add_edge : graph -> vertex -> vertex -> unit

  val add_edge_e : graph -> edge -> unit

  val remove_edge : graph -> vertex -> vertex -> unit

  val remove_edge_e : graph -> edge -> unit

  val has_changed : unit -> bool

  val reset_changed : unit -> unit
end

module Changed (G : GRAPH) () :
  CHANGED with type graph = G.t and type vertex = G.V.t and type edge = G.E.t
