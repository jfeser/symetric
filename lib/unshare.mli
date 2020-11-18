module type LABELED_GRAPH = sig
  type vertex

  include
    Graph_ext.GRAPH
      with type vertex := vertex
       and type edge = vertex * int * vertex
end

module One_to_many : sig
  type ('a, 'b) t = { forward : 'a -> 'b Sequence.t; backward : 'b -> 'a }
end

module Make
    (G : LABELED_GRAPH) (K : sig
      val kind : G.V.t -> [ `Args | `State ]
    end) : sig
  module V_ref : sig
    type t

    val pp : G.V.t Fmt.t -> t Fmt.t

    val vertex : t -> G.V.t
  end

  module G_replicated :
    LABELED_GRAPH
      with type vertex = V_ref.t
       and type edge = V_ref.t * int * V_ref.t

  val unshare : G.t -> G_replicated.t * (G.V.t, V_ref.t) One_to_many.t
end
