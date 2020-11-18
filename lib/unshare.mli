module type LABELED_GRAPH = sig
  type vertex

  include
    Graph_ext.GRAPH
      with type vertex := vertex
       and type edge = vertex * int * vertex
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
    Graph.Sig.I
      with type V.t = V_ref.t
       and type V.label = V_ref.t
       and type E.t = V_ref.t * int * V_ref.t
       and type E.label = int

  val unshare : G.t -> G_replicated.t
end
