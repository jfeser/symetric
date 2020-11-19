open Graph_ext

module One_to_many : sig
  type ('a, 'b) t = { forward : 'a -> 'b Sequence.t; backward : 'b -> 'a }
end

module Make
    (G : LABELED_GRAPH with type label = int) (K : sig
      val kind : G.V.t -> [ `Args | `State ]
    end) : sig
  module G_replicated : LABELED_GRAPH with type label = int

  val unshare :
    G.t -> G_replicated.t * (G.V.t, G_replicated.vertex) One_to_many.t
end
