module Reachable (G : Graph.Fixpoint.G) =
  Graph.Fixpoint.Make
    (G)
    (struct
      type vertex = G.V.t
      type edge = G.E.t
      type g = G.t
      type data = bool

      let direction = Graph.Fixpoint.Forward
      let equal = Bool.( = )
      let join = ( || )
      let analyze _ x = x
    end)

module Inv_reachable (G : Graph.Fixpoint.G) =
  Graph.Fixpoint.Make
    (G)
    (struct
      type vertex = G.V.t
      type edge = G.E.t
      type g = G.t
      type data = bool

      let direction = Graph.Fixpoint.Backward
      let equal = Bool.( = )
      let join = ( || )
      let analyze _ x = x
    end)
