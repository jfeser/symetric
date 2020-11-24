open Search_state

module Refinement : sig
  type t = { old : Args.t; new_ : Set.M(Abs).t }
end

val get_refinement : t -> State.t -> (Refinement.t list, Set.M(G.E).t) Either.t
