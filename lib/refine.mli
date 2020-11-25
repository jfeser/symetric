module Refinement : sig
  type t = { old : Search_state.Args.t; new_ : Set.M(Abs).t }
end

val get_refinement :
  Search_state.t ->
  Search_state.State.t ->
  (Refinement.t list, Set.M(Search_state.G.E).t) Either.t
