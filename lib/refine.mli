module Refinement : sig
  type t = Set.M(Abs).t Map.M(Search_state.Args).t
end

val get_refinement :
  Search_state.t -> Search_state.State.t -> (Refinement.t, Program.t) Either.t
