open Search_state

module Refinement : sig
  type t = { context : Args.t; splits : (State.t * Abs.t list) list }
end

val get_refinement :
  t ->
  State.t ->
  bool array ->
  Node.t list ->
  (Refinement.t list, Set.M(E).t) Either.t
