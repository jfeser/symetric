open Search_state

module Refinement : sig
  type t = { context : Args.t; splits : Abs.t list }
end

val get_refinement :
  t ->
  State.t ->
  Conc.t ->
  Node.t list ->
  (Refinement.t list, Set.M(E).t) Either.t
