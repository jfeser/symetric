open Search_state

module Refinement : sig
  type t = { context : Args.t * State.t list; splits : Set.M(Abs).t }
end

val get_refinement :
  ?use_fallback:bool ->
  t ->
  State.t ->
  Conc.t ->
  Node.t list ->
  (Refinement.t list option, Set.M(E).t) Either.t
