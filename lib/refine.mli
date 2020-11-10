open Search_state

module Refinement : sig
  type t = { old : State.t; new_ : Set.M(Abs).t }
end

val get_refinement :
  ?use_fallback:bool ->
  t ->
  State.t ->
  Conc.t ->
  State.t list ->
  (Refinement.t list option, Set.M(E).t) Either.t
