open Search_state

module Refinement : sig
  type t = { old : Args.t; new_ : Set.M(Abs).t }
end

val get_refinement :
  t ->
  State.t ->
  Conc.t ->
  Set.M(Args).t ->
  (Refinement.t list option, Set.M(G.E).t) Either.t
