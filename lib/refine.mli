module Refinement : sig
  type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t } [@@deriving sexp]

  type t = s Map.M(Search_state.Args).t [@@deriving sexp_of]
end

val get_refinement :
  Search_state.t -> Search_state.State.t -> (Refinement.t, Program.t) Either.t
