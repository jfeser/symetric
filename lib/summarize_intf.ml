module type S = sig
  module Search_state : sig
    type t

    module Args : Comparator.S

    module State : sig
      type t
    end
  end

  module Abs : Comparator.S

  type op

  module Refinement : sig
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t } [@@deriving sexp]
    type t = s Map.M(Search_state.Args).t [@@deriving sexp_of]
  end

  val summarize : Search_state.t -> Search_state.State.t list -> Refinement.t
end
