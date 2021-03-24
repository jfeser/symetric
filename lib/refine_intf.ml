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

  open Search_state

  module Refinement : sig
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t } [@@deriving sexp]

    type t = s Map.M(Args).t [@@deriving sexp_of]
  end

  val refine : t -> State.t list -> (Refinement.t, op Program.t) Either.t

  val summarize : (t -> State.t list -> (Abs.t * State.t list) list) option
end
