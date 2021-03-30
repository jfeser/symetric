module type S = sig
  module Search_state : sig
    type t

    module Args : Comparator.S

    module State : sig
      type t
    end

    module G : sig
      module V : sig
        type t
      end

      module E : sig
        type t
      end
    end
  end

  module Abs : Comparator.S

  type op

  open Search_state

  module Refinement : sig
    type elem =
      | Remove_edge of G.V.t * G.V.t
      | Add_edge of G.E.t
      | Add_merge of State.t list * Abs.t
    [@@deriving compare, sexp_of]

    type t = elem list [@@deriving compare, sexp_of]
  end

  val refine : t -> State.t list -> (Refinement.t, op Program.t) Either.t

  val summarize : (t -> State.t list -> (Abs.t * State.t list) list) option
end
